% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(smoosh_channel).
-behaviour(gen_server).
-vsn(1).
-include_lib("couch/include/couch_db.hrl").

% public api.
-export([start_link/1, close/1, suspend/1, resume/1, get_status/1]).
-export([enqueue/3, last_updated/2, flush/1, is_key/2]).

% gen_server api.
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(DEFAULT_CHECKPOINT_INTERVAL_IN_SEC, 180).

-define(VSN, 1).

% records.

-record(state, {
    active = [],
    name,
    waiting,
    paused = true,
    starting = [],
    opened = false
}).

% public functions.

start_link(Name) ->
    gen_server:start_link(?MODULE, Name, []).

suspend(ServerRef) ->
    gen_server:call(ServerRef, suspend).

resume(ServerRef) ->
    gen_server:call(ServerRef, resume).

enqueue(ServerRef, Object, Priority) ->
    gen_server:cast(ServerRef, {enqueue, Object, Priority}).

last_updated(ServerRef, Object) ->
    gen_server:call(ServerRef, {last_updated, Object}).

get_status(ServerRef) ->
    gen_server:call(ServerRef, status).

close(ServerRef) ->
    gen_server:call(ServerRef, close).

flush(ServerRef) ->
    gen_server:call(ServerRef, flush).

is_key(ServerRef, Key) ->
    gen_server:call(ServerRef, {is_key, Key}).

% gen_server functions.

init(Name) ->
    schedule_unpause(),
    erlang:send_after(60 * 1000, self(), check_window),
    process_flag(trap_exit, true),
    Queue = smoosh_priority_queue:new(Name),
    State = #state{name = Name, waiting = Queue},
    ok = gen_server:cast(self(), init),
    {ok, State}.

handle_call({last_updated, Object}, _From, State0) ->
    State = maybe_open_queue(State0),
    LastUpdated = smoosh_priority_queue:last_updated(Object, State#state.waiting),
    {reply, LastUpdated, State};
handle_call(suspend, _From, State0) ->
    State = maybe_open_queue(State0),
    #state{active = Active} = State,
    [
        catch erlang:suspend_process(Pid, [unless_suspending])
     || {_, Pid} <- Active
    ],
    {reply, ok, State#state{paused = true}};
handle_call(resume, _From, State0) ->
    State = maybe_open_queue(State0),
    #state{active = Active} = State,
    [catch erlang:resume_process(Pid) || {_, Pid} <- Active],
    {reply, ok, State#state{paused = false}};
handle_call(status, _From, State0) ->
    State = maybe_open_queue(State0),
    {reply,
        {ok, [
            {active, length(State#state.active)},
            {starting, length(State#state.starting)},
            {waiting, smoosh_priority_queue:info(State#state.waiting)}
        ]},
        State};
handle_call(close, _From, State0) ->
    State = maybe_open_queue(State0),
    #state{waiting = Q} = State,
    smoosh_priority_queue:close(Q),
    {stop, normal, ok, State#state{waiting = nil, opened = false}};
handle_call(flush, _From, State0) ->
    #state{waiting = Q} = State = maybe_open_queue(State0),
    {reply, ok, State#state{waiting = smoosh_priority_queue:flush(Q)}};
handle_call({is_key, Key}, _From, State0) ->
    State = maybe_open_queue(State0),
    #state{waiting = Waiting} = State,
    {reply, smoosh_priority_queue:is_key(Key, Waiting), State}.

handle_cast(init, State0) ->
    State1 = maybe_recover_state(State0),
    State2 = maybe_open_queue(State1),
    {noreply, State3} = handle_info(persist_queue, State2),
    {noreply, State3};
handle_cast({enqueue, _Object, 0}, State0) ->
    State = maybe_open_queue(State0),
    {noreply, State};
handle_cast({enqueue, Object, Priority}, State0) ->
    State = maybe_open_queue(State0),
    {noreply, maybe_start_compaction(add_to_queue(Object, Priority, State))}.

% We accept noproc here due to possibly having monitored a restarted compaction
% pid after it finished.
handle_info({'DOWN', Ref, _, Job, Reason}, State0) when
    Reason == normal;
    Reason == noproc
->
    State = maybe_open_queue(State0),
    #state{active = Active, starting = Starting} = State,
    {noreply,
        maybe_start_compaction(
            State#state{
                active = lists:keydelete(Job, 2, Active),
                starting = lists:keydelete(Ref, 1, Starting)
            }
        )};
handle_info({'DOWN', Ref, _, Job, Reason}, State0) ->
    State = maybe_open_queue(State0),
    #state{active = Active0, starting = Starting0} = State,
    case lists:keytake(Job, 2, Active0) of
        {value, {Key, _Pid}, Active1} ->
            State1 = maybe_remonitor_cpid(
                State#state{active = Active1},
                Key,
                Reason
            ),
            {noreply, maybe_start_compaction(State1)};
        false ->
            case lists:keytake(Ref, 1, Starting0) of
                {value, {_, Key}, Starting1} ->
                    couch_log:warning("failed to start compaction of ~p: ~p", [
                        smoosh_utils:stringify(Key), Reason
                    ]),
                    {ok, _} = timer:apply_after(5000, smoosh_server, enqueue, [Key]),
                    {noreply, maybe_start_compaction(State#state{starting = Starting1})};
                false ->
                    {noreply, State}
            end
    end;
handle_info({Ref, {ok, Pid}}, State0) when is_reference(Ref) ->
    State = maybe_open_queue(State0),
    case lists:keytake(Ref, 1, State#state.starting) of
        {value, {_, Key}, Starting1} ->
            couch_log:notice(
                "~s: Started compaction for ~s",
                [State#state.name, smoosh_utils:stringify(Key)]
            ),
            erlang:monitor(process, Pid),
            erlang:demonitor(Ref, [flush]),
            {noreply, State#state{
                active = [{Key, Pid} | State#state.active],
                starting = Starting1
            }};
        false ->
            {noreply, State}
    end;
handle_info(check_window, State0) ->
    State = maybe_open_queue(State0),
    #state{paused = Paused, name = Name} = State,
    StrictWindow = smoosh_utils:get(Name, "strict_window", "false"),
    FinalState =
        case {not Paused, smoosh_utils:in_allowed_window(Name)} of
            {false, false} ->
                % already in desired state
                State;
            {true, true} ->
                % already in desired state
                State;
            {false, true} ->
                % resume is always safe even if we did not previously suspend
                {reply, ok, NewState} = handle_call(resume, nil, State),
                NewState;
            {true, false} ->
                if
                    StrictWindow =:= "true" ->
                        {reply, ok, NewState} = handle_call(suspend, nil, State),
                        NewState;
                    true ->
                        State#state{paused = true}
                end
        end,
    erlang:send_after(60 * 1000, self(), check_window),
    {noreply, FinalState};
handle_info(persist_queue, State0) ->
    #state{waiting = Queue} = State0,
    write_state_to_file(State0),
    smoosh_priority_queue:write_to_file(Queue),
    Checkpoint =
        config:get_integer(
            "smoosh", "state_checkpoint_interval_in_sec", ?DEFAULT_CHECKPOINT_INTERVAL_IN_SEC
        ) * 1000,
    erlang:send_after(Checkpoint, self(), persist_queue),
    {noreply, State0};
handle_info(pause, State0) ->
    State = maybe_open_queue(State0),
    {noreply, State#state{paused = true}};
handle_info(unpause, State0) ->
    State = maybe_open_queue(State0),
    {noreply, maybe_start_compaction(State#state{paused = false})}.

terminate(_Reason, #state{name = Name, waiting = Q}) ->
    file:delete(active_file_name(Name)),
    file:delete(starting_file_name(Name)),
    if
        Q =/= nil ->
            smoosh_priority_queue:close(Q);
        true ->
            nil
    end,
    ok.

maybe_recover_state(#state{name = Name} = State) ->
    Active = recover(active_file_name(Name)),
    Starting = recover(starting_file_name(Name)),
    DatabaseDir = config:get("couchdb", "database_dir"),
    ViewDir = config:get("couchdb", "view_index_dir"),
    Active1 = get_matching_compact_files(DatabaseDir, Active),
    Active2 = get_matching_compact_files(ViewDir, Active),
    Active3 = Active1 ++ Active2,
    State#state{active = Active3, starting = Starting}.

get_matching_compact_files(Dir, Active) ->
    MatchingFiles = filelib:fold_files(
        Dir,
        "^[a-zA-Z0-9_.-]*.compact$",
        true,
        (fun(FilePath, Acc) ->
            FilePrefix = filename:rootname(FilePath, ".compact"),
            case lists:keyfind(FilePrefix, 1, Active) of
                false ->
                    Acc;
                Tuple ->
                    [Tuple | Acc]
            end
        end),
        []
    ),
    lists:reverse(MatchingFiles).

recover(FilePath) ->
    case do_recover(FilePath) of
        {ok, List} ->
            List;
        error ->
            []
    end.

do_recover(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            <<Vsn, Binary/binary>> = Content,
            try parse_state(Vsn, ?VSN, Binary) of
                Term ->
                    {ok, Term}
            catch
                error:Reason ->
                    couch_log:error(
                        "~p Invalid state file (~p). Deleting ~s", [?MODULE, Reason, FilePath]
                    ),
                    file:delete(FilePath),
                    error
            end;
        {error, enoent} ->
            couch_log:notice(
                "~p (~p) State file ~s does not exist. Not restoring.", [?MODULE, enoent, FilePath]
            ),
            error;
        {error, Reason} ->
            couch_log:error(
                "~p Cannot read the state file (~p). Deleting ~s", [?MODULE, Reason, FilePath]
            ),
            file:delete(FilePath),
            error
    end.

parse_state(1, ?VSN, Binary) ->
    erlang:binary_to_term(Binary, [safe]);
parse_state(Vsn, ?VSN, _) ->
    error({unsupported_version, Vsn}).

write_state_to_file(#state{name = Name, active = Active, starting = Starting}) ->
    write_to_file(Active, active_file_name(Name)),
    write_to_file(Starting, starting_file_name(Name)).

write_to_file(List, FileName) ->
    OnDisk = <<?VSN, (erlang:term_to_binary(List, [compressed, {minor_version, 1}]))/binary>>,
    TmpFileName = FileName ++ ".tmp",
    file:delete(TmpFileName),
    file:write_file(TmpFileName, OnDisk, [sync]),
    file:delete(FileName),
    file:rename(TmpFileName, FileName).

active_file_name(Name) ->
    filename:join(config:get("smoosh", "state_dir", "."), Name ++ ".active").

starting_file_name(Name) ->
    filename:join(config:get("smoosh", "state_dir", "."), Name ++ ".starting").

% private functions.

maybe_open_queue(#state{opened = true} = State) ->
    State;
maybe_open_queue(#state{opened = false, waiting = Queue} = State) ->
    State#state{waiting = smoosh_priority_queue:open(Queue), opened = true}.

add_to_queue(Key, Priority, State) ->
    #state{active = Active, waiting = Q} = State,
    case lists:keymember(Key, 1, Active) of
        true ->
            State;
        false ->
            Capacity = list_to_integer(smoosh_utils:get(State#state.name, "capacity", "9999")),
            couch_log:notice(
                "~s: adding ~p to internal compactor queue with priority ~p",
                [State#state.name, Key, Priority]
            ),
            State#state{
                waiting = smoosh_priority_queue:in(Key, Priority, Priority, Capacity, Q)
            }
    end.

maybe_start_compaction(#state{paused = true} = State) ->
    State;
maybe_start_compaction(State) ->
    Concurrency = list_to_integer(
        smoosh_utils:get(
            State#state.name,
            "concurrency",
            "1"
        )
    ),
    if
        length(State#state.active) + length(State#state.starting) < Concurrency ->
            case smoosh_priority_queue:out(State#state.waiting) of
                false ->
                    State;
                {Key, Priority, Q} ->
                    try
                        State2 =
                            case start_compact(State, Key) of
                                false ->
                                    State;
                                State1 ->
                                    couch_log:notice(
                                        "~s: Starting compaction for ~s (priority ~p)",
                                        [State#state.name, smoosh_utils:stringify(Key), Priority]
                                    ),
                                    State1
                            end,
                        maybe_start_compaction(State2#state{waiting = Q})
                    catch
                        Class:Exception ->
                            couch_log:notice(
                                "~s: ~p ~p for ~s",
                                [
                                    State#state.name,
                                    Class,
                                    Exception,
                                    smoosh_utils:stringify(Key)
                                ]
                            ),
                            maybe_start_compaction(State#state{waiting = Q})
                    end
            end;
        true ->
            State
    end.

start_compact(State, {schema, DbName, GroupId}) ->
    case smoosh_utils:ignore_db({DbName, GroupId}) of
        false ->
            {ok, Pid} = couch_md_index_manager:get_group_pid(
                DbName,
                GroupId
            ),
            Ref = erlang:monitor(process, Pid),
            Pid ! {'$gen_call', {self(), Ref}, compact},
            State#state{starting = [{Ref, {schema, DbName, GroupId}} | State#state.starting]};
        _ ->
            false
    end;
start_compact(State, DbName) when is_list(DbName) ->
    start_compact(State, ?l2b(DbName));
start_compact(State, DbName) when is_binary(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    try
        start_compact(State, Db)
    after
        couch_db:close(Db)
    end;
start_compact(State, {Shard, GroupId}) ->
    case smoosh_utils:ignore_db({Shard, GroupId}) of
        false ->
            DbName = mem3:dbname(Shard),
            {ok, Pid} = couch_index_server:get_index(
                couch_mrview_index, Shard, GroupId
            ),
            spawn(fun() -> cleanup_index_files(DbName, Shard) end),
            Ref = erlang:monitor(process, Pid),
            Pid ! {'$gen_call', {self(), Ref}, compact},
            State#state{starting = [{Ref, {Shard, GroupId}} | State#state.starting]};
        _ ->
            false
    end;
start_compact(State, Db) ->
    case smoosh_utils:ignore_db(Db) of
        false ->
            DbPid = couch_db:get_pid(Db),
            Key = couch_db:name(Db),
            case couch_db:get_compactor_pid(Db) of
                nil ->
                    Ref = erlang:monitor(process, DbPid),
                    DbPid ! {'$gen_call', {self(), Ref}, start_compact},
                    State#state{starting = [{Ref, Key} | State#state.starting]};
                % Compaction is already running, so monitor existing compaction pid.
                CPid ->
                    couch_log:notice(
                        "Db ~s continuing compaction",
                        [smoosh_utils:stringify(Key)]
                    ),
                    erlang:monitor(process, CPid),
                    State#state{active = [{Key, CPid} | State#state.active]}
            end;
        _ ->
            false
    end.

maybe_remonitor_cpid(State, DbName, Reason) when is_binary(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    case couch_db:get_compactor_pid_sync(Db) of
        nil ->
            couch_log:warning(
                "exit for compaction of ~p: ~p",
                [smoosh_utils:stringify(DbName), Reason]
            ),
            {ok, _} = timer:apply_after(5000, smoosh_server, enqueue, [DbName]),
            State;
        CPid ->
            couch_log:notice(
                "~s compaction already running. Re-monitor Pid ~p",
                [smoosh_utils:stringify(DbName), CPid]
            ),
            erlang:monitor(process, CPid),
            State#state{active = [{DbName, CPid} | State#state.active]}
    end;
% not a database compaction, so ignore the pid check
maybe_remonitor_cpid(State, Key, Reason) ->
    couch_log:warning(
        "exit for compaction of ~p: ~p",
        [smoosh_utils:stringify(Key), Reason]
    ),
    {ok, _} = timer:apply_after(5000, smoosh_server, enqueue, [Key]),
    State.

schedule_unpause() ->
    WaitSecs = list_to_integer(config:get("smoosh", "wait_secs", "30")),
    erlang:send_after(WaitSecs * 1000, self(), unpause).

cleanup_index_files(DbName, _Shard) ->
    case config:get("smoosh", "cleanup_index_files", "false") of
        "true" ->
            fabric:cleanup_index_files(DbName);
        _ ->
            ok
    end.
