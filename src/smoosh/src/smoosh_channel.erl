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
-export([start_link/1, close/1, suspend/1, resume/1, activate/1, get_status/1]).
-export([enqueue/3, last_updated/2, flush/1, is_key/2, is_activated/1]).

% gen_server api.
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(VIEW_CHANNELS,
    smoosh_utils:split(
        config:get("smoosh", "view_channels", "upgrade_views,ratio_views,slack_views")
    )
).
-define(VSN, 1).
-define(CHECKPOINT_INTERVAL_IN_MSEC, 180000).

-ifndef(TEST).
-define(START_DELAY_IN_MSEC, 60000).
-define(ACTIVATE_DELAY_IN_MSEC, 30000).
-else.
-define(START_DELAY_IN_MSEC, 0).
-define(ACTIVATE_DELAY_IN_MSEC, 0).
-export([persist/1]).
-endif.

% records.

% When the state is set to activated = true, the channel has completed the state
% recovery process that occurs on (re)start and is accepting new compaction jobs.
% Note: if activated = false and a request for a new compaction job is received,
% smoosh will enqueue this new job after the state recovery process has finished.
% When the state is set to paused = false, the channel is actively compacting any
% compaction jobs that are scheduled.
% See operator_guide.md --> State diagram.

-record(state, {
    active = [],
    name,
    waiting,
    paused = true,
    starting = [],
    activated = false,
    requests = []
}).

% public functions.

start_link(Name) ->
    gen_server:start_link(?MODULE, Name, []).

suspend(ServerRef) ->
    gen_server:call(ServerRef, suspend).

resume(ServerRef) ->
    gen_server:call(ServerRef, resume_and_activate).

activate(ServerRef) ->
    gen_server:call(ServerRef, activate).

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

is_activated(ServerRef) ->
    gen_server:call(ServerRef, is_activated).

% Only exported to force persistence in tests
persist(ServerRef) ->
    gen_server:call(ServerRef, persist).

% gen_server functions.

init(Name) ->
    erlang:send_after(60 * 1000, self(), check_window),
    process_flag(trap_exit, true),
    Waiting = smoosh_priority_queue:new(Name),
    Persist = config:get("smoosh", "persist", "false"),
    case lists:member(Name, ?VIEW_CHANNELS) orelse Persist =:= "false" of
        true ->
            State = #state{name = Name, waiting = Waiting, paused = true, activated = true},
            schedule_unpause();
        false ->
            State = #state{name = Name, waiting = Waiting, paused = true, activated = false},
            erlang:send_after(?START_DELAY_IN_MSEC, self(), start_recovery)
    end,
    {ok, State}.

handle_call({last_updated, Object}, _From, State) ->
    LastUpdated = smoosh_priority_queue:last_updated(Object, State#state.waiting),
    {reply, LastUpdated, State};
handle_call(suspend, _From, State) ->
    #state{active = Active} = State,
    [
        catch erlang:suspend_process(Pid, [unless_suspending])
     || {_, Pid} <- Active
    ],
    {reply, ok, State#state{paused = true}};
handle_call(resume_and_activate, _From, State) ->
    #state{active = Active} = State,
    [catch erlang:resume_process(Pid) || {_, Pid} <- Active],
    {reply, ok, State#state{paused = false, activated = true}};
handle_call(activate, _From, State) ->
    {reply, ok, State#state{activated = true}};
handle_call(status, _From, State) ->
    {reply,
        {ok, [
            {active, length(State#state.active)},
            {starting, length(State#state.starting)},
            {waiting, smoosh_priority_queue:info(State#state.waiting)}
        ]},
        State};
handle_call(close, _From, State) ->
    {stop, normal, ok, State};
handle_call(flush, _From, #state{waiting = Q} = State) ->
    {reply, ok, State#state{waiting = smoosh_priority_queue:flush(Q)}};
handle_call({is_key, Key}, _From, #state{waiting = Waiting} = State) ->
    {reply, smoosh_priority_queue:is_key(Key, Waiting), State};
handle_call(is_activated, _From, #state{activated = Activated} = State0) ->
    {reply, Activated, State0};
handle_call(persist, _From, State) ->
    persist_queue(State),
    {reply, ok, State}.

handle_cast({enqueue, _Object, 0}, #state{} = State) ->
    {noreply, State};
handle_cast({enqueue, Object, Priority}, #state{activated = true} = State) ->
    {noreply, maybe_start_compaction(add_to_queue(Object, Priority, State))};
handle_cast({enqueue, Object, Priority}, #state{activated = false, requests = Requests} = State0) ->
    Level = smoosh_utils:log_level("compaction_log_level", "debug"),
    couch_log:Level(
        "~p Channel is not activated yet. Adding ~p to requests with priority ~p.", [
            ?MODULE,
            Object,
            Priority
        ]
    ),
    {noreply, State0#state{requests = [{Object, Priority} | Requests]}}.

% We accept noproc here due to possibly having monitored a restarted compaction
% pid after it finished.
handle_info({'DOWN', Ref, _, Job, Reason}, State) when
    Reason == normal;
    Reason == noproc
->
    #state{active = Active, starting = Starting} = State,
    {noreply,
        maybe_start_compaction(
            State#state{
                active = lists:keydelete(Job, 2, Active),
                starting = lists:keydelete(Ref, 1, Starting)
            }
        )};
handle_info({'DOWN', Ref, _, Job, Reason}, State) ->
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
                        smoosh_utils:stringify(Key),
                        Reason
                    ]),
                    {ok, _} = timer:apply_after(5000, smoosh_server, enqueue, [Key]),
                    {noreply, maybe_start_compaction(State#state{starting = Starting1})};
                false ->
                    {noreply, State}
            end
    end;
handle_info({Ref, {ok, Pid}}, State) when is_reference(Ref) ->
    case lists:keytake(Ref, 1, State#state.starting) of
        {value, {_, Key}, Starting1} ->
            Level = smoosh_utils:log_level("compaction_log_level", "notice"),
            couch_log:Level(
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
handle_info(check_window, State) ->
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
                {reply, ok, NewState} = handle_call(resume_and_activate, nil, State),
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
handle_info(start_recovery, #state{name = Name, waiting = Waiting0} = State0) ->
    RecActive = recover(active_file_name(Name)),
    Waiting1 = lists:foldl(
        fun(DbName, Acc) ->
            case couch_db:exists(DbName) andalso couch_db:is_compacting(DbName) of
                true ->
                    Priority = smoosh_server:get_priority(Name, DbName),
                    smoosh_priority_queue:in(DbName, Priority, Priority, Acc);
                false ->
                    Acc
            end
        end,
        Waiting0,
        RecActive
    ),
    State1 = maybe_start_compaction(State0#state{paused = false, waiting = Waiting1}),
    Level = smoosh_utils:log_level("compaction_log_level", "debug"),
    couch_log:Level(
        "~p Previously active compaction jobs (if any) have been successfully recovered and restarted.",
        [?MODULE]
    ),
    erlang:send_after(?ACTIVATE_DELAY_IN_MSEC, self(), activate),
    {noreply, State1#state{paused = true}};
handle_info(activate, State) ->
    {noreply, activate_channel(State)};
handle_info(persist, State) ->
    persist_queue(State),
    erlang:send_after(?CHECKPOINT_INTERVAL_IN_MSEC, self(), persist),
    {noreply, State};
handle_info(pause, State) ->
    {noreply, State#state{paused = true}};
handle_info(unpause, State) ->
    {noreply, maybe_start_compaction(State#state{paused = false})}.

terminate(_Reason, _State) ->
    ok.

persist_queue(State) ->
    write_state_to_file(State).

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
                    Level = smoosh_utils:log_level("compaction_log_level", "debug"),
                    couch_log:Level(
                        "~p Successfully restored state file ~s", [?MODULE, FilePath]
                    ),
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
            Level = smoosh_utils:log_level("compaction_log_level", "debug"),
            couch_log:Level(
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

write_state_to_file(#state{name = Name, active = Active, starting = Starting, waiting = Waiting}) ->
    Active1 = lists:foldl(
        fun({DbName, _}, Acc) ->
            [DbName | Acc]
        end,
        [],
        Active
    ),
    Starting1 = lists:foldl(
        fun({_, DbName}, Acc) ->
            [DbName | Acc]
        end,
        [],
        Starting
    ),
    smoosh_utils:write_to_file(Active1, active_file_name(Name), ?VSN),
    smoosh_utils:write_to_file(Starting1, starting_file_name(Name), ?VSN),
    smoosh_priority_queue:write_to_file(Waiting).

active_file_name(Name) ->
    filename:join(config:get("smoosh", "state_dir", "."), Name ++ ".active").

starting_file_name(Name) ->
    filename:join(config:get("smoosh", "state_dir", "."), Name ++ ".starting").

% private functions.

add_to_queue(Key, Priority, State) ->
    #state{active = Active, waiting = Q} = State,
    case lists:keymember(Key, 1, Active) of
        true ->
            State;
        false ->
            Capacity = list_to_integer(smoosh_utils:get(State#state.name, "capacity", "9999")),
            Level = smoosh_utils:log_level("compaction_log_level", "notice"),
            couch_log:Level(
                "~s: adding ~p to internal compactor queue with priority ~p",
                [State#state.name, Key, Priority]
            ),
            State#state{
                waiting = smoosh_priority_queue:in(Key, Priority, Priority, Capacity, Q)
            }
    end.

maybe_activate(#state{activated = true} = State) ->
    State;
maybe_activate(State) ->
    activate_channel(State).

activate_channel(#state{name = Name, waiting = Waiting0, requests = Requests0} = State0) ->
    RecStarting = recover(starting_file_name(Name)),
    Starting = lists:foldl(
        fun(DbName, Acc) ->
            case couch_db:exists(DbName) of
                true ->
                    Priority = smoosh_server:get_priority(Name, DbName),
                    smoosh_priority_queue:in(DbName, Priority, Priority, Acc);
                false ->
                    Acc
            end
        end,
        Waiting0,
        RecStarting
    ),
    Waiting1 = smoosh_priority_queue:recover(Starting),
    Requests1 = lists:reverse(Requests0),
    Waiting2 = lists:foldl(
        fun({DbName, Priority}, Acc) ->
            case couch_db:exists(DbName) of
                true ->
                    smoosh_priority_queue:in(DbName, Priority, Priority, Acc);
                false ->
                    Acc
            end
        end,
        Waiting1,
        Requests1
    ),
    State1 = maybe_start_compaction(State0#state{
        waiting = Waiting2, paused = false, activated = true, requests = []
    }),
    handle_info(persist, State1),
    schedule_unpause(),
    State1#state{paused = true}.

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
                    maybe_activate(State);
                {Key, Priority, Q} ->
                    try
                        State2 =
                            case start_compact(State, Key) of
                                false ->
                                    State;
                                State1 ->
                                    Level = smoosh_utils:log_level(
                                        "compaction_log_level",
                                        "notice"
                                    ),
                                    couch_log:Level(
                                        "~s: Starting compaction for ~s (priority ~p)",
                                        [State#state.name, smoosh_utils:stringify(Key), Priority]
                                    ),
                                    State1
                            end,
                        maybe_start_compaction(State2#state{waiting = Q})
                    catch
                        Class:Exception ->
                            couch_log:warning(
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
                    Level = smoosh_utils:log_level("compaction_log_level", "notice"),
                    couch_log:Level(
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
    case couch_db:open_int(DbName, []) of
        {ok, Db} ->
            case couch_db:get_compactor_pid_sync(Db) of
                nil ->
                    couch_log:warning(
                        "exit for compaction of ~p: ~p",
                        [smoosh_utils:stringify(DbName), Reason]
                    ),
                    {ok, _} = timer:apply_after(5000, smoosh_server, enqueue, [DbName]),
                    State;
                CPid ->
                    Level = smoosh_utils:log_level("compaction_log_level", "notice"),
                    couch_log:Level(
                        "~s compaction already running. Re-monitor Pid ~p",
                        [smoosh_utils:stringify(DbName), CPid]
                    ),
                    erlang:monitor(process, CPid),
                    State#state{active = [{DbName, CPid} | State#state.active]}
            end;
        {not_found, no_db_file} ->
            couch_log:warning(
                "exit for compaction of ~p: ~p",
                [smoosh_utils:stringify(DbName), {not_found, no_db_file}]
            ),
            State
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
