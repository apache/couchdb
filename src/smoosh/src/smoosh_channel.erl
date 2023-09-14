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

% public api.
-export([start_link/1, close/1, suspend/1, resume/1, get_status/1]).
-export([enqueue/3, flush/1]).
-export([get_status_table/1]).

% gen_server api.
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(INDEX_CLEANUP, index_cleanup).
-define(TIME_WINDOW_MSEC, 60 * 1000).
-define(CHECKPOINT_INTERVAL_MSEC, 180000).

-ifdef(TEST).
-define(RE_ENQUEUE_INTERVAL, 50).
-define(STATUS_UPDATE_INTERVAL_MSEC, 490).
-else.
-define(RE_ENQUEUE_INTERVAL, 5000).
-define(STATUS_UPDATE_INTERVAL_MSEC, 4900).
-endif.

% If persistence is configured, on startup the channel will try to load its
% waiting queue from a persisted queue data file. Then, during every
% CHECKPOINT_INTERVAL_MSEC, it will spawn an checkpointer process to write the
% persisted queue state to disk.

-record(state, {
    % Channel name
    name,
    % smoosh_persisted_queue:new() object
    waiting,
    % Paused flag. The channel starts in a the paused state.
    paused = true,
    % #{Key => Pid}
    active = #{},
    % #{Ref => Key}
    starting = #{},
    % Monitor reference of the checkpointer process
    cref,
    % ETS status table handle. Used to publish channel status.
    stab
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

get_status(StatusTab) when is_reference(StatusTab) ->
    try ets:lookup(StatusTab, status) of
        [{status, Status}] -> Status;
        [] -> #{}
    catch
        error:badarg ->
            #{}
    end.

close(ServerRef) ->
    gen_server:call(ServerRef, close).

flush(ServerRef) ->
    gen_server:call(ServerRef, flush).

get_status_table(ServerRef) ->
    gen_server:call(ServerRef, get_status_table).

% gen_server functions.

init(Name) ->
    process_flag(trap_exit, true),
    process_flag(message_queue_data, off_heap),
    schedule_check_window(),
    schedule_update_status(),
    schedule_checkpoint(),
    schedule_unpause(),
    STab = ets:new(smoosh_stats, [{read_concurrency, true}]),
    Waiting = unpersist(Name),
    State = #state{name = Name, waiting = Waiting, stab = STab},
    {ok, set_status(State)}.

handle_call(get_status_table, _From, #state{} = State) ->
    State1 = set_status(State),
    {reply, {ok, State#state.stab}, State1};
handle_call(suspend, _From, #state{} = State) ->
    {reply, ok, do_suspend(State)};
handle_call(resume, _From, #state{} = State) ->
    {reply, ok, do_resume(State)};
handle_call(close, _From, State) ->
    {stop, normal, ok, State};
handle_call(flush, _From, #state{waiting = Q} = State) ->
    State1 = State#state{waiting = smoosh_priority_queue:flush(Q)},
    smoosh_persist:persist(State1#state.waiting, #{}, #{}),
    State2 = set_status(State1),
    {reply, ok, State2}.

handle_cast({enqueue, Object, Priority}, #state{} = State) ->
    State1 = add_to_queue(Object, Priority, State),
    {noreply, maybe_start_compaction(State1)}.

handle_info({'DOWN', Ref, _, _, _}, #state{cref = Ref} = State) ->
    {noreply, State#state{cref = undefined}};
% We accept noproc here due to possibly having monitored a restarted compaction
% pid after it finished.
handle_info({'DOWN', Ref, _, Job, Reason}, #state{} = State) when
    Reason == normal;
    Reason == noproc
->
    #state{active = Active, starting = Starting} = State,
    Active1 = maps:filter(fun(_, Pid) -> Pid =/= Job end, Active),
    Starting1 = maps:remove(Ref, Starting),
    State1 = State#state{active = Active1, starting = Starting1},
    {noreply, maybe_start_compaction(State1)};
handle_info({'DOWN', Ref, _, Job, Reason}, #state{} = State) ->
    #state{name = Name, active = Active, starting = Starting} = State,
    FoundActive = maps:filter(fun(_, Pid) -> Pid =:= Job end, Active),
    case maps:to_list(FoundActive) of
        [{Key, _Pid}] ->
            Active1 = maps:without([Key], Active),
            State1 = State#state{active = maps:without([Key], Active1)},
            State2 = maybe_remonitor_cpid(State1, Key, Reason),
            {noreply, maybe_start_compaction(State2)};
        [] ->
            case maps:take(Ref, Starting) of
                {Key, Starting1} ->
                    LogMsg = "~s : failed to start compaction of ~p: ~p",
                    LogArgs = [Name, smoosh_utils:stringify(Key), Reason],
                    couch_log:warning(LogMsg, LogArgs),
                    re_enqueue(Key),
                    State1 = State#state{starting = Starting1},
                    {noreply, maybe_start_compaction(State1)};
                error ->
                    {noreply, State}
            end
    end;
% This is the '$gen_call' response handling
handle_info({Ref, {ok, Pid}}, #state{} = State) when is_reference(Ref) ->
    #state{name = Name, active = Active, starting = Starting} = State,
    case maps:take(Ref, Starting) of
        {Key, Starting1} ->
            Level = smoosh_utils:log_level("compaction_log_level", "notice"),
            LogMsg = "~s: Started compaction for ~s",
            LogArgs = [Name, smoosh_utils:stringify(Key)],
            couch_log:Level(LogMsg, LogArgs),
            erlang:monitor(process, Pid),
            erlang:demonitor(Ref, [flush]),
            Active1 = Active#{Key => Pid},
            State1 = State#state{active = Active1, starting = Starting1},
            {noreply, set_status(State1)};
        error ->
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
                do_resume(State);
            {true, false} when StrictWindow =:= "true" ->
                % suspend immediately
                do_suspend(State);
            {true, false} ->
                % prevent new jobs from starting, active ones keep running
                State#state{paused = true}
        end,
    schedule_check_window(),
    {noreply, FinalState};
handle_info(update_status, #state{} = State) ->
    schedule_update_status(),
    {noreply, set_status(State)};
handle_info(checkpoint, #state{cref = Ref} = State) when is_reference(Ref) ->
    % If a checkpointer process is still running, don't start another one.
    schedule_checkpoint(),
    {noreply, State};
handle_info(checkpoint, #state{cref = undefined} = State) ->
    % Start an asyncronous checkpoint process so we don't block the channel
    #state{waiting = Waiting, active = Active, starting = Starting} = State,
    Args = [Waiting, Active, Starting],
    {_, Ref} = spawn_monitor(smoosh_persist, persist, Args),
    schedule_checkpoint(),
    {noreply, State#state{cref = Ref}};
handle_info(unpause, State) ->
    {noreply, maybe_start_compaction(State#state{paused = false})}.

% private functions.

unpersist(Name) ->
    % Insert into the access table with a current
    % timestamp to prevent the same dbs from being re-enqueued
    % again after startup
    Waiting = smoosh_persist:unpersist(Name),
    MapFun = fun(Object, _Priority) ->
        smoosh_server:update_access(Object)
    end,
    maps:map(MapFun, smoosh_priority_queue:to_map(Waiting)),
    Waiting.

% Periodically cache in status ets table to avoid having to block on gen_server
% get_status() calls.
%
set_status(#state{} = State) ->
    #state{active = Active, starting = Starting, waiting = Waiting} = State,
    Status = #{
        active => map_size(Active),
        starting => map_size(Starting),
        waiting => smoosh_priority_queue:info(Waiting)
    },
    true = ets:insert(State#state.stab, {status, Status}),
    State.

add_to_queue(Key, Priority, State) ->
    #state{name = Name, active = Active, waiting = Q} = State,
    case is_map_key(Key, Active) of
        true ->
            State;
        false ->
            Capacity = smoosh_utils:capacity(State#state.name),
            Level = smoosh_utils:log_level("compaction_log_level", "notice"),
            LogMsg = "~s: enqueueing ~p to compact with priority ~p",
            LogArgs = [Name, Key, Priority],
            couch_log:Level(LogMsg, LogArgs),
            Q1 = smoosh_priority_queue:in(Key, Priority, Capacity, Q),
            State#state{waiting = Q1}
    end.

maybe_start_compaction(#state{paused = true} = State) ->
    State;
maybe_start_compaction(#state{paused = false, name = Name} = State) ->
    Concurrency = smoosh_utils:concurrency(Name),
    maybe_start_compaction(Concurrency, State).

maybe_start_compaction(Concurrency, #state{active = A, starting = S} = State) when
    map_size(A) + map_size(S) >= Concurrency
->
    State;
maybe_start_compaction(Concurrency, #state{} = State) ->
    case smoosh_priority_queue:out(State#state.waiting) of
        false ->
            State;
        {Key, Q} ->
            State1 = State#state{waiting = Q},
            % Re-check priority since by the time the object was in queue, or
            % was un-persisted after a node was down, the db or ddoc might be
            % long gone and we don't want to crash the channel attemping to
            % compact it
            State2 =
                case priority(State1, Key) of
                    0 -> State1;
                    _ -> try_compact(State1, Key)
                end,
            maybe_start_compaction(Concurrency, State2)
    end.

priority(#state{name = Name}, Key) ->
    try
        smoosh_server:get_priority(Name, Key)
    catch
        Tag:Error ->
            % We are being defensive as we don't want to crash the channel
            Level = smoosh_utils:log_level("compaction_log_level", "notice"),
            LogMsg = "~s: Failed to get priority for ~s in queue ~p:~p",
            LogArgs = [Name, smoosh_utils:stringify(Key), Tag, Error],
            couch_log:Level(LogMsg, LogArgs),
            0
    end.

try_compact(#state{name = Name} = State, Key) ->
    try start_compact(State, Key) of
        false ->
            State;
        #state{} = State1 ->
            Level = smoosh_utils:log_level("compaction_log_level", "notice"),
            LogMsg = "~s: Starting compaction for ~s",
            LogArgs = [Name, smoosh_utils:stringify(Key)],
            couch_log:Level(LogMsg, LogArgs),
            State1
    catch
        Class:Exception ->
            LogArgs = [Name, Class, Exception, smoosh_utils:stringify(Key)],
            couch_log:warning("~s: compaction error ~p:~p for ~s", LogArgs),
            State
    end.

start_compact(#state{} = State, {?INDEX_CLEANUP, DbName} = Key) ->
    #state{name = Name, active = Active} = State,
    case smoosh_utils:ignore_db(DbName) of
        false ->
            {Pid, _Ref} = spawn_monitor(fun() -> cleanup_index_files(DbName) end),
            Level = smoosh_utils:log_level("compaction_log_level", "notice"),
            LogMsg = "~s: Starting index cleanup for ~s",
            LogArgs = [Name, smoosh_utils:stringify(Key)],
            couch_log:Level(LogMsg, LogArgs),
            State#state{active = Active#{Key => Pid}};
        _ ->
            false
    end;
start_compact(#state{name = Name} = State, DbName) when is_binary(DbName) ->
    case couch_db:open_int(DbName, []) of
        {ok, Db} ->
            try
                start_compact(State, Db)
            after
                couch_db:close(Db)
            end;
        Error = {not_found, no_db_file} ->
            LogMsg = "~s : Error starting compaction for ~p: ~p",
            LogArgs = [Name, smoosh_utils:stringify(DbName), Error],
            couch_log:warning(LogMsg, LogArgs),
            false
    end;
start_compact(#state{} = State, {Shard, GroupId} = Key) ->
    #state{name = Name, starting = Starting} = State,
    case smoosh_utils:ignore_db({Shard, GroupId}) of
        false ->
            case couch_index_server:get_index(couch_mrview_index, Shard, GroupId) of
                {ok, Pid} ->
                    schedule_cleanup_index_files(Shard),
                    Ref = erlang:monitor(process, Pid),
                    Pid ! {'$gen_call', {self(), Ref}, compact},
                    State#state{starting = Starting#{Ref => Key}};
                Error ->
                    LogMsg = "~s : Error starting view compaction for ~p: ~p",
                    LogArgs = [Name, smoosh_utils:stringify(Key), Error],
                    couch_log:warning(LogMsg, LogArgs),
                    false
            end;
        _ ->
            false
    end;
start_compact(#state{} = State, Db) ->
    #state{name = Name, starting = Starting, active = Active} = State,
    Key = couch_db:name(Db),
    case smoosh_utils:ignore_db(Key) of
        false ->
            case couch_db:get_compactor_pid(Db) of
                nil ->
                    DbPid = couch_db:get_pid(Db),
                    Ref = erlang:monitor(process, DbPid),
                    DbPid ! {'$gen_call', {self(), Ref}, start_compact},
                    State#state{starting = Starting#{Ref => Key}};
                % Compaction is already running, so monitor existing compaction pid.
                CPid when is_pid(CPid) ->
                    erlang:monitor(process, CPid),
                    Level = smoosh_utils:log_level("compaction_log_level", "notice"),
                    LogMsg = "~s : db ~s continuing compaction",
                    LogArgs = [Name, smoosh_utils:stringify(Key)],
                    couch_log:Level(LogMsg, LogArgs),
                    State#state{active = Active#{Key => CPid}}
            end;
        _ ->
            false
    end.

maybe_remonitor_cpid(#state{} = State, DbName, Reason) when is_binary(DbName) ->
    #state{name = Name, active = Active} = State,
    case couch_db:open_int(DbName, []) of
        {ok, Db} ->
            try couch_db:get_compactor_pid_sync(Db) of
                nil ->
                    LogMsg = "~s : exit for compaction of ~p: ~p",
                    LogArgs = [Name, smoosh_utils:stringify(DbName), Reason],
                    couch_log:warning(LogMsg, LogArgs),
                    re_enqueue(DbName),
                    State;
                CPid when is_pid(CPid) ->
                    erlang:monitor(process, CPid),
                    Level = smoosh_utils:log_level("compaction_log_level", "notice"),
                    LogMsg = "~s: ~s compaction already running. Re-monitor Pid ~p",
                    LogArgs = [Name, smoosh_utils:stringify(DbName), CPid],
                    couch_log:Level(LogMsg, LogArgs),
                    State#state{active = Active#{DbName => CPid}}
            catch
                _:Error ->
                    LogMsg = "~s: error remonitoring db compaction ~p error:~p",
                    LogArgs = [Name, smoosh_utils:stringify(DbName), Error],
                    couch_log:warning(LogMsg, LogArgs),
                    re_enqueue(DbName),
                    State
            end;
        Error = {not_found, no_db_file} ->
            LogMsg = "~s : exit for compaction of ~p: ~p",
            LogArgs = [Name, smoosh_utils:stringify(DbName), Error],
            couch_log:warning(LogMsg, LogArgs),
            State
    end;
% not a database compaction, so ignore the pid check
maybe_remonitor_cpid(#state{name = Name} = State, Key, Reason) ->
    LogMsg = "~s: exit for compaction of ~p: ~p",
    LogArgs = [Name, smoosh_utils:stringify(Key), Reason],
    couch_log:warning(LogMsg, LogArgs),
    re_enqueue(Key),
    State.

schedule_check_window() ->
    erlang:send_after(?TIME_WINDOW_MSEC, self(), check_window).

schedule_update_status() ->
    erlang:send_after(?STATUS_UPDATE_INTERVAL_MSEC, self(), update_status).

schedule_unpause() ->
    WaitSecs = config:get_integer("smoosh", "wait_secs", 30),
    erlang:send_after(WaitSecs * 1000, self(), unpause).

schedule_checkpoint() ->
    erlang:send_after(?CHECKPOINT_INTERVAL_MSEC, self(), checkpoint).

re_enqueue(Obj) ->
    case whereis(smoosh_server) of
        Pid when is_pid(Pid) ->
            Cast = {'$gen_cast', {enqueue, Obj}},
            erlang:send_after(?RE_ENQUEUE_INTERVAL, Pid, Cast),
            ok;
        _ ->
            ok
    end.

cleanup_index_files(DbName) ->
    case should_clean_up_indices() of
        true -> fabric:cleanup_index_files(DbName);
        false -> ok
    end.

schedule_cleanup_index_files(Shard) ->
    case should_clean_up_indices() of
        true ->
            % Since cleanup is at the cluster level, schedule it with a chance
            % inversely proportional to the number of local shards
            DbName = mem3:dbname(Shard),
            try length(mem3:local_shards(DbName)) of
                ShardCount when ShardCount >= 1 ->
                    case rand:uniform() < (1 / ShardCount) of
                        true ->
                            Arg = {?INDEX_CLEANUP, DbName},
                            smoosh_server:enqueue(Arg);
                        false ->
                            ok
                    end;
                _ ->
                    ok
            catch
                error:database_does_not_exist ->
                    ok
            end;
        false ->
            ok
    end.

should_clean_up_indices() ->
    config:get_boolean("smoosh", "cleanup_index_files", true).

do_suspend(#state{active = Active} = State) ->
    [suspend_pid(Pid) || Pid <- maps:values(Active)],
    State#state{paused = true}.

do_resume(#state{active = Active} = State) ->
    [resume_pid(Pid) || Pid <- maps:values(Active)],
    State#state{paused = false}.

suspend_pid(Pid) when is_pid(Pid) ->
    catch erlang:suspend_process(Pid, [unless_suspending]).

resume_pid(Pid) when is_pid(Pid) ->
    catch erlang:resume_process(Pid).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

start_compact_errors_test_() ->
    {
        foreach,
        fun setup_purge_seq/0,
        fun teardown_purge_seq/1,
        [
            ?TDEF_FE(t_start_db_with_missing_db),
            ?TDEF_FE(t_start_view_with_missing_db),
            ?TDEF_FE(t_start_view_with_missing_index),
            ?TDEF_FE(t_start_compact_throws)
        ]
    }.

setup_purge_seq() ->
    meck:new(couch_log, [passthrough]),
    meck:new(couch_db, [passthrough]),
    meck:new(smoosh_utils, [passthrough]),
    Ctx = test_util:start_couch(),
    DbName = ?tempdb(),
    {ok, Db} = couch_server:create(DbName, []),
    couch_db:close(Db),
    {Ctx, DbName}.

teardown_purge_seq({Ctx, DbName}) ->
    couch_server:delete(DbName, []),
    test_util:stop_couch(Ctx),
    meck:unload().

t_start_db_with_missing_db({_, _}) ->
    State = #state{name = "ratio_dbs"},
    meck:reset(couch_log),
    try_compact(State, <<"missing_db">>),
    ?assertEqual(1, meck:num_calls(couch_log, warning, 2)).

t_start_view_with_missing_db({_, _}) ->
    State = #state{name = "ratio_views"},
    meck:reset(couch_log),
    try_compact(State, {<<"missing_db">>, <<"_design/nope">>}),
    ?assertEqual(1, meck:num_calls(couch_log, warning, 2)).

t_start_view_with_missing_index({_, DbName}) ->
    State = #state{name = "ratio_views"},
    meck:reset(couch_log),
    try_compact(State, {DbName, <<"_design/nope">>}),
    ?assertEqual(1, meck:num_calls(couch_log, warning, 2)).

t_start_compact_throws({_, _}) ->
    State = #state{name = "ratio_dbs"},
    % Make something explode inside start_compact, so pick smoosh_util:ignore/1
    meck:expect(smoosh_utils, ignore_db, 1, meck:raise(error, foo)),
    meck:reset(couch_log),
    try_compact(State, {<<"some_db">>, <<"_design/some_view">>}),
    ?assertEqual(1, meck:num_calls(couch_log, warning, 2)).

-endif.
