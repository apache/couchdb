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

-module(smoosh_server).
-behaviour(gen_server).
-behaviour(config_listener).

% public api.
-export([
    start_link/0,
    suspend/0,
    resume/0,
    flush/0,
    enqueue/1,
    sync_enqueue/1,
    handle_db_event/3,
    status/0
]).

% gen_server api.
-export([
    init/1,
    handle_continue/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

% config_listener api
-export([handle_config_change/5, handle_config_terminate/3]).

% exported but for internal use.
-export([
    enqueue_request/2,
    get_priority/2,
    update_access/1,
    access_cleaner/0
]).

-ifdef(TEST).
-define(STALENESS_MIN, 0).
-define(ACCEESS_CLEAN_INTERVAL_MSEC, 300).
-define(RELISTEN_DELAY, 50).
-else.
-define(STALENESS_MIN, 5).
-define(ACCEESS_CLEAN_INTERVAL_MSEC, 3000).
-define(RELISTEN_DELAY, 5000).
-endif.

-define(INDEX_CLEANUP, index_cleanup).
-define(ACCESS, smoosh_access).
-define(ACCESS_MAX_SIZE, 250000).
-define(ACCESS_NEVER, -1 bsl 58).

% private records.

-record(state, {
    db_channels = [],
    view_channels = [],
    cleanup_channels = [],
    event_listener,
    waiting = #{},
    waiting_by_ref = #{},
    access_cleaner
}).

-record(channel, {
    name,
    pid,
    stab
}).

% public functions.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

suspend() ->
    gen_server:call(?MODULE, suspend, infinity).

resume() ->
    gen_server:call(?MODULE, resume, infinity).

flush() ->
    gen_server:call(?MODULE, flush, infinity).

status() ->
    ChannelsStatus =
        try ets:foldl(fun get_channel_status/2, #{}, ?MODULE) of
            Res -> Res
        catch
            error:badarg ->
                #{}
        end,
    {ok, #{channels => ChannelsStatus}}.

enqueue(Object0) ->
    Object = smoosh_utils:validate_arg(Object0),
    case stale_enough(Object) of
        true -> gen_server:cast(?MODULE, {enqueue, Object});
        false -> ok
    end.

sync_enqueue(Object0) ->
    Object = smoosh_utils:validate_arg(Object0),
    case stale_enough(Object) of
        true -> gen_server:call(?MODULE, {enqueue, Object}, infinity);
        false -> ok
    end.

handle_db_event(DbName, local_updated, St) ->
    enqueue(DbName),
    {ok, St};
handle_db_event(DbName, updated, St) ->
    enqueue(DbName),
    {ok, St};
handle_db_event(DbName, {index_commit, IdxName}, St) ->
    enqueue({DbName, IdxName}),
    {ok, St};
handle_db_event(DbName, {index_collator_upgrade, IdxName}, St) ->
    enqueue({DbName, IdxName}),
    {ok, St};
handle_db_event(_DbName, _Event, St) ->
    {ok, St}.

% gen_server functions.

init([]) ->
    process_flag(trap_exit, true),
    process_flag(message_queue_data, off_heap),
    ok = config:listen_for_changes(?MODULE, nil),
    Opts = [named_table, {read_concurrency, true}],
    ets:new(?MODULE, Opts ++ [{keypos, #channel.name}]),
    ets:new(?ACCESS, Opts ++ [{write_concurrency, true}, public]),
    State = #state{
        access_cleaner = spawn_link(?MODULE, access_cleaner, []),
        db_channels = smoosh_utils:db_channels(),
        view_channels = smoosh_utils:view_channels(),
        cleanup_channels = smoosh_utils:cleanup_channels()
    },
    {ok, State, {continue, create_channels}}.

handle_config_change("smoosh", "db_channels", _, _, _) ->
    {ok, gen_server:cast(?MODULE, new_db_channels)};
handle_config_change("smoosh", "view_channels", _, _, _) ->
    {ok, gen_server:cast(?MODULE, new_view_channels)};
handle_config_change("smoosh", "cleanup_channels", _, _, _) ->
    {ok, gen_server:cast(?MODULE, new_cleanup_channels)};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

handle_config_terminate(_Server, stop, _State) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    erlang:send_after(
        ?RELISTEN_DELAY,
        whereis(?MODULE),
        restart_config_listener
    ).

handle_continue(create_channels, #state{} = State) ->
    % Warn users about smoosh persistence misconfiguration issues. Do it once
    % on startup to avoid continuously spamming logs with errors.
    smoosh_persist:check_setup(),
    State1 = create_missing_channels(State),
    {ok, Pid} = start_event_listener(),
    {noreply, State1#state{event_listener = Pid}}.

handle_call({enqueue, Object}, _From, State) ->
    {noreply, NewState} = handle_cast({enqueue, Object}, State),
    {reply, ok, NewState};
handle_call(suspend, _From, State) ->
    Fun = fun(#channel{name = Name, pid = P}, _) ->
        Level = smoosh_utils:log_level("compaction_log_level", "debug"),
        couch_log:Level("Suspending ~p", [Name]),
        smoosh_channel:suspend(P)
    end,
    ets:foldl(Fun, ok, ?MODULE),
    {reply, ok, State};
handle_call(resume, _From, State) ->
    Fun = fun(#channel{name = Name, pid = P}, _) ->
        Level = smoosh_utils:log_level("compaction_log_level", "debug"),
        couch_log:Level("Resuming ~p", [Name]),
        smoosh_channel:resume(P)
    end,
    ets:foldl(Fun, ok, ?MODULE),
    {reply, ok, State};
handle_call(flush, _From, State) ->
    Fun = fun(#channel{pid = P}, _) -> smoosh_channel:flush(P) end,
    ets:foldl(Fun, ok, ?MODULE),
    {reply, ok, State}.

handle_cast(new_db_channels, #state{} = State) ->
    Channels = smoosh_utils:db_channels(),
    Closed = State#state.db_channels -- Channels,
    [smoosh_channel:close(channel_pid(C)) || C <- Closed],
    State1 = State#state{db_channels = Channels},
    {noreply, create_missing_channels(State1)};
handle_cast(new_view_channels, #state{} = State) ->
    Channels = smoosh_utils:view_channels(),
    Closed = State#state.view_channels -- Channels,
    [smoosh_channel:close(channel_pid(C)) || C <- Closed],
    State1 = State#state{view_channels = Channels},
    {noreply, create_missing_channels(State1)};
handle_cast(new_cleanup_channels, #state{} = State) ->
    Channels = smoosh_utils:cleanup_channels(),
    Closed = State#state.cleanup_channels -- Channels,
    [smoosh_channel:close(channel_pid(C)) || C <- Closed],
    State1 = State#state{cleanup_channels = Channels},
    {noreply, create_missing_channels(State1)};
handle_cast({enqueue, Object}, #state{waiting = Waiting} = State) ->
    case is_map_key(Object, Waiting) of
        true ->
            {noreply, State};
        false ->
            {_Pid, Ref} = spawn_monitor(?MODULE, enqueue_request, [State, Object]),
            {noreply, add_enqueue_ref(Object, Ref, State)}
    end.

handle_info({'EXIT', Pid, Reason}, #state{event_listener = Pid} = State) ->
    Level = smoosh_utils:log_level("compaction_log_level", "notice"),
    couch_log:Level("update notifier died ~p", [Reason]),
    {ok, Pid1} = start_event_listener(),
    {noreply, State#state{event_listener = Pid1}};
handle_info({'EXIT', Pid, Reason}, #state{access_cleaner = Pid} = State) ->
    Level = smoosh_utils:log_level("compaction_log_level", "notice"),
    couch_log:Level("access cleaner died ~p", [Reason]),
    Pid1 = spawn_link(?MODULE, access_cleaner, []),
    {noreply, State#state{access_cleaner = Pid1}};
handle_info({'EXIT', Pid, Reason}, State) ->
    Level = smoosh_utils:log_level("compaction_log_level", "notice"),
    couch_log:Level("~p ~p died ~p", [?MODULE, Pid, Reason]),
    case ets:match_object(?MODULE, #channel{pid = Pid, _ = '_'}) of
        [#channel{name = Name}] ->
            ets:delete(?MODULE, Name);
        _ ->
            ok
    end,
    {noreply, create_missing_channels(State)};
handle_info({'DOWN', Ref, process, _, _}, #state{} = State) ->
    {noreply, remove_enqueue_ref(Ref, State)};
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{access_cleaner = CPid}) ->
    catch unlink(CPid),
    exit(CPid, kill),
    Fun = fun(#channel{pid = P}, _) ->
        smoosh_channel:close(P)
    end,
    ets:foldl(Fun, ok, ?MODULE).

update_access(Object) ->
    Now = erlang:monotonic_time(second),
    true = ets:insert(?ACCESS, {Object, Now}),
    ok.

% private functions.

add_enqueue_ref(Object, Ref, #state{} = State) when is_reference(Ref) ->
    #state{waiting = Waiting, waiting_by_ref = WaitingByRef} = State,
    Waiting1 = Waiting#{Object => Ref},
    WaitingByRef1 = WaitingByRef#{Ref => Object},
    State#state{waiting = Waiting1, waiting_by_ref = WaitingByRef1}.

remove_enqueue_ref(Ref, #state{} = State) when is_reference(Ref) ->
    #state{waiting = Waiting, waiting_by_ref = WaitingByRef} = State,
    {Object, WaitingByRef1} = maps:take(Ref, WaitingByRef),
    {Ref, Waiting1} = maps:take(Object, Waiting),
    State#state{waiting = Waiting1, waiting_by_ref = WaitingByRef1}.

get_channel_status(#channel{name = Name, stab = Tab}, Acc) ->
    Status = smoosh_channel:get_status(Tab),
    Acc#{list_to_atom(Name) => Status};
get_channel_status(_, Acc) ->
    Acc.

start_event_listener() ->
    couch_event:link_listener(?MODULE, handle_db_event, nil, [all_dbs]).

enqueue_request(State, Object) ->
    try
        case find_channel(State, Object) of
            false ->
                ok;
            {ok, Pid, Priority} ->
                case ets:info(?ACCESS, size) of
                    Size when Size =< ?ACCESS_MAX_SIZE ->
                        ok = update_access(Object);
                    _ ->
                        ok
                end,
                QuantizedPriority = quantize(Priority),
                smoosh_channel:enqueue(Pid, Object, QuantizedPriority)
        end
    catch
        Tag:Exception:Stack ->
            Args = [?MODULE, Tag, Exception, smoosh_utils:stringify(Object), Stack],
            couch_log:warning("~s: ~p ~p for ~s : ~p", Args),
            ok
    end.

find_channel(#state{} = State, {?INDEX_CLEANUP, DbName}) ->
    find_channel(State, State#state.cleanup_channels, {?INDEX_CLEANUP, DbName});
find_channel(#state{} = State, {Shard, GroupId}) when is_binary(Shard) ->
    find_channel(State, State#state.view_channels, {Shard, GroupId});
find_channel(#state{} = State, DbName) ->
    find_channel(State, State#state.db_channels, DbName).

find_channel(#state{} = _State, [], _Object) ->
    false;
find_channel(#state{} = State, [Channel | Rest], Object) ->
    case stale_enough(Object) of
        true ->
            case smoosh_utils:ignore_db(Object) of
                true ->
                    find_channel(State, Rest, Object);
                _ ->
                    case get_priority(Channel, Object) of
                        0 ->
                            find_channel(State, Rest, Object);
                        Priority ->
                            {ok, channel_pid(Channel), Priority}
                    end
            end;
        false ->
            find_channel(State, Rest, Object)
    end.

stale_enough(Object) ->
    LastUpdatedSec = last_updated(Object),
    Staleness = erlang:monotonic_time(second) - LastUpdatedSec,
    Staleness >= min_staleness_sec().

channel_pid(Channel) ->
    [#channel{pid = Pid}] = ets:lookup(?MODULE, Channel),
    Pid.

create_missing_channels(#state{} = State) ->
    create_missing_channels_type(State#state.db_channels),
    create_missing_channels_type(State#state.view_channels),
    create_missing_channels_type(State#state.cleanup_channels),
    State.

create_missing_channels_type([]) ->
    ok;
create_missing_channels_type([Channel | Rest]) ->
    case ets:lookup(?MODULE, Channel) of
        [] ->
            {ok, Pid} = smoosh_channel:start_link(Channel),
            {ok, STab} = smoosh_channel:get_status_table(Pid),
            Chan = #channel{
                name = Channel,
                pid = Pid,
                stab = STab
            },
            true = ets:insert(?MODULE, Chan);
        _ ->
            ok
    end,
    create_missing_channels_type(Rest).

get_priority(_Channel, {?INDEX_CLEANUP, DbName}) ->
    try mem3:local_shards(mem3:dbname(DbName)) of
        [_ | _] -> 1;
        [] -> 0
    catch
        error:database_does_not_exist ->
            0
    end;
get_priority(Channel, {Shard, GroupId}) ->
    try couch_index_server:get_index(couch_mrview_index, Shard, GroupId) of
        {ok, Pid} ->
            try
                {ok, ViewInfo} = couch_index:get_info(Pid),
                {SizeInfo} = couch_util:get_value(sizes, ViewInfo),
                DiskSize = couch_util:get_value(file, SizeInfo),
                ActiveSize = couch_util:get_value(active, SizeInfo),
                NeedsUpgrade = needs_upgrade(ViewInfo),
                get_priority(Channel, DiskSize, ActiveSize, NeedsUpgrade)
            catch
                exit:{timeout, _} ->
                    0
            end;
        {not_found, _Reason} ->
            0;
        {database_does_not_exist, _Stack} ->
            0;
        {error, Reason} ->
            couch_log:warning(
                "Failed to get group_pid for ~p ~p ~p: ~p",
                [Channel, Shard, GroupId, Reason]
            ),
            0
    catch
        throw:{not_found, _} ->
            0
    end;
get_priority(Channel, DbName) when is_binary(DbName) ->
    case couch_db:open_int(DbName, []) of
        {ok, Db} ->
            try
                get_priority(Channel, Db)
            after
                couch_db:close(Db)
            end;
        {not_found, no_db_file} ->
            % It's expected that a db might be deleted while waiting in queue
            0
    end;
get_priority(Channel, Db) ->
    {ok, DocInfo} = couch_db:get_db_info(Db),
    {SizeInfo} = couch_util:get_value(sizes, DocInfo),
    DiskSize = couch_util:get_value(file, SizeInfo),
    ActiveSize = couch_util:get_value(active, SizeInfo),
    NeedsUpgrade = needs_upgrade(DocInfo),
    case db_changed(Channel, DocInfo) of
        true -> get_priority(Channel, DiskSize, ActiveSize, NeedsUpgrade);
        false -> 0
    end.

get_priority(Channel, DiskSize, DataSize, NeedsUpgrade) ->
    Priority = get_priority(Channel),
    MinSize = to_number(Channel, "min_size", "1048576"),
    MaxSize = to_number(Channel, "max_size", "infinity"),
    DefaultMinPriority =
        case Priority of
            "slack" -> "536870912";
            _ -> "2.0"
        end,
    MinPriority = to_number(Channel, "min_priority", DefaultMinPriority),
    MaxPriority = to_number(Channel, "max_priority", "infinity"),
    if
        Priority =:= "upgrade", NeedsUpgrade ->
            1;
        DiskSize =< MinSize ->
            0;
        DiskSize > MaxSize ->
            0;
        DataSize =:= 0 ->
            MinPriority;
        Priority =:= "ratio", DiskSize / DataSize =< MinPriority ->
            0;
        Priority =:= "ratio", DiskSize / DataSize > MaxPriority ->
            0;
        Priority =:= "ratio" ->
            DiskSize / DataSize;
        Priority =:= "slack", DiskSize - DataSize =< MinPriority ->
            0;
        Priority =:= "slack", DiskSize - DataSize > MaxPriority ->
            0;
        Priority =:= "slack" ->
            DiskSize - DataSize;
        true ->
            0
    end.

db_changed(Channel, Info) ->
    case couch_util:get_value(compacted_seq, Info) of
        undefined ->
            true;
        CompactedSeq ->
            MinChanges = list_to_integer(
                smoosh_utils:get(Channel, "min_changes", "0")
            ),
            UpdateSeq = couch_util:get_value(update_seq, Info),
            UpdateSeq - CompactedSeq >= MinChanges
    end.

to_number(Channel, Name, Default) ->
    case smoosh_utils:get(Channel, Name, Default) of
        "infinity" ->
            infinity;
        Value ->
            try
                list_to_float(Value)
            catch
                error:badarg ->
                    list_to_integer(Value)
            end
    end.

get_priority("ratio_dbs") ->
    "ratio";
get_priority("ratio_views") ->
    "ratio";
get_priority("slack_dbs") ->
    "slack";
get_priority("slack_views") ->
    "slack";
get_priority("upgrade_dbs") ->
    "upgrade";
get_priority("upgrade_views") ->
    "upgrade";
get_priority(Channel) ->
    smoosh_utils:get(Channel, "priority", "ratio").

needs_upgrade(Props) ->
    db_needs_upgrade(Props) orelse view_needs_upgrade(Props).

db_needs_upgrade(Props) ->
    DiskVersion = couch_util:get_value(disk_format_version, Props),
    case couch_util:get_value(engine, Props) of
        couch_bt_engine ->
            (couch_bt_engine_header:latest(DiskVersion) =:= false);
        _ ->
            false
    end.

view_needs_upgrade(Props) ->
    case couch_util:get_value(collator_versions, Props) of
        undefined ->
            false;
        Versions when is_list(Versions) ->
            Enabled = couch_mrview_util:compact_on_collator_upgrade(),
            Enabled andalso length(Versions) >= 2
    end.

access_cleaner() ->
    JitterMSec = rand:uniform(?ACCEESS_CLEAN_INTERVAL_MSEC),
    timer:sleep(?ACCEESS_CLEAN_INTERVAL_MSEC + JitterMSec),
    NowSec = erlang:monotonic_time(second),
    Limit = NowSec - min_staleness_sec(),
    Head = {'_', '$1'},
    Guard = {'<', '$1', Limit},
    ets:select_delete(?ACCESS, [{Head, [Guard], [true]}]),
    access_cleaner().

min_staleness_sec() ->
    Min = config:get_integer("smoosh", "staleness", ?STALENESS_MIN),
    Min * 60.

last_updated(Object) ->
    try ets:lookup(?ACCESS, Object) of
        [{_, AccessSec}] ->
            AccessSec;
        [] ->
            ?ACCESS_NEVER
    catch
        error:badarg ->
            0
    end.

quantize(Ratio) when is_integer(Ratio) ->
    Ratio;
quantize(Ratio) when is_float(Ratio), Ratio >= 16 ->
    round(Ratio);
quantize(Ratio) when is_float(Ratio) ->
    round(Ratio * 16) / 16.

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

setup_all() ->
    Ctx = setup_all_no_mock(),
    Pid = list_to_pid("<0.0.0>"),
    meck:expect(couch_index_server, get_index, 3, {ok, Pid}),
    meck:expect(config, get, fun(_, _, Default) -> Default end),
    Ctx.

setup_all_no_mock() ->
    Ctx = test_util:start_couch([couch_log]),
    meck:new([config, couch_index, couch_index_server], [passthrough]),
    meck:expect(config, get, fun(_, _, Default) -> Default end),
    Ctx.

teardown_all(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).

setup() ->
    Shard = <<"shards/00000000-1fffffff/test.1529510412">>,
    GroupId = <<"_design/ddoc">>,
    {ok, Shard, GroupId}.

teardown(_) ->
    ok.

config_change_test_() ->
    {
        "Test config updates",
        {
            foreach,
            fun() -> test_util:start_couch([smoosh]) end,
            fun test_util:stop_couch/1,
            [
                ?TDEF_FE(t_restart_config_listener)
            ]
        }
    }.

get_priority_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_ratio_view),
                ?TDEF_FE(t_slack_view),
                ?TDEF_FE(t_no_data_view),
                ?TDEF_FE(t_below_min_priority_view),
                ?TDEF_FE(t_below_min_size_view),
                ?TDEF_FE(t_timeout_view),
                ?TDEF_FE(t_missing_view),
                ?TDEF_FE(t_invalid_view)
            ]
        }
    }.

get_priority_no_mock_test_() ->
    {
        setup,
        fun setup_all_no_mock/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_missing_db)
            ]
        }
    }.

t_restart_config_listener(_) ->
    ConfigMonitor = config_listener_mon(),
    ?assert(is_process_alive(ConfigMonitor)),
    test_util:stop_sync(ConfigMonitor),
    ?assertNot(is_process_alive(ConfigMonitor)),
    NewConfigMonitor = test_util:wait(fun() ->
        case config_listener_mon() of
            undefined -> wait;
            Pid -> Pid
        end
    end),
    ?assert(is_process_alive(NewConfigMonitor)).

t_ratio_view({ok, Shard, GroupId}) ->
    meck:expect(couch_index, get_info, fun(_) ->
        {ok, [{sizes, {[{file, 5242880}, {active, 524288}]}}]}
    end),
    ?assertEqual(10.0, get_priority("ratio_views", {Shard, GroupId})),
    ?assertEqual(0, get_priority("slack_views", {Shard, GroupId})),
    ?assertEqual(0, get_priority("upgrade_views", {Shard, GroupId})).

t_slack_view({ok, Shard, GroupId}) ->
    meck:expect(couch_index, get_info, fun(_) ->
        {ok, [{sizes, {[{file, 1073741824}, {active, 536870911}]}}]}
    end),
    ?assertEqual(2.0000000037252903, get_priority("ratio_views", {Shard, GroupId})),
    ?assertEqual(536870913, get_priority("slack_views", {Shard, GroupId})),
    ?assertEqual(0, get_priority("upgrade_views", {Shard, GroupId})).

t_no_data_view({ok, Shard, GroupId}) ->
    meck:expect(couch_index, get_info, fun(_) ->
        {ok, [{sizes, {[{file, 5242880}, {active, 0}]}}]}
    end),
    ?assertEqual(2.0, get_priority("ratio_views", {Shard, GroupId})),
    ?assertEqual(536870912, get_priority("slack_views", {Shard, GroupId})),
    ?assertEqual(2.0, get_priority("upgrade_views", {Shard, GroupId})).

t_below_min_priority_view({ok, Shard, GroupId}) ->
    meck:expect(couch_index, get_info, fun(_) ->
        {ok, [{sizes, {[{file, 5242880}, {active, 1048576}]}}]}
    end),
    ?assertEqual(5.0, get_priority("ratio_views", {Shard, GroupId})),
    ?assertEqual(0, get_priority("slack_views", {Shard, GroupId})),
    ?assertEqual(0, get_priority("upgrade_views", {Shard, GroupId})).

t_below_min_size_view({ok, Shard, GroupId}) ->
    meck:expect(couch_index, get_info, fun(_) ->
        {ok, [{sizes, {[{file, 1048576}, {active, 512000}]}}]}
    end),
    ?assertEqual(0, get_priority("ratio_views", {Shard, GroupId})),
    ?assertEqual(0, get_priority("slack_views", {Shard, GroupId})),
    ?assertEqual(0, get_priority("upgrade_views", {Shard, GroupId})).

t_timeout_view({ok, Shard, GroupId}) ->
    meck:expect(couch_index, get_info, fun(_) ->
        exit({timeout, get_info})
    end),
    ?assertEqual(0, get_priority("ratio_views", {Shard, GroupId})),
    ?assertEqual(0, get_priority("slack_views", {Shard, GroupId})),
    ?assertEqual(0, get_priority("upgrade_views", {Shard, GroupId})).

t_missing_db(_) ->
    ShardGroup = {<<"shards/80000000-ffffffff/db2.1666895357">>, <<"x">>},
    ?assertEqual(0, get_priority("ratio_views", ShardGroup)),
    ?assertEqual(0, get_priority("slack_views", ShardGroup)),
    ?assertEqual(0, get_priority("upgrade_views", ShardGroup)).

t_missing_view({ok, Shard, GroupId}) ->
    meck:expect(couch_index_server, get_index, 3, {not_found, missing}),
    ?assertEqual(0, get_priority("ratio_views", {Shard, GroupId})),
    ?assertEqual(0, get_priority("slack_views", {Shard, GroupId})),
    ?assertEqual(0, get_priority("upgrade_views", {Shard, GroupId})).

t_invalid_view({ok, Shard, GroupId}) ->
    meck:expect(couch_index_server, get_index, 3, {error, undef}),
    ?assertEqual(0, get_priority("ratio_views", {Shard, GroupId})),
    ?assertEqual(0, get_priority("slack_views", {Shard, GroupId})),
    ?assertEqual(0, get_priority("upgrade_views", {Shard, GroupId})).

config_listener_mon() ->
    IsConfigMonitor = fun(P) ->
        [M | _] = string:tokens(couch_debug:process_name(P), ":"),
        M =:= "config_listener_mon"
    end,
    [{_, MonitoredBy}] = process_info(whereis(?MODULE), [monitored_by]),
    case lists:filter(IsConfigMonitor, MonitoredBy) of
        [Pid] -> Pid;
        [] -> undefined
    end.

add_remove_enqueue_ref_test() ->
    ObjCount = 10000,
    ObjRefs = [{I, make_ref()} || I <- lists:seq(1, ObjCount)],

    St = lists:foldl(
        fun({I, Ref}, #state{} = Acc) ->
            add_enqueue_ref(I, Ref, Acc)
        end,
        #state{},
        ObjRefs
    ),

    ?assertEqual(ObjCount, map_size(St#state.waiting)),
    ?assertEqual(ObjCount, map_size(St#state.waiting_by_ref)),

    {_Objs, Refs} = lists:unzip(ObjRefs),
    St1 = lists:foldl(
        fun(Ref, #state{} = Acc) ->
            remove_enqueue_ref(Ref, Acc)
        end,
        St,
        Refs
    ),

    % It's basically back to an initial (empty) state
    ?assertEqual(St1, #state{}).

quantize_test() ->
    ?assertEqual(0, quantize(0)),
    ?assertEqual(1, quantize(1)),
    ?assertEqual(0.0, quantize(0.0)),
    ?assertEqual(16, quantize(16.0)),
    ?assertEqual(15.0, quantize(15.0)),
    ?assertEqual(0.0, quantize(0.01)),
    ?assertEqual(0.125, quantize(0.1)),
    ?assertEqual(0.125, quantize(0.1042)),
    ?assertEqual(0.125, quantize(0.125111111111)),
    ?assertEqual(10.0, quantize(10.0002)).

-endif.
