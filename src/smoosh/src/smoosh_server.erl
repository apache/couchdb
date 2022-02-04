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
-vsn(4).
-behaviour(config_listener).
-include_lib("couch/include/couch_db.hrl").

% public api.
-export([
    start_link/0,
    suspend/0,
    resume/0,
    enqueue/1,
    sync_enqueue/1,
    sync_enqueue/2,
    handle_db_event/3,
    status/0
]).

-define(SECONDS_PER_MINUTE, 60).

% gen_server api.
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

% config_listener api
-export([handle_config_change/5, handle_config_terminate/3]).

% exported but for internal use.
-export([enqueue_request/2]).

-ifdef(TEST).
-define(RELISTEN_DELAY, 50).
-else.
-define(RELISTEN_DELAY, 5000).
-endif.

% private records.

-record(state, {
    db_channels = [],
    view_channels = [],
    schema_channels = [],
    tab,
    event_listener,
    waiting = dict:new()
}).

-record(channel, {
    name,
    pid
}).

% public functions.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

suspend() ->
    gen_server:call(?MODULE, suspend).

resume() ->
    gen_server:call(?MODULE, resume).

status() ->
    gen_server:call(?MODULE, status).

enqueue(Object) ->
    gen_server:cast(?MODULE, {enqueue, Object}).

sync_enqueue(Object) ->
    gen_server:call(?MODULE, {enqueue, Object}).

sync_enqueue(Object, Timeout) ->
    gen_server:call(?MODULE, {enqueue, Object}, Timeout).

handle_db_event(DbName, local_updated, St) ->
    smoosh_server:enqueue(DbName),
    {ok, St};
handle_db_event(DbName, updated, St) ->
    smoosh_server:enqueue(DbName),
    {ok, St};
handle_db_event(DbName, {index_commit, IdxName}, St) ->
    smoosh_server:enqueue({DbName, IdxName}),
    {ok, St};
handle_db_event(DbName, {index_collator_upgrade, IdxName}, St) ->
    smoosh_server:enqueue({DbName, IdxName}),
    {ok, St};
handle_db_event(DbName, {schema_updated, DDocId}, St) ->
    smoosh_server:enqueue({schema, DbName, DDocId}),
    {ok, St};
handle_db_event(_DbName, _Event, St) ->
    {ok, St}.

% gen_server functions.

init([]) ->
    process_flag(trap_exit, true),
    ok = config:listen_for_changes(?MODULE, nil),
    {ok, Pid} = start_event_listener(),
    DbChannels = smoosh_utils:split(
        config:get("smoosh", "db_channels", "upgrade_dbs,ratio_dbs,slack_dbs")
    ),
    ViewChannels = smoosh_utils:split(
        config:get("smoosh", "view_channels", "upgrade_views,ratio_views,slack_views")
    ),
    SchemaChannels = smoosh_utils:split(
        config:get(
            "smoosh",
            "schema_channels",
            "ratio_schemas,slack_schemas"
        )
    ),
    Tab = ets:new(channels, [{keypos, #channel.name}]),
    {ok,
        create_missing_channels(#state{
            db_channels = DbChannels,
            view_channels = ViewChannels,
            schema_channels = SchemaChannels,
            event_listener = Pid,
            tab = Tab
        })}.

handle_config_change("smoosh", "db_channels", L, _, _) ->
    {ok, gen_server:cast(?MODULE, {new_db_channels, smoosh_utils:split(L)})};
handle_config_change("smoosh", "view_channels", L, _, _) ->
    {ok, gen_server:cast(?MODULE, {new_view_channels, smoosh_utils:split(L)})};
handle_config_change("smoosh", "schema_channels", L, _, _) ->
    {ok, gen_server:cast(?MODULE, {new_schema_channels, smoosh_utils:split(L)})};
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

handle_call(status, _From, State) ->
    Acc = ets:foldl(fun get_channel_status/2, [], State#state.tab),
    {reply, {ok, Acc}, State};
handle_call({enqueue, Object}, _From, State) ->
    {noreply, NewState} = handle_cast({enqueue, Object}, State),
    {reply, ok, NewState};
handle_call(suspend, _From, State) ->
    ets:foldl(
        fun(#channel{name = Name, pid = P}, _) ->
            couch_log:notice("Suspending ~p", [Name]),
            smoosh_channel:suspend(P)
        end,
        0,
        State#state.tab
    ),
    {reply, ok, State};
handle_call(resume, _From, State) ->
    ets:foldl(
        fun(#channel{name = Name, pid = P}, _) ->
            couch_log:notice("Resuming ~p", [Name]),
            smoosh_channel:resume(P)
        end,
        0,
        State#state.tab
    ),
    {reply, ok, State}.

handle_cast({new_db_channels, Channels}, State) ->
    [
        smoosh_channel:close(channel_pid(State#state.tab, C))
     || C <- State#state.db_channels -- Channels
    ],
    {noreply, create_missing_channels(State#state{db_channels = Channels})};
handle_cast({new_view_channels, Channels}, State) ->
    [
        smoosh_channel:close(channel_pid(State#state.tab, C))
     || C <- State#state.view_channels -- Channels
    ],
    {noreply, create_missing_channels(State#state{view_channels = Channels})};
handle_cast({new_schema_channels, Channels}, State) ->
    [
        smoosh_channel:close(channel_pid(State#state.tab, C))
     || C <- State#state.schema_channels -- Channels
    ],
    {noreply, create_missing_channels(State#state{view_channels = Channels})};
handle_cast({enqueue, Object}, State) ->
    #state{waiting = Waiting} = State,
    case dict:is_key(Object, Waiting) of
        true ->
            {noreply, State};
        false ->
            {_Pid, Ref} = spawn_monitor(?MODULE, enqueue_request, [State, Object]),
            {noreply, State#state{waiting = dict:store(Object, Ref, Waiting)}}
    end.

handle_info({'EXIT', Pid, Reason}, #state{event_listener = Pid} = State) ->
    couch_log:notice("update notifier died ~p", [Reason]),
    {ok, Pid1} = start_event_listener(),
    {noreply, State#state{event_listener = Pid1}};
handle_info({'EXIT', Pid, Reason}, State) ->
    couch_log:notice("~p ~p died ~p", [?MODULE, Pid, Reason]),
    case ets:match_object(State#state.tab, #channel{pid = Pid, _ = '_'}) of
        [#channel{name = Name}] ->
            ets:delete(State#state.tab, Name);
        _ ->
            ok
    end,
    {noreply, create_missing_channels(State)};
handle_info({'DOWN', Ref, _, _, _}, State) ->
    Waiting = dict:filter(
        fun(_Key, Value) -> Value =/= Ref end,
        State#state.waiting
    ),
    {noreply, State#state{waiting = Waiting}};
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ets:foldl(
        fun(#channel{pid = P}, _) -> smoosh_channel:close(P) end,
        0,
        State#state.tab
    ),
    ok.

code_change(_OldVsn, {state, DbChannels, ViewChannels, Tab, EventListener, Waiting}, _Extra) ->
    {ok, #state{
        db_channels = DbChannels,
        view_channels = ViewChannels,
        schema_channels = [],
        tab = Tab,
        event_listener = EventListener,
        waiting = Waiting
    }};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private functions.

get_channel_status(#channel{name = Name, pid = P}, Acc0) when is_pid(P) ->
    try gen_server:call(P, status) of
        {ok, Status} ->
            [{Name, Status} | Acc0];
        _ ->
            Acc0
    catch
        _:_ ->
            Acc0
    end;
get_channel_status(_, Acc0) ->
    Acc0.

start_event_listener() ->
    couch_event:link_listener(?MODULE, handle_db_event, nil, [all_dbs]).

enqueue_request(State, Object) ->
    try
        case find_channel(State, Object) of
        false ->
            ok;
        {ok, Pid, Priority} ->
            smoosh_channel:enqueue(Pid, Object, Priority)
        end
    catch
        ?STACKTRACE(Class, Exception, Stack)
        couch_log:notice("~s: ~p ~p for ~s : ~p",
            [?MODULE, Class, Exception,
                smoosh_utils:stringify(Object), Stack])
    end.

find_channel(#state{} = State, {schema, DbName, GroupId}) ->
    find_channel(State#state.tab, State#state.schema_channels, {schema, DbName, GroupId});
find_channel(#state{} = State, {Shard, GroupId}) ->
    find_channel(State#state.tab, State#state.view_channels, {Shard, GroupId});
find_channel(#state{} = State, DbName) ->
    find_channel(State#state.tab, State#state.db_channels, DbName).

find_channel(_Tab, [], _Object) ->
    false;
find_channel(Tab, [Channel | Rest], Object) ->
    Pid = channel_pid(Tab, Channel),
    LastUpdated = smoosh_channel:last_updated(Pid, Object),
    StalenessInSec =
        config:get_integer("smoosh", "staleness", 5) *
            ?SECONDS_PER_MINUTE,
    Staleness = erlang:convert_time_unit(StalenessInSec, seconds, native),
    Now = erlang:monotonic_time(),
    case LastUpdated =:= false orelse Now - LastUpdated > Staleness of
        true ->
            case smoosh_utils:ignore_db(Object) of
                true ->
                    find_channel(Tab, Rest, Object);
                _ ->
                    case get_priority(Channel, Object) of
                        0 ->
                            find_channel(Tab, Rest, Object);
                        Priority ->
                            {ok, Pid, Priority}
                    end
            end;
        false ->
            find_channel(Tab, Rest, Object)
    end.

channel_pid(Tab, Channel) ->
    [#channel{pid = Pid}] = ets:lookup(Tab, Channel),
    Pid.

create_missing_channels(State) ->
    create_missing_channels(State#state.tab, State#state.db_channels),
    create_missing_channels(State#state.tab, State#state.view_channels),
    create_missing_channels(State#state.tab, State#state.schema_channels),
    State.

create_missing_channels(_Tab, []) ->
    ok;
create_missing_channels(Tab, [Channel | Rest]) ->
    case ets:lookup(Tab, Channel) of
        [] ->
            {ok, Pid} = smoosh_channel:start_link(Channel),
            true = ets:insert(Tab, [#channel{name = Channel, pid = Pid}]);
        _ ->
            ok
    end,
    create_missing_channels(Tab, Rest).

get_priority(Channel, {Shard, GroupId}) ->
    case couch_index_server:get_index(couch_mrview_index, Shard, GroupId) of
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
        {error, Reason} ->
            couch_log:warning(
                "Failed to get group_pid for ~p ~p ~p: ~p",
                [Channel, Shard, GroupId, Reason]
            ),
            0
    end;
get_priority(Channel, {schema, DbName, DDocId}) ->
    case couch_md_index_manager:get_group_pid(DbName, DDocId) of
        {ok, Pid} ->
            {ok, SchemaInfo} = couch_md_index:get_info(Pid),
            DiskSize = couch_util:get_value(disk_size, SchemaInfo),
            DataSize = couch_util:get_value(data_size, SchemaInfo),
            get_priority(Channel, DiskSize, DataSize, false);
        {error, Reason} ->
            couch_log:warning(
                "Failed to get group_pid for ~p ~p ~p: ~p",
                [Channel, DbName, DDocId, Reason]
            ),
            0
    end;
get_priority(Channel, DbName) when is_list(DbName) ->
    get_priority(Channel, ?l2b(DbName));
get_priority(Channel, DbName) when is_binary(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    try
        get_priority(Channel, Db)
    after
        couch_db:close(Db)
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
get_priority("ratio_schemas") ->
    "ratio";
get_priority("slack_dbs") ->
    "slack";
get_priority("slack_views") ->
    "slack";
get_priority("slack_schemas") ->
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup_all() ->
    Ctx = test_util:start_couch([couch_log]),
    meck:new([config, couch_index, couch_index_server], [passthrough]),
    Pid = list_to_pid("<0.0.0>"),
    meck:expect(couch_index_server, get_index, 3, {ok, Pid}),
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
                fun t_restart_config_listener/1
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
                fun t_ratio_view/1,
                fun t_slack_view/1,
                fun t_no_data_view/1,
                fun t_below_min_priority_view/1,
                fun t_below_min_size_view/1,
                fun t_timeout_view/1,
                fun t_missing_view/1,
                fun t_invalid_view/1
            ]
        }
    }.

t_restart_config_listener(_) ->
    ?_test(begin
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
        ?assert(is_process_alive(NewConfigMonitor))
    end).

t_ratio_view({ok, Shard, GroupId}) ->
    ?_test(begin
        meck:expect(couch_index, get_info, fun(_) ->
            {ok, [{sizes, {[{file, 5242880}, {active, 524288}]}}]}
        end),
        ?assertEqual(10.0, get_priority("ratio_views", {Shard, GroupId})),
        ?assertEqual(0, get_priority("slack_views", {Shard, GroupId})),
        ?assertEqual(0, get_priority("upgrade_views", {Shard, GroupId}))
    end).

t_slack_view({ok, Shard, GroupId}) ->
    ?_test(begin
        meck:expect(couch_index, get_info, fun(_) ->
            {ok, [{sizes, {[{file, 1073741824}, {active, 536870911}]}}]}
        end),
        ?assertEqual(2.0000000037252903, get_priority("ratio_views", {Shard, GroupId})),
        ?assertEqual(536870913, get_priority("slack_views", {Shard, GroupId})),
        ?assertEqual(0, get_priority("upgrade_views", {Shard, GroupId}))
    end).

t_no_data_view({ok, Shard, GroupId}) ->
    ?_test(begin
        meck:expect(couch_index, get_info, fun(_) ->
            {ok, [{sizes, {[{file, 5242880}, {active, 0}]}}]}
        end),
        ?assertEqual(2.0, get_priority("ratio_views", {Shard, GroupId})),
        ?assertEqual(536870912, get_priority("slack_views", {Shard, GroupId})),
        ?assertEqual(2.0, get_priority("upgrade_views", {Shard, GroupId}))
    end).

t_below_min_priority_view({ok, Shard, GroupId}) ->
    ?_test(begin
        meck:expect(couch_index, get_info, fun(_) ->
            {ok, [{sizes, {[{file, 5242880}, {active, 1048576}]}}]}
        end),
        ?assertEqual(5.0, get_priority("ratio_views", {Shard, GroupId})),
        ?assertEqual(0, get_priority("slack_views", {Shard, GroupId})),
        ?assertEqual(0, get_priority("upgrade_views", {Shard, GroupId}))
    end).

t_below_min_size_view({ok, Shard, GroupId}) ->
    ?_test(begin
        meck:expect(couch_index, get_info, fun(_) ->
            {ok, [{sizes, {[{file, 1048576}, {active, 512000}]}}]}
        end),
        ?assertEqual(0, get_priority("ratio_views", {Shard, GroupId})),
        ?assertEqual(0, get_priority("slack_views", {Shard, GroupId})),
        ?assertEqual(0, get_priority("upgrade_views", {Shard, GroupId}))
    end).

t_timeout_view({ok, Shard, GroupId}) ->
    ?_test(begin
        meck:expect(couch_index, get_info, fun(_) ->
            exit({timeout, get_info})
        end),
        ?assertEqual(0, get_priority("ratio_views", {Shard, GroupId})),
        ?assertEqual(0, get_priority("slack_views", {Shard, GroupId})),
        ?assertEqual(0, get_priority("upgrade_views", {Shard, GroupId}))
    end).

t_missing_view({ok, Shard, GroupId}) ->
    ?_test(begin
        meck:expect(couch_index_server, get_index, 3, {not_found, missing}),
        ?assertEqual(0, get_priority("ratio_views", {Shard, GroupId})),
        ?assertEqual(0, get_priority("slack_views", {Shard, GroupId})),
        ?assertEqual(0, get_priority("upgrade_views", {Shard, GroupId}))
    end).

t_invalid_view({ok, Shard, GroupId}) ->
    ?_test(begin
        meck:expect(couch_index_server, get_index, 3, {error, undef}),
        ?assertEqual(0, get_priority("ratio_views", {Shard, GroupId})),
        ?assertEqual(0, get_priority("slack_views", {Shard, GroupId})),
        ?assertEqual(0, get_priority("upgrade_views", {Shard, GroupId}))
    end).

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

-endif.
