% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(mem3_sync_event_listener).
-behavior(couch_event_listener).
-vsn(1).

-export([
    start_link/0
]).

-export([
    init/1,
    terminate/2,
    handle_event/3,
    handle_cast/2,
    handle_info/2
]).

-include_lib("mem3/include/mem3.hrl").

-ifdef(TEST).
-define(RELISTEN_DELAY, 500).
-else.
-define(RELISTEN_DELAY, 5000).
-endif.

-record(state, {
    nodes,
    shards,
    users,
    delay,
    frequency,
    last_push,
    buckets
}).

%% Calling mem3_sync:push/2 on every update has a measurable performance cost,
%% so we'd like to coalesce multiple update messages from couch_event in to a
%% single push call. Doing this while ensuring both correctness (i.e., no lost
%% updates) and an even load profile is somewhat subtle. This implementation
%% groups updated shards in a list of "buckets" (see bucket_shard/2) and
%% guarantees that each shard is in no more than one bucket at a time - i.e.,
%% any update messages received before the shard's current bucket has been
%% pushed will be ignored - thereby reducing the frequency with which a single
%% shard will be pushed. mem3_sync:push/2 is called on all shards in the
%% *oldest* bucket roughly every mem3.sync_frequency milliseconds (see
%% maybe_push_shards/1) to even out the load on mem3_sync.

start_link() ->
    couch_event_listener:start_link(?MODULE, [], [all_dbs]).

init(_) ->
    ok = subscribe_for_config(),
    Delay = config:get_integer("mem3", "sync_delay", 5000),
    Frequency = config:get_integer("mem3", "sync_frequency", 500),
    Buckets = lists:duplicate(Delay div Frequency + 1, sets:new()),
    St = #state{
        nodes = mem3_sync:nodes_db(),
        shards = mem3_sync:shards_db(),
        users = mem3_sync:users_db(),
        delay = Delay,
        frequency = Frequency,
        buckets = Buckets
    },
    {ok, St}.

terminate(_Reason, _State) ->
    ok.

handle_event(NodesDb, updated, #state{nodes = NodesDb} = St) ->
    Nodes = mem3:nodes(),
    Live = nodes(),
    [mem3_sync:push(NodesDb, N) || N <- Nodes, lists:member(N, Live)],
    maybe_push_shards(St);
handle_event(ShardsDb, updated, #state{shards = ShardsDb} = St) ->
    mem3_sync:push(ShardsDb, mem3_sync:find_next_node()),
    maybe_push_shards(St);
handle_event(UsersDb, updated, #state{users = UsersDb} = St) ->
    mem3_sync:push(UsersDb, mem3_sync:find_next_node()),
    maybe_push_shards(St);
handle_event(<<"shards/", _/binary>> = ShardName, updated, St) ->
    Buckets = bucket_shard(ShardName, St#state.buckets),
    maybe_push_shards(St#state{buckets=Buckets});
handle_event(<<"shards/", _:18/binary, _/binary>> = ShardName, deleted, St) ->
    mem3_sync:remove_shard(ShardName),
    maybe_push_shards(St);
handle_event(_DbName, _Event, St) ->
    maybe_push_shards(St).

handle_cast({set_frequency, Frequency}, St) ->
    #state{delay = Delay, buckets = Buckets0} = St,
    Buckets1 = rebucket_shards(Delay, Frequency, Buckets0),
    maybe_push_shards(St#state{frequency=Frequency, buckets=Buckets1});
handle_cast({set_delay, Delay}, St) ->
    #state{frequency = Frequency, buckets = Buckets0} = St,
    Buckets1 = rebucket_shards(Delay, Frequency, Buckets0),
    maybe_push_shards(St#state{delay=Delay, buckets=Buckets1});
handle_cast(Msg, St) ->
    couch_log:notice("unexpected cast to mem3_sync_event_listener: ~p", [Msg]),
    maybe_push_shards(St).

handle_info(timeout, St) ->
    maybe_push_shards(St);
handle_info({config_change, "mem3", "sync_delay", Value, _}, St) ->
    set_config(set_delay, Value, "ignoring bad value for mem3.sync_delay"),
    maybe_push_shards(St);
handle_info({config_change, "mem3", "sync_frequency", Value, _}, St) ->
    set_config(set_frequency, Value, "ignoring bad value for mem3.sync_frequency"),
    maybe_push_shards(St);
handle_info({gen_event_EXIT, _Handler, _Reason}, St) ->
    erlang:send_after(?RELISTEN_DELAY, self(), restart_config_listener),
    maybe_push_shards(St);
handle_info(restart_config_listener, St) ->
    ok = subscribe_for_config(),
    maybe_push_shards(St);
handle_info({get_state, Ref, Caller}, St) ->
    Caller ! {Ref, St},
    {ok, St};
handle_info(Msg, St) ->
    couch_log:notice("unexpected info to mem3_sync_event_listener: ~p", [Msg]),
    maybe_push_shards(St).

set_config(Cmd, Value, Error) ->
    try list_to_integer(Value) of
        IntegerValue ->
            couch_event_listener:cast(self(), {Cmd, IntegerValue})
    catch error:badarg ->
        couch_log:warning("~s: ~p", [Error, Value])
    end.

bucket_shard(ShardName, [B|Bs]=Buckets0) ->
    case waiting(ShardName, Buckets0) of
        true -> Buckets0;
        false -> [sets:add_element(ShardName, B)|Bs]
    end.

waiting(_, []) ->
    false;
waiting(ShardName, [B|Bs]) ->
    case sets:is_element(ShardName, B) of
        true -> true;
        false -> waiting(ShardName, Bs)
    end.

rebucket_shards(Frequency, Delay, Buckets0) ->
    case (Delay div Frequency + 1) - length(Buckets0) of
        0 ->
            Buckets0;
        N when N < 0 ->
            %% Reduce the number of buckets by merging the last N + 1 together
            {ToMerge, [B|Buckets1]} = lists:split(abs(N), Buckets0),
            [sets:union([B|ToMerge])|Buckets1];
        M ->
            %% Extend the number of buckets by M
            lists:duplicate(M, sets:new()) ++ Buckets0
    end.

%% To ensure that mem3_sync:push/2 is indeed called with roughly the frequency
%% specified by #state.frequency, every message callback must return via a call
%% to maybe_push_shards/1 rather than directly. All timing coordination - i.e.,
%% calling mem3_sync:push/2 or setting a proper timeout to ensure that pending
%% messages aren't dropped in case no further messages arrive - is handled here.
maybe_push_shards(#state{last_push=undefined} = St) ->
    {ok, St#state{last_push=os:timestamp()}, St#state.frequency};
maybe_push_shards(St) ->
    #state{frequency=Frequency, last_push=LastPush, buckets=Buckets0} = St,
    Now = os:timestamp(),
    Delta = timer:now_diff(Now, LastPush) div 1000,
    case Delta > Frequency of
        true ->
            {Buckets1, [ToPush]} = lists:split(length(Buckets0) - 1, Buckets0),
            Buckets2 = [sets:new()|Buckets1],
            %% There's no sets:map/2!
            sets:fold(
                fun(ShardName, _) -> push_shard(ShardName) end,
                undefined,
                ToPush
            ),
            {ok, St#state{last_push=Now, buckets=Buckets2}, Frequency};
        false ->
            {ok, St, Frequency - Delta}
    end.

push_shard(ShardName) ->
    try mem3_shards:for_shard_range(ShardName) of
    Shards ->
        Live = nodes(),
        lists:foreach(
            fun(#shard{node=N}) ->
                case lists:member(N, Live) of
                    true -> mem3_sync:push(ShardName, N);
                    false -> ok
                end
            end,
            Shards
        )
    catch error:database_does_not_exist ->
        ok
    end.

subscribe_for_config() ->
    config:subscribe_for_changes([
        {"mem3", "sync_delay"},
        {"mem3", "sync_frequency"}
    ]).

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

setup() ->
    ok = meck:new(couch_event, [passthrough]),
    ok = meck:expect(couch_event, register_all, ['_'], ok),

    ok = meck:new(config_notifier, [passthrough]),
    ok = meck:expect(config_notifier, handle_event, [
        {[{'_', '_', '_', "error", '_'}, '_'], meck:raise(throw, raised_error)},
        {['_', '_'], meck:passthrough()}
    ]),

    application:start(config),
    {ok, Pid} = ?MODULE:start_link(),
    erlang:unlink(Pid),
    meck:wait(config_notifier, subscribe, '_', 1000),
    Pid.

teardown(Pid) ->
    exit(Pid, shutdown),
    application:stop(config),
    (catch meck:unload(couch_event)),
    (catch meck:unload(config_notifier)),
    ok.

subscribe_for_config_test_() ->
    {
        "Subscrive for configuration changes",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                fun should_set_sync_delay/1,
                fun should_set_sync_frequency/1,
                fun should_restart_listener/1,
                fun should_terminate/1
            ]
        }
    }.

should_set_sync_delay(Pid) ->
    ?_test(begin
        config:set("mem3", "sync_delay", "123", false),
        wait_state(Pid, #state.delay, 123),
        ?assertMatch(#state{delay = 123}, get_state(Pid)),
        ok
    end).

should_set_sync_frequency(Pid) ->
    ?_test(begin
        config:set("mem3", "sync_frequency", "456", false),
        wait_state(Pid, #state.frequency, 456),
        ?assertMatch(#state{frequency = 456}, get_state(Pid)),
        ok
    end).

should_restart_listener(_Pid) ->
    ?_test(begin
        meck:reset(config_notifier),
        config:set("mem3", "sync_frequency", "error", false),

        meck:wait(config_notifier, subscribe, '_', 1000),
        ok
    end).

should_terminate(Pid) ->
    ?_test(begin
        ?assert(is_process_alive(Pid)),

        EventMgr = whereis(config_event),

        RestartFun = fun() -> exit(EventMgr, kill) end,
        test_util:with_process_restart(config_event, RestartFun),

        ?assertNot(is_process_alive(EventMgr)),
        ?assertNot(is_process_alive(Pid)),
        ?assert(is_process_alive(whereis(config_event))),
        ok
    end).


get_state(Pid) ->
    Ref = make_ref(),
    Pid ! {get_state, Ref, self()},
    receive
        {Ref, State} -> State
    after 500 ->
        timeout
    end.


wait_state(Pid, Field, Val) when is_pid(Pid), is_integer(Field) ->
    WaitFun = fun() ->
        case get_state(Pid) of
            #state{} = S when element(Field, S) == Val ->
                true;
            _ ->
                wait
        end
    end,
    test_util:wait(WaitFun).

-endif.
