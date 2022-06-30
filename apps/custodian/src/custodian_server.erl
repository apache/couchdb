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

-module(custodian_server).
-behaviour(gen_server).
-vsn(3).
-behaviour(config_listener).

% public api.
-export([start_link/0]).

% gen_server api.
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

% exported for callback.
-export([
    check_shards/0,
    handle_db_event/3
]).

% config_listener callback
-export([handle_config_change/5, handle_config_terminate/3]).

% private records.
-record(state, {
    event_listener,
    shard_checker,
    rescan = false
}).

-define(VSN_0_2_7, 184129240591641721395874905059581858099).

-ifdef(TEST).
-define(RELISTEN_DELAY, 50).
-else.
-define(RELISTEN_DELAY, 5000).
-endif.

% public functions.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_config_change("couchdb", "maintenance_mode", _, _, S) ->
    ok = gen_server:cast(?MODULE, refresh),
    {ok, S};
handle_config_change(_, _, _, _, S) ->
    {ok, S}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    erlang:send_after(?RELISTEN_DELAY, whereis(?MODULE), restart_config_listener).

% gen_server functions.
init(_) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    ok = config:listen_for_changes(?MODULE, nil),
    {ok, LisPid} = start_event_listener(),
    {ok,
        start_shard_checker(#state{
            event_listener = LisPid
        })}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(refresh, State) ->
    {noreply, start_shard_checker(State)}.

handle_info({nodeup, _}, State) ->
    {noreply, start_shard_checker(State)};
handle_info({nodedown, _}, State) ->
    {noreply, start_shard_checker(State)};
handle_info({'EXIT', Pid, normal}, #state{shard_checker = Pid} = State) ->
    NewState = State#state{shard_checker = undefined},
    case State#state.rescan of
        true ->
            {noreply, start_shard_checker(NewState)};
        false ->
            {noreply, NewState}
    end;
handle_info({'EXIT', Pid, Reason}, #state{shard_checker = Pid} = State) ->
    couch_log:notice("custodian shard checker died ~p", [Reason]),
    NewState = State#state{shard_checker = undefined},
    {noreply, start_shard_checker(NewState)};
handle_info({'EXIT', Pid, Reason}, #state{event_listener = Pid} = State) ->
    couch_log:notice("custodian update notifier died ~p", [Reason]),
    {ok, Pid1} = start_event_listener(),
    {noreply, State#state{event_listener = Pid1}};
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State}.

terminate(_Reason, State) ->
    couch_event:stop_listener(State#state.event_listener),
    couch_util:shutdown_sync(State#state.shard_checker),
    ok.

code_change(?VSN_0_2_7, State, _Extra) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {ok, State};
code_change(_OldVsn, #state{} = State, _Extra) ->
    {ok, State}.

% private functions

start_shard_checker(#state{shard_checker = undefined} = State) ->
    State#state{
        shard_checker = spawn_link(fun ?MODULE:check_shards/0),
        rescan = false
    };
start_shard_checker(#state{shard_checker = Pid} = State) when is_pid(Pid) ->
    State#state{rescan = true}.

start_event_listener() ->
    DbName = mem3_sync:shards_db(),
    couch_event:link_listener(
        ?MODULE, handle_db_event, nil, [{dbname, DbName}]
    ).

handle_db_event(_DbName, updated, _St) ->
    gen_server:cast(?MODULE, refresh),
    {ok, nil};
handle_db_event(_DbName, _Event, _St) ->
    {ok, nil}.

check_shards() ->
    [send_event(Item) || Item <- custodian:summary()].

send_event({_, Count} = Item) ->
    Description = describe(Item),
    Name = check_name(Item),
    case Count of
        0 ->
            ok;
        1 ->
            couch_log:critical("~s", [Description]);
        _ ->
            couch_log:warning("~s", [Description])
    end,
    ?CUSTODIAN_MONITOR:send_event(Name, Count, Description).

describe({{safe, N}, Count}) ->
    lists:concat([
        Count,
        " ",
        shards(Count),
        " in cluster with only ",
        N,
        " ",
        copies(N),
        " on nodes that are currently up"
    ]);
describe({{live, N}, Count}) ->
    lists:concat([
        Count,
        " ",
        shards(Count),
        " in cluster with only ",
        N,
        " ",
        copies(N),
        " on nodes not in maintenance mode"
    ]);
describe({conflicted, Count}) ->
    lists:concat([Count, " conflicted ", shards(Count), " in cluster"]).

check_name({{Type, N}, _}) ->
    lists:concat(["custodian-", N, "-", Type, "-shards-check"]);
check_name({Type, _}) ->
    lists:concat(["custodian-", Type, "-shards-check"]).

shards(1) ->
    "shard";
shards(_) ->
    "shards".

copies(1) ->
    "copy";
copies(_) ->
    "copies".

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

config_update_test_() ->
    {
        "Test config updates",
        {
            foreach,
            fun() -> test_util:start_couch([custodian]) end,
            fun test_util:stop_couch/1,
            [
                fun t_restart_config_listener/1
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
        ?assertNotEqual(ConfigMonitor, NewConfigMonitor),
        ?assert(is_process_alive(NewConfigMonitor))
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
