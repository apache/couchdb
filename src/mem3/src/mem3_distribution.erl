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

% This module is in charge of keeping the cluster connected. If nodes
% disconnect they are reconnected with net_kernel:connect_node/1.

-module(mem3_distribution).

-behaviour(gen_server).

-export([
    start_link/0,
    connect_node/1,
    events/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(JITTER_PERCENT, 0.25).
% init + 3 up/down pairs
-define(EVENT_MAX_HISTORY, 7).

-record(st, {
    tref,
    % #{Node => [Evt, ...]}
    events = #{}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect_node(Node) when is_atom(Node) ->
    net_kernel:connect_node(Node).

events() ->
    gen_server:call(?MODULE, events, infinity).

init(_) ->
    connect(false),
    net_kernel:monitor_nodes(true, [nodedown_reason]),
    St = #st{
        tref = schedule_connect(),
        events = init_events(nodes())
    },
    {ok, St}.

handle_call(events, _From, #st{} = St) ->
    {reply, St#st.events, St};
handle_call(Msg, _From, #st{} = St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.

handle_cast(Msg, #st{} = St) ->
    {stop, {bad_cast, Msg}, St}.

handle_info({nodeup, Node, _Info}, #st{events = Events} = St) ->
    Events1 = update_event(Node, nodeup, null, Events),
    {noreply, St#st{events = Events1}};
handle_info({nodedown, Node, Info}, #st{events = Events} = St) ->
    Reason = couch_util:get_value(nodedown_reason, Info),
    log_node_down(Node, Reason),
    Events1 = update_event(Node, nodedown, Reason, Events),
    {noreply, St#st{events = Events1}};
handle_info(connect, #st{} = St) ->
    erlang:cancel_timer(St#st.tref),
    ok = connect(true),
    {noreply, St#st{tref = schedule_connect()}};
handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.

connect(Log) ->
    Expected = ordsets:from_list([N || N <- mem3:nodes(), N =/= node()]),
    Connected = ordsets:from_list(nodes()),
    NotConnected = ordsets:subtract(Expected, Connected),
    connect(ordsets:to_list(NotConnected), Log).

connect([], _Log) ->
    ok;
connect([N | Rest], Log) ->
    ConnectRes = ?MODULE:connect_node(N),
    log(Log, ConnectRes, N),
    connect(Rest, Log).

log(true, true, Node) ->
    couch_log:warning("~s : reconnected to ~s", [?MODULE, Node]),
    ok;
log(_, _, _) ->
    % Failed to connect or we don't want to log it
    ok.

log_node_down(_Node, connection_closed) ->
    % Received on a regular node shutdown
    ok;
log_node_down(_Node, disconnect) ->
    % This node requested a disconnect
    ok;
log_node_down(Node, Reason) ->
    couch_log:warning("~s : node ~s down, reason: ~p", [?MODULE, Node, Reason]).

schedule_connect() ->
    erlang:send_after(wait_msec(), self(), connect).

wait_msec() ->
    IntervalSec = config:get_integer("cluster", "reconnect_interval_sec", 37),
    IntervalMSec = IntervalSec * 1000,
    IntervalMSec + jitter(IntervalMSec).

jitter(Interval) ->
    Jitter = round(Interval * ?JITTER_PERCENT),
    % rand:uniform(0) crashes!
    rand:uniform(max(1, Jitter)).

init_events(Nodes) ->
    NowSec = erlang:system_time(second),
    maps:from_keys(Nodes, [make_event(NowSec, init, null)]).

update_event(Node, Event, Reason, EventMap) ->
    NowSec = erlang:system_time(second),
    Fun = fun(Events) ->
        History = Events ++ [make_event(NowSec, Event, Reason)],
        Rev = lists:reverse(History),
        RevTrim = lists:sublist(Rev, ?EVENT_MAX_HISTORY),
        lists:reverse(RevTrim)
    end,
    maps:update_with(Node, Fun, [make_event(NowSec, Event, Reason)], EventMap).

make_event(TSec, Event, Reason) when is_integer(TSec), TSec > 0, is_atom(Event) ->
    TStr = calendar:system_time_to_rfc3339(TSec, [{offset, "Z"}]),
    Res = [list_to_binary(TStr), Event],
    % For non-TCP (custom) dist protocol reason maybe a non-atom
    case Reason of
        null -> Res;
        Atom when is_atom(Atom) -> Res ++ [Atom];
        Val -> Res ++ [couch_util:to_binary(Val)]
    end.
