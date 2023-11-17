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
    connect_node/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(JITTER_PERCENT, 0.25).

-record(st, {
    tref
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect_node(Node) when is_atom(Node) ->
    net_kernel:connect_node(Node).

init(_) ->
    connect(false),
    {ok, #st{tref = erlang:send_after(wait_msec(), self(), connect)}}.

handle_call(Msg, _From, #st{} = St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.

handle_cast(Msg, #st{} = St) ->
    {stop, {bad_cast, Msg}, St}.

handle_info(connect, #st{} = St) ->
    erlang:cancel_timer(St#st.tref),
    ok = connect(true),
    {noreply, St#st{tref = erlang:send_after(wait_msec(), self(), connect)}};
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

wait_msec() ->
    IntervalSec = config:get_integer("cluster", "reconnect_interval_sec", 37),
    IntervalMSec = IntervalSec * 1000,
    IntervalMSec + jitter(IntervalMSec).

jitter(Interval) ->
    Jitter = round(Interval * ?JITTER_PERCENT),
    % rand:uniform(0) crashes!
    rand:uniform(max(1, Jitter)).
