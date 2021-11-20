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

-module(mem3_sync_event).
-behaviour(gen_event).
-vsn(1).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

init(_) ->
    net_kernel:monitor_nodes(true),
    {ok, nil}.

handle_event({add_node, Node}, State) when Node =/= node() ->
    net_kernel:connect_node(Node),
    mem3_sync_nodes:add([Node]),
    {ok, State};
handle_event({remove_node, Node}, State) ->
    mem3_sync:remove_node(Node),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info({nodeup, Node}, State) ->
    Nodes0 = lists:usort([node() | drain_nodeups([Node])]),
    Nodes = lists:filter(fun(N) -> lists:member(N, mem3:nodes()) end, Nodes0),
    wait_for_rexi(Nodes, 5),
    {ok, State};
handle_info({nodedown, Node}, State) ->
    mem3_sync:remove_node(Node),
    {ok, State};
handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

drain_nodeups(Acc) ->
    receive
        {nodeup, Node} ->
            drain_nodeups([Node | Acc])
    after 0 ->
        Acc
    end.

wait_for_rexi([], _Retries) ->
    ok;
wait_for_rexi(Waiting, Retries) ->
    % Hack around rpc:multicall/4 so that we can
    % be sure which nodes gave which response
    Msg = {call, rexi_server_mon, status, [], group_leader()},
    {Resp, _Bad} = gen_server:multi_call(Waiting, rex, Msg, 1000),
    Up = [N || {N, R} <- Resp, R == ok],
    NotUp = Waiting -- Up,
    case length(Up) > 0 of
        true ->
            mem3_sync_nodes:add(Up);
        false ->
            ok
    end,
    case length(NotUp) > 0 andalso Retries > 0 of
        true ->
            timer:sleep(1000),
            wait_for_rexi(NotUp, Retries - 1);
        false ->
            ok
    end.
