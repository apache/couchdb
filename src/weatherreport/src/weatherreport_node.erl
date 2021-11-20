%% -------------------------------------------------------------------
%%
%% derived from riaknostic - automated diagnostic tools for Riak
%%
%% Copyright (c) 2011 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%%
%% File renamed from riaknostic_node.erl to weatherreport_node.erl and
%% modified to work with Apache CouchDB
%%
%% Copyright (c) 2014 Cloudant
%%
%% -------------------------------------------------------------------

%% @doc Functions that help diagnostics interact with the local
%% node or other members of the cluster.
-module(weatherreport_node).

-export([
    can_connect/0,
    can_connect_all/0,
    pid/0,
    local_command/2,
    local_command/3,
    local_command/4,
    multicall/5,
    nodename/0
]).

%% @doc Calls the given 0-arity module and function on the local
%% node and returns the result of that call.
%% @equiv local_command(Module, Function, [])
%% @see can_connect/0.
-spec local_command(Module :: atom(), Function :: atom()) -> term().
local_command(Module, Function) ->
    local_command(Module, Function, []).

%% @doc Calls the given module and function with the given arguments
%% on the local node and returns the result of that call.
%% @equiv local_command(Module, Function, Args, 5000)
%% @see can_connect/0
-spec local_command(Module :: atom(), Function :: atom(), Args :: [term()]) -> term().
local_command(Module, Function, Args) ->
    local_command(Module, Function, Args, weatherreport_config:timeout()).

%% @doc Calls the given module and function with the given arguments
%% on the local node and returns the result of that call,
%% returning an error if the call doesn't complete within the given
%% timeout.
%% @equiv rpc:call(NodeName, Module, Function, Args, Timeout)
%% @see can_connect/0
-spec local_command(Module :: atom(), Function :: atom(), Args :: [term()], Timeout :: integer()) ->
    term().
local_command(Module, Function, Args, Timeout) ->
    case is_cluster_node() of
        true ->
            weatherreport_log:log(
                node(),
                debug,
                "Local function call: ~p:~p(~p)",
                [Module, Function, Args]
            ),
            erlang:apply(Module, Function, Args);
        _ ->
            weatherreport_log:log(
                node(),
                debug,
                "Local RPC: ~p:~p(~p) [~p]",
                [Module, Function, Args, Timeout]
            ),
            rpc:call(nodename(), Module, Function, Args, Timeout)
    end.

%% @doc Call rpc:multicall/5 from the local cluster node rather than the
%% escript.
-spec multicall(
    [node()], Module :: atom(), Function :: atom(), Args :: [term()], Timeout :: integer()
) -> term().
multicall(Nodes, Module, Function, Args, Timeout) ->
    case local_command(rpc, multicall, [Nodes, Module, Function, Args, Timeout]) of
        {badrpc, Reason} ->
            {[{badrpc, Reason}], []};
        Resp ->
            Resp
    end.

%% @doc Retrieves the operating system's process ID of the local
%% node.
%% @equiv local_command(os, getpid)
%% @see can_connect/0
-spec pid() -> string().
pid() ->
    local_command(os, getpid).

%% @doc Attempts to connect to the local node if it is not
%% already, and returns whether connection was successful.
-spec can_connect() -> true | false.
can_connect() ->
    case is_connected() or is_cluster_node() of
        true ->
            true;
        false ->
            weatherreport_log:log(
                node(),
                debug,
                "Not connected to the local cluster node, trying to connect. alive:~p connect_failed:~p",
                [is_alive(), connect_failed()]
            ),
            maybe_connect()
    end.

-spec can_connect_all() -> true | false.
can_connect_all() ->
    case is_connected() of
        true ->
            case weatherreport_check_nodes_connected:check([]) of
                [] -> true;
                _ -> false
            end;
        false ->
            false
    end.

nodename() ->
    Name =
        case weatherreport_config:node_name() of
            undefined ->
                atom_to_list(node());
            {_, NodeName} ->
                NodeName
        end,
    case string:tokens(Name, "@") of
        [_Node, _Host] ->
            list_to_atom(Name);
        [Node] ->
            [_, Host] = string:tokens(atom_to_list(node()), "@"),
            list_to_atom(lists:concat([Node, "@", Host]))
    end.

%% Private functions
is_cluster_node() ->
    nodename() =:= node().

is_connected() ->
    is_alive() andalso connect_failed() =/= true.

maybe_connect() ->
    case connect_failed() of
        true -> false;
        _ -> try_connect()
    end.

try_connect() ->
    TargetNode = nodename(),
    case is_alive() of
        true -> ok;
        _ -> start_net()
    end,
    case {net_kernel:hidden_connect_node(TargetNode), net_adm:ping(TargetNode)} of
        {true, pong} ->
            application:set_env(weatherreport, connect_failed, false),
            weatherreport_log:log(
                node(),
                debug,
                "Connected to local cluster node ~p.",
                [TargetNode]
            ),
            true;
        _ ->
            application:set_env(weatherreport, connect_failed, true),
            weatherreport_log:log(
                node(),
                warning,
                "Could not connect to the local cluster node ~p, some checks will not run.",
                [TargetNode]
            ),
            false
    end.

connect_failed() ->
    case application:get_env(weatherreport, connect_failed) of
        {ok, true} -> true;
        undefined -> undefined;
        _ -> false
    end.

start_net() ->
    weatherreport_log:log(node(), debug, "Starting distributed Erlang."),
    {Type, NodeName} = weatherreport_config:node_name(),
    ThisNode = append_node_suffix(NodeName, "_diag"),
    {ok, _} = net_kernel:start([ThisNode, Type]),
    erlang:set_cookie(node(), weatherreport_config:cookie()).

append_node_suffix(Name, Suffix) ->
    case string:tokens(Name, "@") of
        [Node, Host] ->
            list_to_atom(lists:concat([Node, Suffix, os:getpid(), "@", Host]));
        [Node] ->
            list_to_atom(lists:concat([Node, Suffix, os:getpid()]))
    end.
