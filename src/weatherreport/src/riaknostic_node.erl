%% -------------------------------------------------------------------
%%
%% riaknostic - automated diagnostic tools for Riak
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

%% @doc Functions that help diagnostics interact with the local Riak
%% node or other members of the cluster.
-module(riaknostic_node).

-export([can_connect/0,
         can_connect_all/0,
         stats/0,
         pid/0,
         local_command/2,
         local_command/3,
         local_command/4,
         cluster_command/2,
         cluster_command/3,
         cluster_command/4
        ]).

%% @doc Calls the given 0-arity module and function on the local Riak
%% node and returns the result of that call.
%% @equiv local_command(Module, Function, [])
%% @see can_connect/0.
-spec local_command(Module::atom(), Function::atom()) -> term().
local_command(Module, Function) ->
    local_command(Module, Function, []).

%% @doc Calls the given module and function with the given arguments
%% on the local Riak node and returns the result of that call.
%% @equiv local_command(Module, Function, Args, 5000)
%% @see can_connect/0
-spec local_command(Module::atom(), Function::atom(), Args::[term()]) -> term().
local_command(Module, Function, Args) ->
    local_command(Module, Function, Args, 5000).

%% @doc Calls the given module and function with the given arguments
%% on the local Riak node and returns the result of that call,
%% returning an error if the call doesn't complete within the given
%% timeout.
%% @equiv rpc:call(RiakNodeName, Module, Function, Args, Timeout)
%% @see can_connect/0
-spec local_command(Module::atom(), Function::atom(), Args::[term()], Timeout::integer()) -> term().
local_command(Module, Function, Args, Timeout) ->
    riaknostic_util:log(debug, "Local RPC: ~p:~p(~p) [~p]", [Module, Function, Args, Timeout]),
    rpc:call(nodename(), Module, Function, Args, Timeout).

%% @doc Calls the given 0-arity module and function on all members of
%% the Riak cluster.
%% @equiv cluster_command(Module, Function, [])
%% @see can_connect/0
-spec cluster_command(Module::atom(), Function::atom()) -> term().
cluster_command(Module, Function) ->
    cluster_command(Module, Function, []).

%% @doc Calls the given module and function with the given arguments
%% on all members of the Riak cluster.
%% @equiv cluster_command(Module, Function, Args, 5000)
%% @see can_connect/0
-spec cluster_command(Module::atom(), Function::atom(), Args::[term()]) -> term().
cluster_command(Module, Function, Args) ->
    cluster_command(Module, Function, Args, 5000).

%% @doc Calls the given module and function with the given arguments
%% on all members for the Riak cluster, returning an error if the call
%% doesn't complete within the given timeout.
%% @equiv rpc:multicall(RiakClusterMembers, Module, Function, Args, Timeout)
%% @see can_connect/0
-spec cluster_command(Module::atom(), Function::atom(), Args::[term()], Timeout::integer()) -> term().
cluster_command(Module, Function, Args, Timeout) ->
    riaknostic_util:log(debug, "Cluster RPC: ~p:~p(~p) [~p]", [Module, Function, Args, Timeout]),
    Stats = stats(),
    {ring_members, RingMembers} = lists:keyfind(ring_members, 1, Stats),
    rpc:multicall(RingMembers, Module, Function, Args, Timeout).

%% @doc Retrieves the operating system's process ID of the local Riak
%% node.
%% @equiv local_command(os, getpid)
%% @see can_connect/0
-spec pid() -> string().
pid() ->
    local_command(os, getpid).

%% @doc Attempts to connect to the local Riak node if it is not
%% already, and returns whether connection was successful.
-spec can_connect() -> true | false.
can_connect() ->
    case is_connected() of
        true -> true;
        false ->
            riaknostic_util:log(debug, "Not connected to the local Riak node, trying to connect. alive:~p connect_failed:~p", [is_alive(), connect_failed()]),
            maybe_connect()
    end.

-spec can_connect_all() -> true | false.
can_connect_all() ->
    case is_connected() of
        true ->
            case riaknostic_check_nodes_connected:check() of
                [] -> true;
                _ -> false
            end;
        false -> false
    end.

%% @doc Fetches or returns previously fetched Riak statistics.
%% @see can_connect/0
-spec stats() -> [proplists:property()].
stats() ->
    case has_stats() of
        {ok, Stats} -> Stats;
        _ -> fetch_stats()
    end.

%% Private functions
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
            application:set_env(riaknostic, connect_failed, false),
            riaknostic_util:log(debug, "Connected to local Riak node ~p.", [TargetNode]),
            true;
        _ ->
            application:set_env(riaknostic, connect_failed, true),
            lager:warning("Could not connect to the local Riak node ~p, some checks will not run.", [TargetNode]),
            false
    end.

connect_failed() ->
    case application:get_env(riaknostic, connect_failed) of
        {ok, true} -> true;
        undefined -> undefined;
        _ -> false
    end.

start_net() ->
    riaknostic_util:log(debug, "Starting distributed Erlang."),
    {Type, RiakName} = riaknostic_config:node_name(),
    ThisNode = append_node_suffix(RiakName, "_diag"),
    {ok, _} = net_kernel:start([ThisNode, Type]),
    erlang:set_cookie(node(), riaknostic_config:cookie()).

nodename() ->
    {_, Name} = riaknostic_config:node_name(),
    case string:tokens(Name, "@") of
        [_Node, _Host] ->
            list_to_atom(Name);
        [Node] ->
            [_, Host] = string:tokens(atom_to_list(node()), "@"),
            list_to_atom(lists:concat([Node, "@", Host]))
    end.

append_node_suffix(Name, Suffix) ->
    case string:tokens(Name, "@") of
        [Node, Host] ->
            list_to_atom(lists:concat([Node, Suffix, os:getpid(), "@", Host]));
        [Node] ->
            list_to_atom(lists:concat([Node, Suffix, os:getpid()]))
    end.

has_stats() ->
    case application:get_env(riaknostic, local_stats) of
        {ok, Stats} ->
            {ok, Stats};
        undefined ->
            false
    end.

fetch_stats() ->
    riaknostic_util:log(debug, "Fetching local riak_kv_status."),
    case local_command(riak_kv_status, statistics) of
        [] -> [];
        PList ->
            application:set_env(riaknostic, local_stats, PList),
            PList
    end.

