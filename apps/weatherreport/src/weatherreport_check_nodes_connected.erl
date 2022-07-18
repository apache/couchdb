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
%% File renamed from riaknostic_check_nodes_connected.erl to
%% weatherreport_check_nodes_connected.erl and modified to work with
%% Apache CouchDB
%%
%% Copyright (c) 2014 Cloudant
%%
%% -------------------------------------------------------------------

%% @doc Diagnostic check that detects cluster members that are down.
-module(weatherreport_check_nodes_connected).
-behaviour(weatherreport_check).

-export([
    description/0,
    valid/0,
    check/1,
    format/1
]).

-spec description() -> string().
description() ->
    "Cluster node liveness".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

-spec check(list()) -> [{atom(), term()}].
check(_Opts) ->
    NodeName = node(),
    ConnectedNodes = [NodeName | erlang:nodes()],
    Members = mem3:nodes(),
    [
        {warning, {node_disconnected, N}}
     || N <- Members,
        N =/= NodeName,
        lists:member(N, ConnectedNodes) == false
    ].

-spec format(term()) -> {io:format(), [term()]}.
format({node_disconnected, Node}) ->
    {"Cluster member ~s is not connected to this node. Please check whether it is down.", [Node]}.
