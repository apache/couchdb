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
%% File renamed from riaknostic_check_ring_membership.erl to
%% weatherreport_check_membership.erl and modified to work with Apache
%% CouchDB
%%
%% Copyright (c) 2014 Cloudant
%%
%% -------------------------------------------------------------------

%% @doc Diagnostic that checks whether the local node is a member of
%% the ring. This might arise when the node name in vm.args has
%% changed but the node has not been renamed in the ring.
-module(weatherreport_check_membership).
-behaviour(weatherreport_check).

-export([
    description/0,
    valid/0,
    check/1,
    format/1
]).

-include_lib("eunit/include/eunit.hrl").

-spec description() -> string().
description() ->
    "Cluster membership validity".

-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().

-spec check(list()) -> [{atom(), term()}].
check(_Opts) ->
    NodeName = node(),
    Members = mem3:nodes(),
    case lists:member(NodeName, Members) of
        true ->
            [];
        false ->
            [{warning, {not_ring_member, NodeName}}]
    end.

-spec format(term()) -> {io:format(), [term()]}.
format({not_ring_member, Nodename}) ->
    {"Local node ~w is not a member of the cluster. Please check that the -name setting in vm.args is correct.",
        [Nodename]}.
