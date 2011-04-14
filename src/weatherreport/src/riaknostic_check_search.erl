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

%% @doc Diagnostic that checks if <code>riak_search</code>
%% is enabled on every node
-module(riaknostic_check_search).
-behaviour(riaknostic_check).

-export([description/0,
         valid/0,
         check/0,
         format/1]).

-spec description() -> string().
description() ->
    "Check whether search is enabled on all nodes".

-spec valid() -> boolean().
valid() ->
    riaknostic_node:can_connect_all().

-spec check() -> [{lager:log_level(), term()}].
check() ->
    Stats = riaknostic_node:stats(),
    {ring_members, RingMembers} = lists:keyfind(ring_members, 1, Stats),

    {SearchEnabled, _} = riaknostic_node:cluster_command(application, get_env, [riak_search, enabled]),

    {_, X} = lists:unzip(SearchEnabled), 
    NodesSearchEnabled = lists:zip(RingMembers, X),

    lists:append([
      [ {warning, {riak_search, NodesSearchEnabled}} || length(lists:usort(SearchEnabled)) > 1 ]
      ]).

-spec format(term()) -> {io:format(), [term()]}.
format({riak_search, Services}) ->
    {"Search is not enabled on all nodes: ~p", [Services]}.
