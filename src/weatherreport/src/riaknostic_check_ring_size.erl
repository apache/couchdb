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

%% @doc Diagnostic that compares the configured
%% <code>ring_creation_size</code> to the actual size of the ring.
-module(riaknostic_check_ring_size).
-behaviour(riaknostic_check).

-export([description/0,
         valid/0,
         check/0,
         format/1]).

-spec description() -> string().
description() ->
    "Ring size valid".

-spec valid() -> boolean().
valid() ->
    riaknostic_node:can_connect().

-spec check() -> [{lager:log_level(), term()}].
check() ->
    Stats = riaknostic_node:stats(),
    {ring_creation_size, RingSize} = lists:keyfind(ring_creation_size, 1, Stats),
    {ring_num_partitions, NumPartitions} = lists:keyfind(ring_num_partitions, 1, Stats),
%    {ring_members, RingMembers} = lists:keyfind(ring_members, 1, Stats),
%    NumRingMembers = length(RingMembers),
%    VnodesPerNode = erlang:round(RingSize / NumRingMembers),
%    MinAcceptableVnodesPerNode = erlang:round(RingSize * 0.03),
%    MaxRecommendedVnodesPerNode = erlang:round(RingSize * 0.7),

    lists:append([
      [ {notice, {ring_size_unequal, RingSize, NumPartitions}} || RingSize /= NumPartitions ],
      [ {critical, {ring_size_not_exp2, RingSize}} || (RingSize band -(bnot RingSize)) /= RingSize]
%      [ {notice, {ring_size_too_small, RingSize, NumRingMembers}} || VnodesPerNode =< MinAcceptableVnodesPerNode ],
%      [ {notice, {too_few_nodes_for_ring, RingSize, NumRingMembers}} || VnodesPerNode >= MaxRecommendedVnodesPerNode ]
      ]).

-spec format(term()) -> {io:format(), [term()]}.
format({ring_size_unequal, S, P}) ->
    {"The configured ring_creation_size (~B) is not equal to the number of partitions in the ring (~B). "
     "Please verify that the ring_creation_size in app.config is correct.", [S, P]};

format({ring_size_not_exp2, S}) ->
    {"The configured ring_creation_size (~B) should always be a power of 2. "
     "Please reconfigure the ring_creation_size in app.config.", [S]}.

%format({ring_size_too_small, S, N}) ->
%    {"With a ring_creation_size (~B) and ~B nodes participating in the cluster, each node is responsible for less than 3% of the data. "
%     " You have too many nodes for this size ring. "
%     "Please consider migrating data to a cluster with 2 or 4x your current ring size.", [S, N]};

%format({too_few_nodes_for_ring, S, N}) ->
%    {"With a ring_creation_size (~B) and ~B nodes participating in the cluster, each node is responsible for more than 70% of the data. "
%     " You have too few nodes for this size ring. "
%     "Please consider joining more nodes to your cluster.", [S, N]}.
