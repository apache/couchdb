% Copyright 2013 Cloudant
%
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

-module(mem3_rebalance).

-export([rebalance/1, rebalance/2]).
-include("mem3.hrl").

rebalance(DbName) ->
    rebalance(DbName, mem3:nodes()).

rebalance(DbName, TargetNodes) when is_binary(DbName) ->
    rebalance(mem3:shards(DbName), TargetNodes);
rebalance(Shards, TargetNodes) when is_list(Shards) ->
    %% first migrate shards off of non-target nodes
    {OK, MoveThese} = lists:partition(fun(#shard{node=Node}) ->
        lists:member(Node, TargetNodes)
    end, Shards),
    ShardsByTargetNode0 = lists:foldl(fun(Shard, Acc) ->
        orddict:append(Shard#shard.node, Shard, Acc)
    end, orddict:new(), OK),
    ShardsByTargetNode = lists:sort(lists:foldl(fun(Node, Acc) ->
        case orddict:is_key(Node, ShardsByTargetNode0) of
            true ->
                Acc;
            false ->
                [{Node, []} | Acc]
        end
    end, ShardsByTargetNode0, TargetNodes)),
    Moves = find_replacements(MoveThese, ShardsByTargetNode, []),
    Moved = [Shard#shard{node = Node} || {Shard, Node} <- Moves],
    TargetLevel = length(Shards) div length(TargetNodes),
    rebalance2(TargetLevel, OK ++ Moved, TargetNodes, TargetNodes, Moves).

find_replacements([], _ShardsByTargetNode, Result) ->
    Result;
find_replacements([Shard | Rest], ShardsByNode, Acc) ->
    Zone = mem3:node_info(Shard#shard.node, <<"zone">>),
    % Find a node in the same zone
    InZone = [{Node, Shards} || {Node, Shards} <- ShardsByNode,
        mem3:node_info(Node, <<"zone">>) =:= Zone],
    % Prefer a node with the fewest number of shards
    if InZone =:= [] ->
        erlang:error({empty_zone, Zone, Shard});
    true ->
        ok
    end,
    [{TargetNode, _} | _] = lists:sort(fun smallest_first/2, InZone),
    TargetShard = Shard#shard{node = TargetNode},
    find_replacements(Rest, orddict:append(TargetNode, TargetShard, ShardsByNode),
        [{Shard, TargetNode} | Acc]).

rebalance2(_TargetLevel, Shards, _Nodes, [], Moves) ->
    {Shards, Moves};
rebalance2(TargetLevel, Shards, Nodes, [Node | Rest], Moves) ->
    ShardsForNode = [S || S <- Shards, S#shard.node =:= Node],
    CurrentLevel = length(ShardsForNode),
    case CurrentLevel < TargetLevel of
        true ->
            case victim(TargetLevel, Shards, Nodes, Node) of
                {ok, Victim} ->
                    rebalance2(TargetLevel,
                             replace(Victim, Victim#shard{node=Node}, Shards),
                             Nodes, [Node|Rest], [{Victim, Node}|Moves]);
                false ->
                    rebalance2(TargetLevel, Shards, Nodes, Rest, Moves)
            end;
        false ->
            rebalance2(TargetLevel, Shards, Nodes, Rest, Moves)
    end.

victim(TargetLevel, Shards, Nodes, TargetNode) ->
    TargetZone = mem3:node_info(TargetNode, <<"zone">>),
    CandidateNodes = lists:usort([Node || Node <- mem3:nodes(),
                                     Node =/= TargetNode,
                                     mem3:node_info(Node, <<"zone">>) =:= TargetZone]),
    %% make {Node, ShardsInNode} list
    GroupedByNode0 = [{Node, [S || S <- Shards, S#shard.node =:= Node]} || Node <- CandidateNodes],
    %% don't take from a balancing node below target level
    GroupedByNode1 = [{N, SS} || {N, SS} <- GroupedByNode0,
        (length(SS) > TargetLevel) orelse (not lists:member(N, Nodes))],
    %% prefer to take from a node with more shards than others
    GroupedByNode2 = lists:sort(fun largest_first/2, GroupedByNode1),
    %% don't take a shard for a range the target already has
    TargetRanges = lists:usort([S#shard.range || S <- Shards, S#shard.node =:= TargetNode]),
    GroupedByNode3 = [{N, lists:filter(fun(S) -> not lists:member(S#shard.range, TargetRanges) end, SS)}
                      || {N, SS} <- GroupedByNode2],
    %% remove nodes with no candidates shards
    GroupedByNode4 = [{N, SS} || {N, SS} <- GroupedByNode3, SS =/= []],
    case GroupedByNode4 of
        [{_, [Victim|_]} | _] -> {ok, Victim};
        [] -> false
    end.

largest_first({_, A}, {_, B}) ->
    length(A) >= length(B).

smallest_first({_, A}, {_, B}) ->
    length(A) < length(B).

replace(A, B, List) ->
    replace(A, B, List, []).

replace(_A, _B, [], Acc) ->
    Acc;
replace(A, B, [A | Rest], Acc) ->
    replace(A, B, Rest, [B | Acc]);
replace(A, B, [C | Rest], Acc) ->
    replace(A, B, Rest, [C | Acc]).
