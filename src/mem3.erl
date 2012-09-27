% Copyright 2010 Cloudant
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

-module(mem3).

-export([start/0, stop/0, restart/0, nodes/0, node_info/2, shards/1, shards/2,
    choose_shards/2, n/1, dbname/1, ushards/1, ushards/2]).
-export([get_shard/3, local_shards/1, fold_shards/2]).
-export([sync_security/0, sync_security/1]).
-export([compare_nodelists/0, compare_shards/1]).
-export([quorum/1, group_by_proximity/1]).
-export([live_shards/2]).

-include("mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

start() ->
    application:start(mem3).

stop() ->
    application:stop(mem3).

restart() ->
    stop(),
    start().

%% @doc Detailed report of cluster-wide membership state.  Queries the state
%%      on all member nodes and builds a dictionary with unique states as the
%%      key and the nodes holding that state as the value.  Also reports member
%%      nodes which fail to respond and nodes which are connected but are not
%%      cluster members.  Useful for debugging.
-spec compare_nodelists() -> [{{cluster_nodes, [node()]} | bad_nodes
    | non_member_nodes, [node()]}].
compare_nodelists() ->
    Nodes = mem3:nodes(),
    AllNodes = erlang:nodes([this, visible]),
    {Replies, BadNodes} = gen_server:multi_call(Nodes, mem3_nodes, get_nodelist),
    Dict = lists:foldl(fun({Node, Nodelist}, D) ->
        orddict:append({cluster_nodes, Nodelist}, Node, D)
    end, orddict:new(), Replies),
    [{non_member_nodes, AllNodes -- Nodes}, {bad_nodes, BadNodes} | Dict].

-spec compare_shards(DbName::iodata()) -> [{bad_nodes | [#shard{}], [node()]}].
compare_shards(DbName) when is_list(DbName) ->
    compare_shards(list_to_binary(DbName));
compare_shards(DbName) ->
    Nodes = mem3:nodes(),
    {Replies, BadNodes} = rpc:multicall(mem3, shards, [DbName]),
    GoodNodes = [N || N <- Nodes, not lists:member(N, BadNodes)],
    Dict = lists:foldl(fun({Shards, Node}, D) ->
        orddict:append(Shards, Node, D)
    end, orddict:new(), lists:zip(Replies, GoodNodes)),
    [{bad_nodes, BadNodes} | Dict].

-spec n(DbName::iodata()) -> integer().
n(DbName) ->
    length(mem3:shards(DbName, <<"foo">>)).

-spec nodes() -> [node()].
nodes() ->
    mem3_nodes:get_nodelist().

node_info(Node, Key) ->
    mem3_nodes:get_node_info(Node, Key).

-spec shards(DbName::iodata()) -> [#shard{}].
shards(DbName) when is_list(DbName) ->
    shards(list_to_binary(DbName));
shards(DbName) ->
    ShardDbName =
        list_to_binary(couch_config:get("mem3", "shard_db", "dbs")),
    case DbName of
    ShardDbName ->
        %% shard_db is treated as a single sharded db to support calls to db_info
        %% and view_all_docs
        [#shard{
            node = node(),
            name = ShardDbName,
            dbname = ShardDbName,
            range = [0, 2 bsl 31]}];
    _ ->
        mem3_shards:for_db(DbName)
    end.

-spec shards(DbName::iodata(), DocId::binary()) -> [#shard{}].
shards(DbName, DocId) when is_list(DbName) ->
    shards(list_to_binary(DbName), DocId);
shards(DbName, DocId) when is_list(DocId) ->
    shards(DbName, list_to_binary(DocId));
shards(DbName, DocId) ->
    mem3_shards:for_docid(DbName, DocId).

-spec ushards(DbName::iodata()) -> [#shard{}].
ushards(DbName) ->
    Nodes = [node()|erlang:nodes()],
    ZoneMap = zone_map(Nodes),
    ushards(live_shards(DbName, Nodes), ZoneMap).

ushards(Shards0, ZoneMap) ->
    {L,S,D} = group_by_proximity(Shards0, ZoneMap),
    % Prefer shards in the local zone over shards in a different zone,
    % but sort each zone separately to ensure a consistent choice between
    % nodes in the same zone.
    Shards = choose_ushards(L ++ S) ++ choose_ushards(D),
    lists:ukeysort(#shard.range, Shards).

get_shard(DbName, Node, Range) ->
    mem3_shards:get(DbName, Node, Range).

local_shards(DbName) ->
    mem3_shards:local(DbName).

fold_shards(Fun, Acc) ->
    mem3_shards:fold(Fun, Acc).

sync_security() ->
    mem3_sync_security:go().

sync_security(Db) ->
    mem3_sync_security:go(dbname(Db)).

-spec choose_shards(DbName::iodata(), Options::list()) -> [#shard{}].
choose_shards(DbName, Options) when is_list(DbName) ->
    choose_shards(list_to_binary(DbName), Options);
choose_shards(DbName, Options) ->
    try shards(DbName)
    catch error:E when E==database_does_not_exist; E==badarg ->
        Nodes = mem3:nodes(),
        case get_placement(Options) of
            undefined ->
                choose_shards(DbName, Nodes, Options);
            Placement ->
                lists:flatmap(fun({Zone, N}) ->
                    NodesInZone = nodes_in_zone(Nodes, Zone),
                    Options1 = lists:keymerge(1, [{n,N}], Options),
                    choose_shards(DbName, NodesInZone, Options1)
                end, Placement)
        end
    end.

choose_shards(DbName, Nodes, Options) ->
    NodeCount = length(Nodes),
    Suffix = couch_util:get_value(shard_suffix, Options, ""),
    N = mem3_util:n_val(couch_util:get_value(n, Options), NodeCount),
    if N =:= 0 -> erlang:error(no_nodes_in_zone);
       true -> ok
    end,
    Q = mem3_util:to_integer(couch_util:get_value(q, Options,
        couch_config:get("cluster", "q", "8"))),
    %% rotate to a random entry in the nodelist for even distribution
    {A, B} = lists:split(crypto:rand_uniform(1,length(Nodes)+1), Nodes),
    RotatedNodes = B ++ A,
    mem3_util:create_partition_map(DbName, N, Q, RotatedNodes, Suffix).

get_placement(Options) ->
    case couch_util:get_value(placement, Options) of
        undefined ->
            case couch_config:get("cluster", "placement") of
                undefined ->
                    undefined;
                PlacementStr ->
                    decode_placement_string(PlacementStr)
            end;
        PlacementStr ->
            decode_placement_string(PlacementStr)
    end.

decode_placement_string(PlacementStr) ->
    [begin
         [Zone, N] = string:tokens(Rule, ":"),
         {list_to_binary(Zone), list_to_integer(N)}
     end || Rule <- string:tokens(PlacementStr, ",")].

-spec dbname(#shard{} | iodata()) -> binary().
dbname(#shard{dbname = DbName}) ->
    DbName;
dbname(<<"shards/", _:8/binary, "-", _:8/binary, "/", DbName/binary>>) ->
    list_to_binary(filename:rootname(binary_to_list(DbName)));
dbname(DbName) when is_list(DbName) ->
    dbname(list_to_binary(DbName));
dbname(DbName) when is_binary(DbName) ->
    DbName;
dbname(_) ->
    erlang:error(badarg).

nodes_in_zone(Nodes, Zone) ->
    [Node || Node <- Nodes, Zone == mem3:node_info(Node, <<"zone">>)].

live_shards(DbName, Nodes) ->
    [S || #shard{node=Node} = S <- shards(DbName), lists:member(Node, Nodes)].

zone_map(Nodes) ->
    [{Node, node_info(Node, <<"zone">>)} || Node <- Nodes].

group_by_proximity(Shards) ->
    Nodes = [N || #shard{node=N} <- lists:ukeysort(#shard.node, Shards)],
    group_by_proximity(Shards, zone_map(Nodes)).

group_by_proximity(Shards, ZoneMap) ->
    {Local, Remote} = lists:partition(fun(S) -> S#shard.node =:= node() end,
        Shards),
    LocalZone = proplists:get_value(node(), ZoneMap),
    Fun = fun(S) -> proplists:get_value(S#shard.node, ZoneMap) =:= LocalZone end,
    {SameZone, DifferentZone} = lists:partition(Fun, Remote),
    {Local, SameZone, DifferentZone}.

choose_ushards(Shards) ->
    Groups = group_by_range(lists:sort(Shards)),
    Fun = fun(Group, {N, Acc}) ->
        {N+1, [lists:nth(1 + N rem length(Group), Group) | Acc]} end,
    {_, Result} = lists:foldl(Fun, {0, []}, Groups),
    Result.

group_by_range(Shards) ->
    Groups0 = lists:foldl(fun(#shard{range=Range}=Shard, Dict) ->
        orddict:append(Range, Shard, Dict) end, orddict:new(), Shards),
    {_, Groups} = lists:unzip(Groups0),
    Groups.

% quorum functions

quorum(#db{name=DbName}) ->
    quorum(DbName);
quorum(DbName) ->
    n(DbName) div 2 + 1.
