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
    choose_shards/2, n/1, dbname/1, ushards/1]).
-export([compare_nodelists/0, compare_shards/1]).
-export([quorum/1]).

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
        try ets:lookup(partitions, DbName) of
        [] ->
            mem3_util:load_shards_from_disk(DbName);
        Else ->
            Else
        catch error:badarg ->
        mem3_util:load_shards_from_disk(DbName)
        end
    end.

-spec shards(DbName::iodata(), DocId::binary()) -> [#shard{}].
shards(DbName, DocId) when is_list(DbName) ->
    shards(list_to_binary(DbName), DocId);
shards(DbName, DocId) when is_list(DocId) ->
    shards(DbName, list_to_binary(DocId));
shards(DbName, DocId) ->
    HashKey = mem3_util:hash(DocId),
    Head = #shard{
        name = '_',
        node = '_',
        dbname = DbName,
        range = ['$1','$2'],
        ref = '_'
    },
    Conditions = [{'=<', '$1', HashKey}, {'=<', HashKey, '$2'}],
    try ets:select(partitions, [{Head, Conditions, ['$_']}]) of
    [] ->
        mem3_util:load_shards_from_disk(DbName, DocId);
    Shards ->
        Shards
    catch error:badarg ->
        mem3_util:load_shards_from_disk(DbName, DocId)
    end.

ushards(DbName) ->
    Shards = mem3:shards(DbName),
    Nodes = rotate_nodes(DbName, live_nodes()),
    Buckets = bucket_by_range(Shards),
    choose_ushards(Buckets, Nodes).

rotate_nodes(DbName, Nodes) ->
    {H, T} = lists:split(erlang:crc32(DbName) rem length(Nodes), Nodes),
    T ++ H.

live_nodes() ->
    lists:sort([node()|erlang:nodes()]).

bucket_by_range(Shards) ->
    Buckets0 = lists:foldl(fun(#shard{range=Range}=Shard, Dict) ->
        orddict:append(Range, Shard, Dict) end, orddict:new(), Shards),
    {_, Buckets} = lists:unzip(Buckets0),
    Buckets.

choose_ushards(Buckets, Nodes) ->
    choose_ushards(Buckets, Nodes, []).

choose_ushards([], _, Acc) ->
    lists:reverse(Acc);
choose_ushards([Bucket|RestBuckets], Nodes, Acc) ->
    #shard{node=Node} = Shard = first_match(Bucket, Bucket, Nodes),
    choose_ushards(RestBuckets, lists:delete(Node, Nodes) ++ [Node],
        [Shard | Acc]).

first_match([], [#shard{range=Range}|_], []) ->
    throw({range_not_available, Range});
first_match([#shard{node=Node}=Shard|_], _, [Node|_]) ->
    Shard;
first_match([], Shards, [_|RestNodes]) ->
    first_match(Shards, Shards, RestNodes);
first_match([_|RestShards], Shards, Nodes) ->
    first_match(RestShards, Shards, Nodes).

-spec choose_shards(DbName::iodata(), Options::list()) -> [#shard{}].
choose_shards(DbName, Options) when is_list(DbName) ->
    choose_shards(list_to_binary(DbName), Options);
choose_shards(DbName, Options) ->
    try shards(DbName)
    catch error:E when E==database_does_not_exist; E==badarg ->
        Nodes = mem3:nodes(),
        NodeCount = length(Nodes),
        Zones = zones(Nodes),
        ZoneCount = length(Zones),
        N = mem3_util:n_val(couch_util:get_value(n, Options), NodeCount),
        Q = mem3_util:to_integer(couch_util:get_value(q, Options,
            couch_config:get("cluster", "q", "8"))),
        Z = mem3_util:z_val(couch_util:get_value(z, Options), NodeCount, ZoneCount),
        Suffix = couch_util:get_value(shard_suffix, Options, ""),
        ChosenZones = lists:sublist(shuffle(Zones), Z),
        lists:flatmap(
            fun({Zone, N1}) ->
                Nodes1 = nodes_in_zone(Nodes, Zone),
                {A, B} = lists:split(crypto:rand_uniform(1,length(Nodes1)+1), Nodes1),
                RotatedNodes = B ++ A,
                mem3_util:create_partition_map(DbName, erlang:min(N1,length(Nodes1)),
                    Q, RotatedNodes, Suffix)
            end,
            lists:zip(ChosenZones, apportion(N, Z)))
    end.

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


zones(Nodes) ->
    lists:usort([mem3:node_info(Node, <<"zone">>) || Node <- Nodes]).

nodes_in_zone(Nodes, Zone) ->
    [Node || Node <- Nodes, Zone == mem3:node_info(Node, <<"zone">>)].

shuffle(List) ->
    %% Determine the log n portion then randomize the list.
    randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
    randomize(List);
randomize(T, List) ->
    lists:foldl(fun(_E, Acc) -> randomize(Acc) end,
                randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
    D = lists:map(fun(A) -> {random:uniform(), A} end, List),
    {_, D1} = lists:unzip(lists:keysort(1, D)),
    D1.

apportion(Shares, Ways) ->
    apportion(Shares, lists:duplicate(Ways, 0), Shares).

apportion(_Shares, Acc, 0) ->
    Acc;
apportion(Shares, Acc, Remaining) ->
    N = Remaining rem length(Acc),
    [H|T] = lists:nthtail(N, Acc),
    apportion(Shares, lists:sublist(Acc, N) ++ [H+1|T], Remaining - 1).

% quorum functions

quorum(#db{name=DbName}) ->
    quorum(DbName);
quorum(DbName) ->
    n(DbName) div 2 + 1.
