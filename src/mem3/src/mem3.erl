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
    choose_shards/2, n/1, n/2, dbname/1, ushards/1]).
-export([get_shard/3, local_shards/1, shard_suffix/1, fold_shards/2]).
-export([sync_security/0, sync_security/1]).
-export([compare_nodelists/0, compare_shards/1]).
-export([quorum/1, group_by_proximity/1]).
-export([live_shards/2]).
-export([belongs/2]).
-export([get_placement/1]).

%% For mem3 use only.
-export([name/1, node/1, range/1]).

-include_lib("mem3/include/mem3.hrl").
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
    n(DbName, <<"foo">>).

n(DbName, DocId) ->
    length(mem3:shards(DbName, DocId)).

-spec nodes() -> [node()].
nodes() ->
    mem3_nodes:get_nodelist().

node_info(Node, Key) ->
    mem3_nodes:get_node_info(Node, Key).

-spec shards(DbName::iodata()) -> [#shard{}].
shards(DbName) ->
    shards_int(DbName, []).

shards_int(DbName, Options) when is_list(DbName) ->
    shards_int(list_to_binary(DbName), Options);
shards_int(DbName, Options) ->
    Ordered = lists:member(ordered, Options),
    ShardDbName =
        list_to_binary(config:get("mem3", "shards_db", "_dbs")),
    case DbName of
    ShardDbName when Ordered ->
        %% shard_db is treated as a single sharded db to support calls to db_info
        %% and view_all_docs
        [#ordered_shard{
            node = node(),
            name = ShardDbName,
            dbname = ShardDbName,
            range = [0, (2 bsl 31)-1],
            order = undefined}];
    ShardDbName ->
        %% shard_db is treated as a single sharded db to support calls to db_info
        %% and view_all_docs
        [#shard{
            node = node(),
            name = ShardDbName,
            dbname = ShardDbName,
            range = [0, (2 bsl 31)-1]}];
    _ ->
        mem3_shards:for_db(DbName, Options)
    end.

-spec shards(DbName::iodata(), DocId::binary()) -> [#shard{}].
shards(DbName, DocId) ->
    shards_int(DbName, DocId, []).

shards_int(DbName, DocId, Options) when is_list(DbName) ->
    shards_int(list_to_binary(DbName), DocId, Options);
shards_int(DbName, DocId, Options) when is_list(DocId) ->
    shards_int(DbName, list_to_binary(DocId), Options);
shards_int(DbName, DocId, Options) ->
    mem3_shards:for_docid(DbName, DocId, Options).


-spec ushards(DbName::iodata()) -> [#shard{}].
ushards(DbName) ->
    Nodes = [node()|erlang:nodes()],
    ZoneMap = zone_map(Nodes),
    Shards = ushards(DbName, live_shards(DbName, Nodes, [ordered]), ZoneMap),
    mem3_util:downcast(Shards).

ushards(DbName, Shards0, ZoneMap) ->
    {L,S,D} = group_by_proximity(Shards0, ZoneMap),
    % Prefer shards in the local zone over shards in a different zone,
    % but sort each zone separately to ensure a consistent choice between
    % nodes in the same zone.
    Shards = choose_ushards(DbName, L ++ S) ++ choose_ushards(DbName, D),
    lists:ukeysort(#shard.range, Shards).

get_shard(DbName, Node, Range) ->
    mem3_shards:get(DbName, Node, Range).

local_shards(DbName) ->
    mem3_shards:local(DbName).

shard_suffix(DbName0) when is_binary(DbName0) ->
    Shard = hd(shards(DbName0)),
    <<"shards/", _:8/binary, "-", _:8/binary, "/", DbName/binary>> =
        Shard#shard.name,
    filename:extension(binary_to_list(DbName));
shard_suffix(Db) ->
    shard_suffix(couch_db:name(Db)).

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
        Nodes = allowed_nodes(),
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
        config:get("cluster", "q", "8"))),
    %% rotate to a random entry in the nodelist for even distribution
    {A, B} = lists:split(crypto:rand_uniform(1,length(Nodes)+1), Nodes),
    RotatedNodes = B ++ A,
    mem3_util:create_partition_map(DbName, N, Q, RotatedNodes, Suffix).

get_placement(Options) ->
    case couch_util:get_value(placement, Options) of
        undefined ->
            case config:get("cluster", "placement") of
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

%% @doc Determine if DocId belongs in shard (identified by record or filename)
belongs(#shard{}=Shard, DocId) when is_binary(DocId) ->
    [Begin, End] = range(Shard),
    belongs(Begin, End, DocId);
belongs(<<"shards/", _/binary>> = ShardName, DocId) when is_binary(DocId) ->
    [Begin, End] = range(ShardName),
    belongs(Begin, End, DocId);
belongs(DbName, DocId) when is_binary(DbName), is_binary(DocId) ->
    true.

belongs(Begin, End, DocId) ->
    HashKey = mem3_util:hash(DocId),
    Begin =< HashKey andalso HashKey =< End.

range(#shard{range = Range}) ->
    Range;
range(#ordered_shard{range = Range}) ->
    Range;
range(<<"shards/", Start:8/binary, "-", End:8/binary, "/", _/binary>>) ->
    [httpd_util:hexlist_to_integer(binary_to_list(Start)),
     httpd_util:hexlist_to_integer(binary_to_list(End))].

allowed_nodes() ->
    [Node || Node <- mem3:nodes(), mem3:node_info(Node, <<"decom">>) =/= true].

nodes_in_zone(Nodes, Zone) ->
    [Node || Node <- Nodes, Zone == mem3:node_info(Node, <<"zone">>)].

live_shards(DbName, Nodes) ->
    live_shards(DbName, Nodes, []).

live_shards(DbName, Nodes, Options) ->
    [S || S <- shards_int(DbName, Options), lists:member(mem3:node(S), Nodes)].

zone_map(Nodes) ->
    [{Node, node_info(Node, <<"zone">>)} || Node <- Nodes].

group_by_proximity(Shards) ->
    Nodes = [mem3:node(S) || S <- lists:ukeysort(#shard.node, Shards)],
    group_by_proximity(Shards, zone_map(Nodes)).

group_by_proximity(Shards, ZoneMap) ->
    {Local, Remote} = lists:partition(fun(S) -> mem3:node(S) =:= node() end,
        Shards),
    LocalZone = proplists:get_value(node(), ZoneMap),
    Fun = fun(S) -> proplists:get_value(mem3:node(S), ZoneMap) =:= LocalZone end,
    {SameZone, DifferentZone} = lists:partition(Fun, Remote),
    {Local, SameZone, DifferentZone}.

choose_ushards(DbName, Shards) ->
    Groups0 = group_by_range(Shards),
    Groups1 = [mem3_util:rotate_list({DbName, R}, order_shards(G))
               || {R, G} <- Groups0],
    [hd(G) || G <- Groups1].

order_shards([#ordered_shard{}|_]=OrderedShards) ->
    lists:keysort(#ordered_shard.order, OrderedShards);
order_shards(UnorderedShards) ->
    UnorderedShards.

group_by_range(Shards) ->
    lists:foldl(fun(Shard, Dict) ->
        orddict:append(mem3:range(Shard), Shard, Dict) end, orddict:new(), Shards).

% quorum functions

quorum(DbName) when is_binary(DbName) ->
    n(DbName) div 2 + 1;
quorum(Db) ->
    quorum(couch_db:name(Db)).


node(#shard{node=Node}) ->
    Node;
node(#ordered_shard{node=Node}) ->
    Node.

name(#shard{name=Name}) ->
    Name;
name(#ordered_shard{name=Name}) ->
    Name.
