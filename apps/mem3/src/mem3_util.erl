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

-module(mem3_util).

-export([hash/1, name_shard/1, create_partition_map/4, build_shards/2,
    n_val/2, to_atom/1, to_integer/1, write_db_doc/1, delete_db_doc/1,
    load_shards_from_disk/1, load_shards_from_disk/2]).

-define(RINGTOP, 2 bsl 31).  % CRC32 space

-include("mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

hash(Item) when is_binary(Item) ->
    erlang:crc32(Item);
hash(Item) ->
    erlang:crc32(term_to_binary(Item)).

name_shard(#shard{dbname = DbName, range=[B,E]} = Shard) ->
    Name = ["shards/", couch_util:to_hex(<<B:32/integer>>), "-",
        couch_util:to_hex(<<E:32/integer>>), "/", DbName],
    Shard#shard{name = ?l2b(Name)}.

create_partition_map(DbName, N, Q, Nodes) ->
    UniqueShards = make_key_ranges((?RINGTOP) div Q, 0, []),
    Shards0 = lists:flatten([lists:duplicate(N, S) || S <- UniqueShards]),
    Shards1 = attach_nodes(Shards0, [], Nodes, []),
    [name_shard(S#shard{dbname=DbName}) || S <- Shards1].

make_key_ranges(_, CurrentPos, Acc) when CurrentPos >= ?RINGTOP ->
    Acc;
make_key_ranges(Increment, Start, Acc) ->
    case Start + 2*Increment of
    X when X > ?RINGTOP ->
        End = ?RINGTOP - 1;
    _ ->
        End = Start + Increment - 1
    end,
    make_key_ranges(Increment, End+1, [#shard{range=[Start, End]} | Acc]).

attach_nodes([], Acc, _, _) ->
    lists:reverse(Acc);
attach_nodes(Shards, Acc, [], UsedNodes) ->
    attach_nodes(Shards, Acc, lists:reverse(UsedNodes), []);
attach_nodes([S | Rest], Acc, [Node | Nodes], UsedNodes) ->
    attach_nodes(Rest, [S#shard{node=Node} | Acc], Nodes, [Node | UsedNodes]).

write_db_doc(Doc) ->
    {ok, Db} = couch_db:open(<<"dbs">>, []),
    try
        update_db_doc(Db, Doc)
    catch conflict ->
        ok % assume this is a race with another shard on this node
    after
        couch_db:close(Db)
    end.

update_db_doc(Db, #doc{id=Id, body=Body} = Doc) ->
    case couch_db:open_doc(Db, Id, []) of
    {not_found, _} ->
        {ok, _} = couch_db:update_doc(Db, Doc, []);
    {ok, #doc{body=Body}} ->
        ok;
    {ok, OldDoc} ->
        {ok, _} = couch_db:update_doc(Db, OldDoc#doc{body=Body}, [])
    end.

delete_db_doc(DocId) ->
    {ok, Db} = couch_db:open(<<"dbs">>, []),
    try
        delete_db_doc(Db, DocId)
    catch conflict ->
        ok
    after
        couch_db:close(Db)
    end.

delete_db_doc(Db, DocId) ->
    case couch_db:open_doc(Db, DocId, []) of
    {not_found, _} ->
        ok;
    {ok, OldDoc} ->
        {ok, _} = couch_db:update_doc(Db, OldDoc#doc{deleted=true}, [])
    end.

build_shards(DbName, DocProps) ->
    {ByNode} = couch_util:get_value(<<"by_node">>, DocProps, {[]}),
    lists:flatmap(fun({Node, Ranges}) ->
        lists:map(fun(Range) ->
            [B,E] = string:tokens(?b2l(Range), "-"),
            Beg = httpd_util:hexlist_to_integer(B),
            End = httpd_util:hexlist_to_integer(E),
            name_shard(#shard{
                dbname = DbName,
                node = to_atom(Node),
                range = [Beg, End]
            })
        end, Ranges)
    end, ByNode).

to_atom(Node) when is_binary(Node) ->
    list_to_atom(binary_to_list(Node));
to_atom(Node) when is_atom(Node) ->
    Node.

to_integer(N) when is_integer(N) ->
    N;
to_integer(N) when is_binary(N) ->
    list_to_integer(binary_to_list(N));
to_integer(N) when is_list(N) ->
    list_to_integer(N).

n_val(undefined, NodeCount) ->
    n_val(couch_config:get("cluster", "n", "3"), NodeCount);
n_val(N, NodeCount) when is_list(N) ->
    n_val(list_to_integer(N), NodeCount);
n_val(N, NodeCount) when is_integer(NodeCount), N > NodeCount ->
    ?LOG_ERROR("Request to create N=~p DB but only ~p node(s)", [N, NodeCount]),
    NodeCount;
n_val(N, _) when N < 1 ->
    1;
n_val(N, _) ->
    N.

load_shards_from_disk(DbName) when is_binary(DbName) ->
    {ok, Db} = couch_db:open(<<"dbs">>, []),
    try load_shards_from_db(Db, DbName) after couch_db:close(Db) end.

load_shards_from_db(#db{} = ShardDb, DbName) ->
    case couch_db:open_doc(ShardDb, DbName, []) of
    {ok, #doc{body = {Props}}} ->
        ?LOG_INFO("dbs cache miss for ~s", [DbName]),
        build_shards(DbName, Props);
    {not_found, _} ->
        erlang:error(database_does_not_exist)
    end.

load_shards_from_disk(DbName, DocId)->
    Shards = load_shards_from_disk(DbName),
    HashKey = hash(DocId),
    [S || #shard{range = [B,E]} = S <- Shards, B < HashKey, HashKey =< E].
