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

-export([hash/1, name_shard/2, create_partition_map/5, build_shards/2,
    n_val/2, to_atom/1, to_integer/1, write_db_doc/1, delete_db_doc/1,
    shard_info/1, ensure_exists/1, open_db_doc/1]).
-export([is_deleted/1, rotate_list/2]).

%% do not use outside mem3.
-export([build_ordered_shards/2, downcast/1]).

-export([create_partition_map/4, name_shard/1]).
-deprecated({create_partition_map, 4, eventually}).
-deprecated({name_shard, 1, eventually}).

-define(RINGTOP, 2 bsl 31).  % CRC32 space

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

hash(Item) when is_binary(Item) ->
    erlang:crc32(Item);
hash(Item) ->
    erlang:crc32(term_to_binary(Item)).

name_shard(Shard) ->
    name_shard(Shard, "").

name_shard(#shard{dbname = DbName, range=Range} = Shard, Suffix) ->
    Name = make_name(DbName, Range, Suffix),
    Shard#shard{name = ?l2b(Name)};

name_shard(#ordered_shard{dbname = DbName, range=Range} = Shard, Suffix) ->
    Name = make_name(DbName, Range, Suffix),
    Shard#ordered_shard{name = ?l2b(Name)}.

make_name(DbName, [B,E], Suffix) ->
    ["shards/", couch_util:to_hex(<<B:32/integer>>), "-",
     couch_util:to_hex(<<E:32/integer>>), "/", DbName, Suffix].

create_partition_map(DbName, N, Q, Nodes) ->
    create_partition_map(DbName, N, Q, Nodes, "").

create_partition_map(DbName, N, Q, Nodes, Suffix) ->
    UniqueShards = make_key_ranges((?RINGTOP) div Q, 0, []),
    Shards0 = lists:flatten([lists:duplicate(N, S) || S <- UniqueShards]),
    Shards1 = attach_nodes(Shards0, [], Nodes, []),
    [name_shard(S#shard{dbname=DbName}, Suffix) || S <- Shards1].

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

open_db_doc(DocId) ->
    DbName = ?l2b(config:get("mem3", "shards_db", "_dbs")),
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    try couch_db:open_doc(Db, DocId, [ejson_body]) after couch_db:close(Db) end.

write_db_doc(Doc) ->
    DbName = ?l2b(config:get("mem3", "shards_db", "_dbs")),
    write_db_doc(DbName, Doc, true).

write_db_doc(DbName, #doc{id=Id, body=Body} = Doc, ShouldMutate) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    try couch_db:open_doc(Db, Id, [ejson_body]) of
    {ok, #doc{body = Body}} ->
        % the doc is already in the desired state, we're done here
        ok;
    {not_found, _} when ShouldMutate ->
        try couch_db:update_doc(Db, Doc, []) of
        {ok, _} ->
            ok
        catch conflict ->
            % check to see if this was a replication race or a different edit
            write_db_doc(DbName, Doc, false)
        end;
    _ ->
        % the doc already exists in a different state
        conflict
    after
        couch_db:close(Db)
    end.

delete_db_doc(DocId) ->
    gen_server:cast(mem3_shards, {cache_remove, DocId}),
    DbName = ?l2b(config:get("mem3", "shards_db", "_dbs")),
    delete_db_doc(DbName, DocId, true).

delete_db_doc(DbName, DocId, ShouldMutate) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_CTX]),
    {ok, Revs} = couch_db:open_doc_revs(Db, DocId, all, []),
    try [Doc#doc{deleted=true} || {ok, #doc{deleted=false}=Doc} <- Revs] of
    [] ->
        not_found;
    Docs when ShouldMutate ->
        try couch_db:update_docs(Db, Docs, []) of
        {ok, _} ->
            ok
        catch conflict ->
            % check to see if this was a replication race or if leafs survived
            delete_db_doc(DbName, DocId, false)
        end;
    _ ->
        % we have live leafs that we aren't allowed to delete. let's bail
        conflict
    after
        couch_db:close(Db)
    end.

%% Always returns original #shard records.
-spec build_shards(binary(), list()) -> [#shard{}].
build_shards(DbName, DocProps) ->
    build_shards_by_node(DbName, DocProps).

%% Will return #ordered_shard records if by_node and by_range
%% are symmetrical, #shard records otherwise.
-spec build_ordered_shards(binary(), list()) ->
    [#shard{}] | [#ordered_shard{}].
build_ordered_shards(DbName, DocProps) ->
    ByNode = build_shards_by_node(DbName, DocProps),
    ByRange = build_shards_by_range(DbName, DocProps),
    Symmetrical = lists:sort(ByNode) =:= lists:sort(downcast(ByRange)),
    case Symmetrical of
        true  -> ByRange;
        false -> ByNode
    end.

build_shards_by_node(DbName, DocProps) ->
    {ByNode} = couch_util:get_value(<<"by_node">>, DocProps, {[]}),
    Suffix = couch_util:get_value(<<"shard_suffix">>, DocProps, ""),
    lists:flatmap(fun({Node, Ranges}) ->
        lists:map(fun(Range) ->
            [B,E] = string:tokens(?b2l(Range), "-"),
            Beg = httpd_util:hexlist_to_integer(B),
            End = httpd_util:hexlist_to_integer(E),
            name_shard(#shard{
                dbname = DbName,
                node = to_atom(Node),
                range = [Beg, End]
            }, Suffix)
        end, Ranges)
    end, ByNode).

build_shards_by_range(DbName, DocProps) ->
    {ByRange} = couch_util:get_value(<<"by_range">>, DocProps, {[]}),
    Suffix = couch_util:get_value(<<"shard_suffix">>, DocProps, ""),
    lists:flatmap(fun({Range, Nodes}) ->
        lists:map(fun({Node, Order}) ->
            [B,E] = string:tokens(?b2l(Range), "-"),
            Beg = httpd_util:hexlist_to_integer(B),
            End = httpd_util:hexlist_to_integer(E),
            name_shard(#ordered_shard{
                dbname = DbName,
                node = to_atom(Node),
                range = [Beg, End],
                order = Order
            }, Suffix)
        end, lists:zip(Nodes, lists:seq(1, length(Nodes))))
    end, ByRange).

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
    n_val(config:get("cluster", "n", "3"), NodeCount);
n_val(N, NodeCount) when is_list(N) ->
    n_val(list_to_integer(N), NodeCount);
n_val(N, NodeCount) when is_integer(NodeCount), N > NodeCount ->
    couch_log:error("Request to create N=~p DB but only ~p node(s)", [N, NodeCount]),
    NodeCount;
n_val(N, _) when N < 1 ->
    1;
n_val(N, _) ->
    N.

shard_info(DbName) ->
    [{n, mem3:n(DbName)},
     {q, length(mem3:shards(DbName)) div mem3:n(DbName)}].

ensure_exists(DbName) when is_list(DbName) ->
    ensure_exists(list_to_binary(DbName));
ensure_exists(DbName) ->
    Options = [nologifmissing, sys_db, {create_if_missing, true}, ?ADMIN_CTX],
    case couch_db:open(DbName, Options) of
    {ok, Db} ->
        {ok, Db};
    file_exists ->
        couch_db:open(DbName, [sys_db, ?ADMIN_CTX])
    end.


is_deleted(Change) ->
    case couch_util:get_value(<<"deleted">>, Change) of
    undefined ->
        % keep backwards compatibility for a while
        couch_util:get_value(deleted, Change, false);
    Else ->
        Else
    end.

rotate_list(_Key, []) ->
    [];
rotate_list(Key, List) when not is_binary(Key) ->
    rotate_list(term_to_binary(Key), List);
rotate_list(Key, List) ->
    {H, T} = lists:split(erlang:crc32(Key) rem length(List), List),
    T ++ H.

downcast(#shard{}=S) ->
    S;
downcast(#ordered_shard{}=S) ->
    #shard{
       name = S#ordered_shard.name,
       node = S#ordered_shard.node,
       dbname = S#ordered_shard.dbname,
       range = S#ordered_shard.range,
       ref = S#ordered_shard.ref
      };
downcast(Shards) when is_list(Shards) ->
    [downcast(Shard) || Shard <- Shards].
