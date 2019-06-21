% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.


%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(dreyfus_bookmark).

-include("dreyfus.hrl").
-include_lib("mem3/include/mem3.hrl").

-export([
    update/3,
    unpack/2,
    pack/1,
    add_missing_shards/2
]).


update(_Sort, Bookmark, []) ->
    Bookmark;
update(relevance, Bookmark, [#sortable{} = Sortable | Rest]) ->
    #sortable{
        order = [Score, Doc],
        shard = Shard
    } = Sortable,
    B1 = fabric_dict:store(Shard, {Score, Doc}, Bookmark),
    B2 = fabric_view:remove_overlapping_shards(Shard, B1),
    update(relevance, B2, Rest);
update(Sort, Bookmark, [#sortable{} = Sortable | Rest]) ->
    #sortable{
        order = Order,
        shard = Shard
    } = Sortable,
    B1 = fabric_dict:store(Shard, Order, Bookmark),
    B2 = fabric_view:remove_overlapping_shards(Shard, B1),
    update(Sort, B2, Rest).


unpack(DbName, #index_query_args{bookmark=nil} = Args) ->
    fabric_dict:init(dreyfus_util:get_shards(DbName, Args), nil);
unpack(DbName, #index_query_args{} = Args) ->
    unpack(DbName, Args#index_query_args.bookmark);
unpack(DbName, Packed) when is_binary(Packed) ->
    lists:map(fun({Node, Range, After}) ->
        case mem3:get_shard(DbName, Node, Range) of
            {ok, Shard} ->
                {Shard, After};
            {error, not_found} ->
                PlaceHolder = #shard{
                    node = Node,
                    range = Range,
                    dbname = DbName,
                    _='_'
                },
                {PlaceHolder, After}
        end
    end, binary_to_term(couch_util:decodeBase64Url(Packed))).


pack(nil) ->
    null;
pack(Workers) ->
    Workers1 = [{N,R,A} || {#shard{node=N, range=R}, A} <- Workers, A =/= nil],
    Bin = term_to_binary(Workers1, [compressed, {minor_version,1}]),
    couch_util:encodeBase64Url(Bin).


add_missing_shards(Bookmark, LiveShards) ->
    {BookmarkShards, _} = lists:unzip(Bookmark),
    add_missing_shards(Bookmark, BookmarkShards, LiveShards).


add_missing_shards(Bookmark, _, []) ->
    Bookmark;
add_missing_shards(Bookmark, BMShards, [H | T]) ->
    Bookmark1 = case lists:keymember(H#shard.range, #shard.range, BMShards) of
        true -> Bookmark;
        false -> fabric_dict:store(H, nil, Bookmark)
    end,
    add_missing_shards(Bookmark1, BMShards, T).
