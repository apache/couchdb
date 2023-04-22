%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(nouveau_bookmark).

-include_lib("mem3/include/mem3.hrl").

-export([new/0, update/3, unpack/2, pack/1, to_ejson/1]).

new() ->
    #{}.

%% Form a bookmark from the last contribution from each shard range
update(DbName, PreviousBookmark, SearchResults) when is_binary(PreviousBookmark) ->
    update(DbName, unpack(DbName, PreviousBookmark), SearchResults);
update(DbName, {EJson}, SearchResults) when is_list(EJson) ->
    update(DbName, from_ejson({EJson}), SearchResults);
update(DbName, PreviousBookmark, SearchResults) when is_map(PreviousBookmark) ->
    #{<<"hits">> := Hits} = SearchResults,
    NewBookmark0 = lists:foldl(
        fun(#{<<"id">> := Id, <<"order">> := Order}, Acc) ->
            Acc#{range_of(DbName, Id) => Order}
        end,
        new(),
        Hits
    ),
    maps:merge(PreviousBookmark, NewBookmark0).

range_of(DbName, DocId) when is_binary(DbName), is_binary(DocId) ->
    [#shard{range = Range} | _] = mem3_shards:for_docid(DbName, DocId),
    Range;
range_of(DbName, Order) when is_binary(DbName), is_list(Order) ->
    #{<<"@type">> := <<"string">>, <<"value">> := DocId} = lists:last(Order),
    range_of(DbName, DocId).

unpack(_DbName, Empty) when Empty == undefined; Empty == nil; Empty == null ->
    new();
unpack(DbName, PackedBookmark) when is_list(PackedBookmark) ->
    unpack(DbName, list_to_binary(PackedBookmark));
unpack(DbName, PackedBookmark) when is_binary(PackedBookmark) ->
    Bookmark = jiffy:decode(base64:decode(PackedBookmark), [return_maps]),
    maps:from_list([{range_of(DbName, V), V} || V <- Bookmark]).

pack(nil) ->
    null;
pack({EJson}) when is_list(EJson) ->
    pack(from_ejson(EJson));
pack(UnpackedBookmark) when is_map(UnpackedBookmark) ->
    base64:encode(jiffy:encode(maps:values(UnpackedBookmark))).

%% legacy use of ejson within mango
from_ejson({Props}) ->
    maps:from_list(Props).

to_ejson(Bookmark) when is_map(Bookmark) ->
    {maps:to_list(Bookmark)}.
