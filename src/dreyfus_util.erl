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

-module(dreyfus_util).

-include("dreyfus.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([get_shards/2, sort/2, upgrade/1, export/1, time/2]).

get_shards(DbName, #index_query_args{stale=ok}) ->
    mem3:ushards(DbName);
get_shards(DbName, #index_query_args{stable=true}) ->
    mem3:ushards(DbName);
get_shards(DbName, #index_query_args{stale=false}) ->
    mem3:shards(DbName);
get_shards(DbName, Args) ->
    get_shards(DbName, upgrade(Args)).


-spec sort(Order :: relevance | [any()], [#sortable{}]) -> [#sortable{}].
sort(Sort, List0) ->
    {List1, Stash} = stash_items(List0),
    List2 = lists:sort(fun(A, B) -> sort(Sort, A, B) end, List1),
    unstash_items(List2, Stash).

stash_items(List) ->
    lists:unzip([stash_item(Item) || Item <- List]).

stash_item(Item) ->
    Ref = make_ref(),
    {Item#sortable{item=Ref}, {Ref, Item#sortable.item}}.

unstash_items(List, Stash) ->
    [unstash_item(Item, Stash) || Item <- List].

unstash_item(Stashed, Stash) ->
    {_, Item} = lists:keyfind(Stashed#sortable.item, 1, Stash),
    Stashed#sortable{item=Item}.

-spec sort(Order :: relevance | [any()], #sortable{}, #sortable{}) -> boolean().
sort(relevance, #sortable{}=A, #sortable{}=B) ->
    sort2(pad([<<"-">>], <<"">>, length(A#sortable.order)), A, B);
sort(Sort, #sortable{}=A, #sortable{}=B) when is_binary(Sort) ->
    sort2(pad([Sort], <<"">>, length(A#sortable.order)), A, B);
sort(Sort, #sortable{}=A, #sortable{}=B) when is_list(Sort) ->
    sort2(pad(Sort, <<"">>, length(A#sortable.order)), A, B).

-spec sort2([any()], #sortable{}, #sortable{}) -> boolean().
sort2([<<"-",_/binary>>|_], #sortable{order=[A|_]}, #sortable{order=[B|_]}) when A =/= B ->
    A > B;
sort2([_|_], #sortable{order=[A|_]}, #sortable{order=[B|_]}) when A =/= B ->
    A < B;
sort2([], #sortable{shard=#shard{range=A}}, #sortable{shard=#shard{range=B}}) ->
    % arbitrary tie-breaker
    A =< B;
sort2([_|Rest], #sortable{order=[_|RestA]}=SortableA, #sortable{order=[_|RestB]}=SortableB) ->
    sort2(Rest, SortableA#sortable{order=RestA}, SortableB#sortable{order=RestB}).

pad(List, _Padding, Length) when length(List) >= Length ->
    List;
pad(List, Padding, Length) ->
    pad(List ++ [Padding], Padding, Length).

upgrade(#index_query_args{}=Args) ->
    Args;
upgrade({index_query_args, Query, Limit, Stale, IncludeDocs, Bookmark,
         Sort, Grouping, Stable}) ->
    #index_query_args{
         q = Query,
         limit = Limit,
         stale = Stale,
         include_docs = IncludeDocs,
         bookmark = Bookmark,
         sort = Sort,
         grouping = Grouping,
         stable = Stable};
upgrade({index_query_args, Query, Limit, Stale, IncludeDocs, Bookmark,
         Sort, Grouping, Stable, Counts, Ranges, Drilldown}) ->
    #index_query_args{
         q = Query,
         limit = Limit,
         stale = Stale,
         include_docs = IncludeDocs,
         bookmark = Bookmark,
         sort = Sort,
         grouping = Grouping,
         stable = Stable,
         counts=Counts,
         ranges = Ranges,
         drilldown = Drilldown};
upgrade({index_query_args, Query, Limit, Stale, IncludeDocs, Bookmark,
         Sort, Grouping, Stable, Counts, Ranges, Drilldown,
         IncludeFields, HighlightFields, HighlightPreTag, HighlightPostTag,
         HighlightNumber, HighlightSize}) ->
    #index_query_args{
        q = Query,
        limit = Limit,
        stale = Stale,
        include_docs = IncludeDocs,
        bookmark = Bookmark,
        sort = Sort,
        grouping =  Grouping,
        stable = Stable,
        counts = Counts,
        ranges = Ranges,
        drilldown = Drilldown,
        include_fields = IncludeFields,
        highlight_fields = HighlightFields,
        highlight_pre_tag = HighlightPreTag,
        highlight_post_tag = HighlightPostTag,
        highlight_number = HighlightNumber,
        highlight_size = HighlightSize
    }.

export(#index_query_args{counts = nil, ranges = nil, drilldown = [],
    include_fields = nil, highlight_fields = nil} = Args) ->
    % Ensure existing searches work during the upgrade by creating an
    % #index_query_args record in the old format
    {index_query_args,
        Args#index_query_args.q,
        Args#index_query_args.limit,
        Args#index_query_args.stale,
        Args#index_query_args.include_docs,
        Args#index_query_args.bookmark,
        Args#index_query_args.sort,
        Args#index_query_args.grouping,
        Args#index_query_args.stable
    };
export(#index_query_args{include_fields = nil, highlight_fields = nil} = Args) ->
    {index_query_args,
        Args#index_query_args.q,
        Args#index_query_args.limit,
        Args#index_query_args.stale,
        Args#index_query_args.include_docs,
        Args#index_query_args.bookmark,
        Args#index_query_args.sort,
        Args#index_query_args.grouping,
        Args#index_query_args.stable,
        Args#index_query_args.counts,
        Args#index_query_args.ranges,
        Args#index_query_args.drilldown
    };
export(QueryArgs) ->
    QueryArgs.

time(Metric, {M, F, A}) when is_list(Metric) ->
    Start = os:timestamp(),
    try
        erlang:apply(M, F, A)
    after
        Length = timer:now_diff(os:timestamp(), Start) / 1000,
        couch_stats:update_histogram([dreyfus | Metric],  Length)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(SORT(T, L), lists:sort(fun(A, B) -> sort(T, A, B) end, L)).
-define(ASC, <<"">>).
-define(DESC, <<"-">>).

%% use proper for this...

empty_test() ->
    ?assertEqual([], ?SORT([], [])).

primary_asc_test() ->
    ?assertMatch([#sortable{order=[1]}, #sortable{order=[2]}],
                 ?SORT([?ASC], [#sortable{order=[2]}, #sortable{order=[1]}])).

primary_desc_test() ->
    ?assertMatch([#sortable{order=[2]}, #sortable{order=[1]}],
                 ?SORT([?DESC], [#sortable{order=[1]}, #sortable{order=[2]}])).

secondary_asc_test() ->
    ?assertMatch([#sortable{order=[1, 1]}, #sortable{order=[1, 2]}],
                 ?SORT([?ASC, ?ASC], [#sortable{order=[1, 2]}, #sortable{order=[1, 1]}])).

secondary_desc_test() ->
    ?assertMatch([#sortable{order=[1, 2]}, #sortable{order=[1, 1]}],
                 ?SORT([?DESC, ?DESC], [#sortable{order=[1, 1]}, #sortable{order=[1, 2]}])).

stash_test() ->
    {Stashed, Stash} = stash_items([#sortable{order=foo, item=bar}]),
    First = hd(Stashed),
    ?assert(is_reference(First#sortable.item)),
    Unstashed = hd(unstash_items(Stashed, Stash)),
    ?assertEqual(Unstashed#sortable.item, bar).

-endif.
