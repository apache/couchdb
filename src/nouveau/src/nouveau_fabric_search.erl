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

-module(nouveau_fabric_search).

-export([go/4]).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include("nouveau_int.hrl").

-record(state, {
    limit,
    sort,
    counters,
    search_results
}).

go(DbName, GroupId, IndexName, QueryArgs0) when is_binary(GroupId) ->
    {ok, DDoc} = fabric:open_doc(
        DbName,
        <<"_design/", GroupId/binary>>,
        [ejson_body]
    ),
    go(DbName, DDoc, IndexName, QueryArgs0);
go(DbName, #doc{} = DDoc, IndexName, QueryArgs0) ->
    case nouveau_util:design_doc_to_index(DbName, DDoc, IndexName) of
        {ok, Index} ->
            go(DbName, DDoc, IndexName, QueryArgs0, Index);
        {error, Reason} ->
            {error, Reason}
    end.

go(DbName, #doc{} = _DDoc, _IndexName, QueryArgs0, Index) ->
    Shards = get_shards(DbName, QueryArgs0),
    {PackedBookmark, #{limit := Limit, sort := Sort} = QueryArgs1} =
        maps:take(bookmark, QueryArgs0),
    TopN = maps:get(top_n, QueryArgs1, ?TOP_N_DEFAULT),
    Bookmark = nouveau_bookmark:unpack(DbName, PackedBookmark),
    Counters0 = lists:map(
        fun(#shard{} = Shard) ->
            After = maps:get(Shard#shard.range, Bookmark, null),
            %% scale TopN by number of shards so we don't clip a global topN
            %% label just because it wasn't in the topN locally.
            Ref = rexi:cast(
                Shard#shard.node,
                {nouveau_rpc, search, [
                    Shard#shard.name,
                    Index,
                    QueryArgs1#{'after' => After, top_n => TopN * length(Shards)}
                ]}
            ),
            Shard#shard{ref = Ref}
        end,
        Shards
    ),
    Counters = fabric_dict:init(Counters0, nil),
    Workers = fabric_dict:fetch_keys(Counters),
    RexiMon = fabric_util:create_monitors(Workers),
    State = #state{
        limit = Limit,
        sort = Sort,
        counters = Counters,
        search_results = #{}
    },
    try
        rexi_utils:recv(
            Workers,
            #shard.ref,
            fun handle_message/3,
            State,
            fabric_util:timeout("nouveau", "infinity"),
            fabric_util:timeout("nouveau_permsg", "3600000")
        )
    of
        {ok, SearchResults} ->
            NewBookmark = nouveau_bookmark:update(DbName, Bookmark, SearchResults),
            {ok, top_n_facets(simplify_hits(SearchResults#{bookmark => NewBookmark}), TopN)};
        {error, Reason} ->
            {error, Reason}
    after
        rexi_monitor:stop(RexiMon),
        fabric_util:cleanup(Workers)
    end.

handle_message({ok, Response}, Shard, State) ->
    case fabric_dict:lookup_element(Shard, State#state.counters) of
        undefined ->
            %% already heard from someone else in this range
            {ok, State};
        nil ->
            SearchResults = merge_search_results(State#state.search_results, Response, State),
            Counters1 = fabric_dict:store(Shard, ok, State#state.counters),
            Counters2 = fabric_view:remove_overlapping_shards(Shard, Counters1),
            State1 = State#state{counters = Counters2, search_results = SearchResults},
            case fabric_dict:any(nil, Counters2) of
                true ->
                    {ok, State1};
                false ->
                    {stop, SearchResults}
            end
    end;
handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _Shard, State) ->
    #state{counters = Counters0} = State,
    case fabric_util:remove_down_workers(Counters0, NodeRef, []) of
        {ok, Counters1} ->
            {ok, State#state{counters = Counters1}};
        error ->
            {error, {nodedown, <<"progress not possible">>}}
    end;
handle_message({error, Reason}, _Shard, _State) ->
    {error, Reason};
handle_message(Else, _Shard, _State) ->
    {error, Else}.

merge_search_results(A, B, #state{} = State) ->
    #{
        <<"total_hits">> => merge_total_hits(
            maps:get(<<"total_hits">>, A, 0), maps:get(<<"total_hits">>, B, 0)
        ),
        <<"total_hits_relation">> => merge_total_hits_relation(
            maps:get(<<"total_hits_relation">>, A, null),
            maps:get(<<"total_hits_relation">>, B, null)
        ),
        <<"hits">> => merge_hits(
            maps:get(<<"hits">>, A, []),
            maps:get(<<"hits">>, B, []),
            State#state.sort,
            State#state.limit
        ),
        <<"counts">> => merge_facets(
            maps:get(<<"counts">>, A, null), maps:get(<<"counts">>, B, null)
        ),
        <<"ranges">> => merge_facets(
            maps:get(<<"ranges">>, A, null), maps:get(<<"ranges">>, B, null)
        ),
        update_latency => max(
            maps:get(update_latency, A, 0), maps:get(update_latency, B, 0)
        )
    }.

merge_total_hits(TotalHitsA, TotalHitsB) ->
    TotalHitsA + TotalHitsB.

merge_total_hits_relation(A, B) when
    A == <<"GREATER_THAN_OR_EQUAL_TO">>; B == <<"GREATER_THAN_OR_EQUAL_TO">>
->
    <<"GREATER_THAN_OR_EQUAL_TO">>;
merge_total_hits_relation(A, B) when A == <<"EQUAL_TO">>; B == <<"EQUAL_TO">> ->
    <<"EQUAL_TO">>;
merge_total_hits_relation(null, null) ->
    %% not supported in selected Lucene version.
    null.

merge_hits(HitsA, HitsB, Sort, Limit) ->
    MergedHits = lists:merge(merge_fun(Sort), HitsA, HitsB),
    lists:sublist(MergedHits, Limit).

simplify_hits(SearchResults) ->
    #{<<"hits">> := Hits} = SearchResults,
    SearchResults#{<<"hits">> => lists:map(fun simplify_hit/1, Hits)}.

simplify_hit(#{} = Hit) ->
    #{<<"fields">> := Fields} = Hit,
    Hit#{<<"fields">> => simplify_fields(Fields)}.

simplify_fields(Fields) when is_list(Fields) ->
    Grouped = maps:groups_from_list(
        fun(#{<<"name">> := Name}) -> Name end,
        fun(#{<<"value">> := Value}) -> Value end,
        Fields
    ),
    %% If stored field has only one value, return it directly,
    %% otherwise, return an array of them.
    Fun = fun
        (_, [V]) -> V;
        (_, V) -> V
    end,
    maps:map(Fun, Grouped).

merge_fun(Sort) ->
    fun(HitA, HitB) ->
        OrderA = maps:get(<<"order">>, HitA),
        OrderB = maps:get(<<"order">>, HitB),
        compare_order(Sort, OrderA, OrderB)
    end.

%% no sort order specified which means sort by relevance (high to low)
compare_order(null, As, Bs) ->
    compare_order([<<"-">>], As, Bs);
%% server-side adds _id on the end of sort order if not present
compare_order([], [A], [B]) ->
    couch_ejson_compare:less(convert_item(A), convert_item(B)) < 1;
%% reverse order specified
compare_order([<<"-", _/binary>> | SortRest], [A | ARest], [B | BRest]) ->
    case couch_ejson_compare:less(convert_item(B), convert_item(A)) of
        0 ->
            compare_order(SortRest, ARest, BRest);
        Less ->
            Less < 1
    end;
%% forward order specified
compare_order([_ | SortRest], [A | ARest], [B | BRest]) ->
    case couch_ejson_compare:less(convert_item(A), convert_item(B)) of
        0 ->
            compare_order(SortRest, ARest, BRest);
        Less ->
            Less < 1
    end.

convert_item(Item) ->
    case maps:get(<<"@type">>, Item) of
        <<"bytes">> ->
            base64:decode(maps:get(<<"value">>, Item));
        _ ->
            maps:get(<<"value">>, Item)
    end.

top_n_facets(SearchResults, TopN) ->
    SearchResults#{
        <<"counts">> => top_n_facets(<<"counts">>, SearchResults, TopN),
        <<"ranges">> => top_n_facets(<<"ranges">>, SearchResults, TopN)
    }.

top_n_facets(Key, SearchResults, TopN) ->
    Facets = maps:get(Key, SearchResults, null),
    top_n_facets_int(Facets, TopN).

top_n_facets_int(null, _TopN) ->
    null;
top_n_facets_int(Facets, TopN) when is_map(Facets), is_integer(TopN), TopN > 0 ->
    MapFun = fun(_, Group) ->
        maps:from_list(
            lists:sublist(
                lists:sort(fun facet_sort/2, maps:to_list(Group)),
                TopN
            )
        )
    end,
    maps:map(MapFun, Facets).

%% sort by value (high to low), tie-break on label (lowest wins)
facet_sort({_K1, V1}, {_K2, V2}) when V1 > V2 ->
    true;
facet_sort({_K1, V1}, {_K2, V2}) when V1 < V2 ->
    false;
facet_sort({K1, V}, {K2, V}) ->
    couch_ejson_compare:less_json(K1, K2).

merge_facets(FacetsA, null) ->
    FacetsA;
merge_facets(null, FacetsB) ->
    FacetsB;
merge_facets(FacetsA, FacetsB) ->
    Combiner = fun(_, V1, V2) -> maps:merge_with(fun(_, V3, V4) -> V3 + V4 end, V1, V2) end,
    maps:merge_with(Combiner, FacetsA, FacetsB).

get_shards(DbName, #{partition := Partition}) when is_binary(Partition) ->
    PartitionId = couch_partition:shard_key(Partition),
    mem3:shards(DbName, PartitionId);
get_shards(DbName, _QueryArgs) ->
    mem3:shards(DbName).
