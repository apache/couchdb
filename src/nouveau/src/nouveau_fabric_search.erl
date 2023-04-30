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
    {ok, Index} = nouveau_util:design_doc_to_index(DbName, DDoc, IndexName),
    Shards = get_shards(DbName, QueryArgs0),
    {PackedBookmark, #{limit := Limit, sort := Sort} = QueryArgs1} =
        maps:take(bookmark, QueryArgs0),
    Bookmark = nouveau_bookmark:unpack(DbName, PackedBookmark),
    Counters0 = lists:map(
        fun(#shard{} = Shard) ->
            After = maps:get(Shard#shard.range, Bookmark, null),
            Ref = rexi:cast(
                Shard#shard.node,
                {nouveau_rpc, search, [Shard#shard.name, Index, QueryArgs1#{'after' => After}]}
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
            {ok, simplify_hits(SearchResults#{bookmark => NewBookmark})};
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
            {ok, Counters1};
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
            maps:get(<<"counts">>, A, null), maps:get(<<"counts">>, B, null), State#state.limit
        ),
        <<"ranges">> => merge_facets(
            maps:get(<<"ranges">>, A, null), maps:get(<<"ranges">>, B, null), State#state.limit
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
    Fun = fun(Field, Acc) ->
        {Key, Value} = simplify_field(Field),
        Acc#{Key => Value}
    end,
    lists:foldl(Fun, #{}, Fields).

simplify_field(#{<<"@type">> := <<"stored">>} = Field) ->
    #{<<"name">> := Key, <<"value">> := Value} = Field,
    {Key, Value}.

merge_fun(Sort) ->
    fun(HitA, HitB) ->
        OrderA = maps:get(<<"order">>, HitA),
        OrderB = maps:get(<<"order">>, HitB),
        compare_order(Sort, OrderA, OrderB)
    end.

%% no sort order specified
compare_order(null, [A | ARest], [B | BRest]) ->
    case couch_ejson_compare:less(convert_item(A), convert_item(B)) of
        0 ->
            compare_order(null, ARest, BRest);
        Less ->
            Less < 1
    end;
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

merge_facets(FacetsA, null, _Limit) ->
    FacetsA;
merge_facets(null, FacetsB, _Limit) ->
    FacetsB;
merge_facets(FacetsA, FacetsB, _Limit) ->
    Combiner = fun(_, V1, V2) -> maps:merge_with(fun(_, V3, V4) -> V3 + V4 end, V1, V2) end,
    maps:merge_with(Combiner, FacetsA, FacetsB).

get_shards(DbName, #{partition := Partition}) when is_binary(Partition) ->
    PartitionId = couch_partition:shard_key(Partition),
    mem3:shards(DbName, PartitionId);
get_shards(DbName, _QueryArgs) ->
    mem3:shards(DbName).
