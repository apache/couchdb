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

-module(dreyfus_fabric_search).

-include("dreyfus.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([go/4]).

-record(state, {
    limit,
    sort,
    top_docs,
    counters,
    start_args,
    replacements,
    ring_opts
}).

go(DbName, GroupId, IndexName, QueryArgs) when is_binary(GroupId) ->
    {ok, DDoc} = fabric:open_doc(DbName, <<"_design/", GroupId/binary>>,
        [ejson_body]),
    dreyfus_util:maybe_deny_index(DbName, GroupId, IndexName),
    go(DbName, DDoc, IndexName, QueryArgs);

go(DbName, DDoc, IndexName, #index_query_args{bookmark=nil}=QueryArgs) ->
    DesignName = dreyfus_util:get_design_docid(DDoc),
    dreyfus_util:maybe_deny_index(DbName, DesignName, IndexName),
    Shards = dreyfus_util:get_shards(DbName, QueryArgs),
    RingOpts = dreyfus_util:get_ring_opts(QueryArgs, Shards),
    Workers = fabric_util:submit_jobs(Shards, dreyfus_rpc, search,
                          [DDoc, IndexName, dreyfus_util:export(QueryArgs)]),
    Counters = fabric_dict:init(Workers, nil),
    go(DbName, DDoc, IndexName, QueryArgs, Counters, Counters, RingOpts);

go(DbName, DDoc, IndexName, #index_query_args{}=QueryArgs) ->
    Bookmark0 = try dreyfus_bookmark:unpack(DbName, QueryArgs)
    catch
        _:_ ->
            throw({bad_request, "Invalid bookmark parameter supplied"})
    end,
    Shards = dreyfus_util:get_shards(DbName, QueryArgs),
    LiveNodes = [node() | nodes()],
    LiveShards = [S || #shard{node=Node} = S <- Shards, lists:member(Node, LiveNodes)],
    Bookmark1 = dreyfus_bookmark:add_missing_shards(Bookmark0, LiveShards),
    Counters0 = lists:flatmap(fun({#shard{name=Name, node=N} = Shard, After}) ->
        QueryArgs1 = dreyfus_util:export(QueryArgs#index_query_args{
            bookmark = After
        }),
        case lists:member(Shard, LiveShards) of
        true ->
            Ref = rexi:cast(N, {dreyfus_rpc, search,
                                [Name, DDoc, IndexName, QueryArgs1]}),
            [Shard#shard{ref = Ref}];
        false ->
            lists:map(fun(#shard{name=Name2, node=N2} = NewShard) ->
                Ref = rexi:cast(N2, {dreyfus_rpc, search,
                                     [Name2, DDoc, IndexName, QueryArgs1]}),
                NewShard#shard{ref = Ref}
            end, find_replacement_shards(Shard, LiveShards))
        end
    end, Bookmark1),
    Counters = fabric_dict:init(Counters0, nil),
    WorkerShards = fabric_dict:fetch_keys(Counters),
    RingOpts = dreyfus_util:get_ring_opts(QueryArgs, WorkerShards),
    QueryArgs2 = QueryArgs#index_query_args{
        bookmark = Bookmark1
    },
    go(DbName, DDoc, IndexName, QueryArgs2, Counters, Bookmark1, RingOpts);
go(DbName, DDoc, IndexName, OldArgs) ->
    go(DbName, DDoc, IndexName, dreyfus_util:upgrade(OldArgs)).

go(DbName, DDoc, IndexName, QueryArgs, Counters, Bookmark, RingOpts) ->
    {Workers, _} = lists:unzip(Counters),
    #index_query_args{
        limit = Limit,
        sort = Sort,
        raw_bookmark = RawBookmark
    } = QueryArgs,
    Replacements = fabric_view:get_shard_replacements(DbName, Workers),
    State = #state{
        limit = Limit,
        sort = Sort,
        top_docs = #top_docs{total_hits=0,hits=[]},
        counters = Counters,
        start_args = [DDoc, IndexName, QueryArgs],
        replacements = Replacements,
        ring_opts = RingOpts
     },
    RexiMon = fabric_util:create_monitors(Workers),
    try rexi_utils:recv(Workers, #shard.ref, fun handle_message/3,
        State, infinity, 1000 * 60 * 60) of
    {ok, Result} ->
        #state{top_docs=TopDocs} = Result,
        #top_docs{total_hits=TotalHits, hits=Hits,
                  counts=Counts, ranges=Ranges} = TopDocs,
        case RawBookmark of
            true ->
                {ok, Bookmark, TotalHits, Hits, Counts, Ranges};
            false ->
                Bookmark1 = dreyfus_bookmark:update(Sort, Bookmark, Hits),
                Hits1 = remove_sortable(Hits),
                {ok, Bookmark1, TotalHits, Hits1, Counts, Ranges}
        end;
    {error, Reason} ->
        {error, Reason}
    after
        rexi_monitor:stop(RexiMon),
        fabric_util:cleanup(Workers)
    end.

handle_message({ok, #top_docs{}=NewTopDocs}, Shard, State0) ->
    State = upgrade_state(State0),
    #state{top_docs=TopDocs, limit=Limit, sort=Sort} = State,
    case fabric_dict:lookup_element(Shard, State#state.counters) of
    undefined ->
        %% already heard from someone else in this range
        {ok, State};
    nil ->
        C1 = fabric_dict:store(Shard, ok, State#state.counters),
        C2 = fabric_view:remove_overlapping_shards(Shard, C1),
        Sortable = make_sortable(Shard, NewTopDocs),
        MergedTopDocs = merge_top_docs(TopDocs, Sortable, Limit, Sort),
        State1 = State#state{
            counters=C2,
            top_docs=MergedTopDocs
        },
        case fabric_dict:any(nil, C2) of
        true ->
            {ok, State1};
        false ->
            {stop, State1}
        end
    end;

% upgrade clause
handle_message({ok, {top_docs, UpdateSeq, TotalHits, Hits}}, Shard, State) ->
    TopDocs = #top_docs{
      update_seq = UpdateSeq,
      total_hits = TotalHits,
      hits = Hits},
    handle_message({ok, TopDocs}, Shard, State);

handle_message(Error, Worker, State0) ->
    State = upgrade_state(State0),
    case dreyfus_fabric:handle_error_message(Error, Worker,
      State#state.counters, State#state.replacements,
      search, State#state.start_args, State#state.ring_opts) of
        {ok, Counters} ->
            {ok, State#state{counters=Counters}};
        {new_refs, NewRefs, NewCounters, NewReplacements} ->
            NewState = State#state{
                counters = NewCounters,
                replacements = NewReplacements
            },
            {new_refs, NewRefs, NewState};
        Else ->
            Else
    end.

find_replacement_shards(#shard{range=Range}, AllShards) ->
    [Shard || Shard <- AllShards, Shard#shard.range =:= Range].

make_sortable(Shard, #top_docs{}=TopDocs) ->
    Hits = make_sortable(Shard, TopDocs#top_docs.hits),
    TopDocs#top_docs{hits=Hits};
make_sortable(Shard, List) when is_list(List) ->
    make_sortable(Shard, List, []).

make_sortable(_, [], Acc) ->
    lists:reverse(Acc);
make_sortable(Shard, [#hit{}=Hit|Rest], Acc) ->
    make_sortable(Shard, Rest, [#sortable{item=Hit, order=Hit#hit.order, shard=Shard} | Acc]).

remove_sortable(List) ->
    remove_sortable(List, []).

remove_sortable([], Acc) ->
    lists:reverse(Acc);
remove_sortable([#sortable{item=Item} | Rest], Acc) ->
    remove_sortable(Rest, [Item | Acc]).

merge_top_docs(#top_docs{}=TopDocsA, #top_docs{}=TopDocsB, Limit, Sort) ->
    MergedTotal = sum_element(#top_docs.total_hits, TopDocsA, TopDocsB),
    MergedHits = lists:sublist(dreyfus_util:sort(Sort,
        TopDocsA#top_docs.hits ++ TopDocsB#top_docs.hits), Limit),
    MergedCounts = merge_facets(TopDocsA#top_docs.counts, TopDocsB#top_docs.counts),
    MergedRanges = merge_facets(TopDocsA#top_docs.ranges, TopDocsB#top_docs.ranges),
    #top_docs{total_hits=MergedTotal, hits=MergedHits,
              counts=MergedCounts, ranges=MergedRanges}.

merge_facets(undefined, undefined) ->
    undefined;
merge_facets(undefined, Facets) ->
    sort_facets(Facets);
merge_facets(Facets, undefined) ->
    sort_facets(Facets);
merge_facets(FacetsA, FacetsB) ->
    merge_facets_int(sort_facets(FacetsA), sort_facets(FacetsB)).

merge_facets_int([], []) ->
    [];
merge_facets_int(FacetsA, []) ->
    FacetsA;
merge_facets_int([], FacetsB) ->
    FacetsB;
merge_facets_int([{KA, _, _}=A | RA], [{KB, _, _} | _]=FB) when KA < KB ->
    [A | merge_facets_int(RA, FB)];
merge_facets_int([{KA, VA, CA} | RA], [{KB, VB, CB} | RB]) when KA =:= KB ->
    [{KA, VA+VB, merge_facets_int(CA, CB)} | merge_facets_int(RA, RB)];
merge_facets_int([{KA, _, _} | _]=FA, [{KB, _, _}=B | RB]) when KA > KB ->
    [B | merge_facets_int(FA, RB)].

sort_facets([]) ->
    [];
sort_facets(Facets) ->
    lists:sort(lists:map(fun({K, V, C}) -> {K, V, sort_facets(C)} end,
                         Facets)).

sum_element(N, T1, T2) ->
    element(N, T1) + element(N, T2).

upgrade_state({state, Limit, Sort, TopDocs, Counters}) ->
    #state{limit=Limit, sort=Sort, top_docs=TopDocs, counters=Counters,
           replacements=[]};
upgrade_state(#state{}=State) ->
    State.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

merge_facets_test() ->
    % empty list is a no-op
    ?assertEqual([{foo, 1.0, []}], merge_facets([{foo, 1.0, []}], [])),

    % one level, one key
    ?assertEqual([{foo, 3.0, []}],
                 merge_facets([{foo, 1.0, []}],
                              [{foo, 2.0, []}])),

    % one level, two keys
    ?assertEqual([{bar, 6.0, []}, {foo, 9.0, []}],
                 merge_facets([{foo, 1.0, []}, {bar, 2.0, []}],
                              [{bar, 4.0, []}, {foo, 8.0, []}])),

    % multi level, multi keys
    ?assertEqual([{foo, 2.0, [{bar, 2.0, []}]}],
                 merge_facets([{foo, 1.0, [{bar, 1.0, []}]}],
                              [{foo, 1.0, [{bar, 1.0, []}]}])),

    ?assertEqual([{foo, 5.0, [{bar, 7.0, [{bar, 1.0, []}, {baz, 3.0, []}, {foo, 6.5, []}]}]}],
                 merge_facets([{foo, 1.0, [{bar, 2.0, [{baz, 3.0, []}, {foo, 0.5, []}]}]}],
                              [{foo, 4.0, [{bar, 5.0, [{foo, 6.0, []}, {bar, 1.0, []}]}]}])).


-endif.
