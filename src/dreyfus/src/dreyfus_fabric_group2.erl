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

-module(dreyfus_fabric_group2).

-include("dreyfus.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([go/4]).

-record(state, {
    limit,
    sort,
    total_hits,
    total_grouped_hits,
    top_groups,
    counters,
    start_args,
    replacements
}).

go(DbName, GroupId, IndexName, QueryArgs) when is_binary(GroupId) ->
    {ok, DDoc} = fabric:open_doc(DbName, <<"_design/", GroupId/binary>>, []),
    dreyfus_util:maybe_deny_index(DbName, GroupId, IndexName),
    go(DbName, DDoc, IndexName, QueryArgs);

go(DbName, DDoc, IndexName, #index_query_args{}=QueryArgs) ->
    DesignName = dreyfus_util:get_design_docid(DDoc),
    dreyfus_util:maybe_deny_index(DbName, DesignName, IndexName),
    Shards = dreyfus_util:get_shards(DbName, QueryArgs),
    Workers = fabric_util:submit_jobs(Shards, dreyfus_rpc, group2,
                          [DDoc, IndexName, dreyfus_util:export(QueryArgs)]),
    Replacements = fabric_view:get_shard_replacements(DbName, Workers),
    Counters = fabric_dict:init(Workers, nil),
    RexiMon = fabric_util:create_monitors(Workers),
    State = #state{
        limit = QueryArgs#index_query_args.limit,
        sort = QueryArgs#index_query_args.sort,
        total_hits = 0,
        total_grouped_hits = 0,
        top_groups = [],
        counters = Counters,
        start_args = [DDoc, IndexName, QueryArgs],
        replacements = Replacements
    },
    try
        rexi_utils:recv(Workers, #shard.ref, fun handle_message/3,
                        State, infinity, 1000 * 60 * 60)
    after
        rexi_monitor:stop(RexiMon),
        fabric_util:cleanup(Workers)
    end;
go(DbName, DDoc, IndexName, OldArgs) ->
    go(DbName, DDoc, IndexName, dreyfus_util:upgrade(OldArgs)).


handle_message({ok, NewTotalHits, NewTotalGroupedHits, NewTopGroups},
               Shard, State0) ->
    State = upgrade_state(State0),
    #state{total_hits=TotalHits, total_grouped_hits=TotalGroupedHits,
           top_groups=TopGroups, limit=Limit, sort=Sort} = State,
    case fabric_dict:lookup_element(Shard, State#state.counters) of
    undefined ->
        %% already heard from someone else in this range
        {ok, State};
    nil ->
        C1 = fabric_dict:store(Shard, ok, State#state.counters),
        C2 = fabric_view:remove_overlapping_shards(Shard, C1),
        MergedTotalHits = NewTotalHits + TotalHits,
        MergedTotalGroupedHits = NewTotalGroupedHits + TotalGroupedHits,
        Sortable = make_sortable(Shard, NewTopGroups),
        MergedTopGroups = merge_top_groups(TopGroups, Sortable, Limit, Sort),
        State1 = State#state{
            counters=C2,
            total_hits=MergedTotalHits,
            total_grouped_hits=MergedTotalGroupedHits,
            top_groups=MergedTopGroups
        },
        case fabric_dict:any(nil, C2) of
        true ->
            {ok, State1};
        false ->
            {stop, {MergedTotalHits, MergedTotalGroupedHits,
                    remove_sortable(MergedTopGroups)}}
        end
    end;

handle_message(Error, Worker, State0) ->
    State = upgrade_state(State0),
    case dreyfus_fabric:handle_error_message(Error, Worker,
      State#state.counters, State#state.replacements,
      group2, State#state.start_args) of
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

merge_top_groups([], TopGroups, _Limit, _Sort) ->
    TopGroups;
merge_top_groups(TopGroupsA, TopGroupsB, Limit, Sort) ->
    lists:zipwith(fun(A,B) -> merge_top_group(A, B, Limit, Sort) end,
                  TopGroupsA,
                  TopGroupsB).

merge_top_group({Name, TotalA, HitsA}, {Name, TotalB, HitsB}, Limit, Sort) ->
    MergedHits = lists:sublist(dreyfus_util:sort(Sort, HitsA ++ HitsB), Limit),
    {Name, TotalA + TotalB, MergedHits}.


make_sortable(Shard, TopGroups) ->
    [make_sortable_group(Shard, TopGroup) || TopGroup <- TopGroups].

make_sortable_group(Shard, {Name, TotalHits, Hits}) ->
    {Name, TotalHits, [make_sortable_hit(Shard, Hit) || Hit <- Hits]}.

make_sortable_hit(Shard, Hit) ->
    #sortable{item=Hit, order=Hit#hit.order, shard=Shard}.

remove_sortable(SortableGroups) ->
    [remove_sortable_group(G) || G <- SortableGroups].

remove_sortable_group({Name, TotalHits, SortableHits}) ->
    {Name, TotalHits, [remove_sortable_hit(H) || H <- SortableHits]}.

remove_sortable_hit(SortableHit) ->
    SortableHit#sortable.item.

upgrade_state({state, Limit, Sort, TotalHits, TotalGroupedHits,
               TopGroups, Counters}) ->
    #state{limit = Limit, sort = Sort, total_hits = TotalHits,
           total_grouped_hits = TotalGroupedHits,
           top_groups = TopGroups, counters = Counters,
           replacements = []};
upgrade_state(#state{} = State) ->
    State.
