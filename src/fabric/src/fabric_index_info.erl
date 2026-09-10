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

% Get status of all the indexes in db. Info gathering is optimized by sending
% the ddocs to the works and they locally gather index info in parallel.
%
% To get an idea about how many shards are built we query every live copy and
% return a summary of expected shards vs actual shards included in the results.
% The result returns the expected number of shards (workers) and each index
% summary will contain the number of copies which actually replied. This gives
% us an idea how many indexes and how far along they are before they are built.
%
% For each index besides their sizes and other info we return:
%
%  minimum : best copy of each range. When 0 it means at least one complete
%    copy of every range exists.
%
%  maximum : the worst copy of each range. When 0 it means every copy of every
%    live range is fully bult.
%
%  copies : live shard copies which returned results. If this value is too low
%    compared to the expected copies the maximum/minimum values may not be
%    trusted much.
%
% For other index info fields use the lowest values per range so they are
% more or less stable from call to call.

-module(fabric_index_info).

-export([go/2]).

-include_lib("mem3/include/mem3.hrl").

go(DbName, Types) ->
    {ok, DDocs} = fabric:design_docs(DbName),
    {CopiesExpected, Responses} = index_info(DbName, DDocs, Types),
    {ok, {CopiesExpected, merge_responses(Responses)}}.

index_info(DbName, DDocs, Types) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, fabric_rpc, index_info, [DDocs, Types]),
    RexiMon = fabric_util:create_monitors(Workers),
    Acc0 = {fabric_dict:init(Workers, nil), []},
    try fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
        {ok, Responses} ->
            {length(Workers), tag_responses(Responses)};
        {timeout, {WorkersDict, Responses}} ->
            DefunctWorkers = fabric_util:remove_done_workers(WorkersDict, nil),
            fabric_util:log_timeout(DefunctWorkers, "index_info"),
            fabric_util:cleanup(DefunctWorkers),
            {length(Workers), tag_responses(Responses)};
        {error, Error} ->
            fabric_util:cleanup(Workers),
            throw(Error)
    after
        rexi_monitor:stop(RexiMon)
    end.

tag_responses(Responses) ->
    [{W#shard.range, W#shard.node, PerDDoc} || {W, PerDDoc} <- Responses].

% Failed / unreachables dropped. Otherwise responses are merged
%
handle_message({ok, Result}, Worker, {Counters, Acc}) ->
    Counters1 = fabric_dict:erase(Worker, Counters),
    maybe_stop(Counters1, [{Worker, Result} | Acc]);
handle_message({rexi_DOWN, _, {_, NRef}, _}, _Worker, {Counters, Acc}) ->
    Counters1 = fabric_dict:filter(fun(#shard{node = N}, _) -> N =/= NRef end, Counters),
    maybe_stop(Counters1, Acc);
handle_message(_Error, Worker, {Counters, Acc}) ->
    Counters1 = fabric_dict:erase(Worker, Counters),
    maybe_stop(Counters1, Acc).

maybe_stop(Counters, Acc) ->
    case fabric_dict:size(Counters) of
        0 -> {stop, Acc};
        _ -> {ok, {Counters, Acc}}
    end.

% Each response is {Range, Node, [{DDocId, Sections}]}
% Sections: {error, Err} | [
%     {view_index, Res},
%     {search_indexes, [{Name, Res}]},
%     {nouveau_indexes, [{Name, Res}]}
%  ]
%  Res: {ok, Info} | {error, Err}
%
% Note: view ddoc is a group so there only one per ddoc
%
merge_responses(Responses) ->
    ByDDoc = lists:foldl(
        fun({Range, Node, PerDDoc}, Acc0) ->
            lists:foldl(
                fun({DDocId, Sect}, Acc) ->
                    orddict:append(DDocId, {Range, Node, Sect}, Acc)
                end,
                Acc0,
                PerDDoc
            )
        end,
        orddict:new(),
        Responses
    ),
    [{DDocId, merge_ddoc(Entries)} || {DDocId, Entries} <- ByDDoc].

merge_ddoc(Entries) ->
    case [Error || {_, _, {error, Error}} <- Entries] of
        [Error | _] ->
            {error, Error};
        [] ->
            BySection = lists:foldl(
                fun({Range, Node, Sect}, Acc0) ->
                    lists:foldl(
                        fun({K, V}, Acc) -> orddict:append(K, {Range, Node, V}, Acc) end,
                        Acc0,
                        Sect
                    )
                end,
                orddict:new(),
                Entries
            ),
            {[merge_section(K, Vs) || {K, Vs} <- BySection]}
    end.

merge_section(view_index, Results) ->
    {view_index, merge_leaf(view, Results)};
merge_section(search_indexes, PerCopyIdxLists) ->
    {search_indexes, {merge_named(search, PerCopyIdxLists)}};
merge_section(nouveau_indexes, PerCopyIdxLists) ->
    {nouveau_indexes, {merge_named(nouveau, PerCopyIdxLists)}}.

merge_named(Type, PerCopyIdxLists) ->
    ByName = lists:foldl(
        fun({Range, Node, IdxList}, Acc0) ->
            lists:foldl(
                fun({Name, Res}, Acc) -> orddict:append(Name, {Range, Node, Res}, Acc) end,
                Acc0,
                IdxList
            )
        end,
        orddict:new(),
        PerCopyIdxLists
    ),
    [{Name, merge_leaf(Type, Results)} || {Name, Results} <- ByName].

% Results: {Range, Node, {ok, Info} | {error, Err}} per copy. Errors "win"
merge_leaf(Type, Results) ->
    case [Error || {_, _, {error, Error}} <- Results] of
        [Error | _] -> {error, Error};
        [] -> merge_oks(Type, Results)
    end.

% Non pending updates are merged by lowest copy. We aggregate pending at the
% end since we're doing a min/max over those.
merge_oks(Type, Results) ->
    ByRange = lists:foldl(
        fun({Range, Node, {ok, Info}}, Acc) -> orddict:append(Range, {Node, Info}, Acc) end,
        orddict:new(),
        Results
    ),
    RangeCopies = [Copies || {_Range, Copies} <- ByRange],
    Reps = [rem_pending(Type, first(Copies)) || Copies <- RangeCopies],
    PerRange = [[pending(Type, Info) || {_Node, Info} <- Copies] || Copies <- RangeCopies],
    add_pending(Type, merge_infos(Type, Reps), agg_pending(PerRange)).

% For some stable-ish order results don't flip flop with each call
first(Copies) ->
    [{_Node, Info} | _] = lists:sort(Copies),
    Info.

% Skip copies which did not report pending
% Ranges with no reporting copies contribute 0
% Return undefined when no copy of any range sent reports
agg_pending(PerRange) ->
    case lists:foldl(fun pending_range/2, undefined, PerRange) of
        undefined -> undefined;
        {Min, Max, Copies} -> [{minimum, Min}, {maximum, Max}, {copies, Copies}]
    end.

pending_range(Vals, Acc) ->
    case lists:sort([P || P <- Vals, is_integer(P)]) of
        [] ->
            Acc;
        [_ | _] = Vs ->
            {Min, Max, Copies} =
                case Acc of
                    undefined -> {0, 0, 0};
                    _ -> Acc
                end,
            {Min + hd(Vs), Max + lists:last(Vs), Copies + length(Vs)}
    end.

merge_infos(view, Infos) ->
    merge_view_infos(Infos);
merge_infos(search, Infos) ->
    merge_search_infos(Infos);
merge_infos(nouveau, Infos) ->
    merge_nouveau_infos(Infos).

pending(nouveau, #{} = Info) ->
    maps:get(pending_updates, Info, undefined);
pending(_Type, Info) ->
    couch_util:get_value(pending_updates, Info).

rem_pending(nouveau, #{} = Info) ->
    maps:remove(pending_updates, Info);
rem_pending(_Type, Info) ->
    lists:keydelete(pending_updates, 1, Info).

add_pending(_Type, Merged, undefined) ->
    Merged;
add_pending(nouveau, #{} = Merged, Bounds) ->
    Merged#{updates_pending => maps:from_list(Bounds)};
add_pending(_Type, {Props}, Bounds) ->
    {lists:keystore(updates_pending, 1, Props, {updates_pending, {Bounds}})}.

% Copied from fabric_group_info:merge_results/1 mostly. Except we remove the pending updates
% then do our own (hopefully better) calculation over them.
merge_view_infos(Infos) ->
    Dict = to_orddict(lists:append(Infos)),
    Merged = orddict:fold(
        fun
            (signature, [X | _], Acc) ->
                [{signature, X} | Acc];
            (language, [X | _], Acc) ->
                [{language, X} | Acc];
            (sizes, X, Acc) ->
                [{sizes, {merge_obj(X)}} | Acc];
            (compact_running, X, Acc) ->
                [{compact_running, lists:member(true, X)} | Acc];
            (updater_running, X, Acc) ->
                [{updater_running, lists:member(true, X)} | Acc];
            (waiting_commit, X, Acc) ->
                [{waiting_commit, lists:member(true, X)} | Acc];
            (waiting_clients, X, Acc) ->
                [{waiting_clients, lists:sum(X)} | Acc];
            (update_seq, X, Acc) ->
                [{update_seq, lists:sum(X)} | Acc];
            (purge_seq, X, Acc) ->
                [{purge_seq, lists:sum(X)} | Acc];
            (collator_versions, X, Acc) ->
                Vs = lists:usort(lists:flatmap(fun(V) -> V end, X)),
                [{collator_versions, Vs} | Acc];
            (_, _, Acc) ->
                Acc
        end,
        [],
        Dict
    ),
    {Merged}.

% Copied from dreyfus_fabric_info:merge_results/1. We remove pending updates
% and add them later.
merge_search_infos(Infos) ->
    Dict = to_orddict(lists:append(Infos)),
    Merged = orddict:fold(
        fun
            (signature, [X | _], Acc) -> [{signature, X} | Acc];
            (disk_size, X, Acc) -> [{disk_size, lists:sum(X)} | Acc];
            (doc_count, X, Acc) -> [{doc_count, lists:sum(X)} | Acc];
            (doc_del_count, X, Acc) -> [{doc_del_count, lists:sum(X)} | Acc];
            (committed_seq, X, Acc) -> [{committed_seq, lists:sum(X)} | Acc];
            (pending_seq, X, Acc) -> [{pending_seq, lists:sum(X)} | Acc];
            (_, _, Acc) -> Acc
        end,
        [],
        Dict
    ),
    {Merged}.

merge_nouveau_infos(Maps) ->
    lists:foldl(fun(M, Acc) -> maps:merge_with(fun merge_nouveau_val/3, M, Acc) end, #{}, Maps).

merge_nouveau_val(signature, Val, Val) ->
    % Can't sum signatures, but we can sum everything else
    Val;
merge_nouveau_val(_Key, Val1, Val2) ->
    Val1 + Val2.

to_orddict(KVs) ->
    lists:foldl(fun({K, V}, D) -> orddict:append(K, V, D) end, orddict:new(), KVs).

% Merge list of {[{K, V}]} objects where V is a number
merge_obj(Objects) ->
    Dict = lists:foldl(
        fun({Props}, D) ->
            lists:foldl(fun({K, V}, D0) -> orddict:append(K, V, D0) end, D, Props)
        end,
        orddict:new(),
        Objects
    ),
    orddict:fold(
        fun(Key, X, Acc) ->
            [{Key, lists:sum(X)} | Acc]
        end,
        [],
        Dict
    ).

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

aggregate_pending_test() ->
    ?assertEqual(undefined, agg_pending([])),
    ?assertEqual(undefined, agg_pending([[]])),
    ?assertEqual(undefined, agg_pending([[undefined]])),
    ?assertEqual([{minimum, 5}, {maximum, 5}, {copies, 1}], agg_pending([[5]])),
    ?assertEqual(
        [{minimum, 3}, {maximum, 11}, {copies, 5}],
        agg_pending([[4, 0, 8], [3, 3]])
    ),
    ?assertEqual(
        [{minimum, 2}, {maximum, 2}, {copies, 1}],
        agg_pending([[undefined, 2], [undefined]])
    ).

viewres(Pending, Seq) ->
    {ok, [{signature, <<"sig">>}, {pending_updates, Pending}, {update_seq, Seq}]}.

merge_responses_test() ->
    % Some field are added while for pending we compute our min/max stats
    Responses = [
        {[0, 10], n1, [{<<"_design/d">>, [{view_index, viewres(0, 100)}]}]},
        {[0, 10], n2, [{<<"_design/d">>, [{view_index, viewres(6, 94)}]}]},
        {[11, 20], n2, [{<<"_design/d">>, [{view_index, viewres(2, 98)}]}]},
        {[11, 20], n3, [{<<"_design/d">>, [{view_index, viewres(4, 96)}]}]}
    ],
    [{<<"_design/d">>, {[{view_index, {Props}}]}}] = merge_responses(Responses),
    ?assertEqual(198, couch_util:get_value(update_seq, Props)),
    ?assertEqual(undefined, couch_util:get_value(pending_updates, Props)),
    ?assertEqual(
        {[{minimum, 2}, {maximum, 10}, {copies, 4}]},
        couch_util:get_value(updates_pending, Props)
    ).

merge_responses_error_wins_test() ->
    Responses = [
        {[0, 10], n1, [{<<"_design/d">>, [{view_index, viewres(0, 100)}]}]},
        {[0, 10], n2, [{<<"_design/d">>, [{view_index, {error, not_found}}]}]}
    ],
    ?assertEqual(
        [{<<"_design/d">>, {[{view_index, {error, not_found}}]}}],
        merge_responses(Responses)
    ).

merge_responses_nouveau_test() ->
    % Nouveau uses maps for info so add a separate test for it
    NRes = fun(Pending, Seq) ->
        {ok, #{signature => <<"s">>, pending_updates => Pending, <<"update_seq">> => Seq}}
    end,
    Responses = [
        {[0, 10], n1, [{<<"_design/d">>, [{nouveau_indexes, [{<<"n">>, NRes(5, 10)}]}]}]},
        {[0, 10], n2, [{<<"_design/d">>, [{nouveau_indexes, [{<<"n">>, NRes(0, 15)}]}]}]}
    ],
    [{<<"_design/d">>, {[{nouveau_indexes, {[{<<"n">>, Merged}]}}]}}] =
        merge_responses(Responses),
    ?assertEqual(10, maps:get(<<"update_seq">>, Merged)),
    ?assertNot(maps:is_key(pending_updates, Merged)),
    ?assertEqual(
        #{minimum => 0, maximum => 5, copies => 2}, maps:get(updates_pending, Merged)
    ).

-endif.
