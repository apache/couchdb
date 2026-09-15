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

% Get index status. This more optimized as it send the design docs to workers
% once and they locally gather index info in parallel. We query live shard
% copies and merge the result for each index. Fields like size, seqs and counts
% are returned from one copy per range and updates_pending object is computed
% from all the responding copies.
%
% Results are grouped per index type: [{Type, [{Ident, {ok, Info} | {error,
% Err}}]}]. Type = view|search|nouveau. Ident identifies and index as [{ddoc,
% Id}] and {name, Name} for search and nouveau indexes, or {views, Names} for
% view groups.

-module(fabric_index_info).

-export([go/2, go/3, go/4]).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-define(UNAVAILABLE, {unavailable, <<"Index info is missing">>}).

go(DbName, Types) ->
    go(DbName, Types, #mrargs{}).

go(DbName, Types, #mrargs{} = Args) ->
    go(DbName, Types, Args, []).

% Args select the design docs like they for in _design_docs does (start_key,
% end_key, limit, skip, keys). Also {max_ddocs, N} specifies the maximum number
% of ddocs. Specifying more than than will return an error. Use pagination to
% get more results.
go(DbName, Types, #mrargs{} = Args0, Opts) ->
    Max = couch_util:get_value(max_ddocs, Opts, infinity),
    Args = limit_ddocs(Args0, Max),
    {ok, Meta, DDocs} = fabric:design_docs(DbName, Args),
    case length(DDocs) > Max of
        true ->
            {error, too_many_design_docs};
        false ->
            {Ctx, Responses} = index_info(DbName, DDocs, Types),
            {ok, Meta, order(merge_res(DDocs, Types, Responses, Ctx), Args)}
    end.

% Fetch one more ddoc to tell when there is more after the limit
limit_ddocs(Args, infinity) ->
    Args;
limit_ddocs(#mrargs{limit = Limit} = Args, Max) ->
    Args#mrargs{limit = min(Limit, Max + 1)}.

% Handle descending order
order(ByType, #mrargs{direction = rev}) ->
    [{Type, lists:reverse(Entries)} || {Type, Entries} <- ByType];
order(ByType, #mrargs{}) ->
    ByType.

% Returns {Ctx, Responses}. Ctx is the set of preferred (ushard) copies and the
% number of copies. Merge will need the number of copies to build
% updates_pending object.
index_info(DbName, DDocs, Types) ->
    Shards = mem3:shards(DbName),
    Ushards = mem3:ushards(DbName),
    USet = couch_util:set_from_list([{Id, N} || #shard{name = Id, node = N} <- Ushards]),
    Workers = fabric_util:submit_jobs(Shards, fabric_rpc, index_info, [DDocs, Types]),
    RexiMon = fabric_util:create_monitors(Workers),
    Ctx = {USet, length(Workers)},
    Acc0 = {fabric_dict:init(Workers, nil), []},
    try fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
        {ok, Responses} ->
            {Ctx, Responses};
        {timeout, {WorkersDict, Responses}} ->
            DefunctWorkers = fabric_util:remove_done_workers(WorkersDict, nil),
            fabric_util:log_timeout(DefunctWorkers, "index_info"),
            fabric_util:cleanup(DefunctWorkers),
            {Ctx, Responses};
        {error, Error} ->
            fabric_util:cleanup(Workers),
            throw(Error)
    after
        rexi_monitor:stop(RexiMon)
    end.

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

% Response = {Shard, [{DDocId, Sections}]}
% Sections: {error, Err} | [
%     {view_index, Res},
%     {search_indexes, [{Name, Res}]},
%     {nouveau_indexes, [{Name, Res}]}
%  ]
%  Res: {ok, Info} | {error, Err}
%
% Note: view ddoc is a group so there only one per ddoc
%
merge_res(DDocs, Types, Responses, Ctx) ->
    {DDocErrors, ByIndex} = collect(Responses),
    [{Type, merge_type(Type, DDocs, DDocErrors, ByIndex, Ctx)} || Type <- Types].

collect(Responses) ->
    lists:foldl(
        fun({Shard, PerDDoc}, Acc0) ->
            lists:foldl(
                fun
                    ({DDocId, {error, Error}}, {Errs, Idx}) ->
                        {maps:merge(#{DDocId => Error}, Errs), Idx};
                    ({DDocId, Sect}, {Errs, Idx}) ->
                        Fun = fun({K, V}, A) -> add_section(DDocId, K, V, Shard, A) end,
                        {Errs, lists:foldl(Fun, Idx, Sect)}
                end,
                Acc0,
                PerDDoc
            )
        end,
        {#{}, orddict:new()},
        Responses
    ).

add_section(DDocId, view_index, Res, Shard, Idx) ->
    orddict:append({view, DDocId, undefined}, {Shard, Res}, Idx);
add_section(DDocId, search_indexes, Named, Shard, Idx) ->
    add_named(search, DDocId, Named, Shard, Idx);
add_section(DDocId, nouveau_indexes, Named, Shard, Idx) ->
    add_named(nouveau, DDocId, Named, Shard, Idx).

add_named(Type, DDocId, Named, Shard, Idx) ->
    lists:foldl(
        fun({Name, Res}, A) -> orddict:append({Type, DDocId, Name}, {Shard, Res}, A) end,
        Idx,
        Named
    ).

% Sort entries by ddoc and then index name
merge_type(Type, DDocs, DDocErrors, ByIndex, Ctx) ->
    ByDDocId = maps:from_list([{ddoc_id(DDoc), DDoc} || DDoc <- DDocs]),
    Keys = lists:usort(lists:flatmap(fun(DDoc) -> declared(Type, DDoc) end, DDocs)),
    [{ident(Key, ByDDocId), merge_index(Type, Key, DDocErrors, ByIndex, Ctx)} || Key <- Keys].

% Works like fabric_rpc:section/5
declared(view, {Props} = DDoc) ->
    case couch_util:get_value(<<"views">>, Props) of
        {[_ | _]} -> [{view, ddoc_id(DDoc), undefined}];
        _ -> []
    end;
declared(search, {Props} = DDoc) ->
    named(search, ddoc_id(DDoc), couch_util:get_value(<<"indexes">>, Props));
declared(nouveau, {Props} = DDoc) ->
    named(nouveau, ddoc_id(DDoc), couch_util:get_value(<<"nouveau">>, Props)).

named(Type, DDocId, {[_ | _] = Indexes}) ->
    [{Type, DDocId, Name} || {Name, _} <- Indexes];
named(_Type, _DDocId, _) ->
    [].

ddoc_id({Props}) ->
    couch_util:get_value(<<"_id">>, Props).

% Identity fields of an entry. For consistency these are borrowed from GET
% /db/_index response. A view group is per ddoc so instead of a name it lists
% its views.
ident({view, DDocId, undefined}, ByDDocId) ->
    [{ddoc, DDocId}, {views, view_names(maps:get(DDocId, ByDDocId))}];
ident({_Type, DDocId, IndexName}, _ByDDocId) ->
    [{ddoc, DDocId}, {name, IndexName}].

view_names({Props}) ->
    {Views} = couch_util:get_value(<<"views">>, Props),
    lists:usort([Name || {Name, _} <- Views, Name =/= <<"lib">>]).

% Show an error if there is one. No results at all is also an error.
merge_index(Type, {_, DDocId, _} = Key, DDocErrors, ByIndex, Ctx) ->
    case DDocErrors of
        #{DDocId := Error} ->
            {error, Error};
        #{} ->
            case orddict:find(Key, ByIndex) of
                {ok, Results} -> merge_leaf(Type, Results, Ctx);
                error -> {error, ?UNAVAILABLE}
            end
    end.

% Results: {Shard, {ok, Info} | {error, Err}} per copy. Errors "win"
merge_leaf(Type, Results, Ctx) ->
    case [Error || {_, {error, Error}} <- Results] of
        [Error | _] -> {error, Error};
        [] -> merge_oks(Type, Results, Ctx)
    end.

% The per index info endpoints do the merging so the entry is exactly what
% they would return: additive fields from one (preferred) copy per range and
% updates_pending across all the copies. Just like there, the merged fields
% must cover the whole database so every range needs at least one reporting
% copy.
merge_oks(Type, Results, {USet, CopiesExpected}) ->
    Resps = [{Shard, Info} || {Shard, {ok, Info}} <- Results],
    Ranges = [{B, E} || {#shard{range = [B, E]}, _} <- Resps],
    case mem3_util:get_ring(Ranges) of
        [] -> {error, ?UNAVAILABLE};
        _ -> {ok, merge_infos(Type, USet, CopiesExpected, Resps)}
    end.

merge_infos(view, USet, CopiesExpected, Resps) ->
    {fabric_group_info:build_final_response(USet, CopiesExpected, Resps)};
merge_infos(search, USet, CopiesExpected, Resps) ->
    {dreyfus_fabric_info:build_final_response(USet, CopiesExpected, Resps)};
merge_infos(nouveau, USet, CopiesExpected, Resps) ->
    nouveau_fabric_info:build_final_response(USet, CopiesExpected, Resps).

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

% Two ranges which form a complete ring (q=2) and a single range one (q=1)
-define(R1, [0, 2147483647]).
-define(R2, [2147483648, 4294967295]).
-define(RALL, [0, 4294967295]).

shard(Range, Node) ->
    #shard{name = list_to_binary(io_lib:format("~p", [Range])), node = Node, range = Range}.

% n1 is the preferred copy of R1 and RALL, n2 of R2; 6 copies expected
ctx() ->
    USet = couch_util:set_from_list([
        {(shard(?R1, n1))#shard.name, n1},
        {(shard(?RALL, n1))#shard.name, n1},
        {(shard(?R2, n2))#shard.name, n2}
    ]),
    {USet, 6}.

pending(Min, Pref, Total, Max, Copies) ->
    [
        {minimum, Min},
        {preferred, Pref},
        {total, Total},
        {maximum, Max},
        {copies, Copies},
        {copies_expected, 6}
    ].

viewres(Pending, Seq) ->
    {ok, [{signature, <<"sig">>}, {pending_updates, Pending}, {update_seq, Seq}]}.

% A ddoc with a two view group (plus a lib), one search index and one nouveau
% index
ddoc() ->
    {[
        {<<"_id">>, <<"_design/d">>},
        {<<"views">>,
            {[
                {<<"v2">>, {[{<<"map">>, <<"function(d){}">>}]}},
                {<<"lib">>, {[{<<"x">>, <<"exports.x = 1">>}]}},
                {<<"v1">>, {[{<<"map">>, <<"function(d){}">>}]}}
            ]}},
        {<<"indexes">>, {[{<<"s">>, {[{<<"index">>, <<"function(d){}">>}]}}]}},
        {<<"nouveau">>, {[{<<"n">>, {[{<<"index">>, <<"function(d){}">>}]}}]}}
    ]}.

view_ident() -> [{ddoc, <<"_design/d">>}, {views, [<<"v1">>, <<"v2">>]}].

merge_responses_test() ->
    % Most fields come from a preferred cover. Pending computed from all shards.
    Res = [
        {shard(?R1, n1), [{<<"_design/d">>, [{view_index, viewres(0, 100)}]}]},
        {shard(?R1, n2), [{<<"_design/d">>, [{view_index, viewres(6, 94)}]}]},
        {shard(?R2, n2), [{<<"_design/d">>, [{view_index, viewres(2, 98)}]}]},
        {shard(?R2, n3), [{<<"_design/d">>, [{view_index, viewres(4, 96)}]}]}
    ],
    [{view, [{Ident, {ok, {Props}}}]}] = merge_res([ddoc()], [view], Res, ctx()),
    ?assertEqual(view_ident(), Ident),
    ?assertEqual(198, couch_util:get_value(update_seq, Props)),
    ?assertEqual(undefined, couch_util:get_value(pending_updates, Props)),
    ?assertEqual({pending(2, 2, 12, 10, 4)}, couch_util:get_value(updates_pending, Props)).

merge_responses_search_test() ->
    SRes = fun(Pending, Docs) ->
        {ok, [{signature, <<"s">>}, {pending_updates, Pending}, {doc_count, Docs}]}
    end,
    Res = [
        {shard(?RALL, n1), [{<<"_design/d">>, [{search_indexes, [{<<"s">>, SRes(3, 7)}]}]}]},
        {shard(?RALL, n2), [{<<"_design/d">>, [{search_indexes, [{<<"s">>, SRes(7, 5)}]}]}]}
    ],
    [{search, [{Ident, {ok, {Props}}}]}] = merge_res([ddoc()], [search], Res, ctx()),
    ?assertEqual([{ddoc, <<"_design/d">>}, {name, <<"s">>}], Ident),
    ?assertEqual(7, couch_util:get_value(doc_count, Props)),
    ?assertEqual(undefined, couch_util:get_value(pending_updates, Props)),
    ?assertEqual({pending(3, 3, 10, 7, 2)}, couch_util:get_value(updates_pending, Props)).

merge_responses_nouveau_test() ->
    % Nouveau uses maps for info so add a separate test for it
    NRes = fun(Pending, Seq) ->
        {ok, #{signature => <<"s">>, pending_updates => Pending, <<"update_seq">> => Seq}}
    end,
    Res = [
        {shard(?RALL, n1), [{<<"_design/d">>, [{nouveau_indexes, [{<<"n">>, NRes(5, 10)}]}]}]},
        {shard(?RALL, n2), [{<<"_design/d">>, [{nouveau_indexes, [{<<"n">>, NRes(0, 15)}]}]}]}
    ],
    [{nouveau, [{Ident, {ok, Merged}}]}] = merge_res([ddoc()], [nouveau], Res, ctx()),
    ?assertEqual([{ddoc, <<"_design/d">>}, {name, <<"n">>}], Ident),
    ?assertEqual(10, maps:get(<<"update_seq">>, Merged)),
    ?assertNot(maps:is_key(pending_updates, Merged)),
    ?assertEqual(maps:from_list(pending(0, 5, 5, 5, 2)), maps:get(updates_pending, Merged)).

merge_responses_error_wins_test() ->
    Res = [
        {shard(?RALL, n1), [{<<"_design/d">>, [{view_index, viewres(0, 100)}]}]},
        {shard(?RALL, n2), [{<<"_design/d">>, [{view_index, {error, not_found}}]}]}
    ],
    ?assertEqual(
        [{view, [{view_ident(), {error, not_found}}]}],
        merge_res([ddoc()], [view], Res, ctx())
    ).

merge_responses_ddoc_error_test() ->
    % Error on one copy marks all of that ddoc's indexes
    Res = [
        {shard(?RALL, n1), [{<<"_design/d">>, {error, bad_ddoc}}]},
        {shard(?RALL, n2), [{<<"_design/d">>, [{view_index, viewres(0, 100)}]}]}
    ],
    ?assertEqual(
        [
            {view, [{view_ident(), {error, bad_ddoc}}]},
            {search, [{[{ddoc, <<"_design/d">>}, {name, <<"s">>}], {error, bad_ddoc}}]},
            {nouveau, [{[{ddoc, <<"_design/d">>}, {name, <<"n">>}], {error, bad_ddoc}}]}
        ],
        merge_res([ddoc()], [view, search, nouveau], Res, ctx())
    ).

merge_responses_no_copies_test() ->
    % If nobody responds we get an error
    NoViews = {[{<<"_id">>, <<"_design/e">>}, {<<"indexes">>, {[{<<"s">>, {[]}}]}}]},
    Expected = [
        {view, []},
        {search, [{[{ddoc, <<"_design/e">>}, {name, <<"s">>}], {error, ?UNAVAILABLE}}]}
    ],
    ?assertEqual(Expected, merge_res([NoViews], [view, search], [], ctx())).

merge_responses_incomplete_ring_test() ->
    % Test failure to cover the whole range
    Res = [
        {shard(?R1, n1), [{<<"_design/d">>, [{view_index, viewres(0, 100)}]}]},
        {shard(?R1, n2), [{<<"_design/d">>, [{view_index, viewres(6, 94)}]}]}
    ],
    ?assertEqual(
        [{view, [{view_ident(), {error, ?UNAVAILABLE}}]}],
        merge_res([ddoc()], [view], Res, ctx())
    ).

merge_responses_sorted_test() ->
    % Entries are sorted by ddoc id then index name
    DDocA = {[{<<"_id">>, <<"_design/a">>}, {<<"indexes">>, {[{<<"z">>, {[]}}, {<<"b">>, {[]}}]}}]},
    Res = [
        {shard(?RALL, n1), [
            {<<"_design/d">>, [{search_indexes, [{<<"s">>, {ok, [{doc_count, 1}]}}]}]},
            {<<"_design/a">>, [
                {search_indexes, [
                    {<<"z">>, {ok, [{doc_count, 2}]}}, {<<"b">>, {ok, [{doc_count, 3}]}}
                ]}
            ]}
        ]}
    ],
    [{search, Entries}] = merge_res([ddoc(), DDocA], [search], Res, ctx()),
    ?assertEqual(
        [
            [{ddoc, <<"_design/a">>}, {name, <<"b">>}],
            [{ddoc, <<"_design/a">>}, {name, <<"z">>}],
            [{ddoc, <<"_design/d">>}, {name, <<"s">>}]
        ],
        [Ident || {Ident, _} <- Entries]
    ).

limit_ddocs_test() ->
    % With a limit at most limit + 1 design docs are fetched
    ?assertEqual(#mrargs{limit = 100}, limit_ddocs(#mrargs{limit = 100}, infinity)),
    ?assertEqual(#mrargs{limit = 3}, limit_ddocs(#mrargs{limit = 100}, 2)),
    ?assertEqual(#mrargs{limit = 1}, limit_ddocs(#mrargs{limit = 1}, 2)),
    ?assert(length([a, b, c]) > 2),
    ?assertNot(length([a, b, c]) > infinity).

-endif.
