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

-module(dreyfus_fabric_info).

-include_lib("mem3/include/mem3.hrl").

-export([go/4]).
% Used by fabric_index_info
-export([build_final_response/3]).

go(DbName, DDocId, IndexName, InfoLevel) when is_binary(DDocId) ->
    {ok, DDoc} = fabric:open_doc(DbName, <<"_design/", DDocId/binary>>, []),
    dreyfus_util:maybe_deny_index(DbName, DDocId, IndexName),
    go(DbName, DDoc, IndexName, InfoLevel);
go(DbName, DDoc, IndexName, InfoLevel) ->
    DesignName = dreyfus_util:get_design_docid(DDoc),
    dreyfus_util:maybe_deny_index(DbName, DesignName, IndexName),
    Shards = mem3:shards(DbName),
    Ushards = mem3:ushards(DbName),
    USet = couch_util:set_from_list([{Id, N} || #shard{name = Id, node = N} <- Ushards]),
    Workers = fabric_util:submit_jobs(Shards, dreyfus_rpc, InfoLevel, [DDoc, IndexName]),
    RexiMon = fabric_util:create_monitors(Shards),
    Acc0 = {fabric_dict:init(Workers, nil), [], USet, length(Workers), undefined},
    try fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
        {timeout, {WorkersDict, Resps, _, Expected, _}} ->
            DefunctWorkers = fabric_util:remove_done_workers(WorkersDict, nil),
            fabric_util:log_timeout(DefunctWorkers, "search_info"),
            fabric_util:cleanup(DefunctWorkers),
            finish(Resps, USet, Expected, timeout);
        Else ->
            Else
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({ok, Info}, Worker, {Counters, Resps, USet, Expected, LastErr}) ->
    Counters1 = fabric_dict:erase(Worker, Counters),
    maybe_stop(Counters1, [{Worker, Info} | Resps], USet, Expected, LastErr);
handle_message(
    {rexi_DOWN, _, {_, NodeRef}, _}, _Worker, {Counters, Resps, USet, Expected, LastErr}
) ->
    Counters1 = fabric_dict:filter(fun(#shard{node = N}, _) -> N =/= NodeRef end, Counters),
    maybe_stop(Counters1, Resps, USet, Expected, LastErr);
handle_message({rexi_EXIT, Reason}, Worker, {Counters, Resps, USet, Expected, _}) ->
    Counters1 = fabric_dict:erase(Worker, Counters),
    maybe_stop(Counters1, Resps, USet, Expected, Reason);
handle_message({error, Reason}, Worker, {Counters, Resps, USet, Expected, _}) ->
    Counters1 = fabric_dict:erase(Worker, Counters),
    maybe_stop(Counters1, Resps, USet, Expected, Reason);
handle_message(Reason, Worker, {Counters, Resps, USet, Expected, _}) ->
    Counters1 = fabric_dict:erase(Worker, Counters),
    maybe_stop(Counters1, Resps, USet, Expected, Reason).

maybe_stop(Counters, Resps, USet, Expected, FinalErr) ->
    case fabric_dict:size(Counters) of
        0 ->
            Error =
                case FinalErr of
                    undefined -> {nodedown, <<"progress not possible">>};
                    _ -> FinalErr
                end,
            case finish(Resps, USet, Expected, Error) of
                {ok, Result} -> {stop, Result};
                {error, _} = E -> E
            end;
        _ ->
            {ok, {Counters, Resps, USet, Expected, FinalErr}}
    end.

% Every range must have at least one result
finish(Resps, USet, Expected, Error) ->
    Ranges = [{B, E} || {#shard{range = [B, E]}, _} <- Resps],
    case mem3_util:get_ring(Ranges) of
        [] -> {error, Error};
        _ -> {ok, build_final_response(USet, Expected, Resps)}
    end.

build_final_response(USet, Expected, Resps) ->
    ByRange = lists:foldl(
        fun({#shard{range = Range, name = Id, node = Node}, Info}, Acc) ->
            orddict:append(Range, {sets:is_element({Id, Node}, USet), Info}, Acc)
        end,
        orddict:new(),
        Resps
    ),
    RangeCopies = [Copies || {_Range, Copies} <- ByRange],
    Reps = [representative(Copies) || Copies <- RangeCopies],
    PerRange = [
        [{Pref, couch_util:get_value(pending_updates, Info)} || {Pref, Info} <- Copies]
     || Copies <- RangeCopies
    ],
    Merged = merge_results(lists:append(Reps)),
    case fabric_util:aggregate_pending(PerRange, Expected) of
        undefined -> Merged;
        Pending -> [{updates_pending, {Pending}} | Merged]
    end.

% Try to pick the ushard copy as a stable representative so result doesn't flop
% and forth with each request as much
representative(Copies) ->
    case [Info || {true, Info} <- Copies] of
        [Info | _] ->
            Info;
        [] ->
            [{_Pref, Info} | _] = Copies,
            Info
    end.

merge_results(Info) ->
    Dict = lists:foldl(
        fun({K, V}, D0) -> orddict:append(K, V, D0) end,
        orddict:new(),
        Info
    ),
    orddict:fold(
        fun
            (disk_size, X, Acc) ->
                [{disk_size, lists:sum(X)} | Acc];
            (doc_count, X, Acc) ->
                [{doc_count, lists:sum(X)} | Acc];
            (doc_del_count, X, Acc) ->
                [{doc_del_count, lists:sum(X)} | Acc];
            (committed_seq, X, Acc) ->
                [{committed_seq, lists:sum(X)} | Acc];
            (pending_seq, X, Acc) ->
                [{pending_seq, lists:sum(X)} | Acc];
            (signature, [X | _], Acc) ->
                [{signature, X} | Acc];
            (_, _, Acc) ->
                Acc
        end,
        [],
        Dict
    ).

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

shard([B, E] = Range, Node) ->
    Name = integer_to_list(B) ++ "-" ++ integer_to_list(E),
    #shard{name = list_to_binary(Name), node = Node, range = Range}.

info(Docs, Pending) ->
    [{doc_count, Docs}, {pending_updates, Pending}, {signature, ~"s"}].

final_resp_test() ->
    R1 = [0, 10],
    R2 = [11, 20],
    R1N1 = shard(R1, n1),
    R2N2 = shard(R2, n2),
    USet = couch_util:set_from_list([
        {R1N1#shard.name, n1}, {R2N2#shard.name, n2}
    ]),
    R1N2 = shard(R1, n2),
    R2N3 = shard(R2, n3),
    Resps = [
        {R1N1, info(100, 0)},
        {R1N2, info(94, 6)},
        {R2N2, info(98, 2)},
        {R2N3, info(96, 4)}
    ],
    Result = build_final_response(USet, 6, Resps),
    ?assertEqual(198, couch_util:get_value(doc_count, Result)),
    ?assertEqual(undefined, couch_util:get_value(pending_updates, Result)),
    ?assertEqual(
        {[
            {minimum, 2},
            {preferred, 2},
            {total, 12},
            {maximum, 10},
            {copies, 4},
            {copies_expected, 6}
        ]},
        couch_util:get_value(updates_pending, Result)
    ),
    % No backlog in this case so no updates_pending either
    NoPending = [{R1N1, [{disk_size, 5}]}, {R2N2, [{disk_size, 7}]}],
    ?assertEqual([{disk_size, 12}], build_final_response(USet, 6, NoPending)).

-endif.
