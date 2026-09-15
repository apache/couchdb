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

-module(fabric_group_info).

-export([go/2]).
% Used by fabric_index_info
-export([build_final_response/3]).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

go(DbName, GroupId) when is_binary(GroupId) ->
    {ok, DDoc} = fabric:open_doc(DbName, GroupId, [?ADMIN_CTX]),
    go(DbName, DDoc);
go(DbName, #doc{id = DDocId}) ->
    Shards = mem3:shards(DbName),
    Ushards = mem3:ushards(DbName),
    Workers = fabric_util:submit_jobs(Shards, group_info, [DDocId]),
    RexiMon = fabric_util:create_monitors(Shards),
    USet = couch_util:set_from_list([{Id, N} || #shard{name = Id, node = N} <- Ushards]),
    Acc = {fabric_dict:init(Workers, nil), [], USet, length(Workers), undefined},
    try fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc) of
        {timeout, {WorkersDict, Resps, _, Expected, _}} ->
            DefunctWorkers = fabric_util:remove_done_workers(WorkersDict, nil),
            fabric_util:log_timeout(DefunctWorkers, "group_info"),
            fabric_util:cleanup(DefunctWorkers),
            finish(Resps, USet, Expected, timeout);
        Else ->
            Else
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({ok, Info}, Shard, {Counters, Resps, USet, Expected, LastErr}) ->
    Counters1 = fabric_dict:erase(Shard, Counters),
    maybe_stop(Counters1, [{Shard, Info} | Resps], USet, Expected, LastErr);
handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _, {Counters, Resps, USet, Expected, LastErr}) ->
    Counters1 = fabric_dict:filter(fun(#shard{node = N}, _) -> N =/= NodeRef end, Counters),
    maybe_stop(Counters1, Resps, USet, Expected, LastErr);
handle_message({rexi_EXIT, Reason}, Shard, {Counters, Resps, USet, Expected, _}) ->
    Counters1 = fabric_dict:erase(Shard, Counters),
    maybe_stop(Counters1, Resps, USet, Expected, Reason);
handle_message(Reason, Shard, {Counters, Resps, USet, Expected, _}) ->
    Counters1 = fabric_dict:erase(Shard, Counters),
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

% Every range needs at least one reporting copy.
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

% If there is a ushard pick that so we don't flop as much from one call to the next
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
            (signature, [X | _], Acc) ->
                [{signature, X} | Acc];
            (language, [X | _], Acc) ->
                [{language, X} | Acc];
            (sizes, X, Acc) ->
                [{sizes, {merge_object(X)}} | Acc];
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
                % Concatenate (undo orddict:append/3), then
                % sort and remove duplicates.
                Vs = lists:usort(lists:flatmap(fun(V) -> V end, X)),
                [{collator_versions, Vs} | Acc];
            (_, _, Acc) ->
                Acc
        end,
        [],
        Dict
    ).

merge_object(Objects) ->
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

shard(Range, Node) ->
    #shard{name = ?l2b(io_lib:format("~p", [Range])), node = Node, range = Range}.

info(Seq, Pending) ->
    [{update_seq, Seq}, {pending_updates, Pending}, {signature, <<"s">>}].

build_final_response_test() ->
    R1 = [0, 10],
    R2 = [11, 20],
    R1N1 = shard(R1, n1),
    R2N2 = shard(R2, n2),
    USet = couch_util:set_from_list([
        {R1N1#shard.name, n1}, {R2N2#shard.name, n2}
    ]),
    Resps = [
        {R1N1, info(100, 0)},
        {shard(R1, n2), info(94, 6)},
        {R2N2, info(98, 2)},
        {shard(R2, n3), info(96, 4)}
    ],
    Result = build_final_response(USet, 6, Resps),
    % update_seq comes from the prefered set of shards only (100+98=198)
    ?assertEqual(198, couch_util:get_value(update_seq, Result)),
    ?assertEqual(<<"s">>, couch_util:get_value(signature, Result)),
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
    ).

finish_requires_complete_ring_test() ->
    A = [0, 2147483647],
    B = [2147483648, 4294967295],
    Info = info(1, 0),
    ?assertEqual({error, boom}, finish([{shard(A, n1), Info}], sets:new(), 4, boom)),
    ?assertMatch(
        {ok, [_ | _]}, finish([{shard(A, n1), Info}, {shard(B, n2), Info}], sets:new(), 4, boom)
    ).

-endif.
