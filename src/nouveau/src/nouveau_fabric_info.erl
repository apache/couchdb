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

-module(nouveau_fabric_info).

-export([go/3]).
% Used by fabric_index_info to merge its per copy results the same way
-export([build_final_response/3]).

-include_lib("mem3/include/mem3.hrl").

go(DbName, DDocId, IndexName) when is_binary(DDocId) ->
    {ok, DDoc} = fabric:open_doc(DbName, <<"_design/", DDocId/binary>>, [ejson_body]),
    go(DbName, DDoc, IndexName);
go(DbName, DDoc, IndexName) ->
    case nouveau_util:design_doc_to_index(DbName, DDoc, IndexName) of
        {ok, Index} ->
            go(DbName, DDoc, IndexName, Index);
        {error, Reason} ->
            {error, Reason}
    end.

go(DbName, _DDoc, _IndexName, Index) ->
    Shards = mem3:shards(DbName),
    Ushards = mem3:ushards(DbName),
    USet = couch_util:set_from_list([{Id, N} || #shard{name = Id, node = N} <- Ushards]),
    Counters0 = lists:map(
        fun(#shard{} = Shard) ->
            Ref = rexi:cast(
                Shard#shard.node,
                {nouveau_rpc, info, [Shard#shard.name, Index]}
            ),
            Shard#shard{ref = Ref}
        end,
        Shards
    ),
    Counters = fabric_dict:init(Counters0, nil),
    Workers = fabric_dict:fetch_keys(Counters),
    RexiMon = fabric_util:create_monitors(Workers),

    Acc0 = {Counters, [], USet, length(Workers), undefined},
    try fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
        {timeout, {WorkersDict, Resps, _, Expected, _}} ->
            DefunctWorkers = fabric_util:remove_done_workers(WorkersDict, nil),
            fabric_util:log_timeout(DefunctWorkers, "nouveau_info"),
            finish(Resps, USet, Expected, timeout);
        Else ->
            Else
    after
        rexi_monitor:stop(RexiMon),
        fabric_util:cleanup(Workers)
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

% Need at least one response per range for succcess
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
    Reps = [maps:remove(pending_updates, representative(Copies)) || Copies <- RangeCopies],
    PerRange = [
        [{Pref, maps:get(pending_updates, Info, undefined)} || {Pref, Info} <- Copies]
     || Copies <- RangeCopies
    ],
    Merged = lists:foldl(
        fun(Info, Acc) -> maps:merge_with(fun merge_info/3, Info, Acc) end,
        #{},
        Reps
    ),
    case fabric_util:aggregate_pending(PerRange, Expected) of
        undefined -> Merged;
        Pending -> Merged#{updates_pending => maps:from_list(Pending)}
    end.

% Prefer ushards if available first
representative(Copies) ->
    case [Info || {true, Info} <- Copies] of
        [Info | _] ->
            Info;
        [] ->
            [{_Pref, Info} | _] = Copies,
            Info
    end.

merge_info(signature, Val, Val) ->
    Val;
merge_info(_Key, Val1, Val2) ->
    Val1 + Val2.

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

shard(Range, Node) ->
    #shard{name = list_to_binary(io_lib:format("~p", [Range])), node = Node, range = Range}.

info(Docs, Pending) ->
    #{~"num_docs" => Docs, pending_updates => Pending, signature => ~"s"}.

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
    ?assertEqual(198, maps:get(~"num_docs", Result)),
    ?assertEqual(~"s", maps:get(signature, Result)),
    ?assertNot(maps:is_key(pending_updates, Result)),
    ?assertEqual(
        #{
            minimum => 2,
            preferred => 2,
            total => 12,
            maximum => 10,
            copies => 4,
            copies_expected => 6
        },
        maps:get(updates_pending, Result)
    ).

-endif.
