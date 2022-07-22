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

-include_lib("fabric/include/fabric.hrl").
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
    USet = sets:from_list([{Id, N} || #shard{name = Id, node = N} <- Ushards]),
    Acc = {fabric_dict:init(Workers, nil), [], USet},
    try fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc) of
        {timeout, {WorkersDict, _, _}} ->
            DefunctWorkers = fabric_util:remove_done_workers(WorkersDict, nil),
            fabric_util:log_timeout(DefunctWorkers, "group_info"),
            {error, timeout};
        Else ->
            Else
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _, {Counters, Resps, USet}) ->
    case fabric_ring:node_down(NodeRef, Counters, Resps) of
        {ok, Counters1} -> {ok, {Counters1, Resps, USet}};
        error -> {error, {nodedown, <<"progress not possible">>}}
    end;
handle_message({rexi_EXIT, Reason}, Shard, {Counters, Resps, USet}) ->
    case fabric_ring:handle_error(Shard, Counters, Resps) of
        {ok, Counters1} -> {ok, {Counters1, Resps, USet}};
        error -> {error, Reason}
    end;
handle_message({ok, Info}, Shard, {Counters, Resps, USet}) ->
    case fabric_ring:handle_response(Shard, Info, Counters, Resps) of
        {ok, {Counters1, Resps1}} ->
            {ok, {Counters1, Resps1, USet}};
        {stop, Resps1} ->
            {stop, build_final_response(USet, Resps1)}
    end;
handle_message(Reason, Shard, {Counters, Resps, USet}) ->
    case fabric_ring:handle_error(Shard, Counters, Resps) of
        {ok, Counters1} -> {ok, {Counters1, Resps, USet}};
        error -> {error, Reason}
    end.

build_final_response(USet, Responses) ->
    AccF = fabric_dict:fold(
        fun(#shard{name = Id, node = Node}, Info, Acc) ->
            IsPreferred = sets:is_element({Id, Node}, USet),
            dict:append(Id, {Node, IsPreferred, Info}, Acc)
        end,
        dict:new(),
        Responses
    ),
    Pending = aggregate_pending(AccF),
    Infos = get_infos(AccF),
    [{updates_pending, {Pending}} | merge_results(Infos)].

get_infos(Acc) ->
    Values = [V || {_, V} <- dict:to_list(Acc)],
    lists:flatten([Info || {_Node, _Pref, Info} <- lists:flatten(Values)]).

aggregate_pending(Dict) ->
    {Preferred, Total, Minimum} =
        dict:fold(
            fun(_Name, Results, {P, T, M}) ->
                {Preferred, Total, Minimum} = calculate_pending(Results),
                {P + Preferred, T + Total, M + Minimum}
            end,
            {0, 0, 0},
            Dict
        ),
    [
        {minimum, Minimum},
        {preferred, Preferred},
        {total, Total}
    ].

calculate_pending(Results) ->
    lists:foldl(
        fun
            ({_Node, true, Info}, {P, T, V}) ->
                Pending = couch_util:get_value(pending_updates, Info),
                {P + Pending, T + Pending, min(Pending, V)};
            ({_Node, false, Info}, {P, T, V}) ->
                Pending = couch_util:get_value(pending_updates, Info),
                {P, T + Pending, min(Pending, V)}
        end,
        {0, 0, infinity},
        Results
    ).

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
