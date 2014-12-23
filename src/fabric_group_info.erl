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
    {ok, DDoc} = fabric:open_doc(DbName, GroupId, []),
    go(DbName, DDoc);

go(DbName, #doc{id=DDocId}) ->
    Shards = mem3:shards(DbName),
    Ushards = mem3:ushards(DbName),
    Workers = fabric_util:submit_jobs(Shards, group_info, [DDocId]),
    RexiMon = fabric_util:create_monitors(Shards),
    Acc = acc_init(Workers, Ushards),
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

handle_message({rexi_DOWN, _, {_,NodeRef},_}, _Shard,
        {Counters, Acc, Ushards}) ->
    case fabric_util:remove_down_workers(Counters, NodeRef) of
    {ok, NewCounters} ->
        {ok, {NewCounters, Acc, Ushards}};
    error ->
        {error, {nodedown, <<"progress not possible">>}}
    end;

handle_message({rexi_EXIT, Reason}, Shard, {Counters, Acc, Ushards}) ->
    NewCounters = lists:keydelete(Shard, #shard.ref, Counters),
    case fabric_view:is_progress_possible(NewCounters) of
    true ->
        {ok, {NewCounters, Acc, Ushards}};
    false ->
        {error, Reason}
    end;

handle_message({ok, Info}, Shard, {Counters0, Acc, Ushards}) ->
    NewAcc = append_result(Info, Shard, Acc, Ushards),
    Counters = fabric_dict:store(Shard, ok, Counters0),
    case is_complete(Counters) of
    false ->
        {ok, {Counters, NewAcc, Ushards}};
    true ->
        Pending = aggregate_pending(NewAcc),
        Infos = get_infos(NewAcc),
        Results = [{updates_pending, {Pending}} | merge_results(Infos)],
        {stop, Results}
    end;
handle_message(_, _, Acc) ->
    {ok, Acc}.

acc_init(Workers, Ushards) ->
    Set = sets:from_list([{Id, N} || #shard{name = Id, node = N} <- Ushards]),
    {fabric_dict:init(Workers, nil), dict:new(), Set}.

is_complete(Counters) ->
    not fabric_dict:any(nil, Counters).

append_result(Info, #shard{name = Name, node = Node}, Acc, Ushards) ->
    IsPreferred = sets:is_element({Name, Node}, Ushards),
    dict:append(Name, {Node, IsPreferred, Info}, Acc).

get_infos(Acc) ->
    Values = [V || {_, V} <- dict:to_list(Acc)],
    lists:flatten([Info || {_Node, _Pref, Info} <- lists:flatten(Values)]).

aggregate_pending(Dict) ->
    {Preferred, Total, Minimum} =
        dict:fold(fun(_Name, Results, {P, T, M}) ->
            {Preferred, Total, Minimum} = calculate_pending(Results),
            {P + Preferred, T + Total, M + Minimum}
        end, {0, 0, 0}, Dict),
    [
        {minimum, Minimum},
        {preferred, Preferred},
        {total, Total}
    ].

calculate_pending(Results) ->
    lists:foldl(fun
    ({_Node, true, Info}, {P, T, V}) ->
       Pending = couch_util:get_value(pending_updates, Info),
       {P + Pending, T + Pending, min(Pending, V)};
    ({_Node, false, Info}, {P, T, V}) ->
       Pending = couch_util:get_value(pending_updates, Info),
       {P, T + Pending, min(Pending, V)}
    end, {0, 0, infinity}, Results).

merge_results(Info) ->
    Dict = lists:foldl(fun({K,V},D0) -> orddict:append(K,V,D0) end,
        orddict:new(), Info),
    orddict:fold(fun
        (signature, [X | _], Acc) ->
            [{signature, X} | Acc];
        (language, [X | _], Acc) ->
            [{language, X} | Acc];
        (disk_size, X, Acc) -> % legacy
            [{disk_size, lists:sum(X)} | Acc];
        (data_size, X, Acc) -> % legacy
            [{data_size, lists:sum(X)} | Acc];
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
        (_, _, Acc) ->
            Acc
    end, [], Dict).

merge_object(Objects) ->
    Dict = lists:foldl(fun({Props}, D) ->
        lists:foldl(fun({K,V},D0) -> orddict:append(K,V,D0) end, D, Props)
    end, orddict:new(), Objects),
    orddict:fold(fun
        (Key, X, Acc) ->
            [{Key, lists:sum(X)} | Acc]
    end, [], Dict).
