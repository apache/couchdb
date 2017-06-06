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

-include("dreyfus.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([go/4]).

go(DbName, DDocId, IndexName, InfoLevel) when is_binary(DDocId) ->
    {ok, DDoc} = fabric:open_doc(DbName, <<"_design/", DDocId/binary>>, []),
    go(DbName, DDoc, IndexName, InfoLevel);

go(DbName, DDoc, IndexName, InfoLevel) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, dreyfus_rpc, InfoLevel, [DDoc, IndexName]),
    RexiMon = fabric_util:create_monitors(Shards),
    Acc0 = {fabric_dict:init(Workers, nil), []},
    try
        fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0)
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({rexi_DOWN, _, {_,NodeRef},_}, _Worker, {Counters, Acc}) ->
    case fabric_util:remove_down_workers(Counters, NodeRef) of
    {ok, NewCounters} ->
        {ok, {NewCounters, Acc}};
    error ->
        {error, {nodedown, <<"progress not possible">>}}
    end;

handle_message({rexi_EXIT, Reason}, Worker, {Counters, Acc}) ->
    NewCounters = fabric_dict:erase(Worker, Counters),
    case fabric_view:is_progress_possible(NewCounters) of
    true ->
        {ok, {NewCounters, Acc}};
    false ->
        {error, Reason}
    end;

handle_message({ok, Info}, Worker, {Counters, Acc}) ->
    case fabric_dict:lookup_element(Worker, Counters) of
    undefined ->
        % already heard from someone else in this range
        {ok, {Counters, Acc}};
    nil ->
        C1 = fabric_dict:store(Worker, ok, Counters),
        C2 = fabric_view:remove_overlapping_shards(Worker, C1),
        case fabric_dict:any(nil, C2) of
        true ->
            {ok, {C2, [Info|Acc]}};
        false ->
            {stop, merge_results(lists:flatten([Info|Acc]))}
        end
    end;

handle_message({error, Reason}, Worker, {Counters, Acc}) ->
    NewCounters = fabric_dict:erase(Worker, Counters),
    case fabric_view:is_progress_possible(NewCounters) of
    true ->
        {ok, {NewCounters, Acc}};
    false ->
        {error, Reason}
    end;
handle_message({'EXIT', _}, Worker, {Counters, Acc}) ->
    NewCounters = fabric_dict:erase(Worker, Counters),
    case fabric_view:is_progress_possible(NewCounters) of
    true ->
        {ok, {NewCounters, Acc}};
    false ->
        {error, {nodedown, <<"progress not possible">>}}
    end.

merge_results(Info) ->
    Dict = lists:foldl(fun({K,V},D0) -> orddict:append(K,V,D0) end,
        orddict:new(), Info),
    orddict:fold(fun
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
        (_, _, Acc) ->
            Acc
    end, [], Dict).
