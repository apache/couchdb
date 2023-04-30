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

-include_lib("mem3/include/mem3.hrl").

go(DbName, DDocId, IndexName) when is_binary(DDocId) ->
    {ok, DDoc} = fabric:open_doc(DbName, <<"_design/", DDocId/binary>>, [ejson_body]),
    go(DbName, DDoc, IndexName);
go(DbName, DDoc, IndexName) ->
    {ok, Index} = nouveau_util:design_doc_to_index(DbName, DDoc, IndexName),
    Shards = mem3:shards(DbName),
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

    Acc0 = {fabric_dict:init(Workers, nil), #{}},
    try
        fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0)
    after
        rexi_monitor:stop(RexiMon),
        fabric_util:cleanup(Workers)
    end.

handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _Worker, {Counters, Acc}) ->
    case fabric_util:remove_down_workers(Counters, NodeRef) of
        {ok, NewCounters} ->
            {ok, {NewCounters, Acc}};
        error ->
            {error, {nodedown, <<"progress not possible">>}}
    end;
handle_message({rexi_EXIT, Reason}, Worker, {Counters, Acc}) ->
    NewCounters = fabric_dict:erase(Worker, Counters),
    case fabric_ring:is_progress_possible(NewCounters) of
        true ->
            {ok, {NewCounters, Acc}};
        false ->
            {error, Reason}
    end;
handle_message({ok, Info}, Worker, {Counters, Acc0}) ->
    case fabric_dict:lookup_element(Worker, Counters) of
        undefined ->
            % already heard from someone else in this range
            {ok, {Counters, Acc0}};
        nil ->
            C1 = fabric_dict:store(Worker, ok, Counters),
            C2 = fabric_view:remove_overlapping_shards(Worker, C1),
            Acc1 = maps:merge_with(fun merge_info/3, Info, Acc0),
            case fabric_dict:any(nil, C2) of
                true ->
                    {ok, {C2, Acc1}};
                false ->
                    {stop, Acc1}
            end
    end;
handle_message({error, Reason}, Worker, {Counters, Acc}) ->
    NewCounters = fabric_dict:erase(Worker, Counters),
    case fabric_ring:is_progress_possible(NewCounters) of
        true ->
            {ok, {NewCounters, Acc}};
        false ->
            {error, Reason}
    end;
handle_message({'EXIT', _}, Worker, {Counters, Acc}) ->
    NewCounters = fabric_dict:erase(Worker, Counters),
    case fabric_ring:is_progress_possible(NewCounters) of
        true ->
            {ok, {NewCounters, Acc}};
        false ->
            {error, {nodedown, <<"progress not possible">>}}
    end.

merge_info(_Key, Val1, Val2) ->
    Val1 + Val2.
