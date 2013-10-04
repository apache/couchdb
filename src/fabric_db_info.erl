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

-module(fabric_db_info).

-export([go/1]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

go(DbName) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, get_db_info, []),
    RexiMon = fabric_util:create_monitors(Shards),
    Fun = fun handle_message/3,
    Acc0 = {fabric_dict:init(Workers, nil), []},
    try
        case fabric_util:recv(Workers, #shard.ref, Fun, Acc0) of
            {ok, Acc} -> {ok, Acc};
            {error, Error} -> throw(Error)
        end
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({rexi_DOWN, _, {_,NodeRef},_}, _Shard, {Counters, Acc}) ->
    case fabric_util:remove_down_workers(Counters, NodeRef) of
    {ok, NewCounters} ->
        {ok, {NewCounters, Acc}};
    error ->
        {error, {nodedown, <<"progress not possible">>}}
    end;

handle_message({rexi_EXIT, Reason}, Shard, {Counters, Acc}) ->
    NewCounters = fabric_dict:erase(Shard, Counters),
    case fabric_view:is_progress_possible(NewCounters) of
    true ->
        {ok, {NewCounters, Acc}};
    false ->
        {error, Reason}
    end;

handle_message({ok, Info}, #shard{dbname=Name} = Shard, {Counters, Acc}) ->
    case fabric_dict:lookup_element(Shard, Counters) of
    undefined ->
        % already heard from someone else in this range
        {ok, {Counters, Acc}};
    nil ->
        Seq = couch_util:get_value(update_seq, Info),
        C1 = fabric_dict:store(Shard, Seq, Counters),
        C2 = fabric_view:remove_overlapping_shards(Shard, C1),
        case fabric_dict:any(nil, C2) of
        true ->
            {ok, {C2, [Info|Acc]}};
        false ->
            {stop, [
                {db_name,Name},
                {update_seq, fabric_view_changes:pack_seqs(C2)} |
                merge_results(lists:flatten([Info|Acc]))
            ]}
        end
    end;
handle_message(_, _, Acc) ->
    {ok, Acc}.

merge_results(Info) ->
    Dict = lists:foldl(fun({K,V},D0) -> orddict:append(K,V,D0) end,
        orddict:new(), Info),
    orddict:fold(fun
        (doc_count, X, Acc) ->
            [{doc_count, lists:sum(X)} | Acc];
        (doc_del_count, X, Acc) ->
            [{doc_del_count, lists:sum(X)} | Acc];
        (purge_seq, X, Acc) ->
            [{purge_seq, lists:sum(X)} | Acc];
        (compact_running, X, Acc) ->
            [{compact_running, lists:member(true, X)} | Acc];
        (disk_size, X, Acc) ->
            [{disk_size, lists:sum(X)} | Acc];
        (other, X, Acc) ->
            [{other, {merge_other_results(X)}} | Acc];
        (disk_format_version, X, Acc) ->
            [{disk_format_version, lists:max(X)} | Acc];
        (_, _, Acc) ->
            Acc
    end, [{instance_start_time, <<"0">>}], Dict).

merge_other_results(Results) ->
    Dict = lists:foldl(fun({Props}, D) ->
        lists:foldl(fun({K,V},D0) -> orddict:append(K,V,D0) end, D, Props)
    end, orddict:new(), Results),
    orddict:fold(fun
        (data_size, X, Acc) ->
            [{data_size, lists:sum(X)} | Acc];
        (_, _, Acc) ->
            Acc
    end, [], Dict).
