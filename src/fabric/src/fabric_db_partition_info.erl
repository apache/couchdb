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

-module(fabric_db_partition_info).

-export([go/2]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

go(DbName, Partition) ->
    Shards = mem3:shards(DbName, <<Partition/binary, ":foo">>),
    Workers = fabric_util:submit_jobs(Shards, get_partition_info, [Partition]),
    RexiMon = fabric_util:create_monitors(Shards),
    Fun = fun handle_message/3,
    Acc0 = {fabric_dict:init(Workers, nil), []},
    try
        case fabric_util:recv(Workers, #shard.ref, Fun, Acc0) of
            {ok, Acc} -> {ok, Acc};
            {timeout, {WorkersDict, _}} ->
                DefunctWorkers = fabric_util:remove_done_workers(
                    WorkersDict,
                    nil
                ),
                fabric_util:log_timeout(
                    DefunctWorkers,
                    "get_partition_info"
                ),
                {error, timeout};
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

handle_message({ok, Sizes}, #shard{dbname=Name} = Shard, {Counters, Acc}) ->
    Acc2 = [Sizes | Acc],
    Counters1 = fabric_dict:erase(Shard, Counters),
    case fabric_dict:size(Counters1) =:= 0 of
        true ->
            [FirstInfo | RestInfos] = Acc2,
            PartitionInfo = get_max_partition_size(FirstInfo, RestInfos),
            {stop, [{db_name, Name} | format_partition(PartitionInfo)]};
        false ->
            {ok, {Counters1, Acc2}}
    end;
    
handle_message(_, _, Acc) ->
    {ok, Acc}.

get_max_partition_size(Max, []) ->
    Max;
get_max_partition_size(MaxInfo, [NextInfo | Rest]) ->
    {sizes, MaxSize} = lists:keyfind(sizes, 1, MaxInfo),
    {sizes, NextSize} = lists:keyfind(sizes, 1, NextInfo),

    {external, MaxExtSize} = lists:keyfind(external, 1, MaxSize),
    {external, NextExtSize} = lists:keyfind(external, 1, NextSize),
    case NextExtSize > MaxExtSize of 
        true ->
            get_max_partition_size(NextInfo, Rest);
        false ->
            get_max_partition_size(MaxInfo, Rest)
    end.


% for JS to work nicely we need to convert the size list
% to a jiffy object
format_partition(PartitionInfo) ->
    {value, {sizes, Size}, PartitionInfo1} = lists:keytake(sizes, 1, PartitionInfo),
    [{sizes, {Size}} | PartitionInfo1].

