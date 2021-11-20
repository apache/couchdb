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

-record(acc, {
    counters,
    replies,
    ring_opts
}).

go(DbName, Partition) ->
    Shards = mem3:shards(DbName, couch_partition:shard_key(Partition)),
    Workers = fabric_util:submit_jobs(Shards, get_partition_info, [Partition]),
    RexiMon = fabric_util:create_monitors(Shards),
    Fun = fun handle_message/3,
    Acc0 = #acc{
        counters = fabric_dict:init(Workers, nil),
        replies = [],
        ring_opts = [{any, Shards}]
    },
    try
        case fabric_util:recv(Workers, #shard.ref, Fun, Acc0) of
            {ok, Res} ->
                {ok, Res};
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
            {error, Error} ->
                throw(Error)
        end
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _Shard, #acc{} = Acc) ->
    #acc{counters = Counters, ring_opts = RingOpts} = Acc,
    case fabric_util:remove_down_workers(Counters, NodeRef, RingOpts) of
        {ok, NewCounters} ->
            {ok, Acc#acc{counters = NewCounters}};
        error ->
            {error, {nodedown, <<"progress not possible">>}}
    end;
handle_message({rexi_EXIT, Reason}, Shard, #acc{} = Acc) ->
    #acc{counters = Counters, ring_opts = RingOpts} = Acc,
    NewCounters = fabric_dict:erase(Shard, Counters),
    case fabric_ring:is_progress_possible(NewCounters, RingOpts) of
        true ->
            {ok, Acc#acc{counters = NewCounters}};
        false ->
            {error, Reason}
    end;
handle_message({ok, Info}, #shard{dbname = Name} = Shard, #acc{} = Acc) ->
    #acc{counters = Counters, replies = Replies} = Acc,
    Replies1 = [Info | Replies],
    Counters1 = fabric_dict:erase(Shard, Counters),
    case fabric_dict:size(Counters1) =:= 0 of
        true ->
            [FirstInfo | RestInfos] = Replies1,
            PartitionInfo = get_max_partition_size(FirstInfo, RestInfos),
            {stop, [{db_name, Name} | format_partition(PartitionInfo)]};
        false ->
            {ok, Acc#acc{counters = Counters1, replies = Replies1}}
    end;
handle_message(_, _, #acc{} = Acc) ->
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

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

node_down_test() ->
    [S1, S2] = [mk_shard("n1", [0, 4]), mk_shard("n2", [0, 8])],
    Acc1 = #acc{
        counters = fabric_dict:init([S1, S2], nil),
        ring_opts = [{any, [S1, S2]}]
    },

    N1 = S1#shard.node,
    {ok, Acc2} = handle_message({rexi_DOWN, nil, {nil, N1}, nil}, nil, Acc1),
    ?assertEqual([{S2, nil}], Acc2#acc.counters),

    N2 = S2#shard.node,
    ?assertEqual(
        {error, {nodedown, <<"progress not possible">>}},
        handle_message({rexi_DOWN, nil, {nil, N2}, nil}, nil, Acc2)
    ).

worker_exit_test() ->
    [S1, S2] = [mk_shard("n1", [0, 4]), mk_shard("n2", [0, 8])],
    Acc1 = #acc{
        counters = fabric_dict:init([S1, S2], nil),
        ring_opts = [{any, [S1, S2]}]
    },

    {ok, Acc2} = handle_message({rexi_EXIT, boom}, S1, Acc1),
    ?assertEqual([{S2, nil}], Acc2#acc.counters),

    ?assertEqual({error, bam}, handle_message({rexi_EXIT, bam}, S2, Acc2)).

mk_shard(Name, Range) ->
    Node = list_to_atom(Name),
    BName = list_to_binary(Name),
    #shard{name = BName, node = Node, range = Range}.

-endif.
