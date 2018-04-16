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

-export([go/1, go/2]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

go(DbName) ->
    go(DbName, [{format, aggregate}]).

go(DbName, Options) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, get_db_info, []),
    RexiMon = fabric_util:create_monitors(Shards),
    Fun = case couch_util:get_value(format, Options, aggregate) of
        set -> fun handle_set_message/3;
        _ -> fun handle_aggr_message/3
    end,
    {ok, ClusterInfo} = get_cluster_info(Shards),
    Acc0 = fabric_dict:init(Workers, nil),
    try
        case fabric_util:recv(Workers, #shard.ref, Fun, Acc0) of
            {ok, Acc} ->
                {ok, [{cluster, {ClusterInfo}} | Acc]};
            {timeout, {WorkersDict, _}} ->
                DefunctWorkers = fabric_util:remove_done_workers(
                    WorkersDict,
                    nil
                ),
                fabric_util:log_timeout(
                    DefunctWorkers,
                    "get_db_info"
                ),
                {error, timeout};
            {error, Error} ->
                throw(Error)
        end
    after
        rexi_monitor:stop(RexiMon)
    end.


handle_aggr_message({ok, Info}, Shard, Counters) ->
    case fabric_dict:lookup_element(Shard, Counters) of
        undefined ->
            % already heard from someone else in this range
            {ok, Counters};
        nil ->
            C1 = fabric_dict:store(Shard, Info, Counters),
            C2 = fabric_view:remove_overlapping_shards(Shard, C1),
            case fabric_dict:any(nil, C2) of
                true ->
                    {ok, C2};
                false ->
                    {stop, merge_results(C2)}
            end
    end;

handle_aggr_message({rexi_DOWN, _, {_,NodeRef},_}, _Shard, Counters) ->
    case fabric_util:remove_down_workers(Counters, NodeRef) of
        {ok, NewCounters} ->
            {ok, NewCounters};
        error ->
            {error, {nodedown, <<"progress not possible">>}}
    end;

handle_aggr_message({rexi_EXIT, Reason}, Shard, Counters) ->
    NewCounters = fabric_dict:erase(Shard, Counters),
    case fabric_view:is_progress_possible(NewCounters) of
        true ->
            {ok, NewCounters};
        false ->
            {error, Reason}
    end;

handle_aggr_message(Else, _Shard, _Acc) ->
    %% TODO: do we want this behavior change?
    {error, Else}.


handle_set_message({ok, Info}, Shard, Counters) ->
    C1 = fabric_dict:store(Shard, Info, Counters),
    case fabric_dict:any(nil, C1) of
        true ->
            {ok, C1};
        false ->
            {stop, [{shards, {format_results(C1)}}]}
    end;

handle_set_message({rexi_DOWN, _, {_,NodeRef},_}, _Shard, Counters) ->
    %% TODO: add error info to Counters rather than deleting
    case fabric_util:remove_down_workers(Counters, NodeRef) of
        {ok, NewCounters} ->
            {ok, NewCounters};
        error ->
            {error, {nodedown, <<"progress not possible">>}}
    end;

handle_set_message({rexi_EXIT, Reason}, Shard, Counters) ->
    %% TODO: add error info to Counters rather than deleting
    NewCounters = fabric_dict:erase(Shard, Counters),
    case fabric_view:is_progress_possible(NewCounters) of
        true ->
            {ok, NewCounters};
        false ->
            {error, Reason}
    end;

handle_set_message(Else, _Shard, _Acc) ->
    %% TODO: do we want this behavior change?
    {error, Else}.


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
        (disk_size, X, Acc) -> % legacy
            [{disk_size, lists:sum(X)} | Acc];
        (data_size, X, Acc) -> % legacy
            [{data_size, lists:sum(X)} | Acc];
        (sizes, X, Acc) ->
            [{sizes, {merge_object(X)}} | Acc];
        (other, X, Acc) -> % legacy
            [{other, {merge_other_results(X)}} | Acc];
        (disk_format_version, X, Acc) ->
            [{disk_format_version, lists:max(X)} | Acc];
        (cluster, [X], Acc) ->
            [{cluster, {X}} | Acc];
        (_, _, Acc) ->
            Acc
    end, [{instance_start_time, <<"0">>}], Dict).


format_results(Counters) ->
    dict:to_list(lists:foldl(
        fun({S,I0}, D) ->
            #shard{
               range = [B, E],
               node = Node
            } = S,
            I = [{node, Node} | I0],
            HB = couch_util:to_hex(<<B:32/integer>>),
            HE = couch_util:to_hex(<<E:32/integer>>),
            R = list_to_binary(HB ++ "-" ++ HE),
            case dict:find(R, D) of
                {ok, L} ->
                    dict:store(R, [{I} | L], D);
                error ->
                    dict:store(R, [{I}], D)
            end
        end,
        dict:new(),
        fabric_dict:to_list(Counters)
    )).

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

merge_object(Objects) ->
    Dict = lists:foldl(fun({Props}, D) ->
        lists:foldl(fun({K,V},D0) -> orddict:append(K,V,D0) end, D, Props)
    end, orddict:new(), Objects),
    orddict:fold(fun
        (Key, X, Acc) ->
            [{Key, lists:sum(X)} | Acc]
    end, [], Dict).

get_cluster_info(Shards) ->
    Dict = lists:foldl(fun(#shard{range = R}, Acc) ->
        dict:update_counter(R, 1, Acc)
    end, dict:new(), Shards),
    Q = dict:size(Dict),
    N = dict:fold(fun(_, X, Acc) -> max(X, Acc) end, 0, Dict),
    %% defaults as per mem3:quorum/1
    WR = N div 2 + 1,
    {ok, [{q, Q}, {n, N}, {w, WR}, {r, WR}]}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_cluster_info_test_() ->
    {
        setup,
        fun setup/0,
        fun get_cluster_info_test_generator/1
    }.


setup() ->
    Quorums = [1, 2, 3],
    Shards = [1, 3, 5, 8, 12, 24],
    [{N, Q} || N <- Quorums, Q <- Shards].

get_cluster_info_test_generator([]) ->
    [];
get_cluster_info_test_generator([{N, Q} | Rest]) ->
    {generator,
    fun() ->
        Nodes = lists:seq(1, 8),
        Shards = mem3_util:create_partition_map(<<"foo">>, N, Q, Nodes),
        {ok, Info} = get_cluster_info(Shards),
        [
            ?_assertEqual(N, couch_util:get_value(n, Info)),
            ?_assertEqual(Q, couch_util:get_value(q, Info))
        ] ++ get_cluster_info_test_generator(Rest)
    end}.

-endif.
