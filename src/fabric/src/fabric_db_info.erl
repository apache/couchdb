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
    CreationTime = mem3:shard_creation_time(DbName),
    Workers = fabric_util:submit_jobs(Shards, get_db_info, []),
    RexiMon = fabric_util:create_monitors(Shards),
    Fun = fun handle_message/3,
    {ok, ClusterInfo} = get_cluster_info(Shards),
    CInfo = [{cluster, ClusterInfo}],
    Acc0 = {fabric_dict:init(Workers, nil), [], CInfo},
    try
        case fabric_util:recv(Workers, #shard.ref, Fun, Acc0) of
            {ok, Acc} ->
                {ok, [{instance_start_time, CreationTime} | Acc]};
            {timeout, {WorkersDict, _, _}} ->
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

handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _, {Counters, Resps, CInfo}) ->
    case fabric_ring:node_down(NodeRef, Counters, Resps) of
        {ok, Counters1} -> {ok, {Counters1, Resps, CInfo}};
        error -> {error, {nodedown, <<"progress not possible">>}}
    end;
handle_message({rexi_EXIT, Reason}, Shard, {Counters, Resps, CInfo}) ->
    case fabric_ring:handle_error(Shard, Counters, Resps) of
        {ok, Counters1} -> {ok, {Counters1, Resps, CInfo}};
        error -> {error, Reason}
    end;
handle_message({ok, Info}, Shard, {Counters, Resps, CInfo}) ->
    case fabric_ring:handle_response(Shard, Info, Counters, Resps) of
        {ok, {Counters1, Resps1}} ->
            {ok, {Counters1, Resps1, CInfo}};
        {stop, Resps1} ->
            {stop, build_final_response(CInfo, Shard#shard.dbname, Resps1)}
    end;
handle_message(Reason, Shard, {Counters, Resps, CInfo}) ->
    case fabric_ring:handle_error(Shard, Counters, Resps) of
        {ok, Counters1} -> {ok, {Counters1, Resps, CInfo}};
        error -> {error, Reason}
    end.

build_final_response(CInfo, DbName, Responses) ->
    AccF = fabric_dict:fold(
        fun(Shard, Info, {Seqs, PSeqs, Infos}) ->
            Seq = build_seq(Shard, Info),
            PSeq = couch_util:get_value(purge_seq, Info),
            {[{Shard, Seq} | Seqs], [{Shard, PSeq} | PSeqs], [Info | Infos]}
        end,
        {[], [], []},
        Responses
    ),
    {Seqs, PSeqs, Infos} = AccF,
    PackedSeq = fabric_view_changes:pack_seqs(Seqs),
    PackedPSeq = fabric_view_changes:pack_seqs(PSeqs),
    MergedInfos = merge_results(lists:flatten([CInfo | Infos])),
    Sequences = [{purge_seq, PackedPSeq}, {update_seq, PackedSeq}],
    [{db_name, DbName}] ++ Sequences ++ MergedInfos.

build_seq(#shard{node = Node}, Info) when is_list(Info) ->
    Seq = couch_util:get_value(update_seq, Info),
    Uuid = couch_util:get_value(uuid, Info),
    PrefixLen = fabric_util:get_uuid_prefix_len(),
    {Seq, binary:part(Uuid, {0, PrefixLen}), Node}.

merge_results(Info) ->
    Dict = lists:foldl(
        fun({K, V}, D0) -> orddict:append(K, V, D0) end,
        orddict:new(),
        Info
    ),
    orddict:fold(
        fun
            (doc_count, X, Acc) ->
                [{doc_count, lists:sum(X)} | Acc];
            (doc_del_count, X, Acc) ->
                [{doc_del_count, lists:sum(X)} | Acc];
            (compact_running, X, Acc) ->
                [{compact_running, lists:member(true, X)} | Acc];
            (sizes, X, Acc) ->
                [{sizes, {merge_object(X)}} | Acc];
            (disk_format_version, X, Acc) ->
                [{disk_format_version, lists:max(X)} | Acc];
            (cluster, [X], Acc) ->
                [{cluster, {X}} | Acc];
            (access, [X], Acc) ->
                [{access, X} | Acc];
            (props, Xs, Acc) ->
                [{props, {merge_object(Xs)}} | Acc];
            (_K, _V, Acc) ->
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
        fun
            (Key, [X | _] = Xs, Acc) when is_integer(X) ->
                [{Key, lists:sum(Xs)} | Acc];
            (Key, [X | _] = Xs, Acc) when is_boolean(X) ->
                [{Key, lists:all(fun all_true/1, Xs)} | Acc];
            (_Key, _Xs, Acc) ->
                Acc
        end,
        [],
        Dict
    ).

all_true(true) -> true;
all_true(_) -> false.

get_cluster_info(Shards) ->
    Dict = lists:foldl(
        fun(#shard{range = R}, Acc) ->
            dict:update_counter(R, 1, Acc)
        end,
        dict:new(),
        Shards
    ),
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
    {generator, fun() ->
        Nodes = lists:seq(1, 8),
        Shards = mem3_util:create_partition_map(<<"foo">>, N, Q, Nodes),
        {ok, Info} = get_cluster_info(Shards),
        [
            ?_assertEqual(N, couch_util:get_value(n, Info)),
            ?_assertEqual(Q, couch_util:get_value(q, Info))
        ] ++ get_cluster_info_test_generator(Rest)
    end}.

-endif.
