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

-module(fabric_time_seq).

-export([
    since/2,
    histogram/2,
    set_time_seq/3,
    get_time_seq/2
]).

-include_lib("mem3/include/mem3.hrl").

% Since sequence RPC call.

% Return a clustered changes sequence which starts before the provided timestamp argument

since(DbName, Time) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, time_seq_since, [Time]),
    RexiMon = fabric_util:create_monitors(Shards),
    Acc0 = {fabric_dict:init(Workers, nil), []},
    try fabric_util:recv(Workers, #shard.ref, fun since_handle_message/3, Acc0) of
        {timeout, {WorkersDict, _}} ->
            DefunctWorkers = fabric_util:remove_done_workers(WorkersDict, nil),
            fabric_util:log_timeout(DefunctWorkers, "time_seq_since"),
            {error, timeout};
        Else ->
            Else
    after
        rexi_monitor:stop(RexiMon)
    end.

since_handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _Shard, {Counters, Resps}) ->
    case fabric_ring:node_down(NodeRef, Counters, Resps) of
        {ok, Counters1} -> {ok, {Counters1, Resps}};
        error -> {error, {nodedown, <<"progress not possible">>}}
    end;
since_handle_message({rexi_EXIT, Reason}, Shard, {Counters, Resps}) ->
    case fabric_ring:handle_error(Shard, Counters, Resps) of
        {ok, Counters1} -> {ok, {Counters1, Resps}};
        error -> {error, Reason}
    end;
since_handle_message(Sequence, Shard, {Counters, Resps}) when is_integer(Sequence) ->
    case fabric_ring:handle_response(Shard, Sequence, Counters, Resps) of
        {ok, {Counters1, Resps1}} ->
            {ok, {Counters1, Resps1}};
        {stop, Resps1} ->
            Seqs = fabric_dict:fold(
                fun(S, Seq, Acc) ->
                    [{S#shard{ref = undefined}, Seq} | Acc]
                end,
                [],
                Resps1
            ),
            {stop, fabric_view_changes:pack_seqs(Seqs)}
    end;
since_handle_message(Reason, Shard, {Counters, Resps}) ->
    case fabric_ring:handle_error(Shard, Counters, Resps) of
        {ok, Counters1} -> {ok, {Counters1, Resps}};
        error -> {error, Reason}
    end.

% Histogram time_seq RPC call

% Return a per/range, per/node histogram nested map result
%    #{Range => Node => Histogram}
%
histogram(DbName, Options) when is_binary(DbName) ->
    Shards = mem3:live_shards(DbName, [config:node_name() | nodes()]),
    Workers = fabric_util:submit_jobs(Shards, time_seq_histogram, [Options]),
    RexiMon = fabric_util:create_monitors(Shards),
    Acc0 = {fabric_dict:init(Workers, nil), []},
    try fabric_util:recv(Workers, #shard.ref, fun histogram_handle_message/3, Acc0) of
        {timeout, {WorkersDict, _}} ->
            DefunctWorkers = fabric_util:remove_done_workers(WorkersDict, nil),
            fabric_util:log_timeout(DefunctWorkers, "time_seq_histogram"),
            {error, timeout};
        Else ->
            Else
    after
        rexi_monitor:stop(RexiMon)
    end.

histogram_handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _Shard, {Cntrs, Res}) ->
    case fabric_ring:node_down(NodeRef, Cntrs, Res, [all]) of
        {ok, Cntrs1} -> {ok, {Cntrs1, Res}};
        error -> {error, {nodedown, <<"progress not possible">>}}
    end;
histogram_handle_message({rexi_EXIT, Reason}, Shard, {Cntrs, Res}) ->
    case fabric_ring:handle_error(Shard, Cntrs, Res, [all]) of
        {ok, Cntrs1} -> {ok, {Cntrs1, Res}};
        error -> {error, Reason}
    end;
histogram_handle_message(Hist, Shard, {Cntrs, Res}) when is_list(Hist) ->
    case fabric_ring:handle_response(Shard, Hist, Cntrs, Res, [all]) of
        {ok, {Cntrs1, Res1}} ->
            {ok, {Cntrs1, Res1}};
        {stop, Res1} ->
            FoldF = fun(#shard{range = R, node = N}, S, Acc) -> [{R, N, S} | Acc] end,
            AsList = fabric_dict:fold(FoldF, [], Res1),
            ByRangeKeyF = fun({R, _, _}) -> R end,
            ByRangeValF = fun({_, N, S}) -> {N, S} end,
            % #{Range1 = [{Node1, Hist1}, {Node2, Hist2}], Range2 => [...], ...}
            ByRange = maps:groups_from_list(ByRangeKeyF, ByRangeValF, AsList),
            MapF = fun(_, ByNode) -> maps:from_list(ByNode) end,
            Result = maps:map(MapF, ByRange),
            {stop, Result}
    end;
histogram_handle_message(Reason, Shard, {Cntrs, Res}) ->
    case fabric_ring:handle_error(Shard, Cntrs, Res, [all]) of
        {ok, Cntrs1} -> {ok, {Cntrs1, Res}};
        error -> {error, Reason}
    end.

% Set time_seq RPC call

set_time_seq(DbName, TSeq, Options) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, set_time_seq, [TSeq, Options]),
    Handler = fun set_time_seq_handle_message/3,
    Acc0 = {Workers, length(Workers) - 1},
    case fabric_util:recv(Workers, #shard.ref, Handler, Acc0) of
        {ok, ok} ->
            ok;
        {timeout, {DefunctWorkers, _}} ->
            fabric_util:log_timeout(DefunctWorkers, "set_time_seq"),
            {error, timeout};
        Error ->
            Error
    end.

set_time_seq_handle_message(ok, _, {_Workers, 0}) ->
    {stop, ok};
set_time_seq_handle_message(ok, Worker, {Workers, Waiting}) ->
    {ok, {lists:delete(Worker, Workers), Waiting - 1}};
set_time_seq_handle_message(Error, _, _Acc) ->
    {error, Error}.

% Get time_seq RPC call

% Return a per/range, per/node time_seq nested map result
%    #{Range => Node => TSeq}
%
get_time_seq(DbName, Options) when is_binary(DbName) ->
    Shards = mem3:live_shards(DbName, [config:node_name() | nodes()]),
    Workers = fabric_util:submit_jobs(Shards, get_time_seq, [Options]),
    RexiMon = fabric_util:create_monitors(Shards),
    Acc0 = {fabric_dict:init(Workers, nil), []},
    try fabric_util:recv(Workers, #shard.ref, fun get_time_seq_handle_message/3, Acc0) of
        {timeout, {WorkersDict, _}} ->
            DefunctWorkers = fabric_util:remove_done_workers(WorkersDict, nil),
            fabric_util:log_timeout(DefunctWorkers, "get_time_seq"),
            {error, timeout};
        Else ->
            Else
    after
        rexi_monitor:stop(RexiMon)
    end.

get_time_seq_handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _Shard, {Cntrs, Res}) ->
    case fabric_ring:node_down(NodeRef, Cntrs, Res, [all]) of
        {ok, Cntrs1} -> {ok, {Cntrs1, Res}};
        error -> {error, {nodedown, <<"progress not possible">>}}
    end;
get_time_seq_handle_message({rexi_EXIT, Reason}, Shard, {Cntrs, Res}) ->
    case fabric_ring:handle_error(Shard, Cntrs, Res, [all]) of
        {ok, Cntrs1} -> {ok, {Cntrs1, Res}};
        error -> {error, Reason}
    end;
get_time_seq_handle_message(#{} = TSeq, Shard, {Cntrs, Res}) ->
    case fabric_ring:handle_response(Shard, TSeq, Cntrs, Res, [all]) of
        {ok, {Cntrs1, Res1}} ->
            {ok, {Cntrs1, Res1}};
        {stop, Res1} ->
            FoldF = fun(#shard{range = R, node = N}, S, Acc) -> [{R, N, S} | Acc] end,
            TSeqList = fabric_dict:fold(FoldF, [], Res1),
            ByRangeKeyF = fun({R, _, _}) -> R end,
            ByRangeValF = fun({_, N, S}) -> {N, S} end,
            % #{Range1 = [{Node1, TSeq1}, {Node2, TSeq2}], Range2 => [...], ...}
            TSeqsByRange = maps:groups_from_list(ByRangeKeyF, ByRangeValF, TSeqList),
            Result = maps:map(
                fun(_Range, NodeTimeSeqs) ->
                    maps:from_list(NodeTimeSeqs)
                end,
                TSeqsByRange
            ),
            {stop, Result}
    end;
get_time_seq_handle_message(Reason, Shard, {Cntrs, Res}) ->
    case fabric_ring:handle_error(Shard, Cntrs, Res, [all]) of
        {ok, Cntrs1} -> {ok, {Cntrs1, Res}};
        error -> {error, Reason}
    end.
