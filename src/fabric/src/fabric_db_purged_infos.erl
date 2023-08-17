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

-module(fabric_db_purged_infos).

-export([go/1]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

-record(pacc, {
    counters,
    replies,
    ring_opts
}).

go(DbName) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, get_purged_infos, []),
    RexiMon = fabric_util:create_monitors(Shards),
    Fun = fun handle_message/3,
    Acc0 = #pacc{
        counters = fabric_dict:init(Workers, nil),
        replies = [],
        ring_opts = [{any, Shards}]
    },
    try
        case fabric_util:recv(Workers, #shard.ref, Fun, Acc0) of
            {ok, Res} ->
                {ok, Res};
            {timeout, {WorkersDict, _}} ->
                DefunctWorkers = fabric_util:remove_done_workers(WorkersDict, nil),
                fabric_util:log_timeout(DefunctWorkers, "get_purged_infos"),
                {error, timeout};
            {error, Error} ->
                throw(Error)
        end
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _Shard, #pacc{} = Acc) ->
    #pacc{counters = Counters, ring_opts = RingOpts} = Acc,
    case fabric_util:remove_down_workers(Counters, NodeRef, RingOpts) of
        {ok, NewCounters} ->
            {ok, Acc#pacc{counters = NewCounters}};
        error ->
            {error, {nodedown, <<"progress not possible">>}}
    end;
handle_message({rexi_EXIT, Reason}, Shard, #pacc{} = Acc) ->
    #pacc{counters = Counters, ring_opts = RingOpts} = Acc,
    NewCounters = fabric_dict:erase(Shard, Counters),
    case fabric_ring:is_progress_possible(NewCounters, RingOpts) of
        true ->
            {ok, Acc#pacc{counters = NewCounters}};
        false ->
            {error, Reason}
    end;
handle_message({ok, Info}, #shard{} = Shard, #pacc{} = Acc) ->
    #pacc{counters = Counters, replies = Replies} = Acc,
    Replies1 = [Info | Replies],
    Counters1 = fabric_dict:erase(Shard, Counters),
    case fabric_dict:size(Counters1) =:= 0 of
        true ->
            {stop, lists:flatten(Replies1)};
        false ->
            {ok, Acc#pacc{counters = Counters1, replies = Replies1}}
    end;
handle_message(_, _, #pacc{} = Acc) ->
    {ok, Acc}.
