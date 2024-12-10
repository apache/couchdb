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

-module(fabric_registered_replication_peers).

-export([go/1]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

go(DbName) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, get_registered_replication_peers, []),
    RexiMon = fabric_util:create_monitors(Shards),
    Acc0 = {fabric_dict:init(Workers, nil), []},
    try fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
        {timeout, {WorkersDict, _}} ->
            DefunctWorkers = fabric_util:remove_done_workers(WorkersDict, nil),
            fabric_util:log_timeout(DefunctWorkers, "get_registered_replication_peers"),
            {error, timeout};
        Else ->
            Else
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _Shard, {Counters, Resps}) ->
    case fabric_ring:node_down(NodeRef, Counters, Resps) of
        {ok, Counters1} -> {ok, {Counters1, Resps}};
        error -> {error, {nodedown, <<"progress not possible">>}}
    end;
handle_message({rexi_EXIT, Reason}, Shard, {Counters, Resps}) ->
    case fabric_ring:handle_error(Shard, Counters, Resps) of
        {ok, Counters1} -> {ok, {Counters1, Resps}};
        error -> {error, Reason}
    end;
handle_message({ok, Reds}, Shard, {Counters, Resps}) ->
    case fabric_ring:handle_response(Shard, Reds, Counters, Resps) of
        {ok, {Counters1, Resps1}} ->
            {ok, {Counters1, Resps1}};
        {stop, Resps1} ->
            FinalRed = fabric_dict:fold(
                fun(_, Red, Acc) -> couch_bt_engine:local_tree_reduce(rereduce, [Red, Acc]) end,
                #{},
                Resps1
            ),
            {stop, FinalRed}
    end;
handle_message(Reason, Shard, {Counters, Resps}) ->
    case fabric_ring:handle_error(Shard, Counters, Resps) of
        {ok, Counters1} -> {ok, {Counters1, Resps}};
        error -> {error, Reason}
    end.
