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

-module(fabric_db_uuids).

-export([go/1]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

go(DbName) when is_binary(DbName) ->
    Shards = mem3:live_shards(DbName, [node() | nodes()]),
    Workers = fabric_util:submit_jobs(Shards, get_uuid, []),
    RexiMon = fabric_util:create_monitors(Shards),
    Acc0 = {fabric_dict:init(Workers, nil), []},
    try fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
        {timeout, {WorkersDict, _}} ->
            DefunctWorkers = fabric_util:remove_done_workers(WorkersDict, nil),
            fabric_util:log_timeout(DefunctWorkers, "db_uuids"),
            {error, timeout};
        Else ->
            Else
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _Shard, {Cntrs, Res}) ->
    case fabric_ring:node_down(NodeRef, Cntrs, Res, [all]) of
        {ok, Cntrs1} -> {ok, {Cntrs1, Res}};
        error -> {error, {nodedown, <<"progress not possible">>}}
    end;
handle_message({rexi_EXIT, Reason}, Shard, {Cntrs, Res}) ->
    case fabric_ring:handle_error(Shard, Cntrs, Res, [all]) of
        {ok, Cntrs1} -> {ok, {Cntrs1, Res}};
        error -> {error, Reason}
    end;
handle_message(Uuid, Shard, {Cntrs, Res}) when is_binary(Uuid) ->
    case fabric_ring:handle_response(Shard, Uuid, Cntrs, Res, [all]) of
        {ok, {Cntrs1, Res1}} ->
            {ok, {Cntrs1, Res1}};
        {stop, Res1} ->
            Uuids = fabric_dict:fold(
                fun(#shard{} = S, Id, #{} = Acc) ->
                    Acc#{Id => S#shard{ref = undefined}}
                end,
                #{},
                Res1
            ),
            {stop, Uuids}
    end;
handle_message(Reason, Shard, {Cntrs, Res}) ->
    case fabric_ring:handle_error(Shard, Cntrs, Res, [all]) of
        {ok, Cntrs1} -> {ok, {Cntrs1, Res}};
        error -> {error, Reason}
    end.
