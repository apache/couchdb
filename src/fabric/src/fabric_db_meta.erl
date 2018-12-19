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

-module(fabric_db_meta).

-export([set_revs_limit/3, set_security/3, get_all_security/2,
    set_purge_infos_limit/3]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-record(acc, {
    workers,
    finished,
    num_workers
}).


set_revs_limit(DbName, Limit, Options) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, set_revs_limit, [Limit, Options]),
    Handler = fun handle_revs_message/3,
    Acc0 = {Workers, length(Workers) - 1},
    case fabric_util:recv(Workers, #shard.ref, Handler, Acc0) of
    {ok, ok} ->
        ok;
    {timeout, {DefunctWorkers, _}} ->
        fabric_util:log_timeout(DefunctWorkers, "set_revs_limit"),
        {error, timeout};
    Error ->
        Error
    end.

handle_revs_message(ok, _, {_Workers, 0}) ->
    {stop, ok};
handle_revs_message(ok, Worker, {Workers, Waiting}) ->
    {ok, {lists:delete(Worker, Workers), Waiting - 1}};
handle_revs_message(Error, _, _Acc) ->
    {error, Error}.


set_purge_infos_limit(DbName, Limit, Options) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, set_purge_infos_limit, [Limit, Options]),
    Handler = fun handle_purge_message/3,
    Acc0 = {Workers, length(Workers) - 1},
    case fabric_util:recv(Workers, #shard.ref, Handler, Acc0) of
        {ok, ok} ->
            ok;
        {timeout, {DefunctWorkers, _}} ->
            fabric_util:log_timeout(DefunctWorkers, "set_purged_docs_limit"),
            {error, timeout};
        Error ->
            Error
    end.

handle_purge_message(ok, _, {_Workers, 0}) ->
    {stop, ok};
handle_purge_message(ok, Worker, {Workers, Waiting}) ->
    {ok, {lists:delete(Worker, Workers), Waiting - 1}};
handle_purge_message(Error, _, _Acc) ->
    {error, Error}.


set_security(DbName, SecObj, Options) ->
    Shards = mem3:shards(DbName),
    RexiMon = fabric_util:create_monitors(Shards),
    Workers = fabric_util:submit_jobs(Shards, set_security, [SecObj, Options]),
    Handler = fun handle_set_message/3,
    Acc = #acc{
        workers=Workers,
        finished=[],
        num_workers=length(Workers)
    },
    try fabric_util:recv(Workers, #shard.ref, Handler, Acc) of
    {ok, #acc{finished=Finished}} ->
        case check_sec_set(length(Workers), Finished) of
            ok -> ok;
            Error -> Error
        end;
    {timeout, #acc{workers=DefunctWorkers}} ->
        fabric_util:log_timeout(DefunctWorkers, "set_security"),
        {error, timeout};
    Error ->
        Error
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_set_message({rexi_DOWN, _, {_, Node}, _}, _, #acc{workers=Wrkrs}=Acc) ->
    RemWorkers = lists:filter(fun(S) -> S#shard.node =/= Node end, Wrkrs),
    maybe_finish_set(Acc#acc{workers=RemWorkers});
handle_set_message(ok, W, Acc) ->
    NewAcc = Acc#acc{
        workers = (Acc#acc.workers -- [W]),
        finished = [W | Acc#acc.finished]
    },
    maybe_finish_set(NewAcc);
handle_set_message({rexi_EXIT, {maintenance_mode, _}}, W, Acc) ->
    NewAcc = Acc#acc{workers = (Acc#acc.workers -- [W])},
    maybe_finish_set(NewAcc);
handle_set_message(Error, W, Acc) ->
    Dst = {W#shard.node, W#shard.name},
    couch_log:error("Failed to set security object on ~p :: ~p", [Dst, Error]),
    NewAcc = Acc#acc{workers = (Acc#acc.workers -- [W])},
    maybe_finish_set(NewAcc).

maybe_finish_set(#acc{workers=[]}=Acc) ->
    {stop, Acc};
maybe_finish_set(#acc{finished=Finished, num_workers=NumWorkers}=Acc) ->
    case check_sec_set(NumWorkers, Finished) of
        ok -> {stop, Acc};
        _ -> {ok, Acc}
    end.

check_sec_set(NumWorkers, SetWorkers) ->
    try
        check_sec_set_int(NumWorkers, SetWorkers)
    catch throw:Reason ->
        {error, Reason}
    end.

check_sec_set_int(NumWorkers, SetWorkers) ->
    case length(SetWorkers) < ((NumWorkers div 2) + 1) of
        true -> throw(no_majority);
        false -> ok
    end,
    % Hack to reuse fabric_view:is_progress_possible/1
    FakeCounters = [{S, 0} || S <- SetWorkers],
    case fabric_view:is_progress_possible(FakeCounters) of
        false -> throw(no_ring);
        true -> ok
    end,
    ok.


get_all_security(DbName, Options) ->
    Shards = case proplists:get_value(shards, Options) of
        Shards0 when is_list(Shards0) -> Shards0;
        _ -> mem3:shards(DbName)
    end,
    RexiMon = fabric_util:create_monitors(Shards),
    Workers = fabric_util:submit_jobs(Shards, get_all_security, [[?ADMIN_CTX]]),
    Handler = fun handle_get_message/3,
    Acc = #acc{
        workers=Workers,
        finished=[],
        num_workers=length(Workers)
    },
    try fabric_util:recv(Workers, #shard.ref, Handler, Acc) of
    {ok, #acc{finished=SecObjs}} when length(SecObjs) > length(Workers) / 2 ->
        {ok, SecObjs};
    {ok, _} ->
        {error, no_majority};
    {timeout, #acc{workers=DefunctWorkers}} ->
        fabric_util:log_timeout(
            DefunctWorkers,
            "get_all_security"
        ),
        {error, timeout};
    Error ->
        Error
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_get_message({rexi_DOWN, _, {_, Node}, _}, _, #acc{workers=Wrkrs}=Acc) ->
    RemWorkers = lists:filter(fun(S) -> S#shard.node =/= Node end, Wrkrs),
    maybe_finish_get(Acc#acc{workers=RemWorkers});
handle_get_message({Props}=SecObj, W, Acc) when is_list(Props) ->
    NewAcc = Acc#acc{
        workers = (Acc#acc.workers -- [W]),
        finished = [{W, SecObj} | Acc#acc.finished]
    },
    maybe_finish_get(NewAcc);
handle_get_message({rexi_EXIT, {maintenance_mode, _}}, W, Acc) ->
    NewAcc = Acc#acc{workers = (Acc#acc.workers -- [W])},
    maybe_finish_get(NewAcc);
handle_get_message(Error, W, Acc) ->
    Dst = {W#shard.node, W#shard.name},
    couch_log:error("Failed to get security object on ~p :: ~p", [Dst, Error]),
    NewAcc = Acc#acc{workers = (Acc#acc.workers -- [W])},
    maybe_finish_get(NewAcc).

maybe_finish_get(#acc{workers=[]}=Acc) ->
    {stop, Acc};
maybe_finish_get(Acc) ->
    {ok, Acc}.
