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

-module(couch_auto_purge_plugin).
-behaviour(couch_scanner_plugin).

-export([
    start/2,
    resume/2,
    complete/1,
    checkpoint/1,
    db/2,
    db_opened/2,
    db_closing/2,
    doc_fdi/3
]).

-include_lib("couch_scanner/include/couch_scanner_plugin.hrl").
-include_lib("stdlib/include/assert.hrl").

start(ScanId, #{}) ->
    St = init_config(ScanId),
    ?INFO("Starting.", [], St),
    {ok, St}.

resume(ScanId, #{}) ->
    St = init_config(ScanId),
    ?INFO("Resuming.", [], St),
    {ok, St}.

complete(St) ->
    ?INFO("Completed", [], St),
    {ok, #{}}.

checkpoint(_St) ->
    {ok, #{}}.

db(St, DbName) ->
    case ttl(St, DbName) of
        TTL when is_integer(TTL) ->
            {ok, St#{ttl => TTL}};
        undefined ->
            {skip, St}
    end.

db_opened(#{} = St, Db) ->
    #{ttl := TTL, queue := Queue} = St,
    ?assert(Queue == [], "Queue is not empty from previous operations"),
    EndSeq = couch_time_seq:since(couch_db:get_time_seq(Db), couch_time_seq:timestamp() - TTL),
    ChangeOpts =
        if
            EndSeq == now -> [];
            true -> [{end_key, EndSeq}]
        end,
    ?INFO("scanning for deleted documents in ~s up to ~p", [couch_db:name(Db), EndSeq], meta(St)),
    {0, ChangeOpts, St#{count => 0, end_seq => EndSeq}}.

db_closing(#{} = St, Db) ->
    St1 = #{count := Count} = flush_queue(St, Db),
    ?INFO("purged ~B deleted documents from ~s", [Count, couch_db:name(Db)], meta(St1)),
    {ok, St1}.

doc_fdi(#{} = St, #full_doc_info{deleted = true} = FDI, Db) ->
    #{end_seq := EndSeq} = St,
    ?assert(
        FDI#full_doc_info.update_seq =< EndSeq, "FDI update_seq should not be greater than end seq"
    ),
    {ok, enqueue(St, FDI, Db)};
doc_fdi(#{} = St, #full_doc_info{}, _Db) ->
    {ok, St}.

enqueue(#{} = St, FDI, Db) ->
    {Id, Revs} = fdi_to_idrevs(FDI),
    enqueue(St, Id, Revs, Db).

enqueue(#{queue := Queue} = St0, Id, Revs, Db) ->
    CurrentQueueSize = queue_size(Queue),
    NewQueueSize = CurrentQueueSize + length(Revs),
    MinBatchSize = min_batch_size(),
    MaxBatchSize = max_batch_size(),
    if
        NewQueueSize > MaxBatchSize ->
            {RevBatch, RevRest} = lists:split(MaxBatchSize - CurrentQueueSize, Revs),
            St1 = flush_queue(St0#{queue := [{Id, RevBatch} | Queue]}, Db),
            enqueue(St1, Id, RevRest, Db);
        NewQueueSize >= MinBatchSize ->
            flush_queue(St0#{queue := [{Id, Revs} | Queue]}, Db);
        NewQueueSize < MinBatchSize ->
            St0#{queue := [{Id, Revs} | Queue]}
    end.

flush_queue(#{queue := []} = St, _Db) ->
    St;
flush_queue(#{queue := IdRevs} = St, Db) ->
    DbName = mem3:dbname(couch_db:name(Db)),
    N = mem3:n(DbName),
    PurgeFun = fun() -> fabric:purge_docs(DbName, IdRevs, [?ADMIN_CTX, {w, N}]) end,
    Timeout = fabric_util:request_timeout(),
    try fabric_util:isolate(PurgeFun, Timeout) of
        {Health, Results} when Health == ok; Health == accepted ->
            ?DEBUG(
                "flushed batch of ~B idrevs from ~s",
                [queue_size(IdRevs), couch_db:name(Db)],
                meta(St)
            ),
            #{count := Count, limiter := Limiter0} = St,
            {Wait, Limiter1} = couch_scanner_rate_limiter:update(
                Limiter0, doc_write, length(Results)
            ),
            timer:sleep(Wait),
            St#{
                count := Count + length(Results),
                limiter := Limiter1,
                queue := []
            };
        Else ->
            ?WARN(
                "Failed to purge deleted documents in ~s for reason ~p",
                [DbName, Else],
                meta(St)
            ),
            % Reset the queue. We'll catch these on the next run.
            St#{queue := []}
    catch
        Class:Reason ->
            ?WARN(
                "Failed to purge deleted documents in ~s for reason ~p:~p",
                [DbName, Class, Reason],
                meta(St)
            ),
            % Reset the queue. We'll catch these on the next run.
            St#{queue := []}
    end.

queue_size(Queue) when is_list(Queue) ->
    lists:sum([length(Revs) || {_Id, Revs} <- Queue]).

fdi_to_idrevs(#full_doc_info{} = FDI) ->
    Revs = [
        couch_doc:rev_to_str({Pos, RevId})
     || {#leaf{}, {Pos, [RevId | _]}} <- couch_key_tree:get_all_leafs(FDI#full_doc_info.rev_tree)
    ],
    {FDI#full_doc_info.id, Revs}.

init_config(ScanId) ->
    #{
        sid => ScanId,
        limiter => couch_scanner_rate_limiter:get(),
        queue => []
    }.

meta(#{sid := ScanId}) ->
    #{sid => ScanId}.

ttl(St, DbName) ->
    DefaultTTL = config:get(atom_to_list(?MODULE), "deleted_document_ttl"),
    DbTTL =
        case fabric:get_auto_purge_props(DbName) of
            {ok, AutoPurgeProps} ->
                case parse_deleted_document_ttl(AutoPurgeProps) of
                    TTL when is_integer(TTL) ->
                        TTL;
                    undefined ->
                        undefined;
                    Else ->
                        ?WARN(
                            "ignoring TTL in ~s as ttl was '~p'",
                            [DbName, Else],
                            meta(St)
                        ),
                        undefined
                end;
            {error, Reason} ->
                ?WARN(
                    "Failed to fetch ttl in ~s for reason ~p",
                    [DbName, Reason],
                    meta(St)
                ),
                undefined
        end,
    if
        DbTTL /= undefined -> DbTTL;
        DefaultTTL /= undefined -> parse_ttl(DefaultTTL);
        true -> undefined
    end.

parse_deleted_document_ttl(AutoPurgeProps) ->
    case couch_util:get_value(<<"deleted_document_ttl">>, AutoPurgeProps) of
        undefined ->
            undefined;
        Else ->
            parse_ttl(Else)
    end.

parse_ttl(Bin) when is_binary(Bin) ->
    parse_ttl(binary_to_list(Bin));
parse_ttl([$- | TTL]) ->
    -(parse_ttl(TTL));
parse_ttl(TTL) ->
    couch_scanner_util:parse_non_weekday_period(TTL).

min_batch_size() ->
    erlang:max(1, config:get_integer(atom_to_list(?MODULE), "min_batch_size", 250)).

max_batch_size() ->
    erlang:max(min_batch_size(), config:get_integer(atom_to_list(?MODULE), "max_batch_size", 500)).
