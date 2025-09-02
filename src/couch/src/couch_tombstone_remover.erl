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

-module(couch_tombstone_remover).
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
    Timeout = fabric_util:request_timeout(),
    GetFun = fun() -> fabric:open_doc(DbName, <<"_local/_tombstone_ttl">>, [?ADMIN_CTX]) end,
    try fabric_util:isolate(GetFun, Timeout) of
        {ok, #doc{} = Doc} ->
            {Props} = couch_doc:to_json_obj(Doc, []),
            case couch_util:get_value(<<"ttl">>, Props) of
                TTL when is_integer(TTL) ->
                    {ok, St#{ttl => TTL}};
                Else ->
                    ?WARN(
                        "Failed to purge tombstones in ~s as ttl was '~p', not integer",
                        [DbName, Else],
                        meta(St)
                    ),
                    {skip, St}
            end;
        {not_found, _Reason} ->
            {skip, St};
        {error, Reason} ->
            ?WARN("Failed to purge tombstones in ~s for reason ~p", [DbName, Reason], meta(St)),
            {skip, St}
    catch
        Class:Reason ->
            ?WARN(
                "Failed to purge tombstones in ~s for reason ~p:~p",
                [DbName, Class, Reason],
                meta(St)
            ),
            {skip, St}
    end.

db_opened(#{} = St, Db) ->
    #{ttl := TTL} = St,
    EndSeq = couch_time_seq:since(couch_db:get_time_seq(Db), couch_time_seq:timestamp() - TTL),
    ChangeOpts =
        if
            EndSeq == now -> [];
            true -> [{end_key, EndSeq}]
        end,
    ?INFO("scanning for tombstones in ~s up to ~p", [couch_db:name(Db), EndSeq], meta(St)),
    {0, ChangeOpts, St#{count => 0, end_seq => EndSeq}}.

db_closing(#{} = St, Db) ->
    #{count := Count} = St,
    ?INFO("purged ~B tombstones from ~s", [Count, couch_db:name(Db)], meta(St)),
    {ok, St}.

doc_fdi(#{} = St, #full_doc_info{deleted = true} = FDI, Db) ->
    #{end_seq := EndSeq} = St,
    if
        FDI#full_doc_info.update_seq =< EndSeq ->
            {ok, purge(St, FDI, Db)};
        true ->
            {ok, St}
    end;
doc_fdi(#{} = St, #full_doc_info{}, _Db) ->
    {ok, St}.

purge(#{} = St, #full_doc_info{} = FDI, Db) ->
    {Id, Revs} = fdi_to_idrevs(FDI),
    MaxBatchSize = config:get_integer("couch_tombstone_remover", "max_batch_size", 100),
    purge(St, Id, Revs, MaxBatchSize, Db).

purge(#{} = St, Id, Revs, MaxBatchSize, Db) when length(Revs) =< MaxBatchSize ->
    DbName = mem3:dbname(couch_db:name(Db)),
    PurgeFun = fun() -> fabric:purge_docs(DbName, [{Id, Revs}], [?ADMIN_CTX]) end,
    Timeout = fabric_util:request_timeout(),
    try fabric_util:isolate(PurgeFun, Timeout) of
        {Health, Results} when Health == ok; Health == accepted ->
            #{count := Count, limiter := Limiter0} = St,
            {Wait, Limiter1} = couch_scanner_rate_limiter:update(
                Limiter0, doc_write, length(Results)
            ),
            timer:sleep(Wait),
            St#{count => Count + length(Results), limiter => Limiter1};
        Else ->
            ?WARN(
                "Failed to purge tombstones in ~s/~s for reason ~p",
                [DbName, Id, Else],
                meta(St)
            ),
            St
    catch
        Class:Reason ->
            ?WARN(
                "Failed to purge tombstones in ~s/~s for reason ~p:~p",
                [DbName, Id, Class, Reason],
                meta(St)
            ),
            St
    end;
purge(#{} = St0, Id, Revs, MaxBatchSize, Db) ->
    {RevBatch, RevRest} = lists:split(MaxBatchSize, Revs),
    St1 = purge(St0, Id, RevBatch, MaxBatchSize, Db),
    purge(St1, Id, RevRest, MaxBatchSize, Db).

fdi_to_idrevs(#full_doc_info{} = FDI) ->
    Revs = [
        couch_doc:rev_to_str({Pos, RevId})
     || {#leaf{}, {Pos, [RevId | _]}} <- couch_key_tree:get_all_leafs(FDI#full_doc_info.rev_tree)
    ],
    {FDI#full_doc_info.id, Revs}.

init_config(ScanId) ->
    #{sid => ScanId, limiter => couch_scanner_rate_limiter:get()}.

meta(#{sid := ScanId}) ->
    #{sid => ScanId}.
