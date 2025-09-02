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
    case should_run() of
        true ->
            ?INFO("Starting.", [], St),
            {ok, St};
        false ->
            ?INFO("Not starting.", [], St),
            skip
    end.

resume(ScanId, #{}) ->
    St = init_config(ScanId),
    case should_run() of
        true ->
            ?INFO("Resuming.", [], St),
            {ok, St};
        false ->
            ?INFO("Not resuming.", [], St),
            skip
    end.

complete(St) ->
    ?INFO("Completed", [], St),
    {ok, #{}}.

checkpoint(_St) ->
    {ok, #{}}.

db(St, DbName) ->
    case config:get_integer("tombstone_remover_ttl", ?b2l(DbName), 0) of
        0 ->
            {skip, St};
        TTL when is_integer(TTL), TTL > 0 ->
            {ok, St#{ttl => TTL}}
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
    IDRevs = fdi_to_idrevs(FDI),
    MaxBatchSize = config:get_integer("couch_tombstone_remover", "max_batch_size", 100),
    purge(St, IDRevs, MaxBatchSize, Db).

purge(#{} = St, {Id, Revs}, MaxBatchSize, Db) when length(Revs) =< MaxBatchSize ->
    DbName = mem3:dbname(couch_db:name(Db)),
    {Pid, Ref} = spawn_monitor(fun() ->
        exit(fabric:purge_docs(DbName, [{Id, Revs}], [?ADMIN_CTX]))
    end),
    receive
        {'DOWN', Ref, process, Pid, {_Health, _Results}} ->
            %% TODO check Results for success/failure
            #{count := Count} = St,
            St#{count => Count + length(Revs)}
    end;
purge(#{} = St0, {Id, Revs}, MaxBatchSize, Db) ->
    {RevBatch, RevRest} = lists:split(MaxBatchSize, Revs),
    St1 = purge(St0, {Id, RevBatch}, MaxBatchSize, Db),
    purge(St1, {Id, RevRest}, MaxBatchSize, Db).

fdi_to_idrevs(#full_doc_info{} = FDI) ->
    Revs = [
        couch_doc:rev_to_str({Pos, RevId})
     || {#leaf{}, {Pos, [RevId | _]}} <- couch_key_tree:get_all_leafs(FDI#full_doc_info.rev_tree)
    ],
    {FDI#full_doc_info.id, Revs}.

init_config(ScanId) ->
    #{sid => ScanId}.

should_run() ->
    true.

meta(#{sid := ScanId}) ->
    #{sid => ScanId}.
