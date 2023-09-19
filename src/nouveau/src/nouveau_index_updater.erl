%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(nouveau_index_updater).
-include_lib("couch/include/couch_db.hrl").
-include("nouveau.hrl").

%% public api
-export([outdated/1]).

%% callbacks
-export([update/1]).

-import(couch_query_servers, [get_os_process/1, ret_os_process/1, proc_prompt/2]).
-import(nouveau_util, [index_path/1]).

-record(acc, {
    db,
    index,
    proc,
    changes_done,
    total_changes,
    exclude_idrevs,
    reqids,
    conn_pid,
    update_seq,
    max_pipeline_size
}).

-record(purge_acc, {
    exclude_list = [],
    index_update_seq,
    index_purge_seq
}).

outdated(#index{} = Index) ->
    case open_or_create_index(Index) of
        {ok, #{} = Info} ->
            #{<<"update_seq">> := IndexUpdateSeq, <<"purge_seq">> := IndexPurgeSeq} = Info,
            {DbUpdateSeq, DbPurgeSeq} = get_db_info(Index),
            DbUpdateSeq > IndexUpdateSeq orelse DbPurgeSeq > IndexPurgeSeq;
        {error, Reason} ->
            {error, Reason}
    end.

update(#index{} = Index) ->
    {ok, Db} = couch_db:open_int(Index#index.dbname, []),
    try
        case open_or_create_index(Db, Index) of
            {error, Reason} ->
                exit({error, Reason});
            {ok, #{} = Info} ->
                #{<<"update_seq">> := IndexUpdateSeq, <<"purge_seq">> := IndexPurgeSeq} = Info,
                ChangesSince = couch_db:count_changes_since(Db, IndexUpdateSeq),
                PurgesSince = couch_db:get_purge_seq(Db) - IndexPurgeSeq,
                TotalChanges = ChangesSince + PurgesSince,
                couch_task_status:add_task([
                    {type, search_indexer},
                    {database, Index#index.dbname},
                    {design_document, Index#index.ddoc_id},
                    {index, Index#index.name},
                    {progress, 0},
                    {changes_done, 0},
                    {total_changes, TotalChanges}
                ]),

                %% update status every half second
                couch_task_status:set_update_frequency(500),

                {ok, ConnPid} = ibrowse:spawn_link_worker_process(nouveau_util:nouveau_url()),
                PurgeAcc0 = #purge_acc{
                    index_update_seq = IndexUpdateSeq,
                    index_purge_seq = IndexPurgeSeq
                },
                {ok, PurgeAcc1} = purge_index(ConnPid, Db, Index, PurgeAcc0),

                NewCurSeq = couch_db:get_update_seq(Db),
                Proc = get_os_process(Index#index.def_lang),
                try
                    true = proc_prompt(Proc, [<<"add_fun">>, Index#index.def, <<"nouveau">>]),

                    Acc0 = #acc{
                        db = Db,
                        index = Index,
                        proc = Proc,
                        changes_done = 0,
                        total_changes = TotalChanges,
                        exclude_idrevs = PurgeAcc1#purge_acc.exclude_list,
                        reqids = queue:new(),
                        conn_pid = ConnPid,
                        update_seq = PurgeAcc1#purge_acc.index_update_seq,
                        max_pipeline_size = nouveau_util:max_pipeline_size()
                    },
                    {ok, Acc1} = couch_db:fold_changes(
                        Db, Acc0#acc.update_seq, fun load_docs/2, Acc0, []
                    ),
                    nouveau_api:drain_async_responses(Acc1#acc.reqids, 0),
                    exit(nouveau_api:set_update_seq(ConnPid, Index, Acc1#acc.update_seq, NewCurSeq))
                after
                    ibrowse:stop_worker_process(ConnPid),
                    ret_os_process(Proc)
                end
        end
    after
        couch_db:close(Db)
    end.

load_docs(#full_doc_info{id = <<"_design/", _/binary>>}, #acc{} = Acc) ->
    {ok, Acc};
load_docs(FDI, #acc{} = Acc0) ->
    %% block for responses so we stay under the max pipeline size
    ReqIds1 = nouveau_api:drain_async_responses(Acc0#acc.reqids, Acc0#acc.max_pipeline_size),
    Acc1 = Acc0#acc{reqids = ReqIds1},

    couch_task_status:update([
        {changes_done, Acc1#acc.changes_done},
        {progress, (Acc1#acc.changes_done * 100) div Acc1#acc.total_changes}
    ]),
    DI = couch_doc:to_doc_info(FDI),
    #doc_info{id = Id, revs = [#rev_info{rev = Rev} | _]} = DI,

    Acc2 =
        case lists:member({Id, Rev}, Acc1#acc.exclude_idrevs) of
            true ->
                Acc1;
            false ->
                case
                    update_or_delete_index(
                        Acc1#acc.conn_pid,
                        Acc1#acc.db,
                        Acc1#acc.index,
                        Acc1#acc.update_seq,
                        DI,
                        Acc1#acc.proc
                    )
                of
                    {ibrowse_req_id, ReqId} ->
                        Acc1#acc{
                            update_seq = DI#doc_info.high_seq,
                            reqids = queue:in(ReqId, Acc1#acc.reqids)
                        };
                    {error, Reason} ->
                        exit({error, Reason})
                end
        end,
    {ok, Acc2#acc{changes_done = Acc2#acc.changes_done + 1}}.

update_or_delete_index(ConnPid, Db, #index{} = Index, MatchSeq, #doc_info{} = DI, Proc) ->
    #doc_info{id = Id, high_seq = Seq, revs = [#rev_info{deleted = Del} | _]} = DI,
    case Del of
        true ->
            nouveau_api:delete_doc_async(ConnPid, Index, Id, MatchSeq, Seq);
        false ->
            {ok, Doc} = couch_db:open_doc(Db, DI, []),
            Json = couch_doc:to_json_obj(Doc, []),
            [Fields | _] = proc_prompt(Proc, [<<"nouveau_index_doc">>, Json]),
            Partition =
                case couch_db:is_partitioned(Db) of
                    true ->
                        couch_partition:from_docid(Id);
                    false ->
                        null
                end,
            case Fields of
                [] ->
                    nouveau_api:delete_doc_async(ConnPid, Index, Id, MatchSeq, Seq);
                _ ->
                    nouveau_api:update_doc_async(
                        ConnPid, Index, Id, MatchSeq, Seq, Partition, Fields
                    )
            end
    end.

open_or_create_index(#index{} = Index) ->
    case nouveau_api:index_info(Index) of
        {ok, #{} = Info} ->
            {ok, Info};
        {error, {not_found, _}} ->
            case nouveau_api:create_index(Index, index_definition(Index)) of
                ok ->
                    nouveau_api:index_info(Index);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

open_or_create_index(Db, #index{} = Index) ->
    case open_or_create_index(Index) of
        {ok, #{} = Info} ->
            nouveau_util:maybe_create_local_purge_doc(Db, Index),
            {ok, Info};
        Else ->
            Else
    end.

get_db_info(#index{} = Index) ->
    {ok, Db} = couch_db:open_int(Index#index.dbname, []),
    try
        UpdateSeq = couch_db:get_update_seq(Db),
        PurgeSeq = couch_db:get_purge_seq(Db),
        {UpdateSeq, PurgeSeq}
    after
        couch_db:close(Db)
    end.

index_definition(#index{} = Index) ->
    #{
        <<"default_analyzer">> => Index#index.default_analyzer,
        <<"field_analyzers">> => Index#index.field_analyzers
    }.

purge_index(ConnPid, Db, Index, #purge_acc{} = PurgeAcc0) ->
    Proc = get_os_process(Index#index.def_lang),
    try
        true = proc_prompt(Proc, [<<"add_fun">>, Index#index.def, <<"nouveau">>]),
        FoldFun = fun({PurgeSeq, _UUID, Id, _Revs}, #purge_acc{} = PurgeAcc1) ->
            PurgeAcc2 =
                case couch_db:get_full_doc_info(Db, Id) of
                    not_found ->
                        ok = nouveau_api:purge_doc(
                            ConnPid, Index, Id, PurgeAcc1#purge_acc.index_purge_seq, PurgeSeq
                        ),
                        PurgeAcc1#purge_acc{index_purge_seq = PurgeSeq};
                    FDI ->
                        DI = couch_doc:to_doc_info(FDI),
                        #doc_info{id = Id, high_seq = Seq, revs = [#rev_info{rev = Rev} | _]} = DI,
                        case lists:member({Id, Rev}, PurgeAcc1#purge_acc.exclude_list) of
                            true ->
                                PurgeAcc1;
                            false ->
                                update_or_delete_index(
                                    ConnPid,
                                    Db,
                                    Index,
                                    PurgeAcc1#purge_acc.index_update_seq,
                                    DI,
                                    Proc
                                ),
                                PurgeAcc1#purge_acc{
                                    exclude_list = [{Id, Rev} | PurgeAcc1#purge_acc.exclude_list],
                                    index_update_seq = Seq
                                }
                        end
                end,
            update_task(1),
            {ok, PurgeAcc2}
        end,

        {ok, #purge_acc{} = PurgeAcc3} = couch_db:fold_purge_infos(
            Db, PurgeAcc0#purge_acc.index_purge_seq, FoldFun, PurgeAcc0, []
        ),
        DbPurgeSeq = couch_db:get_purge_seq(Db),
        ok = nouveau_api:set_purge_seq(
            ConnPid, Index, PurgeAcc3#purge_acc.index_purge_seq, DbPurgeSeq
        ),
        update_local_doc(Db, Index, DbPurgeSeq),
        {ok, PurgeAcc3}
    after
        ret_os_process(Proc)
    end.

update_task(NumChanges) ->
    [Changes, Total] = couch_task_status:get([changes_done, total_changes]),
    Changes2 = Changes + NumChanges,
    Progress =
        case Total of
            0 ->
                0;
            _ ->
                (Changes2 * 100) div Total
        end,
    couch_task_status:update([{progress, Progress}, {changes_done, Changes2}]).

update_local_doc(Db, #index{} = Index, PurgeSeq) ->
    DocId = nouveau_util:get_local_purge_doc_id(Index#index.sig),
    DocContent = nouveau_util:get_local_purge_doc_body(DocId, PurgeSeq, Index),
    couch_db:update_doc(Db, DocContent, []).
