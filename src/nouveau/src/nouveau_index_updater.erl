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

                {ok, ExcludeIdRevs} = purge_index(Db, Index, IndexPurgeSeq),

                Proc = get_os_process(Index#index.def_lang),
                try
                    true = proc_prompt(Proc, [<<"add_fun">>, Index#index.def, <<"nouveau">>]),
                    Acc0 = {Db, Index, Proc, 0, TotalChanges, ExcludeIdRevs},
                    {ok, _} = couch_db:fold_changes(Db, IndexUpdateSeq, fun load_docs/2, Acc0, [])
                after
                    ret_os_process(Proc)
                end
        end
    after
        couch_db:close(Db)
    end.

load_docs(#full_doc_info{id = <<"_design/", _/binary>>}, Acc) ->
    {ok, Acc};
load_docs(FDI, {Db, Index, Proc, ChangesDone, TotalChanges, ExcludeIdRevs}) ->
    couch_task_status:update([
        {changes_done, ChangesDone}, {progress, (ChangesDone * 100) div TotalChanges}
    ]),
    DI = couch_doc:to_doc_info(FDI),
    #doc_info{id = Id, revs = [#rev_info{rev = Rev} | _]} = DI,
    case lists:member({Id, Rev}, ExcludeIdRevs) of
        true -> ok;
        false -> update_or_delete_index(Db, Index, DI, Proc)
    end,
    {ok, {Db, Index, Proc, ChangesDone + 1, TotalChanges, ExcludeIdRevs}}.

update_or_delete_index(Db, #index{} = Index, #doc_info{} = DI, Proc) ->
    #doc_info{id = Id, high_seq = Seq, revs = [#rev_info{deleted = Del} | _]} = DI,
    case Del of
        true ->
            ok = nouveau_api:delete_doc(Index, Id, Seq);
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
                    ok = nouveau_api:delete_doc(Index, Id, Seq);
                _ ->
                    case nouveau_api:update_doc(Index, Id, Seq, Partition, Fields) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            exit({error, Reason})
                    end
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

purge_index(Db, Index, IndexPurgeSeq) ->
    Proc = get_os_process(Index#index.def_lang),
    try
        true = proc_prompt(Proc, [<<"add_fun">>, Index#index.def, <<"nouveau">>]),
        FoldFun = fun({PurgeSeq, _UUID, Id, _Revs}, {Acc, _}) ->
            Acc0 =
                case couch_db:get_full_doc_info(Db, Id) of
                    not_found ->
                        ok = nouveau_api:purge_doc(Index, Id, PurgeSeq),
                        Acc;
                    FDI ->
                        DI = couch_doc:to_doc_info(FDI),
                        #doc_info{id = Id, revs = [#rev_info{rev = Rev} | _]} = DI,
                        case lists:member({Id, Rev}, Acc) of
                            true ->
                                Acc;
                            false ->
                                update_or_delete_index(Db, Index, DI, Proc),
                                [{Id, Rev} | Acc]
                        end
                end,
            update_task(1),
            {ok, {Acc0, PurgeSeq}}
        end,

        {ok, {ExcludeList, NewPurgeSeq}} = couch_db:fold_purge_infos(
            Db, IndexPurgeSeq, FoldFun, {[], 0}, []
        ),
        update_local_doc(Db, Index, NewPurgeSeq),
        {ok, ExcludeList}
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
