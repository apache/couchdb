% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(dreyfus_index_updater).
-include_lib("couch/include/couch_db.hrl").
-include("dreyfus.hrl").

-export([update/2, load_docs/2]).

-import(couch_query_servers, [get_os_process/1, ret_os_process/1, proc_prompt/2]).

update(IndexPid, Index) ->
    #index{
        current_seq = CurSeq,
        dbname = DbName,
        ddoc_id = DDocId,
        name = IndexName
    } = Index,
    erlang:put(io_priority, {search, DbName, IndexName}),
    {ok, Db} = couch_db:open_int(DbName, []),
    try
        CheckpointPSeq = get_local_doc_purge_seq(Db, Index),
        {ok, ClouseauPSeq} = clouseau_rpc:get_purge_seq(IndexPid),
        IdxPurgeSeq = get_index_purge_seq(Db, CheckpointPSeq, ClouseauPSeq, DDocId, IndexName),
        DbPurgeSeq = couch_db:get_purge_seq(Db),
        TotalPurgeChanges = DbPurgeSeq - IdxPurgeSeq,
        TotalUpdateChanges = couch_db:count_changes_since(Db, CurSeq),
        TotalChanges = TotalUpdateChanges + TotalPurgeChanges,

        couch_task_status:add_task([
            {type, search_indexer},
            {database, DbName},
            {design_document, DDocId},
            {index, IndexName},
            {progress, 0},
            {changes_done, 0},
            {total_changes, TotalChanges}
        ]),

        %% update status every half second
        couch_task_status:set_update_frequency(500),

        %ExcludeIdRevs is [{Id1, Rev1}, {Id2, Rev2}, ...]
        %The Rev is the final Rev, not purged Rev.
        {ok, ExcludeIdRevs} = purge_index(Db, IndexPid, Index, IdxPurgeSeq, ClouseauPSeq),
        %% compute on all docs modified since we last computed.

        NewCurSeq = couch_db:get_update_seq(Db),
        Proc = get_os_process(Index#index.def_lang),
        try
            true = proc_prompt(Proc, [<<"add_fun">>, Index#index.def]),
            EnumFun = fun ?MODULE:load_docs/2,
            [Changes] = couch_task_status:get([changes_done]),
            Acc0 = {Changes, IndexPid, Db, Proc, TotalChanges, erlang:timestamp(), ExcludeIdRevs},
            {ok, _} = couch_db:fold_changes(Db, CurSeq, EnumFun, Acc0, []),
            ok = clouseau_rpc:commit(IndexPid, NewCurSeq)
        after
            ret_os_process(Proc)
        end,
        exit({updated, NewCurSeq})
    after
        couch_db:close(Db)
    end.

load_docs(FDI, {I, IndexPid, Db, Proc, Total, LastCommitTime, ExcludeIdRevs} = Acc) ->
    couch_task_status:update([{changes_done, I}, {progress, (I * 100) div Total}]),
    DI = couch_doc:to_doc_info(FDI),
    #doc_info{id = Id, high_seq = Seq, revs = [#rev_info{rev = Rev} | _]} = DI,
    %check if it is processed in purge_index to avoid update the index again.
    case lists:member({Id, Rev}, ExcludeIdRevs) of
        true -> ok;
        false -> update_or_delete_index(IndexPid, Db, DI, Proc)
    end,
    %% Force a commit every minute
    case timer:now_diff(Now = erlang:timestamp(), LastCommitTime) >= 60000000 of
        true ->
            ok = clouseau_rpc:commit(IndexPid, Seq),
            {ok, {I + 1, IndexPid, Db, Proc, Total, Now, ExcludeIdRevs}};
        false ->
            {ok, setelement(1, Acc, I + 1)}
    end.

purge_index(Db, IndexPid, Index, IdxPurgeSeq, OldClouseauPSeq) ->
    Proc = get_os_process(Index#index.def_lang),
    try
        true = proc_prompt(Proc, [<<"add_fun">>, Index#index.def]),
        FoldFun = fun({_PurgeSeq, _UUID, Id, _Revs}, Acc) ->
            case couch_db:get_full_doc_info(Db, Id) of
                not_found ->
                    ok = clouseau_rpc:delete(IndexPid, Id),
                    update_task(1),
                    {ok, Acc};
                FDI ->
                    DI = couch_doc:to_doc_info(FDI),
                    #doc_info{id = Id, revs = [#rev_info{rev = Rev} | _]} = DI,
                    case lists:member({Id, Rev}, Acc) of
                        true ->
                            {ok, Acc};
                        false ->
                            update_or_delete_index(IndexPid, Db, DI, Proc),
                            update_task(1),
                            {ok, [{Id, Rev} | Acc]}
                    end
            end
        end,
        {ok, ExcludeList} = couch_db:fold_purge_infos(Db, IdxPurgeSeq, FoldFun, []),
        NewPurgeSeq = couch_db:get_purge_seq(Db),
        case NewPurgeSeq > OldClouseauPSeq of
            true ->
                ok = clouseau_rpc:set_purge_seq(IndexPid, NewPurgeSeq);
            false ->
                % Save an rpc call if aren't actualy advancing the purge sequence
                ok
        end,
        update_local_doc(Db, Index, NewPurgeSeq),
        {ok, ExcludeList}
    after
        ret_os_process(Proc)
    end.

update_or_delete_index(IndexPid, Db, DI, Proc) ->
    #doc_info{id = Id, revs = [#rev_info{deleted = Del} | _]} = DI,
    case Del of
        true ->
            ok = clouseau_rpc:delete(IndexPid, Id);
        false ->
            case maybe_skip_doc(Db, Id) of
                true ->
                    ok;
                false ->
                    {ok, Doc} = couch_db:open_doc(Db, DI, []),
                    Json = couch_doc:to_json_obj(Doc, []),
                    [Fields | _] = proc_prompt(Proc, [<<"index_doc">>, Json]),
                    Fields1 = [list_to_tuple(Field) || Field <- Fields],
                    Fields2 = maybe_add_partition(Db, Id, Fields1),
                    case Fields2 of
                        [] -> ok = clouseau_rpc:delete(IndexPid, Id);
                        _ -> ok = clouseau_rpc:update(IndexPid, Id, Fields2)
                    end
            end
    end.

update_local_doc(Db, Index, PurgeSeq) ->
    DocId = dreyfus_util:get_local_purge_doc_id(Index#index.sig),
    DocContent = dreyfus_util:get_local_purge_doc_body(Db, DocId, PurgeSeq, Index),
    couch_db:update_doc(Db, DocContent, []).

get_local_doc_purge_seq(Db, Index) ->
    DocId = dreyfus_util:get_local_purge_doc_id(Index#index.sig),
    % We're implicitly asserting this purge checkpoint doc should exist. This is
    % created either on open or during compaction in on_compact handler
    {ok, #doc{body = {[_ | _] = Props}}} = couch_db:open_doc(Db, DocId),
    couch_util:get_value(<<"purge_seq">>, Props).

get_index_purge_seq(Db, CheckpointPSeq, ClouseauPSeq, DDocId, IndexName) when
    is_integer(CheckpointPSeq), is_integer(ClouseauPSeq), CheckpointPSeq >= 0, ClouseauPSeq >= 0
->
    if
        CheckpointPSeq == ClouseauPSeq ->
            % The default state is that they should match.
            CheckpointPSeq;
        CheckpointPSeq > ClouseauPSeq ->
            % Somehow index fell behind. We should reset the index but don't really
            % have a facility for it, so log an error instead. We still can only start folding
            % purges from the checkpoint sequence onwards nd not below.
            DbName = couch_db:name(Db),
            Msg = "~p : index pseq:~p is behind the checkpoint pseq:~p db:~p ddoc:~p index:~p",
            couch_log:error(Msg, [?MODULE, ClouseauPSeq, CheckpointPSeq, DbName, DDocId, IndexName]),
            CheckpointPSeq;
        CheckpointPSeq < ClouseauPSeq ->
            % Somehow the checkpoint fell behind. Perhaps someone manually
            % manipulated checkpoint docs or the index the system crashed right after
            % the set_purge_seq was called but before the checkpoint doc was
            % written. Choose to reprocess the changes from the checkpointed
            % sequence, it may add extra work but should not lead to an
            % inconsistent index.
            CheckpointPSeq
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

maybe_skip_doc(Db, <<"_design/", _/binary>>) ->
    couch_db:is_partitioned(Db);
maybe_skip_doc(_Db, _Id) ->
    false.

maybe_add_partition(_Db, _Id, []) ->
    [];
maybe_add_partition(Db, Id, Fields) ->
    case couch_db:is_partitioned(Db) of
        true ->
            Partition = couch_partition:from_docid(Id),
            [{<<"_partition">>, Partition, {[]}} | Fields];
        false ->
            Fields
    end.
