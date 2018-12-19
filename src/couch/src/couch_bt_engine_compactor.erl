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

-module(couch_bt_engine_compactor).


-export([
    start/4
]).


-include_lib("couch/include/couch_db.hrl").
-include("couch_bt_engine.hrl").


-record(comp_header, {
    db_header,
    meta_state
}).

-record(merge_st, {
    id_tree,
    seq_tree,
    curr,
    rem_seqs,
    infos
}).


start(#st{} = St, DbName, Options, Parent) ->
    erlang:put(io_priority, {db_compact, DbName}),
    #st{
        filepath = FilePath,
        header = Header
    } = St,
    couch_log:debug("Compaction process spawned for db \"~s\"", [DbName]),

    couch_db_engine:trigger_on_compact(DbName),

    {ok, NewSt, DName, DFd, MFd, Retry} =
            open_compaction_files(Header, FilePath, Options),
    erlang:monitor(process, MFd),

    % This is a bit worrisome. init_db/4 will monitor the data fd
    % but it doesn't know about the meta fd. For now I'll maintain
    % that the data fd is the old normal fd and meta fd is special
    % and hope everything works out for the best.
    unlink(DFd),

    NewSt1 = copy_purge_info(DbName, St, NewSt, Retry),
    NewSt2 = copy_compact(DbName, St, NewSt1, Retry),
    NewSt3 = sort_meta_data(NewSt2),
    NewSt4 = commit_compaction_data(NewSt3),
    NewSt5 = copy_meta_data(NewSt4),
    {ok, NewSt6} = couch_bt_engine:commit_data(NewSt5),
    ok = couch_bt_engine:decref(NewSt6),
    ok = couch_file:close(MFd),

    % Done
    gen_server:cast(Parent, {compact_done, couch_bt_engine, DName}).


open_compaction_files(SrcHdr, DbFilePath, Options) ->
    DataFile = DbFilePath ++ ".compact.data",
    MetaFile = DbFilePath ++ ".compact.meta",
    {ok, DataFd, DataHdr} = open_compaction_file(DataFile),
    {ok, MetaFd, MetaHdr} = open_compaction_file(MetaFile),
    DataHdrIsDbHdr = couch_bt_engine_header:is_header(DataHdr),
    case {DataHdr, MetaHdr} of
        {#comp_header{}=A, #comp_header{}=A} ->
            DbHeader = A#comp_header.db_header,
            St0 = couch_bt_engine:init_state(
                    DataFile, DataFd, DbHeader, Options),
            St1 = bind_emsort(St0, MetaFd, A#comp_header.meta_state),
            {ok, St1, DataFile, DataFd, MetaFd, St0#st.id_tree};
        _ when DataHdrIsDbHdr ->
            Header = couch_bt_engine_header:from(SrcHdr),
            ok = reset_compaction_file(MetaFd, Header),
            St0 = couch_bt_engine:init_state(
                    DataFile, DataFd, DataHdr, Options),
            St1 = bind_emsort(St0, MetaFd, nil),
            {ok, St1, DataFile, DataFd, MetaFd, St0#st.id_tree};
        _ ->
            Header = couch_bt_engine_header:from(SrcHdr),
            ok = reset_compaction_file(DataFd, Header),
            ok = reset_compaction_file(MetaFd, Header),
            St0 = couch_bt_engine:init_state(DataFile, DataFd, Header, Options),
            St1 = bind_emsort(St0, MetaFd, nil),
            {ok, St1, DataFile, DataFd, MetaFd, nil}
    end.


copy_purge_info(DbName, OldSt, NewSt, Retry) ->
    MinPurgeSeq = couch_util:with_db(DbName, fun(Db) ->
        couch_db:get_minimum_purge_seq(Db)
    end),
    OldPSTree = OldSt#st.purge_seq_tree,
    StartSeq = couch_bt_engine:get_purge_seq(NewSt) + 1,
    BufferSize = config:get_integer(
            "database_compaction", "doc_buffer_size", 524288),
    CheckpointAfter = config:get(
            "database_compaction", "checkpoint_after", BufferSize * 10),

    EnumFun = fun(Info, _Reds, {StAcc0, InfosAcc, InfosSize, CopiedSize}) ->
        NewInfosSize = InfosSize + ?term_size(Info),
        if NewInfosSize >= BufferSize ->
            StAcc1 = copy_purge_infos(
                    OldSt, StAcc0, [Info | InfosAcc], MinPurgeSeq, Retry),
            NewCopiedSize = CopiedSize + NewInfosSize,
            if NewCopiedSize >= CheckpointAfter ->
                StAcc2 = commit_compaction_data(StAcc1),
                {ok, {StAcc2, [], 0, 0}};
            true ->
                {ok, {StAcc1, [], 0, NewCopiedSize}}
            end;
        true ->
            NewInfosAcc = [Info | InfosAcc],
            {ok, {StAcc0, NewInfosAcc, NewInfosSize, CopiedSize}}
        end
    end,

    InitAcc = {NewSt, [], 0, 0},
    Opts = [{start_key, StartSeq}],
    {ok, _, FinalAcc} = couch_btree:fold(OldPSTree, EnumFun, InitAcc, Opts),
    {NewStAcc, Infos, _, _} = FinalAcc,
    copy_purge_infos(OldSt, NewStAcc, Infos, MinPurgeSeq, Retry).


copy_purge_infos(OldSt, NewSt0, Infos, MinPurgeSeq, Retry) ->
    #st{
        id_tree = OldIdTree
    } = OldSt,

    % Re-bind our id_tree to the backing btree
    NewIdTreeState = couch_bt_engine_header:id_tree_state(NewSt0#st.header),
    MetaFd = couch_emsort:get_fd(NewSt0#st.id_tree),
    MetaState = couch_emsort:get_state(NewSt0#st.id_tree),
    NewSt1 = bind_id_tree(NewSt0, NewSt0#st.fd, NewIdTreeState),

    #st{
        id_tree = NewIdTree0,
        seq_tree = NewSeqTree0,
        purge_tree = NewPurgeTree0,
        purge_seq_tree = NewPurgeSeqTree0
    } = NewSt1,

    % Copy over the purge infos
    InfosToAdd = lists:filter(fun({PSeq, _, _, _}) ->
        PSeq > MinPurgeSeq
    end, Infos),
    {ok, NewPurgeTree1} = couch_btree:add(NewPurgeTree0, InfosToAdd),
    {ok, NewPurgeSeqTree1} = couch_btree:add(NewPurgeSeqTree0, InfosToAdd),

    NewSt2 = NewSt1#st{
        purge_tree = NewPurgeTree1,
        purge_seq_tree = NewPurgeSeqTree1
    },

    % If we're peforming a retry compaction we have to check if
    % any of the referenced docs have been completely purged
    % from the database. Any doc that has been completely purged
    % must then be removed from our partially compacted database.
    NewSt3 = if Retry == nil -> NewSt2; true ->
        AllDocIds = [DocId || {_PurgeSeq, _UUID, DocId, _Revs} <- Infos],
        UniqDocIds = lists:usort(AllDocIds),
        OldIdResults = couch_btree:lookup(OldIdTree, UniqDocIds),
        OldZipped = lists:zip(UniqDocIds, OldIdResults),

        % The list of non-existant docs in the database being compacted
        MaybeRemDocIds = [DocId || {DocId, not_found} <- OldZipped],

        % Removing anything that exists in the partially compacted database
        NewIdResults = couch_btree:lookup(NewIdTree0, MaybeRemDocIds),
        ToRemove = [Doc || {ok, Doc} <- NewIdResults, Doc /= {ok, not_found}],

        {RemIds, RemSeqs} = lists:unzip(lists:map(fun(FDI) ->
            #full_doc_info{
                id = Id,
                update_seq = Seq
            } = FDI,
            {Id, Seq}
        end, ToRemove)),

        {ok, NewIdTree1} = couch_btree:add_remove(NewIdTree0, [], RemIds),
        {ok, NewSeqTree1} = couch_btree:add_remove(NewSeqTree0, [], RemSeqs),

        NewSt2#st{
            id_tree = NewIdTree1,
            seq_tree = NewSeqTree1
        }
    end,

    Header = couch_bt_engine:update_header(NewSt3, NewSt3#st.header),
    NewSt4 = NewSt3#st{
        header = Header
    },
    bind_emsort(NewSt4, MetaFd, MetaState).


copy_compact(DbName, St, NewSt0, Retry) ->
    Compression = couch_compress:get_compression_method(),
    NewSt = NewSt0#st{compression = Compression},
    NewUpdateSeq = couch_bt_engine:get_update_seq(NewSt0),
    TotalChanges = couch_bt_engine:count_changes_since(St, NewUpdateSeq),
    BufferSize = list_to_integer(
        config:get("database_compaction", "doc_buffer_size", "524288")),
    CheckpointAfter = couch_util:to_integer(
        config:get("database_compaction", "checkpoint_after",
            BufferSize * 10)),

    EnumBySeqFun =
    fun(DocInfo, _Offset,
            {AccNewSt, AccUncopied, AccUncopiedSize, AccCopiedSize}) ->

        Seq = case DocInfo of
            #full_doc_info{} -> DocInfo#full_doc_info.update_seq;
            #doc_info{} -> DocInfo#doc_info.high_seq
        end,

        AccUncopiedSize2 = AccUncopiedSize + ?term_size(DocInfo),
        if AccUncopiedSize2 >= BufferSize ->
            NewSt2 = copy_docs(
                St, AccNewSt, lists:reverse([DocInfo | AccUncopied]), Retry),
            AccCopiedSize2 = AccCopiedSize + AccUncopiedSize2,
            if AccCopiedSize2 >= CheckpointAfter ->
                {ok, NewSt3} = couch_bt_engine:set_update_seq(NewSt2, Seq),
                CommNewSt3 = commit_compaction_data(NewSt3),
                {ok, {CommNewSt3, [], 0, 0}};
            true ->
                {ok, NewSt3} = couch_bt_engine:set_update_seq(NewSt2, Seq),
                {ok, {NewSt3, [], 0, AccCopiedSize2}}
            end;
        true ->
            {ok, {AccNewSt, [DocInfo | AccUncopied], AccUncopiedSize2,
                AccCopiedSize}}
        end
    end,

    TaskProps0 = [
        {type, database_compaction},
        {database, DbName},
        {progress, 0},
        {changes_done, 0},
        {total_changes, TotalChanges}
    ],
    case (Retry =/= nil) and couch_task_status:is_task_added() of
    true ->
        couch_task_status:update([
            {retry, true},
            {progress, 0},
            {changes_done, 0},
            {total_changes, TotalChanges}
        ]);
    false ->
        couch_task_status:add_task(TaskProps0),
        couch_task_status:set_update_frequency(500)
    end,

    {ok, _, {NewSt2, Uncopied, _, _}} =
        couch_btree:foldl(St#st.seq_tree, EnumBySeqFun,
            {NewSt, [], 0, 0},
            [{start_key, NewUpdateSeq + 1}]),

    NewSt3 = copy_docs(St, NewSt2, lists:reverse(Uncopied), Retry),

    % Copy the security information over
    SecProps = couch_bt_engine:get_security(St),
    {ok, NewSt4} = couch_bt_engine:copy_security(NewSt3, SecProps),

    FinalUpdateSeq = couch_bt_engine:get_update_seq(St),
    {ok, NewSt5} = couch_bt_engine:set_update_seq(NewSt4, FinalUpdateSeq),
    commit_compaction_data(NewSt5).


copy_docs(St, #st{} = NewSt, MixedInfos, Retry) ->
    DocInfoIds = [Id || #doc_info{id=Id} <- MixedInfos],
    LookupResults = couch_btree:lookup(St#st.id_tree, DocInfoIds),
    % COUCHDB-968, make sure we prune duplicates during compaction
    NewInfos0 = lists:usort(fun(#full_doc_info{id=A}, #full_doc_info{id=B}) ->
        A =< B
    end, merge_lookups(MixedInfos, LookupResults)),

    NewInfos1 = lists:map(fun(Info) ->
        {NewRevTree, FinalAcc} = couch_key_tree:mapfold(fun
            ({RevPos, RevId}, #leaf{ptr=Sp}=Leaf, leaf, SizesAcc) ->
                {Body, AttInfos} = copy_doc_attachments(St, Sp, NewSt),
                % In the future, we should figure out how to do this for
                % upgrade purposes.
                ExternalSize = case is_binary(Body) of
                    true ->
                        couch_compress:uncompressed_size(Body);
                    false ->
                        ?term_size(Body)
                end,
                Doc0 = #doc{
                    id = Info#full_doc_info.id,
                    revs = {RevPos, [RevId]},
                    deleted = Leaf#leaf.deleted,
                    body = Body,
                    atts = AttInfos
                },
                Doc1 = couch_bt_engine:serialize_doc(NewSt, Doc0),
                {ok, Doc2, ActiveSize} =
                        couch_bt_engine:write_doc_body(NewSt, Doc1),
                AttSizes = [{element(3,A), element(4,A)} || A <- AttInfos],
                NewLeaf = Leaf#leaf{
                    ptr = Doc2#doc.body,
                    sizes = #size_info{
                        active = ActiveSize,
                        external = ExternalSize
                    },
                    atts = AttSizes
                },
                {NewLeaf, couch_db_updater:add_sizes(leaf, NewLeaf, SizesAcc)};
            (_Rev, _Leaf, branch, SizesAcc) ->
                {?REV_MISSING, SizesAcc}
        end, {0, 0, []}, Info#full_doc_info.rev_tree),
        {FinalAS, FinalES, FinalAtts} = FinalAcc,
        TotalAttSize = lists:foldl(fun({_, S}, A) -> S + A end, 0, FinalAtts),
        NewActiveSize = FinalAS + TotalAttSize,
        NewExternalSize = FinalES + TotalAttSize,
        Info#full_doc_info{
            rev_tree = NewRevTree,
            sizes = #size_info{
                active = NewActiveSize,
                external = NewExternalSize
            }
        }
    end, NewInfos0),

    Limit = couch_bt_engine:get_revs_limit(St),
    NewInfos = lists:map(fun(FDI) ->
        FDI#full_doc_info{
            rev_tree = couch_key_tree:stem(FDI#full_doc_info.rev_tree, Limit)
        }
    end, NewInfos1),

    RemoveSeqs =
    case Retry of
    nil ->
        [];
    OldDocIdTree ->
        % Compaction is being rerun to catch up to writes during the
        % first pass. This means we may have docs that already exist
        % in the seq_tree in the .data file. Here we lookup any old
        % update_seqs so that they can be removed.
        Ids = [Id || #full_doc_info{id=Id} <- NewInfos],
        Existing = couch_btree:lookup(OldDocIdTree, Ids),
        [Seq || {ok, #full_doc_info{update_seq=Seq}} <- Existing]
    end,

    {ok, SeqTree} = couch_btree:add_remove(
            NewSt#st.seq_tree, NewInfos, RemoveSeqs),

    FDIKVs = lists:map(fun(#full_doc_info{id=Id, update_seq=Seq}=FDI) ->
        {{Id, Seq}, FDI}
    end, NewInfos),
    {ok, IdEms} = couch_emsort:add(NewSt#st.id_tree, FDIKVs),
    update_compact_task(length(NewInfos)),
    NewSt#st{id_tree=IdEms, seq_tree=SeqTree}.


copy_doc_attachments(#st{} = SrcSt, SrcSp, DstSt) ->
    {ok, {BodyData, BinInfos0}} = couch_file:pread_term(SrcSt#st.fd, SrcSp),
    BinInfos = case BinInfos0 of
    _ when is_binary(BinInfos0) ->
        couch_compress:decompress(BinInfos0);
    _ when is_list(BinInfos0) ->
        % pre 1.2 file format
        BinInfos0
    end,
    % copy the bin values
    NewBinInfos = lists:map(
        fun({Name, Type, BinSp, AttLen, RevPos, ExpectedMd5}) ->
            % 010 UPGRADE CODE
            {ok, SrcStream} = couch_bt_engine:open_read_stream(SrcSt, BinSp),
            {ok, DstStream} = couch_bt_engine:open_write_stream(DstSt, []),
            ok = couch_stream:copy(SrcStream, DstStream),
            {NewStream, AttLen, AttLen, ActualMd5, _IdentityMd5} =
                couch_stream:close(DstStream),
            {ok, NewBinSp} = couch_stream:to_disk_term(NewStream),
            couch_util:check_md5(ExpectedMd5, ActualMd5),
            {Name, Type, NewBinSp, AttLen, AttLen, RevPos, ExpectedMd5, identity};
        ({Name, Type, BinSp, AttLen, DiskLen, RevPos, ExpectedMd5, Enc1}) ->
            {ok, SrcStream} = couch_bt_engine:open_read_stream(SrcSt, BinSp),
            {ok, DstStream} = couch_bt_engine:open_write_stream(DstSt, []),
            ok = couch_stream:copy(SrcStream, DstStream),
            {NewStream, AttLen, _, ActualMd5, _IdentityMd5} =
                couch_stream:close(DstStream),
            {ok, NewBinSp} = couch_stream:to_disk_term(NewStream),
            couch_util:check_md5(ExpectedMd5, ActualMd5),
            Enc = case Enc1 of
            true ->
                % 0110 UPGRADE CODE
                gzip;
            false ->
                % 0110 UPGRADE CODE
                identity;
            _ ->
                Enc1
            end,
            {Name, Type, NewBinSp, AttLen, DiskLen, RevPos, ExpectedMd5, Enc}
        end, BinInfos),
    {BodyData, NewBinInfos}.


sort_meta_data(St0) ->
    {ok, Ems} = couch_emsort:merge(St0#st.id_tree),
    St0#st{id_tree=Ems}.


copy_meta_data(#st{} = St) ->
    #st{
        fd = Fd,
        header = Header,
        id_tree = Src
    } = St,
    DstState = couch_bt_engine_header:id_tree_state(Header),
    {ok, IdTree0} = couch_btree:open(DstState, Fd, [
        {split, fun couch_bt_engine:id_tree_split/1},
        {join, fun couch_bt_engine:id_tree_join/2},
        {reduce, fun couch_bt_engine:id_tree_reduce/2}
    ]),
    {ok, Iter} = couch_emsort:iter(Src),
    Acc0 = #merge_st{
        id_tree=IdTree0,
        seq_tree=St#st.seq_tree,
        rem_seqs=[],
        infos=[]
    },
    Acc = merge_docids(Iter, Acc0),
    {ok, IdTree} = couch_btree:add(Acc#merge_st.id_tree, Acc#merge_st.infos),
    {ok, SeqTree} = couch_btree:add_remove(
        Acc#merge_st.seq_tree, [], Acc#merge_st.rem_seqs
    ),
    St#st{id_tree=IdTree, seq_tree=SeqTree}.


open_compaction_file(FilePath) ->
    case couch_file:open(FilePath, [nologifmissing]) of
        {ok, Fd} ->
            case couch_file:read_header(Fd) of
                {ok, Header} -> {ok, Fd, Header};
                no_valid_header -> {ok, Fd, nil}
            end;
        {error, enoent} ->
            {ok, Fd} = couch_file:open(FilePath, [create]),
            {ok, Fd, nil}
    end.


reset_compaction_file(Fd, Header) ->
    ok = couch_file:truncate(Fd, 0),
    ok = couch_file:write_header(Fd, Header).


commit_compaction_data(#st{}=St) ->
    % Compaction needs to write headers to both the data file
    % and the meta file so if we need to restart we can pick
    % back up from where we left off.
    commit_compaction_data(St, couch_emsort:get_fd(St#st.id_tree)),
    commit_compaction_data(St, St#st.fd).


commit_compaction_data(#st{header = OldHeader} = St0, Fd) ->
    DataState = couch_bt_engine_header:id_tree_state(OldHeader),
    MetaFd = couch_emsort:get_fd(St0#st.id_tree),
    MetaState = couch_emsort:get_state(St0#st.id_tree),
    St1 = bind_id_tree(St0, St0#st.fd, DataState),
    Header = couch_bt_engine:update_header(St1, St1#st.header),
    CompHeader = #comp_header{
        db_header = Header,
        meta_state = MetaState
    },
    ok = couch_file:sync(Fd),
    ok = couch_file:write_header(Fd, CompHeader),
    St2 = St1#st{
        header = Header
    },
    bind_emsort(St2, MetaFd, MetaState).


bind_emsort(St, Fd, nil) ->
    {ok, Ems} = couch_emsort:open(Fd),
    St#st{id_tree=Ems};
bind_emsort(St, Fd, State) ->
    {ok, Ems} = couch_emsort:open(Fd, [{root, State}]),
    St#st{id_tree=Ems}.


bind_id_tree(St, Fd, State) ->
    {ok, IdBtree} = couch_btree:open(State, Fd, [
        {split, fun couch_bt_engine:id_tree_split/1},
        {join, fun couch_bt_engine:id_tree_join/2},
        {reduce, fun couch_bt_engine:id_tree_reduce/2}
    ]),
    St#st{id_tree=IdBtree}.


merge_lookups(Infos, []) ->
    Infos;
merge_lookups([], _) ->
    [];
merge_lookups([#doc_info{}=DI | RestInfos], [{ok, FDI} | RestLookups]) ->
    % Assert we've matched our lookups
    if DI#doc_info.id == FDI#full_doc_info.id -> ok; true ->
        erlang:error({mismatched_doc_infos, DI#doc_info.id})
    end,
    [FDI | merge_lookups(RestInfos, RestLookups)];
merge_lookups([FDI | RestInfos], Lookups) ->
    [FDI | merge_lookups(RestInfos, Lookups)].


merge_docids(Iter, #merge_st{infos=Infos}=Acc) when length(Infos) > 1000 ->
    #merge_st{
        id_tree=IdTree0,
        seq_tree=SeqTree0,
        rem_seqs=RemSeqs
    } = Acc,
    {ok, IdTree1} = couch_btree:add(IdTree0, Infos),
    {ok, SeqTree1} = couch_btree:add_remove(SeqTree0, [], RemSeqs),
    Acc1 = Acc#merge_st{
        id_tree=IdTree1,
        seq_tree=SeqTree1,
        rem_seqs=[],
        infos=[]
    },
    merge_docids(Iter, Acc1);
merge_docids(Iter, #merge_st{curr=Curr}=Acc) ->
    case next_info(Iter, Curr, []) of
        {NextIter, NewCurr, FDI, Seqs} ->
            Acc1 = Acc#merge_st{
                infos = [FDI | Acc#merge_st.infos],
                rem_seqs = Seqs ++ Acc#merge_st.rem_seqs,
                curr = NewCurr
            },
            merge_docids(NextIter, Acc1);
        {finished, FDI, Seqs} ->
            Acc#merge_st{
                infos = [FDI | Acc#merge_st.infos],
                rem_seqs = Seqs ++ Acc#merge_st.rem_seqs,
                curr = undefined
            };
        empty ->
            Acc
    end.


next_info(Iter, undefined, []) ->
    case couch_emsort:next(Iter) of
        {ok, {{Id, Seq}, FDI}, NextIter} ->
            next_info(NextIter, {Id, Seq, FDI}, []);
        finished ->
            empty
    end;
next_info(Iter, {Id, Seq, FDI}, Seqs) ->
    case couch_emsort:next(Iter) of
        {ok, {{Id, NSeq}, NFDI}, NextIter} ->
            next_info(NextIter, {Id, NSeq, NFDI}, [Seq | Seqs]);
        {ok, {{NId, NSeq}, NFDI}, NextIter} ->
            {NextIter, {NId, NSeq, NFDI}, FDI, Seqs};
        finished ->
            {finished, FDI, Seqs}
    end.


update_compact_task(NumChanges) ->
    [Changes, Total] = couch_task_status:get([changes_done, total_changes]),
    Changes2 = Changes + NumChanges,
    Progress = case Total of
    0 ->
        0;
    _ ->
        (Changes2 * 100) div Total
    end,
    couch_task_status:update([{changes_done, Changes2}, {progress, Progress}]).

