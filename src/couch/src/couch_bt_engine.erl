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

-module(couch_bt_engine).
-behavior(couch_db_engine).

-export([
    exists/1,

    delete/3,
    delete_compaction_files/3,

    is_compacting/1,

    init/2,
    terminate/2,
    handle_db_updater_call/2,
    handle_db_updater_info/2,

    incref/1,
    decref/1,
    monitored_by/1,

    last_activity/1,

    get_compacted_seq/1,
    get_del_doc_count/1,
    get_disk_version/1,
    get_doc_count/1,
    get_epochs/1,
    get_purge_seq/1,
    get_oldest_purge_seq/1,
    get_purge_infos_limit/1,
    get_revs_limit/1,
    get_security/1,
    get_props/1,
    get_size_info/1,
    get_partition_info/2,
    get_update_seq/1,
    get_uuid/1,

    set_revs_limit/2,
    set_purge_infos_limit/2,
    set_security/2,
    set_props/2,

    set_update_seq/2,

    open_docs/2,
    open_local_docs/2,
    read_doc_body/2,
    load_purge_infos/2,

    serialize_doc/2,
    write_doc_body/2,
    write_doc_infos/3,
    purge_docs/3,
    copy_purge_infos/2,

    raft_lookup/2,
    raft_insert/2,
    raft_discard/2,
    raft_last/1,

    commit_data/1,

    open_write_stream/2,
    open_read_stream/2,
    is_active_stream/2,

    fold_docs/4,
    fold_local_docs/4,
    fold_changes/5,
    fold_purge_infos/5,
    count_changes_since/2,

    start_compaction/4,
    finish_compaction/4
]).

-export([
    init_state/4
]).

-export([
    id_tree_split/1,
    id_tree_join/2,
    id_tree_reduce/2,

    seq_tree_split/1,
    seq_tree_join/2,
    seq_tree_reduce/2,

    local_tree_split/1,
    local_tree_join/2,

    purge_tree_split/1,
    purge_tree_join/2,
    purge_tree_reduce/2,
    purge_seq_tree_split/1,
    purge_seq_tree_join/2,

    raft_tree_split/1,
    raft_tree_join/2,
    raft_tree_reduce/2
]).

% Used by the compactor
-export([
    update_header/2,
    copy_security/2,
    copy_props/2
]).

-include_lib("kernel/include/file.hrl").
-include_lib("couch/include/couch_db.hrl").
-include("couch_bt_engine.hrl").

exists(FilePath) ->
    case is_file(FilePath) of
        true ->
            true;
        false ->
            is_file(FilePath ++ ".compact")
    end.

delete(RootDir, FilePath, Async) ->
    %% Delete any leftover compaction files. If we don't do this a
    %% subsequent request for this DB will try to open them to use
    %% as a recovery.
    delete_compaction_files(RootDir, FilePath, [{context, compaction}]),

    % Delete the actual database file
    couch_file:delete(RootDir, FilePath, Async).

delete_compaction_files(RootDir, FilePath, DelOpts) ->
    lists:foreach(
        fun(Ext) ->
            couch_file:delete(RootDir, FilePath ++ Ext, DelOpts)
        end,
        [".compact", ".compact.data", ".compact.meta"]
    ).

is_compacting(DbName) ->
    lists:any(
        fun(Ext) ->
            filelib:is_regular(?b2l(DbName) ++ Ext)
        end,
        [".compact", ".compact.data", ".compact.meta"]
    ).

init(FilePath, Options) ->
    {ok, Fd} = open_db_file(FilePath, Options),
    Header =
        case lists:member(create, Options) of
            true ->
                delete_compaction_files(FilePath),
                Header0 = couch_bt_engine_header:new(),
                Header1 = init_set_props(Fd, Header0, Options),
                ok = couch_file:write_header(Fd, Header1),
                Header1;
            false ->
                case couch_file:read_header(Fd) of
                    {ok, Header0} ->
                        Header0;
                    no_valid_header ->
                        delete_compaction_files(FilePath),
                        Header0 = couch_bt_engine_header:new(),
                        ok = couch_file:write_header(Fd, Header0),
                        Header0
                end
        end,
    {ok, init_state(FilePath, Fd, Header, Options)}.

terminate(_Reason, St) ->
    % If the reason we died is because our fd disappeared
    % then we don't need to try closing it again.
    Ref = St#st.fd_monitor,
    if
        Ref == closed ->
            ok;
        true ->
            ok = couch_file:close(St#st.fd),
            receive
                {'DOWN', Ref, _, _, _} ->
                    ok
            after 500 ->
                ok
            end
    end,
    couch_util:shutdown_sync(St#st.fd),
    ok.

handle_db_updater_call(Msg, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.

handle_db_updater_info({'DOWN', Ref, _, _, _}, #st{fd_monitor = Ref} = St) ->
    {stop, normal, St#st{fd = undefined, fd_monitor = closed}}.

incref(St) ->
    {ok, St#st{fd_monitor = erlang:monitor(process, St#st.fd)}}.

decref(St) ->
    true = erlang:demonitor(St#st.fd_monitor, [flush]),
    ok.

monitored_by(St) ->
    case erlang:process_info(St#st.fd, monitored_by) of
        {monitored_by, Pids} ->
            lists:filter(fun is_pid/1, Pids);
        _ ->
            []
    end.

last_activity(#st{fd = Fd}) ->
    couch_file:last_read(Fd).

get_compacted_seq(#st{header = Header}) ->
    couch_bt_engine_header:get(Header, compacted_seq).

get_del_doc_count(#st{} = St) ->
    {ok, Reds} = couch_btree:full_reduce(St#st.id_tree),
    element(2, Reds).

get_disk_version(#st{header = Header}) ->
    couch_bt_engine_header:get(Header, disk_version).

get_doc_count(#st{} = St) ->
    {ok, Reds} = couch_btree:full_reduce(St#st.id_tree),
    element(1, Reds).

get_epochs(#st{header = Header}) ->
    couch_bt_engine_header:get(Header, epochs).

get_purge_seq(#st{purge_seq_tree = PurgeSeqTree}) ->
    Fun = fun({PurgeSeq, _, _, _}, _Reds, _Acc) ->
        {stop, PurgeSeq}
    end,
    {ok, _, PurgeSeq} = couch_btree:fold(PurgeSeqTree, Fun, 0, [{dir, rev}]),
    PurgeSeq.

get_oldest_purge_seq(#st{purge_seq_tree = PurgeSeqTree}) ->
    Fun = fun({PurgeSeq, _, _, _}, _Reds, _Acc) ->
        {stop, PurgeSeq}
    end,
    {ok, _, PurgeSeq} = couch_btree:fold(PurgeSeqTree, Fun, 0, []),
    PurgeSeq.

get_purge_infos_limit(#st{header = Header}) ->
    couch_bt_engine_header:get(Header, purge_infos_limit).

get_revs_limit(#st{header = Header}) ->
    couch_bt_engine_header:get(Header, revs_limit).

get_size_info(#st{} = St) ->
    {ok, FileSize} = couch_file:bytes(St#st.fd),
    {ok, DbReduction} = couch_btree:full_reduce(St#st.id_tree),
    SizeInfo0 = element(3, DbReduction),
    SizeInfo =
        case SizeInfo0 of
            SI when is_record(SI, size_info) ->
                SI;
            {AS, ES} ->
                #size_info{active = AS, external = ES};
            AS ->
                #size_info{active = AS}
        end,
    ActiveSize = active_size(St, SizeInfo),
    ExternalSize = SizeInfo#size_info.external,
    [
        {active, ActiveSize},
        {external, ExternalSize},
        {file, FileSize}
    ].

partition_size_cb(traverse, Key, {DC, DDC, Sizes}, {Partition, DCAcc, DDCAcc, SizesAcc}) ->
    case couch_partition:is_member(Key, Partition) of
        true ->
            {skip, {Partition, DC + DCAcc, DDC + DDCAcc, reduce_sizes(Sizes, SizesAcc)}};
        false ->
            {ok, {Partition, DCAcc, DDCAcc, SizesAcc}}
    end;
partition_size_cb(visit, FDI, _PrevReds, {Partition, DCAcc, DDCAcc, Acc}) ->
    InPartition = couch_partition:is_member(FDI#full_doc_info.id, Partition),
    Deleted = FDI#full_doc_info.deleted,
    case {InPartition, Deleted} of
        {true, true} ->
            {ok, {Partition, DCAcc, DDCAcc + 1, reduce_sizes(FDI#full_doc_info.sizes, Acc)}};
        {true, false} ->
            {ok, {Partition, DCAcc + 1, DDCAcc, reduce_sizes(FDI#full_doc_info.sizes, Acc)}};
        {false, _} ->
            {ok, {Partition, DCAcc, DDCAcc, Acc}}
    end.

get_partition_info(#st{} = St, Partition) ->
    StartKey = couch_partition:start_key(Partition),
    EndKey = couch_partition:end_key(Partition),
    Fun = fun partition_size_cb/4,
    InitAcc = {Partition, 0, 0, #size_info{}},
    Options = [{start_key, StartKey}, {end_key, EndKey}],
    {ok, _, OutAcc} = couch_btree:fold(St#st.id_tree, Fun, InitAcc, Options),
    {Partition, DocCount, DocDelCount, SizeInfo} = OutAcc,
    [
        {partition, Partition},
        {doc_count, DocCount},
        {doc_del_count, DocDelCount},
        {sizes, [
            {active, SizeInfo#size_info.active},
            {external, SizeInfo#size_info.external}
        ]}
    ].

get_security(#st{header = Header} = St) ->
    case couch_bt_engine_header:get(Header, security_ptr) of
        undefined ->
            [];
        Pointer ->
            {ok, SecProps} = couch_file:pread_term(St#st.fd, Pointer),
            SecProps
    end.

get_props(#st{header = Header} = St) ->
    case couch_bt_engine_header:get(Header, props_ptr) of
        undefined ->
            [];
        Pointer ->
            {ok, Props} = couch_file:pread_term(St#st.fd, Pointer),
            Props
    end.

get_update_seq(#st{header = Header}) ->
    couch_bt_engine_header:get(Header, update_seq).

get_uuid(#st{header = Header}) ->
    couch_bt_engine_header:get(Header, uuid).

set_revs_limit(#st{header = Header} = St, RevsLimit) ->
    NewSt = St#st{
        header = couch_bt_engine_header:set(Header, [
            {revs_limit, RevsLimit}
        ]),
        needs_commit = true
    },
    {ok, increment_update_seq(NewSt)}.

set_purge_infos_limit(#st{header = Header} = St, PurgeInfosLimit) ->
    NewSt = St#st{
        header = couch_bt_engine_header:set(Header, [
            {purge_infos_limit, PurgeInfosLimit}
        ]),
        needs_commit = true
    },
    {ok, increment_update_seq(NewSt)}.

set_security(#st{header = Header} = St, NewSecurity) ->
    Options = [{compression, St#st.compression}],
    {ok, Ptr, _} = couch_file:append_term(St#st.fd, NewSecurity, Options),
    NewSt = St#st{
        header = couch_bt_engine_header:set(Header, [
            {security_ptr, Ptr}
        ]),
        needs_commit = true
    },
    {ok, increment_update_seq(NewSt)}.

set_props(#st{header = Header} = St, Props) ->
    Options = [{compression, St#st.compression}],
    {ok, Ptr, _} = couch_file:append_term(St#st.fd, Props, Options),
    NewSt = St#st{
        header = couch_bt_engine_header:set(Header, [
            {props_ptr, Ptr}
        ]),
        needs_commit = true
    },
    {ok, increment_update_seq(NewSt)}.

open_docs(#st{} = St, DocIds) ->
    Results = couch_btree:lookup(St#st.id_tree, DocIds),
    lists:map(
        fun
            ({ok, FDI}) -> FDI;
            (not_found) -> not_found
        end,
        Results
    ).

open_local_docs(#st{} = St, DocIds) ->
    Results = couch_btree:lookup(St#st.local_tree, DocIds),
    lists:map(
        fun
            ({ok, Doc}) -> Doc;
            (not_found) -> not_found
        end,
        Results
    ).

read_doc_body(#st{} = St, #doc{} = Doc) ->
    {ok, {Body, Atts}} = couch_file:pread_term(St#st.fd, Doc#doc.body),
    Doc#doc{
        body = Body,
        atts = Atts
    }.

load_purge_infos(St, UUIDs) ->
    Results = couch_btree:lookup(St#st.purge_tree, UUIDs),
    lists:map(
        fun
            ({ok, Info}) -> Info;
            (not_found) -> not_found
        end,
        Results
    ).

serialize_doc(#st{} = St, #doc{} = Doc) ->
    Compress = fun(Term) ->
        case couch_compress:is_compressed(Term, St#st.compression) of
            true -> Term;
            false -> couch_compress:compress(Term, St#st.compression)
        end
    end,
    Body = Compress(Doc#doc.body),
    Atts = Compress(Doc#doc.atts),
    SummaryBin = ?term_to_bin({Body, Atts}),
    Md5 = couch_hash:md5_hash(SummaryBin),
    Data = couch_file:assemble_file_chunk(SummaryBin, Md5),
    % TODO: This is a terrible hack to get around the issues
    %       in COUCHDB-3255. We'll need to come back and figure
    %       out a better approach to handling the case when we
    %       need to generate a new revision id after the doc
    %       has been serialized.
    Doc#doc{
        body = Data,
        meta = [{comp_body, Body} | Doc#doc.meta]
    }.

write_doc_body(St, #doc{} = Doc) ->
    #st{
        fd = Fd
    } = St,
    {ok, Ptr, Written} = couch_file:append_raw_chunk(Fd, Doc#doc.body),
    {ok, Doc#doc{body = Ptr}, Written}.

write_doc_infos(#st{} = St, Pairs, LocalDocs) ->
    #st{
        id_tree = IdTree,
        seq_tree = SeqTree,
        local_tree = LocalTree
    } = St,
    FinalAcc = lists:foldl(
        fun({OldFDI, NewFDI}, Acc) ->
            {AddAcc, RemIdsAcc, RemSeqsAcc} = Acc,
            case {OldFDI, NewFDI} of
                {not_found, #full_doc_info{}} ->
                    {[NewFDI | AddAcc], RemIdsAcc, RemSeqsAcc};
                {#full_doc_info{id = Id}, #full_doc_info{id = Id}} ->
                    NewAddAcc = [NewFDI | AddAcc],
                    NewRemSeqsAcc = [OldFDI#full_doc_info.update_seq | RemSeqsAcc],
                    {NewAddAcc, RemIdsAcc, NewRemSeqsAcc};
                {#full_doc_info{id = Id}, not_found} ->
                    NewRemIdsAcc = [Id | RemIdsAcc],
                    NewRemSeqsAcc = [OldFDI#full_doc_info.update_seq | RemSeqsAcc],
                    {AddAcc, NewRemIdsAcc, NewRemSeqsAcc}
            end
        end,
        {[], [], []},
        Pairs
    ),

    {Add, RemIds, RemSeqs} = FinalAcc,
    {ok, IdTree2} = couch_btree:add_remove(IdTree, Add, RemIds),
    {ok, SeqTree2} = couch_btree:add_remove(SeqTree, Add, RemSeqs),

    {AddLDocs, RemLDocIds} = lists:foldl(
        fun(Doc, {AddAcc, RemAcc}) ->
            case Doc#doc.deleted of
                true ->
                    {AddAcc, [Doc#doc.id | RemAcc]};
                false ->
                    {[Doc | AddAcc], RemAcc}
            end
        end,
        {[], []},
        LocalDocs
    ),
    {ok, LocalTree2} = couch_btree:add_remove(LocalTree, AddLDocs, RemLDocIds),

    NewUpdateSeq = lists:foldl(
        fun(#full_doc_info{update_seq = Seq}, Acc) ->
            erlang:max(Seq, Acc)
        end,
        get_update_seq(St),
        Add
    ),

    NewHeader = couch_bt_engine_header:set(St#st.header, [
        {update_seq, NewUpdateSeq}
    ]),

    {ok, St#st{
        header = NewHeader,
        id_tree = IdTree2,
        seq_tree = SeqTree2,
        local_tree = LocalTree2,
        needs_commit = true
    }}.

purge_docs(#st{} = St, Pairs, PurgeInfos) ->
    #st{
        id_tree = IdTree,
        seq_tree = SeqTree,
        purge_tree = PurgeTree,
        purge_seq_tree = PurgeSeqTree
    } = St,

    RemDocIds = [Old#full_doc_info.id || {Old, not_found} <- Pairs],
    RemSeqs = [Old#full_doc_info.update_seq || {Old, _} <- Pairs],
    DocsToAdd = [New || {_, New} <- Pairs, New /= not_found],
    CurrSeq = couch_bt_engine_header:get(St#st.header, update_seq),
    Seqs = [FDI#full_doc_info.update_seq || FDI <- DocsToAdd],
    NewSeq = lists:max([CurrSeq | Seqs]),

    % We bump NewUpdateSeq because we have to ensure that
    % indexers see that they need to process the new purge
    % information.
    UpdateSeq =
        case NewSeq == CurrSeq of
            true -> CurrSeq + 1;
            false -> NewSeq
        end,
    Header = couch_bt_engine_header:set(St#st.header, [
        {update_seq, UpdateSeq}
    ]),

    {ok, IdTree2} = couch_btree:add_remove(IdTree, DocsToAdd, RemDocIds),
    {ok, SeqTree2} = couch_btree:add_remove(SeqTree, DocsToAdd, RemSeqs),
    {ok, PurgeTree2} = couch_btree:add(PurgeTree, PurgeInfos),
    {ok, PurgeSeqTree2} = couch_btree:add(PurgeSeqTree, PurgeInfos),
    {ok, St#st{
        header = Header,
        id_tree = IdTree2,
        seq_tree = SeqTree2,
        purge_tree = PurgeTree2,
        purge_seq_tree = PurgeSeqTree2,
        needs_commit = true
    }}.

copy_purge_infos(#st{} = St, PurgeInfos) ->
    #st{
        purge_tree = PurgeTree,
        purge_seq_tree = PurgeSeqTree
    } = St,
    {ok, PurgeTree2} = couch_btree:add(PurgeTree, PurgeInfos),
    {ok, PurgeSeqTree2} = couch_btree:add(PurgeSeqTree, PurgeInfos),
    {ok, St#st{
        purge_tree = PurgeTree2,
        purge_seq_tree = PurgeSeqTree2,
        needs_commit = true
    }}.

commit_data(St) ->
    #st{
        fd = Fd,
        header = OldHeader,
        needs_commit = NeedsCommit
    } = St,

    NewHeader = update_header(St, OldHeader),

    case NewHeader /= OldHeader orelse NeedsCommit of
        true ->
            couch_file:sync(Fd),
            ok = couch_file:write_header(Fd, NewHeader),
            couch_file:sync(Fd),
            {ok, St#st{
                header = NewHeader,
                needs_commit = false
            }};
        false ->
            {ok, St}
    end.

open_write_stream(#st{} = St, Options) ->
    couch_stream:open({couch_bt_engine_stream, {St#st.fd, []}}, Options).

open_read_stream(#st{} = St, StreamSt) ->
    {ok, {couch_bt_engine_stream, {St#st.fd, StreamSt}}}.

is_active_stream(#st{} = St, {couch_bt_engine_stream, {Fd, _}}) ->
    St#st.fd == Fd;
is_active_stream(_, _) ->
    false.

fold_docs(St, UserFun, UserAcc, Options) ->
    fold_docs_int(St, St#st.id_tree, UserFun, UserAcc, Options).

fold_local_docs(St, UserFun, UserAcc, Options) ->
    case fold_docs_int(St, St#st.local_tree, UserFun, UserAcc, Options) of
        {ok, _Reds, FinalAcc} -> {ok, null, FinalAcc};
        {ok, FinalAcc} -> {ok, FinalAcc}
    end.

fold_changes(St, SinceSeq, UserFun, UserAcc, Options) ->
    Fun = fun drop_reductions/4,
    InAcc = {UserFun, UserAcc},
    Opts = [{start_key, SinceSeq + 1}] ++ Options,
    {ok, _, OutAcc} = couch_btree:fold(St#st.seq_tree, Fun, InAcc, Opts),
    {_, FinalUserAcc} = OutAcc,
    {ok, FinalUserAcc}.

fold_purge_infos(St, StartSeq0, UserFun, UserAcc, Options) ->
    PurgeSeqTree = St#st.purge_seq_tree,
    StartSeq = StartSeq0 + 1,
    MinSeq = get_oldest_purge_seq(St),
    if
        MinSeq =< StartSeq -> ok;
        true -> erlang:error({invalid_start_purge_seq, StartSeq0})
    end,
    Wrapper = fun(Info, _Reds, UAcc) ->
        UserFun(Info, UAcc)
    end,
    Opts = [{start_key, StartSeq}] ++ Options,
    {ok, _, OutAcc} = couch_btree:fold(PurgeSeqTree, Wrapper, UserAcc, Opts),
    {ok, OutAcc}.

count_changes_since(St, SinceSeq) ->
    BTree = St#st.seq_tree,
    FoldFun = fun(_SeqStart, PartialReds, 0) ->
        {ok, couch_btree:final_reduce(BTree, PartialReds)}
    end,
    Opts = [{start_key, SinceSeq + 1}],
    {ok, Changes} = couch_btree:fold_reduce(BTree, FoldFun, 0, Opts),
    Changes.

raft_insert(#st{} = St, Entries) when is_list(Entries) ->
    #st{
        raft_tree = RaftTree0
    } = St,
    {ok, RaftTree1} = couch_btree:add_remove(RaftTree0, Entries, []),
    {ok, St#st{
        raft_tree = RaftTree1,
        needs_commit = true
    }}.

raft_lookup(#st{} = St, Indexes) ->
    Results = couch_btree:lookup(St#st.raft_tree, Indexes),
    lists:map(
        fun
            ({ok, Entry}) -> Entry;
            (not_found) -> not_found
        end,
        Results
    ).

raft_discard(#st{} = St, UpTo) ->
    #st{
        raft_tree = RaftTree0
    } = St,
    {ok, {First, _Last}} = couch_btree:full_reduce(RaftTree0),
    {FirstIndex, _FirstTerm} = First,
    Remove = lists:seq(FirstIndex, UpTo),
    {ok, RaftTree1} = couch_btree:add_remove(RaftTree0, [], Remove),
    {ok, St#st{
        raft_tree = RaftTree1,
        needs_commit = true
    }}.


raft_last(#st{} = St) ->
    {ok, {_First, Last}} = couch_btree:full_reduce(St#st.raft_tree),
    Last.

start_compaction(St, DbName, Options, Parent) ->
    Args = [St, DbName, Options, Parent],
    Pid = spawn_link(couch_bt_engine_compactor, start, Args),
    {ok, St, Pid}.

finish_compaction(OldState, DbName, Options, CompactFilePath) ->
    {ok, NewState1} = ?MODULE:init(CompactFilePath, Options),
    OldSeq = get_update_seq(OldState),
    NewSeq = get_update_seq(NewState1),
    case OldSeq == NewSeq of
        true ->
            finish_compaction_int(OldState, NewState1);
        false ->
            Level = list_to_existing_atom(
                config:get(
                    "couchdb", "compaction_log_level", "info"
                )
            ),
            couch_log:Level(
                "Compaction file still behind main file "
                "(update seq=~p. compact update seq=~p). Retrying.",
                [OldSeq, NewSeq]
            ),
            ok = decref(NewState1),
            start_compaction(OldState, DbName, Options, self())
    end.

id_tree_split(#full_doc_info{} = Info) ->
    #full_doc_info{
        id = Id,
        update_seq = Seq,
        deleted = Deleted,
        sizes = SizeInfo,
        rev_tree = Tree
    } = Info,
    {Id, {Seq, ?b2i(Deleted), split_sizes(SizeInfo), disk_tree(Tree)}}.

id_tree_join(Id, {HighSeq, Deleted, DiskTree}) ->
    % Handle old formats before data_size was added
    id_tree_join(Id, {HighSeq, Deleted, #size_info{}, DiskTree});
id_tree_join(Id, {HighSeq, Deleted, Sizes, DiskTree}) ->
    #full_doc_info{
        id = Id,
        update_seq = HighSeq,
        deleted = ?i2b(Deleted),
        sizes = couch_db_updater:upgrade_sizes(Sizes),
        rev_tree = rev_tree(DiskTree)
    }.

id_tree_reduce(reduce, FullDocInfos) ->
    lists:foldl(
        fun(Info, {NotDeleted, Deleted, Sizes}) ->
            Sizes2 = reduce_sizes(Sizes, Info#full_doc_info.sizes),
            case Info#full_doc_info.deleted of
                true ->
                    {NotDeleted, Deleted + 1, Sizes2};
                false ->
                    {NotDeleted + 1, Deleted, Sizes2}
            end
        end,
        {0, 0, #size_info{}},
        FullDocInfos
    );
id_tree_reduce(rereduce, Reds) ->
    lists:foldl(
        fun
            ({NotDeleted, Deleted}, {AccNotDeleted, AccDeleted, _AccSizes}) ->
                % pre 1.2 format, will be upgraded on compaction
                {AccNotDeleted + NotDeleted, AccDeleted + Deleted, nil};
            ({NotDeleted, Deleted, Sizes}, {AccNotDeleted, AccDeleted, AccSizes}) ->
                AccSizes2 = reduce_sizes(AccSizes, Sizes),
                {AccNotDeleted + NotDeleted, AccDeleted + Deleted, AccSizes2}
        end,
        {0, 0, #size_info{}},
        Reds
    ).

seq_tree_split(#full_doc_info{} = Info) ->
    #full_doc_info{
        id = Id,
        update_seq = Seq,
        deleted = Del,
        sizes = SizeInfo,
        rev_tree = Tree
    } = Info,
    {Seq, {Id, ?b2i(Del), split_sizes(SizeInfo), disk_tree(Tree)}}.

seq_tree_join(Seq, {Id, Del, DiskTree}) when is_integer(Del) ->
    seq_tree_join(Seq, {Id, Del, {0, 0}, DiskTree});
seq_tree_join(Seq, {Id, Del, Sizes, DiskTree}) when is_integer(Del) ->
    #full_doc_info{
        id = Id,
        update_seq = Seq,
        deleted = ?i2b(Del),
        sizes = join_sizes(Sizes),
        rev_tree = rev_tree(DiskTree)
    };
seq_tree_join(KeySeq, {Id, RevInfos, DeletedRevInfos}) ->
    % Older versions stored #doc_info records in the seq_tree.
    % Compact to upgrade.
    Revs = lists:map(
        fun({Rev, Seq, Bp}) ->
            #rev_info{rev = Rev, seq = Seq, deleted = false, body_sp = Bp}
        end,
        RevInfos
    ),
    DeletedRevs = lists:map(
        fun({Rev, Seq, Bp}) ->
            #rev_info{rev = Rev, seq = Seq, deleted = true, body_sp = Bp}
        end,
        DeletedRevInfos
    ),
    #doc_info{
        id = Id,
        high_seq = KeySeq,
        revs = Revs ++ DeletedRevs
    }.

seq_tree_reduce(reduce, DocInfos) ->
    % count the number of documents
    length(DocInfos);
seq_tree_reduce(rereduce, Reds) ->
    lists:sum(Reds).

local_tree_split(#doc{revs = {0, [Rev]}} = Doc) when is_binary(Rev) ->
    #doc{
        id = Id,
        body = BodyData
    } = Doc,
    {Id, {binary_to_integer(Rev), BodyData}};
local_tree_split(#doc{revs = {0, [Rev]}} = Doc) when is_integer(Rev) ->
    #doc{
        id = Id,
        body = BodyData
    } = Doc,
    {Id, {Rev, BodyData}}.

local_tree_join(Id, {Rev, BodyData}) when is_binary(Rev) ->
    #doc{
        id = Id,
        revs = {0, [Rev]},
        body = BodyData
    };
local_tree_join(Id, {Rev, BodyData}) when is_integer(Rev) ->
    #doc{
        id = Id,
        revs = {0, [integer_to_binary(Rev)]},
        body = BodyData
    }.

purge_tree_split({PurgeSeq, UUID, DocId, Revs}) ->
    {UUID, {PurgeSeq, DocId, Revs}}.

purge_tree_join(UUID, {PurgeSeq, DocId, Revs}) ->
    {PurgeSeq, UUID, DocId, Revs}.

purge_seq_tree_split({PurgeSeq, UUID, DocId, Revs}) ->
    {PurgeSeq, {UUID, DocId, Revs}}.

purge_seq_tree_join(PurgeSeq, {UUID, DocId, Revs}) ->
    {PurgeSeq, UUID, DocId, Revs}.

purge_tree_reduce(reduce, IdRevs) ->
    % count the number of purge requests
    length(IdRevs);
purge_tree_reduce(rereduce, Reds) ->
    lists:sum(Reds).

raft_tree_split({Index, Term, Value}) ->
    {Index, {Term, Value}}.

raft_tree_join(Index, {Term, Value}) ->
    {Index, Term, Value}.


raft_tree_reduce(reduce, []) ->
    {{0, 0}, {0, 0}};
raft_tree_reduce(reduce, Entries) ->
    {MinIndex, MinTerm, _} = lists:min(Entries),
    {MaxIndex, MaxTerm, _} = lists:max(Entries),
    {{MinIndex, MinTerm}, {MaxIndex, MaxTerm}};
raft_tree_reduce(rereduce, Reds) ->
    {Mins, Maxs} = lists:unzip(Reds),
    {lists:min(Mins), lists:max(Maxs)}.

set_update_seq(#st{header = Header} = St, UpdateSeq) ->
    {ok, St#st{
        header = couch_bt_engine_header:set(Header, [
            {update_seq, UpdateSeq}
        ]),
        needs_commit = true
    }}.

copy_security(#st{header = Header} = St, SecProps) ->
    Options = [{compression, St#st.compression}],
    {ok, Ptr, _} = couch_file:append_term(St#st.fd, SecProps, Options),
    {ok, St#st{
        header = couch_bt_engine_header:set(Header, [
            {security_ptr, Ptr}
        ]),
        needs_commit = true
    }}.

copy_props(#st{header = Header} = St, Props) ->
    Options = [{compression, St#st.compression}],
    {ok, Ptr, _} = couch_file:append_term(St#st.fd, Props, Options),
    {ok, St#st{
        header = couch_bt_engine_header:set(Header, [
            {props_ptr, Ptr}
        ]),
        needs_commit = true
    }}.

open_db_file(FilePath, Options) ->
    case couch_file:open(FilePath, Options) of
        {ok, Fd} ->
            {ok, Fd};
        {error, enoent} ->
            % Couldn't find file. is there a compact version? This ca
            % happen (rarely) if we crashed during the file switch.
            case couch_file:open(FilePath ++ ".compact", [nologifmissing]) of
                {ok, Fd} ->
                    Fmt = "Recovering from compaction file: ~s~s",
                    couch_log:info(Fmt, [FilePath, ".compact"]),
                    ok = file:rename(FilePath ++ ".compact", FilePath),
                    ok = couch_file:sync(Fd),
                    {ok, Fd};
                {error, enoent} ->
                    throw({not_found, no_db_file})
            end;
        Error ->
            throw(Error)
    end.

init_state(FilePath, Fd, Header0, Options) ->
    ok = couch_file:sync(Fd),

    Compression = couch_compress:get_compression_method(),

    Header1 = couch_bt_engine_header:upgrade(Header0),
    Header2 = set_default_security_object(Fd, Header1, Compression, Options),
    Header = upgrade_purge_info(Fd, Header2),

    IdTreeState = couch_bt_engine_header:id_tree_state(Header),
    {ok, IdTree} = couch_btree:open(IdTreeState, Fd, [
        {split, fun ?MODULE:id_tree_split/1},
        {join, fun ?MODULE:id_tree_join/2},
        {reduce, fun ?MODULE:id_tree_reduce/2},
        {compression, Compression}
    ]),

    SeqTreeState = couch_bt_engine_header:seq_tree_state(Header),
    {ok, SeqTree} = couch_btree:open(SeqTreeState, Fd, [
        {split, fun ?MODULE:seq_tree_split/1},
        {join, fun ?MODULE:seq_tree_join/2},
        {reduce, fun ?MODULE:seq_tree_reduce/2},
        {compression, Compression}
    ]),

    LocalTreeState = couch_bt_engine_header:local_tree_state(Header),
    {ok, LocalTree} = couch_btree:open(LocalTreeState, Fd, [
        {split, fun ?MODULE:local_tree_split/1},
        {join, fun ?MODULE:local_tree_join/2},
        {compression, Compression}
    ]),

    PurgeTreeState = couch_bt_engine_header:purge_tree_state(Header),
    {ok, PurgeTree} = couch_btree:open(PurgeTreeState, Fd, [
        {split, fun ?MODULE:purge_tree_split/1},
        {join, fun ?MODULE:purge_tree_join/2},
        {reduce, fun ?MODULE:purge_tree_reduce/2}
    ]),

    PurgeSeqTreeState = couch_bt_engine_header:purge_seq_tree_state(Header),
    {ok, PurgeSeqTree} = couch_btree:open(PurgeSeqTreeState, Fd, [
        {split, fun ?MODULE:purge_seq_tree_split/1},
        {join, fun ?MODULE:purge_seq_tree_join/2},
        {reduce, fun ?MODULE:purge_tree_reduce/2}
    ]),

    RaftTreeState = couch_bt_engine_header:raft_tree_state(Header),
    {ok, RaftTree} = couch_btree:open(RaftTreeState, Fd, [
        {split, fun ?MODULE:raft_tree_split/1},
        {join, fun ?MODULE:raft_tree_join/2},
        {reduce, fun ?MODULE:raft_tree_reduce/2}
    ]),

    ok = couch_file:set_db_pid(Fd, self()),

    St = #st{
        filepath = FilePath,
        fd = Fd,
        fd_monitor = erlang:monitor(process, Fd),
        header = Header,
        needs_commit = false,
        id_tree = IdTree,
        seq_tree = SeqTree,
        local_tree = LocalTree,
        compression = Compression,
        purge_tree = PurgeTree,
        purge_seq_tree = PurgeSeqTree,
        raft_tree = RaftTree
    },

    % If this is a new database we've just created a
    % new UUID and default security object which need
    % to be written to disk.
    case Header /= Header0 of
        true ->
            {ok, NewSt} = commit_data(St#st{needs_commit = true}),
            NewSt;
        false ->
            St
    end.

update_header(St, Header) ->
    couch_bt_engine_header:set(Header, [
        {seq_tree_state, couch_btree:get_state(St#st.seq_tree)},
        {id_tree_state, couch_btree:get_state(St#st.id_tree)},
        {local_tree_state, couch_btree:get_state(St#st.local_tree)},
        {purge_tree_state, couch_btree:get_state(St#st.purge_tree)},
        {purge_seq_tree_state, couch_btree:get_state(St#st.purge_seq_tree)},
        {raft_tree_state, couch_btree:get_state(St#st.raft_tree)}
    ]).

increment_update_seq(#st{header = Header} = St) ->
    UpdateSeq = couch_bt_engine_header:get(Header, update_seq),
    St#st{
        header = couch_bt_engine_header:set(Header, [
            {update_seq, UpdateSeq + 1}
        ])
    }.

set_default_security_object(Fd, Header, Compression, Options) ->
    case couch_bt_engine_header:get(Header, security_ptr) of
        Pointer when is_integer(Pointer) ->
            Header;
        _ ->
            Default = couch_util:get_value(default_security_object, Options),
            AppendOpts = [{compression, Compression}],
            {ok, Ptr, _} = couch_file:append_term(Fd, Default, AppendOpts),
            couch_bt_engine_header:set(Header, security_ptr, Ptr)
    end.

% This function is here, and not in couch_bt_engine_header
% because it requires modifying file contents
upgrade_purge_info(Fd, Header) ->
    case couch_bt_engine_header:get(Header, purge_tree_state) of
        nil ->
            Header;
        Ptr when is_tuple(Ptr) ->
            Header;
        PurgeSeq when is_integer(PurgeSeq) ->
            % Pointer to old purged ids/revs is in purge_seq_tree_state
            Ptr = couch_bt_engine_header:get(Header, purge_seq_tree_state),

            case Ptr of
                nil ->
                    PTS = couch_bt_engine_header:purge_tree_state(Header),
                    PurgeTreeSt =
                        case PTS of
                            0 -> nil;
                            Else -> Else
                        end,
                    couch_bt_engine_header:set(Header, [
                        {purge_tree_state, PurgeTreeSt}
                    ]);
                _ ->
                    {ok, PurgedIdsRevs} = couch_file:pread_term(Fd, Ptr),

                    {Infos, _} = lists:foldl(
                        fun({Id, Revs}, {InfoAcc, PSeq}) ->
                            Info = {PSeq, couch_uuids:random(), Id, Revs},
                            {[Info | InfoAcc], PSeq + 1}
                        end,
                        {[], PurgeSeq},
                        PurgedIdsRevs
                    ),

                    {ok, PurgeTree} = couch_btree:open(nil, Fd, [
                        {split, fun ?MODULE:purge_tree_split/1},
                        {join, fun ?MODULE:purge_tree_join/2},
                        {reduce, fun ?MODULE:purge_tree_reduce/2}
                    ]),
                    {ok, PurgeTree2} = couch_btree:add(PurgeTree, Infos),
                    PurgeTreeSt = couch_btree:get_state(PurgeTree2),

                    {ok, PurgeSeqTree} = couch_btree:open(nil, Fd, [
                        {split, fun ?MODULE:purge_seq_tree_split/1},
                        {join, fun ?MODULE:purge_seq_tree_join/2},
                        {reduce, fun ?MODULE:purge_tree_reduce/2}
                    ]),
                    {ok, PurgeSeqTree2} = couch_btree:add(PurgeSeqTree, Infos),
                    PurgeSeqTreeSt = couch_btree:get_state(PurgeSeqTree2),

                    couch_bt_engine_header:set(Header, [
                        {purge_tree_state, PurgeTreeSt},
                        {purge_seq_tree_state, PurgeSeqTreeSt}
                    ])
            end
    end.

init_set_props(Fd, Header, Options) ->
    case couch_util:get_value(props, Options) of
        undefined ->
            Header;
        InitialProps ->
            Compression = couch_compress:get_compression_method(),
            AppendOpts = [{compression, Compression}],
            {ok, Ptr, _} = couch_file:append_term(Fd, InitialProps, AppendOpts),
            couch_bt_engine_header:set(Header, props_ptr, Ptr)
    end.

delete_compaction_files(FilePath) ->
    RootDir = config:get("couchdb", "database_dir", "."),
    DelOpts = [{context, compaction}],
    delete_compaction_files(RootDir, FilePath, DelOpts).

rev_tree(DiskTree) ->
    couch_key_tree:map(
        fun
            (_RevId, {Del, Ptr, Seq}) ->
                #leaf{
                    deleted = ?i2b(Del),
                    ptr = Ptr,
                    seq = Seq
                };
            (_RevId, {Del, Ptr, Seq, Size}) ->
                #leaf{
                    deleted = ?i2b(Del),
                    ptr = Ptr,
                    seq = Seq,
                    sizes = couch_db_updater:upgrade_sizes(Size)
                };
            (_RevId, {Del, Ptr, Seq, Sizes, Atts}) ->
                #leaf{
                    deleted = ?i2b(Del),
                    ptr = Ptr,
                    seq = Seq,
                    sizes = couch_db_updater:upgrade_sizes(Sizes),
                    atts = Atts
                };
            (_RevId, ?REV_MISSING) ->
                ?REV_MISSING
        end,
        DiskTree
    ).

disk_tree(RevTree) ->
    couch_key_tree:map(
        fun
            (_RevId, ?REV_MISSING) ->
                ?REV_MISSING;
            (_RevId, #leaf{} = Leaf) ->
                #leaf{
                    deleted = Del,
                    ptr = Ptr,
                    seq = Seq,
                    sizes = Sizes,
                    atts = Atts
                } = Leaf,
                {?b2i(Del), Ptr, Seq, split_sizes(Sizes), Atts}
        end,
        RevTree
    ).

split_sizes(#size_info{} = SI) ->
    {SI#size_info.active, SI#size_info.external}.

join_sizes({Active, External}) when is_integer(Active), is_integer(External) ->
    #size_info{active = Active, external = External}.

reduce_sizes(nil, _) ->
    nil;
reduce_sizes(_, nil) ->
    nil;
reduce_sizes(#size_info{} = S1, #size_info{} = S2) ->
    #size_info{
        active = S1#size_info.active + S2#size_info.active,
        external = S1#size_info.external + S2#size_info.external
    };
reduce_sizes(S1, S2) ->
    US1 = couch_db_updater:upgrade_sizes(S1),
    US2 = couch_db_updater:upgrade_sizes(S2),
    reduce_sizes(US1, US2).

active_size(#st{} = St, #size_info{} = SI) ->
    Trees = [
        St#st.id_tree,
        St#st.seq_tree,
        St#st.local_tree,
        St#st.purge_tree,
        St#st.purge_seq_tree,
        St#st.raft_tree
    ],
    lists:foldl(
        fun(T, Acc) ->
            case couch_btree:size(T) of
                _ when Acc == null ->
                    null;
                nil ->
                    null;
                Size ->
                    Acc + Size
            end
        end,
        SI#size_info.active,
        Trees
    ).

fold_docs_int(St, Tree, UserFun, UserAcc, Options) ->
    Fun =
        case lists:member(include_deleted, Options) of
            true -> fun include_deleted/4;
            false -> fun skip_deleted/4
        end,
    RedFun =
        case lists:member(include_reductions, Options) of
            true -> fun include_reductions/4;
            false -> fun drop_reductions/4
        end,
    InAcc = {RedFun, {UserFun, UserAcc}},
    {ok, Reds, OutAcc} = couch_btree:fold(Tree, Fun, InAcc, Options),
    {_, {_, FinalUserAcc}} = OutAcc,
    case lists:member(include_reductions, Options) of
        true when Tree == St#st.id_tree ->
            {ok, fold_docs_reduce_to_count(Reds), FinalUserAcc};
        true when Tree == St#st.local_tree ->
            {ok, 0, FinalUserAcc};
        false ->
            {ok, FinalUserAcc}
    end.

include_deleted(Case, Entry, Reds, {UserFun, UserAcc}) ->
    {Go, NewUserAcc} = UserFun(Case, Entry, Reds, UserAcc),
    {Go, {UserFun, NewUserAcc}}.

% First element of the reductions is the total
% number of undeleted documents.
skip_deleted(traverse, _Entry, {0, _, _} = _Reds, Acc) ->
    {skip, Acc};
skip_deleted(visit, #full_doc_info{deleted = true}, _, Acc) ->
    {ok, Acc};
skip_deleted(Case, Entry, Reds, {UserFun, UserAcc}) ->
    {Go, NewUserAcc} = UserFun(Case, Entry, Reds, UserAcc),
    {Go, {UserFun, NewUserAcc}}.

include_reductions(visit, FDI, Reds, {UserFun, UserAcc}) ->
    {Go, NewUserAcc} = UserFun(FDI, Reds, UserAcc),
    {Go, {UserFun, NewUserAcc}};
include_reductions(_, _, _, Acc) ->
    {ok, Acc}.

drop_reductions(visit, FDI, _Reds, {UserFun, UserAcc}) ->
    {Go, NewUserAcc} = UserFun(FDI, UserAcc),
    {Go, {UserFun, NewUserAcc}};
drop_reductions(_, _, _, Acc) ->
    {ok, Acc}.

fold_docs_reduce_to_count(Reds) ->
    RedFun = fun id_tree_reduce/2,
    FinalRed = couch_btree:final_reduce(RedFun, Reds),
    element(1, FinalRed).

finish_compaction_int(#st{} = OldSt, #st{} = NewSt1) ->
    #st{
        filepath = FilePath,
        local_tree = OldLocal,
        raft_tree = OldRaft
    } = OldSt,
    #st{
        filepath = CompactDataPath,
        header = Header,
        local_tree = NewLocal1,
        raft_tree = NewRaft1
    } = NewSt1,

    % suck up all the local docs into memory and write them to the new db
    LoadFun = fun(Value, _Offset, Acc) ->
        {ok, [Value | Acc]}
    end,
    {ok, _, LocalDocs} = couch_btree:foldl(OldLocal, LoadFun, []),
    {ok, NewLocal2} = couch_btree:add(NewLocal1, LocalDocs),

    % do the same for the raft log
    {ok, _, RaftLog} = couch_btree:foldl(OldRaft, LoadFun, []),
    {ok, NewRaft2} = couch_btree:add(NewRaft1, RaftLog),

    {ok, NewSt2} = commit_data(NewSt1#st{
        header = couch_bt_engine_header:set(Header, [
            {compacted_seq, get_update_seq(OldSt)},
            {revs_limit, get_revs_limit(OldSt)},
            {purge_infos_limit, get_purge_infos_limit(OldSt)}
        ]),
        local_tree = NewLocal2,
        raft_tree = NewRaft2
    }),

    % Rename our *.compact.data file to *.compact so that if we
    % die between deleting the old file and renaming *.compact
    % we can recover correctly.
    ok = file:rename(CompactDataPath, FilePath ++ ".compact"),

    % Remove the uncompacted database file
    RootDir = config:get("couchdb", "database_dir", "."),
    couch_file:delete(RootDir, FilePath),

    % Move our compacted file into its final location
    ok = file:rename(FilePath ++ ".compact", FilePath),

    % Delete the old meta compaction file after promoting
    % the compaction file.
    couch_file:delete(RootDir, FilePath ++ ".compact.meta"),

    % We're finished with our old state
    decref(OldSt),

    % And return our finished new state
    {ok,
        NewSt2#st{
            filepath = FilePath
        },
        undefined}.

is_file(Path) ->
    case file:read_file_info(Path, [raw]) of
        {ok, #file_info{type = regular}} -> true;
        {ok, #file_info{type = directory}} -> true;
        _ -> false
    end.
