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

-module(couch_db_updater).
-behaviour(gen_server).

-export([btree_by_id_split/1,btree_by_id_join/2,btree_by_id_reduce/2]).
-export([btree_by_seq_split/1,btree_by_seq_join/2,btree_by_seq_reduce/2]).
-export([init/1,terminate/2,handle_call/3,handle_cast/2,code_change/3,handle_info/2]).

-include("couch_db.hrl").


init({DbName, Filepath, Fd, Options}) ->
    case lists:member(create, Options) of
    true ->
        % create a new header and writes it to the file
        Header =  #db_header{},
        ok = couch_file:write_header(Fd, Header),
        % delete any old compaction files that might be hanging around
        RootDir = couch_config:get("couchdb", "database_dir", "."),
        couch_file:delete(RootDir, Filepath ++ ".compact");
    false ->
        ok = couch_file:upgrade_old_header(Fd, <<$g, $m, $k, 0>>), % 09 UPGRADE CODE
        case couch_file:read_header(Fd) of
        {ok, Header} ->
            ok;
        no_valid_header ->
            % create a new header and writes it to the file
            Header =  #db_header{},
            ok = couch_file:write_header(Fd, Header),
            % delete any old compaction files that might be hanging around
            file:delete(Filepath ++ ".compact")
        end
    end,

    Db = init_db(DbName, Filepath, Fd, Header),
    couch_stats_collector:track_process_count({couchdb, open_databases}),
    % we don't load validation funs here because the fabric query is liable to
    % race conditions.  Instead see couch_db:validate_doc_update, which loads
    % them lazily
    {ok, Db#db{main_pid = self(), is_sys_db = lists:member(sys_db, Options)}}.


terminate(_Reason, Db) ->
    ok = couch_file:close(Db#db.fd),
    couch_util:shutdown_sync(Db#db.compactor_pid),
    couch_util:shutdown_sync(Db#db.fd),
    ok.

handle_call(start_compact, _From, Db) ->
    {noreply, NewDb} = handle_cast(start_compact, Db),
    {reply, {ok, NewDb#db.compactor_pid}, NewDb};

handle_call(get_db, _From, Db) ->
    {reply, {ok, Db}, Db};
handle_call(full_commit, _From, #db{waiting_delayed_commit=nil}=Db) ->
    {reply, ok, Db};
handle_call(full_commit, _From, Db) ->
    {reply, ok, commit_data(Db)};

handle_call({full_commit, _}, _From, #db{waiting_delayed_commit=nil}=Db) ->
    {reply, ok, Db}; % no data waiting, return ok immediately
handle_call({full_commit, RequiredSeq}, _From, Db) when RequiredSeq =<
        Db#db.committed_update_seq ->
    {reply, ok, Db};
handle_call({full_commit, _}, _, Db) ->
    {reply, ok, commit_data(Db)}; % commit the data and return ok
handle_call(increment_update_seq, _From, Db) ->
    Db2 = commit_data(Db#db{update_seq=Db#db.update_seq+1}),
    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    couch_db_update_notifier:notify({updated, Db#db.name}),
    {reply, {ok, Db2#db.update_seq}, Db2};

handle_call({set_security, NewSec}, _From, Db) ->
    {ok, Ptr} = couch_file:append_term(Db#db.fd, NewSec),
    Db2 = commit_data(Db#db{security=NewSec, security_ptr=Ptr,
            update_seq=Db#db.update_seq+1}),
    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    {reply, ok, Db2};

handle_call({set_revs_limit, Limit}, _From, Db) ->
    Db2 = commit_data(Db#db{revs_limit=Limit,
            update_seq=Db#db.update_seq+1}),
    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    {reply, ok, Db2};

handle_call({purge_docs, _IdRevs}, _From,
        #db{compactor_pid=Pid}=Db) when Pid /= nil ->
    {reply, {error, purge_during_compaction}, Db};
handle_call({purge_docs, IdRevs}, _From, Db) ->
    #db{
        fd=Fd,
        id_tree = DocInfoByIdBTree,
        seq_tree = DocInfoBySeqBTree,
        update_seq = LastSeq,
        header = Header = #db_header{purge_seq=PurgeSeq}
        } = Db,
    DocLookups = couch_btree:lookup(DocInfoByIdBTree,
            [Id || {Id, _Revs} <- IdRevs]),

    NewDocInfos = lists:zipwith(
        fun({_Id, Revs}, {ok, #full_doc_info{rev_tree=Tree}=FullDocInfo}) ->
            case couch_key_tree:remove_leafs(Tree, Revs) of
            {_, []=_RemovedRevs} -> % no change
                nil;
            {NewTree, RemovedRevs} ->
                {FullDocInfo#full_doc_info{rev_tree=NewTree},RemovedRevs}
            end;
        (_, not_found) ->
            nil
        end,
        IdRevs, DocLookups),

    SeqsToRemove = [Seq
            || {#full_doc_info{update_seq=Seq},_} <- NewDocInfos],

    FullDocInfoToUpdate = [FullInfo
            || {#full_doc_info{rev_tree=Tree}=FullInfo,_}
            <- NewDocInfos, Tree /= []],

    IdRevsPurged = [{Id, Revs}
            || {#full_doc_info{id=Id}, Revs} <- NewDocInfos],

    {DocInfoToUpdate, NewSeq} = lists:mapfoldl(
        fun(#full_doc_info{rev_tree=Tree}=FullInfo, SeqAcc) ->
            Tree2 = couch_key_tree:map_leafs(
                fun(_RevId, {IsDeleted, BodyPointer, _UpdateSeq}) ->
                    {IsDeleted, BodyPointer, SeqAcc + 1}
                end, Tree),
            {couch_doc:to_doc_info(FullInfo#full_doc_info{rev_tree=Tree2}),
                SeqAcc + 1}
        end, LastSeq, FullDocInfoToUpdate),

    IdsToRemove = [Id || {#full_doc_info{id=Id,rev_tree=[]},_}
            <- NewDocInfos],

    {ok, DocInfoBySeqBTree2} = couch_btree:add_remove(DocInfoBySeqBTree,
            DocInfoToUpdate, SeqsToRemove),
    {ok, DocInfoByIdBTree2} = couch_btree:add_remove(DocInfoByIdBTree,
            FullDocInfoToUpdate, IdsToRemove),
    {ok, Pointer} = couch_file:append_term(Fd, IdRevsPurged),

    Db2 = commit_data(
        Db#db{
            id_tree = DocInfoByIdBTree2,
            seq_tree = DocInfoBySeqBTree2,
            update_seq = NewSeq + 1,
            header=Header#db_header{purge_seq=PurgeSeq+1, purged_docs=Pointer}}),

    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    couch_db_update_notifier:notify({updated, Db#db.name}),
    {reply, {ok, (Db2#db.header)#db_header.purge_seq, IdRevsPurged}, Db2}.


handle_cast({load_validation_funs, ValidationFuns}, Db) ->
    Db2 = Db#db{validate_doc_funs = ValidationFuns},
    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    {noreply, Db2};
handle_cast(start_compact, Db) ->
    case Db#db.compactor_pid of
    nil ->
        ?LOG_INFO("Starting compaction for db \"~s\"", [Db#db.name]),
        Pid = spawn_link(fun() -> start_copy_compact(Db) end),
        Db2 = Db#db{compactor_pid=Pid},
        ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
        {noreply, Db2};
    _ ->
        % compact currently running, this is a no-op
        {noreply, Db}
    end;
handle_cast({compact_done, CompactFilepath}, #db{filepath=Filepath}=Db) ->
    {ok, NewFd} = couch_file:open(CompactFilepath),
    {ok, NewHeader} = couch_file:read_header(NewFd),
    #db{update_seq=NewSeq} = NewDb =
            init_db(Db#db.name, Filepath, NewFd, NewHeader),
    unlink(NewFd),
    case Db#db.update_seq == NewSeq of
    true ->
        % suck up all the local docs into memory and write them to the new db
        {ok, _, LocalDocs} = couch_btree:foldl(Db#db.local_tree,
                fun(Value, _Offset, Acc) -> {ok, [Value | Acc]} end, []),
        {ok, NewLocalBtree} = couch_btree:add(NewDb#db.local_tree, LocalDocs),

        NewDb2 = commit_data(NewDb#db{
            local_tree = NewLocalBtree,
            main_pid = self(),
            filepath = Filepath,
            instance_start_time = Db#db.instance_start_time,
            revs_limit = Db#db.revs_limit
        }),

        ?LOG_DEBUG("CouchDB swapping files ~s and ~s.",
                [Filepath, CompactFilepath]),
        RootDir = couch_config:get("couchdb", "database_dir", "."),
        couch_file:delete(RootDir, Filepath),
        ok = file:rename(CompactFilepath, Filepath),
        close_db(Db),
        NewDb3 = refresh_validate_doc_funs(NewDb2),
        ok = gen_server:call(couch_server, {db_updated, NewDb3}, infinity),
        couch_db_update_notifier:notify({compacted, NewDb3#db.name}),
        ?LOG_INFO("Compaction for db \"~s\" completed.", [Db#db.name]),
        {noreply, NewDb3#db{compactor_pid=nil}};
    false ->
        ?LOG_INFO("Compaction for ~s still behind main file "
            "(update seq=~p. compact update seq=~p). Retrying.",
            [Db#db.name, Db#db.update_seq, NewSeq]),
        close_db(NewDb),
        Pid = spawn_link(fun() -> start_copy_compact(Db) end),
        Db2 = Db#db{compactor_pid=Pid},
        {noreply, Db2}
    end.


handle_info({update_docs, Client, GroupedDocs, NonRepDocs, MergeConflicts,
        FullCommit}, Db) ->
    GroupedDocs2 = [[{Client, D} || D <- DocGroup] || DocGroup <- GroupedDocs],
    if NonRepDocs == [] ->
        {GroupedDocs3, Clients, FullCommit2} = collect_updates(GroupedDocs2,
                [Client], MergeConflicts, FullCommit);
    true ->
        GroupedDocs3 = GroupedDocs2,
        FullCommit2 = FullCommit,
        Clients = [Client]
    end,
    NonRepDocs2 = [{Client, NRDoc} || NRDoc <- NonRepDocs],
    try update_docs_int(Db, GroupedDocs3, NonRepDocs2, MergeConflicts,
                FullCommit2) of
    {ok, Db2} ->
        ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
        if Db2#db.update_seq /= Db#db.update_seq ->
            couch_db_update_notifier:notify({updated, Db2#db.name});
        true -> ok
        end,
        [catch(ClientPid ! {done, self()}) || ClientPid <- Clients],
        {noreply, Db2}
    catch
        throw: retry ->
            [catch(ClientPid ! {retry, self()}) || ClientPid <- Clients],
            {noreply, Db}
    end;
handle_info(delayed_commit, #db{waiting_delayed_commit=nil}=Db) ->
    %no outstanding delayed commits, ignore
    {noreply, Db};
handle_info(delayed_commit, Db) ->
    case commit_data(Db) of
        Db ->
            {noreply, Db};
        Db2 ->
            ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
            {noreply, Db2}
    end;
handle_info({'EXIT', _Pid, normal}, Db) ->
    {noreply, Db};
handle_info({'EXIT', _Pid, Reason}, Db) ->
    {stop, Reason, Db};
handle_info({'DOWN', Ref, _, _, Reason}, #db{fd_monitor=Ref, name=Name} = Db) ->
    ?LOG_ERROR("DB ~s shutting down - Fd ~p", [Name, Reason]),
    {stop, normal, Db}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


merge_updates([], RestB, AccOutGroups) ->
    lists:reverse(AccOutGroups, RestB);
merge_updates(RestA, [], AccOutGroups) ->
    lists:reverse(AccOutGroups, RestA);
merge_updates([[{_, #doc{id=IdA}}|_]=GroupA | RestA],
        [[{_, #doc{id=IdB}}|_]=GroupB | RestB], AccOutGroups) ->
    if IdA == IdB ->
        merge_updates(RestA, RestB, [GroupA ++ GroupB | AccOutGroups]);
    IdA < IdB ->
        merge_updates(RestA, [GroupB | RestB], [GroupA | AccOutGroups]);
    true ->
        merge_updates([GroupA | RestA], RestB, [GroupB | AccOutGroups])
    end.

collect_updates(GroupedDocsAcc, ClientsAcc, MergeConflicts, FullCommit) ->
    receive
        % Only collect updates with the same MergeConflicts flag and without
        % local docs. It's easier to just avoid multiple _local doc
        % updaters than deal with their possible conflicts, and local docs
        % writes are relatively rare. Can be optmized later if really needed.
        {update_docs, Client, GroupedDocs, [], MergeConflicts, FullCommit2} ->
            GroupedDocs2 = [[{Client, Doc} || Doc <- DocGroup]
                    || DocGroup <- GroupedDocs],
            GroupedDocsAcc2 =
                merge_updates(GroupedDocsAcc, GroupedDocs2, []),
            collect_updates(GroupedDocsAcc2, [Client | ClientsAcc],
                    MergeConflicts, (FullCommit or FullCommit2))
    after 0 ->
        {GroupedDocsAcc, ClientsAcc, FullCommit}
    end.


rev_tree(DiskTree) ->
    couch_key_tree:map(fun(_RevId, {Del, Ptr, Seq}) ->
        #leaf{deleted=(Del==1), ptr=Ptr, seq=Seq};
    (_RevId, {Del, Ptr, Seq, Size, Atts}) ->
        #leaf{deleted=(Del==1), ptr=Ptr, seq=Seq, size=Size, atts=Atts};
    (_RevId, ?REV_MISSING) ->
        ?REV_MISSING
    end, DiskTree).

disk_tree(RevTree) ->
    couch_key_tree:map(fun(_RevId, ?REV_MISSING) ->
        ?REV_MISSING;
    (_RevId, #leaf{deleted=Del, ptr=Ptr, seq=Seq, size=Size, atts=Atts}) ->
        {if Del -> 1; true -> 0 end, Ptr, Seq, Size, Atts}
    end, RevTree).

btree_by_seq_split(#full_doc_info{id=Id, update_seq=Seq, deleted=Del, rev_tree=T}) ->
    {Seq, {Id, if Del -> 1; true -> 0 end, disk_tree(T)}}.

btree_by_seq_join(Seq, {Id, Del, T}) when is_integer(Del) ->
    #full_doc_info{id=Id, update_seq=Seq, deleted=Del==1, rev_tree=rev_tree(T)};
btree_by_seq_join(KeySeq, {Id, RevInfos, DeletedRevInfos}) ->
    % 1.0 stored #doc_info records in the seq tree.  compact to upgrade.
    #doc_info{
        id = Id,
        high_seq=KeySeq,
        revs =
            [#rev_info{rev=Rev,seq=Seq,deleted=false,body_sp = Bp} ||
                {Rev, Seq, Bp} <- RevInfos] ++
            [#rev_info{rev=Rev,seq=Seq,deleted=true,body_sp = Bp} ||
                {Rev, Seq, Bp} <- DeletedRevInfos]};
btree_by_seq_join(KeySeq,{Id, Rev, Bp, Conflicts, DelConflicts, Deleted}) ->
    % 09 UPGRADE CODE
    % this is the 0.9.0 and earlier by_seq record. It's missing the body pointers
    % and individual seq nums for conflicts that are currently in the index,
    % meaning the filtered _changes api will not work except for on main docs.
    % Simply compact a 0.9.0 database to upgrade the index.
    #doc_info{
        id=Id,
        high_seq=KeySeq,
        revs = [#rev_info{rev=Rev,seq=KeySeq,deleted=Deleted,body_sp=Bp}] ++
            [#rev_info{rev=Rev1,seq=KeySeq,deleted=false} || Rev1 <- Conflicts] ++
            [#rev_info{rev=Rev2,seq=KeySeq,deleted=true} || Rev2 <- DelConflicts]}.

btree_by_id_split(#full_doc_info{id=Id, update_seq=Seq,
        data_size=Size, deleted=Deleted, rev_tree=Tree}) ->
    {Id, {Seq, if Deleted -> 1; true -> 0 end, Size, disk_tree(Tree)}}.

%% handle old formats before `data_size` added
btree_by_id_join(Id, {HighSeq, Deleted, DiskTree}) ->
    btree_by_id_join(Id, {HighSeq, Deleted, 0, DiskTree});

btree_by_id_join(Id, {HighSeq, Deleted, Size, DiskTree}) ->
    #full_doc_info{id=Id, update_seq=HighSeq,
                   deleted=Deleted==1, data_size=Size,
                   rev_tree=rev_tree(DiskTree)}.

btree_by_id_reduce(reduce, FullDocInfos) ->
    lists:foldl(
        fun(#full_doc_info{deleted = false, data_size=Size},
            {NotDeleted, Deleted, DocSize}) ->
                {NotDeleted + 1, Deleted, DocSize + Size};
           (#full_doc_info{deleted = true, data_size=Size},
            {NotDeleted, Deleted, DocSize}) ->
                {NotDeleted, Deleted + 1, DocSize + Size}
        end,
        {0, 0, 0}, FullDocInfos);

btree_by_id_reduce(rereduce, Reductions) ->
    lists:foldl(
        fun({NotDeleted, Deleted}, {AccNotDeleted, AccDeleted, AccDocSizes}) ->
            {AccNotDeleted + NotDeleted, AccDeleted + Deleted, AccDocSizes};
           ({NotDeleted, Deleted, DocSizes}, {AccNotDeleted, AccDeleted, AccDocSizes}) ->
            {AccNotDeleted + NotDeleted, AccDeleted + Deleted, DocSizes + AccDocSizes}
        end,
        {0, 0, 0}, Reductions).

btree_by_seq_reduce(reduce, DocInfos) ->
    % count the number of documents
    length(DocInfos);
btree_by_seq_reduce(rereduce, Reds) ->
    lists:sum(Reds).

simple_upgrade_record(Old, New) when tuple_size(Old) =:= tuple_size(New) ->
    Old;
simple_upgrade_record(Old, New) when tuple_size(Old) < tuple_size(New) ->
    OldSz = tuple_size(Old),
    NewValuesTail =
        lists:sublist(tuple_to_list(New), OldSz + 1, tuple_size(New) - OldSz),
    list_to_tuple(tuple_to_list(Old) ++ NewValuesTail).


init_db(DbName, Filepath, Fd, Header0) ->
    Header1 = simple_upgrade_record(Header0, #db_header{}),
    Header =
    case element(2, Header1) of
    1 -> Header1#db_header{unused = 0, security_ptr = nil}; % 0.9
    2 -> Header1#db_header{unused = 0, security_ptr = nil}; % post 0.9 and pre 0.10
    3 -> Header1#db_header{security_ptr = nil}; % post 0.9 and pre 0.10
    4 -> Header1#db_header{security_ptr = nil}; % 0.10 and pre 0.11
    ?LATEST_DISK_VERSION -> Header1;
    _ -> throw({database_disk_version_error, "Incorrect disk header version"})
    end,

    {ok, FsyncOptions} = couch_util:parse_term(
            couch_config:get("couchdb", "fsync_options",
                    "[before_header, after_header, on_file_open]")),

    case lists:member(on_file_open, FsyncOptions) of
    true -> ok = couch_file:sync(Filepath);
    _ -> ok
    end,

    {ok, IdBtree} = couch_btree:open(Header#db_header.id_tree_state, Fd,
        [{split, fun ?MODULE:btree_by_id_split/1},
         {join, fun ?MODULE:btree_by_id_join/2},
         {reduce, fun ?MODULE:btree_by_id_reduce/2}]),
    {ok, SeqBtree} = couch_btree:open(Header#db_header.seq_tree_state, Fd,
        [{split, fun ?MODULE:btree_by_seq_split/1},
         {join, fun ?MODULE:btree_by_seq_join/2},
         {reduce, fun ?MODULE:btree_by_seq_reduce/2}]),
    {ok, LocalDocsBtree} = couch_btree:open(Header#db_header.local_tree_state, Fd),
    case Header#db_header.security_ptr of
    nil ->
        Security = [],
        SecurityPtr = nil;
    SecurityPtr ->
        {ok, Security} = couch_file:pread_term(Fd, SecurityPtr)
    end,
    % convert start time tuple to microsecs and store as a binary string
    {MegaSecs, Secs, MicroSecs} = now(),
    StartTime = ?l2b(io_lib:format("~p",
            [(MegaSecs*1000000*1000000) + (Secs*1000000) + MicroSecs])),
    #db{
        fd=Fd,
        fd_monitor = erlang:monitor(process,Fd),
        header=Header,
        id_tree = IdBtree,
        seq_tree = SeqBtree,
        local_tree = LocalDocsBtree,
        committed_update_seq = Header#db_header.update_seq,
        update_seq = Header#db_header.update_seq,
        name = DbName,
        filepath = Filepath,
        security = Security,
        security_ptr = SecurityPtr,
        instance_start_time = StartTime,
        revs_limit = Header#db_header.revs_limit,
        fsync_options = FsyncOptions
        }.


close_db(#db{fd_monitor = Ref}) ->
    erlang:demonitor(Ref).


refresh_validate_doc_funs(Db) ->
    {ok, DesignDocs} = couch_db:get_design_docs(Db),
    ProcessDocFuns = lists:flatmap(
        fun(DesignDoc) ->
            case couch_doc:get_validate_doc_fun(DesignDoc) of
            nil -> [];
            Fun -> [Fun]
            end
        end, DesignDocs),
    case Db#db.name of
    <<"shards/", _/binary>> = ShardName ->
        fabric:reset_validation_funs(mem3:dbname(ShardName)),
        Db#db{validate_doc_funs=undefined};
    _ ->
        Db#db{validate_doc_funs=ProcessDocFuns}
    end.

% rev tree functions

flush_trees(_Db, [], AccFlushedTrees) ->
    {ok, lists:reverse(AccFlushedTrees)};
flush_trees(#db{fd=Fd,header=Header}=Db,
        [InfoUnflushed | RestUnflushed], AccFlushed) ->
    #full_doc_info{update_seq=UpdateSeq, rev_tree=Unflushed} = InfoUnflushed,
    Flushed = couch_key_tree:map(
        fun(_Rev, Value) ->
            case Value of
            #doc{atts=Atts,deleted=IsDeleted}=Doc ->
                % this node value is actually an unwritten document summary,
                % write to disk.
                % make sure the Fd in the written bins is the same Fd we are
                % and convert bins, removing the FD.
                % All bins should have been written to disk already.
                {DiskAtts, SizeInfo} =
                case Atts of
                [] -> {[],[]};
                [#att{data={BinFd, _Sp}} | _ ] when BinFd == Fd ->
                    {[{N,T,P,AL,DL,R,M,E}
                        || #att{name=N,type=T,data={_,P},md5=M,revpos=R,
                               att_len=AL,disk_len=DL,encoding=E}
                        <- Atts],
                     [{P1,AL1}
                        || #att{data={_,P1},att_len=AL1}
                        <- Atts]};
                _ ->
                    % BinFd must not equal our Fd. This can happen when a database
                    % is being switched out during a compaction
                    ?LOG_DEBUG("File where the attachments are written has"
                            " changed. Possibly retrying.", []),
                    throw(retry)
                end,
                {ok, NewSummaryPointer} =
                case Header#db_header.disk_version < 4 of
                true ->
                    {ok, _} = couch_file:append_term(Fd, {Doc#doc.body, DiskAtts});
                false ->
                    {ok, _} = couch_file:append_term_md5(Fd, {Doc#doc.body, DiskAtts})
                end,
                #leaf{
                    deleted = IsDeleted,
                    ptr = NewSummaryPointer,
                    seq = UpdateSeq,
                    size = size(term_to_binary(Doc#doc.body)),
                    atts = SizeInfo
                };
            _ ->
                Value
            end
        end, Unflushed),
    flush_trees(Db, RestUnflushed, [InfoUnflushed#full_doc_info{rev_tree=Flushed} | AccFlushed]).


send_result(Client, Id, OriginalRevs, NewResult) ->
    % used to send a result to the client
    catch(Client ! {result, self(), {{Id, OriginalRevs}, NewResult}}).

merge_rev_trees(_Limit, _Merge, [], [], AccNewInfos, AccRemoveSeqs, AccSeq) ->
    {ok, lists:reverse(AccNewInfos), AccRemoveSeqs, AccSeq};
merge_rev_trees(Limit, MergeConflicts, [NewDocs|RestDocsList],
        [OldDocInfo|RestOldInfo], AccNewInfos, AccRemoveSeqs, AccSeq) ->
    #full_doc_info{id=Id,rev_tree=OldTree,deleted=OldDeleted,update_seq=OldSeq}
            = OldDocInfo,
    NewRevTree = lists:foldl(
        fun({Client, #doc{revs={Pos,[_Rev|PrevRevs]}}=NewDoc}, AccTree) ->
            if not MergeConflicts ->
                case couch_key_tree:merge(AccTree, couch_db:doc_to_tree(NewDoc),
                    Limit) of
                {_NewTree, conflicts} when (not OldDeleted) ->
                    send_result(Client, Id, {Pos-1,PrevRevs}, conflict),
                    AccTree;
                {NewTree, conflicts} when PrevRevs /= [] ->
                    % Check to be sure if prev revision was specified, it's
                    % a leaf node in the tree
                    Leafs = couch_key_tree:get_all_leafs(AccTree),
                    IsPrevLeaf = lists:any(fun({_, {LeafPos, [LeafRevId|_]}}) ->
                            {LeafPos, LeafRevId} == {Pos-1, hd(PrevRevs)}
                        end, Leafs),
                    if IsPrevLeaf ->
                        NewTree;
                    true ->
                        send_result(Client, Id, {Pos-1,PrevRevs}, conflict),
                        AccTree
                    end;
                {NewTree, no_conflicts} when  AccTree == NewTree ->
                    % the tree didn't change at all
                    % meaning we are saving a rev that's already
                    % been editted again.
                    if (Pos == 1) and OldDeleted ->
                        % this means we are recreating a brand new document
                        % into a state that already existed before.
                        % put the rev into a subsequent edit of the deletion
                        #doc_info{revs=[#rev_info{rev={OldPos,OldRev}}|_]} =
                                couch_doc:to_doc_info(OldDocInfo),
                        NewRevId = couch_db:new_revid(
                                NewDoc#doc{revs={OldPos, [OldRev]}}),
                        NewDoc2 = NewDoc#doc{revs={OldPos + 1, [NewRevId, OldRev]}},
                        {NewTree2, _} = couch_key_tree:merge(AccTree,
                                couch_db:doc_to_tree(NewDoc2), Limit),
                        % we changed the rev id, this tells the caller we did
                        send_result(Client, Id, {Pos-1,PrevRevs},
                                {ok, {OldPos + 1, NewRevId}}),
                        NewTree2;
                    true ->
                        send_result(Client, Id, {Pos-1,PrevRevs}, conflict),
                        AccTree
                    end;
                {NewTree, _} ->
                    NewTree
                end;
            true ->
                {NewTree, _} = couch_key_tree:merge(AccTree,
                            couch_db:doc_to_tree(NewDoc), Limit),
                NewTree
            end
        end,
        OldTree, NewDocs),
    if NewRevTree == OldTree ->
        % nothing changed
        merge_rev_trees(Limit, MergeConflicts, RestDocsList, RestOldInfo,
            AccNewInfos, AccRemoveSeqs, AccSeq);
    true ->
        % we have updated the document, give it a new seq #
        NewInfo = #full_doc_info{id=Id,update_seq=AccSeq+1,rev_tree=NewRevTree},
        RemoveSeqs = case OldSeq of
            0 -> AccRemoveSeqs;
            _ -> [OldSeq | AccRemoveSeqs]
        end,
        merge_rev_trees(Limit, MergeConflicts, RestDocsList, RestOldInfo,
            [NewInfo|AccNewInfos], RemoveSeqs, AccSeq+1)
    end.



new_index_entries([], Acc) ->
    Acc;
new_index_entries([Info|Rest], Acc) ->
    #doc_info{revs=[#rev_info{deleted=Del}|_]} = couch_doc:to_doc_info(Info),
    new_index_entries(Rest, [Info#full_doc_info{deleted=Del}|Acc]).


stem_full_doc_infos(#db{revs_limit=Limit}, DocInfos) ->
    [Info#full_doc_info{rev_tree=couch_key_tree:stem(Tree, Limit)} ||
            #full_doc_info{rev_tree=Tree}=Info <- DocInfos].

update_docs_int(Db, DocsList, NonRepDocs, MergeConflicts, FullCommit) ->
    #db{
        id_tree = DocInfoByIdBTree,
        seq_tree = DocInfoBySeqBTree,
        update_seq = LastSeq,
        revs_limit = RevsLimit
        } = Db,
    Ids = [Id || [{_Client, #doc{id=Id}}|_] <- DocsList],
    % lookup up the old documents, if they exist.
    OldDocLookups = couch_btree:lookup(DocInfoByIdBTree, Ids),
    OldDocInfos = lists:zipwith(
        fun(_Id, {ok, FullDocInfo}) ->
            FullDocInfo;
        (Id, not_found) ->
            #full_doc_info{id=Id}
        end,
        Ids, OldDocLookups),
    % Merge the new docs into the revision trees.
    {ok, NewFullDocInfos, RemoveSeqs, NewSeq} = merge_rev_trees(RevsLimit,
            MergeConflicts, DocsList, OldDocInfos, [], [], LastSeq),

    % All documents are now ready to write.

    {ok, Db2}  = update_local_docs(Db, NonRepDocs),

    % Write out the document summaries (the bodies are stored in the nodes of
    % the trees, the attachments are already written to disk)
    {ok, FlushedFullDocInfos} = flush_trees(Db2, NewFullDocInfos, []),
    IndexInfos =
        new_index_entries(compute_data_sizes(FlushedFullDocInfos, []),
                          []),
    % and the indexes
    {ok, DocInfoByIdBTree2} = couch_btree:add_remove(DocInfoByIdBTree,
        IndexInfos, []),
    {ok, DocInfoBySeqBTree2} = couch_btree:add_remove(DocInfoBySeqBTree,
        IndexInfos, RemoveSeqs),

    Db3 = Db2#db{
        id_tree = DocInfoByIdBTree2,
        seq_tree = DocInfoBySeqBTree2,
        update_seq = NewSeq},

    % Check if we just updated any design documents, and update the validation
    % funs if we did.
    case lists:any(
        fun(<<"_design/", _/binary>>) -> true; (_) -> false end, Ids) of
    false ->
        Db4 = Db3;
    true ->
        Db4 = refresh_validate_doc_funs(Db3)
    end,

    {ok, commit_data(Db4, not FullCommit)}.

compute_data_sizes([], Acc) ->
    lists:reverse(Acc);

compute_data_sizes([FullDocInfo | RestDocInfos], Acc) ->
    #full_doc_info{rev_tree=Tree} = FullDocInfo,
    Size = couch_key_tree:compute_data_size(Tree),
    compute_data_sizes(RestDocInfos,
                       [FullDocInfo#full_doc_info{data_size=Size}
                        | Acc]).



update_local_docs(Db, []) ->
    {ok, Db};
update_local_docs(#db{local_tree=Btree}=Db, Docs) ->
    Ids = [Id || {_Client, #doc{id=Id}} <- Docs],
    OldDocLookups = couch_btree:lookup(Btree, Ids),
    BtreeEntries = lists:zipwith(
        fun({Client, #doc{id=Id,deleted=Delete,revs={0,PrevRevs},body=Body}},
            _OldDocLookup) ->
            case PrevRevs of
            [RevStr|_] ->
                PrevRev = list_to_integer(?b2l(RevStr));
            [] ->
                PrevRev = 0
            end,
            %% disabled conflict checking for local docs -- APK 16 June 2010
            % OldRev =
            % case OldDocLookup of
            %     {ok, {_, {OldRev0, _}}} -> OldRev0;
            %     not_found -> 0
            % end,
            % case OldRev == PrevRev of
            % true ->
                case Delete of
                    false ->
                        send_result(Client, Id, {0, PrevRevs}, {ok,
                                {0, ?l2b(integer_to_list(PrevRev + 1))}}),
                        {update, {Id, {PrevRev + 1, Body}}};
                    true  ->
                        send_result(Client, Id, {0, PrevRevs},
                                {ok, {0, <<"0">>}}),
                        {remove, Id}
                end%;
            % false ->
            %     send_result(Client, Id, {0, PrevRevs}, conflict),
            %     ignore
            % end
        end, Docs, OldDocLookups),

    BtreeIdsRemove = [Id || {remove, Id} <- BtreeEntries],
    BtreeIdsUpdate = [{Key, Val} || {update, {Key, Val}} <- BtreeEntries],

    {ok, Btree2} =
        couch_btree:add_remove(Btree, BtreeIdsUpdate, BtreeIdsRemove),

    {ok, Db#db{local_tree = Btree2}}.


commit_data(Db) ->
    commit_data(Db, false).

db_to_header(Db, Header) ->
    Header#db_header{
        update_seq = Db#db.update_seq,
        seq_tree_state = couch_btree:get_state(Db#db.seq_tree),
        id_tree_state = couch_btree:get_state(Db#db.id_tree),
        local_tree_state = couch_btree:get_state(Db#db.local_tree),
        security_ptr = Db#db.security_ptr,
        revs_limit = Db#db.revs_limit}.

commit_data(#db{waiting_delayed_commit=nil} = Db, true) ->
    Db#db{waiting_delayed_commit=erlang:send_after(1000,self(),delayed_commit)};
commit_data(Db, true) ->
    Db;
commit_data(Db, _) ->
    #db{
        fd = Fd,
        filepath = Filepath,
        header = OldHeader,
        fsync_options = FsyncOptions,
        waiting_delayed_commit = Timer
    } = Db,
    if is_reference(Timer) -> erlang:cancel_timer(Timer); true -> ok end,
    case db_to_header(Db, OldHeader) of
    OldHeader ->
        Db#db{waiting_delayed_commit=nil};
    Header ->
        case lists:member(before_header, FsyncOptions) of
        true -> ok = couch_file:sync(Filepath);
        _    -> ok
        end,

        ok = couch_file:write_header(Fd, Header),

        case lists:member(after_header, FsyncOptions) of
        true -> ok = couch_file:sync(Filepath);
        _    -> ok
        end,

        Db#db{waiting_delayed_commit=nil,
            header=Header,
            committed_update_seq=Db#db.update_seq}
    end.


copy_doc_attachments(#db{fd=SrcFd}=SrcDb, {Pos,_RevId}, SrcSp, DestFd) ->
    {ok, {BodyData, BinInfos}} = couch_db:read_doc(SrcDb, SrcSp),
    % copy the bin values
    NewBinInfos = lists:map(
        fun({Name, {Type, BinSp, AttLen}}) when is_tuple(BinSp) orelse BinSp == null ->
            % 09 UPGRADE CODE
            {NewBinSp, AttLen, AttLen, Md5, _IdentityMd5} =
                couch_stream:old_copy_to_new_stream(SrcFd, BinSp, AttLen, DestFd),
            {Name, Type, NewBinSp, AttLen, AttLen, Pos, Md5, identity};
        ({Name, {Type, BinSp, AttLen}}) ->
            % 09 UPGRADE CODE
            {NewBinSp, AttLen, AttLen, Md5, _IdentityMd5} =
                couch_stream:copy_to_new_stream(SrcFd, BinSp, DestFd),
            {Name, Type, NewBinSp, AttLen, AttLen, Pos, Md5, identity};
        ({Name, Type, BinSp, AttLen, _RevPos, <<>>}) when
            is_tuple(BinSp) orelse BinSp == null ->
            % 09 UPGRADE CODE
            {NewBinSp, AttLen, AttLen, Md5, _IdentityMd5} =
                couch_stream:old_copy_to_new_stream(SrcFd, BinSp, AttLen, DestFd),
            {Name, Type, NewBinSp, AttLen, AttLen, AttLen, Md5, identity};
        ({Name, Type, BinSp, AttLen, RevPos, Md5}) ->
            % 010 UPGRADE CODE
            {NewBinSp, AttLen, AttLen, Md5, _IdentityMd5} =
                couch_stream:copy_to_new_stream(SrcFd, BinSp, DestFd),
            {Name, Type, NewBinSp, AttLen, AttLen, RevPos, Md5, identity};
        ({Name, Type, BinSp, AttLen, DiskLen, RevPos, Md5, Enc1}) ->
            {NewBinSp, AttLen, _, Md5, _IdentityMd5} =
                couch_stream:copy_to_new_stream(SrcFd, BinSp, DestFd),
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
            {Name, Type, NewBinSp, AttLen, DiskLen, RevPos, Md5, Enc}
        end, BinInfos),
    {BodyData, NewBinInfos}.

merge_lookups(Infos, []) ->
    Infos;
merge_lookups([], _) ->
    [];
merge_lookups([#doc_info{}|RestInfos], [{ok, FullDocInfo}|RestLookups]) ->
    [FullDocInfo|merge_lookups(RestInfos, RestLookups)];
merge_lookups([FullDocInfo|RestInfos], Lookups) ->
    [FullDocInfo|merge_lookups(RestInfos, Lookups)].

copy_docs(Db, #db{fd=DestFd}=NewDb, MixedInfos, Retry) ->
    % lookup any necessary full_doc_infos
    DocInfoIds = [Id || #doc_info{id=Id} <- MixedInfos],
    LookupResults = couch_btree:lookup(Db#db.id_tree, DocInfoIds),
    % COUCHDB-968, make sure we prune duplicates during compaction
    Infos = lists:usort(fun(#full_doc_info{id=A}, #full_doc_info{id=B}) ->
        A =< B
    end, merge_lookups(MixedInfos, LookupResults)),

    NewInfos1 = [Info#full_doc_info{rev_tree=couch_key_tree:map(
        fun(Rev, #leaf{ptr=Sp, size=Size0}=Leaf, leaf) ->
            {Body, AttInfos} = copy_doc_attachments(Db, Rev, Sp, DestFd),
            {ok, Pos} = couch_file:append_term_md5(DestFd, {Body, AttInfos}),
            if Size0 > 0 ->
                Leaf#leaf{ptr=Pos};
            true ->
                DocSize = byte_size(term_to_binary(Body)),
                AttSizes = [{element(3,A), element(4,A)} || A <- AttInfos],
                Leaf#leaf{ptr=Pos, size=DocSize, atts=AttSizes}
            end;
        (_, _, branch) ->
            ?REV_MISSING
        end, RevTree)} || #full_doc_info{rev_tree=RevTree}=Info <- Infos],

    NewInfos = stem_full_doc_infos(Db, compute_data_sizes(NewInfos1, [])),
    RemoveSeqs =
    case Retry of
    false ->
        [];
    true ->
        % We are retrying a compaction, meaning the documents we are copying may
        % already exist in our file and must be removed from the by_seq index.
        Ids = [Id || #full_doc_info{id=Id} <- Infos],
        Existing = couch_btree:lookup(NewDb#db.id_tree, Ids),
        [Seq || {ok, #full_doc_info{update_seq=Seq}} <- Existing]
    end,

    {ok, SeqTree} = couch_btree:add_remove(
            NewDb#db.seq_tree, NewInfos, RemoveSeqs),
    {ok, IdTree} = couch_btree:add_remove(
            NewDb#db.id_tree, NewInfos, []),
    NewDb#db{id_tree=IdTree, seq_tree=SeqTree}.



copy_compact(Db, NewDb0, Retry) ->
    FsyncOptions = [Op || Op <- NewDb0#db.fsync_options, Op == before_header],
    NewDb = NewDb0#db{fsync_options=FsyncOptions},
    TotalChanges = couch_db:count_changes_since(Db, NewDb#db.update_seq),
    EnumBySeqFun =
    fun(DocInfo, _Offset, {AccNewDb, AccUncopied, TotalCopied}) ->
        case DocInfo of
        #full_doc_info{update_seq=Seq} ->
            ok;
        #doc_info{high_seq=Seq} ->
            ok
        end,
        couch_task_status:update("Copied ~p of ~p changes (~p%)",
                [TotalCopied, TotalChanges, (TotalCopied*100) div TotalChanges]),
        if TotalCopied rem 1000 =:= 0 ->
            NewDb2 = copy_docs(Db, AccNewDb, lists:reverse([DocInfo | AccUncopied]), Retry),
            if TotalCopied rem 10000 =:= 0 ->
                NewDb3 = commit_data(NewDb2#db{update_seq=Seq}),
                {ok, {NewDb3, [], TotalCopied + 1}};
            true ->
                {ok, {NewDb2#db{update_seq=Seq}, [], TotalCopied + 1}}
            end;
        true ->
            {ok, {AccNewDb, [DocInfo | AccUncopied], TotalCopied + 1}}
        end
    end,

    couch_task_status:set_update_frequency(500),

    {ok, _, {NewDb2, Uncopied, TotalChanges}} =
        couch_btree:foldl(Db#db.seq_tree, EnumBySeqFun,
            {NewDb, [], 0},
            [{start_key, NewDb#db.update_seq + 1}]),

    couch_task_status:update("Flushing"),

    NewDb3 = copy_docs(Db, NewDb2, lists:reverse(Uncopied), Retry),

    % copy misc header values
    if NewDb3#db.security /= Db#db.security ->
        {ok, Ptr} = couch_file:append_term(NewDb3#db.fd, Db#db.security),
        NewDb4 = NewDb3#db{security=Db#db.security, security_ptr=Ptr};
    true ->
        NewDb4 = NewDb3
    end,

    commit_data(NewDb4#db{update_seq=Db#db.update_seq}).

start_copy_compact(#db{name=Name,filepath=Filepath,header=#db_header{purge_seq=PurgeSeq}}=Db) ->
    CompactFile = Filepath ++ ".compact",
    ?LOG_DEBUG("Compaction process spawned for db \"~s\"", [Name]),
    case couch_file:open(CompactFile) of
    {ok, Fd} ->
        couch_task_status:add_task(<<"Database Compaction">>, <<Name/binary, " retry">>, <<"Starting">>),
        Retry = true,
        case couch_file:read_header(Fd) of
        {ok, Header} ->
            ok;
        no_valid_header ->
            ok = couch_file:write_header(Fd, Header=#db_header{})
        end;
    {error, enoent} ->
        couch_task_status:add_task(<<"Database Compaction">>, Name, <<"Starting">>),
        {ok, Fd} = couch_file:open(CompactFile, [create]),
        Retry = false,
        ok = couch_file:write_header(Fd, Header=#db_header{})
    end,
    NewDb = init_db(Name, CompactFile, Fd, Header),
    NewDb2 = if PurgeSeq > 0 ->
        {ok, PurgedIdsRevs} = couch_db:get_last_purged(Db),
        {ok, Pointer} = couch_file:append_term(Fd, PurgedIdsRevs),
        NewDb#db{header=Header#db_header{purge_seq=PurgeSeq, purged_docs=Pointer}};
    true ->
        NewDb
    end,
    unlink(Fd),

    NewDb3 = copy_compact(Db, NewDb2, Retry),
    close_db(NewDb3),
    gen_server:cast(Db#db.main_pid, {compact_done, CompactFile}).

