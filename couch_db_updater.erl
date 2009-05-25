% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_db_updater).
-behaviour(gen_server).

-export([btree_by_id_reduce/2,btree_by_seq_reduce/2]).
-export([less_docid/2]).
-export([init/1,terminate/2,handle_call/3,handle_cast/2,code_change/3,handle_info/2]).

-include("couch_db.hrl").


init({MainPid, DbName, Filepath, Fd, Options}) ->
    case lists:member(create, Options) of
    true ->
        % create a new header and writes it to the file
        Header =  #db_header{},
        ok = couch_file:write_header(Fd, Header),
        % delete any old compaction files that might be hanging around
        file:delete(Filepath ++ ".compact");
    false ->
        ok = couch_file:upgrade_old_header(Fd, <<$g, $m, $k, 0>>), % 09 UPGRADE CODE
        {ok, Header} = couch_file:read_header(Fd)
    end,
    
    Db = init_db(DbName, Filepath, Fd, Header),
    Db2 = refresh_validate_doc_funs(Db),
    {ok, Db2#db{main_pid=MainPid}}.


terminate(Reason, _Srv) ->
    couch_util:terminate_linked(Reason),
    ok.

handle_call(get_db, _From, Db) ->
    {reply, {ok, Db}, Db};
handle_call({update_docs, DocActions, Options}, _From, Db) ->
    try update_docs_int(Db, DocActions, Options) of
    {ok, Conflicts, Db2} ->
        ok = gen_server:call(Db#db.main_pid, {db_updated, Db2}),
        couch_db_update_notifier:notify({updated, Db2#db.name}),
        {reply, {ok, Conflicts}, Db2}
    catch
        throw: retry ->
            {reply, retry, Db}
    end;
handle_call(full_commit, _From, #db{waiting_delayed_commit=nil}=Db) ->
    {reply, ok, Db}; % no data waiting, return ok immediately
handle_call(full_commit, _From,  Db) ->
    {reply, ok, commit_data(Db)}; % commit the data and return ok
handle_call(increment_update_seq, _From, Db) ->
    Db2 = commit_data(Db#db{update_seq=Db#db.update_seq+1}),
    ok = gen_server:call(Db2#db.main_pid, {db_updated, Db2}),
    couch_db_update_notifier:notify({updated, Db#db.name}),
    {reply, {ok, Db2#db.update_seq}, Db2};

handle_call({set_admins, NewAdmins}, _From, Db) ->
    {ok, Ptr} = couch_file:append_term(Db#db.fd, NewAdmins),
    Db2 = commit_data(Db#db{admins=NewAdmins, admins_ptr=Ptr,
            update_seq=Db#db.update_seq+1}),
    ok = gen_server:call(Db2#db.main_pid, {db_updated, Db2}),
    {reply, ok, Db2};

handle_call({set_revs_limit, Limit}, _From, Db) ->
    Db2 = commit_data(Db#db{revs_limit=Limit,
            update_seq=Db#db.update_seq+1}),
    ok = gen_server:call(Db2#db.main_pid, {db_updated, Db2}),
    {reply, ok, Db2};

handle_call({purge_docs, _IdRevs}, _From,
        #db{compactor_pid=Pid}=Db) when Pid /= nil ->
    {reply, {error, purge_during_compaction}, Db};
handle_call({purge_docs, IdRevs}, _From, Db) ->
    #db{
        fd=Fd,
        fulldocinfo_by_id_btree = DocInfoByIdBTree,
        docinfo_by_seq_btree = DocInfoBySeqBTree,
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
            Tree2 = couch_key_tree:map_leafs( fun(RevInfo) ->
                    RevInfo#rev_info{seq=SeqAcc + 1}
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
            fulldocinfo_by_id_btree = DocInfoByIdBTree2,
            docinfo_by_seq_btree = DocInfoBySeqBTree2,
            update_seq = NewSeq + 1,
            header=Header#db_header{purge_seq=PurgeSeq+1, purged_docs=Pointer}}),
    
    ok = gen_server:call(Db2#db.main_pid, {db_updated, Db2}),
    couch_db_update_notifier:notify({updated, Db#db.name}),
    {reply, {ok, Db2#db.update_seq, IdRevsPurged}, Db2}.
    

handle_cast(start_compact, Db) ->
    case Db#db.compactor_pid of
    nil ->
        ?LOG_INFO("Starting compaction for db \"~s\"", [Db#db.name]),
        Pid = spawn_link(fun() -> start_copy_compact(Db) end),
        Db2 = Db#db{compactor_pid=Pid},
        ok = gen_server:call(Db#db.main_pid, {db_updated, Db2}),
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
        {ok, LocalDocs} = couch_btree:foldl(Db#db.local_docs_btree,
                fun(Value, _Offset, Acc) -> {ok, [Value | Acc]} end, []),
        {ok, NewLocalBtree} = couch_btree:add(NewDb#db.local_docs_btree, LocalDocs),
        
        NewDb2 = commit_data( NewDb#db{local_docs_btree=NewLocalBtree,
                main_pid = Db#db.main_pid,filepath = Filepath}),
            
        ?LOG_DEBUG("CouchDB swapping files ~s and ~s.",
                [Filepath, CompactFilepath]),
        file:delete(Filepath),
        ok = file:rename(CompactFilepath, Filepath),
        close_db(Db),
        ok = gen_server:call(Db#db.main_pid, {db_updated, NewDb2}),
        ?LOG_INFO("Compaction for db \"~s\" completed.", [Db#db.name]),
        {noreply, NewDb2#db{compactor_pid=nil}};
    false ->
        ?LOG_INFO("Compaction file still behind main file "
            "(update seq=~p. compact update seq=~p). Retrying.",
            [Db#db.update_seq, NewSeq]),
        close_db(NewDb),
        Pid = spawn_link(fun() -> start_copy_compact(Db) end),
        Db2 = Db#db{compactor_pid=Pid},
        {noreply, Db2}
    end.

handle_info(delayed_commit, Db) ->
    {noreply, commit_data(Db)}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


btree_by_seq_split(#doc_info{id=Id, high_seq=KeySeq, revs=Revs}) ->
    RevInfos = [{Rev, Seq, Bp} ||  
        #rev_info{rev=Rev,seq=Seq,deleted=false,body_sp=Bp} <- Revs],
    DeletedRevInfos = [{Rev, Seq, Bp} ||  
        #rev_info{rev=Rev,seq=Seq,deleted=true,body_sp=Bp} <- Revs],
    {KeySeq,{Id, RevInfos, DeletedRevInfos}}.
    
btree_by_seq_join(KeySeq, {Id, RevInfos, DeletedRevInfos}) ->
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
        deleted=Deleted, rev_tree=Tree}) ->
    DiskTree =
    couch_key_tree:map(
        fun(_RevId, {IsDeleted, BodyPointer, UpdateSeq}) ->
            {if IsDeleted -> 1; true -> 0 end, BodyPointer, UpdateSeq};
        (_RevId, ?REV_MISSING) ->
            ?REV_MISSING
        end, Tree),
    {Id, {Seq, if Deleted -> 1; true -> 0 end, DiskTree}}.

btree_by_id_join(Id, {HighSeq, Deleted, DiskTree}) ->
    Tree =
    couch_key_tree:map(
        fun(_RevId, {IsDeleted, BodyPointer, UpdateSeq}) ->
            {IsDeleted == 1, BodyPointer, UpdateSeq};
        (_RevId, ?REV_MISSING) ->
            ?REV_MISSING;
        (_RevId, {IsDeleted, BodyPointer}) ->
            % 09 UPGRADE CODE
            % this is the 0.9.0 and earlier rev info record. It's missing the seq
            % nums, which means couchdb will sometimes reexamine unchanged
            % documents with the _changes API.
            % This is fixed by compacting the database.
            {IsDeleted, BodyPointer, HighSeq}
        end, DiskTree),
    
    #full_doc_info{id=Id, update_seq=HighSeq, deleted=Deleted==1, rev_tree=Tree}.

btree_by_id_reduce(reduce, FullDocInfos) ->
    % count the number of not deleted documents
    {length([1 || #full_doc_info{deleted=false} <- FullDocInfos]),
        length([1 || #full_doc_info{deleted=true} <- FullDocInfos])};
btree_by_id_reduce(rereduce, Reds) ->
    {lists:sum([Count || {Count,_} <- Reds]),
        lists:sum([DelCount || {_, DelCount} <- Reds])}.
            
btree_by_seq_reduce(reduce, DocInfos) ->
    % count the number of documents
    length(DocInfos);
btree_by_seq_reduce(rereduce, Reds) ->
    lists:sum(Reds).

simple_upgrade_record(Old, New) when size(Old) == size(New)->
    Old;
simple_upgrade_record(Old, New) ->
    NewValuesTail =
        lists:sublist(tuple_to_list(New), size(Old) + 1, size(New)-size(Old)),
    list_to_tuple(tuple_to_list(Old) ++ NewValuesTail).

% used for doc insertion, also for the PassedEndFun on all_docs view
less_docid(A, B) when A==B -> false;
less_docid(nil, _) -> true; % nil - special key sorts before all
less_docid({}, _) -> false; % {} -> special key sorts after all
less_docid(A, B) -> A < B.


init_db(DbName, Filepath, Fd, Header0) ->
    Header1 = simple_upgrade_record(Header0, #db_header{}),
    Header =
    case element(2, Header1) of
    1 -> Header1#db_header{unused = 0}; % 0.9
    2 -> Header1#db_header{unused = 0}; % post 0.9 and pre 0.10
    ?LATEST_DISK_VERSION -> Header1;
    _ -> throw({database_disk_version_error, "Incorrect disk header version"})
    end,
    Less = fun less_docid/2,
        
    {ok, FsyncOptions} = couch_util:parse_term(
            couch_config:get("couchdb", "fsync_options", 
                    "[before_header, after_header, on_file_open]")),
    
    case lists:member(on_file_open, FsyncOptions) of
    true -> ok = couch_file:sync(Fd);
    _ -> ok
    end,
        
    {ok, IdBtree} = couch_btree:open(Header#db_header.fulldocinfo_by_id_btree_state, Fd,
        [{split, fun(X) -> btree_by_id_split(X) end},
        {join, fun(X,Y) -> btree_by_id_join(X,Y) end},
        {reduce, fun(X,Y) -> btree_by_id_reduce(X,Y) end},
        {less, Less}]),
    {ok, SeqBtree} = couch_btree:open(Header#db_header.docinfo_by_seq_btree_state, Fd,
            [{split, fun(X) -> btree_by_seq_split(X) end},
            {join, fun(X,Y) -> btree_by_seq_join(X,Y) end},
            {reduce, fun(X,Y) -> btree_by_seq_reduce(X,Y) end}]),
    {ok, LocalDocsBtree} = couch_btree:open(Header#db_header.local_docs_btree_state, Fd),
    case Header#db_header.admins_ptr of
    nil ->
        Admins = [],
        AdminsPtr = nil;
    AdminsPtr ->
        {ok, Admins} = couch_file:pread_term(Fd, AdminsPtr)
    end,
    % convert start time tuple to microsecs and store as a binary string
    {MegaSecs, Secs, MicroSecs} = now(),
    StartTime = ?l2b(io_lib:format("~p",
            [(MegaSecs*1000000*1000000) + (Secs*1000000) + MicroSecs])),
    {ok, RefCntr} = couch_ref_counter:start([Fd]),
    #db{
        update_pid=self(),
        fd=Fd,
        fd_ref_counter = RefCntr,
        header=Header,
        fulldocinfo_by_id_btree = IdBtree,
        docinfo_by_seq_btree = SeqBtree,
        local_docs_btree = LocalDocsBtree,
        committed_update_seq = Header#db_header.update_seq,
        update_seq = Header#db_header.update_seq,
        name = DbName,
        filepath = Filepath,
        admins = Admins,
        admins_ptr = AdminsPtr,
        instance_start_time = StartTime,
        revs_limit = Header#db_header.revs_limit,
        fsync_options = FsyncOptions
        }.


close_db(#db{fd_ref_counter = RefCntr}) ->
    couch_ref_counter:drop(RefCntr).
    

refresh_validate_doc_funs(Db) ->
    {ok, DesignDocs} = get_design_docs(Db),
    ProcessDocFuns = lists:flatmap(
        fun(DesignDoc) ->
            case couch_doc:get_validate_doc_fun(DesignDoc) of
            nil -> [];
            Fun -> [Fun]
            end
        end, DesignDocs),
    Db#db{validate_doc_funs=ProcessDocFuns}.

get_design_docs(#db{fulldocinfo_by_id_btree=Btree}=Db) ->
    couch_btree:foldl(Btree, <<"_design/">>,
        fun(#full_doc_info{id= <<"_design/",_/binary>>}=FullDocInfo, _Reds, AccDocs) ->
            {ok, Doc} = couch_db:open_doc_int(Db, FullDocInfo, []),
            {ok, [Doc | AccDocs]};
        (_, _Reds, AccDocs) ->
            {stop, AccDocs}
        end,
        []).

% rev tree functions

flush_trees(_Db, [], AccFlushedTrees) ->
    {ok, lists:reverse(AccFlushedTrees)};
flush_trees(#db{fd=Fd}=Db, [InfoUnflushed | RestUnflushed], AccFlushed) ->
    #full_doc_info{update_seq=UpdateSeq, rev_tree=Unflushed} = InfoUnflushed,
    Flushed = couch_key_tree:map(
        fun(_Rev, Value) ->
            case Value of
            #doc{attachments=Atts,deleted=IsDeleted}=Doc ->
                % this node value is actually an unwritten document summary,
                % write to disk.
                % make sure the Fd in the written bins is the same Fd we are.
                Bins =
                case Atts of
                [] -> [];
                [{_BName, {_Type, {BinFd, _Sp, _Len}}} | _ ] when BinFd == Fd ->
                    % convert bins, removing the FD.
                    % All bins should have been flushed to disk already.
                    [{BinName, {BinType, BinSp, BinLen}}
                        || {BinName, {BinType, {_Fd, BinSp, BinLen}}}
                        <- Atts];
                _ ->
                    % BinFd must not equal our Fd. This can happen when a database
                    % is being switched out during a compaction
                    ?LOG_DEBUG("File where the attachments are written has changed. Possibly retrying.", []),
                    throw(retry)
                end,
                {ok, NewSummaryPointer} = couch_file:append_term(Fd, {Doc#doc.body, Bins}),
                {IsDeleted, NewSummaryPointer, UpdateSeq};
            _ ->
                Value
            end
        end, Unflushed),
    flush_trees(Db, RestUnflushed, [InfoUnflushed#full_doc_info{rev_tree=Flushed} | AccFlushed]).

merge_rev_trees(_MergeConflicts, [], [], AccNewInfos, AccRemoveSeqs, AccConflicts, AccSeq) ->
    {ok, lists:reverse(AccNewInfos), AccRemoveSeqs, AccConflicts, AccSeq};
merge_rev_trees(MergeConflicts, [NewDocs|RestDocsList],
        [OldDocInfo|RestOldInfo], AccNewInfos, AccRemoveSeqs, AccConflicts, AccSeq) ->
    #full_doc_info{id=Id,rev_tree=OldTree,deleted=OldDeleted,update_seq=OldSeq}
            = OldDocInfo,
    {NewRevTree, NewConflicts} = lists:foldl(
        fun(#doc{revs={Pos,[Rev|_]}}=NewDoc, {AccTree, AccConflicts2}) ->
            case couch_key_tree:merge(AccTree, [couch_db:doc_to_tree(NewDoc)]) of
            {_NewTree, conflicts}
                    when (not OldDeleted) and (not MergeConflicts) ->
                {AccTree, [{{Id, {Pos,Rev}}, conflict} | AccConflicts2]};
            {NewTree, _} ->
                {NewTree, AccConflicts2}
            end
        end,
        {OldTree, AccConflicts}, NewDocs),
    if NewRevTree == OldTree ->
        % nothing changed
        merge_rev_trees(MergeConflicts, RestDocsList, RestOldInfo, AccNewInfos,
                AccRemoveSeqs, NewConflicts, AccSeq);
    true ->
        % we have updated the document, give it a new seq #
        NewInfo = #full_doc_info{id=Id,update_seq=AccSeq+1,rev_tree=NewRevTree},
        RemoveSeqs = case OldSeq of
            0 -> AccRemoveSeqs;
            _ -> [OldSeq | AccRemoveSeqs]
        end,
        merge_rev_trees(MergeConflicts, RestDocsList, RestOldInfo, 
                [NewInfo|AccNewInfos], RemoveSeqs, NewConflicts, AccSeq+1)
    end.



new_index_entries([], AccById, AccBySeq) ->
    {AccById, AccBySeq};
new_index_entries([FullDocInfo|RestInfos], AccById, AccBySeq) ->
    #doc_info{revs=[#rev_info{deleted=Deleted}|_]} = DocInfo =
            couch_doc:to_doc_info(FullDocInfo),
    new_index_entries(RestInfos,
        [FullDocInfo#full_doc_info{deleted=Deleted}|AccById],
        [DocInfo|AccBySeq]).


stem_full_doc_infos(#db{revs_limit=Limit}, DocInfos) ->
    [Info#full_doc_info{rev_tree=couch_key_tree:stem(Tree, Limit)} ||
            #full_doc_info{rev_tree=Tree}=Info <- DocInfos].
        

update_docs_int(Db, DocsList, Options) ->
    #db{
        fulldocinfo_by_id_btree = DocInfoByIdBTree,
        docinfo_by_seq_btree = DocInfoBySeqBTree,
        update_seq = LastSeq
        } = Db,
    % separate out the NonRep documents from the rest of the documents
    {DocsList2, NonRepDocs} = lists:foldl(
        fun([#doc{id=Id}=Doc | _]=Docs, {DocsListAcc, NonRepDocsAcc}) ->
            case Id of
            <<?LOCAL_DOC_PREFIX, _/binary>> ->
                {DocsListAcc, [Doc | NonRepDocsAcc]};
            Id->
                {[Docs | DocsListAcc], NonRepDocsAcc}
            end
        end, {[], []}, DocsList),
    
    Ids = [Id || [#doc{id=Id}|_] <- DocsList2], 
    
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
    {ok, NewDocInfos0, RemoveSeqs, Conflicts, NewSeq} = merge_rev_trees(
            lists:member(merge_conflicts, Options),
            DocsList2, OldDocInfos, [], [], [], LastSeq),
    
    NewFullDocInfos = stem_full_doc_infos(Db, NewDocInfos0),
    
    % All documents are now ready to write.
    
    {ok, LocalConflicts, Db2}  = update_local_docs(Db, NonRepDocs),
    
    % Write out the document summaries (the bodies are stored in the nodes of
    % the trees, the attachments are already written to disk)
    {ok, FlushedFullDocInfos} = flush_trees(Db2, NewFullDocInfos, []),
    
    {IndexFullDocInfos, IndexDocInfos} = 
            new_index_entries(FlushedFullDocInfos, [], []),

    % and the indexes
    {ok, DocInfoByIdBTree2} = couch_btree:add_remove(DocInfoByIdBTree, IndexFullDocInfos, []),
    {ok, DocInfoBySeqBTree2} = couch_btree:add_remove(DocInfoBySeqBTree, IndexDocInfos, RemoveSeqs),

    Db3 = Db2#db{
        fulldocinfo_by_id_btree = DocInfoByIdBTree2,
        docinfo_by_seq_btree = DocInfoBySeqBTree2,
        update_seq = NewSeq},
    
    % Check if we just updated any design documents, and update the validation
    % funs if we did.
    case [1 || <<"_design/",_/binary>> <- Ids] of
    [] ->
        Db4 = Db3;
    _ ->
        Db4 = refresh_validate_doc_funs(Db3)
    end,
    
    {ok, LocalConflicts ++ Conflicts, 
            commit_data(Db4, not lists:member(full_commit, Options))}.


update_local_docs(#db{local_docs_btree=Btree}=Db, Docs) ->
    Ids = [Id || #doc{id=Id} <- Docs],
    OldDocLookups = couch_btree:lookup(Btree, Ids),
    BtreeEntries = lists:zipwith(
        fun(#doc{id=Id,deleted=Delete,revs={0,[RevStr]},body=Body}, OldDocLookup) ->
            NewRev = list_to_integer(?b2l(RevStr)),
            OldRev =
            case OldDocLookup of
                {ok, {_, {OldRev0, _}}} -> OldRev0;
                not_found -> 0
            end,
            case OldRev + 1 == NewRev of
            true ->
                case Delete of
                    false -> {update, {Id, {NewRev, Body}}};
                    true  -> {remove, Id}
                end;
            false ->
                {conflict, {Id, {0, RevStr}}}
            end
            
        end, Docs, OldDocLookups),

    BtreeIdsRemove = [Id || {remove, Id} <- BtreeEntries],
    BtreeIdsUpdate = [ByIdDocInfo || {update, ByIdDocInfo} <- BtreeEntries],
    Conflicts = [{conflict, IdRev} || {conflict, IdRev} <- BtreeEntries],
    
    {ok, Btree2} =
        couch_btree:add_remove(Btree, BtreeIdsUpdate, BtreeIdsRemove),

    {ok, Conflicts, Db#db{local_docs_btree = Btree2}}.


commit_data(Db) ->
    commit_data(Db, false).

db_to_header(Db, Header) ->
    Header#db_header{
        update_seq = Db#db.update_seq,
        docinfo_by_seq_btree_state = couch_btree:get_state(Db#db.docinfo_by_seq_btree),
        fulldocinfo_by_id_btree_state = couch_btree:get_state(Db#db.fulldocinfo_by_id_btree),
        local_docs_btree_state = couch_btree:get_state(Db#db.local_docs_btree),
        admins_ptr = Db#db.admins_ptr,
        revs_limit = Db#db.revs_limit}.

commit_data(#db{fd=Fd,header=OldHeader,fsync_options=FsyncOptions}=Db, Delay) ->
    Header = db_to_header(Db, OldHeader),
    if OldHeader == Header ->
        Db;
    Delay and (Db#db.waiting_delayed_commit == nil) ->
        Db#db{waiting_delayed_commit=
                erlang:send_after(1000, self(), delayed_commit)};
    Delay ->
        Db;
    true ->
        if Db#db.waiting_delayed_commit /= nil ->
            case erlang:cancel_timer(Db#db.waiting_delayed_commit) of
            false -> receive delayed_commit -> ok after 0 -> ok end;
            _ -> ok
            end;
        true -> ok
        end,
        case lists:member(before_header, FsyncOptions) of
        true -> ok = couch_file:sync(Fd);
        _    -> ok
        end,
        
        ok = couch_file:write_header(Fd, Header),
        
        case lists:member(after_header, FsyncOptions) of
        true -> ok = couch_file:sync(Fd);
        _    -> ok
        end,
        
        Db#db{waiting_delayed_commit=nil,
            header=Header,
            committed_update_seq=Db#db.update_seq}
    end.


copy_doc_attachments(SrcFd, SrcSp, DestFd) ->
    {ok, {BodyData, BinInfos}} = couch_db:read_doc(SrcFd, SrcSp),
    % copy the bin values
    NewBinInfos = lists:map(
        fun({Name, {Type, BinSp, Len}}) when is_tuple(BinSp) orelse BinSp == null ->
            % 09 UPGRADE CODE
            {NewBinSp, Len} = couch_stream:old_copy_to_new_stream(SrcFd, BinSp, Len, DestFd),
            {Name, {Type, NewBinSp, Len}};
        ({Name, {Type, BinSp, Len}}) ->
            {NewBinSp, Len} = couch_stream:copy_to_new_stream(SrcFd, BinSp, DestFd),
            {Name, {Type, NewBinSp, Len}}
        end, BinInfos),
    {BodyData, NewBinInfos}.

copy_rev_tree_attachments(_SrcFd, _DestFd, []) ->
    [];
copy_rev_tree_attachments(SrcFd, DestFd, [{Start, Tree} | RestTree]) ->
    % root nner node, only copy info/data from leaf nodes
    [Tree2] = copy_rev_tree_attachments(SrcFd, DestFd, [Tree]),
    [{Start, Tree2} | copy_rev_tree_attachments(SrcFd, DestFd, RestTree)];
copy_rev_tree_attachments(SrcFd, DestFd, [{RevId, {IsDel, Sp, Seq}, []} | RestTree]) ->
    % This is a leaf node, copy it over
    DocBody = copy_doc_attachments(SrcFd, Sp, DestFd),
    [{RevId, {IsDel, DocBody, Seq}, []} | copy_rev_tree_attachments(SrcFd, DestFd, RestTree)];
copy_rev_tree_attachments(SrcFd, DestFd, [{RevId, _, SubTree} | RestTree]) ->
    % inner node, only copy info/data from leaf nodes
    [{RevId, ?REV_MISSING, copy_rev_tree_attachments(SrcFd, DestFd, SubTree)} | copy_rev_tree_attachments(SrcFd, DestFd, RestTree)].

    
copy_docs(#db{fd=SrcFd}=Db, #db{fd=DestFd}=NewDb, InfoBySeq, Retry) ->
    Ids = [Id || #doc_info{id=Id} <- InfoBySeq],
    LookupResults = couch_btree:lookup(Db#db.fulldocinfo_by_id_btree, Ids),
    
    % write out the attachments
    NewFullDocInfos0 = lists:map(
        fun({ok, #full_doc_info{rev_tree=RevTree}=Info}) ->
            Info#full_doc_info{rev_tree=copy_rev_tree_attachments(SrcFd, DestFd, RevTree)}
        end, LookupResults),
    % write out the docs
    % we do this in 2 stages so the docs are written out contiguously, making
    % view indexing and replication faster.
    NewFullDocInfos1 = lists:map(
        fun(#full_doc_info{rev_tree=RevTree}=Info) ->
            Info#full_doc_info{rev_tree=couch_key_tree:map_leafs(
                fun(_Key, {IsDel, DocBody, Seq}) ->
                    {ok, Pos} = couch_file:append_term(DestFd, DocBody),
                    {IsDel, Pos, Seq}
                end, RevTree)}
        end, NewFullDocInfos0),

    NewFullDocInfos = stem_full_doc_infos(Db, NewFullDocInfos1),
    NewDocInfos = [couch_doc:to_doc_info(Info) || Info <- NewFullDocInfos],
    RemoveSeqs =
    case Retry of
    false ->
        [];
    true ->
        % We are retrying a compaction, meaning the documents we are copying may
        % already exist in our file and must be removed from the by_seq index.
        Existing = couch_btree:lookup(NewDb#db.fulldocinfo_by_id_btree, Ids),
        [Seq || {ok, #full_doc_info{update_seq=Seq}} <- Existing]
    end,
        
    {ok, DocInfoBTree} = couch_btree:add_remove(
            NewDb#db.docinfo_by_seq_btree, NewDocInfos, RemoveSeqs),
    {ok, FullDocInfoBTree} = couch_btree:add_remove(
            NewDb#db.fulldocinfo_by_id_btree, NewFullDocInfos, []),
    NewDb#db{ fulldocinfo_by_id_btree=FullDocInfoBTree,
              docinfo_by_seq_btree=DocInfoBTree}.


          
copy_compact(Db, NewDb0, Retry) ->
    FsyncOptions = [Op || Op <- NewDb0#db.fsync_options, Op == before_header],
    NewDb = NewDb0#db{fsync_options=FsyncOptions},
    TotalChanges = couch_db:count_changes_since(Db, NewDb#db.update_seq),
    EnumBySeqFun =
    fun(#doc_info{high_seq=Seq}=DocInfo, _Offset, {AccNewDb, AccUncopied, TotalCopied}) ->
        couch_task_status:update("Copied ~p of ~p changes (~p%)", 
                [TotalCopied, TotalChanges, (TotalCopied*100) div TotalChanges]),
        if TotalCopied rem 1000 == 0 ->
            NewDb2 = copy_docs(Db, AccNewDb, lists:reverse([DocInfo | AccUncopied]), Retry),
            if TotalCopied rem 10000 == 0 ->
                {ok, {commit_data(NewDb2#db{update_seq=Seq}), [], TotalCopied + 1}};
            true ->
                {ok, {NewDb2#db{update_seq=Seq}, [], TotalCopied + 1}}
            end;
        true ->    
            {ok, {AccNewDb, [DocInfo | AccUncopied], TotalCopied + 1}}
        end
    end,
    
    couch_task_status:set_update_frequency(500),
     
    {ok, {NewDb2, Uncopied, TotalChanges}} =
        couch_btree:foldl(Db#db.docinfo_by_seq_btree, NewDb#db.update_seq + 1, EnumBySeqFun, {NewDb, [], 0}),
    
    couch_task_status:update("Flushing"), 
        
    NewDb3 = copy_docs(Db, NewDb2, lists:reverse(Uncopied), Retry),
    
    % copy misc header values
    if NewDb3#db.admins /= Db#db.admins ->
        {ok, Ptr} = couch_file:append_term(NewDb3#db.fd, Db#db.admins),
        NewDb4 = NewDb3#db{admins=Db#db.admins, admins_ptr=Ptr};
    true ->
        NewDb4 = NewDb3
    end,
    
    commit_data(NewDb4#db{update_seq=Db#db.update_seq}).

start_copy_compact(#db{name=Name,filepath=Filepath}=Db) ->
    CompactFile = Filepath ++ ".compact",
    ?LOG_DEBUG("Compaction process spawned for db \"~s\"", [Name]),
    case couch_file:open(CompactFile) of
    {ok, Fd} ->
        couch_task_status:add_task(<<"Database Compaction">>, <<Name/binary, " retry">>, <<"Starting">>),
        Retry = true,
        {ok, Header} = couch_file:read_header(Fd);
    {error, enoent} ->
        couch_task_status:add_task(<<"Database Compaction">>, Name, <<"Starting">>),
        {ok, Fd} = couch_file:open(CompactFile, [create]),
        Retry = false,
        ok = couch_file:write_header(Fd, Header=#db_header{})
    end,
    NewDb = init_db(Name, CompactFile, Fd, Header),
    NewDb2 = copy_compact(Db, NewDb, Retry),
    
    gen_server:cast(Db#db.update_pid, {compact_done, CompactFile}),
    close_db(NewDb2).
    
