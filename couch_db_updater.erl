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
-export([init/1,terminate/2,handle_call/3,handle_cast/2,code_change/3,handle_info/2]).

-include("couch_db.hrl").

-define(HEADER_SIG, <<$g, $m, $k, 0>>).

init({MainPid, DbName, Filepath, Fd, Options}) ->
    link(Fd),
    case lists:member(create, Options) of
    true ->
        % create a new header and writes it to the file
        Header =  #db_header{},
        ok = couch_file:write_header(Fd, ?HEADER_SIG, Header),
        % delete any old compaction files that might be hanging around
        file:delete(Filepath ++ ".compact"),
        file:delete(Filepath ++ ".old");
    false ->
        {ok, Header} = couch_file:read_header(Fd, ?HEADER_SIG)
    end,
    
    Db = init_db(DbName, Filepath, Fd, Header),
    {ok, Db#db{main_pid=MainPid}}.

terminate(_Reason, Db) ->
    close_db(Db).

handle_call(get_db, _From, Db) ->
    {reply, {ok, Db}, Db};
handle_call({update_docs, DocActions, Options}, _From, Db) ->
    try update_docs_int(Db, DocActions, Options) of
    {ok, Db2} ->
        ok = gen_server:call(Db#db.main_pid, {db_updated, Db2}),
        couch_db_update_notifier:notify({updated, Db2#db.name}),
        {reply, ok, Db2}
    catch
        throw: retry ->
            {reply, retry, Db};
        throw: conflict ->
            {reply, conflict, Db}
    end;
handle_call(increment_update_seq, _From, Db) ->
    Db2 = commit_data(Db#db{update_seq=Db#db.update_seq+1}),
    ok = gen_server:call(Db2#db.main_pid, {db_updated, Db2}),
    couch_db_update_notifier:notify({updated, Db#db.name}),
    {reply, {ok, Db2#db.update_seq}, Db2};

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
        fun(FullInfo, SeqAcc) ->
            Info = couch_doc:to_doc_info(FullInfo),
            {Info#doc_info{update_seq=SeqAcc + 1}, SeqAcc + 1}
        end, LastSeq, FullDocInfoToUpdate),
    
    IdsToRemove = [Id || {#full_doc_info{id=Id,rev_tree=Tree},_}
            <- NewDocInfos, Tree == []],
    
    {ok, DocInfoBySeqBTree2} = couch_btree:add_remove(DocInfoBySeqBTree,
            DocInfoToUpdate, SeqsToRemove),
    {ok, DocInfoByIdBTree2} = couch_btree:add_remove(DocInfoByIdBTree,
            FullDocInfoToUpdate, IdsToRemove),
    {ok, Pointer} = couch_file:append_term(Fd, IdRevsPurged),
    
    Db2 = commit_data(
        Db#db{
            fulldocinfo_by_id_btree = DocInfoByIdBTree2,
            docinfo_by_seq_btree = DocInfoBySeqBTree2,
            update_seq = NewSeq,
            header=Header#db_header{purge_seq=PurgeSeq+1, purged_docs=Pointer}}),
    
    ok = gen_server:call(Db2#db.main_pid, {db_updated, Db2}),
    couch_db_update_notifier:notify({updated, Db#db.name}),
    {reply, {ok, Db2#db.update_seq, IdRevsPurged}, Db2}.
    

handle_cast(start_compact, Db) ->
    case Db#db.compactor_pid of
    nil ->
        ?LOG_INFO("Starting compaction for db \"~s\"", [Db#db.name]),
        Pid = spawn_link(fun() -> start_copy_compact_int(Db) end),
        Db2 = Db#db{compactor_pid=Pid},
        ok = gen_server:call(Db#db.main_pid, {db_updated, Db2}),
        {noreply, Db2};
    _ ->
        % compact currently running, this is a no-op
        {noreply, Db}
    end;
handle_cast({compact_done, CompactFilepath}, #db{filepath=Filepath}=Db) ->
    {ok, NewFd} = couch_file:open(CompactFilepath),
    {ok, NewHeader} = couch_file:read_header(NewFd, ?HEADER_SIG),
    #db{update_seq=NewSeq} = NewDb =
            init_db(Db#db.name, CompactFilepath, NewFd, NewHeader),
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
        
        couch_stream:close(Db#db.summary_stream),
        couch_file:close_maybe(Db#db.fd),
        file:delete(Filepath ++ ".old"),
            
        ok = gen_server:call(Db#db.main_pid, {db_updated, NewDb2}),
        ?LOG_INFO("Compaction for db \"~s\" completed.", [Db#db.name]),
        {noreply, NewDb2#db{compactor_pid=nil}};
    false ->
        ?LOG_INFO("Compaction file still behind main file "
            "(update seq=~p. compact update seq=~p). Retrying.",
            [Db#db.update_seq, NewSeq]),
        Pid = spawn_link(fun() -> start_copy_compact_int(Db) end),
        Db2 = Db#db{compactor_pid=Pid},
        couch_file:close(NewFd),
        {noreply, Db2}
    end.

handle_info(Msg, Db) ->
    ?LOG_ERROR("Bad message received for db ~s: ~p", [Db#db.name, Msg]),
    exit({error, Msg}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


btree_by_seq_split(DocInfo) ->
    #doc_info{
        id = Id,
        rev = Rev,
        update_seq = Seq,
        summary_pointer = Sp,
        conflict_revs = Conflicts,
        deleted_conflict_revs = DelConflicts,
        deleted = Deleted} = DocInfo,
    {Seq,{Id, Rev, Sp, Conflicts, DelConflicts, Deleted}}.
    
btree_by_seq_join(Seq,{Id, Rev, Sp, Conflicts, DelConflicts, Deleted}) ->
    #doc_info{
        id = Id,
        rev = Rev,
        update_seq = Seq,
        summary_pointer = Sp,
        conflict_revs = Conflicts,
        deleted_conflict_revs = DelConflicts,
        deleted = Deleted}.

btree_by_id_split(#full_doc_info{id=Id, update_seq=Seq,
        deleted=Deleted, rev_tree=Tree}) ->
    {Id, {Seq, case Deleted of true -> 1; false-> 0 end, Tree}}.

btree_by_id_join(Id, {Seq, Deleted, Tree}) ->
    #full_doc_info{id=Id, update_seq=Seq, deleted=Deleted==1, rev_tree=Tree}.

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

init_db(DbName, Filepath, Fd, Header) ->
    {ok, SummaryStream} = couch_stream:open(Header#db_header.summary_stream_state, Fd),
    ok = couch_stream:set_min_buffer(SummaryStream, 10000),
    Less =
        fun(A,B) when A==B -> false;
        (nil, _) -> true; % nil - special key sorts before all
        ({}, _) -> false; % {} -> special key sorts after all
        (A, B) -> A < B
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

    #db{
        update_pid=self(),
        fd=Fd,
        header=Header,
        summary_stream = SummaryStream,
        fulldocinfo_by_id_btree = IdBtree,
        docinfo_by_seq_btree = SeqBtree,
        local_docs_btree = LocalDocsBtree,
        update_seq = Header#db_header.update_seq,
        name = DbName,
        filepath=Filepath }.

close_db(#db{fd=Fd,summary_stream=Ss}) ->
    couch_file:close(Fd),
    couch_stream:close(Ss).

% rev tree functions

doc_to_tree(Doc) ->
    doc_to_tree(Doc, lists:reverse(Doc#doc.revs)).

doc_to_tree(Doc, [RevId]) ->
    [{RevId, Doc, []}];
doc_to_tree(Doc, [RevId | Rest]) ->
    [{RevId, ?REV_MISSING, doc_to_tree(Doc, Rest)}].

flush_trees(_Db, [], AccFlushedTrees) ->
    {ok, lists:reverse(AccFlushedTrees)};
flush_trees(#db{fd=Fd}=Db, [InfoUnflushed | RestUnflushed], AccFlushed) ->
        #full_doc_info{rev_tree=Unflushed} = InfoUnflushed,
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
                {ok, NewSummaryPointer} = couch_stream:write_term(Db#db.summary_stream, {Doc#doc.body, Bins}),
                {IsDeleted, NewSummaryPointer};
            _ ->
                Value
            end
        end, Unflushed),
    flush_trees(Db, RestUnflushed, [InfoUnflushed#full_doc_info{rev_tree=Flushed} | AccFlushed]).

merge_rev_trees(_NoConflicts, [], [], AccNewInfos, AccSeq) ->
    {ok, lists:reverse(AccNewInfos), AccSeq};
merge_rev_trees(NoConflicts, [NewDocs|RestDocsList],
        [OldDocInfo|RestOldInfo], AccNewInfos, AccSeq) ->
    #full_doc_info{id=Id,rev_tree=OldTree}=OldDocInfo,
    UpdatesRevTree = lists:foldl(
        fun(NewDoc, AccTree) ->
            couch_key_tree:merge(AccTree, doc_to_tree(NewDoc))
        end,
        [], NewDocs),
    NewRevTree = couch_key_tree:merge(OldTree, UpdatesRevTree),
    if NewRevTree == OldTree ->
        % nothing changed
        merge_rev_trees(NoConflicts, RestDocsList, RestOldInfo, AccNewInfos, AccSeq);
    true ->
        if NoConflicts andalso OldTree /= [] ->
            OldConflicts = couch_key_tree:count_leafs(OldTree),
            NewConflicts = couch_key_tree:count_leafs(NewRevTree),
            if NewConflicts > OldConflicts ->
                throw(conflict);
            true -> ok
            end;
        true -> ok
        end,
        NewInfo = #full_doc_info{id=Id,update_seq=AccSeq+1,rev_tree=NewRevTree},
        merge_rev_trees(NoConflicts, RestDocsList,RestOldInfo, 
                [NewInfo|AccNewInfos],AccSeq+1)
    end.

new_index_entries([], AccById, AccBySeq) ->
    {ok, AccById, AccBySeq};
new_index_entries([FullDocInfo|RestInfos], AccById, AccBySeq) ->
    #doc_info{deleted=Deleted} = DocInfo = couch_doc:to_doc_info(FullDocInfo),
    new_index_entries(RestInfos,
        [FullDocInfo#full_doc_info{deleted=Deleted}|AccById],
        [DocInfo|AccBySeq]).

update_docs_int(Db, DocsList, Options) ->
    #db{
        fulldocinfo_by_id_btree = DocInfoByIdBTree,
        docinfo_by_seq_btree = DocInfoBySeqBTree,
        update_seq = LastSeq
        } = Db,

    % separate out the NonRep documents from the rest of the documents
    {DocsList2, NonRepDocs} = lists:foldl(
        fun([#doc{id=Id}=Doc | Rest]=Docs, {DocsListAcc, NonRepDocsAcc}) ->
            case Id of
            <<?LOCAL_DOC_PREFIX, _/binary>> when Rest==[] ->
                % when saving NR (non rep) documents, you can only save a single rev
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
    NoConflicts = lists:member(new_edits, Options),
    {ok, NewDocInfos, NewSeq} = merge_rev_trees(NoConflicts, DocsList2, OldDocInfos, [], LastSeq),
    
    RemoveSeqs =
        [ OldSeq || {ok, #full_doc_info{update_seq=OldSeq}} <- OldDocLookups],
    
    % All regular documents are now ready to write.
    
    % Try to write the local documents first, a conflict might be generated
    {ok, Db2}  = update_local_docs(Db, NonRepDocs),
    
    % Write out the document summaries (they are stored in the nodes of the rev trees)
    {ok, FlushedDocInfos} = flush_trees(Db2, NewDocInfos, []),
    
    {ok, InfoById, InfoBySeq} = new_index_entries(FlushedDocInfos, [], []),

    % and the indexes to the documents
    {ok, DocInfoBySeqBTree2} = couch_btree:add_remove(DocInfoBySeqBTree, InfoBySeq, RemoveSeqs),
    {ok, DocInfoByIdBTree2} = couch_btree:add_remove(DocInfoByIdBTree, InfoById, []),

    Db3 = Db2#db{
        fulldocinfo_by_id_btree = DocInfoByIdBTree2,
        docinfo_by_seq_btree = DocInfoBySeqBTree2,
        update_seq = NewSeq},

    case lists:member(delay_commit, Options) of
    true ->
        {ok, Db3};
    false ->
        {ok, commit_data(Db3)}
    end.

update_local_docs(#db{local_docs_btree=Btree}=Db, Docs) ->
    Ids = [Id || #doc{id=Id} <- Docs],
    OldDocLookups = couch_btree:lookup(Btree, Ids),
    BtreeEntries = lists:zipwith(
        fun(#doc{id=Id,deleted=Delete,revs=Revs,body=Body}, OldDocLookup) ->
            NewRev =
            case Revs of
                [] -> 0;
                [RevStr|_] -> list_to_integer(binary_to_list(RevStr))
            end,
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
                throw(conflict)
            end
            
        end, Docs, OldDocLookups),

    BtreeIdsRemove = [Id || {remove, Id} <- BtreeEntries],
    BtreeIdsUpdate = [ByIdDocInfo || {update, ByIdDocInfo} <- BtreeEntries],

    {ok, Btree2} =
        couch_btree:add_remove(Btree, BtreeIdsUpdate, BtreeIdsRemove),

    {ok, Db#db{local_docs_btree = Btree2}}.



commit_data(#db{fd=Fd, header=Header} = Db) ->
    Header2 = Header#db_header{
        update_seq = Db#db.update_seq,
        summary_stream_state = couch_stream:get_state(Db#db.summary_stream),
        docinfo_by_seq_btree_state = couch_btree:get_state(Db#db.docinfo_by_seq_btree),
        fulldocinfo_by_id_btree_state = couch_btree:get_state(Db#db.fulldocinfo_by_id_btree),
        local_docs_btree_state = couch_btree:get_state(Db#db.local_docs_btree)
        },
    if Header == Header2 ->
        Db; % unchanged. nothing to do
    true ->
        ok = couch_file:write_header(Fd, ?HEADER_SIG, Header2),
        Db#db{header = Header2}
    end.

copy_raw_doc(SrcFd, SrcSp, DestFd, DestStream) ->
    {ok, {BodyData, BinInfos}} = couch_stream:read_term(SrcFd, SrcSp),
    % copy the bin values
    NewBinInfos = lists:map(fun({Name, {Type, BinSp, Len}}) ->
        {ok, NewBinSp} = couch_stream:copy_to_new_stream(SrcFd, BinSp, Len, DestFd),
        {Name, {Type, NewBinSp, Len}}
        end, BinInfos),
    % now write the document summary
    {ok, Sp} = couch_stream:write_term(DestStream, {BodyData, NewBinInfos}),
    Sp.

copy_rev_tree(_SrcFd, _DestFd, _DestStream, []) ->
    [];
copy_rev_tree(SrcFd, DestFd, DestStream, [{RevId, {IsDel, Sp}, []} | RestTree]) ->
    % This is a leaf node, copy it over
    NewSp = copy_raw_doc(SrcFd, Sp, DestFd, DestStream),
    [{RevId, {IsDel, NewSp}, []} | copy_rev_tree(SrcFd, DestFd, DestStream, RestTree)];
copy_rev_tree(SrcFd, DestFd, DestStream, [{RevId, _, SubTree} | RestTree]) ->
    % inner node, only copy info/data from leaf nodes
    [{RevId, ?REV_MISSING, copy_rev_tree(SrcFd, DestFd, DestStream, SubTree)} | copy_rev_tree(SrcFd, DestFd, DestStream, RestTree)].
    
copy_docs(#db{fd=SrcFd}=Db, #db{fd=DestFd,summary_stream=DestStream}=NewDb, InfoBySeq, Retry) ->
    Ids = [Id || #doc_info{id=Id} <- InfoBySeq],
    LookupResults = couch_btree:lookup(Db#db.fulldocinfo_by_id_btree, Ids),
    NewFullDocInfos = lists:map(
        fun({ok, #full_doc_info{rev_tree=RevTree}=Info}) ->
            Info#full_doc_info{rev_tree=copy_rev_tree(SrcFd, DestFd, DestStream, RevTree)}
        end, LookupResults),
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


          
copy_compact_docs(Db, NewDb, Retry) ->
    EnumBySeqFun =
    fun(#doc_info{update_seq=Seq}=DocInfo, _Offset, {AccNewDb, AccUncopied}) ->
        case couch_util:should_flush() of
        true ->
            NewDb2 = copy_docs(Db, AccNewDb, lists:reverse([DocInfo | AccUncopied]), Retry),
            {ok, {commit_data(NewDb2#db{update_seq=Seq}), []}};
        false ->    
            {ok, {AccNewDb, [DocInfo | AccUncopied]}}
        end
    end,
    {ok, {NewDb2, Uncopied}} =
        couch_btree:foldl(Db#db.docinfo_by_seq_btree, NewDb#db.update_seq + 1, EnumBySeqFun, {NewDb, []}),

    case Uncopied of
    [#doc_info{update_seq=LastSeq} | _] ->
        commit_data( copy_docs(Db, NewDb2#db{update_seq=LastSeq},
            lists:reverse(Uncopied), Retry));
    [] ->
        NewDb2
    end.

start_copy_compact_int(#db{name=Name,filepath=Filepath}=Db) ->
    CompactFile = Filepath ++ ".compact",
    ?LOG_DEBUG("Compaction process spawned for db \"~s\"", [Name]),
    case couch_file:open(CompactFile) of
    {ok, Fd} ->
        ?LOG_DEBUG("Found existing compaction file for db \"~s\"", [Name]),
        Retry = true,
        {ok, Header} = couch_file:read_header(Fd, ?HEADER_SIG);
    {error, enoent} ->
        {ok, Fd} = couch_file:open(CompactFile, [create]),
        Retry = false,
        ok = couch_file:write_header(Fd, ?HEADER_SIG, Header=#db_header{})
    end,
    NewDb = init_db(Name, CompactFile, Fd, Header),
    NewDb2 = copy_compact_docs(Db, NewDb, Retry),
    close_db(NewDb2),
    
    gen_server:cast(Db#db.update_pid, {compact_done, CompactFile}).
    
