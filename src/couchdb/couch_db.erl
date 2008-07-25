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

-module(couch_db).
-behaviour(gen_server).

-export([open/2,create/2,create/3,get_doc_info/2,start_compact/1]).
-export([save_docs/2, save_docs/3, get_db_info/1, update_doc/3, update_docs/2, update_docs/3]).
-export([delete_doc/3,open_doc/2,open_doc/3,enum_docs_since/4,enum_docs_since/5]).
-export([enum_docs/4,enum_docs/5, open_doc_revs/4, get_missing_revs/2]).
-export([enum_docs_since_reduce_to_count/1,enum_docs_reduce_to_count/1]).
-export([increment_update_seq/1]).
-export([start_update_loop/2]).
-export([init/1,terminate/2,handle_call/3,handle_cast/2,code_change/3,handle_info/2]).
-export([start_copy_compact_int/2]).

-export([btree_by_id_split/1,
            btree_by_id_join/2,
            btree_by_id_reduce/2,
            btree_by_seq_split/1,
            btree_by_seq_join/2,
            btree_by_seq_reduce/2]).

-include("couch_db.hrl").

-record(db_header,
    {write_version = 0,
     update_seq = 0,
     summary_stream_state = nil,
     fulldocinfo_by_id_btree_state = nil,
     docinfo_by_seq_btree_state = nil,
     local_docs_btree_state = nil,
     doc_count=0,
     doc_del_count=0
    }).

-record(db,
    {main_pid=nil,
    update_pid=nil,
    compactor_pid=nil,
    fd,
    header = #db_header{},
    summary_stream,
    fulldocinfo_by_id_btree,
    docinfo_by_seq_btree,
    local_docs_btree,
    update_seq,
    doc_count,
    doc_del_count,
    name,
    filepath
    }).

% small value used in revision trees to indicate the revision isn't stored
-define(REV_MISSING, []).

-define(HEADER_SIG, <<$g, $m, $k, 0>>).

start_link(DbName, Filepath, Options) ->
    catch start_link0(DbName, Filepath, Options).
        
start_link0(DbName, Filepath, Options) ->
     % first delete the old file previous compaction
    Fd = 
    case couch_file:open(Filepath, Options) of
    {ok, Fd0} ->
        Fd0;
    {error, enoent} ->
        % couldn't find file. is there a compact version? This can happen if
        % crashed during the file switch.
        case couch_file:open(Filepath ++ ".compact") of
        {ok, Fd0} ->
            ?LOG_INFO("Found ~s~s compaction file, using as primary storage.", [Filepath, ".compact"]),
            ok = file:rename(Filepath ++ ".compact", Filepath),
            Fd0;
        {error, enoent} ->
            throw({error, not_found})
        end;
    Else ->
        throw(Else)
    end,
    
    StartResult = gen_server:start_link(couch_db, {DbName, Filepath, Fd, Options}, []),
    unlink(Fd),
    case StartResult of
    {ok, _} ->
        % We successfully opened the db, delete old storage files if around
        case file:delete(Filepath ++ ".old") of
        ok ->
            ?LOG_INFO("Deleted old storage file ~s~s", [Filepath, ".old"]);
        {error, enoent} ->
            ok  % normal result
        end;
    _ ->
        ok
    end,
    StartResult.

%%% Interface functions %%%

create(Filepath, Options) ->
    create(Filepath, Filepath, Options).

create(DbName, Filepath, Options) when is_list(Options) ->
    start_link(DbName, Filepath, [create | Options]).

open(DbName, Filepath) ->
    start_link(DbName, Filepath, []).


% Compaction still needs work. Right now readers and writers can get an error 
% file compaction changeover. This doesn't need to be the case.
start_compact(MainPid) ->
    gen_server:cast(MainPid, start_compact).

delete_doc(MainPid, Id, Revisions) ->
    DeletedDocs = [#doc{id=Id, revs=[Rev], deleted=true} || Rev <- Revisions],
    {ok, [Result]} = update_docs(MainPid, DeletedDocs, []),
    {ok, Result}.

open_doc(MainPid, IdOrDocInfo) ->
    open_doc(MainPid, IdOrDocInfo, []).

open_doc(MainPid, Id, Options) ->
    case open_doc_int(get_db(MainPid), Id, Options) of
    {ok, #doc{deleted=true}=Doc} ->
        case lists:member(deleted, Options) of
        true ->
            {ok, Doc};
        false ->
            {not_found, deleted}
        end;
    Else ->
        Else
    end.

open_doc_revs(MainPid, Id, Revs, Options) ->
    [Result] = open_doc_revs_int(get_db(MainPid), [{Id, Revs}], Options),
    Result.

get_missing_revs(MainPid, IdRevsList) ->
    Ids = [Id1 || {Id1, _Revs} <- IdRevsList],
    FullDocInfoResults = get_full_doc_infos(MainPid, Ids),
    Results = lists:zipwith(
        fun({Id, Revs}, FullDocInfoResult) ->
            case FullDocInfoResult of
            {ok, #full_doc_info{rev_tree=RevisionTree}} ->
                {Id, couch_key_tree:find_missing(RevisionTree, Revs)};
            not_found ->
                {Id, Revs}
            end
        end,
        IdRevsList, FullDocInfoResults),
    % strip out the non-missing ids
    Missing = [{Id, Revs} || {Id, Revs} <- Results, Revs /= []],
    {ok, Missing}.

get_doc_info(Db, Id) ->
    case get_full_doc_info(Db, Id) of
    {ok, DocInfo} ->
        {ok, couch_doc:to_doc_info(DocInfo)};
    Else ->
        Else
    end.
    
%   returns {ok, DocInfo} or not_found
get_full_doc_info(Db, Id) ->
    [Result] = get_full_doc_infos(Db, [Id]),
    Result.


get_full_doc_infos(MainPid, Ids) when is_pid(MainPid) ->
    get_full_doc_infos(get_db(MainPid), Ids);
get_full_doc_infos(#db{}=Db, Ids) ->
    couch_btree:lookup(Db#db.fulldocinfo_by_id_btree, Ids).

increment_update_seq(MainPid) ->
    gen_server:call(MainPid, increment_update_seq).
        
        
get_db_info(MainPid) when is_pid(MainPid) ->
    get_db_info(get_db(MainPid));
get_db_info(Db) ->
    #db{fd=Fd,
        compactor_pid=Compactor,
        doc_count=Count,
        doc_del_count=DelCount,
        update_seq=SeqNum} = Db,
    {ok, Size} = couch_file:bytes(Fd),
    InfoList = [
        {doc_count, Count},
        {doc_del_count, DelCount},
        {update_seq, SeqNum},
        {compact_running, Compactor/=nil},
        {disk_size, Size}
        ],
    {ok, InfoList}.

update_doc(MainPid, Doc, Options) ->
    {ok, [NewRev]} = update_docs(MainPid, [Doc], Options),
    {ok, NewRev}.

update_docs(MainPid, Docs) ->
    update_docs(MainPid, Docs, []).
    
% group_alike_docs groups the sorted documents into sublist buckets, by id.
% ([DocA, DocA, DocB, DocC], []) -> [[DocA, DocA], [DocB], [DocC]]
group_alike_docs(Docs) ->
    Sorted = lists:sort(fun(#doc{id=A},#doc{id=B})-> A < B end, Docs),
    group_alike_docs(Sorted, []).

group_alike_docs([], Buckets) ->
    lists:reverse(Buckets);
group_alike_docs([Doc|Rest], []) ->
    group_alike_docs(Rest, [[Doc]]);
group_alike_docs([Doc|Rest], [Bucket|RestBuckets]) ->
    [#doc{id=BucketId}|_] = Bucket,
    case Doc#doc.id == BucketId of
    true ->
        % add to existing bucket
        group_alike_docs(Rest, [[Doc|Bucket]|RestBuckets]);
    false ->
        % add to new bucket
       group_alike_docs(Rest, [[Doc]|[Bucket|RestBuckets]])
    end.
    

prepare_doc_for_new_edit(Db, #doc{id=Id,revs=[NewRev|PrevRevs]}=Doc, OldFullDocInfo, LeafRevsDict) ->
    case PrevRevs of
    [PrevRev|_] ->
        case dict:find(PrevRev, LeafRevsDict) of
        {ok, {Deleted, Sp, DiskRevs}} ->
            case couch_doc:has_stubs(Doc) of
            true ->
                DiskDoc = make_doc(Db, Id, Deleted, Sp, DiskRevs),
                Doc2 = couch_doc:merge_stubs(Doc, DiskDoc),
                Doc2#doc{revs=[NewRev|DiskRevs]};
            false ->
                Doc#doc{revs=[NewRev|DiskRevs]}
            end;
        error ->
            throw(conflict)
        end;
    [] ->
        % new doc, and we have existing revs. 
        OldDocInfo = couch_doc:to_doc_info(OldFullDocInfo),
        if OldDocInfo#doc_info.deleted ->
            % existing doc is a deleton
            % allow this new doc to be a later revision.
            {_Deleted, _Sp, Revs} = dict:fetch(OldDocInfo#doc_info.rev, LeafRevsDict),
            Doc#doc{revs=[NewRev|Revs]};
        true ->
            throw(conflict)
        end
    end.

update_docs(MainPid, Docs, Options) ->
    % go ahead and generate the new revision ids for the documents.
    Docs2 = lists:map(
        fun(#doc{id=Id,revs=Revs}=Doc) ->
            case Id of
            ?LOCAL_DOC_PREFIX ++ _ ->
                Rev = case Revs of [] -> 0; [Rev0|_] -> list_to_integer(Rev0) end,
                Doc#doc{revs=[integer_to_list(Rev + 1)]};
            _ ->
                Doc#doc{revs=[integer_to_list(couch_util:rand32()) | Revs]}
            end
        end, Docs),
    NewRevs = [NewRev || #doc{revs=[NewRev|_]} <- Docs2],
    DocBuckets = group_alike_docs(Docs2),
    Ids = [Id || [#doc{id=Id}|_] <- DocBuckets],
    Db = get_db(MainPid),
    
    % lookup the doc by id and get the most recent
    
    ExistingDocs = get_full_doc_infos(Db, Ids),
    
    DocBuckets2 = lists:zipwith(
        fun(Bucket, not_found) ->
            % no existing revs, make sure no old revision is specified.
            [throw(conflict) || #doc{revs=[_NewRev, _OldRev | _]} <- Bucket],
            Bucket;
        (Bucket, {ok, #full_doc_info{rev_tree=OldRevTree}=OldFullDocInfo}) ->
            Leafs = couch_key_tree:get_all_leafs(OldRevTree),
            LeafRevsDict = dict:from_list([{Rev, {Deleted, Sp, Revs}} || {Rev, {Deleted, Sp}, Revs} <- Leafs]),
            [prepare_doc_for_new_edit(Db, Doc, OldFullDocInfo, LeafRevsDict) || Doc <- Bucket]
        end,
        DocBuckets, ExistingDocs),
    % flush unwritten binaries to disk.
    DocBuckets3 = [[doc_flush_binaries(Doc, Db#db.fd) || Doc <- Bucket] || Bucket <- DocBuckets2],

    case gen_server:call(MainPid, {update_docs, DocBuckets3, [new_edits | Options]}, infinity) of
    ok -> {ok, NewRevs};
    retry ->
        Db2 = get_db(MainPid),
        DocBuckets4 = [[doc_flush_binaries(Doc, Db2#db.fd) || Doc <- Bucket] || Bucket <- DocBuckets3],
        % We only retry once
        case gen_server:call(MainPid, {update_docs, DocBuckets4, [new_edits | Options]}, infinity) of
        ok -> {ok, NewRevs};
        Else -> throw(Else)
        end;
    Else->
        throw(Else)
    end.

save_docs(MainPid, Docs) ->
    save_docs(MainPid, Docs, []).

save_docs(MainPid, Docs, Options) ->
    % flush unwritten binaries to disk.
    Db = get_db(MainPid),
    DocBuckets = group_alike_docs(Docs),
    DocBuckets2 = [[doc_flush_binaries(Doc, Db#db.fd) || Doc <- Bucket] || Bucket <- DocBuckets],
    ok = gen_server:call(MainPid, {update_docs, DocBuckets2, Options}, infinity).


doc_flush_binaries(Doc, Fd) ->
    % calc size of binaries to write out
    Bins = Doc#doc.attachments,
    PreAllocSize = lists:foldl(
        fun(BinValue, SizeAcc) ->
            case BinValue of
            {_Key, {_Type, {Fd0, _StreamPointer, _Len}}} when Fd0 == Fd ->
                % already written to our file, nothing to write
                SizeAcc;
            {_Key, {_Type, {_OtherFd, _StreamPointer, Len}}} ->
                % written to a different file
                SizeAcc + Len;
            {_Key, {_Type, Bin}} when is_binary(Bin) ->
                SizeAcc + size(Bin)
            end
        end,
        0, Bins),

    {ok, OutputStream} = couch_stream:open(Fd),
    ok = couch_stream:ensure_buffer(OutputStream, PreAllocSize),

    NewBins = lists:map(
        fun({Key, {Type, BinValue}}) ->
            NewBinValue =
            case BinValue of
            {Fd0, StreamPointer, Len} when Fd0 == Fd ->
                % already written to our file, nothing to write
                {Fd, StreamPointer, Len};
            {OtherFd, StreamPointer, Len} ->
                % written to a different file (or a closed file
                % instance, which will cause an error)
                {ok, {NewStreamPointer, Len}, _EndSp} =
                couch_stream:foldl(OtherFd, StreamPointer, Len,
                    fun(Bin, {BeginPointer, SizeAcc}) ->
                        {ok, Pointer} = couch_stream:write(OutputStream, Bin),
                        case SizeAcc of
                        0 -> % this was the first write, record the pointer
                            {ok, {Pointer, size(Bin)}};
                        _ ->
                            {ok, {BeginPointer, SizeAcc  + size(Bin)}}
                        end
                    end,
                    {{0,0}, 0}),
                {Fd, NewStreamPointer, Len};
            Bin when is_binary(Bin) ->
                {ok, StreamPointer} = couch_stream:write(OutputStream, Bin),
                {Fd, StreamPointer, size(Bin)}
            end,
            {Key, {Type, NewBinValue}}
        end, Bins),

    {ok, _FinalPos} = couch_stream:close(OutputStream),

    Doc#doc{attachments = NewBins}.

enum_docs_since_reduce_to_count(Reds) ->
    couch_btree:final_reduce(fun btree_by_seq_reduce/2, Reds).

enum_docs_reduce_to_count(Reds) ->
    couch_btree:final_reduce(fun btree_by_id_reduce/2, Reds).

enum_docs_since(MainPid, SinceSeq, Direction, InFun, Ctx) ->
    Db = get_db(MainPid),
    couch_btree:fold(Db#db.docinfo_by_seq_btree, SinceSeq + 1, Direction, InFun, Ctx).

enum_docs_since(MainPid, SinceSeq, InFun, Acc) ->
    enum_docs_since(MainPid, SinceSeq, fwd, InFun, Acc).

enum_docs(MainPid, StartId, Direction, InFun, InAcc) ->
    Db = get_db(MainPid),
    couch_btree:fold(Db#db.fulldocinfo_by_id_btree, StartId, Direction, InFun, InAcc).

enum_docs(MainPid, StartId, InFun, Ctx) ->
    enum_docs(MainPid, StartId, fwd, InFun, Ctx).

% server functions

init(InitArgs) ->
    spawn_link(couch_db, start_update_loop, [self(), InitArgs]),
    receive
    {initialized, Db} ->
        {ok, Db}
    end.

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
    % count the number of deleted documents
    length([1 || #full_doc_info{deleted=false} <- FullDocInfos]);
btree_by_id_reduce(rereduce, Reds) ->
    lists:sum(Reds).
            
btree_by_seq_reduce(reduce, DocInfos) ->
    % count the number of deleted documents
    length(DocInfos);
btree_by_seq_reduce(rereduce, Reds) ->
    lists:sum(Reds).

init_db(DbName, Filepath, Fd, Header) ->
    {ok, SummaryStream} = couch_stream:open(Header#db_header.summary_stream_state, Fd),
    ok = couch_stream:set_min_buffer(SummaryStream, 10000),
    {ok, IdBtree} = couch_btree:open(Header#db_header.fulldocinfo_by_id_btree_state, Fd,
        [{split, fun btree_by_id_split/1},
        {join, fun btree_by_id_join/2},
        {reduce, fun btree_by_id_reduce/2}]),
    {ok, SeqBtree} = couch_btree:open(Header#db_header.docinfo_by_seq_btree_state, Fd,
            [{split, fun btree_by_seq_split/1},
            {join, fun btree_by_seq_join/2},
            {reduce, fun btree_by_seq_reduce/2}]),
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
        doc_count = Header#db_header.doc_count,
        doc_del_count = Header#db_header.doc_del_count,
        name = DbName,
        filepath=Filepath }.

close_db(#db{fd=Fd,summary_stream=Ss}) ->
    couch_file:close(Fd),
    couch_stream:close(Ss).
    
terminate(_Reason, Db) ->
    exit(Db#db.update_pid, kill).
    
handle_call({update_docs, DocActions, Options}, From, #db{update_pid=Updater}=Db) ->
    Updater ! {From, update_docs, DocActions, Options},
    {noreply, Db};
handle_call(increment_update_seq, From, #db{update_pid=Updater}=Db) ->
    Updater ! {From, increment_update_seq},
    {noreply, Db};
handle_call(get_db, _From, Db) ->
    {reply, {ok, Db}, Db};
handle_call({db_updated, NewDb}, _From, _OldDb) ->
    {reply, ok, NewDb}.


handle_cast(start_compact, #db{update_pid=Updater}=Db) ->
    Updater ! compact,
    {noreply, Db}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(Msg, Db) ->
    ?LOG_ERROR("Bad message received for db ~s: ~p", [Db#db.name, Msg]),
    exit({error, Msg}).


%%% Internal function %%%

start_update_loop(MainPid, {DbName, Filepath, Fd, Options}) ->
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
    Db2 = Db#db{main_pid=MainPid},
    MainPid ! {initialized, Db2},
    update_loop(Db2).
    
update_loop(#db{fd=Fd,name=Name,
            filepath=Filepath,
            main_pid=MainPid,
            update_seq=UpdateSeq}=Db) ->
    receive
    {OrigFrom, update_docs, DocActions, Options} ->
        case (catch update_docs_int(Db, DocActions, Options)) of
        {ok, Db2} ->
            ok = gen_server:call(MainPid, {db_updated, Db2}),
            gen_server:reply(OrigFrom, ok),
            couch_db_update_notifier:notify({updated, Name}),
            update_loop(Db2);
        retry ->
            gen_server:reply(OrigFrom, retry),
            update_loop(Db);
        conflict ->
            gen_server:reply(OrigFrom, conflict),
            update_loop(Db);
        Error ->
            exit(Error) % we crashed
        end;
    compact ->
        case Db#db.compactor_pid of
        nil ->
            ?LOG_INFO("Starting compaction for db \"~s\"", [Name]),
            Pid = spawn_link(couch_db, start_copy_compact_int, [Db, true]),
            Db2 = Db#db{compactor_pid=Pid},
            ok = gen_server:call(MainPid, {db_updated, Db2}),
            update_loop(Db2);
        _ ->
            update_loop(Db) % already started
        end;
    {compact_done, CompactFilepath} ->
        {ok, NewFd} = couch_file:open(CompactFilepath),
        {ok, NewHeader} = couch_file:read_header(NewFd, ?HEADER_SIG),
        #db{update_seq=NewSeq}= NewDb =
                init_db(Name, CompactFilepath, NewFd, NewHeader),
        case Db#db.update_seq == NewSeq of
        true ->
            NewDb2 = commit_data(
                NewDb#db{
                    main_pid = Db#db.main_pid,
                    doc_count = Db#db.doc_count,
                    doc_del_count = Db#db.doc_del_count,
                    filepath = Filepath}),
                
            ?LOG_DEBUG("CouchDB swapping files ~s and ~s.", [Filepath, CompactFilepath]),
            ok = file:rename(Filepath, Filepath ++ ".old"),
            ok = file:rename(CompactFilepath, Filepath),
            
            couch_stream:close(Db#db.summary_stream),
            % close file handle async.
            % wait 5 secs before closing, allowing readers to finish
            unlink(Fd),
            spawn_link(fun() ->
                receive after 5000 -> ok end,
                couch_file:close(Fd),
                file:delete(Filepath ++ ".old")
                end),
                
            ok = gen_server:call(MainPid, {db_updated, NewDb2}),
            ?LOG_INFO("Compaction for db ~p completed.", [Name]),
            update_loop(NewDb2#db{compactor_pid=nil});
        false ->
            ?LOG_INFO("Compaction file still behind main file "
                "(update seq=~p. compact update seq=~p). Retrying.",
                [Db#db.update_seq, NewSeq]),
            Pid = spawn_link(couch_db, start_copy_compact_int, [Db, false]),
            Db2 = Db#db{compactor_pid=Pid},
            couch_file:close(NewFd),
            update_loop(Db2)
        end;
    {OrigFrom, increment_update_seq} ->
        Db2 = commit_data(Db#db{update_seq=UpdateSeq+1}),
        ok = gen_server:call(MainPid, {db_updated, Db2}),
        gen_server:reply(OrigFrom, {ok, UpdateSeq+1}),
        couch_db_update_notifier:notify({updated, Name}),
        update_loop(Db2);
    Else ->
        ?LOG_ERROR("Unknown message received in db ~s:~p", [Db#db.name, Else]),
        exit({error, Else})
    end.

get_db(MainPid) ->
    {ok, Db} = gen_server:call(MainPid, get_db),
    Db.

open_doc_revs_int(Db, IdRevs, Options) ->
    Ids = [Id || {Id, _Revs} <- IdRevs],
    LookupResults = get_full_doc_infos(Db, Ids),
    lists:zipwith(
        fun({Id, Revs}, Lookup) ->
            case Lookup of
            {ok, #full_doc_info{rev_tree=RevTree}} ->
                {FoundRevs, MissingRevs} =
                case Revs of
                all ->
                    {couch_key_tree:get_all_leafs(RevTree), []};
                _ ->
                    case lists:member(latest, Options) of
                    true ->
                        couch_key_tree:get_key_leafs(RevTree, Revs);
                    false ->
                        couch_key_tree:get(RevTree, Revs)
                    end
                end,
                FoundResults =
                lists:map(fun({Rev, Value, FoundRevPath}) ->
                    case Value of
                    ?REV_MISSING ->
                        % we have the rev in our list but know nothing about it
                        {{not_found, missing}, Rev};
                    {IsDeleted, SummaryPtr} ->
                        {ok, make_doc(Db, Id, IsDeleted, SummaryPtr, FoundRevPath)}
                    end
                end, FoundRevs),
                Results = FoundResults ++ [{{not_found, missing}, MissingRev} || MissingRev <- MissingRevs],
                {ok, Results};
            not_found when Revs == all ->
                {ok, []};
            not_found ->
                {ok, [{{not_found, missing}, Rev} || Rev <- Revs]}
            end
        end,
        IdRevs, LookupResults).

open_doc_int(Db, ?LOCAL_DOC_PREFIX ++ _ = Id, _Options) ->
    case couch_btree:lookup(Db#db.local_docs_btree, [Id]) of
    [{ok, {_, {Rev, BodyData}}}] ->
        {ok, #doc{id=Id, revs=[integer_to_list(Rev)], body=BodyData}};
    [not_found] ->
        {not_found, missing}
    end;
open_doc_int(Db, #doc_info{id=Id,rev=Rev,deleted=IsDeleted,summary_pointer=Sp}=DocInfo, Options) ->
    Doc = make_doc(Db, Id, IsDeleted, Sp, [Rev]),
    {ok, Doc#doc{meta=doc_meta_info(DocInfo, [], Options)}};
open_doc_int(Db, #full_doc_info{id=Id,rev_tree=RevTree}=FullDocInfo, Options) ->
    #doc_info{deleted=IsDeleted,rev=Rev,summary_pointer=Sp} = DocInfo =
        couch_doc:to_doc_info(FullDocInfo),
    {[{_Rev,_Value, Revs}], []} = couch_key_tree:get(RevTree, [Rev]),
    Doc = make_doc(Db, Id, IsDeleted, Sp, Revs),
    {ok, Doc#doc{meta=doc_meta_info(DocInfo, RevTree, Options)}};
open_doc_int(Db, Id, Options) ->
    case get_full_doc_info(Db, Id) of
    {ok, FullDocInfo} ->
        open_doc_int(Db, FullDocInfo, Options);
    not_found ->
        throw({not_found, missing})
    end.

doc_meta_info(DocInfo, RevTree, Options) ->
    case lists:member(revs_info, Options) of
    false -> [];
    true ->
        {[RevPath],[]} = 
            couch_key_tree:get_full_key_paths(RevTree, [DocInfo#doc_info.rev]),
        [{revs_info, lists:map(
            fun({Rev, {true, _Sp}}) -> 
                {Rev, deleted};
            ({Rev, {false, _Sp}}) ->
                {Rev, available};
            ({Rev, ?REV_MISSING}) ->
                {Rev, missing}
            end, RevPath)}]
    end ++
    case lists:member(conflicts, Options) of
    false -> [];
    true ->
        case DocInfo#doc_info.conflict_revs of
        [] -> [];
        _ -> [{conflicts, DocInfo#doc_info.conflict_revs}]
        end
    end ++
    case lists:member(deleted_conflicts, Options) of
    false -> [];
    true ->
        case DocInfo#doc_info.deleted_conflict_revs of
        [] -> [];
        _ -> [{deleted_conflicts, DocInfo#doc_info.deleted_conflict_revs}]
        end
    end.

% rev tree functions

doc_to_tree(Doc) ->
    doc_to_tree(Doc, lists:reverse(Doc#doc.revs)).

doc_to_tree(Doc, [RevId]) ->
    [{RevId, Doc, []}];
doc_to_tree(Doc, [RevId | Rest]) ->
    [{RevId, ?REV_MISSING, doc_to_tree(Doc, Rest)}].

make_doc(Db, Id, Deleted, SummaryPointer, RevisionPath) ->
    {BodyData, BinValues} =
    case SummaryPointer of
    nil ->
        {[], []};
    _ ->
        {ok, {BodyData0, BinValues0}} = couch_stream:read_term(Db#db.summary_stream, SummaryPointer),
        {BodyData0, [{Name, {Type, {Db#db.fd, Sp, Len}}} || {Name, {Type, Sp, Len}} <- BinValues0]}   
    end,
    #doc{
        id = Id,
        revs = RevisionPath,
        body = BodyData,
        attachments = BinValues,
        deleted = Deleted
        }.

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
                    % is being updated during a compaction
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

new_index_entries([], DocCount, DelCount, AccById, AccBySeq) ->
    {ok, DocCount, DelCount, AccById, AccBySeq};
new_index_entries([FullDocInfo|RestInfos], DocCount, DelCount, AccById, AccBySeq) ->
    #doc_info{deleted=Deleted} = DocInfo = couch_doc:to_doc_info(FullDocInfo),
    {DocCount2, DelCount2} =
    if Deleted -> {DocCount, DelCount + 1};
    true -> {DocCount + 1, DelCount} 
    end,
    new_index_entries(RestInfos, DocCount2, DelCount2, 
        [FullDocInfo#full_doc_info{deleted=Deleted}|AccById],
        [DocInfo|AccBySeq]).

update_docs_int(Db, DocsList, Options) ->
    #db{
        fulldocinfo_by_id_btree = DocInfoByIdBTree,
        docinfo_by_seq_btree = DocInfoBySeqBTree,
        update_seq = LastSeq,
        doc_count = FullDocCount,
        doc_del_count = FullDelCount
        } = Db,

    % separate out the NonRep documents from the rest of the documents
    {DocsList2, NonRepDocs} = lists:foldl(
        fun([#doc{id=Id}=Doc | Rest]=Docs, {DocsListAcc, NonRepDocsAcc}) ->
            case Id of
            ?LOCAL_DOC_PREFIX ++ _ when Rest==[] ->
                % when saving NR (non rep) documents, you can only save a single rev
                {DocsListAcc, [Doc | NonRepDocsAcc]};
            Id->
                {[Docs | DocsListAcc], NonRepDocsAcc}
            end
        end, {[], []}, DocsList),
    
    Ids = [Id || [#doc{id=Id}|_] <- DocsList2], 
    
    % lookup up the existing documents, if they exist.
    OldDocLookups = couch_btree:lookup(DocInfoByIdBTree, Ids),
    OldDocInfos = lists:zipwith(
        fun(_Id, {ok, FullDocInfo}) ->
            FullDocInfo;
        (Id, not_found) ->
            #full_doc_info{id=Id}
        end,
        Ids, OldDocLookups),
    
    {OldCount, OldDelCount} = lists:foldl(
        fun({ok, FullDocInfo}, {OldCountAcc, OldDelCountAcc}) ->
            case couch_doc:to_doc_info(FullDocInfo) of
            #doc_info{deleted=false} ->
                {OldCountAcc + 1, OldDelCountAcc};
            _ ->
                {OldCountAcc, OldDelCountAcc + 1}
            end;
        (not_found, Acc) ->
            Acc
        end, {0, 0}, OldDocLookups),
    
    % Merge the new docs into the revision trees.
    NoConflicts = lists:member(new_edits, Options),
    {ok, NewDocInfos, NewSeq} = merge_rev_trees(NoConflicts, DocsList2, OldDocInfos, [], LastSeq),
    
    RemoveSeqs =
        [ OldSeq || {ok, #full_doc_info{update_seq=OldSeq}} <- OldDocLookups],
    
    % All regular documents are now ready to write.
    
    % Try to write the local documents first, a conflict might be generated
    {ok, Db2}  = update_local_docs(Db, NonRepDocs),
    
    % Write out the documents summaries (they are stored in the nodes of the rev trees)
    {ok, FlushedDocInfos} = flush_trees(Db2, NewDocInfos, []),
    
    {ok, NewDocsCount, NewDelCount, InfoById, InfoBySeq} =
        new_index_entries(FlushedDocInfos, 0, 0, [], []),

    % and the indexes to the documents
    {ok, DocInfoBySeqBTree2} = couch_btree:add_remove(DocInfoBySeqBTree, InfoBySeq, RemoveSeqs),
    {ok, DocInfoByIdBTree2} = couch_btree:add_remove(DocInfoByIdBTree, InfoById, []),

    Db3 = Db2#db{
        fulldocinfo_by_id_btree = DocInfoByIdBTree2,
        docinfo_by_seq_btree = DocInfoBySeqBTree2,
        update_seq = NewSeq,
        doc_count = FullDocCount + NewDocsCount - OldCount,
        doc_del_count = FullDelCount + NewDelCount - OldDelCount},

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
                [RevStr|_] -> list_to_integer(RevStr)
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
        local_docs_btree_state = couch_btree:get_state(Db#db.local_docs_btree),
        doc_count = Db#db.doc_count,
        doc_del_count = Db#db.doc_del_count
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
    
copy_docs(#db{fd=SrcFd}=Db, #db{fd=DestFd,summary_stream=DestStream}=NewDb, InfoBySeq) ->
    Ids = [Id || #doc_info{id=Id} <- InfoBySeq],
    LookupResults = couch_btree:lookup(Db#db.fulldocinfo_by_id_btree, Ids),
    NewFullDocInfos = lists:map(
        fun({ok, #full_doc_info{rev_tree=RevTree}=Info}) ->
            Info#full_doc_info{rev_tree=copy_rev_tree(SrcFd, DestFd, DestStream, RevTree)}
        end, LookupResults),
    NewDocInfos = [couch_doc:to_doc_info(FullDocInfo) || FullDocInfo <- NewFullDocInfos],
    {ok, DocInfoBTree} =
        couch_btree:add_remove(NewDb#db.docinfo_by_seq_btree, NewDocInfos, []),
    {ok, FullDocInfoBTree} =
        couch_btree:add_remove(NewDb#db.fulldocinfo_by_id_btree, NewFullDocInfos, []),
    NewDb#db{fulldocinfo_by_id_btree=FullDocInfoBTree, docinfo_by_seq_btree=DocInfoBTree}.


          
copy_compact_docs(Db, NewDb) ->
    EnumBySeqFun =
    fun(#doc_info{update_seq=Seq}=DocInfo, _Offset, {AccNewDb, AccUncopied}) ->
        case couch_util:should_flush() of
        true ->
            NewDb2 = copy_docs(Db, AccNewDb, lists:reverse([DocInfo | AccUncopied])),
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
            lists:reverse(Uncopied)));
    [] ->
        NewDb2
    end.

start_copy_compact_int(#db{name=Name,filepath=Filepath}=Db, CopyLocal) ->
    CompactFile = Filepath ++ ".compact",
    ?LOG_DEBUG("Compaction process spawned for db \"~s\"", [Name]),
    case couch_file:open(CompactFile) of
    {ok, Fd} ->
        ?LOG_DEBUG("Found existing compaction file for db \"~s\"", [Name]),
        {ok, Header} = couch_file:read_header(Fd, ?HEADER_SIG);
    {error, enoent} -> %
        {ok, Fd} = couch_file:open(CompactFile, [create]),
        Header =  #db_header{},
        ok = couch_file:write_header(Fd, ?HEADER_SIG, Header)
    end,
    NewDb = init_db(Name, CompactFile, Fd, Header),
    NewDb2 = copy_compact_docs(Db, NewDb),
    NewDb3 =
    case CopyLocal of
    true ->
        % suck up all the local docs into memory and write them to the new db
        {ok, LocalDocs} = couch_btree:foldl(Db#db.local_docs_btree,
                fun(Value, _Offset, Acc) -> {ok, [Value | Acc]} end, []),
        {ok, NewLocalBtree} = couch_btree:add(NewDb2#db.local_docs_btree, LocalDocs),
        commit_data(NewDb2#db{local_docs_btree=NewLocalBtree});
    _ ->
        NewDb2
    end,
    close_db(NewDb3),
    Db#db.update_pid ! {compact_done, CompactFile}.
    
    
    