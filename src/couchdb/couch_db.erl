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

-export([open/2,close/1,create/2,start_compact/1,get_db_info/1]).
-export([open_ref_counted/2,is_idle/1,monitor/1,count_changes_since/2]).
-export([update_doc/3,update_docs/4,update_docs/2,update_docs/3,delete_doc/3]).
-export([get_doc_info/2,open_doc/2,open_doc/3,open_doc_revs/4]).
-export([set_revs_limit/2,get_revs_limit/1]).
-export([get_missing_revs/2,name/1,doc_to_tree/1,get_update_seq/1,get_committed_update_seq/1]).
-export([enum_docs/4,enum_docs/5,enum_docs_since/4,enum_docs_since/5]).
-export([enum_docs_since_reduce_to_count/1,enum_docs_reduce_to_count/1]).
-export([increment_update_seq/1,get_purge_seq/1,purge_docs/2,get_last_purged/1]).
-export([start_link/3,make_doc/2,set_admins/2,get_admins/1,ensure_full_commit/1]).
-export([init/1,terminate/2,handle_call/3,handle_cast/2,code_change/3,handle_info/2]).

-include("couch_db.hrl").


start_link(DbName, Filepath, Options) ->
    case open_db_file(Filepath, Options) of
    {ok, Fd} ->
        StartResult = gen_server:start_link(couch_db, {DbName, Filepath, Fd, Options}, []),
        unlink(Fd),
        StartResult;
    Else ->
        Else
    end.

open_db_file(Filepath, Options) ->
    case couch_file:open(Filepath, Options) of
    {ok, Fd} ->
        {ok, Fd};
    {error, enoent} ->
        % couldn't find file. is there a compact version? This can happen if
        % crashed during the file switch.
        case couch_file:open(Filepath ++ ".compact") of
        {ok, Fd} ->
            ?LOG_INFO("Found ~s~s compaction file, using as primary storage.", [Filepath, ".compact"]),
            ok = file:rename(Filepath ++ ".compact", Filepath),
            {ok, Fd};
        {error, enoent} ->
            not_found
        end;
    Error ->
        Error
    end.


create(DbName, Options) ->
    couch_server:create(DbName, Options).

open(DbName, Options) ->
    couch_server:open(DbName, Options).

ensure_full_commit(#db{update_pid=UpdatePid,instance_start_time=StartTime}) ->
    ok = gen_server:call(UpdatePid, full_commit, infinity),
    {ok, StartTime}.

close(#db{fd_ref_counter=RefCntr}) ->
    couch_ref_counter:drop(RefCntr).

open_ref_counted(MainPid, UserCtx) ->
    {ok, Db} = gen_server:call(MainPid, {open_ref_count, self()}),
    {ok, Db#db{user_ctx=UserCtx}}.

is_idle(MainPid) ->
    gen_server:call(MainPid, is_idle).

monitor(#db{main_pid=MainPid}) ->
    erlang:monitor(process, MainPid).

start_compact(#db{update_pid=Pid}) ->
    gen_server:cast(Pid, start_compact).

delete_doc(Db, Id, Revisions) ->
    DeletedDocs = [#doc{id=Id, revs=[Rev], deleted=true} || Rev <- Revisions],
    {ok, [Result]} = update_docs(Db, DeletedDocs, []),
    {ok, Result}.

open_doc(Db, IdOrDocInfo) ->
    open_doc(Db, IdOrDocInfo, []).

open_doc(Db, Id, Options) ->
    couch_stats_collector:increment({couchdb, database_reads}),
    case open_doc_int(Db, Id, Options) of
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

open_doc_revs(Db, Id, Revs, Options) ->
    couch_stats_collector:increment({couchdb, database_reads}),
    [Result] = open_doc_revs_int(Db, [{Id, Revs}], Options),
    Result.

get_missing_revs(Db, IdRevsList) ->
    Ids = [Id1 || {Id1, _Revs} <- IdRevsList],
    FullDocInfoResults = get_full_doc_infos(Db, Ids),
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

get_full_doc_infos(Db, Ids) ->
    couch_btree:lookup(Db#db.fulldocinfo_by_id_btree, Ids).

increment_update_seq(#db{update_pid=UpdatePid}) ->
    gen_server:call(UpdatePid, increment_update_seq).

purge_docs(#db{update_pid=UpdatePid}, IdsRevs) ->
    gen_server:call(UpdatePid, {purge_docs, IdsRevs}).
    
get_committed_update_seq(#db{header=#db_header{update_seq=Seq}}) ->
    Seq.

get_update_seq(#db{update_seq=Seq})->
    Seq.
    
get_purge_seq(#db{header=#db_header{purge_seq=PurgeSeq}})->
    PurgeSeq.

get_last_purged(#db{header=#db_header{purged_docs=nil}}) ->
    {ok, []};
get_last_purged(#db{fd=Fd, header=#db_header{purged_docs=PurgedPointer}}) ->
    couch_file:pread_term(Fd, PurgedPointer).

get_db_info(Db) ->
    #db{fd=Fd,
        compactor_pid=Compactor,
        update_seq=SeqNum,
        name=Name,
        fulldocinfo_by_id_btree=FullDocBtree,
        instance_start_time=StartTime} = Db,
    {ok, Size} = couch_file:bytes(Fd),
    {ok, {Count, DelCount}} = couch_btree:full_reduce(FullDocBtree),
    InfoList = [
        {db_name, Name},
        {doc_count, Count},
        {doc_del_count, DelCount},
        {update_seq, SeqNum},
        {purge_seq, couch_db:get_purge_seq(Db)},
        {compact_running, Compactor/=nil},
        {disk_size, Size},
        {instance_start_time, StartTime}
        ],
    {ok, InfoList}.

check_is_admin(#db{admins=Admins, user_ctx=#user_ctx{name=Name,roles=Roles}}) ->
    DbAdmins = [<<"_admin">> | Admins],
    case DbAdmins -- [Name | Roles] of
    DbAdmins -> % same list, not an admin
        throw({unauthorized, <<"You are not a db or server admin.">>});
    _ ->
        ok
    end.

get_admins(#db{admins=Admins}) ->
    Admins.

set_admins(#db{update_pid=Pid}=Db, Admins) when is_list(Admins) ->
    check_is_admin(Db),
    gen_server:call(Pid, {set_admins, Admins}, infinity).


get_revs_limit(#db{revs_limit=Limit}) ->
    Limit.

set_revs_limit(#db{update_pid=Pid}=Db, Limit) when Limit > 0 ->
    check_is_admin(Db),
    gen_server:call(Pid, {set_revs_limit, Limit}, infinity);
set_revs_limit(_Db, _Limit) ->
    throw(invalid_revs_limit).

name(#db{name=Name}) ->
    Name.
    
update_doc(Db, Doc, Options) ->
    case update_docs(Db, [Doc], Options) of
    {ok, [{ok, NewRev}]} ->
        {ok, NewRev};
    {ok, [Error]} ->
        throw(Error)
    end.

update_docs(Db, Docs) ->
    update_docs(Db, Docs, []).
    
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


validate_doc_update(#db{user_ctx=UserCtx, admins=Admins},
        #doc{id= <<"_design/",_/binary>>}, _GetDiskDocFun) ->
    UserNames = [UserCtx#user_ctx.name | UserCtx#user_ctx.roles],
    % if the user is a server admin or db admin, allow the save
    case length(UserNames -- [<<"_admin">> | Admins]) == length(UserNames) of
    true ->
        % not an admin
        {unauthorized, <<"You are not a server or database admin.">>};
    false ->
        ok
    end;
validate_doc_update(#db{validate_doc_funs=[]}, _Doc, _GetDiskDocFun) ->
    ok;
validate_doc_update(_Db, #doc{id= <<"_local/",_/binary>>}, _GetDiskDocFun) ->
    ok;
validate_doc_update(#db{name=DbName,user_ctx=Ctx}=Db, Doc, GetDiskDocFun) ->
    DiskDoc = GetDiskDocFun(),
    JsonCtx =  {[{<<"db">>, DbName},
            {<<"name">>,Ctx#user_ctx.name},
            {<<"roles">>,Ctx#user_ctx.roles}]},
    try [case Fun(Doc, DiskDoc, JsonCtx) of
            ok -> ok;
            Error -> throw(Error)
        end || Fun <- Db#db.validate_doc_funs],
        ok
    catch
        throw:Error ->
            Error
    end.


prep_and_validate_update(Db, #doc{id=Id,revs={RevStart, [_NewRev|PrevRevs]}}=Doc,
        OldFullDocInfo, LeafRevsDict) ->
    case PrevRevs of
    [PrevRev|_] ->
        case dict:find({RevStart-1, PrevRev}, LeafRevsDict) of
        {ok, {Deleted, DiskSp, DiskRevs}} ->
            case couch_doc:has_stubs(Doc) of
            true ->
                DiskDoc = make_doc(Db, Id, Deleted, DiskSp, DiskRevs),
                Doc2 = couch_doc:merge_stubs(Doc, DiskDoc),
                {validate_doc_update(Db, Doc2, fun() -> DiskDoc end), Doc2};
            false ->
                LoadDiskDoc = fun() -> make_doc(Db,Id,Deleted,DiskSp,DiskRevs) end,
                {validate_doc_update(Db, Doc, LoadDiskDoc), Doc}
            end;
        error ->
            {conflict, Doc}
        end;
    [] ->
        % new doc, and we have existing revs.
        if OldFullDocInfo#full_doc_info.deleted ->
            % existing docs are deletions
            {validate_doc_update(Db, Doc, fun() -> nil end), Doc};
        true ->
            {conflict, Doc}
        end
    end.



prep_and_validate_updates(_Db, [], [], AccPrepped, AccFatalErrors) ->
   {AccPrepped, AccFatalErrors};
prep_and_validate_updates(Db, [DocBucket|RestBuckets], [not_found|RestLookups], AccPrepped, AccErrors) ->
    [#doc{id=Id}|_]=DocBucket,
    % no existing revs are known,
    {PreppedBucket, AccErrors3} = lists:foldl(
        fun(#doc{revs=Revs}=Doc, {AccBucket, AccErrors2}) ->
            case Revs of
            {Pos, [NewRev,_OldRev|_]} ->
                % old revs specified but none exist, a conflict
                {AccBucket, [{{Id, {Pos, NewRev}}, conflict} | AccErrors2]};
            {Pos, [NewRev]} ->
                case validate_doc_update(Db, Doc, fun() -> nil end) of
                ok ->
                    {[Doc | AccBucket], AccErrors2};
                Error ->
                    {AccBucket, [{{Id, {Pos, NewRev}}, Error} | AccErrors2]}
                end
            end
        end,
        {[], AccErrors}, DocBucket),

    prep_and_validate_updates(Db, RestBuckets, RestLookups,
            [PreppedBucket | AccPrepped], AccErrors3);
prep_and_validate_updates(Db, [DocBucket|RestBuckets],
        [{ok, #full_doc_info{rev_tree=OldRevTree}=OldFullDocInfo}|RestLookups],
        AccPrepped, AccErrors) ->
    Leafs = couch_key_tree:get_all_leafs(OldRevTree),
    LeafRevsDict = dict:from_list([{{Start, RevId}, {Deleted, Sp, Revs}} ||
            {{Deleted, Sp}, {Start, [RevId|_]}=Revs} <- Leafs]),
    {PreppedBucket, AccErrors3} = lists:foldl(
        fun(Doc, {Docs2Acc, AccErrors2}) ->
            case prep_and_validate_update(Db, Doc, OldFullDocInfo,
                    LeafRevsDict) of
            {ok, Doc2} ->
                {[Doc2 | Docs2Acc], AccErrors2};
            {Error, #doc{id=Id,revs={Pos, [NewRev|_]}}} ->
                % Record the error
                {Docs2Acc, [{{Id, {Pos, NewRev}}, Error} |AccErrors2]}
            end
        end,
        {[], AccErrors}, DocBucket),
    prep_and_validate_updates(Db, RestBuckets, RestLookups, [PreppedBucket | AccPrepped], AccErrors3).


update_docs(#db{update_pid=UpdatePid}=Db, Docs, Options) ->
    update_docs(#db{update_pid=UpdatePid}=Db, Docs, Options, interactive_edit).


prep_and_validate_replicated_updates(_Db, [], [], AccPrepped, AccErrors) ->
    Errors2 = [{{Id, {Pos, Rev}}, Error} || 
            {#doc{id=Id,revs={Pos,[Rev|_]}}, Error} <- AccErrors],
    {lists:reverse(AccPrepped), lists:reverse(Errors2)};
prep_and_validate_replicated_updates(Db, [Bucket|RestBuckets], [OldInfo|RestOldInfo], AccPrepped, AccErrors) ->
    case OldInfo of
    not_found ->
        {ValidatedBucket, AccErrors3} = lists:foldl(
            fun(Doc, {AccPrepped2, AccErrors2}) ->
                case validate_doc_update(Db, Doc, fun() -> nil end) of
                ok ->
                    {[Doc | AccPrepped2], AccErrors2};
                Error ->
                    {AccPrepped2, [{Doc, Error} | AccErrors2]}
                end
            end,
            {[], AccErrors}, Bucket),
        prep_and_validate_replicated_updates(Db, RestBuckets, RestOldInfo, [ValidatedBucket | AccPrepped], AccErrors3);
    {ok, #full_doc_info{rev_tree=OldTree}} ->
        NewRevTree = lists:foldl(
            fun(NewDoc, AccTree) ->
                {NewTree, _} = couch_key_tree:merge(AccTree, [couch_db:doc_to_tree(NewDoc)]),
                NewTree
            end,
            OldTree, Bucket),
        Leafs = couch_key_tree:get_all_leafs_full(NewRevTree),
        LeafRevsFullDict = dict:from_list( [{{Start, RevId}, FullPath} || {Start, [{RevId, _}|_]}=FullPath <- Leafs]),
        {ValidatedBucket, AccErrors3} =
        lists:foldl(
            fun(#doc{id=Id,revs={Pos, [RevId|_]}}=Doc, {AccValidated, AccErrors2}) ->
                case dict:find({Pos, RevId}, LeafRevsFullDict) of
                {ok, {Start, Path}} ->
                    % our unflushed doc is a leaf node. Go back on the path 
                    % to find the previous rev that's on disk.
                    PrevRevResult = 
                    case couch_doc:has_stubs(Doc) of
                    true ->
                        [_PrevRevFull | [PrevRevFull | _]=PrevPath]  = Path,
                        case PrevRevFull of
                        {_RevId, ?REV_MISSING} ->
                            conflict;
                        {RevId, {IsDel, DiskSp}} ->
                            DiskDoc = make_doc(Db, Id, IsDel, DiskSp, PrevPath),
                            Doc2 = couch_doc:merge_stubs(Doc, DiskDoc),
                            {ok, Doc2, fun() -> DiskDoc end}
                        end;
                    false ->    
                        {ok, Doc,
                            fun() ->
                                make_first_doc_on_disk(Db,Id,Start-1, tl(Path))
                            end}
                    end,
                    case PrevRevResult of
                    {ok, NewDoc, LoadPrevRevFun} ->                        
                        case validate_doc_update(Db, NewDoc, LoadPrevRevFun) of
                        ok ->
                            {[NewDoc | AccValidated], AccErrors2};
                        Error ->
                            {AccValidated, [{NewDoc, Error} | AccErrors2]}
                        end;
                    Error ->
                        {AccValidated, [{Doc, Error} | AccErrors2]}
                    end;
                _ ->
                    % this doc isn't a leaf or already exists in the tree.
                    % ignore but consider it a success.
                    {AccValidated, AccErrors2}
                end
            end,
            {[], AccErrors}, Bucket),
        prep_and_validate_replicated_updates(Db, RestBuckets, RestOldInfo, [ValidatedBucket | AccPrepped], AccErrors3)
    end.

update_docs(Db, Docs, Options, replicated_changes) ->
    couch_stats_collector:increment({couchdb, database_writes}),
    DocBuckets = group_alike_docs(Docs),
    
    case (Db#db.validate_doc_funs /= []) orelse
        lists:any(
            fun(#doc{id= <<?DESIGN_DOC_PREFIX, _/binary>>}) -> true;
            (_) -> false
            end, Docs) of
    true ->
        Ids = [Id || [#doc{id=Id}|_] <- DocBuckets],
        ExistingDocs = get_full_doc_infos(Db, Ids),
    
        {DocBuckets2, DocErrors} =
                prep_and_validate_replicated_updates(Db, DocBuckets, ExistingDocs, [], []),
        DocBuckets3 = [Bucket || [_|_]=Bucket <- DocBuckets2]; % remove empty buckets
    false ->
        DocErrors = [],
        DocBuckets3 = DocBuckets
    end,
    {ok, []} = write_and_commit(Db, DocBuckets3, [merge_conflicts | Options]),
    {ok, DocErrors};
    
update_docs(Db, Docs, Options, interactive_edit) ->
    couch_stats_collector:increment({couchdb, database_writes}),
    AllOrNothing = lists:member(all_or_nothing, Options),
    % go ahead and generate the new revision ids for the documents.
    Docs2 = lists:map(
        fun(#doc{id=Id,revs={Start, RevIds}}=Doc) ->
            case Id of
            <<?LOCAL_DOC_PREFIX, _/binary>> ->
                Rev = case RevIds of [] -> 0; [Rev0|_] -> list_to_integer(?b2l(Rev0)) end,
                Doc#doc{revs={Start, [?l2b(integer_to_list(Rev + 1))]}};
            _ ->
                Doc#doc{revs={Start+1, [?l2b(integer_to_list(couch_util:rand32())) | RevIds]}}
            end
        end, Docs),
    DocBuckets = group_alike_docs(Docs2),
    
    case (Db#db.validate_doc_funs /= []) orelse
        lists:any(
            fun(#doc{id= <<?DESIGN_DOC_PREFIX, _/binary>>}) ->
                true;
            (#doc{attachments=Atts}) ->
                Atts /= []
            end, Docs) of
    true ->
        % lookup the doc by id and get the most recent
        Ids = [Id || [#doc{id=Id}|_] <- DocBuckets],
        ExistingDocInfos = get_full_doc_infos(Db, Ids),
        
        {DocBucketsPrepped, Failures} =
        case AllOrNothing of
        true ->
            prep_and_validate_replicated_updates(Db, DocBuckets, 
                    ExistingDocInfos, [], []);
        false ->
            prep_and_validate_updates(Db, DocBuckets, ExistingDocInfos, [], [])
        end,
        
        % strip out any empty buckets
        DocBuckets2 = [Bucket || [_|_] = Bucket <- DocBucketsPrepped];
    false ->
        Failures = [],
        DocBuckets2 = DocBuckets
    end,

    if (AllOrNothing) and (Failures /= []) ->
         {aborted, Failures};
    true ->
        Options2 = if AllOrNothing -> [merge_conflicts]; 
                true -> [] end ++ Options,
        {ok, CommitFailures} = write_and_commit(Db, DocBuckets2, Options2),
        FailDict = dict:from_list(CommitFailures ++ Failures),
        % the output for each is either {ok, NewRev} or Error
        {ok, lists:map(
            fun(#doc{id=Id,revs={Pos, [NewRevId|_]}}) ->
                case dict:find({Id, {Pos, NewRevId}}, FailDict) of
                {ok, Error} ->
                    Error;
                error ->
                    {ok, {Pos, NewRevId}}
                end
            end, Docs2)}
    end.

% Returns the first available document on disk. Input list is a full rev path
% for the doc.
make_first_doc_on_disk(_Db, _Id, _Pos, []) ->
    nil;
make_first_doc_on_disk(Db, Id, Pos, [{_Rev, ?REV_MISSING}|RestPath]) ->
    make_first_doc_on_disk(Db, Id, Pos - 1, RestPath);
make_first_doc_on_disk(Db, Id, Pos, [{_Rev, {IsDel, Sp}} |_]=DocPath) ->
    Revs = [Rev || {Rev, _} <- DocPath],
    make_doc(Db, Id, IsDel, Sp, {Pos, Revs}).


write_and_commit(#db{update_pid=UpdatePid, user_ctx=Ctx}=Db, DocBuckets,
        Options) ->
    % flush unwritten binaries to disk.
    DocBuckets2 = [[doc_flush_binaries(Doc, Db#db.fd) || Doc <- Bucket] || Bucket <- DocBuckets],
    case gen_server:call(UpdatePid, {update_docs, DocBuckets2, Options}, infinity) of
    {ok, Conflicts} -> {ok, Conflicts};
    retry ->
        % This can happen if the db file we wrote to was swapped out by
        % compaction. Retry by reopening the db and writing to the current file
        {ok, Db2} = open_ref_counted(Db#db.main_pid, Ctx),
        DocBuckets3 = [[doc_flush_binaries(Doc, Db2#db.fd) || Doc <- Bucket] || Bucket <- DocBuckets],
        % We only retry once
        close(Db2),
        case gen_server:call(UpdatePid, {update_docs, DocBuckets3, Options}, infinity) of
        {ok, Conflicts} -> {ok, Conflicts};
        retry -> throw({update_error, compaction_retry})
        end
    end.


doc_flush_binaries(Doc, Fd) ->
    NewAttachments = lists:map(
        fun({Key, {Type, BinValue}}) ->
            NewBinValue = flush_binary(Fd, BinValue),
            {Key, {Type, NewBinValue}}
        end, Doc#doc.attachments),
    Doc#doc{attachments = NewAttachments}.

flush_binary(Fd, {Fd0, StreamPointer, Len}) when Fd0 == Fd ->
    % already written to our file, nothing to write
    {Fd, StreamPointer, Len};
  
flush_binary(Fd, {OtherFd, StreamPointer, Len}) ->
    with_stream(Fd, fun(OutputStream) -> 
        % written to a different file (or a closed file
        % instance, which will cause an error)
        ok = couch_stream:set_min_buffer(OutputStream, Len),
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
        {Fd, NewStreamPointer, Len}
    end);
                         
flush_binary(Fd, Bin) when is_binary(Bin) ->
    with_stream(Fd, fun(OutputStream) -> 
        ok = couch_stream:set_min_buffer(OutputStream, size(Bin)),
        {ok, StreamPointer} = couch_stream:write(OutputStream, Bin),
        {Fd, StreamPointer, size(Bin)}
    end);
                 
flush_binary(Fd, {StreamFun, undefined}) when is_function(StreamFun) ->
    % max_attachment_chunk_size control the max we buffer in memory
    MaxChunkSize = list_to_integer(couch_config:get("couchdb", 
        "max_attachment_chunk_size","4294967296")),
    with_stream(Fd, fun(OutputStream) -> 
        % StreamFun(MaxChunkSize, WriterFun) must call WriterFun
        % once for each chunk of the attachment.
        WriterFun = make_writer_fun(OutputStream),
        {ok, {TotalLength, NewStreamPointer}} = 
            StreamFun(MaxChunkSize, WriterFun, {0, nil}),
        {Fd, NewStreamPointer, TotalLength}
    end);
             
flush_binary(Fd, {Fun, Len}) when is_function(Fun) ->
    with_stream(Fd, fun(OutputStream) -> 
        ok = couch_stream:set_min_buffer(OutputStream, Len),
        {ok, StreamPointer} =
            write_streamed_attachment(OutputStream, Fun, Len, nil),
        {Fd, StreamPointer, Len}
    end).
            
with_stream(Fd, Fun) ->
    {ok, OutputStream} = couch_stream:open(Fd),
    Result = Fun(OutputStream),
    couch_stream:close(OutputStream),
    Result.

make_writer_fun(Stream) ->
    % WriterFun({Length, Binary}, State)
    % WriterFun({0, _Footers}, State)
    % Called with Length == 0 on the last time.
    % WriterFun returns NewState.
    fun
        ({0, _Footers}, {FinalLen, SpFin}) ->
            % last block, return the final tuple
            {ok, {FinalLen, SpFin}};
        ({Length, Bin}, {Total, nil}) ->
            % save StreamPointer 
            ok = couch_stream:set_min_buffer(Stream, Length),
            {ok, StreamPointer} = couch_stream:write(Stream, Bin),
            {Total+Length, StreamPointer};
        ({Length, Bin}, {Total, SpAcc}) ->
            % write the Bin to disk 
            ok = couch_stream:set_min_buffer(Stream, Length),
            {ok, _Sp} = couch_stream:write(Stream, Bin),
            {Total+Length, SpAcc}
    end.
    
write_streamed_attachment(_Stream, _F, 0, SpAcc) ->
    {ok, SpAcc};
write_streamed_attachment(Stream, F, LenLeft, nil) ->
    Bin = F(),
    {ok, StreamPointer} = couch_stream:write(Stream, Bin),
    write_streamed_attachment(Stream, F, LenLeft - size(Bin), StreamPointer);
write_streamed_attachment(Stream, F, LenLeft, SpAcc) ->
    Bin = F(),
    {ok, _} = couch_stream:write(Stream, Bin),
    write_streamed_attachment(Stream, F, LenLeft - size(Bin), SpAcc).

enum_docs_since_reduce_to_count(Reds) ->
    couch_btree:final_reduce(
            fun couch_db_updater:btree_by_seq_reduce/2, Reds).

enum_docs_reduce_to_count(Reds) ->
    {Count, _DelCount} = couch_btree:final_reduce(
            fun couch_db_updater:btree_by_id_reduce/2, Reds),
    Count.
    
count_changes_since(Db, SinceSeq) ->
    {ok, Changes} = 
    couch_btree:fold_reduce(Db#db.docinfo_by_seq_btree,
        SinceSeq + 1, % startkey
        ok, % endkey
        fun(_,_) -> true end, % groupkeys
        fun(_SeqStart, PartialReds, 0) ->
            {ok, couch_btree:final_reduce(Db#db.docinfo_by_seq_btree, PartialReds)}
        end,
        0),
    Changes.

enum_docs_since(Db, SinceSeq, Direction, InFun, Ctx) ->
    couch_btree:fold(Db#db.docinfo_by_seq_btree, SinceSeq + 1, Direction, InFun, Ctx).

enum_docs_since(Db, SinceSeq, InFun, Acc) ->
    enum_docs_since(Db, SinceSeq, fwd, InFun, Acc).

enum_docs(Db, StartId, Direction, InFun, InAcc) ->
    couch_btree:fold(Db#db.fulldocinfo_by_id_btree, StartId, Direction, InFun, InAcc).

enum_docs(Db, StartId, InFun, Ctx) ->
    enum_docs(Db, StartId, fwd, InFun, Ctx).

% server functions

init({DbName, Filepath, Fd, Options}) ->
    {ok, UpdaterPid} = gen_server:start_link(couch_db_updater, {self(), DbName, Filepath, Fd, Options}, []),
    {ok, #db{fd_ref_counter=RefCntr}=Db} = gen_server:call(UpdaterPid, get_db),
    couch_ref_counter:add(RefCntr),
    {ok, Db}.

terminate(Reason, _Db) ->
    couch_util:terminate_linked(Reason),
    ok.
    
handle_call({open_ref_count, OpenerPid}, _, #db{fd_ref_counter=RefCntr}=Db) ->
    ok = couch_ref_counter:add(RefCntr, OpenerPid),
    {reply, {ok, Db}, Db};
handle_call(is_idle, _From, #db{fd_ref_counter=RefCntr, compactor_pid=Compact, 
            waiting_delayed_commit=Delay}=Db) ->
    % Idle means no referrers. Unless in the middle of a compaction file switch, 
    % there are always at least 2 referrers, couch_db_updater and us.
    {reply, (Delay == nil) and (Compact == nil) and (couch_ref_counter:count(RefCntr) == 2), Db};
handle_call({db_updated, #db{fd_ref_counter=NewRefCntr}=NewDb}, _From,
        #db{fd_ref_counter=OldRefCntr}) ->
    case NewRefCntr == OldRefCntr of
    true -> ok;
    false ->
        couch_ref_counter:add(NewRefCntr),
        couch_ref_counter:drop(OldRefCntr)
    end,
    {reply, ok, NewDb}.


handle_cast(Msg, Db) ->
    ?LOG_ERROR("Bad cast message received for db ~s: ~p", [Db#db.name, Msg]),
    exit({error, Msg}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(Msg, Db) ->
    ?LOG_ERROR("Bad message received for db ~s: ~p", [Db#db.name, Msg]),
    exit({error, Msg}).


%%% Internal function %%%
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
                lists:map(fun({Value, {Pos, [Rev|_]}=FoundRevPath}) ->
                    case Value of
                    ?REV_MISSING ->
                        % we have the rev in our list but know nothing about it
                        {{not_found, missing}, {Pos, Rev}};
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

open_doc_int(Db, <<?LOCAL_DOC_PREFIX, _/binary>> = Id, _Options) ->
    case couch_btree:lookup(Db#db.local_docs_btree, [Id]) of
    [{ok, {_, {Rev, BodyData}}}] ->
        {ok, #doc{id=Id, revs={0, [list_to_binary(integer_to_list(Rev))]}, body=BodyData}};
    [not_found] ->
        {not_found, missing}
    end;
open_doc_int(Db, #doc_info{id=Id,rev={Pos,RevId},deleted=IsDeleted,summary_pointer=Sp}=DocInfo, Options) ->
    Doc = make_doc(Db, Id, IsDeleted, Sp, {Pos,[RevId]}),
    {ok, Doc#doc{meta=doc_meta_info(DocInfo, [], Options)}};
open_doc_int(Db, #full_doc_info{id=Id,rev_tree=RevTree}=FullDocInfo, Options) ->
    #doc_info{deleted=IsDeleted,rev=Rev,summary_pointer=Sp} = DocInfo =
        couch_doc:to_doc_info(FullDocInfo),
    {[{_, RevPath}], []} = couch_key_tree:get(RevTree, [Rev]),
    Doc = make_doc(Db, Id, IsDeleted, Sp, RevPath),
    {ok, Doc#doc{meta=doc_meta_info(DocInfo, RevTree, Options)}};
open_doc_int(Db, Id, Options) ->
    case get_full_doc_info(Db, Id) of
    {ok, FullDocInfo} ->
        open_doc_int(Db, FullDocInfo, Options);
    not_found ->
        {not_found, missing}
    end.

doc_meta_info(DocInfo, RevTree, Options) ->
    case lists:member(revs_info, Options) of
    false -> [];
    true ->
        {[{Pos, RevPath}],[]} = 
            couch_key_tree:get_full_key_paths(RevTree, [DocInfo#doc_info.rev]),
        
        [{revs_info, Pos, lists:map(
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


doc_to_tree(#doc{revs={Start, RevIds}}=Doc) ->
    [Tree] = doc_to_tree_simple(Doc, lists:reverse(RevIds)),
    {Start - length(RevIds) + 1, Tree}.


doc_to_tree_simple(Doc, [RevId]) ->
    [{RevId, Doc, []}];
doc_to_tree_simple(Doc, [RevId | Rest]) ->
    [{RevId, ?REV_MISSING, doc_to_tree_simple(Doc, Rest)}].

make_doc(Db, FullDocInfo) ->
    {#doc_info{id=Id,deleted=Deleted,summary_pointer=Sp}, RevPath}
            = couch_doc:to_doc_info_path(FullDocInfo),
    make_doc(Db, Id, Deleted, Sp, RevPath).
    
make_doc(#db{fd=Fd}=Db, Id, Deleted, BodySp, RevisionPath) ->
    {BodyData, BinValues} =
    case BodySp of
    nil ->
        {[], []};
    _ ->
        {ok, {BodyData0, BinValues0}} =
            couch_stream:read_term( Db#db.summary_stream, BodySp),
        {BodyData0,
            [{Name,{Type,{Fd,Sp,Len}}} || {Name,{Type,Sp,Len}} <- BinValues0]}
    end,
    #doc{
        id = Id,
        revs = RevisionPath,
        body = BodyData,
        attachments = BinValues,
        deleted = Deleted
        }.
    
    
    
