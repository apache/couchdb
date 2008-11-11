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
-export([open_ref_counted/3,num_refs/1,monitor/1]).
-export([update_doc/3,update_docs/4,update_docs/2,update_docs/3,delete_doc/3]).
-export([get_doc_info/2,open_doc/2,open_doc/3,open_doc_revs/4]).
-export([get_missing_revs/2,name/1,doc_to_tree/1]).
-export([enum_docs/4,enum_docs/5,enum_docs_since/4,enum_docs_since/5]).
-export([enum_docs_since_reduce_to_count/1,enum_docs_reduce_to_count/1]).
-export([increment_update_seq/1,get_purge_seq/1,purge_docs/2,get_last_purged/1]).
-export([start_link/3,make_doc/2]).
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

close(#db{fd=Fd}) ->
    couch_file:drop_ref(Fd).

open_ref_counted(MainPid, OpeningPid, UserCred) ->
    {ok, Db} = gen_server:call(MainPid, {open_ref_counted_instance, OpeningPid}),
    {ok, Db#db{user_ctx=UserCred}}.

num_refs(MainPid) ->
    gen_server:call(MainPid, num_refs).

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
        fulldocinfo_by_id_btree=FullDocBtree} = Db,
    {ok, Size} = couch_file:bytes(Fd),
    {ok, {Count, DelCount}} = couch_btree:full_reduce(FullDocBtree),
    InfoList = [
        {db_name, Name},
        {doc_count, Count},
        {doc_del_count, DelCount},
        {update_seq, SeqNum},
        {purge_seq, couch_db:get_purge_seq(Db)},
        {compact_running, Compactor/=nil},
        {disk_size, Size}
        ],
    {ok, InfoList}.

name(#db{name=Name}) ->
    Name.
    
update_doc(Db, Doc, Options) ->
    {ok, [NewRev]} = update_docs(Db, [Doc], Options),
    {ok, NewRev}.

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

validate_doc_update(#db{validate_doc_funs=[]}, Doc, _GetDiskDocFun) ->
    Doc;
validate_doc_update(_Db, #doc{id= <<"_design/",_/binary>>}=Doc, _GetDiskDocFun) ->
    Doc;
validate_doc_update(_Db, #doc{id= <<"_local/",_/binary>>}=Doc, _GetDiskDocFun) ->
    Doc;
validate_doc_update(#db{name=DbName,user_ctx={CtxProps}}=Db, Doc, GetDiskDocFun) ->
    DiskDoc = GetDiskDocFun(),
    [case Fun(Doc, DiskDoc, {[{<<"db">>, DbName} | CtxProps]}) of
        ok -> ok;
        Error -> throw(Error)
    end || Fun <- Db#db.validate_doc_funs],
    Doc.


prep_and_validate_new_edit(Db, #doc{id=Id,revs=[NewRev|PrevRevs]}=Doc,
        OldFullDocInfo, LeafRevsDict) ->
    case PrevRevs of
    [PrevRev|_] ->
        case dict:find(PrevRev, LeafRevsDict) of
        {ok, {Deleted, Sp, DiskRevs}} ->
            Doc2 = Doc#doc{revs=[NewRev|DiskRevs]},
            case couch_doc:has_stubs(Doc2) of
            true ->
                DiskDoc = make_doc(Db, Id, Deleted, Sp, DiskRevs),
                Doc3 = couch_doc:merge_stubs(Doc2, DiskDoc),
                validate_doc_update(Db, Doc3, fun() -> DiskDoc end);
            false ->
                LoadDiskDoc = fun() -> make_doc(Db,Id,Deleted,Sp,DiskRevs) end,
                validate_doc_update(Db, Doc2, LoadDiskDoc)
            end;
        error ->
            throw(conflict)
        end;
    [] ->
        % new doc, and we have existing revs.
        if OldFullDocInfo#full_doc_info.deleted ->
            % existing docs are deletions
            validate_doc_update(Db, Doc, nil);
        true ->
            throw(conflict)
        end
    end.

update_docs(#db{update_pid=UpdatePid}=Db, Docs, Options) ->
    update_docs(#db{update_pid=UpdatePid}=Db, Docs, Options, true).

update_docs(Db, Docs, Options, false) ->
    DocBuckets = group_alike_docs(Docs),
    Ids = [Id || [#doc{id=Id}|_] <- DocBuckets],
    
    ExistingDocs = get_full_doc_infos(Db, Ids),
    
    DocBuckets2 = lists:zipwith(
        fun(Bucket, not_found) ->
            [validate_doc_update(Db, Doc, fun()-> nil end) || Doc <- Bucket];
        (Bucket, {ok, #full_doc_info{rev_tree=OldRevTree}}) ->
            NewTree = lists:foldl(
                fun(Doc, RevTreeAcc) ->
                    couch_key_tree:merge(RevTreeAcc, doc_to_tree(Doc))
                end,
                OldRevTree, Bucket),
            Leafs = couch_key_tree:get_all_leafs_full(NewTree),
            LeafRevsFullDict = dict:from_list( [{Rev, FullPath} || [{Rev, _}|_]=FullPath <- Leafs]),
            lists:flatmap(
                fun(#doc{revs=[Rev|_]}=Doc) ->
                    case dict:find(Rev, LeafRevsFullDict) of
                    {ok, [{Rev, #doc{id=Id}}|_]=Path} ->
                        % our unflushed doc is a leaf node. Go back on the path 
                        % to find the previous rev that's on disk.
                        LoadPrevRev = fun() ->
                            make_first_doc_on_disk(Db, Id, Path)
                        end,
                        [validate_doc_update(Db, Doc, LoadPrevRev)];
                    _ ->
                        % this doc isn't a leaf or is already exists in the tree. ignore
                        []
                    end
                end, Bucket)
        end,
        DocBuckets, ExistingDocs),
    write_and_commit(Db, DocBuckets2, Options);
    
update_docs(Db, Docs, Options, true) ->
        % go ahead and generate the new revision ids for the documents.
    Docs2 = lists:map(
        fun(#doc{id=Id,revs=Revs}=Doc) ->
            case Id of
            <<?LOCAL_DOC_PREFIX, _/binary>> ->
                Rev = case Revs of [] -> 0; [Rev0|_] -> list_to_integer(binary_to_list(Rev0)) end,
                Doc#doc{revs=[list_to_binary(integer_to_list(Rev + 1))]};
            _ ->
                Doc#doc{revs=[list_to_binary(integer_to_list(couch_util:rand32())) | Revs]}
            end
        end, Docs),
    DocBuckets = group_alike_docs(Docs2),
    Ids = [Id || [#doc{id=Id}|_] <- DocBuckets],
    
    % lookup the doc by id and get the most recent
    
    ExistingDocs = get_full_doc_infos(Db, Ids),
    
    DocBuckets2 = lists:zipwith(
        fun(Bucket, not_found) ->
            % no existing revs on disk, make sure no old revs specified.
            [throw(conflict) || #doc{revs=[_NewRev, _OldRev | _]} <- Bucket],
            [validate_doc_update(Db, Doc, fun()-> nil end) || Doc <- Bucket];
        (Bucket, {ok, #full_doc_info{rev_tree=OldRevTree}=OldFullDocInfo}) ->
            Leafs = couch_key_tree:get_all_leafs(OldRevTree),
            LeafRevsDict = dict:from_list([{Rev, {Deleted, Sp, Revs}} || {Rev, {Deleted, Sp}, Revs} <- Leafs]),
            [prep_and_validate_new_edit(Db, Doc, OldFullDocInfo, LeafRevsDict) || Doc <- Bucket]
        end,
        DocBuckets, ExistingDocs),
    ok = write_and_commit(Db, DocBuckets2, [new_edits | Options]),
    {ok, [NewRev ||#doc{revs=[NewRev|_]} <- Docs2]}.


% Returns the first available document on disk. Input list is a full rev path
% for the doc.
make_first_doc_on_disk(_Db, _Id, []) ->
    nil;
make_first_doc_on_disk(Db, Id, [{_Rev, ?REV_MISSING}|RestPath]) ->
    make_first_doc_on_disk(Db, Id, RestPath);
make_first_doc_on_disk(Db, Id, [{_Rev, {IsDel, Sp}} |_]=DocPath) ->
    Revs = [Rev || {Rev, _} <- DocPath],
    make_doc(Db, Id, IsDel, Sp, Revs).


write_and_commit(#db{update_pid=UpdatePid}=Db, DocBuckets, Options) ->

    % flush unwritten binaries to disk.
    DocBuckets2 = [[doc_flush_binaries(Doc, Db#db.fd) || Doc <- Bucket] || Bucket <- DocBuckets],
    case gen_server:call(UpdatePid, {update_docs, DocBuckets2, Options}, infinity) of
    ok -> ok;
    retry ->
        % This can happen if the db file we wrote to was swapped out by
        % compaction. Retry writing to the current file
        {ok, Db2} = open_ref_counted(Db#db.main_pid, self(), {[]}),
        DocBuckets3 = [[doc_flush_binaries(Doc, Db2#db.fd) || Doc <- Bucket] || Bucket <- DocBuckets],
        % We only retry once
        close(Db2),
        case gen_server:call(UpdatePid, {update_docs, DocBuckets3, Options}, infinity) of
        ok -> ok;
        Else -> throw(Else)
        end;
    Else->
        throw(Else)
    end.


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
    couch_btree:final_reduce(
            fun couch_db_updater:btree_by_seq_reduce/2, Reds).

enum_docs_reduce_to_count(Reds) ->
    {Count, _DelCount} = couch_btree:final_reduce(
            fun couch_db_updater:btree_by_id_reduce/2, Reds),
    Count.

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
    ok = couch_file:add_ref(Fd),
    gen_server:call(UpdaterPid, get_db).

terminate(_Reason, Db) ->
    exit(Db#db.update_pid, kill).
    
handle_call({open_ref_counted_instance, OpenerPid}, _From, #db{fd=Fd}=Db) ->
    ok = couch_file:add_ref(Fd, OpenerPid),
    {reply, {ok, Db}, Db};
handle_call(num_refs, _From, #db{fd=Fd}=Db) ->
    {reply, couch_file:num_refs(Fd) - 1, Db};
handle_call({db_updated, #db{fd=NewFd}=NewDb}, _From, #db{fd=OldFd}) ->
    case NewFd == OldFd of
    true -> ok;
    false ->
        couch_file:add_ref(NewFd),
        couch_file:drop_ref(OldFd)
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

open_doc_int(Db, <<?LOCAL_DOC_PREFIX, _/binary>> = Id, _Options) ->
    case couch_btree:lookup(Db#db.local_docs_btree, [Id]) of
    [{ok, {_, {Rev, BodyData}}}] ->
        {ok, #doc{id=Id, revs=[list_to_binary(integer_to_list(Rev))], body=BodyData}};
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


doc_to_tree(Doc) ->
    doc_to_tree(Doc, lists:reverse(Doc#doc.revs)).

doc_to_tree(Doc, [RevId]) ->
    [{RevId, Doc, []}];
doc_to_tree(Doc, [RevId | Rest]) ->
    [{RevId, ?REV_MISSING, doc_to_tree(Doc, Rest)}].

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
    
    
    