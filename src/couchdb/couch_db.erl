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

-module(couch_db).
-behaviour(gen_server).

-export([open/2,open_int/2,close/1,create/2,get_db_info/1,get_design_docs/1]).
-export([start_compact/1, cancel_compact/1]).
-export([open_ref_counted/2,is_idle/1,monitor/1,count_changes_since/2]).
-export([update_doc/3,update_doc/4,update_docs/4,update_docs/2,update_docs/3,delete_doc/3]).
-export([get_doc_info/2,open_doc/2,open_doc/3,open_doc_revs/4]).
-export([set_revs_limit/2,get_revs_limit/1]).
-export([get_missing_revs/2,name/1,get_update_seq/1,get_committed_update_seq/1]).
-export([enum_docs/4,enum_docs_since/5]).
-export([enum_docs_since_reduce_to_count/1,enum_docs_reduce_to_count/1]).
-export([increment_update_seq/1,get_purge_seq/1,purge_docs/2,get_last_purged/1]).
-export([start_link/3,open_doc_int/3,ensure_full_commit/1]).
-export([set_security/2,get_security/1]).
-export([init/1,terminate/2,handle_call/3,handle_cast/2,code_change/3,handle_info/2]).
-export([changes_since/4,changes_since/5,read_doc/2,new_revid/1]).
-export([check_is_admin/1, check_is_member/1]).
-export([reopen/1, is_system_db/1]).

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
            ok = couch_file:sync(Fd),
            {ok, Fd};
        {error, enoent} ->
            {not_found, no_db_file}
        end;
    Error ->
        Error
    end.


create(DbName, Options) ->
    couch_server:create(DbName, Options).

% this is for opening a database for internal purposes like the replicator
% or the view indexer. it never throws a reader error.
open_int(DbName, Options) ->
    couch_server:open(DbName, Options).

% this should be called anytime an http request opens the database.
% it ensures that the http userCtx is a valid reader
open(DbName, Options) ->
    case couch_server:open(DbName, Options) of
        {ok, Db} ->
            try
                check_is_member(Db),
                {ok, Db}
            catch
                throw:Error ->
                    close(Db),
                    throw(Error)
            end;
        Else -> Else
    end.

reopen(#db{main_pid = Pid, fd_ref_counter = OldRefCntr, user_ctx = UserCtx}) ->
    {ok, #db{fd_ref_counter = NewRefCntr} = NewDb} =
        gen_server:call(Pid, get_db, infinity),
    case NewRefCntr =:= OldRefCntr of
    true ->
        ok;
    false ->
        couch_ref_counter:add(NewRefCntr),
        catch couch_ref_counter:drop(OldRefCntr)
    end,
    {ok, NewDb#db{user_ctx = UserCtx}}.

is_system_db(#db{options = Options}) ->
    lists:member(sys_db, Options).

ensure_full_commit(#db{update_pid=UpdatePid,instance_start_time=StartTime}) ->
    ok = gen_server:call(UpdatePid, full_commit, infinity),
    {ok, StartTime}.

close(#db{fd_ref_counter=RefCntr}) ->
    couch_ref_counter:drop(RefCntr).

open_ref_counted(MainPid, OpenedPid) ->
    gen_server:call(MainPid, {open_ref_count, OpenedPid}).

is_idle(#db{main_pid = MainPid}) ->
    is_idle(MainPid);
is_idle(MainPid) ->
    gen_server:call(MainPid, is_idle).

monitor(#db{main_pid=MainPid}) ->
    erlang:monitor(process, MainPid).

start_compact(#db{update_pid=Pid}) ->
    gen_server:call(Pid, start_compact).

cancel_compact(#db{update_pid=Pid}) ->
    gen_server:call(Pid, cancel_compact).

delete_doc(Db, Id, Revisions) ->
    DeletedDocs = [#doc{id=Id, revs=[Rev], deleted=true} || Rev <- Revisions],
    {ok, [Result]} = update_docs(Db, DeletedDocs, []),
    {ok, Result}.

open_doc(Db, IdOrDocInfo) ->
    open_doc(Db, IdOrDocInfo, []).

open_doc(Db, Id, Options) ->
    increment_stat(Db, {couchdb, database_reads}),
    case open_doc_int(Db, Id, Options) of
    {ok, #doc{deleted=true}=Doc} ->
        case lists:member(deleted, Options) of
        true ->
            apply_open_options({ok, Doc},Options);
        false ->
            {not_found, deleted}
        end;
    Else ->
        apply_open_options(Else,Options)
    end.

apply_open_options({ok, Doc},Options) ->
    apply_open_options2(Doc,Options);
apply_open_options(Else,_Options) ->
    Else.

apply_open_options2(Doc,[]) ->
    {ok, Doc};
apply_open_options2(#doc{atts=Atts,revs=Revs}=Doc,
        [{atts_since, PossibleAncestors}|Rest]) ->
    RevPos = find_ancestor_rev_pos(Revs, PossibleAncestors),
    apply_open_options2(Doc#doc{atts=[A#att{data=
        if AttPos>RevPos -> Data; true -> stub end}
        || #att{revpos=AttPos,data=Data}=A <- Atts]}, Rest);
apply_open_options2(Doc, [ejson_body | Rest]) ->
    apply_open_options2(couch_doc:with_ejson_body(Doc), Rest);
apply_open_options2(Doc,[_|Rest]) ->
    apply_open_options2(Doc,Rest).


find_ancestor_rev_pos({_, []}, _AttsSinceRevs) ->
    0;
find_ancestor_rev_pos(_DocRevs, []) ->
    0;
find_ancestor_rev_pos({RevPos, [RevId|Rest]}, AttsSinceRevs) ->
    case lists:member({RevPos, RevId}, AttsSinceRevs) of
    true ->
        RevPos;
    false ->
        find_ancestor_rev_pos({RevPos - 1, Rest}, AttsSinceRevs)
    end.

open_doc_revs(Db, Id, Revs, Options) ->
    increment_stat(Db, {couchdb, database_reads}),
    [{ok, Results}] = open_doc_revs_int(Db, [{Id, Revs}], Options),
    {ok, [apply_open_options(Result, Options) || Result <- Results]}.

% Each returned result is a list of tuples:
% {Id, MissingRevs, PossibleAncestors}
% if no revs are missing, it's omitted from the results.
get_missing_revs(Db, IdRevsList) ->
    Results = get_full_doc_infos(Db, [Id1 || {Id1, _Revs} <- IdRevsList]),
    {ok, find_missing(IdRevsList, Results)}.

find_missing([], []) ->
    [];
find_missing([{Id, Revs}|RestIdRevs], [{ok, FullInfo} | RestLookupInfo]) ->
    case couch_key_tree:find_missing(FullInfo#full_doc_info.rev_tree, Revs) of
    [] ->
        find_missing(RestIdRevs, RestLookupInfo);
    MissingRevs ->
        #doc_info{revs=RevsInfo} = couch_doc:to_doc_info(FullInfo),
        LeafRevs = [Rev || #rev_info{rev=Rev} <- RevsInfo],
        % Find the revs that are possible parents of this rev
        PossibleAncestors =
        lists:foldl(fun({LeafPos, LeafRevId}, Acc) ->
            % this leaf is a "possible ancenstor" of the missing
            % revs if this LeafPos lessthan any of the missing revs
            case lists:any(fun({MissingPos, _}) ->
                    LeafPos < MissingPos end, MissingRevs) of
            true ->
                [{LeafPos, LeafRevId} | Acc];
            false ->
                Acc
            end
        end, [], LeafRevs),
        [{Id, MissingRevs, PossibleAncestors} |
                find_missing(RestIdRevs, RestLookupInfo)]
    end;
find_missing([{Id, Revs}|RestIdRevs], [not_found | RestLookupInfo]) ->
    [{Id, Revs, []} | find_missing(RestIdRevs, RestLookupInfo)].

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
    couch_btree:lookup(by_id_btree(Db), Ids).

increment_update_seq(#db{update_pid=UpdatePid}) ->
    gen_server:call(UpdatePid, increment_update_seq).

purge_docs(#db{update_pid=UpdatePid}, IdsRevs) ->
    gen_server:call(UpdatePid, {purge_docs, IdsRevs}).

get_committed_update_seq(#db{committed_update_seq=Seq}) ->
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
        header=#db_header{disk_version=DiskVersion},
        compactor_pid=Compactor,
        update_seq=SeqNum,
        name=Name,
        instance_start_time=StartTime,
        committed_update_seq=CommittedUpdateSeq,
        fulldocinfo_by_id_btree = IdBtree,
        docinfo_by_seq_btree = SeqBtree,
        local_docs_btree = LocalBtree
    } = Db,
    {ok, Size} = couch_file:bytes(Fd),
    {ok, DbReduction} = couch_btree:full_reduce(by_id_btree(Db)),
    InfoList = [
        {db_name, Name},
        {doc_count, element(1, DbReduction)},
        {doc_del_count, element(2, DbReduction)},
        {update_seq, SeqNum},
        {purge_seq, couch_db:get_purge_seq(Db)},
        {compact_running, Compactor/=nil},
        {disk_size, Size},
        {data_size, db_data_size(DbReduction, [SeqBtree, IdBtree, LocalBtree])},
        {instance_start_time, StartTime},
        {disk_format_version, DiskVersion},
        {committed_update_seq, CommittedUpdateSeq}
        ],
    {ok, InfoList}.

db_data_size({_Count, _DelCount}, _Trees) ->
    % pre 1.2 format, upgraded on compaction
    null;
db_data_size({_Count, _DelCount, nil}, _Trees) ->
    null;
db_data_size({_Count, _DelCount, DocAndAttsSize}, Trees) ->
    sum_tree_sizes(DocAndAttsSize, Trees).

sum_tree_sizes(Acc, []) ->
    Acc;
sum_tree_sizes(Acc, [T | Rest]) ->
    case couch_btree:size(T) of
    nil ->
        null;
    Sz ->
        sum_tree_sizes(Acc + Sz, Rest)
    end.

get_design_docs(Db) ->
    {ok, _, Docs} = couch_view:fold(
        #view{btree=by_id_btree(Db)},
        fun(#full_doc_info{deleted = true}, _Reds, AccDocs) ->
            {ok, AccDocs};
        (#full_doc_info{id= <<"_design/",_/binary>>}=FullDocInfo, _Reds, AccDocs) ->
            {ok, Doc} = open_doc_int(Db, FullDocInfo, [ejson_body]),
            {ok, [Doc | AccDocs]};
        (_, _Reds, AccDocs) ->
            {stop, AccDocs}
        end,
        [], [{start_key, <<"_design/">>}, {end_key_gt, <<"_design0">>}]),
    {ok, Docs}.

check_is_admin(#db{user_ctx=#user_ctx{name=Name,roles=Roles}}=Db) ->
    {Admins} = get_admins(Db),
    AdminRoles = [<<"_admin">> | couch_util:get_value(<<"roles">>, Admins, [])],
    AdminNames = couch_util:get_value(<<"names">>, Admins,[]),
    case AdminRoles -- Roles of
    AdminRoles -> % same list, not an admin role
        case AdminNames -- [Name] of
        AdminNames -> % same names, not an admin
            throw({unauthorized, <<"You are not a db or server admin.">>});
        _ ->
            ok
        end;
    _ ->
        ok
    end.

check_is_member(#db{user_ctx=#user_ctx{name=Name,roles=Roles}=UserCtx}=Db) ->
    case (catch check_is_admin(Db)) of
    ok -> ok;
    _ ->
        {Members} = get_members(Db),
        ReaderRoles = couch_util:get_value(<<"roles">>, Members,[]),
        WithAdminRoles = [<<"_admin">> | ReaderRoles],
        ReaderNames = couch_util:get_value(<<"names">>, Members,[]),
        case ReaderRoles ++ ReaderNames of
        [] -> ok; % no readers == public access
        _Else ->
            case WithAdminRoles -- Roles of
            WithAdminRoles -> % same list, not an reader role
                case ReaderNames -- [Name] of
                ReaderNames -> % same names, not a reader
                    ?LOG_DEBUG("Not a reader: UserCtx ~p vs Names ~p Roles ~p",[UserCtx, ReaderNames, WithAdminRoles]),
                    throw({unauthorized, <<"You are not authorized to access this db.">>});
                _ ->
                    ok
                end;
            _ ->
                ok
            end
        end
    end.

get_admins(#db{security=SecProps}) ->
    couch_util:get_value(<<"admins">>, SecProps, {[]}).

get_members(#db{security=SecProps}) ->
    % we fallback to readers here for backwards compatibility
    couch_util:get_value(<<"members">>, SecProps,
        couch_util:get_value(<<"readers">>, SecProps, {[]})).

get_security(#db{security=SecProps}) ->
    {SecProps}.

set_security(#db{update_pid=Pid}=Db, {NewSecProps}) when is_list(NewSecProps) ->
    check_is_admin(Db),
    ok = validate_security_object(NewSecProps),
    ok = gen_server:call(Pid, {set_security, NewSecProps}, infinity),
    {ok, _} = ensure_full_commit(Db),
    ok;
set_security(_, _) ->
    throw(bad_request).

validate_security_object(SecProps) ->
    Admins = couch_util:get_value(<<"admins">>, SecProps, {[]}),
    % we fallback to readers here for backwards compatibility
    Members = couch_util:get_value(<<"members">>, SecProps,
        couch_util:get_value(<<"readers">>, SecProps, {[]})),
    ok = validate_names_and_roles(Admins),
    ok = validate_names_and_roles(Members),
    ok.

% validate user input
validate_names_and_roles({Props}) when is_list(Props) ->
    case couch_util:get_value(<<"names">>,Props,[]) of
    Ns when is_list(Ns) ->
            [throw("names must be a JSON list of strings") ||N <- Ns, not is_binary(N)],
            Ns;
    _ -> throw("names must be a JSON list of strings")
    end,
    case couch_util:get_value(<<"roles">>,Props,[]) of
    Rs when is_list(Rs) ->
        [throw("roles must be a JSON list of strings") ||R <- Rs, not is_binary(R)],
        Rs;
    _ -> throw("roles must be a JSON list of strings")
    end,
    ok.

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
    update_doc(Db, Doc, Options, interactive_edit).

update_doc(Db, Doc, Options, UpdateType) ->
    case update_docs(Db, [Doc], Options, UpdateType) of
    {ok, [{ok, NewRev}]} ->
        {ok, NewRev};
    {ok, [{{_Id, _Rev}, Error}]} ->
        throw(Error);
    {ok, [Error]} ->
        throw(Error);
    {ok, []} ->
        % replication success
        {Pos, [RevId | _]} = Doc#doc.revs,
        {ok, {Pos, RevId}}
    end.

update_docs(Db, Docs) ->
    update_docs(Db, Docs, []).

% group_alike_docs groups the sorted documents into sublist buckets, by id.
% ([DocA, DocA, DocB, DocC], []) -> [[DocA, DocA], [DocB], [DocC]]
group_alike_docs(Docs) ->
    Sorted = lists:sort(fun({#doc{id=A},_},{#doc{id=B},_})-> A < B end, Docs),
    group_alike_docs(Sorted, []).

group_alike_docs([], Buckets) ->
    lists:reverse(lists:map(fun lists:reverse/1, Buckets));
group_alike_docs([Doc|Rest], []) ->
    group_alike_docs(Rest, [[Doc]]);
group_alike_docs([{Doc,Ref}|Rest], [Bucket|RestBuckets]) ->
    [{#doc{id=BucketId},_Ref}|_] = Bucket,
    case Doc#doc.id == BucketId of
    true ->
        % add to existing bucket
        group_alike_docs(Rest, [[{Doc,Ref}|Bucket]|RestBuckets]);
    false ->
        % add to new bucket
       group_alike_docs(Rest, [[{Doc,Ref}]|[Bucket|RestBuckets]])
    end.

validate_doc_update(#db{}=Db, #doc{id= <<"_design/",_/binary>>}, _GetDiskDocFun) ->
    catch check_is_admin(Db);
validate_doc_update(#db{validate_doc_funs=[]}, _Doc, _GetDiskDocFun) ->
    ok;
validate_doc_update(_Db, #doc{id= <<"_local/",_/binary>>}, _GetDiskDocFun) ->
    ok;
validate_doc_update(Db, Doc, GetDiskDocFun) ->
    DiskDoc = GetDiskDocFun(),
    JsonCtx = couch_util:json_user_ctx(Db),
    SecObj = get_security(Db),
    try [case Fun(Doc, DiskDoc, JsonCtx, SecObj) of
            ok -> ok;
            Error -> throw(Error)
        end || Fun <- Db#db.validate_doc_funs],
        ok
    catch
        throw:Error ->
            Error
    end.


prep_and_validate_update(Db, #doc{id=Id,revs={RevStart, Revs}}=Doc,
        OldFullDocInfo, LeafRevsDict, AllowConflict) ->
    case Revs of
    [PrevRev|_] ->
        case dict:find({RevStart, PrevRev}, LeafRevsDict) of
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
        error when AllowConflict ->
            couch_doc:merge_stubs(Doc, #doc{}), % will generate error if
                                                        % there are stubs
            {validate_doc_update(Db, Doc, fun() -> nil end), Doc};
        error ->
            {conflict, Doc}
        end;
    [] ->
        % new doc, and we have existing revs.
        % reuse existing deleted doc
        if OldFullDocInfo#full_doc_info.deleted orelse AllowConflict ->
            {validate_doc_update(Db, Doc, fun() -> nil end), Doc};
        true ->
            {conflict, Doc}
        end
    end.



prep_and_validate_updates(_Db, [], [], _AllowConflict, AccPrepped,
        AccFatalErrors) ->
   {AccPrepped, AccFatalErrors};
prep_and_validate_updates(Db, [DocBucket|RestBuckets], [not_found|RestLookups],
        AllowConflict, AccPrepped, AccErrors) ->
    {PreppedBucket, AccErrors3} = lists:foldl(
        fun({#doc{revs=Revs}=Doc,Ref}, {AccBucket, AccErrors2}) ->
            case couch_doc:has_stubs(Doc) of
            true ->
                couch_doc:merge_stubs(Doc, #doc{}); % will throw exception
            false -> ok
            end,
            case Revs of
            {0, []} ->
                case validate_doc_update(Db, Doc, fun() -> nil end) of
                ok ->
                    {[{Doc, Ref} | AccBucket], AccErrors2};
                Error ->
                    {AccBucket, [{Ref, Error} | AccErrors2]}
                end;
            _ ->
                % old revs specified but none exist, a conflict
                {AccBucket, [{Ref, conflict} | AccErrors2]}
            end
        end,
        {[], AccErrors}, DocBucket),

    prep_and_validate_updates(Db, RestBuckets, RestLookups, AllowConflict,
            [lists:reverse(PreppedBucket) | AccPrepped], AccErrors3);
prep_and_validate_updates(Db, [DocBucket|RestBuckets],
        [{ok, #full_doc_info{rev_tree=OldRevTree}=OldFullDocInfo}|RestLookups],
        AllowConflict, AccPrepped, AccErrors) ->
    Leafs = couch_key_tree:get_all_leafs(OldRevTree),
    LeafRevsDict = dict:from_list([
        begin
            Deleted = element(1, LeafVal),
            Sp = element(2, LeafVal),
            {{Start, RevId}, {Deleted, Sp, Revs}}
        end ||
        {LeafVal, {Start, [RevId | _]} = Revs} <- Leafs
    ]),
    {PreppedBucket, AccErrors3} = lists:foldl(
        fun({Doc, Ref}, {Docs2Acc, AccErrors2}) ->
            case prep_and_validate_update(Db, Doc, OldFullDocInfo,
                    LeafRevsDict, AllowConflict) of
            {ok, Doc2} ->
                {[{Doc2, Ref} | Docs2Acc], AccErrors2};
            {Error, #doc{}} ->
                % Record the error
                {Docs2Acc, [{Ref, Error} |AccErrors2]}
            end
        end,
        {[], AccErrors}, DocBucket),
    prep_and_validate_updates(Db, RestBuckets, RestLookups, AllowConflict,
            [PreppedBucket | AccPrepped], AccErrors3).


update_docs(Db, Docs, Options) ->
    update_docs(Db, Docs, Options, interactive_edit).


prep_and_validate_replicated_updates(_Db, [], [], AccPrepped, AccErrors) ->
    Errors2 = [{{Id, {Pos, Rev}}, Error} ||
            {#doc{id=Id,revs={Pos,[Rev|_]}}, Error} <- AccErrors],
    {lists:reverse(AccPrepped), lists:reverse(Errors2)};
prep_and_validate_replicated_updates(Db, [Bucket|RestBuckets], [OldInfo|RestOldInfo], AccPrepped, AccErrors) ->
    case OldInfo of
    not_found ->
        {ValidatedBucket, AccErrors3} = lists:foldl(
            fun({Doc, Ref}, {AccPrepped2, AccErrors2}) ->
                case couch_doc:has_stubs(Doc) of
                true ->
                    couch_doc:merge_stubs(Doc, #doc{}); % will throw exception
                false -> ok
                end,
                case validate_doc_update(Db, Doc, fun() -> nil end) of
                ok ->
                    {[{Doc, Ref} | AccPrepped2], AccErrors2};
                Error ->
                    {AccPrepped2, [{Doc, Error} | AccErrors2]}
                end
            end,
            {[], AccErrors}, Bucket),
        prep_and_validate_replicated_updates(Db, RestBuckets, RestOldInfo, [ValidatedBucket | AccPrepped], AccErrors3);
    {ok, #full_doc_info{rev_tree=OldTree}} ->
        NewRevTree = lists:foldl(
            fun({NewDoc, _Ref}, AccTree) ->
                {NewTree, _} = couch_key_tree:merge(AccTree,
                    couch_doc:to_path(NewDoc), Db#db.revs_limit),
                NewTree
            end,
            OldTree, Bucket),
        Leafs = couch_key_tree:get_all_leafs_full(NewRevTree),
        LeafRevsFullDict = dict:from_list( [{{Start, RevId}, FullPath} || {Start, [{RevId, _}|_]}=FullPath <- Leafs]),
        {ValidatedBucket, AccErrors3} =
        lists:foldl(
            fun({#doc{id=Id,revs={Pos, [RevId|_]}}=Doc, Ref}, {AccValidated, AccErrors2}) ->
                case dict:find({Pos, RevId}, LeafRevsFullDict) of
                {ok, {Start, Path}} ->
                    % our unflushed doc is a leaf node. Go back on the path
                    % to find the previous rev that's on disk.

                    LoadPrevRevFun = fun() ->
                                make_first_doc_on_disk(Db,Id,Start-1, tl(Path))
                            end,

                    case couch_doc:has_stubs(Doc) of
                    true ->
                        DiskDoc = LoadPrevRevFun(),
                        Doc2 = couch_doc:merge_stubs(Doc, DiskDoc),
                        GetDiskDocFun = fun() -> DiskDoc end;
                    false ->
                        Doc2 = Doc,
                        GetDiskDocFun = LoadPrevRevFun
                    end,

                    case validate_doc_update(Db, Doc2, GetDiskDocFun) of
                    ok ->
                        {[{Doc2, Ref} | AccValidated], AccErrors2};
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
        prep_and_validate_replicated_updates(Db, RestBuckets, RestOldInfo,
                [ValidatedBucket | AccPrepped], AccErrors3)
    end.



new_revid(#doc{body=Body,revs={OldStart,OldRevs},
        atts=Atts,deleted=Deleted}) ->
    case [{N, T, M} || #att{name=N,type=T,md5=M} <- Atts, M =/= <<>>] of
    Atts2 when length(Atts) =/= length(Atts2) ->
        % We must have old style non-md5 attachments
        ?l2b(integer_to_list(couch_util:rand32()));
    Atts2 ->
        OldRev = case OldRevs of [] -> 0; [OldRev0|_] -> OldRev0 end,
        couch_util:md5(term_to_binary([Deleted, OldStart, OldRev, Body, Atts2]))
    end.

new_revs([], OutBuckets, IdRevsAcc) ->
    {lists:reverse(OutBuckets), IdRevsAcc};
new_revs([Bucket|RestBuckets], OutBuckets, IdRevsAcc) ->
    {NewBucket, IdRevsAcc3} = lists:mapfoldl(
        fun({#doc{revs={Start, RevIds}}=Doc, Ref}, IdRevsAcc2)->
        NewRevId = new_revid(Doc),
        {{Doc#doc{revs={Start+1, [NewRevId | RevIds]}}, Ref},
            [{Ref, {ok, {Start+1, NewRevId}}} | IdRevsAcc2]}
    end, IdRevsAcc, Bucket),
    new_revs(RestBuckets, [NewBucket|OutBuckets], IdRevsAcc3).

check_dup_atts(#doc{atts=Atts}=Doc) ->
    Atts2 = lists:sort(fun(#att{name=N1}, #att{name=N2}) -> N1 < N2 end, Atts),
    check_dup_atts2(Atts2),
    Doc.

check_dup_atts2([#att{name=N}, #att{name=N} | _]) ->
    throw({bad_request, <<"Duplicate attachments">>});
check_dup_atts2([_ | Rest]) ->
    check_dup_atts2(Rest);
check_dup_atts2(_) ->
    ok.


update_docs(Db, Docs, Options, replicated_changes) ->
    increment_stat(Db, {couchdb, database_writes}),

    % associate reference with each doc in order to track duplicates
    Docs2 = lists:map(fun(Doc) -> {Doc, make_ref()} end, Docs),
    DocBuckets = before_docs_update(Db, group_alike_docs(Docs2)),

    case (Db#db.validate_doc_funs /= []) orelse
        lists:any(
            fun({#doc{id= <<?DESIGN_DOC_PREFIX, _/binary>>}, _Ref}) -> true;
            ({#doc{atts=Atts}, _Ref}) ->
                Atts /= []
            end, Docs2) of
    true ->
        Ids = [Id || [{#doc{id=Id}, _Ref}|_] <- DocBuckets],
        ExistingDocs = get_full_doc_infos(Db, Ids),

        {DocBuckets2, DocErrors} =
                prep_and_validate_replicated_updates(Db, DocBuckets, ExistingDocs, [], []),
        DocBuckets3 = [Bucket || [_|_]=Bucket <- DocBuckets2]; % remove empty buckets
    false ->
        DocErrors = [],
        DocBuckets3 = DocBuckets
    end,
    DocBuckets4 = [[{doc_flush_atts(check_dup_atts(Doc), Db#db.updater_fd), Ref}
            || {Doc, Ref} <- Bucket] || Bucket <- DocBuckets3],
    {ok, []} = write_and_commit(Db, DocBuckets4, [], [merge_conflicts | Options]),
    {ok, DocErrors};

update_docs(Db, Docs, Options, interactive_edit) ->
    increment_stat(Db, {couchdb, database_writes}),
    AllOrNothing = lists:member(all_or_nothing, Options),
    % go ahead and generate the new revision ids for the documents.
    % separate out the NonRep documents from the rest of the documents

    % associate reference with each doc in order to track duplicates
    Docs2 = lists:map(fun(Doc) -> {Doc, make_ref()} end,Docs),
    {Docs3, NonRepDocs} = lists:foldl(
         fun({#doc{id=Id},_Ref}=Doc, {DocsAcc, NonRepDocsAcc}) ->
            case Id of
            <<?LOCAL_DOC_PREFIX, _/binary>> ->
                {DocsAcc, [Doc | NonRepDocsAcc]};
            Id->
                {[Doc | DocsAcc], NonRepDocsAcc}
            end
        end, {[], []}, Docs2),

    DocBuckets = before_docs_update(Db, group_alike_docs(Docs3)),

    case (Db#db.validate_doc_funs /= []) orelse
        lists:any(
            fun({#doc{id= <<?DESIGN_DOC_PREFIX, _/binary>>}, _Ref}) ->
                true;
            ({#doc{atts=Atts}, _Ref}) ->
                Atts /= []
            end, Docs3) of
    true ->
        % lookup the doc by id and get the most recent
        Ids = [Id || [{#doc{id=Id}, _Ref}|_] <- DocBuckets],
        ExistingDocInfos = get_full_doc_infos(Db, Ids),

        {DocBucketsPrepped, PreCommitFailures} = prep_and_validate_updates(Db,
                DocBuckets, ExistingDocInfos, AllOrNothing, [], []),

        % strip out any empty buckets
        DocBuckets2 = [Bucket || [_|_] = Bucket <- DocBucketsPrepped];
    false ->
        PreCommitFailures = [],
        DocBuckets2 = DocBuckets
    end,

    if (AllOrNothing) and (PreCommitFailures /= []) ->
        {aborted,
         lists:foldl(fun({#doc{id=Id,revs={Pos, RevIds}}, Ref},Acc) ->
                         case lists:keyfind(Ref,1,PreCommitFailures) of
                         {Ref, Error} ->
                             [{{Id,{Pos,RevIds}}, Error} | Acc];
                         false ->
                             Acc
                         end
                     end,[],Docs3)};

    true ->
        Options2 = if AllOrNothing -> [merge_conflicts];
                true -> [] end ++ Options,
        DocBuckets3 = [[
                {doc_flush_atts(set_new_att_revpos(
                        check_dup_atts(Doc)), Db#db.updater_fd), Ref}
                || {Doc, Ref} <- B] || B <- DocBuckets2],
        {DocBuckets4, IdRevs} = new_revs(DocBuckets3, [], []),

        {ok, CommitResults} = write_and_commit(Db, DocBuckets4, NonRepDocs, Options2),

        ResultsDict = dict:from_list(IdRevs ++ CommitResults ++ PreCommitFailures),
        {ok, lists:map(
            fun({#doc{}, Ref}) ->
                {ok, Result} = dict:find(Ref, ResultsDict),
                Result
            end, Docs2)}
    end.

% Returns the first available document on disk. Input list is a full rev path
% for the doc.
make_first_doc_on_disk(_Db, _Id, _Pos, []) ->
    nil;
make_first_doc_on_disk(Db, Id, Pos, [{_Rev, #doc{}} | RestPath]) ->
    make_first_doc_on_disk(Db, Id, Pos-1, RestPath);
make_first_doc_on_disk(Db, Id, Pos, [{_Rev, ?REV_MISSING}|RestPath]) ->
    make_first_doc_on_disk(Db, Id, Pos - 1, RestPath);
make_first_doc_on_disk(Db, Id, Pos, [{_Rev, RevValue} |_]=DocPath) ->
    IsDel = element(1, RevValue),
    Sp = element(2, RevValue),
    Revs = [Rev || {Rev, _} <- DocPath],
    make_doc(Db, Id, IsDel, Sp, {Pos, Revs}).

set_commit_option(Options) ->
    CommitSettings = {
        [true || O <- Options, O==full_commit orelse O==delay_commit],
        couch_config:get("couchdb", "delayed_commits", "false")
    },
    case CommitSettings of
    {[true], _} ->
        Options; % user requested explicit commit setting, do not change it
    {_, "true"} ->
        Options; % delayed commits are enabled, do nothing
    {_, "false"} ->
        [full_commit|Options];
    {_, Else} ->
        ?LOG_ERROR("[couchdb] delayed_commits setting must be true/false, not ~p",
            [Else]),
        [full_commit|Options]
    end.

collect_results(UpdatePid, MRef, ResultsAcc) ->
    receive
    {result, UpdatePid, Result} ->
        collect_results(UpdatePid, MRef, [Result | ResultsAcc]);
    {done, UpdatePid} ->
        {ok, ResultsAcc};
    {retry, UpdatePid} ->
        retry;
    {'DOWN', MRef, _, _, Reason} ->
        exit(Reason)
    end.

write_and_commit(#db{update_pid=UpdatePid}=Db, DocBuckets1,
        NonRepDocs, Options0) ->
    DocBuckets = prepare_doc_summaries(Db, DocBuckets1),
    Options = set_commit_option(Options0),
    MergeConflicts = lists:member(merge_conflicts, Options),
    FullCommit = lists:member(full_commit, Options),
    MRef = erlang:monitor(process, UpdatePid),
    try
        UpdatePid ! {update_docs, self(), DocBuckets, NonRepDocs, MergeConflicts, FullCommit},
        case collect_results(UpdatePid, MRef, []) of
        {ok, Results} -> {ok, Results};
        retry ->
            % This can happen if the db file we wrote to was swapped out by
            % compaction. Retry by reopening the db and writing to the current file
            {ok, Db2} = open_ref_counted(Db#db.main_pid, self()),
            DocBuckets2 = [
                [{doc_flush_atts(Doc, Db2#db.updater_fd), Ref} || {Doc, Ref} <- Bucket] ||
                Bucket <- DocBuckets1
            ],
            % We only retry once
            DocBuckets3 = prepare_doc_summaries(Db2, DocBuckets2),
            close(Db2),
            UpdatePid ! {update_docs, self(), DocBuckets3, NonRepDocs, MergeConflicts, FullCommit},
            case collect_results(UpdatePid, MRef, []) of
            {ok, Results} -> {ok, Results};
            retry -> throw({update_error, compaction_retry})
            end
        end
    after
        erlang:demonitor(MRef, [flush])
    end.


prepare_doc_summaries(Db, BucketList) ->
    [lists:map(
        fun({#doc{body = Body, atts = Atts} = Doc, Ref}) ->
            DiskAtts = [{N, T, P, AL, DL, R, M, E} ||
                #att{name = N, type = T, data = {_, P}, md5 = M, revpos = R,
                    att_len = AL, disk_len = DL, encoding = E} <- Atts],
            AttsFd = case Atts of
            [#att{data = {Fd, _}} | _] ->
                Fd;
            [] ->
                nil
            end,
            SummaryChunk = couch_db_updater:make_doc_summary(Db, {Body, DiskAtts}),
            {Doc#doc{body = {summary, SummaryChunk, AttsFd}}, Ref}
        end,
        Bucket) || Bucket <- BucketList].


before_docs_update(#db{before_doc_update = nil}, BucketList) ->
    BucketList;
before_docs_update(#db{before_doc_update = Fun} = Db, BucketList) ->
    [lists:map(
        fun({Doc, Ref}) ->
            NewDoc = Fun(couch_doc:with_ejson_body(Doc), Db),
			{NewDoc, Ref}
        end,
        Bucket) || Bucket <- BucketList].


set_new_att_revpos(#doc{revs={RevPos,_Revs},atts=Atts}=Doc) ->
    Doc#doc{atts= lists:map(fun(#att{data={_Fd,_Sp}}=Att) ->
            % already commited to disk, do not set new rev
            Att;
        (Att) ->
            Att#att{revpos=RevPos+1}
        end, Atts)}.


doc_flush_atts(Doc, Fd) ->
    Doc#doc{atts=[flush_att(Fd, Att) || Att <- Doc#doc.atts]}.

check_md5(_NewSig, <<>>) -> ok;
check_md5(Sig, Sig) -> ok;
check_md5(_, _) -> throw(md5_mismatch).

flush_att(Fd, #att{data={Fd0, _}}=Att) when Fd0 == Fd ->
    % already written to our file, nothing to write
    Att;

flush_att(Fd, #att{data={OtherFd,StreamPointer}, md5=InMd5,
    disk_len=InDiskLen} = Att) ->
    {NewStreamData, Len, _IdentityLen, Md5, IdentityMd5} =
            couch_stream:copy_to_new_stream(OtherFd, StreamPointer, Fd),
    check_md5(IdentityMd5, InMd5),
    Att#att{data={Fd, NewStreamData}, md5=Md5, att_len=Len, disk_len=InDiskLen};

flush_att(Fd, #att{data=Data}=Att) when is_binary(Data) ->
    with_stream(Fd, Att, fun(OutputStream) ->
        couch_stream:write(OutputStream, Data)
    end);

flush_att(Fd, #att{data=Fun,att_len=undefined}=Att) when is_function(Fun) ->
    with_stream(Fd, Att, fun(OutputStream) ->
        % Fun(MaxChunkSize, WriterFun) must call WriterFun
        % once for each chunk of the attachment,
        Fun(4096,
            % WriterFun({Length, Binary}, State)
            % WriterFun({0, _Footers}, State)
            % Called with Length == 0 on the last time.
            % WriterFun returns NewState.
            fun({0, Footers}, _) ->
                F = mochiweb_headers:from_binary(Footers),
                case mochiweb_headers:get_value("Content-MD5", F) of
                undefined ->
                    ok;
                Md5 ->
                    {md5, base64:decode(Md5)}
                end;
            ({_Length, Chunk}, _) ->
                couch_stream:write(OutputStream, Chunk)
            end, ok)
    end);

flush_att(Fd, #att{data=Fun,att_len=AttLen}=Att) when is_function(Fun) ->
    with_stream(Fd, Att, fun(OutputStream) ->
        write_streamed_attachment(OutputStream, Fun, AttLen)
    end).


compressible_att_type(MimeType) when is_binary(MimeType) ->
    compressible_att_type(?b2l(MimeType));
compressible_att_type(MimeType) ->
    TypeExpList = re:split(
        couch_config:get("attachments", "compressible_types", ""),
        "\\s*,\\s*",
        [{return, list}]
    ),
    lists:any(
        fun(TypeExp) ->
            Regexp = ["^\\s*", re:replace(TypeExp, "\\*", ".*"),
                "(?:\\s*;.*?)?\\s*", $$],
            re:run(MimeType, Regexp, [caseless]) =/= nomatch
        end,
        [T || T <- TypeExpList, T /= []]
    ).

% From RFC 2616 3.6.1 - Chunked Transfer Coding
%
%   In other words, the origin server is willing to accept
%   the possibility that the trailer fields might be silently
%   discarded along the path to the client.
%
% I take this to mean that if "Trailers: Content-MD5\r\n"
% is present in the request, but there is no Content-MD5
% trailer, we're free to ignore this inconsistency and
% pretend that no Content-MD5 exists.
with_stream(Fd, #att{md5=InMd5,type=Type,encoding=Enc}=Att, Fun) ->
    {ok, OutputStream} = case (Enc =:= identity) andalso
        compressible_att_type(Type) of
    true ->
        CompLevel = list_to_integer(
            couch_config:get("attachments", "compression_level", "0")
        ),
        couch_stream:open(Fd, gzip, [{compression_level, CompLevel}]);
    _ ->
        couch_stream:open(Fd)
    end,
    ReqMd5 = case Fun(OutputStream) of
        {md5, FooterMd5} ->
            case InMd5 of
                md5_in_footer -> FooterMd5;
                _ -> InMd5
            end;
        _ ->
            InMd5
    end,
    {StreamInfo, Len, IdentityLen, Md5, IdentityMd5} =
        couch_stream:close(OutputStream),
    check_md5(IdentityMd5, ReqMd5),
    {AttLen, DiskLen, NewEnc} = case Enc of
    identity ->
        case {Md5, IdentityMd5} of
        {Same, Same} ->
            {Len, IdentityLen, identity};
        _ ->
            {Len, IdentityLen, gzip}
        end;
    gzip ->
        case {Att#att.att_len, Att#att.disk_len} of
        {AL, DL} when AL =:= undefined orelse DL =:= undefined ->
            % Compressed attachment uploaded through the standalone API.
            {Len, Len, gzip};
        {AL, DL} ->
            % This case is used for efficient push-replication, where a
            % compressed attachment is located in the body of multipart
            % content-type request.
            {AL, DL, gzip}
        end
    end,
    Att#att{
        data={Fd,StreamInfo},
        att_len=AttLen,
        disk_len=DiskLen,
        md5=Md5,
        encoding=NewEnc
    }.


write_streamed_attachment(_Stream, _F, 0) ->
    ok;
write_streamed_attachment(Stream, F, LenLeft) when LenLeft > 0 ->
    Bin = read_next_chunk(F, LenLeft),
    ok = couch_stream:write(Stream, Bin),
    write_streamed_attachment(Stream, F, LenLeft - size(Bin)).

read_next_chunk(F, _) when is_function(F, 0) ->
    F();
read_next_chunk(F, LenLeft) when is_function(F, 1) ->
    F(lists:min([LenLeft, 16#2000])).

enum_docs_since_reduce_to_count(Reds) ->
    couch_btree:final_reduce(
            fun couch_db_updater:btree_by_seq_reduce/2, Reds).

enum_docs_reduce_to_count(Reds) ->
    FinalRed = couch_btree:final_reduce(
            fun couch_db_updater:btree_by_id_reduce/2, Reds),
    element(1, FinalRed).

changes_since(Db, StartSeq, Fun, Acc) ->
    changes_since(Db, StartSeq, Fun, [], Acc).

changes_since(Db, StartSeq, Fun, Options, Acc) ->
    Wrapper = fun(DocInfo, _Offset, Acc2) -> Fun(DocInfo, Acc2) end,
    {ok, _LastReduction, AccOut} = couch_btree:fold(by_seq_btree(Db),
        Wrapper, Acc, [{start_key, StartSeq + 1}] ++ Options),
    {ok, AccOut}.

count_changes_since(Db, SinceSeq) ->
    BTree = by_seq_btree(Db),
    {ok, Changes} =
    couch_btree:fold_reduce(BTree,
        fun(_SeqStart, PartialReds, 0) ->
            {ok, couch_btree:final_reduce(BTree, PartialReds)}
        end,
        0, [{start_key, SinceSeq + 1}]),
    Changes.

enum_docs_since(Db, SinceSeq, InFun, Acc, Options) ->
    {ok, LastReduction, AccOut} = couch_btree:fold(
        by_seq_btree(Db), InFun, Acc, [{start_key, SinceSeq + 1} | Options]),
    {ok, enum_docs_since_reduce_to_count(LastReduction), AccOut}.

enum_docs(Db, InFun, InAcc, Options) ->
    {ok, LastReduce, OutAcc} = couch_view:fold(
        #view{btree=by_id_btree(Db)}, InFun, InAcc, Options),
    {ok, enum_docs_reduce_to_count(LastReduce), OutAcc}.

% server functions

init({DbName, Filepath, Fd, Options}) ->
    {ok, UpdaterPid} = gen_server:start_link(couch_db_updater, {self(), DbName, Filepath, Fd, Options}, []),
    {ok, #db{fd_ref_counter=RefCntr}=Db} = gen_server:call(UpdaterPid, get_db),
    couch_ref_counter:add(RefCntr),
    case lists:member(sys_db, Options) of
    true ->
        ok;
    false ->
        couch_stats_collector:track_process_count({couchdb, open_databases})
    end,
    process_flag(trap_exit, true),
    {ok, Db}.

terminate(_Reason, Db) ->
    couch_util:shutdown_sync(Db#db.update_pid),
    ok.

handle_call({open_ref_count, OpenerPid}, _, #db{fd_ref_counter=RefCntr}=Db) ->
    ok = couch_ref_counter:add(RefCntr, OpenerPid),
    {reply, {ok, Db}, Db};
handle_call(is_idle, _From, #db{fd_ref_counter=RefCntr, compactor_pid=Compact,
            waiting_delayed_commit=Delay}=Db) ->
    % Idle means no referrers. Unless in the middle of a compaction file switch,
    % there are always at least 2 referrers, couch_db_updater and us.
    {reply, (Delay == nil) andalso (Compact == nil) andalso (couch_ref_counter:count(RefCntr) == 2), Db};
handle_call({db_updated, NewDb}, _From, #db{fd_ref_counter=OldRefCntr}) ->
    #db{fd_ref_counter=NewRefCntr}=NewDb,
    case NewRefCntr =:= OldRefCntr of
    true -> ok;
    false ->
        couch_ref_counter:add(NewRefCntr),
        couch_ref_counter:drop(OldRefCntr)
    end,
    {reply, ok, NewDb};
handle_call(get_db, _From, Db) ->
    {reply, {ok, Db}, Db}.


handle_cast(Msg, Db) ->
    ?LOG_ERROR("Bad cast message received for db ~s: ~p", [Db#db.name, Msg]),
    exit({error, Msg}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({'EXIT', _Pid, normal}, Db) ->
    {noreply, Db};
handle_info({'EXIT', _Pid, Reason}, Server) ->
    {stop, Reason, Server};
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
                    RevValue ->
                        IsDeleted = element(1, RevValue),
                        SummaryPtr = element(2, RevValue),
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

open_doc_int(Db, <<?LOCAL_DOC_PREFIX, _/binary>> = Id, Options) ->
    case couch_btree:lookup(local_btree(Db), [Id]) of
    [{ok, {_, {Rev, BodyData}}}] ->
        Doc = #doc{id=Id, revs={0, [?l2b(integer_to_list(Rev))]}, body=BodyData},
        apply_open_options({ok, Doc}, Options);
    [not_found] ->
        {not_found, missing}
    end;
open_doc_int(Db, #doc_info{id=Id,revs=[RevInfo|_]}=DocInfo, Options) ->
    #rev_info{deleted=IsDeleted,rev={Pos,RevId},body_sp=Bp} = RevInfo,
    Doc = make_doc(Db, Id, IsDeleted, Bp, {Pos,[RevId]}),
    apply_open_options(
       {ok, Doc#doc{meta=doc_meta_info(DocInfo, [], Options)}}, Options);
open_doc_int(Db, #full_doc_info{id=Id,rev_tree=RevTree}=FullDocInfo, Options) ->
    #doc_info{revs=[#rev_info{deleted=IsDeleted,rev=Rev,body_sp=Bp}|_]} =
        DocInfo = couch_doc:to_doc_info(FullDocInfo),
    {[{_, RevPath}], []} = couch_key_tree:get(RevTree, [Rev]),
    Doc = make_doc(Db, Id, IsDeleted, Bp, RevPath),
    apply_open_options(
        {ok, Doc#doc{meta=doc_meta_info(DocInfo, RevTree, Options)}}, Options);
open_doc_int(Db, Id, Options) ->
    case get_full_doc_info(Db, Id) of
    {ok, FullDocInfo} ->
        open_doc_int(Db, FullDocInfo, Options);
    not_found ->
        {not_found, missing}
    end.

doc_meta_info(#doc_info{high_seq=Seq,revs=[#rev_info{rev=Rev}|RestInfo]}, RevTree, Options) ->
    case lists:member(revs_info, Options) of
    false -> [];
    true ->
        {[{Pos, RevPath}],[]} =
            couch_key_tree:get_full_key_paths(RevTree, [Rev]),

        [{revs_info, Pos, lists:map(
            fun({Rev1, ?REV_MISSING}) ->
                {Rev1, missing};
            ({Rev1, RevValue}) ->
                case element(1, RevValue) of
                true ->
                    {Rev1, deleted};
                false ->
                    {Rev1, available}
                end
            end, RevPath)}]
    end ++
    case lists:member(conflicts, Options) of
    false -> [];
    true ->
        case [Rev1 || #rev_info{rev=Rev1,deleted=false} <- RestInfo] of
        [] -> [];
        ConflictRevs -> [{conflicts, ConflictRevs}]
        end
    end ++
    case lists:member(deleted_conflicts, Options) of
    false -> [];
    true ->
        case [Rev1 || #rev_info{rev=Rev1,deleted=true} <- RestInfo] of
        [] -> [];
        DelConflictRevs -> [{deleted_conflicts, DelConflictRevs}]
        end
    end ++
    case lists:member(local_seq, Options) of
    false -> [];
    true -> [{local_seq, Seq}]
    end.

read_doc(#db{fd=Fd}, Pos) ->
    couch_file:pread_term(Fd, Pos).


make_doc(#db{updater_fd = Fd} = Db, Id, Deleted, Bp, RevisionPath) ->
    {BodyData, Atts} =
    case Bp of
    nil ->
        {[], []};
    _ ->
        {ok, {BodyData0, Atts00}} = read_doc(Db, Bp),
        Atts0 = case Atts00 of
        _ when is_binary(Atts00) ->
            couch_compress:decompress(Atts00);
        _ when is_list(Atts00) ->
            % pre 1.2 format
            Atts00
        end,
        {BodyData0,
            lists:map(
                fun({Name,Type,Sp,AttLen,DiskLen,RevPos,Md5,Enc}) ->
                    #att{name=Name,
                        type=Type,
                        att_len=AttLen,
                        disk_len=DiskLen,
                        md5=Md5,
                        revpos=RevPos,
                        data={Fd,Sp},
                        encoding=
                            case Enc of
                            true ->
                                % 0110 UPGRADE CODE
                                gzip;
                            false ->
                                % 0110 UPGRADE CODE
                                identity;
                            _ ->
                                Enc
                            end
                    };
                ({Name,Type,Sp,AttLen,RevPos,Md5}) ->
                    #att{name=Name,
                        type=Type,
                        att_len=AttLen,
                        disk_len=AttLen,
                        md5=Md5,
                        revpos=RevPos,
                        data={Fd,Sp}};
                ({Name,{Type,Sp,AttLen}}) ->
                    #att{name=Name,
                        type=Type,
                        att_len=AttLen,
                        disk_len=AttLen,
                        md5= <<>>,
                        revpos=0,
                        data={Fd,Sp}}
                end, Atts0)}
    end,
    Doc = #doc{
        id = Id,
        revs = RevisionPath,
        body = BodyData,
        atts = Atts,
        deleted = Deleted
    },
    after_doc_read(Db, Doc).


after_doc_read(#db{after_doc_read = nil}, Doc) ->
    Doc;
after_doc_read(#db{after_doc_read = Fun} = Db, Doc) ->
    Fun(couch_doc:with_ejson_body(Doc), Db).


increment_stat(#db{options = Options}, Stat) ->
    case lists:member(sys_db, Options) of
    true ->
        ok;
    false ->
        couch_stats_collector:increment(Stat)
    end.

local_btree(#db{local_docs_btree = BTree, fd = ReaderFd}) ->
    BTree#btree{fd = ReaderFd}.

by_seq_btree(#db{docinfo_by_seq_btree = BTree, fd = ReaderFd}) ->
    BTree#btree{fd = ReaderFd}.

by_id_btree(#db{fulldocinfo_by_id_btree = BTree, fd = ReaderFd}) ->
    BTree#btree{fd = ReaderFd}.
