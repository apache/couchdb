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

-export([
    create/2,
    open/2,
    open_int/2,
    incref/1,
    reopen/1,
    close/1,

    clustered_db/2,
    clustered_db/3,

    monitor/1,
    monitored_by/1,
    is_idle/1,

    is_admin/1,
    check_is_admin/1,
    check_is_member/1,

    name/1,
    compression/1,
    get_after_doc_read_fun/1,
    get_before_doc_update_fun/1,
    get_committed_update_seq/1,
    get_compacted_seq/1,
    get_compactor_pid/1,
    get_db_info/1,
    get_doc_count/1,
    get_epochs/1,
    get_filepath/1,
    get_instance_start_time/1,
    get_last_purged/1,
    get_pid/1,
    get_revs_limit/1,
    get_security/1,
    get_update_seq/1,
    get_user_ctx/1,
    get_uuid/1,
    get_purge_seq/1,

    is_db/1,
    is_system_db/1,
    is_clustered/1,

    increment_update_seq/1,
    set_revs_limit/2,
    set_security/2,
    set_user_ctx/2,

    ensure_full_commit/1,
    ensure_full_commit/2,

    load_validation_funs/1,

    open_doc/2,
    open_doc/3,
    open_doc_revs/4,
    open_doc_int/3,
    read_doc/2,
    get_doc_info/2,
    get_full_doc_info/2,
    get_full_doc_infos/2,
    get_missing_revs/2,
    get_design_docs/1,

    update_doc/3,
    update_doc/4,
    update_docs/4,
    update_docs/2,
    update_docs/3,
    delete_doc/3,

    purge_docs/2,

    with_stream/3,

    fold_docs/4,
    fold_local_docs/4,
    enum_docs/4,
    enum_docs_reduce_to_count/1,

    enum_docs_since/5,
    enum_docs_since_reduce_to_count/1,
    changes_since/4,
    changes_since/5,
    count_changes_since/2,

    calculate_start_seq/3,
    owner_of/2,

    start_compact/1,
    cancel_compact/1,
    wait_for_compaction/1,
    wait_for_compaction/2,

    dbname_suffix/1,
    normalize_dbname/1,
    validate_dbname/1,

    check_md5/2,
    make_doc/5,
    new_revid/1
]).


-export([
    start_link/3
]).


-include_lib("couch/include/couch_db.hrl").
-include("couch_db_int.hrl").

-define(DBNAME_REGEX,
    "^[a-z][a-z0-9\\_\\$()\\+\\-\\/]*" % use the stock CouchDB regex
    "(\\.[0-9]{10,})?$" % but allow an optional shard timestamp at the end
).

start_link(DbName, Filepath, Options) ->
    case open_db_file(Filepath, Options) of
    {ok, Fd} ->
        {ok, UpdaterPid} = gen_server:start_link(couch_db_updater, {DbName,
            Filepath, Fd, Options}, []),
        unlink(Fd),
        gen_server:call(UpdaterPid, get_db);
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
        case couch_file:open(Filepath ++ ".compact", [nologifmissing]) of
        {ok, Fd} ->
            couch_log:info("Found ~s~s compaction file, using as primary"
                           " storage.", [Filepath, ".compact"]),
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

reopen(#db{main_pid = Pid, fd = Fd, fd_monitor = OldRef, user_ctx = UserCtx}) ->
    {ok, #db{fd = NewFd} = NewDb} = gen_server:call(Pid, get_db, infinity),
    case NewFd =:= Fd of
    true ->
        {ok, NewDb#db{user_ctx = UserCtx}};
    false ->
        erlang:demonitor(OldRef, [flush]),
        NewRef = erlang:monitor(process, NewFd),
        {ok, NewDb#db{user_ctx = UserCtx, fd_monitor = NewRef}}
    end.

incref(#db{fd = Fd} = Db) ->
    Ref = erlang:monitor(process, Fd),
    {ok, Db#db{fd_monitor = Ref}}.

clustered_db(DbName, UserCtx) ->
    clustered_db(DbName, UserCtx, []).

clustered_db(DbName, UserCtx, SecProps) ->
    {ok, #db{name = DbName, user_ctx = UserCtx, security = SecProps}}.

is_db(#db{}) ->
    true;
is_db(_) ->
    false.

is_system_db(#db{options = Options}) ->
    lists:member(sys_db, Options).

is_clustered(#db{main_pid = nil}) ->
    true;
is_clustered(#db{}) ->
    false;
is_clustered(?NEW_PSE_DB = Db) ->
    ?PSE_DB_MAIN_PID(Db) == undefined.

ensure_full_commit(#db{main_pid=Pid, instance_start_time=StartTime}) ->
    ok = gen_server:call(Pid, full_commit, infinity),
    {ok, StartTime}.

ensure_full_commit(Db, RequiredSeq) ->
    #db{main_pid=Pid, instance_start_time=StartTime} = Db,
    ok = gen_server:call(Pid, {full_commit, RequiredSeq}, infinity),
    {ok, StartTime}.

close(#db{fd_monitor=Ref}) ->
    erlang:demonitor(Ref, [flush]),
    ok;
close(?NEW_PSE_DB) ->
    ok.

is_idle(#db{compactor_pid=nil, waiting_delayed_commit=nil} = Db) ->
    monitored_by(Db) == [];
is_idle(_Db) ->
    false.

monitored_by(Db) ->
    case erlang:process_info(Db#db.fd, monitored_by) of
    undefined ->
        [];
    {monitored_by, Pids} ->
        PidTracker = whereis(couch_stats_process_tracker),
        Pids -- [Db#db.main_pid, PidTracker]
    end.


monitor(#db{main_pid=MainPid}) ->
    erlang:monitor(process, MainPid).

start_compact(#db{main_pid=Pid}) ->
    gen_server:call(Pid, start_compact).

cancel_compact(#db{main_pid=Pid}) ->
    gen_server:call(Pid, cancel_compact).

wait_for_compaction(Db) ->
    wait_for_compaction(Db, infinity).

wait_for_compaction(#db{main_pid=Pid}=Db, Timeout) ->
    Start = os:timestamp(),
    case gen_server:call(Pid, compactor_pid) of
        CPid when is_pid(CPid) ->
            Ref = erlang:monitor(process, CPid),
            receive
                {'DOWN', Ref, _, _, normal} when Timeout == infinity ->
                    wait_for_compaction(Db, Timeout);
                {'DOWN', Ref, _, _, normal} ->
                    Elapsed = timer:now_diff(os:timestamp(), Start) div 1000,
                    wait_for_compaction(Db, Timeout - Elapsed);
                {'DOWN', Ref, _, _, Reason} ->
                    {error, Reason}
            after Timeout ->
                erlang:demonitor(Ref, [flush]),
                {error, Timeout}
            end;
        _ ->
            ok
    end.

delete_doc(Db, Id, Revisions) ->
    DeletedDocs = [#doc{id=Id, revs=[Rev], deleted=true} || Rev <- Revisions],
    {ok, [Result]} = update_docs(Db, DeletedDocs, []),
    {ok, Result}.

open_doc(Db, IdOrDocInfo) ->
    open_doc(Db, IdOrDocInfo, []).

open_doc(Db, Id, Options) ->
    increment_stat(Db, [couchdb, database_reads]),
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
apply_open_options2(#doc{atts=Atts0,revs=Revs}=Doc,
        [{atts_since, PossibleAncestors}|Rest]) ->
    RevPos = find_ancestor_rev_pos(Revs, PossibleAncestors),
    Atts = lists:map(fun(Att) ->
        [AttPos, Data] = couch_att:fetch([revpos, data], Att),
        if  AttPos > RevPos -> couch_att:store(data, Data, Att);
            true -> couch_att:store(data, stub, Att)
        end
    end, Atts0),
    apply_open_options2(Doc#doc{atts=Atts}, Rest);
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
    increment_stat(Db, [couchdb, database_reads]),
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
    couch_btree:lookup(Db#db.id_tree, Ids).

increment_update_seq(#db{main_pid=Pid}) ->
    gen_server:call(Pid, increment_update_seq).

purge_docs(#db{main_pid=Pid}, IdsRevs) ->
    gen_server:call(Pid, {purge_docs, IdsRevs}).

get_after_doc_read_fun(#db{after_doc_read = Fun}) ->
    Fun.

get_before_doc_update_fun(#db{before_doc_update = Fun}) ->
    Fun.

get_committed_update_seq(#db{committed_update_seq=Seq}) ->
    Seq.

get_update_seq(#db{update_seq=Seq})->
    Seq.

get_user_ctx(#db{user_ctx = UserCtx}) ->
    UserCtx;
get_user_ctx(?NEW_PSE_DB = Db) ->
    ?PSE_DB_USER_CTX(Db).

get_purge_seq(#db{}=Db) ->
    couch_db_header:purge_seq(Db#db.header).

get_last_purged(#db{}=Db) ->
    case couch_db_header:purged_docs(Db#db.header) of
        nil ->
            {ok, []};
        Pointer ->
            couch_file:pread_term(Db#db.fd, Pointer)
    end.

get_pid(#db{main_pid = Pid}) ->
    Pid.

get_doc_count(Db) ->
    {ok, Reds} = couch_btree:full_reduce(Db#db.id_tree),
    {ok, element(1, Reds)}.

get_uuid(#db{}=Db) ->
    couch_db_header:uuid(Db#db.header).

get_epochs(#db{}=Db) ->
    Epochs = couch_db_header:epochs(Db#db.header),
    validate_epochs(Epochs),
    Epochs.

get_filepath(#db{filepath = FilePath}) ->
    FilePath.

get_instance_start_time(#db{instance_start_time = IST}) ->
    IST.

get_compacted_seq(#db{}=Db) ->
    couch_db_header:compacted_seq(Db#db.header).

get_compactor_pid(#db{compactor_pid = Pid}) ->
    Pid.

get_db_info(Db) ->
    #db{fd=Fd,
        header=Header,
        compactor_pid=Compactor,
        update_seq=SeqNum,
        name=Name,
        instance_start_time=StartTime,
        committed_update_seq=CommittedUpdateSeq,
        id_tree = IdBtree
    } = Db,
    {ok, FileSize} = couch_file:bytes(Fd),
    {ok, DbReduction} = couch_btree:full_reduce(IdBtree),
    SizeInfo0 = element(3, DbReduction),
    SizeInfo = case SizeInfo0 of
        SI when is_record(SI, size_info) ->
            SI;
        {AS, ES} ->
            #size_info{active=AS, external=ES};
        AS ->
            #size_info{active=AS}
    end,
    ActiveSize = active_size(Db, SizeInfo),
    DiskVersion = couch_db_header:disk_version(Header),
    Uuid = case get_uuid(Db) of
        undefined -> null;
        Uuid0 -> Uuid0
    end,
    CompactedSeq = case get_compacted_seq(Db) of
        undefined -> null;
        Else1 -> Else1
    end,
    InfoList = [
        {db_name, Name},
        {doc_count, element(1, DbReduction)},
        {doc_del_count, element(2, DbReduction)},
        {update_seq, SeqNum},
        {purge_seq, couch_db:get_purge_seq(Db)},
        {compact_running, Compactor/=nil},
        {disk_size, FileSize}, % legacy
        {other, {[{data_size, SizeInfo#size_info.external}]}}, % legacy
        {data_size, ActiveSize}, % legacy
        {sizes, {[
            {file, FileSize},
            {active, ActiveSize},
            {external, SizeInfo#size_info.external}
        ]}},
        {instance_start_time, StartTime},
        {disk_format_version, DiskVersion},
        {committed_update_seq, CommittedUpdateSeq},
        {compacted_seq, CompactedSeq},
        {uuid, Uuid}
        ],
    {ok, InfoList}.

active_size(#db{}=Db, Size) when is_integer(Size) ->
    active_size(Db, #size_info{active=Size});
active_size(#db{}=Db, #size_info{}=SI) ->
    Trees = [
        Db#db.id_tree,
        Db#db.seq_tree,
        Db#db.local_tree
    ],
    lists:foldl(fun(T, Acc) ->
        case couch_btree:size(T) of
            _ when Acc == null ->
                null;
            nil ->
                null;
            Size ->
                Acc + Size
        end
    end, SI#size_info.active, Trees).

get_design_docs(#db{name = <<"shards/", _:18/binary, DbName/binary>>}) ->
    {_, Ref} = spawn_monitor(fun() -> exit(fabric:design_docs(DbName)) end),
    receive {'DOWN', Ref, _, _, Response} ->
        Response
    end;
get_design_docs(#db{id_tree = IdBtree}) ->
    FoldFun = pipe([fun skip_deleted/4], fun
        (#full_doc_info{deleted = true}, _Reds, Acc) ->
            {ok, Acc};
        (#full_doc_info{id= <<"_design/",_/binary>>}=FullDocInfo, _Reds, Acc) ->
            {ok, [FullDocInfo | Acc]};
        (_, _Reds, Acc) ->
            {stop, Acc}
    end),
    KeyOpts = [{start_key, <<"_design/">>}, {end_key_gt, <<"_design0">>}],
    {ok, _, Docs} = couch_btree:fold(IdBtree, FoldFun, [], KeyOpts),
    {ok, Docs}.


check_is_admin(#db{user_ctx=UserCtx}=Db) ->
    case is_admin(Db) of
        true -> ok;
        false ->
            Reason = <<"You are not a db or server admin.">>,
            throw_security_error(UserCtx, Reason)
    end.

check_is_member(#db{user_ctx=UserCtx}=Db) ->
    case is_member(Db) of
        true -> ok;
        false -> throw_security_error(UserCtx)
    end.

is_admin(#db{user_ctx=UserCtx}=Db) ->
    case couch_db_plugin:check_is_admin(Db) of
        true -> true;
        false ->
            {Admins} = get_admins(Db),
            is_authorized(UserCtx, Admins)
    end.

is_member(#db{user_ctx=UserCtx}=Db) ->
    case is_admin(Db) of
        true -> true;
        false ->
            case is_public_db(Db) of
                true -> true;
                false ->
                    {Members} = get_members(Db),
                    is_authorized(UserCtx, Members)
            end
    end.

is_public_db(#db{}=Db) ->
    {Members} = get_members(Db),
    Names = couch_util:get_value(<<"names">>, Members, []),
    Roles = couch_util:get_value(<<"roles">>, Members, []),
    Names =:= [] andalso Roles =:= [].

is_authorized(#user_ctx{name=UserName,roles=UserRoles}, Security) ->
    Names = couch_util:get_value(<<"names">>, Security, []),
    Roles = couch_util:get_value(<<"roles">>, Security, []),
    case check_security(roles, UserRoles, [<<"_admin">> | Roles]) of
        true -> true;
        false -> check_security(names, UserName, Names)
    end.

check_security(roles, [], _) ->
    false;
check_security(roles, UserRoles, Roles) ->
    UserRolesSet = ordsets:from_list(UserRoles),
    RolesSet = ordsets:from_list(Roles),
    not ordsets:is_disjoint(UserRolesSet, RolesSet);
check_security(names, _, []) ->
    false;
check_security(names, null, _) ->
    false;
check_security(names, UserName, Names) ->
    lists:member(UserName, Names).

throw_security_error(#user_ctx{name=null}=UserCtx) ->
    Reason = <<"You are not authorized to access this db.">>,
    throw_security_error(UserCtx, Reason);
throw_security_error(#user_ctx{name=_}=UserCtx) ->
    Reason = <<"You are not allowed to access this db.">>,
    throw_security_error(UserCtx, Reason).
throw_security_error(#user_ctx{}=UserCtx, Reason) ->
    Error = security_error_type(UserCtx),
    throw({Error, Reason}).

security_error_type(#user_ctx{name=null}) ->
    unauthorized;
security_error_type(#user_ctx{name=_}) ->
    forbidden.


get_admins(#db{security=SecProps}) ->
    couch_util:get_value(<<"admins">>, SecProps, {[]}).

get_members(#db{security=SecProps}) ->
    % we fallback to readers here for backwards compatibility
    couch_util:get_value(<<"members">>, SecProps,
        couch_util:get_value(<<"readers">>, SecProps, {[]})).

get_security(#db{security=SecProps}) ->
    {SecProps};
get_security(?NEW_PSE_DB = Db) ->
    {?PSE_DB_SECURITY(Db)}.

set_security(#db{main_pid=Pid}=Db, {NewSecProps}) when is_list(NewSecProps) ->
    check_is_admin(Db),
    ok = validate_security_object(NewSecProps),
    ok = gen_server:call(Pid, {set_security, NewSecProps}, infinity),
    {ok, _} = ensure_full_commit(Db),
    ok;
set_security(_, _) ->
    throw(bad_request).

set_user_ctx(#db{} = Db, UserCtx) ->
    {ok, Db#db{user_ctx = UserCtx}}.

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

set_revs_limit(#db{main_pid=Pid}=Db, Limit) when Limit > 0 ->
    check_is_admin(Db),
    gen_server:call(Pid, {set_revs_limit, Limit}, infinity);
set_revs_limit(_Db, _Limit) ->
    throw(invalid_revs_limit).

name(#db{name=Name}) ->
    Name;
name(?NEW_PSE_DB = Db) ->
    ?PSE_DB_NAME(Db).

compression(#db{compression=Compression}) ->
    Compression.

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
    % Here we're just asserting that our doc sort is stable so that
    % if we have duplicate docids we don't have to worry about the
    % behavior of lists:sort/2 which isn't documented anyhwere as
    % being stable.
    WithPos = lists:zip(Docs, lists:seq(1, length(Docs))),
    SortFun = fun({D1, P1}, {D2, P2}) -> {D1#doc.id, P1} =< {D2#doc.id, P2} end,
    SortedDocs = [D || {D, _} <- lists:sort(SortFun, WithPos)],
    group_alike_docs(SortedDocs, []).

group_alike_docs([], Buckets) ->
    lists:reverse(lists:map(fun lists:reverse/1, Buckets));
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

validate_doc_update(#db{}=Db, #doc{id= <<"_design/",_/binary>>}=Doc, _GetDiskDocFun) ->
    case catch check_is_admin(Db) of
        ok -> validate_ddoc(Db#db.name, Doc);
        Error -> Error
    end;
validate_doc_update(#db{validate_doc_funs = undefined} = Db, Doc, Fun) ->
    ValidationFuns = load_validation_funs(Db),
    validate_doc_update(Db#db{validate_doc_funs=ValidationFuns}, Doc, Fun);
validate_doc_update(#db{validate_doc_funs=[]}, _Doc, _GetDiskDocFun) ->
    ok;
validate_doc_update(_Db, #doc{id= <<"_local/",_/binary>>}, _GetDiskDocFun) ->
    ok;
validate_doc_update(Db, Doc, GetDiskDocFun) ->
    case get(io_priority) of
        {internal_repl, _} ->
            ok;
        _ ->
            validate_doc_update_int(Db, Doc, GetDiskDocFun)
    end.

validate_ddoc(DbName, DDoc) ->
    try
        ok = couch_index_server:validate(DbName, couch_doc:with_ejson_body(DDoc))
    catch
        throw:{invalid_design_doc, Reason} ->
            {bad_request, invalid_design_doc, Reason};
        throw:{compilation_error, Reason} ->
            {bad_request, compilation_error, Reason};
        throw:Error ->
            Error
    end.

validate_doc_update_int(Db, Doc, GetDiskDocFun) ->
    Fun = fun() ->
        DiskDoc = GetDiskDocFun(),
        JsonCtx = couch_util:json_user_ctx(Db),
        SecObj = get_security(Db),
        try
            [case Fun(Doc, DiskDoc, JsonCtx, SecObj) of
                ok -> ok;
                Error -> throw(Error)
             end || Fun <- Db#db.validate_doc_funs],
            ok
        catch
            throw:Error ->
                Error
        end
    end,
    couch_stats:update_histogram([couchdb, query_server, vdu_process_time],
                                 Fun).


% to be safe, spawn a middleman here
load_validation_funs(#db{main_pid=Pid, name = <<"shards/", _/binary>>}=Db) ->
    {_, Ref} = spawn_monitor(fun() ->
        exit(ddoc_cache:open(mem3:dbname(Db#db.name), validation_funs))
    end),
    receive
        {'DOWN', Ref, _, _, {ok, Funs}} ->
            gen_server:cast(Pid, {load_validation_funs, Funs}),
            Funs;
        {'DOWN', Ref, _, _, Reason} ->
            couch_log:error("could not load validation funs ~p", [Reason]),
            throw(internal_server_error)
    end;
load_validation_funs(#db{main_pid=Pid}=Db) ->
    {ok, DDocInfos} = get_design_docs(Db),
    OpenDocs = fun
        (#full_doc_info{}=D) ->
            {ok, Doc} = open_doc_int(Db, D, [ejson_body]),
            Doc
    end,
    DDocs = lists:map(OpenDocs, DDocInfos),
    Funs = lists:flatmap(fun(DDoc) ->
        case couch_doc:get_validate_doc_fun(DDoc) of
            nil -> [];
            Fun -> [Fun]
        end
    end, DDocs),
    gen_server:cast(Pid, {load_validation_funs, Funs}),
    Funs.

prep_and_validate_update(Db, #doc{id=Id,revs={RevStart, Revs}}=Doc,
        OldFullDocInfo, LeafRevsDict, AllowConflict) ->
    case Revs of
    [PrevRev|_] ->
        case dict:find({RevStart, PrevRev}, LeafRevsDict) of
        {ok, {#leaf{deleted=Deleted, ptr=DiskSp}, DiskRevs}} ->
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
    AccPrepped2 = lists:reverse(lists:map(fun lists:reverse/1, AccPrepped)),
    {AccPrepped2, AccFatalErrors};
prep_and_validate_updates(Db, [DocBucket|RestBuckets], [not_found|RestLookups],
        AllowConflict, AccPrepped, AccErrors) ->
    % no existing revs are known,
    {PreppedBucket, AccErrors3} = lists:foldl(
        fun(#doc{revs=Revs}=Doc, {AccBucket, AccErrors2}) ->
            case couch_doc:has_stubs(Doc) of
            true ->
                couch_doc:merge_stubs(Doc, #doc{}); % will throw exception
            false -> ok
            end,
            case Revs of
            {0, []} ->
                case validate_doc_update(Db, Doc, fun() -> nil end) of
                ok ->
                    {[Doc | AccBucket], AccErrors2};
                Error ->
                    {AccBucket, [{doc_tag(Doc), Error} | AccErrors2]}
                end;
            _ ->
                % old revs specified but none exist, a conflict
                {AccBucket, [{doc_tag(Doc), conflict} | AccErrors2]}
            end
        end,
        {[], AccErrors}, DocBucket),

    prep_and_validate_updates(Db, RestBuckets, RestLookups, AllowConflict,
            [PreppedBucket | AccPrepped], AccErrors3);
prep_and_validate_updates(Db, [DocBucket|RestBuckets],
        [{ok, #full_doc_info{rev_tree=OldRevTree}=OldFullDocInfo}|RestLookups],
        AllowConflict, AccPrepped, AccErrors) ->
    Leafs = couch_key_tree:get_all_leafs(OldRevTree),
    LeafRevsDict = dict:from_list([
        {{Start, RevId}, {Leaf, Revs}} ||
        {Leaf, {Start, [RevId | _]} = Revs} <- Leafs
    ]),
    {PreppedBucket, AccErrors3} = lists:foldl(
        fun(Doc, {Docs2Acc, AccErrors2}) ->
            case prep_and_validate_update(Db, Doc, OldFullDocInfo,
                    LeafRevsDict, AllowConflict) of
            {ok, Doc2} ->
                {[Doc2 | Docs2Acc], AccErrors2};
            {Error, _} ->
                % Record the error
                {Docs2Acc, [{doc_tag(Doc), Error} |AccErrors2]}
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
    AccPrepped2 = lists:reverse(lists:map(fun lists:reverse/1, AccPrepped)),
    {AccPrepped2, lists:reverse(Errors2)};
prep_and_validate_replicated_updates(Db, [Bucket|RestBuckets], [OldInfo|RestOldInfo], AccPrepped, AccErrors) ->
    case OldInfo of
    not_found ->
        {ValidatedBucket, AccErrors3} = lists:foldl(
            fun(Doc, {AccPrepped2, AccErrors2}) ->
                case couch_doc:has_stubs(Doc) of
                true ->
                    couch_doc:merge_stubs(Doc, #doc{}); % will throw exception
                false -> ok
                end,
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
        OldLeafs = couch_key_tree:get_all_leafs_full(OldTree),
        OldLeafsLU = [{Start, RevId} || {Start, [{RevId, _}|_]} <- OldLeafs],
        NewRevTree = lists:foldl(
            fun(NewDoc, AccTree) ->
                {NewTree, _} = couch_key_tree:merge(AccTree,
                    couch_doc:to_path(NewDoc), Db#db.revs_limit),
                NewTree
            end,
            OldTree, Bucket),
        Leafs = couch_key_tree:get_all_leafs_full(NewRevTree),
        LeafRevsFullDict = dict:from_list( [{{Start, RevId}, FullPath} || {Start, [{RevId, _}|_]}=FullPath <- Leafs]),
        {ValidatedBucket, AccErrors3} =
        lists:foldl(
            fun(#doc{id=Id,revs={Pos, [RevId|_]}}=Doc, {AccValidated, AccErrors2}) ->
                IsOldLeaf = lists:member({Pos, RevId}, OldLeafsLU),
                case dict:find({Pos, RevId}, LeafRevsFullDict) of
                {ok, {Start, Path}} when not IsOldLeaf ->
                    % our unflushed doc is a leaf node. Go back on the path
                    % to find the previous rev that's on disk.

                    LoadPrevRevFun = fun() ->
                                make_first_doc_on_disk(Db,Id,Start-1, tl(Path))
                            end,

                    case couch_doc:has_stubs(Doc) of
                    true ->
                        DiskDoc = case LoadPrevRevFun() of
                            #doc{} = DiskDoc0 ->
                                DiskDoc0;
                            _ ->
                                % Force a missing_stub exception
                                couch_doc:merge_stubs(Doc, #doc{})
                        end,
                        Doc2 = couch_doc:merge_stubs(Doc, DiskDoc),
                        GetDiskDocFun = fun() -> DiskDoc end;
                    false ->
                        Doc2 = Doc,
                        GetDiskDocFun = LoadPrevRevFun
                    end,

                    case validate_doc_update(Db, Doc2, GetDiskDocFun) of
                    ok ->
                        {[Doc2 | AccValidated], AccErrors2};
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



new_revid(#doc{body=Body0, revs={OldStart,OldRevs}, atts=Atts, deleted=Deleted}) ->
    DigestedAtts = lists:foldl(fun(Att, Acc) ->
        [N, T, M] = couch_att:fetch([name, type, md5], Att),
        case M == <<>> of
            true -> Acc;
            false -> [{N, T, M} | Acc]
        end
    end, [], Atts),
    Body = case Body0 of
        {summary, [_Len, _Md5, BodyAtts], _SizeInfo, _AttsFd} ->
            {CompBody, _CompAtts} = binary_to_term(BodyAtts),
            couch_compress:decompress(CompBody);
        {summary, [_Len, BodyAtts], _SizeInfo, _AttsFd} ->
            {CompBody, _CompAtts} = binary_to_term(BodyAtts),
            couch_compress:decompress(CompBody);
        Else ->
            Else
    end,
    case DigestedAtts of
        Atts2 when length(Atts) =/= length(Atts2) ->
            % We must have old style non-md5 attachments
            ?l2b(integer_to_list(couch_util:rand32()));
        Atts2 ->
            OldRev = case OldRevs of [] -> 0; [OldRev0|_] -> OldRev0 end,
            crypto:hash(md5, term_to_binary([Deleted, OldStart, OldRev, Body, Atts2], [{minor_version, 1}]))
    end.

new_revs([], OutBuckets, IdRevsAcc) ->
    {lists:reverse(OutBuckets), IdRevsAcc};
new_revs([Bucket|RestBuckets], OutBuckets, IdRevsAcc) ->
    {NewBucket, IdRevsAcc3} = lists:mapfoldl(
        fun(#doc{revs={Start, RevIds}}=Doc, IdRevsAcc2)->
        NewRevId = new_revid(Doc),
        {Doc#doc{revs={Start+1, [NewRevId | RevIds]}},
            [{doc_tag(Doc), {ok, {Start+1, NewRevId}}} | IdRevsAcc2]}
    end, IdRevsAcc, Bucket),
    new_revs(RestBuckets, [NewBucket|OutBuckets], IdRevsAcc3).

check_dup_atts(#doc{atts=Atts}=Doc) ->
    lists:foldl(fun(Att, Names) ->
        Name = couch_att:fetch(name, Att),
        case ordsets:is_element(Name, Names) of
            true -> throw({bad_request, <<"Duplicate attachments">>});
            false -> ordsets:add_element(Name, Names)
        end
    end, ordsets:new(), Atts),
    Doc.

tag_docs([]) ->
    [];
tag_docs([#doc{meta=Meta}=Doc | Rest]) ->
    [Doc#doc{meta=[{ref, make_ref()} | Meta]} | tag_docs(Rest)].

doc_tag(#doc{meta=Meta}) ->
    case lists:keyfind(ref, 1, Meta) of
        {ref, Ref} when is_reference(Ref) -> Ref;
        false -> throw(doc_not_tagged);
        Else -> throw({invalid_doc_tag, Else})
    end.

update_docs(Db, Docs0, Options, replicated_changes) ->
    increment_stat(Db, [couchdb, database_writes]),
    Docs = tag_docs(Docs0),
    DocBuckets = before_docs_update(Db, group_alike_docs(Docs)),

    case (Db#db.validate_doc_funs /= []) orelse
        lists:any(
            fun(#doc{id= <<?DESIGN_DOC_PREFIX, _/binary>>}) -> true;
            (#doc{atts=Atts}) ->
                Atts /= []
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
    DocBuckets4 = [[doc_flush_atts(check_dup_atts(Doc), Db#db.fd)
            || Doc <- Bucket] || Bucket <- DocBuckets3],
    {ok, []} = write_and_commit(Db, DocBuckets4, [], [merge_conflicts | Options]),
    {ok, DocErrors};

update_docs(Db, Docs0, Options, interactive_edit) ->
    increment_stat(Db, [couchdb, database_writes]),
    AllOrNothing = lists:member(all_or_nothing, Options),
    Docs = tag_docs(Docs0),

    % Separate _local docs from normal docs
    IsLocal = fun
        (#doc{id= <<?LOCAL_DOC_PREFIX, _/binary>>}) -> true;
        (_) -> false
    end,
    {NonRepDocs, Docs2} = lists:partition(IsLocal, Docs),

    DocBuckets = before_docs_update(Db, group_alike_docs(Docs2)),

    case (Db#db.validate_doc_funs /= []) orelse
        lists:any(
            fun(#doc{id= <<?DESIGN_DOC_PREFIX, _/binary>>}) ->
                true;
            (#doc{atts=Atts}) ->
                Atts /= []
            end, Docs2) of
    true ->
        % lookup the doc by id and get the most recent
        Ids = [Id || [#doc{id=Id}|_] <- DocBuckets],
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
        RefErrorDict = dict:from_list([{doc_tag(Doc), Doc} || Doc <- Docs]),
        {aborted, lists:map(fun({Ref, Error}) ->
            #doc{id=Id,revs={Start,RevIds}} = dict:fetch(Ref, RefErrorDict),
            case {Start, RevIds} of
                {Pos, [RevId | _]} -> {{Id, {Pos, RevId}}, Error};
                {0, []} -> {{Id, {0, <<>>}}, Error}
            end
        end, PreCommitFailures)};
    true ->
        Options2 = if AllOrNothing -> [merge_conflicts];
                true -> [] end ++ Options,
        DocBuckets3 = [[
                doc_flush_atts(set_new_att_revpos(
                        check_dup_atts(Doc)), Db#db.fd)
                || Doc <- B] || B <- DocBuckets2],
        {DocBuckets4, IdRevs} = new_revs(DocBuckets3, [], []),

        {ok, CommitResults} = write_and_commit(Db, DocBuckets4, NonRepDocs, Options2),

        ResultsDict = lists:foldl(fun({Key, Resp}, ResultsAcc) ->
            dict:store(Key, Resp, ResultsAcc)
        end, dict:from_list(IdRevs), CommitResults ++ PreCommitFailures),
        {ok, lists:map(fun(Doc) ->
            dict:fetch(doc_tag(Doc), ResultsDict)
        end, Docs)}
    end.

% Returns the first available document on disk. Input list is a full rev path
% for the doc.
make_first_doc_on_disk(_Db, _Id, _Pos, []) ->
    nil;
make_first_doc_on_disk(Db, Id, Pos, [{_Rev, #doc{}} | RestPath]) ->
    make_first_doc_on_disk(Db, Id, Pos-1, RestPath);
make_first_doc_on_disk(Db, Id, Pos, [{_Rev, ?REV_MISSING}|RestPath]) ->
    make_first_doc_on_disk(Db, Id, Pos - 1, RestPath);
make_first_doc_on_disk(Db, Id, Pos, [{_Rev, #leaf{deleted=IsDel, ptr=Sp}} |_]=DocPath) ->
    Revs = [Rev || {Rev, _} <- DocPath],
    make_doc(Db, Id, IsDel, Sp, {Pos, Revs}).

set_commit_option(Options) ->
    CommitSettings = {
        [true || O <- Options, O==full_commit orelse O==delay_commit],
        config:get("couchdb", "delayed_commits", "false")
    },
    case CommitSettings of
    {[true], _} ->
        Options; % user requested explicit commit setting, do not change it
    {_, "true"} ->
        Options; % delayed commits are enabled, do nothing
    {_, "false"} ->
        [full_commit|Options];
    {_, Else} ->
        couch_log:error("[couchdb] delayed_commits setting must be true/false,"
                        " not ~p", [Else]),
        [full_commit|Options]
    end.

collect_results_with_metrics(Pid, MRef, []) ->
    Begin = os:timestamp(),
    try
        collect_results(Pid, MRef, [])
    after
        ResultsTime = timer:now_diff(os:timestamp(), Begin) div 1000,
        couch_stats:update_histogram(
            [couchdb, collect_results_time],
            ResultsTime
        )
    end.

collect_results(Pid, MRef, ResultsAcc) ->
    receive
    {result, Pid, Result} ->
        collect_results(Pid, MRef, [Result | ResultsAcc]);
    {done, Pid} ->
        {ok, ResultsAcc};
    {retry, Pid} ->
        retry;
    {'DOWN', MRef, _, _, Reason} ->
        exit(Reason)
    end.

write_and_commit(#db{main_pid=Pid, user_ctx=Ctx}=Db, DocBuckets1,
        NonRepDocs, Options0) ->
    DocBuckets = prepare_doc_summaries(Db, DocBuckets1),
    Options = set_commit_option(Options0),
    MergeConflicts = lists:member(merge_conflicts, Options),
    FullCommit = lists:member(full_commit, Options),
    MRef = erlang:monitor(process, Pid),
    try
        Pid ! {update_docs, self(), DocBuckets, NonRepDocs, MergeConflicts, FullCommit},
        case collect_results_with_metrics(Pid, MRef, []) of
        {ok, Results} -> {ok, Results};
        retry ->
            % This can happen if the db file we wrote to was swapped out by
            % compaction. Retry by reopening the db and writing to the current file
            {ok, Db2} = open(Db#db.name, [{user_ctx, Ctx}]),
            DocBuckets2 = [
                [doc_flush_atts(Doc, Db2#db.fd) || Doc <- Bucket] ||
                Bucket <- DocBuckets1
            ],
            % We only retry once
            DocBuckets3 = prepare_doc_summaries(Db2, DocBuckets2),
            close(Db2),
            Pid ! {update_docs, self(), DocBuckets3, NonRepDocs, MergeConflicts, FullCommit},
            case collect_results_with_metrics(Pid, MRef, []) of
            {ok, Results} -> {ok, Results};
            retry -> throw({update_error, compaction_retry})
            end
        end
    after
        erlang:demonitor(MRef, [flush])
    end.


prepare_doc_summaries(Db, BucketList) ->
    [lists:map(
        fun(#doc{body = Body, atts = Atts} = Doc) ->
            DiskAtts = [couch_att:to_disk_term(Att) || Att <- Atts],
            {ok, SizeInfo} = couch_att:size_info(Atts),
            AttsFd = case Atts of
            [Att | _] ->
                {Fd, _} = couch_att:fetch(data, Att),
                Fd;
            [] ->
                nil
            end,
            SummaryChunk = couch_db_updater:make_doc_summary(Db, {Body, DiskAtts}),
            Meta = Doc#doc.meta,
            Doc#doc{
                body = {summary, SummaryChunk, SizeInfo, AttsFd},
                meta = [{ejson_size, ?term_size(Body)} | Meta]
            }
        end,
        Bucket) || Bucket <- BucketList].


before_docs_update(#db{} = Db, BucketList) ->
    [lists:map(
            fun(Doc) ->
                DocWithBody = couch_doc:with_ejson_body(Doc),
                couch_db_plugin:before_doc_update(Db, DocWithBody)
            end,
        Bucket) || Bucket <- BucketList].


set_new_att_revpos(#doc{revs={RevPos,_Revs},atts=Atts0}=Doc) ->
    Atts = lists:map(
        fun(Att) ->
            case couch_att:fetch(data, Att) of
                {_Fd, _Sp} -> Att; % already commited to disk, don't set new rev
                _ -> couch_att:store(revpos, RevPos+1, Att)
            end
        end, Atts0),
    Doc#doc{atts = Atts}.


doc_flush_atts(Doc, Fd) ->
    Doc#doc{atts=[couch_att:flush(Fd, Att) || Att <- Doc#doc.atts]}.

check_md5(_NewSig, <<>>) -> ok;
check_md5(Sig, Sig) -> ok;
check_md5(_, _) -> throw(md5_mismatch).


compressible_att_type(MimeType) when is_binary(MimeType) ->
    compressible_att_type(?b2l(MimeType));
compressible_att_type(MimeType) ->
    TypeExpList = re:split(
        config:get("attachments", "compressible_types", ""),
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
with_stream(Fd, Att, Fun) ->
    [InMd5, Type, Enc] = couch_att:fetch([md5, type, encoding], Att),
    BufferSize = list_to_integer(
        config:get("couchdb", "attachment_stream_buffer_size", "4096")),
    {ok, OutputStream} = case (Enc =:= identity) andalso
        compressible_att_type(Type) of
    true ->
        CompLevel = list_to_integer(
            config:get("attachments", "compression_level", "0")
        ),
        couch_stream:open(Fd, [{buffer_size, BufferSize},
            {encoding, gzip}, {compression_level, CompLevel}]);
    _ ->
        couch_stream:open(Fd, [{buffer_size, BufferSize}])
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
        case couch_att:fetch([att_len, disk_len], Att) of
            [AL, DL] when AL =:= undefined orelse DL =:= undefined ->
                % Compressed attachment uploaded through the standalone API.
                {Len, Len, gzip};
            [AL, DL] ->
                % This case is used for efficient push-replication, where a
                % compressed attachment is located in the body of multipart
                % content-type request.
                {AL, DL, gzip}
        end
    end,
    couch_att:store([
        {data, {Fd,StreamInfo}},
        {att_len, AttLen},
        {disk_len, DiskLen},
        {md5, Md5},
        {encoding, NewEnc}
    ], Att).


enum_docs_since_reduce_to_count(Reds) ->
    couch_btree:final_reduce(
            fun couch_db_updater:btree_by_seq_reduce/2, Reds).

enum_docs_reduce_to_count(Reds) ->
    FinalRed = couch_btree:final_reduce(
            fun couch_db_updater:btree_by_id_reduce/2, Reds),
    element(1, FinalRed).

changes_since(Db, StartSeq, Fun, Acc) ->
    changes_since(Db, StartSeq, Fun, [], Acc).

changes_since(Db, StartSeq, Fun, Options, Acc) when is_record(Db, db) ->
    changes_since(Db#db.seq_tree, StartSeq, Fun, Options, Acc);
changes_since(SeqTree, StartSeq, Fun, Options, Acc) ->
    Wrapper = fun(FullDocInfo, _Offset, Acc2) ->
        DocInfo = case FullDocInfo of
            #full_doc_info{} ->
                couch_doc:to_doc_info(FullDocInfo);
            #doc_info{} ->
                FullDocInfo
        end,
        Fun(DocInfo, Acc2)
    end,
    {ok, _LastReduction, AccOut} = couch_btree:fold(SeqTree,
        Wrapper, Acc, [{start_key, StartSeq + 1}] ++ Options),
    {ok, AccOut}.

count_changes_since(Db, SinceSeq) ->
    BTree = Db#db.seq_tree,
    {ok, Changes} =
    couch_btree:fold_reduce(BTree,
        fun(_SeqStart, PartialReds, 0) ->
            {ok, couch_btree:final_reduce(BTree, PartialReds)}
        end,
        0, [{start_key, SinceSeq + 1}]),
    Changes.

enum_docs_since(Db, SinceSeq, InFun, Acc, Options) ->
    {ok, LastReduction, AccOut} = couch_btree:fold(
        Db#db.seq_tree, InFun, Acc,
            [{start_key, SinceSeq + 1} | Options]),
    {ok, enum_docs_since_reduce_to_count(LastReduction), AccOut}.


fold_docs(Db, InFun, InAcc, Opts) ->
    Wrapper = fun(FDI, _, Acc) -> InFun(FDI, Acc) end,
    {ok, _, AccOut} = couch_btree:fold(Db#db.id_tree, Wrapper, InAcc, Opts),
    {ok, AccOut}.

fold_local_docs(Db, InFun, InAcc, Opts) ->
    Wrapper = fun(FDI, _, Acc) -> InFun(FDI, Acc) end,
    {ok, _, AccOut} = couch_btree:fold(Db#db.local_tree, Wrapper, InAcc, Opts),
    {ok, AccOut}.

enum_docs(Db, InFun, InAcc, Options0) ->
    {NS, Options} = extract_namespace(Options0),
    enum_docs(Db, NS, InFun, InAcc, Options).

enum_docs(Db, undefined, InFun, InAcc, Options) ->
    FoldFun = pipe([fun skip_deleted/4], InFun),
    {ok, LastReduce, OutAcc} = couch_btree:fold(
        Db#db.id_tree, FoldFun, InAcc, Options),
    {ok, enum_docs_reduce_to_count(LastReduce), OutAcc};
enum_docs(Db, <<"_local">>, InFun, InAcc, Options) ->
    FoldFun = pipe([fun skip_deleted/4], InFun),
    {ok, _LastReduce, OutAcc} = couch_btree:fold(
        Db#db.local_tree, FoldFun, InAcc, Options),
    {ok, null, OutAcc};
enum_docs(Db, NS, InFun, InAcc, Options0) ->
    FoldFun = pipe([
        fun skip_deleted/4,
        stop_on_leaving_namespace(NS)], InFun),
    Options = set_namespace_range(Options0, NS),
    {ok, LastReduce, OutAcc} = couch_btree:fold(
        Db#db.id_tree, FoldFun, InAcc, Options),
    {ok, enum_docs_reduce_to_count(LastReduce), OutAcc}.


calculate_start_seq(_Db, _Node, Seq) when is_integer(Seq) ->
    Seq;
calculate_start_seq(Db, Node, {Seq, Uuid}) ->
    % Treat the current node as the epoch node
    calculate_start_seq(Db, Node, {Seq, Uuid, Node});
calculate_start_seq(Db, _Node, {Seq, Uuid, EpochNode}) ->
    case is_prefix(Uuid, get_uuid(Db)) of
        true ->
            case is_owner(EpochNode, Seq, get_epochs(Db)) of
                true -> Seq;
                false -> 0
            end;
        false ->
            %% The file was rebuilt, most likely in a different
            %% order, so rewind.
            0
    end;
calculate_start_seq(Db, _Node, {replace, OriginalNode, Uuid, Seq}) ->
    case is_prefix(Uuid, couch_db:get_uuid(Db)) of
        true ->
            start_seq(get_epochs(Db), OriginalNode, Seq);
        false ->
            {replace, OriginalNode, Uuid, Seq}
    end.


validate_epochs(Epochs) ->
    %% Assert uniqueness.
    case length(Epochs) == length(lists:ukeysort(2, Epochs)) of
        true  -> ok;
        false -> erlang:error(duplicate_epoch)
    end,
    %% Assert order.
    case Epochs == lists:sort(fun({_, A}, {_, B}) -> B =< A end, Epochs) of
        true  -> ok;
        false -> erlang:error(epoch_order)
    end.


is_prefix(Pattern, Subject) ->
     binary:longest_common_prefix([Pattern, Subject]) == size(Pattern).


is_owner(Node, Seq, Epochs) ->
    Node =:= owner_of(Epochs, Seq).


owner_of(Db, Seq) when not is_list(Db) ->
    owner_of(get_epochs(Db), Seq);
owner_of([], _Seq) ->
    undefined;
owner_of([{EpochNode, EpochSeq} | _Rest], Seq) when Seq > EpochSeq ->
    EpochNode;
owner_of([_ | Rest], Seq) ->
    owner_of(Rest, Seq).


start_seq([{OrigNode, EpochSeq} | _], OrigNode, Seq) when Seq > EpochSeq ->
    %% OrigNode is the owner of the Seq so we can safely stream from there
    Seq;
start_seq([{_, NewSeq}, {OrigNode, _} | _], OrigNode, Seq) when Seq > NewSeq ->
    %% We transferred this file before Seq was written on OrigNode, so we need
    %% to stream from the beginning of the next epoch. Note that it is _not_
    %% necessary for the current node to own the epoch beginning at NewSeq
    NewSeq;
start_seq([_ | Rest], OrigNode, Seq) ->
    start_seq(Rest, OrigNode, Seq);
start_seq([], OrigNode, Seq) ->
    erlang:error({epoch_mismatch, OrigNode, Seq}).


extract_namespace(Options0) ->
    case proplists:split(Options0, [namespace]) of
        {[[{namespace, NS}]], Options} ->
            {NS, Options};
        {_, Options} ->
            {undefined, Options}
    end.

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
                    #leaf{deleted=IsDeleted, ptr=SummaryPtr} ->
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
    case couch_btree:lookup(Db#db.local_tree, [Id]) of
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
            ({Rev1, Leaf}) ->
                case Leaf#leaf.deleted of
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


make_doc(_Db, Id, Deleted, nil = _Bp, RevisionPath) ->
    #doc{
        id = Id,
        revs = RevisionPath,
        body = [],
        atts = [],
        deleted = Deleted
    };
make_doc(#db{fd=Fd, revs_limit=RevsLimit}=Db, Id, Deleted, Bp, {Pos, Revs}) ->
    {BodyData, Atts0} = case Bp of
        nil ->
            {[], []};
        _ ->
            case read_doc(Db, Bp) of
                {ok, {BodyData0, Atts1}} when is_binary(Atts1) ->
                    {BodyData0, couch_compress:decompress(Atts1)};
                {ok, {BodyData0, Atts1}} when is_list(Atts1) ->
                    % pre 1.2 format
                    {BodyData0, Atts1}
            end
    end,
    Atts = [couch_att:from_disk_term(Fd, T) || T <- Atts0],
    Doc = #doc{
        id = Id,
        revs = {Pos, lists:sublist(Revs, 1, RevsLimit)},
        body = BodyData,
        atts = Atts,
        deleted = Deleted
    },
    after_doc_read(Db, Doc).


after_doc_read(#db{} = Db, Doc) ->
    DocWithBody = couch_doc:with_ejson_body(Doc),
    couch_db_plugin:after_doc_read(Db, DocWithBody).

increment_stat(#db{options = Options}, Stat) ->
    case lists:member(sys_db, Options) of
    true ->
        ok;
    false ->
        couch_stats:increment_counter(Stat)
    end.

skip_deleted(traverse, LK, {Undeleted, _, _} = Reds, Acc) when Undeleted == 0 ->
    {skip, LK, Reds, Acc};
skip_deleted(Case, A, B, C) ->
    {Case, A, B, C}.

stop_on_leaving_namespace(NS) ->
    fun
        (visit, #full_doc_info{id = Key} = FullInfo, Reds, Acc) ->
            case has_prefix(Key, NS) of
                true ->
                    {visit, FullInfo, Reds, Acc};
                false ->
                    {stop, FullInfo, Reds, Acc}
            end;
        (Case, KV, Reds, Acc) ->
            {Case, KV, Reds, Acc}
    end.

has_prefix(Bin, Prefix) ->
    S = byte_size(Prefix),
    case Bin of
        <<Prefix:S/binary, "/", _/binary>> ->
            true;
        _Else ->
            false
    end.

pipe(Filters, Final) ->
    Wrap =
        fun
            (visit, KV, Reds, Acc) ->
                Final(KV, Reds, Acc);
            (skip, _KV, _Reds, Acc) ->
                {skip, Acc};
            (stop, _KV, _Reds, Acc) ->
                {stop, Acc};
            (traverse, _, _, Acc) ->
                {ok, Acc}
        end,
    do_pipe(Filters, Wrap).

do_pipe([], Fun) -> Fun;
do_pipe([Filter|Rest], F0) ->
    F1 = fun(C0, KV0, Reds0, Acc0) ->
        {C, KV, Reds, Acc} = Filter(C0, KV0, Reds0, Acc0),
        F0(C, KV, Reds, Acc)
    end,
    do_pipe(Rest, F1).

set_namespace_range(Options, undefined) -> Options;
set_namespace_range(Options, NS) ->
    %% FIXME depending on order we might need to swap keys
    SK = select_gt(
           proplists:get_value(start_key, Options, <<"">>),
           <<NS/binary, "/">>),
    EK = select_lt(
           proplists:get_value(end_key, Options, <<NS/binary, "0">>),
           <<NS/binary, "0">>),
    [{start_key, SK}, {end_key_gt, EK}].

select_gt(V1, V2) when V1 < V2 -> V2;
select_gt(V1, _V2) -> V1.

select_lt(V1, V2) when V1 > V2 -> V2;
select_lt(V1, _V2) -> V1.

-spec normalize_dbname(list() | binary()) -> binary().

normalize_dbname(DbName) when is_list(DbName) ->
    normalize_dbname(list_to_binary(DbName));
normalize_dbname(DbName) when is_binary(DbName) ->
    mem3:dbname(couch_util:drop_dot_couch_ext(DbName)).


-spec dbname_suffix(list() | binary()) -> binary().

dbname_suffix(DbName) ->
    filename:basename(normalize_dbname(DbName)).


validate_dbname(DbName) when is_list(DbName) ->
    validate_dbname(?l2b(DbName));
validate_dbname(DbName) when is_binary(DbName) ->
    Normalized = normalize_dbname(DbName),
    couch_db_plugin:validate_dbname(
        DbName, Normalized, fun validate_dbname_int/2).

validate_dbname_int(DbName, Normalized) when is_binary(DbName) ->
    DbNoExt = couch_util:drop_dot_couch_ext(DbName),
    case re:run(DbNoExt, ?DBNAME_REGEX, [{capture,none}, dollar_endonly]) of
        match ->
            ok;
        nomatch ->
            case is_systemdb(Normalized) of
                true -> ok;
                false -> {error, {illegal_database_name, DbName}}
            end
    end.

is_systemdb(DbName) when is_list(DbName) ->
    is_systemdb(?l2b(DbName));
is_systemdb(DbName) when is_binary(DbName) ->
    lists:member(dbname_suffix(DbName), ?SYSTEM_DATABASES).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    ok = meck:new(couch_epi, [passthrough]),
    ok = meck:expect(couch_epi, decide, fun(_, _, _, _, _) -> no_decision end),
    ok.

teardown(_) ->
    (catch meck:unload(couch_epi)).

validate_dbname_success_test_() ->
    Cases =
        generate_cases_with_shards("long/co$mplex-/path+/something")
        ++ generate_cases_with_shards("something")
        ++ lists:append(
            [generate_cases_with_shards(?b2l(SystemDb))
                || SystemDb <- ?SYSTEM_DATABASES]),
    {
        foreach, fun setup/0, fun teardown/1,
        [should_pass_validate_dbname(A) || {_, A} <- Cases]
    }.

validate_dbname_fail_test_() ->
    Cases = generate_cases("_long/co$mplex-/path+/_something")
       ++ generate_cases("_something")
       ++ generate_cases_with_shards("long/co$mplex-/path+/_something#")
       ++ generate_cases_with_shards("long/co$mplex-/path+/some.thing"),
    {
        foreach, fun setup/0, fun teardown/1,
        [should_fail_validate_dbname(A) || {_, A} <- Cases]
    }.

normalize_dbname_test_() ->
    Cases = generate_cases_with_shards("long/co$mplex-/path+/_something")
       ++ generate_cases_with_shards("_something"),
    WithExpected = [{?l2b(filename:rootname(A)), B} || {A, B} <- Cases],
    [{test_name({Expected, Db}), ?_assertEqual(Expected, normalize_dbname(Db))}
        || {Expected, Db} <- WithExpected].

dbname_suffix_test_() ->
    Cases = generate_cases_with_shards("long/co$mplex-/path+/_something")
       ++ generate_cases_with_shards("_something"),
    WithExpected = [{?l2b(filename:basename(Arg)), Db} || {Arg, Db} <- Cases],
    [{test_name({Expected, Db}), ?_assertEqual(Expected, dbname_suffix(Db))}
        || {Expected, Db} <- WithExpected].

is_systemdb_test_() ->
    Cases = lists:append([
        generate_cases_with_shards("long/co$mplex-/path+/" ++ ?b2l(Db))
            || Db <- ?SYSTEM_DATABASES]
        ++ [generate_cases_with_shards(?b2l(Db)) || Db <- ?SYSTEM_DATABASES
    ]),
    WithExpected = [{?l2b(filename:basename(filename:rootname(Arg))), Db}
        || {Arg, Db} <- Cases],
    [{test_name({Expected, Db}) ++ " in ?SYSTEM_DATABASES",
        ?_assert(is_systemdb(Db))} || {Expected, Db} <- WithExpected].

should_pass_validate_dbname(DbName) ->
    {test_name(DbName), ?_assertEqual(ok, validate_dbname(DbName))}.

should_fail_validate_dbname(DbName) ->
    {test_name(DbName), ?_test(begin
        Result = validate_dbname(DbName),
        ?assertMatch({error, {illegal_database_name, _}}, Result),
        {error, {illegal_database_name, FailedDbName}} = Result,
        ?assertEqual(to_binary(DbName), FailedDbName),
        ok
    end)}.

calculate_start_seq_test() ->
    %% uuid mismatch is always a rewind.
    Hdr1 = couch_db_header:new(),
    Hdr2 = couch_db_header:set(Hdr1, [{epochs, [{node1, 1}]}, {uuid, <<"uuid1">>}]),
    ?assertEqual(0, calculate_start_seq(#db{header=Hdr2}, node1, {1, <<"uuid2">>})),
    %% uuid matches and seq is owned by node.
    Hdr3 = couch_db_header:set(Hdr2, [{epochs, [{node1, 1}]}]),
    ?assertEqual(2, calculate_start_seq(#db{header=Hdr3}, node1, {2, <<"uuid1">>})),
    %% uuids match but seq is not owned by node.
    Hdr4 = couch_db_header:set(Hdr2, [{epochs, [{node2, 2}, {node1, 1}]}]),
    ?assertEqual(0, calculate_start_seq(#db{header=Hdr4}, node1, {3, <<"uuid1">>})),
    %% return integer if we didn't get a vector.
    ?assertEqual(4, calculate_start_seq(#db{}, foo, 4)).

is_owner_test() ->
    ?assertNot(is_owner(foo, 1, [])),
    ?assertNot(is_owner(foo, 1, [{foo, 1}])),
    ?assert(is_owner(foo, 2, [{foo, 1}])),
    ?assert(is_owner(foo, 50, [{bar, 100}, {foo, 1}])),
    ?assert(is_owner(foo, 50, [{baz, 200}, {bar, 100}, {foo, 1}])),
    ?assert(is_owner(bar, 150, [{baz, 200}, {bar, 100}, {foo, 1}])),
    ?assertError(duplicate_epoch, validate_epochs([{foo, 1}, {bar, 1}])),
    ?assertError(epoch_order, validate_epochs([{foo, 100}, {bar, 200}])).

to_binary(DbName) when is_list(DbName) ->
    ?l2b(DbName);
to_binary(DbName) when is_binary(DbName) ->
    DbName.

test_name({Expected, DbName}) ->
    lists:flatten(io_lib:format("~p -> ~p", [DbName, Expected]));
test_name(DbName) ->
    lists:flatten(io_lib:format("~p", [DbName])).

generate_cases_with_shards(DbName) ->
    DbNameWithShard = add_shard(DbName),
    DbNameWithShardAndExtension = add_shard(DbName) ++ ".couch",
    Cases = [
        DbName, ?l2b(DbName),
        DbNameWithShard, ?l2b(DbNameWithShard),
        DbNameWithShardAndExtension, ?l2b(DbNameWithShardAndExtension)
    ],
    [{DbName, Case} || Case <- Cases].

add_shard(DbName) ->
    "shards/00000000-3fffffff/" ++ DbName ++ ".1415960794".

generate_cases(DbName) ->
    [{DbName, DbName}, {DbName, ?l2b(DbName)}].

-endif.
