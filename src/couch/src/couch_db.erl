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
    exists/1,

    clustered_db/2,
    clustered_db/3,

    monitor/1,
    monitored_by/1,
    is_idle/1,

    is_admin/1,
    check_is_admin/1,
    check_is_member/1,

    name/1,
    get_after_doc_read_fun/1,
    get_before_doc_update_fun/1,
    get_committed_update_seq/1,
    get_compacted_seq/1,
    get_compactor_pid/1,
    get_compactor_pid_sync/1,
    get_db_info/1,
    get_partition_info/2,
    get_del_doc_count/1,
    get_doc_count/1,
    get_epochs/1,
    get_filepath/1,
    get_instance_start_time/1,
    get_pid/1,
    get_revs_limit/1,
    get_security/1,
    get_update_seq/1,
    get_user_ctx/1,
    get_uuid/1,
    get_purge_seq/1,
    get_oldest_purge_seq/1,
    get_purge_infos_limit/1,

    is_db/1,
    is_system_db/1,
    is_clustered/1,
    is_system_db_name/1,
    is_partitioned/1,

    set_revs_limit/2,
    set_purge_infos_limit/2,
    set_security/2,
    set_user_ctx/2,

    load_validation_funs/1,
    reload_validation_funs/1,

    open_doc/2,
    open_doc/3,
    open_doc_revs/4,
    open_doc_int/3,
    get_doc_info/2,
    get_full_doc_info/2,
    get_full_doc_infos/2,
    get_missing_revs/2,
    get_design_doc/2,
    get_design_docs/1,
    get_design_doc_count/1,
    get_purge_infos/2,

    get_minimum_purge_seq/1,
    purge_client_exists/3,

    validate_docid/2,
    doc_from_json_obj_validate/2,

    update_doc/3,
    update_doc/4,
    update_docs/4,
    update_docs/2,
    update_docs/3,
    delete_doc/3,

    purge_docs/2,
    purge_docs/3,

    with_stream/3,
    open_write_stream/2,
    open_read_stream/2,
    is_active_stream/2,

    fold_docs/3,
    fold_docs/4,
    fold_local_docs/4,
    fold_design_docs/4,
    fold_changes/4,
    fold_changes/5,
    count_changes_since/2,
    fold_purge_infos/4,
    fold_purge_infos/5,

    raft_insert/2,
    raft_lookup/2,
    raft_discard/2,
    raft_last/1,

    calculate_start_seq/3,
    owner_of/2,

    start_compact/1,
    cancel_compact/1,
    wait_for_compaction/1,
    wait_for_compaction/2,
    is_compacting/1,

    dbname_suffix/1,
    normalize_dbname/1,
    validate_dbname/1,

    make_doc/5,
    new_revid/1
]).

-export([
    start_link/4
]).

-include_lib("couch/include/couch_db.hrl").
-include("couch_db_int.hrl").

-define(DBNAME_REGEX,
    % use the stock CouchDB regex
    "^[a-z][a-z0-9\\_\\$()\\+\\-\\/]*"
    % but allow an optional shard timestamp at the end
    "(\\.[0-9]{10,})?$"
).
-define(DEFAULT_COMPRESSIBLE_TYPES,
    "text/*, application/javascript, application/json, application/xml"
).

start_link(Engine, DbName, Filepath, Options) ->
    Arg = {Engine, DbName, Filepath, Options},
    proc_lib:start_link(couch_db_updater, init, [Arg]).

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
        Else ->
            Else
    end.

reopen(#db{} = Db) ->
    % We could have just swapped out the storage engine
    % for this database during a compaction so we just
    % reimplement this as a close/open pair now.
    try
        open(Db#db.name, [{user_ctx, Db#db.user_ctx} | Db#db.options])
    after
        close(Db)
    end.

% You shouldn't call this. Its part of the ref counting between
% couch_server and couch_db instances.
incref(#db{} = Db) ->
    couch_db_engine:incref(Db).

clustered_db(DbName, Options) when is_list(Options) ->
    UserCtx = couch_util:get_value(user_ctx, Options, #user_ctx{}),
    SecProps = couch_util:get_value(security, Options, []),
    Props = couch_util:get_value(props, Options, []),
    {ok, #db{
        name = DbName,
        user_ctx = UserCtx,
        security = SecProps,
        options = [{props, Props}]
    }};
clustered_db(DbName, #user_ctx{} = UserCtx) ->
    clustered_db(DbName, [{user_ctx, UserCtx}]).

clustered_db(DbName, UserCtx, SecProps) ->
    clustered_db(DbName, [{user_ctx, UserCtx}, {security, SecProps}]).

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
is_clustered(?OLD_DB_REC = Db) ->
    ?OLD_DB_MAIN_PID(Db) == undefined.

is_partitioned(#db{options = Options}) ->
    Props = couch_util:get_value(props, Options, []),
    couch_util:get_value(partitioned, Props, false).

close(#db{} = Db) ->
    ok = couch_db_engine:decref(Db);
close(?OLD_DB_REC) ->
    ok.

exists(DbName) ->
    couch_server:exists(DbName).

is_idle(#db{compactor_pid = nil} = Db) ->
    monitored_by(Db) == [];
is_idle(_Db) ->
    false.

monitored_by(Db) ->
    case couch_db_engine:monitored_by(Db) of
        Pids when is_list(Pids) ->
            PidTracker = whereis(couch_stats_process_tracker),
            Pids -- [Db#db.main_pid, PidTracker];
        undefined ->
            []
    end.

monitor(#db{main_pid = MainPid}) ->
    erlang:monitor(process, MainPid).

start_compact(#db{} = Db) ->
    gen_server:call(Db#db.main_pid, start_compact).

cancel_compact(#db{main_pid = Pid}) ->
    gen_server:call(Pid, cancel_compact).

wait_for_compaction(Db) ->
    wait_for_compaction(Db, infinity).

wait_for_compaction(#db{main_pid = Pid} = Db, Timeout) ->
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

is_compacting(DbName) ->
    couch_server:is_compacting(DbName).

delete_doc(Db, Id, Revisions) ->
    DeletedDocs = [#doc{id = Id, revs = [Rev], deleted = true} || Rev <- Revisions],
    {ok, [Result]} = update_docs(Db, DeletedDocs, []),
    {ok, Result}.

open_doc(Db, IdOrDocInfo) ->
    open_doc(Db, IdOrDocInfo, []).

open_doc(Db, Id, Options) ->
    increment_stat(Db, [couchdb, database_reads]),
    case open_doc_int(Db, Id, Options) of
        {ok, #doc{deleted = true} = Doc} ->
            case lists:member(deleted, Options) of
                true ->
                    apply_open_options({ok, Doc}, Options);
                false ->
                    {not_found, deleted}
            end;
        Else ->
            apply_open_options(Else, Options)
    end.

apply_open_options({ok, Doc}, Options) ->
    apply_open_options2(Doc, Options);
apply_open_options(Else, _Options) ->
    Else.

apply_open_options2(Doc, []) ->
    {ok, Doc};
apply_open_options2(
    #doc{atts = Atts0, revs = Revs} = Doc,
    [{atts_since, PossibleAncestors} | Rest]
) ->
    RevPos = find_ancestor_rev_pos(Revs, PossibleAncestors),
    Atts = lists:map(
        fun(Att) ->
            [AttPos, Data] = couch_att:fetch([revpos, data], Att),
            if
                AttPos > RevPos -> couch_att:store(data, Data, Att);
                true -> couch_att:store(data, stub, Att)
            end
        end,
        Atts0
    ),
    apply_open_options2(Doc#doc{atts = Atts}, Rest);
apply_open_options2(Doc, [ejson_body | Rest]) ->
    apply_open_options2(couch_doc:with_ejson_body(Doc), Rest);
apply_open_options2(Doc, [_ | Rest]) ->
    apply_open_options2(Doc, Rest).

find_ancestor_rev_pos({_, []}, _AttsSinceRevs) ->
    0;
find_ancestor_rev_pos(_DocRevs, []) ->
    0;
find_ancestor_rev_pos({RevPos, [RevId | Rest]}, AttsSinceRevs) ->
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
% if no revs are missing, MissingRevs is []
get_missing_revs(Db, IdRevsList) ->
    FDIs = get_full_doc_infos(Db, [Id || {Id, _Revs} <- IdRevsList]),
    Results = lists:zipwith(
        fun
            ({Id, Revs}, #full_doc_info{rev_tree = RevTree} = FDI) ->
                MissingRevs = couch_key_tree:find_missing(RevTree, Revs),
                {Id, MissingRevs, possible_ancestors(FDI, MissingRevs)};
            ({Id, Revs}, not_found) ->
                {Id, Revs, []}
        end,
        IdRevsList,
        FDIs
    ),
    {ok, Results}.

get_doc_info(Db, Id) ->
    case get_full_doc_info(Db, Id) of
        #full_doc_info{} = FDI ->
            {ok, couch_doc:to_doc_info(FDI)};
        Else ->
            Else
    end.

get_full_doc_info(Db, Id) ->
    [Result] = get_full_doc_infos(Db, [Id]),
    Result.

get_full_doc_infos(Db, Ids) ->
    couch_db_engine:open_docs(Db, Ids).

purge_docs(Db, IdRevs) ->
    purge_docs(Db, IdRevs, []).

-spec purge_docs(#db{}, [{UUId, Id, [Rev]}], [PurgeOption]) ->
    {ok, [Reply]}
when
    UUId :: binary(),
    Id :: binary() | list(),
    Rev :: {non_neg_integer(), binary()},
    PurgeOption :: interactive_edit | replicated_changes,
    Reply :: {ok, []} | {ok, [Rev]}.
purge_docs(#db{main_pid = Pid} = Db, UUIDsIdsRevs, Options) ->
    UUIDsIdsRevs2 = [
        {UUID, couch_util:to_binary(Id), Revs}
     || {UUID, Id, Revs} <- UUIDsIdsRevs
    ],
    % Check here if any UUIDs already exist when
    % we're not replicating purge infos
    IsRepl = lists:member(replicated_changes, Options),
    if
        IsRepl ->
            ok;
        true ->
            UUIDs = [UUID || {UUID, _, _} <- UUIDsIdsRevs2],
            lists:foreach(
                fun(Resp) ->
                    if
                        Resp == not_found ->
                            ok;
                        true ->
                            Fmt = "Duplicate purge info UIUD: ~s",
                            Reason = io_lib:format(Fmt, [element(2, Resp)]),
                            throw({badreq, Reason})
                    end
                end,
                get_purge_infos(Db, UUIDs)
            )
    end,
    increment_stat(Db, [couchdb, database_purges]),
    gen_server:call(Pid, {purge_docs, UUIDsIdsRevs2, Options}).

-spec get_purge_infos(#db{}, [UUId]) -> [PurgeInfo] when
    UUId :: binary(),
    PurgeInfo :: {PurgeSeq, UUId, Id, [Rev]} | not_found,
    PurgeSeq :: non_neg_integer(),
    Id :: binary(),
    Rev :: {non_neg_integer(), binary()}.
get_purge_infos(Db, UUIDs) ->
    couch_db_engine:load_purge_infos(Db, UUIDs).

get_minimum_purge_seq(#db{} = Db) ->
    PurgeSeq = couch_db_engine:get_purge_seq(Db),
    OldestPurgeSeq = couch_db_engine:get_oldest_purge_seq(Db),
    PurgeInfosLimit = couch_db_engine:get_purge_infos_limit(Db),

    FoldFun = fun(#doc{id = DocId, body = {Props}}, SeqAcc) ->
        case DocId of
            <<?LOCAL_DOC_PREFIX, "purge-", _/binary>> ->
                ClientSeq = couch_util:get_value(<<"purge_seq">>, Props),
                DbName = couch_db:name(Db),
                % If there's a broken doc we have to keep every
                % purge info until the doc is fixed or removed.
                Fmt = "Invalid purge doc '~s' on ~p with purge_seq '~w'",
                case ClientSeq of
                    CS when is_integer(CS), CS >= PurgeSeq - PurgeInfosLimit ->
                        {ok, SeqAcc};
                    CS when is_integer(CS) ->
                        case purge_client_exists(DbName, DocId, Props) of
                            true ->
                                {ok, erlang:min(CS, SeqAcc)};
                            false ->
                                couch_log:error(Fmt, [DocId, DbName, ClientSeq]),
                                {ok, SeqAcc}
                        end;
                    _ ->
                        couch_log:error(Fmt, [DocId, DbName, ClientSeq]),
                        {ok, erlang:min(OldestPurgeSeq, SeqAcc)}
                end;
            _ ->
                {stop, SeqAcc}
        end
    end,
    InitMinSeq = PurgeSeq - PurgeInfosLimit,
    Opts = [
        {start_key, list_to_binary(?LOCAL_DOC_PREFIX ++ "purge-")}
    ],
    {ok, MinIdxSeq} = couch_db:fold_local_docs(Db, FoldFun, InitMinSeq, Opts),
    FinalSeq =
        case MinIdxSeq < PurgeSeq - PurgeInfosLimit of
            true -> MinIdxSeq;
            false -> erlang:max(0, PurgeSeq - PurgeInfosLimit)
        end,
    % Log a warning if we've got a purge sequence exceeding the
    % configured threshold.
    if
        FinalSeq >= (PurgeSeq - PurgeInfosLimit) ->
            ok;
        true ->
            Fmt = "The purge sequence for '~s' exceeds configured threshold",
            couch_log:warning(Fmt, [couch_db:name(Db)])
    end,
    FinalSeq.

purge_client_exists(DbName, DocId, Props) ->
    % Warn about clients that have not updated their purge
    % checkpoints in the last "index_lag_warn_seconds"
    LagWindow = config:get_integer(
        % Default 24 hours
        "purge",
        "index_lag_warn_seconds",
        86400
    ),

    {Mega, Secs, _} = os:timestamp(),
    NowSecs = Mega * 1000000 + Secs,
    LagThreshold = NowSecs - LagWindow,

    try
        Exists = couch_db_plugin:is_valid_purge_client(DbName, Props),
        if
            not Exists ->
                ok;
            true ->
                Updated = couch_util:get_value(<<"updated_on">>, Props),
                if
                    is_integer(Updated) and Updated > LagThreshold ->
                        ok;
                    true ->
                        Diff = NowSecs - Updated,
                        Fmt1 =
                            "Purge checkpoint '~s' not updated in ~p seconds\n"
                            "                    in database ~p",
                        couch_log:error(Fmt1, [DocId, Diff, DbName])
                end
        end,
        Exists
    catch
        _:_ ->
            % If we fail to check for a client we have to assume that
            % it exists.
            Fmt2 =
                "Failed to check purge checkpoint using\n"
                "            document '~p' in database ~p",
            couch_log:error(Fmt2, [DocId, DbName]),
            true
    end.

set_purge_infos_limit(#db{main_pid = Pid} = Db, Limit) when Limit > 0 ->
    check_is_admin(Db),
    gen_server:call(Pid, {set_purge_infos_limit, Limit}, infinity);
set_purge_infos_limit(_Db, _Limit) ->
    throw(invalid_purge_infos_limit).

get_after_doc_read_fun(#db{after_doc_read = Fun}) ->
    Fun.

get_before_doc_update_fun(#db{before_doc_update = Fun}) ->
    Fun.

get_committed_update_seq(#db{committed_update_seq = Seq}) ->
    Seq.

get_update_seq(#db{} = Db) ->
    couch_db_engine:get_update_seq(Db).

get_user_ctx(#db{user_ctx = UserCtx}) ->
    UserCtx;
get_user_ctx(?OLD_DB_REC = Db) ->
    ?OLD_DB_USER_CTX(Db).

get_purge_seq(#db{} = Db) ->
    couch_db_engine:get_purge_seq(Db).

get_oldest_purge_seq(#db{} = Db) ->
    couch_db_engine:get_oldest_purge_seq(Db).

get_purge_infos_limit(#db{} = Db) ->
    couch_db_engine:get_purge_infos_limit(Db).

get_pid(#db{main_pid = Pid}) ->
    Pid.

get_del_doc_count(Db) ->
    {ok, couch_db_engine:get_del_doc_count(Db)}.

get_doc_count(Db) ->
    {ok, couch_db_engine:get_doc_count(Db)}.

get_uuid(#db{} = Db) ->
    couch_db_engine:get_uuid(Db).

get_epochs(#db{} = Db) ->
    Epochs = couch_db_engine:get_epochs(Db),
    validate_epochs(Epochs),
    Epochs.

get_filepath(#db{filepath = FilePath}) ->
    FilePath.

get_instance_start_time(#db{instance_start_time = IST}) ->
    IST.

get_compacted_seq(#db{} = Db) ->
    couch_db_engine:get_compacted_seq(Db).

get_compactor_pid(#db{compactor_pid = Pid}) ->
    Pid.

get_compactor_pid_sync(#db{main_pid = Pid}) ->
    case gen_server:call(Pid, compactor_pid, infinity) of
        CPid when is_pid(CPid) ->
            CPid;
        _ ->
            nil
    end.

get_db_info(Db) ->
    #db{
        name = Name,
        compactor_pid = Compactor,
        instance_start_time = StartTime,
        committed_update_seq = CommittedUpdateSeq
    } = Db,
    {ok, DocCount} = get_doc_count(Db),
    {ok, DelDocCount} = get_del_doc_count(Db),
    SizeInfo = couch_db_engine:get_size_info(Db),
    DiskVersion = couch_db_engine:get_disk_version(Db),
    Uuid =
        case get_uuid(Db) of
            undefined -> null;
            Uuid0 -> Uuid0
        end,
    CompactedSeq =
        case get_compacted_seq(Db) of
            undefined -> null;
            Else1 -> Else1
        end,
    Props =
        case couch_db_engine:get_props(Db) of
            undefined -> null;
            Else2 -> {Else2}
        end,
    InfoList = [
        {db_name, Name},
        {engine, couch_db_engine:get_engine(Db)},
        {doc_count, DocCount},
        {doc_del_count, DelDocCount},
        {update_seq, get_update_seq(Db)},
        {purge_seq, couch_db_engine:get_purge_seq(Db)},
        {compact_running, Compactor /= nil},
        {sizes, {SizeInfo}},
        {instance_start_time, StartTime},
        {disk_format_version, DiskVersion},
        {committed_update_seq, CommittedUpdateSeq},
        {compacted_seq, CompactedSeq},
        {props, Props},
        {uuid, Uuid}
    ],
    {ok, InfoList}.

get_partition_info(#db{} = Db, Partition) when is_binary(Partition) ->
    Info = couch_db_engine:get_partition_info(Db, Partition),
    {ok, Info};
get_partition_info(_Db, _Partition) ->
    throw({bad_request, <<"`partition` is not valid">>}).

get_design_doc(#db{name = <<"shards/", _/binary>> = ShardDbName}, DDocId0) ->
    DDocId = couch_util:normalize_ddoc_id(DDocId0),
    DbName = mem3:dbname(ShardDbName),
    {_, Ref} = spawn_monitor(fun() ->
        exit(fabric:open_doc(DbName, DDocId, []))
    end),
    receive
        {'DOWN', Ref, _, _, Response} ->
            Response
    end;
get_design_doc(#db{} = Db, DDocId0) ->
    DDocId = couch_util:normalize_ddoc_id(DDocId0),
    couch_db:open_doc_int(Db, DDocId, [ejson_body]).

get_design_docs(#db{name = <<"shards/", _/binary>> = ShardDbName}) ->
    DbName = mem3:dbname(ShardDbName),
    {_, Ref} = spawn_monitor(fun() -> exit(fabric:design_docs(DbName)) end),
    receive
        {'DOWN', Ref, _, _, Response} ->
            Response
    end;
get_design_docs(#db{} = Db) ->
    FoldFun = fun(FDI, Acc) -> {ok, [FDI | Acc]} end,
    {ok, Docs} = fold_design_docs(Db, FoldFun, [], []),
    {ok, lists:reverse(Docs)}.

get_design_doc_count(#db{} = Db) ->
    FoldFun = fun(_, Acc) -> {ok, Acc + 1} end,
    fold_design_docs(Db, FoldFun, 0, []).

check_is_admin(#db{user_ctx = UserCtx} = Db) ->
    case is_admin(Db) of
        true ->
            ok;
        false ->
            Reason = <<"You are not a db or server admin.">>,
            throw_security_error(UserCtx, Reason)
    end.

check_is_member(#db{user_ctx = UserCtx} = Db) ->
    case is_member(Db) of
        true -> ok;
        false -> throw_security_error(UserCtx)
    end.

is_admin(#db{user_ctx = UserCtx} = Db) ->
    case couch_db_plugin:check_is_admin(Db) of
        true ->
            true;
        false ->
            {Admins} = get_admins(Db),
            is_authorized(UserCtx, Admins)
    end.

is_member(#db{user_ctx = UserCtx} = Db) ->
    case is_admin(Db) of
        true ->
            true;
        false ->
            case is_public_db(Db) of
                true ->
                    true;
                false ->
                    {Members} = get_members(Db),
                    is_authorized(UserCtx, Members)
            end
    end.

is_public_db(#db{} = Db) ->
    {Members} = get_members(Db),
    Names = couch_util:get_value(<<"names">>, Members, []),
    Roles = couch_util:get_value(<<"roles">>, Members, []),
    Names =:= [] andalso Roles =:= [].

is_authorized(#user_ctx{name = UserName, roles = UserRoles}, Security) ->
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

throw_security_error(#user_ctx{name = null} = UserCtx) ->
    Reason = <<"You are not authorized to access this db.">>,
    throw_security_error(UserCtx, Reason);
throw_security_error(#user_ctx{name = _} = UserCtx) ->
    Reason = <<"You are not allowed to access this db.">>,
    throw_security_error(UserCtx, Reason).
throw_security_error(#user_ctx{} = UserCtx, Reason) ->
    Error = security_error_type(UserCtx),
    throw({Error, Reason}).

security_error_type(#user_ctx{name = null}) ->
    unauthorized;
security_error_type(#user_ctx{name = _}) ->
    forbidden.

get_admins(#db{security = SecProps}) ->
    couch_util:get_value(<<"admins">>, SecProps, {[]}).

get_members(#db{security = SecProps}) ->
    % we fallback to readers here for backwards compatibility
    couch_util:get_value(
        <<"members">>,
        SecProps,
        couch_util:get_value(<<"readers">>, SecProps, {[]})
    ).

get_security(#db{security = SecProps}) ->
    {SecProps};
get_security(?OLD_DB_REC = Db) ->
    {?OLD_DB_SECURITY(Db)}.

set_security(#db{main_pid = Pid} = Db, {NewSecProps}) when is_list(NewSecProps) ->
    check_is_admin(Db),
    ok = validate_security_object(NewSecProps),
    gen_server:call(Pid, {set_security, NewSecProps}, infinity);
set_security(_, _) ->
    throw(bad_request).

set_user_ctx(#db{} = Db, UserCtx) ->
    {ok, Db#db{user_ctx = UserCtx}}.

validate_security_object(SecProps) ->
    Admins = couch_util:get_value(<<"admins">>, SecProps, {[]}),
    % we fallback to readers here for backwards compatibility
    Members = couch_util:get_value(
        <<"members">>,
        SecProps,
        couch_util:get_value(<<"readers">>, SecProps, {[]})
    ),
    ok = validate_names_and_roles(Admins),
    ok = validate_names_and_roles(Members),
    ok.

% validate user input
validate_names_and_roles({Props}) when is_list(Props) ->
    case couch_util:get_value(<<"names">>, Props, []) of
        Ns when is_list(Ns) ->
            [throw("names must be a JSON list of strings") || N <- Ns, not is_binary(N)],
            Ns;
        _ ->
            throw("names must be a JSON list of strings")
    end,
    case couch_util:get_value(<<"roles">>, Props, []) of
        Rs when is_list(Rs) ->
            [throw("roles must be a JSON list of strings") || R <- Rs, not is_binary(R)],
            Rs;
        _ ->
            throw("roles must be a JSON list of strings")
    end,
    ok;
validate_names_and_roles(_) ->
    throw("admins or members must be a JSON list of strings").

get_revs_limit(#db{} = Db) ->
    couch_db_engine:get_revs_limit(Db).

set_revs_limit(#db{main_pid = Pid} = Db, Limit) when Limit > 0 ->
    check_is_admin(Db),
    gen_server:call(Pid, {set_revs_limit, Limit}, infinity);
set_revs_limit(_Db, _Limit) ->
    throw(invalid_revs_limit).

name(#db{name = Name}) ->
    Name;
name(?OLD_DB_REC = Db) ->
    ?OLD_DB_NAME(Db).

validate_docid(#db{} = Db, DocId) when is_binary(DocId) ->
    couch_doc:validate_docid(DocId, name(Db)),
    case is_partitioned(Db) of
        true ->
            couch_partition:validate_docid(DocId);
        false ->
            ok
    end.

doc_from_json_obj_validate(#db{} = Db, DocJson) ->
    Doc = couch_doc:from_json_obj_validate(DocJson, name(Db)),
    {Props} = DocJson,
    case couch_util:get_value(<<"_id">>, Props) of
        DocId when is_binary(DocId) ->
            % Only validate the docid if it was provided
            validate_docid(Db, DocId);
        _ ->
            ok
    end,
    Doc.

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
group_alike_docs([Doc | Rest], []) ->
    group_alike_docs(Rest, [[Doc]]);
group_alike_docs([Doc | Rest], [Bucket | RestBuckets]) ->
    [#doc{id = BucketId} | _] = Bucket,
    case Doc#doc.id == BucketId of
        true ->
            % add to existing bucket
            group_alike_docs(Rest, [[Doc | Bucket] | RestBuckets]);
        false ->
            % add to new bucket
            group_alike_docs(Rest, [[Doc] | [Bucket | RestBuckets]])
    end.

validate_doc_update(#db{} = Db, #doc{id = <<"_design/", _/binary>>} = Doc, _GetDiskDocFun) ->
    case catch check_is_admin(Db) of
        ok -> validate_ddoc(Db, Doc);
        Error -> Error
    end;
validate_doc_update(#db{validate_doc_funs = undefined} = Db, Doc, Fun) ->
    ValidationFuns = load_validation_funs(Db),
    validate_doc_update(Db#db{validate_doc_funs = ValidationFuns}, Doc, Fun);
validate_doc_update(#db{validate_doc_funs = []}, _Doc, _GetDiskDocFun) ->
    ok;
validate_doc_update(_Db, #doc{id = <<"_local/", _/binary>>}, _GetDiskDocFun) ->
    ok;
validate_doc_update(Db, Doc, GetDiskDocFun) ->
    case get(io_priority) of
        {internal_repl, _} ->
            ok;
        _ ->
            validate_doc_update_int(Db, Doc, GetDiskDocFun)
    end.

validate_ddoc(Db, DDoc) ->
    try
        ok = couch_index_server:validate(Db, couch_doc:with_ejson_body(DDoc))
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
            [
                case Fun(Doc, DiskDoc, JsonCtx, SecObj) of
                    ok -> ok;
                    Error -> throw(Error)
                end
             || Fun <- Db#db.validate_doc_funs
            ],
            ok
        catch
            throw:Error ->
                Error
        end
    end,
    couch_stats:update_histogram(
        [couchdb, query_server, vdu_process_time],
        Fun
    ).

% to be safe, spawn a middleman here
load_validation_funs(#db{main_pid = Pid, name = <<"shards/", _/binary>>} = Db) ->
    {_, Ref} = spawn_monitor(fun() ->
        exit(ddoc_cache:open(mem3:dbname(Db#db.name), validation_funs))
    end),
    receive
        {'DOWN', Ref, _, _, {ok, Funs}} ->
            gen_server:cast(Pid, {load_validation_funs, Funs}),
            Funs;
        {'DOWN', Ref, _, _, {database_does_not_exist, _StackTrace}} ->
            ok = couch_server:close_db_if_idle(Db#db.name),
            erlang:error(database_does_not_exist);
        {'DOWN', Ref, _, _, Reason} ->
            couch_log:error("could not load validation funs ~p", [Reason]),
            throw(internal_server_error)
    end;
load_validation_funs(#db{main_pid = Pid} = Db) ->
    {ok, DDocInfos} = get_design_docs(Db),
    OpenDocs = fun(#full_doc_info{} = D) ->
        {ok, Doc} = open_doc_int(Db, D, [ejson_body]),
        Doc
    end,
    DDocs = lists:map(OpenDocs, DDocInfos),
    Funs = lists:flatmap(
        fun(DDoc) ->
            case couch_doc:get_validate_doc_fun(DDoc) of
                nil -> [];
                Fun -> [Fun]
            end
        end,
        DDocs
    ),
    gen_server:cast(Pid, {load_validation_funs, Funs}),
    Funs.

reload_validation_funs(#db{} = Db) ->
    gen_server:cast(Db#db.main_pid, {load_validation_funs, undefined}).

prep_and_validate_update(
    Db,
    #doc{id = Id, revs = {RevStart, Revs}} = Doc,
    OldFullDocInfo,
    LeafRevsDict,
    AllowConflict
) ->
    case Revs of
        [PrevRev | _] ->
            case dict:find({RevStart, PrevRev}, LeafRevsDict) of
                {ok, {#leaf{deleted = Deleted, ptr = DiskSp}, DiskRevs}} ->
                    case couch_doc:has_stubs(Doc) of
                        true ->
                            DiskDoc = make_doc(Db, Id, Deleted, DiskSp, DiskRevs),
                            Doc2 = couch_doc:merge_stubs(Doc, DiskDoc),
                            {validate_doc_update(Db, Doc2, fun() -> DiskDoc end), Doc2};
                        false ->
                            LoadDiskDoc = fun() -> make_doc(Db, Id, Deleted, DiskSp, DiskRevs) end,
                            {validate_doc_update(Db, Doc, LoadDiskDoc), Doc}
                    end;
                error when AllowConflict ->
                    % will generate error if
                    couch_doc:merge_stubs(Doc, #doc{}),
                    % there are stubs
                    {validate_doc_update(Db, Doc, fun() -> nil end), Doc};
                error ->
                    {conflict, Doc}
            end;
        [] ->
            % new doc, and we have existing revs.
            % reuse existing deleted doc
            if
                OldFullDocInfo#full_doc_info.deleted orelse AllowConflict ->
                    {validate_doc_update(Db, Doc, fun() -> nil end), Doc};
                true ->
                    {conflict, Doc}
            end
    end.

prep_and_validate_updates(
    _Db,
    [],
    [],
    _AllowConflict,
    AccPrepped,
    AccFatalErrors
) ->
    AccPrepped2 = lists:reverse(lists:map(fun lists:reverse/1, AccPrepped)),
    {AccPrepped2, AccFatalErrors};
prep_and_validate_updates(
    Db,
    [DocBucket | RestBuckets],
    [not_found | RestLookups],
    AllowConflict,
    AccPrepped,
    AccErrors
) ->
    % no existing revs are known,
    {PreppedBucket, AccErrors3} = lists:foldl(
        fun(#doc{revs = Revs} = Doc, {AccBucket, AccErrors2}) ->
            case couch_doc:has_stubs(Doc) of
                true ->
                    % will throw exception
                    couch_doc:merge_stubs(Doc, #doc{});
                false ->
                    ok
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
        {[], AccErrors},
        DocBucket
    ),

    prep_and_validate_updates(
        Db,
        RestBuckets,
        RestLookups,
        AllowConflict,
        [PreppedBucket | AccPrepped],
        AccErrors3
    );
prep_and_validate_updates(
    Db,
    [DocBucket | RestBuckets],
    [#full_doc_info{rev_tree = OldRevTree} = OldFullDocInfo | RestLookups],
    AllowConflict,
    AccPrepped,
    AccErrors
) ->
    Leafs = couch_key_tree:get_all_leafs(OldRevTree),
    LeafRevsDict = dict:from_list([
        {{Start, RevId}, {Leaf, Revs}}
     || {Leaf, {Start, [RevId | _]} = Revs} <- Leafs
    ]),
    {PreppedBucket, AccErrors3} = lists:foldl(
        fun(Doc, {Docs2Acc, AccErrors2}) ->
            case
                prep_and_validate_update(
                    Db,
                    Doc,
                    OldFullDocInfo,
                    LeafRevsDict,
                    AllowConflict
                )
            of
                {ok, Doc2} ->
                    {[Doc2 | Docs2Acc], AccErrors2};
                {Error, _} ->
                    % Record the error
                    {Docs2Acc, [{doc_tag(Doc), Error} | AccErrors2]}
            end
        end,
        {[], AccErrors},
        DocBucket
    ),
    prep_and_validate_updates(
        Db,
        RestBuckets,
        RestLookups,
        AllowConflict,
        [PreppedBucket | AccPrepped],
        AccErrors3
    ).

update_docs(Db, Docs, Options) ->
    update_docs(Db, Docs, Options, interactive_edit).

prep_and_validate_replicated_updates(_Db, [], [], AccPrepped, AccErrors) ->
    Errors2 = [
        {{Id, {Pos, Rev}}, Error}
     || {#doc{id = Id, revs = {Pos, [Rev | _]}}, Error} <- AccErrors
    ],
    AccPrepped2 = lists:reverse(lists:map(fun lists:reverse/1, AccPrepped)),
    {AccPrepped2, lists:reverse(Errors2)};
prep_and_validate_replicated_updates(
    Db, [Bucket | RestBuckets], [OldInfo | RestOldInfo], AccPrepped, AccErrors
) ->
    case OldInfo of
        not_found ->
            {ValidatedBucket, AccErrors3} = lists:foldl(
                fun(Doc, {AccPrepped2, AccErrors2}) ->
                    case couch_doc:has_stubs(Doc) of
                        true ->
                            % will throw exception
                            couch_doc:merge_stubs(Doc, #doc{});
                        false ->
                            ok
                    end,
                    case validate_doc_update(Db, Doc, fun() -> nil end) of
                        ok ->
                            {[Doc | AccPrepped2], AccErrors2};
                        Error ->
                            {AccPrepped2, [{Doc, Error} | AccErrors2]}
                    end
                end,
                {[], AccErrors},
                Bucket
            ),
            prep_and_validate_replicated_updates(
                Db, RestBuckets, RestOldInfo, [ValidatedBucket | AccPrepped], AccErrors3
            );
        #full_doc_info{rev_tree = OldTree} ->
            OldLeafs = couch_key_tree:get_all_leafs_full(OldTree),
            OldLeafsLU = [{Start, RevId} || {Start, [{RevId, _} | _]} <- OldLeafs],
            NewPaths = lists:map(fun couch_doc:to_path/1, Bucket),
            NewRevTree = couch_key_tree:multi_merge(OldTree, NewPaths),
            Leafs = couch_key_tree:get_all_leafs_full(NewRevTree),
            LeafRevsFullDict = dict:from_list([
                {{Start, RevId}, FullPath}
             || {Start, [{RevId, _} | _]} = FullPath <- Leafs
            ]),
            {ValidatedBucket, AccErrors3} =
                lists:foldl(
                    fun(#doc{id = Id, revs = {Pos, [RevId | _]}} = Doc, {AccValidated, AccErrors2}) ->
                        IsOldLeaf = lists:member({Pos, RevId}, OldLeafsLU),
                        case dict:find({Pos, RevId}, LeafRevsFullDict) of
                            {ok, {Start, Path}} when not IsOldLeaf ->
                                % our unflushed doc is a leaf node. Go back on the path
                                % to find the previous rev that's on disk.

                                LoadPrevRevFun = fun() ->
                                    make_first_doc_on_disk(Db, Id, Start - 1, tl(Path))
                                end,

                                case couch_doc:has_stubs(Doc) of
                                    true ->
                                        DiskDoc =
                                            case LoadPrevRevFun() of
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
                    {[], AccErrors},
                    Bucket
                ),
            prep_and_validate_replicated_updates(
                Db,
                RestBuckets,
                RestOldInfo,
                [ValidatedBucket | AccPrepped],
                AccErrors3
            )
    end.

new_revid(#doc{body = Body, revs = {OldStart, OldRevs}, atts = Atts, deleted = Deleted}) ->
    DigestedAtts = lists:foldl(
        fun(Att, Acc) ->
            [N, T, M] = couch_att:fetch([name, type, md5], Att),
            case M == <<>> of
                true -> Acc;
                false -> [{N, T, M} | Acc]
            end
        end,
        [],
        Atts
    ),
    case DigestedAtts of
        Atts2 when length(Atts) =/= length(Atts2) ->
            % We must have old style non-md5 attachments
            ?l2b(integer_to_list(couch_util:rand32()));
        Atts2 ->
            OldRev =
                case OldRevs of
                    [] -> 0;
                    [OldRev0 | _] -> OldRev0
                end,
            couch_hash:md5_hash(
                term_to_binary([Deleted, OldStart, OldRev, Body, Atts2], [{minor_version, 1}])
            )
    end.

new_revs([], OutBuckets, IdRevsAcc) ->
    {lists:reverse(OutBuckets), IdRevsAcc};
new_revs([Bucket | RestBuckets], OutBuckets, IdRevsAcc) ->
    {NewBucket, IdRevsAcc3} = lists:mapfoldl(
        fun(#doc{revs = {Start, RevIds}} = Doc, IdRevsAcc2) ->
            NewRevId = new_revid(Doc),
            {Doc#doc{revs = {Start + 1, [NewRevId | RevIds]}}, [
                {doc_tag(Doc), {ok, {Start + 1, NewRevId}}} | IdRevsAcc2
            ]}
        end,
        IdRevsAcc,
        Bucket
    ),
    new_revs(RestBuckets, [NewBucket | OutBuckets], IdRevsAcc3).

check_dup_atts(#doc{atts = Atts} = Doc) ->
    lists:foldl(
        fun(Att, Names) ->
            Name = couch_att:fetch(name, Att),
            case ordsets:is_element(Name, Names) of
                true -> throw({bad_request, <<"Duplicate attachments">>});
                false -> ordsets:add_element(Name, Names)
            end
        end,
        ordsets:new(),
        Atts
    ),
    Doc.

tag_docs([]) ->
    [];
tag_docs([#doc{meta = Meta} = Doc | Rest]) ->
    [Doc#doc{meta = [{ref, make_ref()} | Meta]} | tag_docs(Rest)].

doc_tag(#doc{meta = Meta}) ->
    case lists:keyfind(ref, 1, Meta) of
        {ref, Ref} when is_reference(Ref) -> Ref;
        false -> throw(doc_not_tagged);
        Else -> throw({invalid_doc_tag, Else})
    end.

update_docs(Db, Docs0, Options, replicated_changes) ->
    Docs = tag_docs(Docs0),

    PrepValidateFun = fun(Db0, DocBuckets0, ExistingDocInfos) ->
        prep_and_validate_replicated_updates(
            Db0,
            DocBuckets0,
            ExistingDocInfos,
            [],
            []
        )
    end,

    {ok, DocBuckets, NonRepDocs, DocErrors} =
        before_docs_update(Db, Docs, PrepValidateFun, replicated_changes),

    DocBuckets2 = [
        [
            doc_flush_atts(Db, check_dup_atts(Doc))
         || Doc <- Bucket
        ]
     || Bucket <- DocBuckets
    ],
    {ok, _} = write_and_commit(
        Db,
        DocBuckets2,
        NonRepDocs,
        [merge_conflicts | Options]
    ),
    {ok, DocErrors};
update_docs(Db, Docs0, Options, interactive_edit) ->
    Docs = tag_docs(Docs0),

    AllOrNothing = lists:member(all_or_nothing, Options),
    PrepValidateFun = fun(Db0, DocBuckets0, ExistingDocInfos) ->
        prep_and_validate_updates(
            Db0,
            DocBuckets0,
            ExistingDocInfos,
            AllOrNothing,
            [],
            []
        )
    end,

    {ok, DocBuckets, NonRepDocs, DocErrors} =
        before_docs_update(Db, Docs, PrepValidateFun, interactive_edit),

    if
        (AllOrNothing) and (DocErrors /= []) ->
            RefErrorDict = dict:from_list([{doc_tag(Doc), Doc} || Doc <- Docs]),
            {aborted,
                lists:map(
                    fun({Ref, Error}) ->
                        #doc{id = Id, revs = {Start, RevIds}} = dict:fetch(Ref, RefErrorDict),
                        case {Start, RevIds} of
                            {Pos, [RevId | _]} -> {{Id, {Pos, RevId}}, Error};
                            {0, []} -> {{Id, {0, <<>>}}, Error}
                        end
                    end,
                    DocErrors
                )};
        true ->
            Options2 =
                if
                    AllOrNothing -> [merge_conflicts];
                    true -> []
                end ++ Options,
            DocBuckets2 = [
                [
                    doc_flush_atts(
                        Db,
                        set_new_att_revpos(
                            check_dup_atts(Doc)
                        )
                    )
                 || Doc <- B
                ]
             || B <- DocBuckets
            ],
            {DocBuckets3, IdRevs} = new_revs(DocBuckets2, [], []),

            {ok, CommitResults} = write_and_commit(
                Db,
                DocBuckets3,
                NonRepDocs,
                Options2
            ),

            ResultsDict = lists:foldl(
                fun({Key, Resp}, ResultsAcc) ->
                    dict:store(Key, Resp, ResultsAcc)
                end,
                dict:from_list(IdRevs),
                CommitResults ++ DocErrors
            ),
            {ok,
                lists:map(
                    fun(Doc) ->
                        dict:fetch(doc_tag(Doc), ResultsDict)
                    end,
                    Docs
                )}
    end.

% Returns the first available document on disk. Input list is a full rev path
% for the doc.
make_first_doc_on_disk(_Db, _Id, _Pos, []) ->
    nil;
make_first_doc_on_disk(Db, Id, Pos, [{_Rev, #doc{}} | RestPath]) ->
    make_first_doc_on_disk(Db, Id, Pos - 1, RestPath);
make_first_doc_on_disk(Db, Id, Pos, [{_Rev, ?REV_MISSING} | RestPath]) ->
    make_first_doc_on_disk(Db, Id, Pos - 1, RestPath);
make_first_doc_on_disk(Db, Id, Pos, [{_Rev, #leaf{deleted = IsDel, ptr = Sp}} | _] = DocPath) ->
    Revs = [Rev || {Rev, _} <- DocPath],
    make_doc(Db, Id, IsDel, Sp, {Pos, Revs}).

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

write_and_commit(
    #db{main_pid = Pid, user_ctx = Ctx} = Db,
    DocBuckets1,
    NonRepDocs,
    Options
) ->
    DocBuckets = prepare_doc_summaries(Db, DocBuckets1),
    MergeConflicts = lists:member(merge_conflicts, Options),
    MRef = erlang:monitor(process, Pid),
    try
        Pid ! {update_docs, self(), DocBuckets, NonRepDocs, MergeConflicts},
        case collect_results_with_metrics(Pid, MRef, []) of
            {ok, Results} ->
                {ok, Results};
            retry ->
                % This can happen if the db file we wrote to was swapped out by
                % compaction. Retry by reopening the db and writing to the current file
                {ok, Db2} = open(Db#db.name, [{user_ctx, Ctx}]),
                DocBuckets2 = [
                    [doc_flush_atts(Db2, Doc) || Doc <- Bucket]
                 || Bucket <- DocBuckets1
                ],
                % We only retry once
                DocBuckets3 = prepare_doc_summaries(Db2, DocBuckets2),
                close(Db2),
                Pid ! {update_docs, self(), DocBuckets3, NonRepDocs, MergeConflicts},
                case collect_results_with_metrics(Pid, MRef, []) of
                    {ok, Results} -> {ok, Results};
                    retry -> throw({update_error, compaction_retry})
                end
        end
    after
        erlang:demonitor(MRef, [flush])
    end.

prepare_doc_summaries(Db, BucketList) ->
    [
        lists:map(
            fun(#doc{body = Body, atts = Atts} = Doc0) ->
                DiskAtts = [couch_att:to_disk_term(Att) || Att <- Atts],
                {ok, SizeInfo} = couch_att:size_info(Atts),
                AttsStream =
                    case Atts of
                        [Att | _] ->
                            {stream, StreamEngine} = couch_att:fetch(data, Att),
                            StreamEngine;
                        [] ->
                            nil
                    end,
                Doc1 = Doc0#doc{
                    atts = DiskAtts,
                    meta =
                        [
                            {size_info, SizeInfo},
                            {atts_stream, AttsStream},
                            {ejson_size, couch_ejson_size:encoded_size(Body)}
                        ] ++ Doc0#doc.meta
                },
                couch_db_engine:serialize_doc(Db, Doc1)
            end,
            Bucket
        )
     || Bucket <- BucketList
    ].

before_docs_update(#db{validate_doc_funs = VDFuns} = Db, Docs, PVFun, UpdateType) ->
    increment_stat(Db, [couchdb, database_writes]),

    % Separate _local docs from normal docs
    IsLocal = fun
        (#doc{id = <<?LOCAL_DOC_PREFIX, _/binary>>}) -> true;
        (_) -> false
    end,
    {NonRepDocs, Docs2} = lists:partition(IsLocal, Docs),

    BucketList = group_alike_docs(Docs2),

    DocBuckets = lists:map(
        fun(Bucket) ->
            lists:map(
                fun(Doc) ->
                    DocWithBody = couch_doc:with_ejson_body(Doc),
                    couch_db_plugin:before_doc_update(Db, DocWithBody, UpdateType)
                end,
                Bucket
            )
        end,
        BucketList
    ),

    ValidatePred = fun
        (#doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>}) -> true;
        (#doc{atts = Atts}) -> Atts /= []
    end,

    case (VDFuns /= []) orelse lists:any(ValidatePred, Docs2) of
        true ->
            % lookup the doc by id and get the most recent
            Ids = [Id || [#doc{id = Id} | _] <- DocBuckets],
            ExistingDocs = get_full_doc_infos(Db, Ids),
            {DocBuckets2, DocErrors} = PVFun(Db, DocBuckets, ExistingDocs),
            % remove empty buckets
            DocBuckets3 = [Bucket || Bucket <- DocBuckets2, Bucket /= []],
            {ok, DocBuckets3, NonRepDocs, DocErrors};
        false ->
            {ok, DocBuckets, NonRepDocs, []}
    end.

set_new_att_revpos(#doc{revs = {RevPos, _Revs}, atts = Atts0} = Doc) ->
    Atts = lists:map(
        fun(Att) ->
            case couch_att:fetch(data, Att) of
                % already commited to disk, don't set new rev
                {stream, _} -> Att;
                {Fd, _} when is_pid(Fd) -> Att;
                % write required so update RevPos
                _ -> couch_att:store(revpos, RevPos + 1, Att)
            end
        end,
        Atts0
    ),
    Doc#doc{atts = Atts}.

doc_flush_atts(Db, Doc) ->
    Doc#doc{atts = [couch_att:flush(Db, Att) || Att <- Doc#doc.atts]}.

compressible_att_type(MimeType) when is_binary(MimeType) ->
    compressible_att_type(?b2l(MimeType));
compressible_att_type(MimeType) ->
    TypeExpList = re:split(
        config:get(
            "attachments",
            "compressible_types",
            ?DEFAULT_COMPRESSIBLE_TYPES
        ),
        "\\s*,\\s*",
        [{return, list}]
    ),
    lists:any(
        fun(TypeExp) ->
            Regexp = [
                "^\\s*",
                re:replace(TypeExp, "\\*", ".*"),
                "(?:\\s*;.*?)?\\s*",
                $$
            ],
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
with_stream(Db, Att, Fun) ->
    [InMd5, Type, Enc] = couch_att:fetch([md5, type, encoding], Att),
    BufferSize = config:get_integer(
        "couchdb",
        "attachment_stream_buffer_size",
        4096
    ),
    Options =
        case (Enc =:= identity) andalso compressible_att_type(Type) of
            true ->
                CompLevel = config:get_integer(
                    "attachments", "compression_level", 8
                ),
                [
                    {buffer_size, BufferSize},
                    {encoding, gzip},
                    {compression_level, CompLevel}
                ];
            _ ->
                [{buffer_size, BufferSize}]
        end,
    {ok, OutputStream} = open_write_stream(Db, Options),
    ReqMd5 =
        case Fun(OutputStream) of
            {md5, FooterMd5} ->
                case InMd5 of
                    md5_in_footer -> FooterMd5;
                    _ -> InMd5
                end;
            _ ->
                InMd5
        end,
    {StreamEngine, Len, IdentityLen, Md5, IdentityMd5} =
        couch_stream:close(OutputStream),
    couch_util:check_md5(IdentityMd5, ReqMd5),
    {AttLen, DiskLen, NewEnc} =
        case Enc of
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
    couch_att:store(
        [
            {data, {stream, StreamEngine}},
            {att_len, AttLen},
            {disk_len, DiskLen},
            {md5, Md5},
            {encoding, NewEnc}
        ],
        Att
    ).

open_write_stream(Db, Options) ->
    couch_db_engine:open_write_stream(Db, Options).

open_read_stream(Db, AttState) ->
    couch_db_engine:open_read_stream(Db, AttState).

is_active_stream(Db, StreamEngine) ->
    couch_db_engine:is_active_stream(Db, StreamEngine).

calculate_start_seq(_Db, _Node, Seq) when is_integer(Seq) ->
    Seq;
calculate_start_seq(Db, Node, {Seq, Uuid}) ->
    % Treat the current node as the epoch node
    calculate_start_seq(Db, Node, {Seq, Uuid, Node});
calculate_start_seq(Db, _Node, {Seq, {split, Uuid}, EpochNode}) ->
    case is_owner(EpochNode, Seq, get_epochs(Db)) of
        true ->
            % Find last replicated sequence from split source to target
            mem3_rep:find_split_target_seq(Db, EpochNode, Uuid, Seq);
        false ->
            couch_log:warning(
                "~p calculate_start_seq not owner "
                "db: ~p, seq: ~p, uuid: ~p, epoch_node: ~p, epochs: ~p",
                [?MODULE, Db#db.name, Seq, Uuid, EpochNode, get_epochs(Db)]
            ),
            0
    end;
calculate_start_seq(Db, Node, {Seq, Uuid, EpochNode}) ->
    case is_prefix(Uuid, get_uuid(Db)) of
        true ->
            case is_owner(EpochNode, Seq, get_epochs(Db)) of
                true ->
                    Seq;
                false ->
                    %% Shard might have been moved from another node. We
                    %% matched the uuid already, try to find last viable
                    %% sequence we can use
                    couch_log:warning(
                        "~p calculate_start_seq not owner, "
                        " trying replacement db: ~p, seq: ~p, uuid: ~p, "
                        "epoch_node: ~p, epochs: ~p",
                        [
                            ?MODULE,
                            Db#db.name,
                            Seq,
                            Uuid,
                            EpochNode,
                            get_epochs(Db)
                        ]
                    ),
                    calculate_start_seq(Db, Node, {replace, EpochNode, Uuid, Seq})
            end;
        false ->
            couch_log:warning(
                "~p calculate_start_seq uuid prefix mismatch "
                "db: ~p, seq: ~p, uuid: ~p, epoch_node: ~p",
                [?MODULE, Db#db.name, Seq, Uuid, EpochNode]
            ),
            %% The file was rebuilt, most likely in a different
            %% order, so rewind.
            0
    end;
calculate_start_seq(Db, _Node, {replace, OriginalNode, Uuid, Seq}) ->
    case is_prefix(Uuid, couch_db:get_uuid(Db)) of
        true ->
            try
                start_seq(get_epochs(Db), OriginalNode, Seq)
            catch
                throw:epoch_mismatch ->
                    couch_log:warning(
                        "~p start_seq duplicate uuid on node: ~p "
                        "db: ~p, seq: ~p, uuid: ~p, epoch_node: ~p",
                        [?MODULE, node(), Db#db.name, Seq, Uuid, OriginalNode]
                    ),
                    0
            end;
        false ->
            {replace, OriginalNode, Uuid, Seq}
    end.

validate_epochs(Epochs) ->
    %% Assert uniqueness.
    case length(Epochs) == length(lists:ukeysort(2, Epochs)) of
        true -> ok;
        false -> erlang:error(duplicate_epoch)
    end,
    %% Assert order.
    case Epochs == lists:sort(fun({_, A}, {_, B}) -> B =< A end, Epochs) of
        true -> ok;
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
owner_of([{EpochNode, EpochSeq} | _Rest], Seq) when Seq >= EpochSeq ->
    EpochNode;
owner_of([_ | Rest], Seq) ->
    owner_of(Rest, Seq).

start_seq([{OrigNode, EpochSeq} | _], OrigNode, Seq) when Seq >= EpochSeq ->
    %% OrigNode is the owner of the Seq so we can safely stream from there
    Seq;
start_seq([{_, NewSeq}, {OrigNode, _} | _], OrigNode, Seq) when Seq >= NewSeq ->
    %% We transferred this file before Seq was written on OrigNode, so we need
    %% to stream from the beginning of the next epoch. Note that it is _not_
    %% necessary for the current node to own the epoch beginning at NewSeq
    NewSeq;
start_seq([_ | Rest], OrigNode, Seq) ->
    start_seq(Rest, OrigNode, Seq);
start_seq([], _OrigNode, _Seq) ->
    throw(epoch_mismatch).

fold_docs(Db, UserFun, UserAcc) ->
    fold_docs(Db, UserFun, UserAcc, []).

fold_docs(Db, UserFun, UserAcc, Options) ->
    couch_db_engine:fold_docs(Db, UserFun, UserAcc, Options).

fold_local_docs(Db, UserFun, UserAcc, Options) ->
    couch_db_engine:fold_local_docs(Db, UserFun, UserAcc, Options).

fold_design_docs(Db, UserFun, UserAcc, Options1) ->
    Options2 = set_design_doc_keys(Options1),
    couch_db_engine:fold_docs(Db, UserFun, UserAcc, Options2).

fold_changes(Db, StartSeq, UserFun, UserAcc) ->
    fold_changes(Db, StartSeq, UserFun, UserAcc, []).

fold_changes(Db, StartSeq, UserFun, UserAcc, Opts) ->
    couch_db_engine:fold_changes(Db, StartSeq, UserFun, UserAcc, Opts).

fold_purge_infos(Db, StartPurgeSeq, Fun, Acc) ->
    fold_purge_infos(Db, StartPurgeSeq, Fun, Acc, []).

fold_purge_infos(Db, StartPurgeSeq, UFun, UAcc, Opts) ->
    couch_db_engine:fold_purge_infos(Db, StartPurgeSeq, UFun, UAcc, Opts).

raft_insert(#db{main_pid = Pid} = Db, Entries) ->
    check_is_admin(Db),
    gen_server:call(Pid, {raft_insert, Entries}, infinity).

raft_lookup(Db, Indexes) ->
    couch_db_engine:raft_lookup(Db, Indexes).

raft_discard(#db{main_pid = Pid} = Db, UpTo) ->
    check_is_admin(Db),
    gen_server:call(Pid, {raft_discard, UpTo}, infinity).

raft_last(Db) ->
    couch_db_engine:raft_last(Db).

count_changes_since(Db, SinceSeq) ->
    couch_db_engine:count_changes_since(Db, SinceSeq).

%%% Internal function %%%
open_doc_revs_int(Db, IdRevs, Options) ->
    Ids = [Id || {Id, _Revs} <- IdRevs],
    LookupResults = get_full_doc_infos(Db, Ids),
    lists:zipwith(
        fun({Id, Revs}, Lookup) ->
            case Lookup of
                #full_doc_info{rev_tree = RevTree} ->
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
                        lists:map(
                            fun({Value, {Pos, [Rev | _]} = FoundRevPath}) ->
                                case Value of
                                    ?REV_MISSING ->
                                        % we have the rev in our list but know nothing about it
                                        {{not_found, missing}, {Pos, Rev}};
                                    #leaf{deleted = IsDeleted, ptr = SummaryPtr} ->
                                        {ok, make_doc(Db, Id, IsDeleted, SummaryPtr, FoundRevPath)}
                                end
                            end,
                            FoundRevs
                        ),
                    Results =
                        FoundResults ++
                            [{{not_found, missing}, MissingRev} || MissingRev <- MissingRevs],
                    {ok, Results};
                not_found when Revs == all ->
                    {ok, []};
                not_found ->
                    {ok, [{{not_found, missing}, Rev} || Rev <- Revs]}
            end
        end,
        IdRevs,
        LookupResults
    ).

open_doc_int(Db, <<?LOCAL_DOC_PREFIX, _/binary>> = Id, Options) ->
    case couch_db_engine:open_local_docs(Db, [Id]) of
        [#doc{} = Doc] ->
            apply_open_options({ok, Doc}, Options);
        [not_found] ->
            {not_found, missing}
    end;
open_doc_int(Db, #doc_info{id = Id, revs = [RevInfo | _]} = DocInfo, Options) ->
    #rev_info{deleted = IsDeleted, rev = {Pos, RevId}, body_sp = Bp} = RevInfo,
    Doc = make_doc(Db, Id, IsDeleted, Bp, {Pos, [RevId]}),
    apply_open_options(
        {ok, Doc#doc{meta = doc_meta_info(DocInfo, [], Options)}}, Options
    );
open_doc_int(Db, #full_doc_info{id = Id, rev_tree = RevTree} = FullDocInfo, Options) ->
    #doc_info{revs = [#rev_info{deleted = IsDeleted, rev = Rev, body_sp = Bp} | _]} =
        DocInfo = couch_doc:to_doc_info(FullDocInfo),
    {[{_, RevPath}], []} = couch_key_tree:get(RevTree, [Rev]),
    Doc = make_doc(Db, Id, IsDeleted, Bp, RevPath),
    apply_open_options(
        {ok, Doc#doc{meta = doc_meta_info(DocInfo, RevTree, Options)}}, Options
    );
open_doc_int(Db, Id, Options) ->
    case get_full_doc_info(Db, Id) of
        #full_doc_info{} = FullDocInfo ->
            open_doc_int(Db, FullDocInfo, Options);
        not_found ->
            {not_found, missing}
    end.

doc_meta_info(
    #doc_info{high_seq = Seq, revs = [#rev_info{rev = Rev} | RestInfo]}, RevTree, Options
) ->
    case lists:member(revs_info, Options) of
        false ->
            [];
        true ->
            {[{Pos, RevPath}], []} =
                couch_key_tree:get_full_key_paths(RevTree, [Rev]),

            [
                {revs_info, Pos,
                    lists:map(
                        fun
                            ({Rev1, ?REV_MISSING}) ->
                                {Rev1, missing};
                            ({Rev1, Leaf}) ->
                                case Leaf#leaf.deleted of
                                    true ->
                                        {Rev1, deleted};
                                    false ->
                                        {Rev1, available}
                                end
                        end,
                        RevPath
                    )}
            ]
    end ++
        case lists:member(conflicts, Options) of
            false ->
                [];
            true ->
                case [Rev1 || #rev_info{rev = Rev1, deleted = false} <- RestInfo] of
                    [] -> [];
                    ConflictRevs -> [{conflicts, ConflictRevs}]
                end
        end ++
        case lists:member(deleted_conflicts, Options) of
            false ->
                [];
            true ->
                case [Rev1 || #rev_info{rev = Rev1, deleted = true} <- RestInfo] of
                    [] -> [];
                    DelConflictRevs -> [{deleted_conflicts, DelConflictRevs}]
                end
        end ++
        case lists:member(local_seq, Options) of
            false -> [];
            true -> [{local_seq, Seq}]
        end.

make_doc(_Db, Id, Deleted, nil = _Bp, RevisionPath) ->
    #doc{
        id = Id,
        revs = RevisionPath,
        body = [],
        atts = [],
        deleted = Deleted
    };
make_doc(#db{} = Db, Id, Deleted, Bp, {Pos, Revs}) ->
    RevsLimit = get_revs_limit(Db),
    Doc0 = couch_db_engine:read_doc_body(Db, #doc{
        id = Id,
        revs = {Pos, lists:sublist(Revs, 1, RevsLimit)},
        body = Bp,
        deleted = Deleted
    }),
    Doc1 =
        case Doc0#doc.atts of
            BinAtts when is_binary(BinAtts) ->
                Doc0#doc{
                    atts = couch_compress:decompress(BinAtts)
                };
            ListAtts when is_list(ListAtts) ->
                Doc0
        end,
    after_doc_read(Db, Doc1#doc{
        atts = [couch_att:from_disk_term(Db, T) || T <- Doc1#doc.atts]
    }).

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
        DbName, Normalized, fun validate_dbname_int/2
    ).

validate_dbname_int(DbName, Normalized) when is_binary(DbName) ->
    DbNoExt = couch_util:drop_dot_couch_ext(DbName),
    case re:run(DbNoExt, ?DBNAME_REGEX, [{capture, none}, dollar_endonly]) of
        match ->
            ok;
        nomatch ->
            case is_system_db_name(Normalized) of
                true -> ok;
                false -> {error, {illegal_database_name, DbName}}
            end
    end.

is_system_db_name(DbName) when is_list(DbName) ->
    is_system_db_name(?l2b(DbName));
is_system_db_name(DbName) when is_binary(DbName) ->
    Normalized = normalize_dbname(DbName),
    Suffix = filename:basename(Normalized),
    case {filename:dirname(Normalized), lists:member(Suffix, ?SYSTEM_DATABASES)} of
        {<<".">>, Result} ->
            Result;
        {_Prefix, false} ->
            false;
        {Prefix, true} ->
            ReOpts = [{capture, none}, dollar_endonly],
            re:run(Prefix, ?DBNAME_REGEX, ReOpts) == match
    end.

set_design_doc_keys(Options1) ->
    Dir =
        case lists:keyfind(dir, 1, Options1) of
            {dir, D0} -> D0;
            _ -> fwd
        end,
    Options2 = set_design_doc_start_key(Options1, Dir),
    set_design_doc_end_key(Options2, Dir).

-define(FIRST_DDOC_KEY, <<"_design/">>).
-define(LAST_DDOC_KEY, <<"_design0">>).

set_design_doc_start_key(Options, fwd) ->
    Key1 = couch_util:get_value(start_key, Options, ?FIRST_DDOC_KEY),
    Key2 =
        case Key1 < ?FIRST_DDOC_KEY of
            true -> ?FIRST_DDOC_KEY;
            false -> Key1
        end,
    lists:keystore(start_key, 1, Options, {start_key, Key2});
set_design_doc_start_key(Options, rev) ->
    Key1 = couch_util:get_value(start_key, Options, ?LAST_DDOC_KEY),
    Key2 =
        case Key1 > ?LAST_DDOC_KEY of
            true -> ?LAST_DDOC_KEY;
            false -> Key1
        end,
    lists:keystore(start_key, 1, Options, {start_key, Key2}).

set_design_doc_end_key(Options, fwd) ->
    case couch_util:get_value(end_key_gt, Options) of
        undefined ->
            Key1 = couch_util:get_value(end_key, Options, ?LAST_DDOC_KEY),
            Key2 =
                case Key1 > ?LAST_DDOC_KEY of
                    true -> ?LAST_DDOC_KEY;
                    false -> Key1
                end,
            lists:keystore(end_key, 1, Options, {end_key, Key2});
        EKeyGT ->
            Key2 =
                case EKeyGT > ?LAST_DDOC_KEY of
                    true -> ?LAST_DDOC_KEY;
                    false -> EKeyGT
                end,
            lists:keystore(end_key_gt, 1, Options, {end_key_gt, Key2})
    end;
set_design_doc_end_key(Options, rev) ->
    case couch_util:get_value(end_key_gt, Options) of
        undefined ->
            Key1 = couch_util:get_value(end_key, Options, ?LAST_DDOC_KEY),
            Key2 =
                case Key1 < ?FIRST_DDOC_KEY of
                    true -> ?FIRST_DDOC_KEY;
                    false -> Key1
                end,
            lists:keystore(end_key, 1, Options, {end_key, Key2});
        EKeyGT ->
            Key2 =
                case EKeyGT < ?FIRST_DDOC_KEY of
                    true -> ?FIRST_DDOC_KEY;
                    false -> EKeyGT
                end,
            lists:keystore(end_key_gt, 1, Options, {end_key_gt, Key2})
    end.

possible_ancestors(_FullInfo, []) ->
    [];
possible_ancestors(FullInfo, MissingRevs) ->
    #doc_info{revs = RevsInfo} = couch_doc:to_doc_info(FullInfo),
    LeafRevs = [Rev || #rev_info{rev = Rev} <- RevsInfo],
    % Find the revs that are possible ancestors of this rev. A leaf is
    % a possible ancestor if its position is less than any of the
    % missing revs, and if it is less than any, it means it is also
    % less than the maximum missing rev, so we just compare against
    % that.
    {MaxMissingPos, _} = lists:max(MissingRevs),
    lists:filter(fun({Pos, _}) -> Pos < MaxMissingPos end, LeafRevs).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup_all() ->
    ok = meck:new(couch_epi, [passthrough]),
    ok = meck:expect(couch_epi, decide, fun(_, _, _, _, _) -> no_decision end),
    ok.

teardown_all(_) ->
    meck:unload().

setup() ->
    meck:reset([couch_epi]).

teardown(_) ->
    ok.

validate_dbname_success_test_() ->
    Cases =
        generate_cases_with_shards("long/co$mplex-/path+/something") ++
            generate_cases_with_shards("something") ++
            lists:append(
                [
                    generate_cases_with_shards(?b2l(SystemDb))
                 || SystemDb <- ?SYSTEM_DATABASES
                ]
            ),
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [should_pass_validate_dbname(A) || {_, A} <- Cases]
        }
    }.

validate_dbname_fail_test_() ->
    Cases =
        generate_cases("_long/co$mplex-/path+/_something") ++
            generate_cases("_something") ++
            generate_cases_with_shards("long/co$mplex-/path+/_something#") ++
            generate_cases_with_shards("long/co$mplex-/path+/some.thing") ++
            generate_cases("!abcdefg/werwej/_users") ++
            generate_cases_with_shards("!abcdefg/werwej/_users"),
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [should_fail_validate_dbname(A) || {_, A} <- Cases]
        }
    }.

normalize_dbname_test_() ->
    Cases =
        generate_cases_with_shards("long/co$mplex-/path+/_something") ++
            generate_cases_with_shards("_something"),
    WithExpected = [{?l2b(filename:rootname(A)), B} || {A, B} <- Cases],
    [
        {test_name({Expected, Db}), ?_assertEqual(Expected, normalize_dbname(Db))}
     || {Expected, Db} <- WithExpected
    ].

dbname_suffix_test_() ->
    Cases =
        generate_cases_with_shards("long/co$mplex-/path+/_something") ++
            generate_cases_with_shards("_something"),
    WithExpected = [{?l2b(filename:basename(Arg)), Db} || {Arg, Db} <- Cases],
    [
        {test_name({Expected, Db}), ?_assertEqual(Expected, dbname_suffix(Db))}
     || {Expected, Db} <- WithExpected
    ].

is_system_db_name_test_() ->
    Cases = lists:append(
        [
            generate_cases_with_shards("long/co$mplex-/path+/" ++ ?b2l(Db))
         || Db <- ?SYSTEM_DATABASES
        ] ++
            [generate_cases_with_shards(?b2l(Db)) || Db <- ?SYSTEM_DATABASES]
    ),
    WithExpected = [
        {?l2b(filename:basename(filename:rootname(Arg))), Db}
     || {Arg, Db} <- Cases
    ],
    [
        {test_name({Expected, Db}) ++ " in ?SYSTEM_DATABASES", ?_assert(is_system_db_name(Db))}
     || {Expected, Db} <- WithExpected
    ].

should_pass_validate_dbname(DbName) ->
    {test_name(DbName), ?_assertEqual(ok, validate_dbname(DbName))}.

should_fail_validate_dbname(DbName) ->
    {
        test_name(DbName),
        ?_test(begin
            Result = validate_dbname(DbName),
            ?assertMatch({error, {illegal_database_name, _}}, Result),
            {error, {illegal_database_name, FailedDbName}} = Result,
            ?assertEqual(to_binary(DbName), FailedDbName),
            ok
        end)
    }.

calculate_start_seq_test_() ->
    {
        setup,
        fun setup_start_seq_all/0,
        fun teardown_start_seq_all/1,
        {
            foreach,
            fun setup_start_seq/0,
            fun teardown_start_seq/1,
            [
                t_calculate_start_seq_uuid_mismatch(),
                t_calculate_start_seq_is_owner(),
                t_calculate_start_seq_not_owner(),
                t_calculate_start_seq_raw(),
                t_calculate_start_seq_epoch_mismatch(),
                t_calculate_start_seq_shard_move()
            ]
        }
    }.

setup_start_seq_all() ->
    meck:new(couch_db_engine, [passthrough]),
    meck:expect(couch_db_engine, get_uuid, fun(_) -> <<"foo">> end),
    ok = meck:expect(couch_log, warning, 2, ok),
    Epochs = [
        {node2, 10},
        {node1, 1}
    ],
    meck:expect(couch_db_engine, get_epochs, fun(_) -> Epochs end).

teardown_start_seq_all(_) ->
    meck:unload().

setup_start_seq() ->
    meck:reset([
        couch_db_engine,
        couch_log
    ]).

teardown_start_seq(_) ->
    ok.

t_calculate_start_seq_uuid_mismatch() ->
    ?_test(begin
        Db = test_util:fake_db([]),
        Seq = calculate_start_seq(Db, node2, {15, <<"baz">>}),
        ?assertEqual(0, Seq)
    end).

t_calculate_start_seq_is_owner() ->
    ?_test(begin
        Db = test_util:fake_db([]),
        Seq = calculate_start_seq(Db, node2, {15, <<"foo">>}),
        ?assertEqual(15, Seq)
    end).

t_calculate_start_seq_not_owner() ->
    ?_test(begin
        Db = test_util:fake_db([]),
        Seq = calculate_start_seq(Db, node3, {15, <<"foo">>}),
        ?assertEqual(0, Seq)
    end).

t_calculate_start_seq_raw() ->
    ?_test(begin
        Db = test_util:fake_db([]),
        Seq = calculate_start_seq(Db, node1, 13),
        ?assertEqual(13, Seq)
    end).

t_calculate_start_seq_epoch_mismatch() ->
    ?_test(begin
        Db = test_util:fake_db([]),
        SeqIn = {replace, not_this_node, get_uuid(Db), 42},
        Seq = calculate_start_seq(Db, node1, SeqIn),
        ?assertEqual(0, Seq)
    end).

t_calculate_start_seq_shard_move() ->
    ?_test(begin
        Db = test_util:fake_db([]),
        % Sequence when shard was on node1
        ?assertEqual(2, calculate_start_seq(Db, node1, {2, <<"foo">>})),
        % Shard moved to node2 with no other updates after the move to node2
        ?assertEqual(10, calculate_start_seq(Db, node2, {10, <<"foo">>})),
        % Sequence from node1 after the move happened, we reset back to the
        % start of the epoch on node2 = 10
        ?assertEqual(10, calculate_start_seq(Db, node1, {16, <<"foo">>})),
        % Invalid node, epoch mismatch, start at 0
        ?assertEqual(0, calculate_start_seq(Db, node3, {16, <<"foo">>}))
    end).

is_owner_test() ->
    ?assertNot(is_owner(foo, 1, [])),
    ?assertNot(is_owner(foo, 1, [{foo, 2}])),
    ?assert(is_owner(foo, 1, [{foo, 1}])),
    ?assert(is_owner(foo, 2, [{foo, 1}])),
    ?assert(is_owner(foo, 50, [{bar, 100}, {foo, 1}])),
    ?assert(is_owner(foo, 50, [{baz, 200}, {bar, 100}, {foo, 1}])),
    ?assert(is_owner(bar, 150, [{baz, 200}, {bar, 100}, {foo, 1}])),
    ?assert(is_owner(bar, 100, [{baz, 200}, {bar, 100}, {foo, 1}])),
    ?assertNot(is_owner(bar, 99, [{baz, 200}, {bar, 100}, {foo, 1}])),
    ?assertNot(is_owner(baz, 199, [{baz, 200}, {bar, 100}, {foo, 1}])),
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
        DbName,
        ?l2b(DbName),
        DbNameWithShard,
        ?l2b(DbNameWithShard),
        DbNameWithShardAndExtension,
        ?l2b(DbNameWithShardAndExtension)
    ],
    [{DbName, Case} || Case <- Cases].

add_shard(DbName) ->
    "shards/00000000-3fffffff/" ++ DbName ++ ".1415960794".

generate_cases(DbName) ->
    [{DbName, DbName}, {DbName, ?l2b(DbName)}].

-endif.
