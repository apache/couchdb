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

-module(couch_replicator).

-export([
    replicate/2,

    jobs/0,
    job/1,
    docs/2,
    doc/2,

    after_db_create/2,
    after_db_delete/2,
    after_doc_write/6,

    ensure_rep_db_exists/0,

    rescan_jobs/0,
    rescan_jobs/1,
    reenqueue_jobs/0,
    reenqueue_jobs/1,
    remove_jobs/0,
    get_job_ids/0
]).


-include_lib("couch/include/couch_db.hrl").
-include("couch_replicator.hrl").


-spec replicate({[_]}, any()) ->
    {ok, {continuous, binary()}} |
    {ok, #{}} |
    {ok, {cancelled, binary()}} |
    {error, any()} |
    no_return().
replicate(Body, #user_ctx{name = User} = UserCtx) ->
    {ok, Id, Rep} = couch_replicator_parse:parse_transient_rep(Body, User),
    #{?OPTIONS := Options} = Rep,
    JobId = case couch_replicator_jobs:get_job_id(undefined, Id) of
        {ok, JobId0} -> JobId0;
        {error, not_found} -> Id
    end,
    case maps:get(<<"cancel">>, Options, false) of
        true ->
            case check_authorization(JobId, UserCtx) of
                ok -> cancel_replication(JobId);
                not_found -> {error, not_found}
            end;
        false ->
            check_authorization(JobId, UserCtx),
            ok = start_transient_job(JobId, Rep),
            case maps:get(<<"continuous">>, Options, false) of
                true ->
                    case couch_replicator_jobs:wait_running(JobId) of
                        {ok, #{?STATE := ?ST_RUNNING} = JobData} ->
                            {ok, {continuous, maps:get(?REP_ID, JobData)}};
                        {ok, #{?STATE := ?ST_FAILED} = JobData} ->
                            {error, maps:get(?STATE_INFO, JobData)};
                        {error, Error} ->
                            {error, Error}
                    end;
                false ->
                    case couch_replicator_jobs:wait_result(JobId) of
                        {ok, #{?STATE := ?ST_COMPLETED} = JobData} ->
                            {ok, maps:get(?CHECKPOINT_HISTORY, JobData)};
                        {ok, #{?STATE := ?ST_FAILED} = JobData} ->
                            {error, maps:get(?STATE_INFO, JobData)};
                        {error, Error} ->
                            {error, Error}
                    end
            end
    end.


jobs() ->
    FoldFun = fun(_JTx, _JobId, CouchJobsState, JobData, Acc) ->
        case CouchJobsState of
            pending -> [job_ejson(JobData) | Acc];
            running -> [job_ejson(JobData) | Acc];
            finished -> Acc
        end
    end,
    couch_replicator_jobs:fold_jobs(undefined, FoldFun, []).


job(Id0) when is_binary(Id0) ->
    Id1 = couch_replicator_ids:convert(Id0),
    JobId = case couch_replicator_jobs:get_job_id(undefined, Id1) of
        {ok, JobId0} -> JobId0;
        {error, not_found} -> Id1
    end,
    case couch_replicator_jobs:get_job_data(undefined, JobId) of
        {ok, #{} = JobData} -> {ok, job_ejson(JobData)};
        {error, not_found} -> {error, not_found}
    end.


docs(#{} = Db, States) when is_list(States) ->
    DbName = fabric2_db:name(Db),
    FoldFun = fun(_JTx, _JobId, _, JobData, Acc) ->
        case JobData of
            #{?DB_NAME := DbName, ?STATE := State} ->
                case {States, lists:member(State, States)} of
                    {[], _} ->  [doc_ejson(JobData) | Acc];
                    {[_ | _], true} ->  [doc_ejson(JobData) | Acc];
                    {[_ | _], false} -> Acc
                end;
            #{} ->
                Acc
        end
    end,
    couch_replicator_jobs:fold_jobs(undefined, FoldFun, []).


doc(#{} = Db, DocId) when is_binary(DocId) ->
    DbUUID = fabric2_db:get_uuid(Db),
    JobId = couch_replicator_ids:job_id(DbUUID, DocId),
    case couch_replicator_jobs:get_job_data(undefined, JobId) of
        {ok, #{} = JobData} -> {ok, doc_ejson(JobData)};
        {error, not_found} ->  {error, not_found}
    end.


after_db_create(DbName, DbUUID) when ?IS_REP_DB(DbName)->
    couch_stats:increment_counter([couch_replicator, docs, dbs_created]),
    try fabric2_db:open(DbName, [{uuid, DbUUID}, ?ADMIN_CTX]) of
        {ok, Db} ->
            fabric2_fdb:transactional(Db, fun(TxDb) ->
                ok = add_jobs_from_db(TxDb)
            end)
    catch
        error:database_does_not_exist ->
            ok
    end;

after_db_create(_DbName, _DbUUID) ->
    ok.


after_db_delete(DbName, DbUUID) when ?IS_REP_DB(DbName) ->
    couch_stats:increment_counter([couch_replicator, docs, dbs_deleted]),
    FoldFun = fun(JTx, JobId, _, JobData, ok) ->
        case JobData of
            #{?DB_UUID := DbUUID} ->
                ok = couch_replicator_jobs:remove_job(JTx, JobId);
            #{} ->
                ok
        end
    end,
    couch_replicator_jobs:fold_jobs(undefined, FoldFun, ok);

after_db_delete(_DbName, _DbUUID) ->
    ok.


after_doc_write(#{name := DbName} = Db, #doc{} = Doc, _NewWinner, _OldWinner,
        _NewRevId, _Seq) when ?IS_REP_DB(DbName) ->
    couch_stats:increment_counter([couch_replicator, docs, db_changes]),
    {Props} = Doc#doc.body,
    case couch_util:get_value(?REPLICATION_STATE, Props) of
        ?ST_COMPLETED -> ok;
        ?ST_FAILED -> ok;
        _ -> process_change(Db, Doc)
    end;

after_doc_write(_Db, _Doc, _NewWinner, _OldWinner, _NewRevId, _Seq) ->
    ok.


% This is called from supervisor, must return ignore.
-spec ensure_rep_db_exists() -> ignore.
ensure_rep_db_exists() ->
    couch_replicator_jobs:set_timeout(),
    case config:get_boolean("replicator", "create_replicator_db", false) of
        true ->
            UserCtx = #user_ctx{roles=[<<"_admin">>, <<"_replicator">>]},
            Opts = [{user_ctx, UserCtx}, sys_db],
            case fabric2_db:create(?REP_DB_NAME, Opts) of
                {error, file_exists} -> ok;
                {ok, _Db} -> ok
            end;
        false ->
            ok
    end,
    ignore.


% Testing and debug functions

rescan_jobs() ->
    rescan_jobs(?REP_DB_NAME).


rescan_jobs(DbName) when is_binary(DbName), ?IS_REP_DB(DbName) ->
    try fabric2_db:open(DbName, [?ADMIN_CTX]) of
        {ok, Db} ->
            after_db_create(DbName, fabric2_db:get_uuid(Db))
    catch
        error:database_does_not_exist ->
            database_does_not_exist
    end.


reenqueue_jobs() ->
    reenqueue_jobs(?REP_DB_NAME).


reenqueue_jobs(DbName) when is_binary(DbName), ?IS_REP_DB(DbName) ->
    try fabric2_db:open(DbName, [?ADMIN_CTX]) of
        {ok, Db} ->
            DbUUID = fabric2_db:get_uuid(Db),
            ok = after_db_delete(DbName, DbUUID),
            ok = after_db_create(DbName, DbUUID)
    catch
        error:database_does_not_exist ->
            database_does_not_exist
    end.


remove_jobs() ->
    % If we clear a large number of jobs make sure to use batching so we don't
    % take too long, if use individual transactions, and also don't timeout if
    % use a single transaction
    FoldFun = fun
        (_, JobId, _, _, Acc) when length(Acc) > 250 ->
            couch_replicator_jobs:remove_jobs(undefined, [JobId | Acc]);
        (_, JobId, _, _, Acc) ->
            [JobId | Acc]
    end,
    Acc = couch_replicator_jobs:fold_jobs(undefined, FoldFun, []),
    [] = couch_replicator_jobs:remove_jobs(undefined, Acc),
    ok.


get_job_ids() ->
    couch_replicator_jobs:get_job_ids(undefined).


% Private functions

-spec start_transient_job(binary(), #{}) -> ok.
start_transient_job(JobId, #{} = Rep) ->
    JobData = couch_replicator_jobs:new_job(Rep, null, null, null,
        ?ST_PENDING, null, null),
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(), fun(JTx) ->
        case couch_replicator_jobs:get_job_data(JTx, JobId) of
            {ok, #{?REP := OldRep, ?STATE := State}} ->
                SameRep = couch_replicator_utils:compare_reps(Rep, OldRep),
                Active = State =:= ?ST_PENDING orelse State =:= ?ST_RUNNING,
                case SameRep andalso Active of
                    true ->
                        % If a job with the same paremeters is running we don't
                        % stop and just ignore the request. This is mainly for
                        % compatibility where users are able to idempotently
                        % POST the same job without it being stopped and
                        % restarted.
                        ok;
                    false ->
                        couch_replicator_jobs:add_job(JTx, JobId, JobData)
                end;
            {error, not_found} ->
                ok = couch_replicator_jobs:add_job(JTx, JobId, JobData)
        end
    end).


-spec cancel_replication(job_id()) ->
    {ok, {cancelled, binary()}} | {error, not_found}.
cancel_replication(JobId) when is_binary(JobId) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(), fun(JTx) ->
        Id = case couch_replicator_jobs:get_job_data(JTx, JobId) of
            {ok, #{?REP_ID := RepId}} when is_binary(RepId) ->
                RepId;
            _ ->
                JobId
        end,
        couch_log:notice("Canceling replication '~s'", [Id]),
        case couch_replicator_jobs:remove_job(JTx, JobId) of
            {error, not_found} ->
                {error, not_found};
            ok ->
                {ok, {cancelled, Id}}
        end
    end).


process_change(_Db, #doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>}) ->
    ok;

process_change(#{} = Db, #doc{deleted = true} = Doc) ->
    DbUUID = fabric2_db:get_uuid(Db),
    JobId = couch_replicator_ids:job_id(DbUUID, Doc#doc.id),
    couch_replicator_jobs:remove_job(undefined, JobId);

process_change(#{} = Db, #doc{deleted = false} = Doc) ->
    #doc{id = DocId, body = {Props} = Body} = Doc,
    DbName = fabric2_db:name(Db),
    DbUUID = fabric2_db:get_uuid(Db),
    {Rep, DocState, Error} = try
        Rep0 = couch_replicator_parse:parse_rep_doc(Body),
        DocState0 = couch_util:get_value(?REPLICATION_STATE, Props, null),
        {Rep0, DocState0, null}
    catch
        throw:{bad_rep_doc, Reason} ->
            {null, null, couch_replicator_utils:rep_error_to_binary(Reason)}
    end,
    JobId = couch_replicator_ids:job_id(DbUUID, DocId),
    JobData = case Rep of
        null ->
            couch_relicator_jobs:new_job(Rep, DbName, DbUUID, DocId,
                ?ST_FAILED, Error, null);
        #{} ->
            couch_replicator_jobs:new_job(Rep, DbName, DbUUID, DocId,
                ?ST_PENDING, null, DocState)
    end,

    LogMsg = "~p : replication doc update db:~s doc:~s job_id:~s doc_state:~s",
    couch_log:notice(LogMsg, [?MODULE, DbName, DocId, JobId, DocState]),

    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Db), fun(JTx) ->
        case couch_replicator_jobs:get_job_data(JTx, JobId) of
            {ok, #{?REP := null, ?STATE_INFO := Error}} when Rep =:= null ->
                % Same error as before occurred, don't bother updating the job
                ok;
            {ok, #{?REP := null}} when Rep =:= null ->
                % New error so the job is updated
                couch_replicator_jobs:add_job(JTx, JobId, JobData);
            {ok, #{?REP := OldRep, ?STATE := State}} when is_map(Rep) ->
                SameRep = couch_replicator_utils:compare_reps(Rep, OldRep),
                Active = State =:= ?ST_PENDING orelse State =:= ?ST_RUNNING,
                case SameRep andalso Active of
                    true ->
                        % Document was changed but none of the parameters
                        % relevent for the replication job have changed, so
                        % make it a no-op
                        ok;
                    false ->
                        couch_replicator_jobs:add_job(JTx, JobId, JobData)
                end;
            {error, not_found} ->
                couch_replicator_jobs:add_job(JTx, JobId, JobData)
        end

    end).


-spec add_jobs_from_db(#{}) -> ok.
add_jobs_from_db(#{} = TxDb) ->
    FoldFun  = fun
        ({meta, _Meta}, ok) ->
            {ok, ok};
        (complete, ok) ->
            {ok, ok};
        ({row, Row}, ok) ->
            Db = TxDb#{tx := undefined},
            ok = process_change(Db, get_doc(TxDb, Row)),
            {ok, ok}
    end,
    Opts = [{restart_tx, true}],
    {ok, ok} = fabric2_db:fold_docs(TxDb, FoldFun, ok, Opts),
    ok.


-spec get_doc(#{}, list()) -> #doc{}.
get_doc(TxDb, Row) ->
    {_, DocId} = lists:keyfind(id, 1, Row),
    {ok, #doc{deleted = false} = Doc} = fabric2_db:open_doc(TxDb, DocId, []),
    Doc.


doc_ejson(#{} = JobData) ->
    #{
        ?REP := Rep,
        ?REP_ID := RepId,
        ?DB_NAME := DbName,
        ?DOC_ID := DocId,
        ?STATE := State,
        ?STATE_INFO := Info0,
        ?ERROR_COUNT := ErrorCount,
        ?LAST_UPDATED := LastUpdatedSec,
        ?REP_NODE := Node,
        ?REP_PID := Pid,
        ?REP_STATS := Stats
    } = JobData,

    #{
        ?SOURCE := #{<<"url">> := Source, <<"proxy_url">> := SourceProxy},
        ?TARGET := #{<<"url">> := Target, <<"proxy_url">> := TargetProxy},
        ?START_TIME := StartSec
    } = Rep,

    LastUpdatedISO8601 = couch_replicator_utils:iso8601(LastUpdatedSec),
    StartISO8601 = couch_replicator_utils:iso8601(StartSec),

    Info = case State of
        ?ST_RUNNING -> Stats;
        ?ST_PENDING -> Stats;
        _Other -> Info0
    end,

    #{
        <<"id">> => RepId,
        <<"database">> => DbName,
        <<"doc_id">> => DocId,
        <<"source">> => ejson_url(Source),
        <<"target">> => ejson_url(Target),
        <<"source_proxy">> => ejson_url(SourceProxy),
        <<"target_proxy">> => ejson_url(TargetProxy),
        <<"state">> => State,
        <<"info">> => Info,
        <<"error_count">> => ErrorCount,
        <<"last_updated">> => LastUpdatedISO8601,
        <<"start_time">> => StartISO8601,
        <<"node">> => Node,
        <<"pid">> => Pid
    }.


job_ejson(#{} = JobData) ->
    #{
        ?REP := Rep,
        ?REP_ID := RepId,
        ?DB_NAME := DbName,
        ?DOC_ID := DocId,
        ?STATE := State,
        ?STATE_INFO := Info0,
        ?JOB_HISTORY := History,
        ?REP_STATS := Stats
    } = JobData,

    #{
        ?SOURCE := #{<<"url">> := Source},
        ?TARGET := #{<<"url">> := Target},
        ?REP_USER := User,
        ?START_TIME := StartSec
    } = Rep,

    StartISO8601 = couch_replicator_utils:iso8601(StartSec),

    History1 = lists:map(fun(#{?HIST_TIMESTAMP := Ts} = Evt) ->
        Evt#{?HIST_TIMESTAMP := couch_replicator_utils:iso8601(Ts)}
    end, History),

    Info = case State of
        ?ST_RUNNING -> Stats;
        ?ST_PENDING -> Stats;
        _Other -> Info0
    end,

    #{
        <<"id">> => RepId,
        <<"database">> => DbName,
        <<"doc_id">> => DocId,
        <<"source">> => ejson_url(Source),
        <<"target">> => ejson_url(Target),
        <<"state">> => State,
        <<"info">> => Info,
        <<"user">> => User,
        <<"history">> => History1,
        <<"start_time">> => StartISO8601
    }.


ejson_url(Url) when is_binary(Url) ->
    list_to_binary(couch_util:url_strip_password(Url));

ejson_url(null) ->
    null.


-spec check_authorization(rep_id(), #user_ctx{}) -> ok | not_found.
check_authorization(JobId, #user_ctx{} = Ctx) when is_binary(JobId) ->
    #user_ctx{name = Name} = Ctx,
    case couch_replicator_jobs:get_job_data(undefined, JobId) of
        {error, not_found} ->
            not_found;
        {ok, #{?DB_NAME := DbName}} when is_binary(DbName) ->
            throw({unauthorized, <<"Persistent replication collision">>});
        {ok, #{?REP := #{?REP_USER := Name}}} ->
            ok;
        {ok, #{}} ->
            couch_httpd:verify_is_server_admin(Ctx)
    end.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

authorization_test_() ->
    {
        foreach,
        fun () -> ok end,
        fun (_) -> meck:unload() end,
        [
            t_admin_is_always_authorized(),
            t_username_must_match(),
            t_replication_not_found()
        ]
    }.


t_admin_is_always_authorized() ->
    ?_test(begin
        expect_job_data({ok, #{?REP => #{?REP_USER => <<"someuser">>}}}),
        UserCtx = #user_ctx{name = <<"adm">>, roles = [<<"_admin">>]},
        ?assertEqual(ok, check_authorization(<<"RepId">>, UserCtx))
    end).


t_username_must_match() ->
    ?_test(begin
        expect_job_data({ok, #{?REP => #{?REP_USER => <<"user1">>}}}),
        UserCtx1 = #user_ctx{name = <<"user1">>, roles = [<<"somerole">>]},
        ?assertEqual(ok, check_authorization(<<"RepId">>, UserCtx1)),
        UserCtx2 = #user_ctx{name = <<"other">>, roles = [<<"somerole">>]},
        ?assertThrow({unauthorized, _}, check_authorization(<<"RepId">>,
            UserCtx2))
    end).


t_replication_not_found() ->
    ?_test(begin
        expect_job_data({error, not_found}),
        UserCtx1 = #user_ctx{name = <<"user">>, roles = [<<"somerole">>]},
        ?assertEqual(not_found, check_authorization(<<"RepId">>, UserCtx1)),
        UserCtx2 = #user_ctx{name = <<"adm">>, roles = [<<"_admin">>]},
        ?assertEqual(not_found, check_authorization(<<"RepId">>, UserCtx2))
    end).


expect_job_data(JobDataRes) ->
    meck:expect(couch_replicator_jobs, get_job_data, 2, JobDataRes).


-endif.
