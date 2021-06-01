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

-module(couch_views_indexer).

-export([
    spawn_link/0
]).

-export([
    init/0,
    map_docs/2,
    write_docs/4
]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

-include("couch_views.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("fabric/include/fabric2.hrl").
-include_lib("kernel/include/logger.hrl").

-define(KEY_SIZE_LIMIT, 8000).
-define(VALUE_SIZE_LIMIT, 64000).

-define(DEFAULT_TX_RETRY_LIMIT, 5).

% These are all of the errors that we can fix by using
% a smaller batch size.
-define(IS_RECOVERABLE_ERROR(Code),
    ((Code == ?ERLFDB_TIMED_OUT) orelse
        (Code == ?ERLFDB_TRANSACTION_TOO_OLD) orelse
        (Code == ?ERLFDB_TRANSACTION_TIMED_OUT) orelse
        (Code == ?ERLFDB_TRANSACTION_TOO_LARGE))
).

spawn_link() ->
    proc_lib:spawn_link(?MODULE, init, []).

init() ->
    Opts = #{no_schedule => true},
    {ok, Job, Data0} = couch_jobs:accept(?INDEX_JOB_TYPE, Opts),

    couch_views_server:accepted(self()),

    Data = upgrade_data(Data0),
    #{
        <<"db_name">> := DbName,
        <<"db_uuid">> := DbUUID,
        <<"ddoc_id">> := DDocId,
        <<"sig">> := JobSig,
        <<"retries">> := Retries
    } = Data,

    {ok, Db} =
        try
            fabric2_db:open(DbName, [?ADMIN_CTX, {uuid, DbUUID}])
        catch
            error:database_does_not_exist ->
                fail_job(Job, Data, db_deleted, "Database was deleted")
        end,

    {ok, DDoc} =
        case fabric2_db:open_doc(Db, DDocId) of
            {ok, DDoc0} ->
                {ok, DDoc0};
            {not_found, _} ->
                fail_job(Job, Data, ddoc_deleted, "Design document was deleted")
        end,

    {ok, Mrst} = couch_views_util:ddoc_to_mrst(DbName, DDoc),
    HexSig = fabric2_util:to_hex(Mrst#mrst.sig),

    if
        HexSig == JobSig -> ok;
        true -> fail_job(Job, Data, sig_changed, "Design document was modified")
    end,

    DbSeq = fabric2_fdb:transactional(Db, fun(TxDb) ->
        fabric2_fdb:with_snapshot(TxDb, fun(SSDb) ->
            fabric2_db:get_update_seq(SSDb)
        end)
    end),

    State = #{
        tx_db => undefined,
        db_uuid => DbUUID,
        db_seq => DbSeq,
        view_seq => undefined,
        last_seq => undefined,
        view_vs => undefined,
        job => Job,
        job_data => Data,
        rows_processed => 0,
        count => 0,
        changes_done => 0,
        doc_acc => [],
        design_opts => Mrst#mrst.design_opts,
        update_stats => #{},
        tx_retry_limit => tx_retry_limit(),
        db_read_vsn => ?VIEW_CURRENT_VSN,
        view_read_vsn => ?VIEW_CURRENT_VSN
    },

    try
        update(Db, Mrst, State)
    catch
        exit:normal ->
            ok;
        error:database_does_not_exist ->
            fail_job(Job, Data, db_deleted, "Database was deleted");
        Error:Reason:Stack ->
            ?LOG_ERROR(#{
                what => view_update_failure,
                db => DbName,
                ddoc => DDocId,
                tag => Error,
                details => Reason,
                stacktrace => Stack
            }),
            Fmt = "Error building view for ddoc ~s in ~s: ~p:~p ~p",
            couch_log:error(Fmt, [DbName, DDocId, Error, Reason, Stack]),

            NewRetry = Retries + 1,
            RetryLimit = retry_limit(),

            case should_retry(NewRetry, RetryLimit, Reason) of
                true ->
                    DataErr = Data#{<<"retries">> := NewRetry},
                    % Set the last_seq to 0 so that it doesn't trigger a
                    % successful view build for anyone listening to the
                    % couch_views_jobs:wait_for_job
                    % Note this won't cause the view to rebuild from 0 again
                    StateErr = State#{job_data := DataErr, last_seq := <<"0">>},
                    report_progress(StateErr, update);
                false ->
                    fail_job(Job, Data, Error, Reason)
            end
    end.

upgrade_data(Data) ->
    Defaults = [
        {<<"retries">>, 0},
        {<<"db_uuid">>, undefined}
    ],
    lists:foldl(
        fun({Key, Default}, Acc) ->
            case maps:is_key(Key, Acc) of
                true -> Acc;
                false -> maps:put(Key, Default, Acc)
            end
        end,
        Data,
        Defaults
    ),
    % initialize active task
    fabric2_active_tasks:update_active_task_info(Data, #{}).

% Transaction limit exceeded don't retry
should_retry(_, _, {erlfdb_error, ?ERLFDB_TRANSACTION_TOO_LARGE}) ->
    false;
should_retry(Retries, RetryLimit, _) when Retries < RetryLimit ->
    true;
should_retry(_, _, _) ->
    false.

add_error(error, {erlfdb_error, Code}, Data) ->
    CodeBin = couch_util:to_binary(Code),
    CodeString = erlfdb:get_error_string(Code),
    Data#{
        error => foundationdb_error,
        reason => list_to_binary([CodeBin, <<"-">>, CodeString])
    };
add_error(Error, Reason, Data) ->
    Data#{
        error => couch_util:to_binary(Error),
        reason => couch_util:to_binary(Reason)
    }.

update(#{} = Db, Mrst0, State0) ->
    Limit = couch_views_batch:start(Mrst0),
    Result =
        try
            do_update(Db, Mrst0, State0#{limit => Limit})
        catch
            error:{erlfdb_error, Error} when ?IS_RECOVERABLE_ERROR(Error) ->
                couch_views_batch:failure(Mrst0),
                update(Db, Mrst0, State0)
        end,
    case Result of
        ok ->
            % Already finished and released map context
            ok;
        {Mrst1, finished} ->
            couch_eval:release_map_context(Mrst1#mrst.qserver);
        {Mrst1, State1} ->
            #{
                update_stats := UpdateStats
            } = State1,
            couch_views_batch:success(Mrst1, UpdateStats),
            update(Db, Mrst1, State1)
    end.

do_update(Db, Mrst0, State0) ->
    TxOpts = #{retry_limit => maps:get(tx_retry_limit, State0)},
    TxResult = fabric2_fdb:transactional(Db, TxOpts, fun(TxDb) ->
        #{
            tx := Tx
        } = TxDb,

        Snapshot = TxDb#{tx := erlfdb:snapshot(Tx)},

        State1 = get_update_start_state(TxDb, Mrst0, State0),
        Mrst1 = couch_views_trees:open(TxDb, Mrst0),

        {ok, State2} = fold_changes(Snapshot, State1),

        #{
            doc_acc := DocAcc,
            last_seq := LastSeq,
            changes_done := ChangesDone0,
            design_opts := DesignOpts
        } = State2,

        DocAcc1 = fetch_docs(Snapshot, DesignOpts, DocAcc),

        {Mrst2, MappedDocs} = map_docs(Mrst0, DocAcc1),
        TotalKVs = write_docs(TxDb, Mrst1, MappedDocs, State2),

        ChangesDone = ChangesDone0 + length(DocAcc),

        UpdateStats = #{
            docs_read => length(DocAcc),
            tx_size => erlfdb:wait(erlfdb:get_approximate_size(Tx)),
            total_kvs => TotalKVs
        },

        case is_update_finished(State2) of
            true ->
                State3 = State2#{changes_done := ChangesDone},
                % We must call report_progress/2 (which, in turn calls
                % couch_jobs:update/3) in every transaction where indexing data
                % is updated, otherwise we risk another indexer taking over and
                % clobbering the indexing data
                State4 = report_progress(State3, update),
                {Mrst2, finished, State4#{
                    db_read_vsn := erlfdb:wait(erlfdb:get_read_version(Tx))
                }};
            false ->
                State3 = report_progress(State2, update),
                {Mrst2, continue, State3#{
                    tx_db := undefined,
                    count := 0,
                    doc_acc := [],
                    changes_done := ChangesDone,
                    view_seq := LastSeq,
                    update_stats := UpdateStats
                }}
        end
    end),
    case TxResult of
        {Mrst, continue, State} ->
            {Mrst, State};
        {Mrst, finished, State} ->
            do_finalize(Mrst, State),
            {Mrst, finished}
    end.

do_finalize(Mrst, State) ->
    #{tx_db := OldDb} = State,
    ViewReadVsn = erlfdb:get_committed_version(maps:get(tx, OldDb)),
    fabric2_fdb:transactional(OldDb#{tx := undefined}, fun(TxDb) ->
        % Use the recent committed version as the read
        % version. However, if transaction retries due to an error,
        % let it acquire its own version to avoid spinning
        % continuously due to conflicts or other errors.
        case erlfdb:get_last_error() of
            undefined ->
                erlfdb:set_read_version(maps:get(tx, TxDb), ViewReadVsn);
            ErrorCode when is_integer(ErrorCode) ->
                ok
        end,
        State1 = State#{
            tx_db := TxDb,
            view_read_vsn := ViewReadVsn
        },
        ViewVS = maps:get(view_vs, State1),
        maybe_set_build_status(TxDb, Mrst, ViewVS, ?INDEX_READY),
        report_progress(State1, finished)
    end).

is_update_finished(State) ->
    #{
        db_seq := DbSeq,
        last_seq := LastSeq,
        view_vs := ViewVs
    } = State,
    AtDbSeq = LastSeq == DbSeq,
    AtViewVs =
        case ViewVs of
            not_found -> false;
            _ -> LastSeq == fabric2_fdb:vs_to_seq(ViewVs)
        end,
    AtDbSeq orelse AtViewVs.

maybe_set_build_status(_TxDb, _Mrst1, not_found, _State) ->
    ok;
maybe_set_build_status(TxDb, Mrst1, _ViewVS, State) ->
    couch_views_fdb:set_build_status(TxDb, Mrst1, State).

% In the first iteration of update we need
% to populate our db and view sequences
get_update_start_state(TxDb, Mrst, #{view_seq := undefined} = State) ->
    #{
        view_vs := ViewVS,
        view_seq := ViewSeq
    } = couch_views_fdb:get_view_state(TxDb, Mrst),

    State#{
        tx_db := TxDb,
        view_vs := ViewVS,
        view_seq := ViewSeq,
        last_seq := ViewSeq
    };
get_update_start_state(TxDb, _Idx, State) ->
    State#{
        tx_db := TxDb
    }.

fold_changes(Snapshot, State) ->
    #{
        view_seq := SinceSeq,
        db_seq := DbSeq,
        limit := Limit
    } = State,

    FoldState = State#{
        rows_processed := 0
    },

    Fun = fun process_changes/2,
    Opts = [
        {end_key, fabric2_fdb:seq_to_vs(DbSeq)},
        {limit, Limit},
        {restart_tx, false}
    ],

    case fabric2_db:fold_changes(Snapshot, SinceSeq, Fun, FoldState, Opts) of
        {ok, #{rows_processed := 0} = FinalState} when Limit > 0 ->
            % If we read zero rows with a non-zero limit
            % it means we've caught up to the DbSeq as our
            % last_seq.
            {ok, FinalState#{last_seq := DbSeq}};
        Result ->
            Result
    end.

process_changes(Change, Acc) ->
    #{
        doc_acc := DocAcc,
        rows_processed := RowsProcessed,
        count := Count,
        design_opts := DesignOpts,
        view_vs := ViewVS
    } = Acc,

    #{
        id := Id,
        sequence := LastSeq
    } = Change,

    IncludeDesign = lists:keymember(<<"include_design">>, 1, DesignOpts),

    Acc1 =
        case {Id, IncludeDesign} of
            {<<?DESIGN_DOC_PREFIX, _/binary>>, false} ->
                maps:merge(Acc, #{
                    rows_processed => RowsProcessed + 1,
                    count => Count + 1,
                    last_seq => LastSeq
                });
            _ ->
                Acc#{
                    doc_acc := DocAcc ++ [Change],
                    rows_processed := RowsProcessed + 1,
                    count := Count + 1,
                    last_seq := LastSeq
                }
        end,

    DocVS = fabric2_fdb:seq_to_vs(LastSeq),

    Go = maybe_stop_at_vs(ViewVS, DocVS),
    {Go, Acc1}.

maybe_stop_at_vs({versionstamp, _} = ViewVS, DocVS) when DocVS >= ViewVS ->
    stop;
maybe_stop_at_vs(_, _) ->
    ok.

map_docs(Mrst, []) ->
    {Mrst, []};
map_docs(Mrst, Docs) ->
    % Run all the non deleted docs through the view engine and
    Mrst1 = start_query_server(Mrst),
    QServer = Mrst1#mrst.qserver,

    {Deleted0, NotDeleted0} = lists:partition(
        fun(Doc) ->
            #{deleted := Deleted} = Doc,
            Deleted
        end,
        Docs
    ),

    Deleted1 = lists:map(
        fun(Doc) ->
            Doc#{results => [[] || _ <- Mrst1#mrst.views]}
        end,
        Deleted0
    ),

    DocsToMap = lists:map(
        fun(Doc) ->
            #{doc := DocRec} = Doc,
            DocRec
        end,
        NotDeleted0
    ),

    {ok, AllResults} = couch_eval:map_docs(QServer, DocsToMap),

    % The expanded function head here is making an assertion
    % that the results match the given doc
    NotDeleted1 = lists:zipwith(
        fun(#{id := DocId} = Doc, {DocId, Results}) ->
            Doc#{results => Results}
        end,
        NotDeleted0,
        AllResults
    ),

    % I'm being a bit careful here resorting the docs
    % in order of the changes feed. Theoretically this is
    % unnecessary since we're inside a single transaction.
    % However, I'm concerned if we ever split this up
    % into multiple transactions that this detail might
    % be important but forgotten.
    MappedDocs = lists:sort(
        fun(A, B) ->
            #{sequence := ASeq} = A,
            #{sequence := BSeq} = B,
            ASeq =< BSeq
        end,
        Deleted1 ++ NotDeleted1
    ),

    {Mrst1, MappedDocs}.

write_docs(TxDb, Mrst, Docs0, State) ->
    #mrst{
        sig = Sig
    } = Mrst,

    #{
        last_seq := LastSeq
    } = State,

    KeyLimit = key_size_limit(),
    ValLimit = value_size_limit(),

    {Docs1, TotalKVCount} = lists:mapfoldl(
        fun(Doc0, KVCount) ->
            Doc1 = check_kv_size_limit(Mrst, Doc0, KeyLimit, ValLimit),
            {Doc1, KVCount + count_kvs(Doc1)}
        end,
        0,
        Docs0
    ),

    couch_views_trees:update_views(TxDb, Mrst, Docs1),

    if
        LastSeq == false -> ok;
        true -> couch_views_fdb:set_update_seq(TxDb, Sig, LastSeq)
    end,

    TotalKVCount.

fetch_docs(Db, DesignOpts, Changes) ->
    {Deleted, NotDeleted} = lists:partition(
        fun(Doc) ->
            #{deleted := Deleted} = Doc,
            Deleted
        end,
        Changes
    ),

    RevState = lists:foldl(
        fun(Change, Acc) ->
            #{id := Id} = Change,
            RevFuture = fabric2_fdb:get_winning_revs_future(Db, Id, 1),
            Acc#{
                RevFuture => {Id, Change}
            }
        end,
        #{},
        NotDeleted
    ),

    RevFutures = maps:keys(RevState),
    BodyState = lists:foldl(
        fun(RevFuture, Acc) ->
            {Id, Change} = maps:get(RevFuture, RevState),
            Revs = fabric2_fdb:get_revs_wait(Db, RevFuture),

            % I'm assuming that in this changes transaction that the winning
            % doc body exists since it is listed in the changes feed as not deleted
            #{winner := true} = RevInfo = lists:last(Revs),
            BodyFuture = fabric2_fdb:get_doc_body_future(Db, Id, RevInfo),
            Acc#{
                BodyFuture => {Id, RevInfo, Change}
            }
        end,
        #{},
        erlfdb:wait_for_all(RevFutures)
    ),

    AddLocalSeq = fabric2_util:get_value(<<"local_seq">>, DesignOpts, false),

    BodyFutures = maps:keys(BodyState),
    ChangesWithDocs = lists:map(
        fun(BodyFuture) ->
            {Id, RevInfo, Change} = maps:get(BodyFuture, BodyState),
            Doc = fabric2_fdb:get_doc_body_wait(Db, Id, RevInfo, BodyFuture),

            Doc1 =
                case maps:get(branch_count, RevInfo, 1) of
                    1 when AddLocalSeq ->
                        {ok, DocWithLocalSeq} = fabric2_db:apply_open_doc_opts(
                            Doc,
                            [RevInfo],
                            [local_seq]
                        ),
                        DocWithLocalSeq;
                    1 ->
                        Doc;
                    _ ->
                        RevConflicts = fabric2_fdb:get_all_revs(Db, Id),
                        DocOpts =
                            if
                                not AddLocalSeq -> [];
                                true -> [local_seq]
                            end,

                        {ok, DocWithConflicts} = fabric2_db:apply_open_doc_opts(
                            Doc,
                            RevConflicts,
                            [conflicts | DocOpts]
                        ),
                        DocWithConflicts
                end,
            Change#{doc => Doc1}
        end,
        erlfdb:wait_for_all(BodyFutures)
    ),

    % This combines the deleted changes with the changes that contain docs
    % Important to note that this is now unsorted. Which is fine for now
    % But later could be an issue if we split this across transactions
    Deleted ++ ChangesWithDocs.

start_query_server(#mrst{qserver = nil} = Mrst) ->
    #mrst{
        db_name = DbName,
        idx_name = DDocId,
        language = Language,
        sig = Sig,
        lib = Lib,
        views = Views
    } = Mrst,
    case
        couch_eval:acquire_map_context(
            DbName,
            DDocId,
            Language,
            Sig,
            Lib,
            [View#mrview.def || View <- Views]
        )
    of
        {ok, QServer} ->
            Mrst#mrst{qserver = QServer};
        {error, Error} ->
            error(Error)
    end;
start_query_server(#mrst{} = Mrst) ->
    Mrst.

check_kv_size_limit(Mrst, Doc, KeyLimit, ValLimit) ->
    #mrst{
        db_name = DbName,
        idx_name = IdxName
    } = Mrst,
    #{
        results := Results
    } = Doc,
    try
        lists:foreach(
            fun(ViewRows) ->
                lists:foreach(
                    fun({K, V}) ->
                        KeySize = couch_ejson_size:encoded_size(K),
                        ValSize = couch_ejson_size:encoded_size(V),

                        if
                            KeySize =< KeyLimit -> ok;
                            true -> throw({size_error, key})
                        end,

                        if
                            ValSize =< ValLimit -> ok;
                            true -> throw({size_error, value})
                        end
                    end,
                    ViewRows
                )
            end,
            Results
        ),
        Doc
    catch
        throw:{size_error, Type} ->
            #{id := DocId} = Doc,
            ?LOG_ERROR(#{
                what => lists:concat(["oversized_", Type]),
                db => DbName,
                docid => DocId,
                index => IdxName
            }),
            Fmt =
                "View ~s size error for docid `~s`, excluded from indexing "
                "in db `~s` for design doc `~s`",
            couch_log:error(Fmt, [Type, DocId, DbName, IdxName]),
            Doc#{
                deleted := true,
                results := [[] || _ <- Mrst#mrst.views],
                kv_sizes => []
            }
    end.

count_kvs(Doc) ->
    #{
        results := Results
    } = Doc,
    lists:foldl(
        fun(ViewRows, Count) ->
            Count + length(ViewRows)
        end,
        0,
        Results
    ).

report_progress(State, UpdateType) ->
    #{
        tx_db := TxDb,
        job := Job1,
        job_data := JobData,
        last_seq := LastSeq,
        db_seq := DBSeq,
        changes_done := ChangesDone,
        db_read_vsn := DbReadVsn,
        view_read_vsn := ViewReadVsn
    } = State,

    #{
        <<"db_name">> := DbName,
        <<"db_uuid">> := DbUUID,
        <<"ddoc_id">> := DDocId,
        <<"sig">> := Sig,
        <<"retries">> := Retries
    } = JobData,

    ActiveTasks = fabric2_active_tasks:get_active_task_info(JobData),
    TotalDone =
        case maps:get(<<"changes_done">>, ActiveTasks, 0) of
            0 -> ChangesDone;
            N -> N + ChangesDone
        end,

    NewActiveTasks = couch_views_util:active_tasks_info(
        TotalDone,
        DbName,
        DDocId,
        LastSeq,
        DBSeq
    ),

    % Reconstruct from scratch to remove any
    % possible existing error state.
    NewData0 = #{
        <<"db_name">> => DbName,
        <<"db_uuid">> => DbUUID,
        <<"ddoc_id">> => DDocId,
        <<"sig">> => Sig,
        <<"view_seq">> => LastSeq,
        <<"retries">> => Retries,
        <<"db_read_vsn">> => DbReadVsn,
        <<"view_read_vsn">> => ViewReadVsn
    },
    NewData = fabric2_active_tasks:update_active_task_info(
        NewData0,
        NewActiveTasks
    ),

    case UpdateType of
        update ->
            case couch_jobs:update(TxDb, Job1, NewData) of
                {ok, Job2} ->
                    State#{job := Job2};
                {error, halt} ->
                    ?LOG_ERROR(#{what => job_halted, job => Job1}),
                    couch_log:error("~s job halted :: ~w", [?MODULE, Job1]),
                    exit(normal)
            end;
        finished ->
            case couch_jobs:finish(TxDb, Job1, NewData) of
                ok ->
                    State;
                {error, halt} ->
                    ?LOG_ERROR(#{what => job_halted, job => Job1}),
                    couch_log:error("~s job halted :: ~w", [?MODULE, Job1]),
                    exit(normal)
            end
    end.

fail_job(Job, Data, Error, Reason) ->
    NewData = add_error(Error, Reason, Data),
    couch_jobs:finish(undefined, Job, NewData),
    exit(normal).

retry_limit() ->
    config:get_integer("couch_views", "retry_limit", 3).

key_size_limit() ->
    config:get_integer("couch_views", "key_size_limit", ?KEY_SIZE_LIMIT).

value_size_limit() ->
    config:get_integer("couch_views", "value_size_limit", ?VALUE_SIZE_LIMIT).

tx_retry_limit() ->
    config:get_integer(
        "couch_views",
        "indexer_tx_retry_limit",
        ?DEFAULT_TX_RETRY_LIMIT
    ).
