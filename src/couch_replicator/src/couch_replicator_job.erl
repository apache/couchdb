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

-module(couch_replicator_job).

-behaviour(gen_server).

-export([
    start_link/0
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    format_status/2,
    code_change/3
]).

-export([
    accept/0,
    health_threshold/0
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").
-include("couch_replicator.hrl").
-include_lib("kernel/include/logger.hrl").

-define(LOWEST_SEQ, 0).
-define(DEFAULT_CHECKPOINT_INTERVAL, 30000).
-define(STARTUP_JITTER_DEFAULT, 5000).
-define(DEFAULT_MIN_BACKOFF_PENALTY_SEC, 32).
-define(DEFAULT_MAX_BACKOFF_PENALTY_SEC, 2 * 24 * 3600).
-define(DEFAULT_HEALTH_THRESHOLD_SEC, 2 * 60).
-define(DEFAULT_MAX_HISTORY, 10).
-define(DEFAULT_STATS_UPDATE_INTERVAL_SEC, 10).

-record(rep_state, {
    job,
    job_data,
    id,
    base_id,
    doc_id,
    db_name,
    db_uuid,
    source_name,
    target_name,
    source,
    target,
    history,
    checkpoint_history,
    start_seq,
    committed_seq,
    current_through_seq,
    seqs_in_progress = [],
    highest_seq_done = {0, ?LOWEST_SEQ},
    source_log,
    target_log,
    rep_starttime,
    src_starttime,
    tgt_starttime,
    checkpoint_timer,
    stats_timer,
    changes_queue,
    changes_manager,
    changes_reader,
    workers,
    stats = couch_replicator_stats:new(),
    session_id,
    source_seq = nil,
    use_checkpoints = true,
    checkpoint_interval = ?DEFAULT_CHECKPOINT_INTERVAL,
    user = null,
    options = #{}
}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, delayed_init, 0}.

terminate(normal, #rep_state{} = State) ->
    #rep_state{
        job = Job,
        job_data = JobData,
        checkpoint_history = History
    } = State,
    ok = complete_job(undefined, Job, JobData, History),
    close_endpoints(State);
terminate(shutdown, #rep_state{} = State0) ->
    % Replication stopped by the job server
    State1 = cancel_timers(State0),
    State3 =
        case do_checkpoint(State1) of
            {ok, State2} ->
                State2;
            Error ->
                ?LOG_ERROR(#{
                    what => checkpoint_failure,
                    in => replicator,
                    jobid => State1#rep_state.id,
                    details => Error
                }),
                Msg = "~p : Failed last checkpoint. Job: ~p Error: ~p",
                couch_log:error(Msg, [?MODULE, State1#rep_state.id, Error]),
                State1
        end,
    #rep_state{job = Job, job_data = JobData} = State3,
    ok = reschedule(undefined, Job, JobData),
    ok = close_endpoints(State3);
terminate({shutdown, Error}, {init_error, Stack}) ->
    % Termination in init, before the job had initialized
    case Error of
        max_backoff ->
            ?LOG_WARNING(#{what => job_backoff, in => replicator}),
            couch_log:warning("~p job backed off", [?MODULE]);
        finished ->
            ?LOG_NOTICE(#{what => job_finished_during_init, in => replicator}),
            couch_log:notice("~p job finished in init", [?MODULE]);
        _ ->
            ?LOG_ERROR(#{
                what => job_failure,
                in => replicator,
                details => Error,
                stacktrace => Stack
            }),
            couch_log:error("~p job failed ~p ~p", [?MODULE, Error, Stack])
    end,
    ok;
terminate({shutdown, finished}, #rep_state{} = State) ->
    % Job state was already updated and job is marked as finished
    ok = close_endpoints(State);
terminate({shutdown, halt}, #rep_state{} = State) ->
    % Job is re-enqueued and possibly already running somewhere else
    ?LOG_ERROR(#{
        what => job_halted,
        in => replicator,
        jobid => State#rep_state.id
    }),
    couch_log:error("~p job ~p halted", [?MODULE, State#rep_state.id]),
    ok = close_endpoints(State);
terminate(Reason0, #rep_state{} = State0) ->
    State = update_job_state(State0),
    Reason =
        case Reason0 of
            {shutdown, Err} -> Err;
            _ -> Reason0
        end,
    #rep_state{
        id = RepId,
        job = Job,
        job_data = JobData,
        source_name = Source,
        target_name = Target
    } = State,
    ?LOG_ERROR(#{
        what => job_failure,
        in => replicator,
        replication_id => RepId,
        source => Source,
        target => Target,
        details => Reason
    }),
    couch_log:error(
        "Replication `~s` (`~s` -> `~s`) failed: ~p",
        [RepId, Source, Target, Reason]
    ),
    ok = reschedule_on_error(undefined, Job, JobData, Reason),
    ok = close_endpoints(State).

handle_call({add_stats, Stats}, From, State) ->
    gen_server:reply(From, ok),
    NewStats = couch_replicator_stats:sum_stats(State#rep_state.stats, Stats),
    {noreply, State#rep_state{stats = NewStats}};
handle_call({report_seq_done, Seq, StatsInc}, From, #rep_state{} = State) ->
    #rep_state{
        seqs_in_progress = SeqsInProgress,
        highest_seq_done = HighestDone,
        current_through_seq = ThroughSeq,
        stats = Stats
    } = State,
    gen_server:reply(From, ok),
    {NewThroughSeq0, NewSeqsInProgress} =
        case SeqsInProgress of
            [] ->
                {Seq, []};
            [Seq | Rest] ->
                {Seq, Rest};
            [_ | _] ->
                {ThroughSeq, ordsets:del_element(Seq, SeqsInProgress)}
        end,
    NewHighestDone = lists:max([HighestDone, Seq]),
    NewThroughSeq =
        case NewSeqsInProgress of
            [] ->
                lists:max([NewThroughSeq0, NewHighestDone]);
            _ ->
                NewThroughSeq0
        end,
    ?LOG_DEBUG(#{
        what => progress_report,
        in => replicator,
        old => #{
            highest_seq_done => HighestDone,
            current_through_seq => ThroughSeq,
            seqs_in_progress => SeqsInProgress
        },
        new => #{
            highest_seq_done => NewHighestDone,
            current_through_seq => NewThroughSeq,
            seqs_in_progress => NewSeqsInProgress
        },
        worker_reported_seq => Seq
    }),
    couch_log:debug(
        "Worker reported seq ~p, through seq was ~p, "
        "new through seq is ~p, highest seq done was ~p, "
        "new highest seq done is ~p~n"
        "Seqs in progress were: ~p~nSeqs in progress are now: ~p",
        [
            Seq,
            ThroughSeq,
            NewThroughSeq,
            HighestDone,
            NewHighestDone,
            SeqsInProgress,
            NewSeqsInProgress
        ]
    ),
    NewState = State#rep_state{
        stats = couch_replicator_stats:sum_stats(Stats, StatsInc),
        current_through_seq = NewThroughSeq,
        seqs_in_progress = NewSeqsInProgress,
        highest_seq_done = NewHighestDone
    },
    {noreply, maybe_update_job_state(NewState)};
handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.

handle_cast(
    {report_seq, Seq},
    #rep_state{seqs_in_progress = SeqsInProgress} = State
) ->
    NewSeqsInProgress = ordsets:add_element(Seq, SeqsInProgress),
    {noreply, State#rep_state{seqs_in_progress = NewSeqsInProgress}};
handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.

handle_info(timeout, delayed_init) ->
    try delayed_init() of
        {ok, State} -> {noreply, State};
        {stop, Reason, State} -> {stop, Reason, State}
    catch
        exit:{shutdown, Exit}:Stack when Exit =:= finished orelse Exit =:= halt ->
            {stop, {shutdown, Exit}, {init_error, Stack}};
        _Tag:Error:Stack ->
            ShutdownReason = {error, replication_start_error(Error)},
            {stop, {shutdown, ShutdownReason}, {init_error, Stack}}
    end;
handle_info(stats_update, #rep_state{} = State) ->
    State1 = cancel_stats_timer(State),
    State2 = update_job_state(State1),
    {noreply, State2};
handle_info(checkpoint, State0) ->
    State = cancel_checkpoint_timer(State0),
    ok = check_user_filter(State),
    case do_checkpoint(State) of
        {ok, State1} ->
            couch_stats:increment_counter([
                couch_replicator,
                checkpoints,
                success
            ]),
            {noreply, start_checkpoint_timer(State1)};
        Error ->
            couch_stats:increment_counter([
                couch_replicator,
                checkpoints,
                failure
            ]),
            {stop, Error, State}
    end;
handle_info(shutdown, St) ->
    {stop, shutdown, St};
handle_info({'EXIT', Pid, max_backoff}, State) ->
    ?LOG_ERROR(#{what => max_backoff, in => replicator, pid => Pid}),
    couch_log:error("Max backoff reached child process ~p", [Pid]),
    {stop, {shutdown, max_backoff}, State};
handle_info({'EXIT', Pid, {shutdown, max_backoff}}, State) ->
    ?LOG_ERROR(#{what => max_backoff, in => replicator, pid => Pid}),
    couch_log:error("Max backoff reached child process ~p", [Pid]),
    {stop, {shutdown, max_backoff}, State};
handle_info({'EXIT', Pid, normal}, #rep_state{changes_reader = Pid} = State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason0}, #rep_state{changes_reader = Pid} = State) ->
    couch_stats:increment_counter([couch_replicator, changes_reader_deaths]),
    Reason =
        case Reason0 of
            {changes_req_failed, _, _} = HttpFail ->
                HttpFail;
            {http_request_failed, _, _, {error, {code, Code}}} ->
                {changes_req_failed, Code};
            {http_request_failed, _, _, {error, Err}} ->
                {changes_req_failed, Err};
            Other ->
                {changes_reader_died, Other}
        end,
    ?LOG_ERROR(#{what => changes_reader_crash, in => replicator, details => Reason}),
    couch_log:error("ChangesReader process died with reason: ~p", [Reason]),
    {stop, {shutdown, Reason}, cancel_timers(State)};
handle_info({'EXIT', Pid, normal}, #rep_state{changes_manager = Pid} = State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, #rep_state{changes_manager = Pid} = State) ->
    couch_stats:increment_counter([couch_replicator, changes_manager_deaths]),
    ?LOG_ERROR(#{what => changes_manager_crash, in => replicator, details => Reason}),
    couch_log:error("ChangesManager process died with reason: ~p", [Reason]),
    {stop, {shutdown, {changes_manager_died, Reason}}, cancel_timers(State)};
handle_info({'EXIT', Pid, normal}, #rep_state{changes_queue = Pid} = State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, #rep_state{changes_queue = Pid} = State) ->
    couch_stats:increment_counter([couch_replicator, changes_queue_deaths]),
    ?LOG_ERROR(#{what => changes_queue_crash, in => replicator, details => Reason}),
    couch_log:error("ChangesQueue process died with reason: ~p", [Reason]),
    {stop, {shutdown, {changes_queue_died, Reason}}, cancel_timers(State)};
handle_info({'EXIT', Pid, normal}, #rep_state{workers = Workers} = State) ->
    case Workers -- [Pid] of
        Workers ->
            %% Processes might be linked by replicator's auth plugins so
            %% we tolerate them exiting `normal` here and don't crash
            ?LOG_WARNING(#{
                what => linked_process_exit,
                in => replicator,
                pid => Pid,
                reason => normal
            }),
            LogMsg = "~p: unknown pid exited `normal` ~p",
            couch_log:error(LogMsg, [?MODULE, Pid]),
            {noreply, State#rep_state{workers = Workers}};
        [] ->
            catch unlink(State#rep_state.changes_manager),
            catch exit(State#rep_state.changes_manager, kill),
            do_last_checkpoint(State);
        Workers2 ->
            {noreply, State#rep_state{workers = Workers2}}
    end;
handle_info({'EXIT', Pid, Reason}, #rep_state{workers = Workers} = State) ->
    State2 = cancel_timers(State),
    case lists:member(Pid, Workers) of
        false ->
            {stop, {unknown_process_died, Pid, Reason}, State2};
        true ->
            couch_stats:increment_counter([couch_replicator, worker_deaths]),
            StopReason =
                case Reason of
                    {shutdown, _} = Err ->
                        Err;
                    Other ->
                        ?LOG_ERROR(#{
                            what => worker_crash,
                            in => replicator,
                            pid => Pid,
                            details => Reason
                        }),
                        ErrLog = "Worker ~p died with reason: ~p",
                        couch_log:error(ErrLog, [Pid, Reason]),
                        {worker_died, Pid, Other}
                end,
            {stop, StopReason, State2}
    end;
handle_info({Ref, ready}, St) when is_reference(Ref) ->
    ?LOG_NOTICE(#{
        what => spurious_future_ready_message,
        in => replicator,
        ref => Ref
    }),
    LogMsg = "~p : spurious erlfdb future ready message ~p",
    couch_log:notice(LogMsg, [?MODULE, Ref]),
    {noreply, St};
handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.

format_status(_Opt, [_PDict, State]) ->
    #rep_state{
        id = Id,
        source = Source,
        target = Target,
        start_seq = StartSeq,
        source_seq = SourceSeq,
        committed_seq = CommitedSeq,
        current_through_seq = ThroughSeq,
        highest_seq_done = HighestSeqDone,
        session_id = SessionId,
        doc_id = DocId,
        db_name = DbName,
        options = Options
    } = state_strip_creds(State),
    [
        {rep_id, Id},
        {source, couch_replicator_api_wrap:db_uri(Source)},
        {target, couch_replicator_api_wrap:db_uri(Target)},
        {db_name, DbName},
        {doc_id, DocId},
        {options, Options},
        {session_id, SessionId},
        {start_seq, StartSeq},
        {source_seq, SourceSeq},
        {committed_seq, CommitedSeq},
        {current_through_seq, ThroughSeq},
        {highest_seq_done, HighestSeqDone}
    ].

code_change(_OldVsn, #rep_state{} = State, _Extra) ->
    {ok, State}.

accept() ->
    couch_stats:increment_counter([couch_replicator, jobs, accepts]),
    Now = erlang:system_time(second),
    case couch_replicator_jobs:accept_job(Now + 5) of
        {ok, Job, #{?REP := Rep} = JobData} ->
            Normal =
                case Rep of
                    #{?OPTIONS := #{} = Options} ->
                        not maps:get(<<"continuous">>, Options, false);
                    _ ->
                        true
                end,
            couch_replicator_job_server:accepted(self(), Normal),
            {ok, Job, JobData};
        {error, not_found} ->
            timer:sleep(accept_jitter_msec()),
            ?MODULE:accept()
    end.

% Health threshold is the minimum amount of time an unhealthy job should run
% crashing before it is considered to be healthy again. HealtThreashold should
% not be 0 as jobs could start and immediately crash, and it shouldn't be
% infinity, since then  consecutive crashes would accumulate forever even if
% job is back to normal.
health_threshold() ->
    config:get_integer(
        "replicator",
        "health_threshold_sec",
        ?DEFAULT_HEALTH_THRESHOLD_SEC
    ).

delayed_init() ->
    {ok, Job, JobData} = accept(),
    try do_init(Job, JobData) of
        State = #rep_state{} -> {ok, State}
    catch
        exit:{http_request_failed, _, _, max_backoff}:Stack ->
            reschedule_on_error(undefined, Job, JobData, max_backoff),
            {stop, {shutdown, max_backoff}, {init_error, Stack}};
        exit:{shutdown, Exit}:Stack when Exit =:= finished orelse Exit =:= halt ->
            {stop, {shutdown, Exit}, {init_error, Stack}};
        _Tag:Error:Stack ->
            Reason = {error, replication_start_error(Error)},
            ?LOG_ERROR(#{
                what => job_failure_during_init,
                job => Job,
                details => Reason,
                stacktrace => Stack
            }),
            ErrMsg = "~p : job ~p failed during startup ~p stack:~p",
            couch_log:error(ErrMsg, [?MODULE, Job, Reason, Stack]),
            reschedule_on_error(undefined, Job, JobData, Reason),
            {stop, {shutdown, Reason}, {init_error, Stack}}
    end.

do_init(Job, #{} = JobData) ->
    couch_stats:increment_counter([couch_replicator, jobs, starts]),
    % This may make a network request, then may fail and reschedule the job
    {RepId, BaseId} = get_rep_id(undefined, Job, JobData),
    #{
        ?DB_NAME := DbName,
        ?DB_UUID := DbUUID,
        ?DOC_ID := DocId
    } = JobData,

    ok = couch_replicator_docs:remove_state_fields(DbName, DbUUID, DocId),

    % Finish if job is in a failed state already
    case JobData of
        #{?STATE := ?ST_FAILED, ?STATE_INFO := Error} ->
            ok = fail_job(undefined, Job, JobData, Error),
            exit({shutdown, finished});
        #{?STATE := St} when is_binary(St), St =/= ?ST_FAILED ->
            ok
    end,

    JobsTx = couch_jobs_fdb:get_jtx(),
    {Job1, JobData1, Owner} = couch_jobs_fdb:tx(JobsTx, fun(JTx) ->
        init_job_data(JTx, Job, JobData, RepId, BaseId)
    end),

    % Handle ownership decision here to be outside of the transaction
    case Owner of
        owner -> ok;
        not_owner -> exit({shutdown, finished})
    end,

    #rep_state{
        source = Source,
        target = Target,
        start_seq = {_Ts, StartSeq},
        options = Options,
        doc_id = DocId,
        db_name = DbName
    } = State = init_state(Job1, JobData1),

    NumWorkers = maps:get(<<"worker_processes">>, Options),
    BatchSize = maps:get(<<"worker_batch_size">>, Options),
    {ok, ChangesQueue} = couch_work_queue:new([
        {max_items, BatchSize * NumWorkers * 2},
        {max_size, 100 * 1024 * NumWorkers}
    ]),

    % This starts the _changes reader process. It adds the changes from the
    % source db to the ChangesQueue.
    {ok, ChangesReader} = couch_replicator_changes_reader:start_link(
        StartSeq,
        Source,
        ChangesQueue,
        Options
    ),

    % Changes manager - responsible for dequeing batches from the changes queue
    % and deliver them to the worker processes.
    ChangesManager = spawn_changes_manager(self(), ChangesQueue, BatchSize),

    % This starts the worker processes. They ask the changes queue manager for
    % a a batch of _changes rows to process -> check which revs are missing in
    % the target, and for the missing ones, it copies them from the source to
    % the target.
    MaxConns = maps:get(<<"http_connections">>, Options),
    Workers = lists:map(
        fun(_) ->
            couch_stats:increment_counter([couch_replicator, workers_started]),
            {ok, Pid} = couch_replicator_worker:start_link(
                self(),
                Source,
                Target,
                ChangesManager,
                MaxConns
            ),
            Pid
        end,
        lists:seq(1, NumWorkers)
    ),

    log_replication_start(State),

    State1 = State#rep_state{
        changes_queue = ChangesQueue,
        changes_manager = ChangesManager,
        changes_reader = ChangesReader,
        workers = Workers
    },

    update_job_state(State1).

init_job_data(#{jtx := true} = JTx, Job, #{} = JobData, RepId, BaseId) ->
    #{
        ?REP := Rep,
        ?REP_ID := OldRepId,
        ?DB_UUID := DbUUID,
        ?DOC_ID := DocId
    } = JobData,
    JobId = couch_replicator_ids:job_id(Rep, DbUUID, DocId),
    Now = erlang:system_time(second),
    JobData1 = JobData#{
        ?REP_ID := RepId,
        ?BASE_ID := BaseId,
        ?STATE := ?ST_RUNNING,
        ?STATE_INFO := null,
        ?LAST_START := Now,
        ?REP_NODE := erlang:atom_to_binary(node(), utf8),
        ?REP_PID := list_to_binary(pid_to_list(self())),
        ?LAST_UPDATED := Now
    },
    JobData2 =
        case is_binary(OldRepId) andalso OldRepId =/= RepId of
            true ->
                % Handle Replication ID change
                ok = couch_replicator_jobs:clear_old_rep_id(JTx, JobId, OldRepId),
                JobData1#{
                    ?REP_STATS := #{},
                    ?JOB_HISTORY := []
                };
            false ->
                JobData1
        end,
    JobData3 = hist_append(?HIST_STARTED, Now, JobData2, undefined),
    case check_ownership(JTx, Job, JobData3) of
        owner ->
            couch_stats:increment_counter([couch_replicator, jobs, starts]),
            {Job1, JobData4} = update_job_data(JTx, Job, JobData3),
            {Job1, JobData4, owner};
        not_owner ->
            {Job, JobData3, not_owner}
    end.

check_ownership(#{jtx := true} = JTx, Job, JobData) ->
    #{
        ?REP_ID := RepId,
        ?REP := Rep,
        ?DB_UUID := DbUUID,
        ?DOC_ID := DocId
    } = JobData,
    JobId = couch_replicator_ids:job_id(Rep, DbUUID, DocId),
    case couch_replicator_jobs:try_update_rep_id(JTx, JobId, RepId) of
        ok ->
            owner;
        {error, {replication_job_conflict, OtherJobId}} ->
            case couch_replicator_jobs:get_job_data(JTx, OtherJobId) of
                {ok, #{?STATE := S, ?DB_NAME := null}} when
                    S == ?ST_RUNNING; S == ?ST_PENDING
                ->
                    % Conflicting job is a transient job, not associated with a
                    % _replicator doc, so we let this job retry. This is also
                    % partly done for compatibility with pervious replicator
                    % behavior.
                    Error = <<"Duplicate job running: ", OtherJobId/binary>>,
                    reschedule_on_error(JTx, Job, JobData, Error),
                    not_owner;
                {ok, #{?STATE := S, ?DB_NAME := <<_/binary>>}} when
                    S == ?ST_RUNNING; S == ?ST_PENDING
                ->
                    % Conflicting job is a permanent replication job, so this
                    % job is marked as failed.
                    Error = <<"Duplicate job running: ", OtherJobId/binary>>,
                    fail_job(JTx, Job, JobData, Error),
                    not_owner;
                {ok, #{}} ->
                    ?LOG_WARNING(#{
                        what => duplicate_job_detected,
                        in => replicator,
                        jobid => JobId,
                        other_jobid => OtherJobId,
                        replication_id => RepId
                    }),
                    LogMsg = "~p : Job ~p usurping job ~p for replication ~p",
                    couch_log:warning(LogMsg, [
                        ?MODULE,
                        JobId,
                        OtherJobId,
                        RepId
                    ]),
                    couch_replicator_jobs:update_rep_id(JTx, JobId, RepId),
                    owner;
                {error, not_found} ->
                    ?LOG_ERROR(#{
                        what => orphaned_job_mapping,
                        in => replicator,
                        replication_id => RepId,
                        jobid => OtherJobId
                    }),
                    LogMsg = "~p : Orphan replication job reference ~p -> ~p",
                    couch_log:error(LogMsg, [?MODULE, RepId, OtherJobId]),
                    couch_replicator_jobs:update_rep_id(JTx, JobId, RepId),
                    owner
            end
    end.

update_job_data(Tx, #rep_state{} = State) ->
    #rep_state{job = Job, job_data = JobData} = State,
    {Job1, JobData1} = update_job_data(Tx, Job, JobData),
    State#rep_state{job = Job1, job_data = JobData1}.

update_job_data(Tx, Job, #{} = JobData) ->
    case couch_replicator_jobs:update_job_data(Tx, Job, JobData) of
        {ok, Job1} ->
            {Job1, JobData};
        {error, halt} ->
            exit({shutdown, halt})
    end.

update_active_task_info(#rep_state{} = State) ->
    #rep_state{
        job_data = JobData,
        user = User,
        id = RepId,
        db_name = DbName,
        doc_id = DocId,
        source_name = Source,
        target_name = Target,
        options = Options,
        highest_seq_done = {_, SourceSeq},
        checkpoint_interval = CheckpointInterval
    } = State,

    #{
        ?REP := #{?START_TIME := StartTime},
        ?REP_STATS := Stats,
        ?REP_NODE := Node,
        ?REP_PID := Pid,
        ?LAST_UPDATED := LastUpdated
    } = JobData,

    Info = maps:merge(Stats, #{
        <<"type">> => <<"replication">>,
        <<"user">> => User,
        <<"replication_id">> => RepId,
        <<"database">> => DbName,
        <<"doc_id">> => DocId,
        <<"source">> => ?l2b(Source),
        <<"target">> => ?l2b(Target),
        <<"continuous">> => maps:get(<<"continuous">>, Options, false),
        <<"source_seq">> => SourceSeq,
        <<"checkpoint_interval">> => CheckpointInterval,
        <<"node">> => Node,
        <<"pid">> => Pid,
        <<"updated_on">> => LastUpdated,
        <<"started_on">> => StartTime
    }),

    JobData1 = fabric2_active_tasks:update_active_task_info(JobData, Info),
    State#rep_state{job_data = JobData1}.

% Transient jobs don't get rescheduled on error with the exception of
% max_backoff errors.
%
reschedule_on_error(JTx, Job, #{?DB_NAME := null} = JobData, Error) when
    Error =/= max_backoff
->
    fail_job(JTx, Job, JobData, Error);
reschedule_on_error(JTx, Job, #{} = JobData0, Error0) ->
    Error = error_info(Error0),

    Now = erlang:system_time(second),

    JobData = maybe_heal(JobData0, Now),
    #{?ERROR_COUNT := ErrorCount} = JobData,
    JobData1 = JobData#{
        ?STATE := ?ST_CRASHING,
        ?STATE_INFO := Error,
        ?ERROR_COUNT := ErrorCount + 1,
        ?LAST_ERROR := Error,
        ?REP_NODE := null,
        ?REP_PID := null
    },
    JobData2 = hist_append(?HIST_CRASHED, Now, JobData1, Error),
    JobData3 = hist_append(?HIST_PENDING, Now, JobData2, undefined),
    JobData4 = fabric2_active_tasks:update_active_task_info(JobData3, #{}),

    couch_stats:increment_counter([couch_replicator, jobs, crashes]),

    Time = get_backoff_time(ErrorCount + 1),
    case couch_replicator_jobs:reschedule_job(JTx, Job, JobData4, Time) of
        ok -> ok;
        {error, halt} -> exit({shutdown, halt})
    end.

reschedule(JTx, Job, #{} = JobData) ->
    Now = erlang:system_time(second),

    JobData1 = JobData#{
        ?STATE := ?ST_PENDING,
        ?STATE_INFO := null,
        ?LAST_ERROR := null,
        ?REP_NODE := null,
        ?REP_PID := null
    },
    JobData2 = hist_append(?HIST_STOPPED, Now, JobData1, undefined),
    JobData3 = hist_append(?HIST_PENDING, Now, JobData2, undefined),
    JobData4 = fabric2_active_tasks:update_active_task_info(JobData3, #{}),

    couch_stats:increment_counter([couch_replicator, jobs, stops]),

    Time = Now + couch_replicator_job_server:scheduling_interval_sec(),
    case couch_replicator_jobs:reschedule_job(JTx, Job, JobData4, Time) of
        ok -> ok;
        {error, halt} -> exit({shutdown, halt})
    end.

fail_job(JTx, Job, #{} = JobData, Error0) ->
    Error = error_info(Error0),

    Now = erlang:system_time(second),

    #{
        ?ERROR_COUNT := ErrorCount,
        ?DB_NAME := DbName,
        ?DB_UUID := DbUUID,
        ?DOC_ID := DocId
    } = JobData,

    JobData1 = JobData#{
        ?STATE := ?ST_FAILED,
        ?STATE_INFO := Error,
        ?ERROR_COUNT := ErrorCount + 1,
        ?REP_NODE := null,
        ?REP_PID := null
    },
    JobData2 = hist_append(?HIST_CRASHED, Now, JobData1, Error),
    JobData3 = fabric2_active_tasks:update_active_task_info(JobData2, #{}),

    couch_stats:increment_counter([couch_replicator, jobs, crashes]),

    case couch_replicator_jobs:finish_job(JTx, Job, JobData3) of
        ok ->
            couch_replicator_docs:update_failed(DbName, DbUUID, DocId, Error),
            ok;
        {error, halt} ->
            exit({shutdown, halt})
    end.

complete_job(JTx, Job, #{} = JobData, CheckpointHistory) ->
    #{
        ?DB_NAME := Db,
        ?DB_UUID := DbUUID,
        ?DOC_ID := DocId,
        ?REP_STATS := RepStats,
        ?REP := Rep
    } = JobData,

    Now = erlang:system_time(second),

    #{?START_TIME := StartTime} = Rep,
    JobData1 = JobData#{
        ?STATE := ?ST_COMPLETED,
        ?CHECKPOINT_HISTORY := CheckpointHistory,
        ?STATE_INFO := RepStats,
        ?REP_NODE := null,
        ?REP_PID := null
    },
    JobData2 = hist_append(?HIST_STOPPED, Now, JobData1, undefined),
    JobData3 = fabric2_active_tasks:update_active_task_info(JobData2, #{}),

    couch_stats:increment_counter([couch_replicator, jobs, stops]),

    case couch_replicator_jobs:finish_job(JTx, Job, JobData3) of
        ok ->
            StartISO8601 = couch_replicator_utils:iso8601(StartTime),
            Stats = maps:merge(RepStats, #{<<"start_time">> => StartISO8601}),
            couch_replicator_docs:update_completed(Db, DbUUID, DocId, Stats),
            ok;
        {error, halt} ->
            exit({shutdown, halt})
    end.

error_info(Error0) ->
    case Error0 of
        <<_/binary>> ->
            Error0;
        undefined ->
            undefined;
        null ->
            null;
        Atom when is_atom(Atom) ->
            atom_to_binary(Atom, utf8);
        {shutdown, Atom} when is_atom(Atom) ->
            atom_to_binary(Atom, utf8);
        {shutdown, Err} ->
            couch_replicator_utils:rep_error_to_binary(Err);
        {error, Atom} when is_atom(Atom) ->
            atom_to_binary(Atom, utf8);
        {error, {Err, Reason}} when is_atom(Err) ->
            ReasonBin = couch_replicator_utils:rep_error_to_binary(Reason),
            #{
                <<"error">> => atom_to_binary(Err, utf8),
                <<"reason">> => ReasonBin
            };
        _Other ->
            couch_replicator_utils:rep_error_to_binary(Error0)
    end.

get_rep_id(JTx, Job, #{} = JobData) ->
    #{?REP := Rep} = JobData,
    try
        couch_replicator_ids:replication_id(Rep)
    catch
        throw:{filter_fetch_error, _} = Error ->
            reschedule_on_error(JTx, Job, JobData, {error, Error}),
            exit({shutdown, finished})
    end.

% After job run continuously for some time we consider it "healed" and reset
% its consecutive error count.
maybe_heal(#{} = JobData, Now) ->
    #{?LAST_START := LastStart} = JobData,
    case Now - LastStart > health_threshold() of
        true -> JobData#{?ERROR_COUNT := 0, ?LAST_ERROR := null};
        false -> JobData
    end.

get_backoff_time(ErrCnt) ->
    Max = min(max_backoff_penalty_sec(), 3600 * 24 * 30),
    Min = max(min_backoff_penalty_sec(), 2),

    % Calculate the max exponent so exponentiation doesn't blow up
    MaxExp = math:log2(Max) - math:log2(Min),

    % This is the recommended backoff amount
    Wait = Min * math:pow(2, min(ErrCnt, MaxExp)),

    % Apply a 25% jitter to avoid a thundering herd effect
    WaitJittered = Wait * 0.75 + rand:uniform(trunc(Wait * 0.25) + 1),
    erlang:system_time(second) + trunc(WaitJittered).

headers_strip_creds([], Acc) ->
    lists:reverse(Acc);
headers_strip_creds([{Key, Value0} | Rest], Acc) ->
    Value =
        case string:to_lower(Key) of
            "authorization" -> "****";
            _ -> Value0
        end,
    headers_strip_creds(Rest, [{Key, Value} | Acc]).

httpdb_strip_creds(#httpdb{url = Url, headers = Headers} = HttpDb) ->
    HttpDb#httpdb{
        url = couch_util:url_strip_password(Url),
        headers = headers_strip_creds(Headers, [])
    };
httpdb_strip_creds(LocalDb) ->
    LocalDb.

state_strip_creds(#rep_state{source = Source, target = Target} = State) ->
    State#rep_state{
        source = httpdb_strip_creds(Source),
        target = httpdb_strip_creds(Target)
    }.

adjust_maxconn(Src = #{<<"http_connections">> := 1}, RepId) ->
    ?LOG_NOTICE(#{
        what => minimum_source_connections_override,
        in => replicator,
        replication_id => RepId,
        details => "adjusting minimum source connections to 2"
    }),
    Msg = "Adjusting minimum number of HTTP source connections to 2 for ~p",
    couch_log:notice(Msg, [RepId]),
    Src#{<<"http_connections">> := 2};
adjust_maxconn(Src, _RepId) ->
    Src.

do_last_checkpoint(
    #rep_state{
        seqs_in_progress = [],
        highest_seq_done = {_Ts, ?LOWEST_SEQ}
    } = State
) ->
    {stop, normal, cancel_timers(State)};
do_last_checkpoint(
    #rep_state{
        seqs_in_progress = [],
        highest_seq_done = Seq
    } = State
) ->
    State1 = State#rep_state{current_through_seq = Seq},
    State2 = cancel_timers(State1),
    case do_checkpoint(State2) of
        {ok, State3} ->
            couch_stats:increment_counter([
                couch_replicator,
                checkpoints,
                success
            ]),
            {stop, normal, State3};
        Error ->
            couch_stats:increment_counter([
                couch_replicator,
                checkpoints,
                failure
            ]),
            {stop, Error, State2}
    end.

start_checkpoint_timer(#rep_state{} = State) ->
    CheckpointAfterMSec = State#rep_state.checkpoint_interval,
    JobTimeoutMSec = couch_replicator_jobs:get_timeout() * 1000,
    Wait1 = min(CheckpointAfterMSec, JobTimeoutMSec div 2),
    Wait2 = trunc(Wait1 * 0.75) + rand:uniform(trunc(Wait1 * 0.25)),
    TRef = erlang:send_after(Wait2, self(), checkpoint),
    State#rep_state{checkpoint_timer = TRef}.

cancel_checkpoint_timer(#rep_state{checkpoint_timer = nil} = State) ->
    State;
cancel_checkpoint_timer(#rep_state{checkpoint_timer = Timer} = State) ->
    erlang:cancel_timer(Timer),
    State#rep_state{checkpoint_timer = nil}.

start_stats_timer(#rep_state{} = State) ->
    MSec = stats_update_interval_sec() * 1000,
    TRef = erlang:send_after(MSec, self(), stats_update),
    State#rep_state{stats_timer = TRef}.

cancel_stats_timer(#rep_state{stats_timer = nil} = State) ->
    State;
cancel_stats_timer(#rep_state{stats_timer = Timer} = State) ->
    erlang:cancel_timer(Timer),
    receive
        stats_update -> ok
    after 0 -> ok
    end,
    State#rep_state{stats_timer = nil}.

cancel_timers(#rep_state{} = State) ->
    State1 = cancel_checkpoint_timer(State),
    cancel_stats_timer(State1).

init_state(#{} = Job, #{} = JobData) ->
    #{
        ?REP := Rep,
        ?REP_ID := Id,
        ?BASE_ID := BaseId,
        ?DB_NAME := DbName,
        ?DB_UUID := DbUUID,
        ?DOC_ID := DocId,
        ?LAST_ERROR := LastError
    } = JobData,
    #{
        ?SOURCE := Src0,
        ?TARGET := Tgt,
        ?START_TIME := StartTime,
        ?OPTIONS := Options0,
        ?REP_USER := User
    } = Rep,

    % Optimize replication parameters if last time the jobs crashed because it
    % was rate limited
    Options = optimize_rate_limited_job(Options0, LastError),

    % Adjust minimum number of http source connections to 2 to avoid deadlock
    Src = adjust_maxconn(Src0, BaseId),
    {ok, Source} = couch_replicator_api_wrap:db_open(Src),
    CreateTgt = maps:get(<<"create_target">>, Options, false),
    TParams = maps:get(<<"create_target_params">>, Options, #{}),

    {ok, Target} = couch_replicator_api_wrap:db_open(Tgt, CreateTgt, TParams),

    {ok, SourceInfo} = couch_replicator_api_wrap:get_db_info(Source),
    {ok, TargetInfo} = couch_replicator_api_wrap:get_db_info(Target),

    [SourceLog, TargetLog] = find_and_migrate_logs(
        [Source, Target],
        Rep,
        BaseId
    ),

    {StartSeq0, History, MatchedSessionIds} = compare_replication_logs(SourceLog, TargetLog),

    if
        not MatchedSessionIds ->
            ?LOG_NOTICE(#{
                what => session_history_mismatch,
                in => replicator,
                calculated_start_seq => StartSeq0,
                source => couch_replicator_api_wrap:db_uri(Source),
                target => couch_replicator_api_wrap:db_uri(Target),
                replication_id => Id,
                details => "scanned histories to find common ancestor"
            });
        true ->
            ok
    end,

    #{?REP_STATS := Stats0} = JobData,
    Stats1 = couch_replicator_stats:new(Stats0),
    HistoryStats =
        case History of
            [{[_ | _] = HProps} | _] -> couch_replicator_stats:new(HProps);
            _ -> couch_replicator_stats:new()
        end,
    Stats2 = couch_replicator_stats:max_stats(Stats1, HistoryStats),

    StartSeq1 = maps:get(<<"since_seq">>, Options, StartSeq0),
    StartSeq = {0, StartSeq1},

    SourceSeq = get_value(<<"update_seq">>, SourceInfo, ?LOWEST_SEQ),

    #doc{body = {CheckpointHistory}} = SourceLog,

    State = #rep_state{
        job = Job,
        job_data = JobData,
        id = Id,
        base_id = BaseId,
        source_name = couch_replicator_api_wrap:db_uri(Source),
        target_name = couch_replicator_api_wrap:db_uri(Target),
        source = Source,
        target = Target,
        options = Options,
        history = History,
        checkpoint_history = {[{<<"no_changes">>, true} | CheckpointHistory]},
        start_seq = StartSeq,
        current_through_seq = StartSeq,
        committed_seq = StartSeq,
        source_log = SourceLog,
        target_log = TargetLog,
        rep_starttime = StartTime,
        src_starttime = get_value(<<"instance_start_time">>, SourceInfo),
        tgt_starttime = get_value(<<"instance_start_time">>, TargetInfo),
        session_id = couch_uuids:random(),
        source_seq = SourceSeq,
        use_checkpoints = maps:get(<<"use_checkpoints">>, Options),
        checkpoint_interval = maps:get(<<"checkpoint_interval">>, Options),
        stats = Stats2,
        stats_timer = nil,
        doc_id = DocId,
        db_name = DbName,
        db_uuid = DbUUID,
        user = User
    },
    start_checkpoint_timer(State).

find_and_migrate_logs(DbList, #{} = Rep, BaseId) when is_binary(BaseId) ->
    LogId = ?l2b(?LOCAL_DOC_PREFIX ++ BaseId),
    fold_replication_logs(DbList, ?REP_ID_VERSION, LogId, LogId, Rep, []).

fold_replication_logs([], _Vsn, _LogId, _NewId, _Rep, Acc) ->
    lists:reverse(Acc);
fold_replication_logs([Db | Rest] = Dbs, Vsn, LogId, NewId, #{} = Rep, Acc) ->
    case couch_replicator_api_wrap:open_doc(Db, LogId, [ejson_body]) of
        {error, <<"not_found">>} when Vsn > 1 ->
            OldRepId = couch_replicator_ids:base_id(Rep, Vsn - 1),
            fold_replication_logs(
                Dbs,
                Vsn - 1,
                ?l2b(?LOCAL_DOC_PREFIX ++ OldRepId),
                NewId,
                Rep,
                Acc
            );
        {error, <<"not_found">>} ->
            fold_replication_logs(
                Rest,
                ?REP_ID_VERSION,
                NewId,
                NewId,
                Rep,
                [#doc{id = NewId} | Acc]
            );
        {ok, Doc} when LogId =:= NewId ->
            fold_replication_logs(
                Rest,
                ?REP_ID_VERSION,
                NewId,
                NewId,
                Rep,
                [Doc | Acc]
            );
        {ok, Doc} ->
            MigratedLog = #doc{id = NewId, body = Doc#doc.body},
            maybe_save_migrated_log(Rep, Db, MigratedLog, Doc#doc.id),
            fold_replication_logs(
                Rest,
                ?REP_ID_VERSION,
                NewId,
                NewId,
                Rep,
                [MigratedLog | Acc]
            )
    end.

maybe_save_migrated_log(#{?OPTIONS := Options}, Db, #doc{} = Doc, OldId) ->
    case maps:get(<<"use_checkpoints">>, Options) of
        true ->
            update_checkpoint(Db, Doc),
            ?LOG_NOTICE(#{
                what => migrated_checkpoint,
                in => replicator,
                db => httpdb_strip_creds(Db),
                old_id => OldId,
                new_id => Doc#doc.id
            }),
            Msg = "Migrated replication checkpoint. Db:~p ~p -> ~p",
            couch_log:notice(Msg, [httpdb_strip_creds(Db), OldId, Doc#doc.id]);
        false ->
            ok
    end.

spawn_changes_manager(Parent, ChangesQueue, BatchSize) ->
    spawn_link(fun() ->
        changes_manager_loop_open(Parent, ChangesQueue, BatchSize, 1)
    end).

changes_manager_loop_open(Parent, ChangesQueue, BatchSize, Ts) ->
    receive
        {get_changes, From} ->
            case couch_work_queue:dequeue(ChangesQueue, BatchSize) of
                closed ->
                    From ! {closed, self()};
                {ok, ChangesOrLastSeqs} ->
                    ReportSeq =
                        case lists:last(ChangesOrLastSeqs) of
                            {last_seq, Seq} -> {Ts, Seq};
                            #doc_info{high_seq = Seq} -> {Ts, Seq}
                        end,
                    Changes = lists:filter(
                        fun
                            (#doc_info{}) -> true;
                            ({last_seq, _Seq}) -> false
                        end,
                        ChangesOrLastSeqs
                    ),
                    ok = gen_server:cast(Parent, {report_seq, ReportSeq}),
                    From ! {changes, self(), Changes, ReportSeq}
            end,
            changes_manager_loop_open(Parent, ChangesQueue, BatchSize, Ts + 1)
    end.

do_checkpoint(#rep_state{use_checkpoints = false} = State) ->
    NewState = State#rep_state{
        checkpoint_history = {[{<<"use_checkpoints">>, false}]}
    },
    {ok, update_job_state(NewState)};
do_checkpoint(#rep_state{current_through_seq = S, committed_seq = S} = State) ->
    {ok, update_job_state(State)};
do_checkpoint(State) ->
    #rep_state{
        source_name = SourceName,
        target_name = TargetName,
        source = Source,
        target = Target,
        history = OldHistory,
        start_seq = {_, StartSeq},
        current_through_seq = {_Ts, NewSeq} = NewTsSeq,
        source_log = SourceLog,
        target_log = TargetLog,
        rep_starttime = RepStartTime,
        src_starttime = SrcInstanceStartTime,
        tgt_starttime = TgtInstanceStartTime,
        stats = Stats,
        options = Options,
        session_id = SessionId
    } = State,
    case commit_to_both(Source, Target) of
        {source_error, Reason} ->
            {checkpoint_commit_failure,
                <<"Failure on source commit: ", (couch_util:to_binary(Reason))/binary>>};
        {target_error, Reason} ->
            {checkpoint_commit_failure,
                <<"Failure on target commit: ", (couch_util:to_binary(Reason))/binary>>};
        {SrcInstanceStartTime, TgtInstanceStartTime} ->
            ?LOG_NOTICE(#{
                what => checkpoint,
                in => replicator,
                source => SourceName,
                target => TargetName,
                sequence => NewSeq
            }),
            couch_log:notice(
                "recording a checkpoint for `~s` -> `~s` at "
                "source update_seq ~p",
                [SourceName, TargetName, NewSeq]
            ),
            StartTime = couch_replicator_utils:rfc1123_local(RepStartTime),
            EndTime = couch_replicator_utils:rfc1123_local(),
            NewHistoryEntry =
                {[
                    {<<"session_id">>, SessionId},
                    {<<"start_time">>, StartTime},
                    {<<"end_time">>, EndTime},
                    {<<"start_last_seq">>, StartSeq},
                    {<<"end_last_seq">>, NewSeq},
                    {<<"recorded_seq">>, NewSeq},
                    {<<"missing_checked">>, couch_replicator_stats:missing_checked(Stats)},
                    {<<"missing_found">>, couch_replicator_stats:missing_found(Stats)},
                    {<<"docs_read">>, couch_replicator_stats:docs_read(Stats)},
                    {<<"docs_written">>, couch_replicator_stats:docs_written(Stats)},
                    {<<"doc_write_failures">>, couch_replicator_stats:doc_write_failures(Stats)}
                ]},
            BaseHistory =
                [
                    {<<"session_id">>, SessionId},
                    {<<"source_last_seq">>, NewSeq},
                    {<<"replication_id_version">>, ?REP_ID_VERSION}
                ] ++
                    case maps:get(<<"doc_ids">>, Options, undefined) of
                        undefined ->
                            [];
                        _DocIds ->
                            % backwards compatibility with the result of a replication
                            % by doc IDs in versions 0.11.x and 1.0.x TODO: deprecate
                            % (use same history format, simplify code)
                            [
                                {<<"start_time">>, StartTime},
                                {<<"end_time">>, EndTime},
                                {<<"docs_read">>, couch_replicator_stats:docs_read(Stats)},
                                {<<"docs_written">>, couch_replicator_stats:docs_written(Stats)},
                                {<<"doc_write_failures">>,
                                    couch_replicator_stats:doc_write_failures(Stats)}
                            ]
                    end,
            % limit history to 50 entries
            NewRepHistory = {
                BaseHistory ++ [{<<"history">>, lists:sublist([NewHistoryEntry | OldHistory], 50)}]
            },

            try
                {SrcRevPos, SrcRevId} = update_checkpoint(
                    Source,
                    SourceLog#doc{body = NewRepHistory},
                    source
                ),
                {TgtRevPos, TgtRevId} = update_checkpoint(
                    Target,
                    TargetLog#doc{body = NewRepHistory},
                    target
                ),
                NewState = State#rep_state{
                    checkpoint_history = NewRepHistory,
                    committed_seq = NewTsSeq,
                    source_log = SourceLog#doc{revs = {SrcRevPos, [SrcRevId]}},
                    target_log = TargetLog#doc{revs = {TgtRevPos, [TgtRevId]}}
                },
                {ok, update_job_state(NewState)}
            catch
                throw:{checkpoint_commit_failure, _} = Failure ->
                    Failure
            end;
        {SrcInstanceStartTime, _NewTgtInstanceStartTime} ->
            {checkpoint_commit_failure, <<
                "Target database out of sync. "
                "Try to increase max_dbs_open at the target's server."
            >>};
        {_NewSrcInstanceStartTime, TgtInstanceStartTime} ->
            {checkpoint_commit_failure, <<
                "Source database out of sync. "
                "Try to increase max_dbs_open at the source's server."
            >>};
        {_NewSrcInstanceStartTime, _NewTgtInstanceStartTime} ->
            {checkpoint_commit_failure, <<
                "Source and target databases out of "
                "sync. Try to increase max_dbs_open at both servers."
            >>}
    end.

update_checkpoint(Db, Doc, DbType) ->
    try
        update_checkpoint(Db, Doc)
    catch
        throw:{checkpoint_commit_failure, Reason} ->
            throw(
                {checkpoint_commit_failure,
                    <<"Error updating the ", (couch_util:to_binary(DbType))/binary,
                        " checkpoint document: ", (couch_util:to_binary(Reason))/binary>>}
            )
    end.

update_checkpoint(Db, #doc{id = LogId, body = LogBody} = Doc) ->
    try
        case couch_replicator_api_wrap:update_doc(Db, Doc, [delay_commit]) of
            {ok, PosRevId} -> PosRevId;
            {error, Reason} -> throw({checkpoint_commit_failure, Reason})
        end
    catch
        throw:conflict ->
            Opts = [ejson_body],
            case (catch couch_replicator_api_wrap:open_doc(Db, LogId, Opts)) of
                {ok, #doc{body = LogBody, revs = {Pos, [RevId | _]}}} ->
                    % This means that we were able to update successfully the
                    % checkpoint doc in a previous attempt but we got a connection
                    % error (timeout for e.g.) before receiving the success
                    % response. Therefore the request was retried and we got a
                    % conflict, as the revision we sent is not the current one. We
                    % confirm this by verifying the doc body we just got is the
                    % same that we have just sent.
                    {Pos, RevId};
                _ ->
                    throw({checkpoint_commit_failure, conflict})
            end
    end.

commit_to_both(Source, Target) ->
    % commit the src async
    ParentPid = self(),
    SrcCommitPid = spawn_link(fun() ->
        Result = (catch couch_replicator_api_wrap:ensure_full_commit(Source)),
        ParentPid ! {self(), Result}
    end),

    % commit tgt sync
    TgtResult = (catch couch_replicator_api_wrap:ensure_full_commit(Target)),

    SrcResult =
        receive
            {SrcCommitPid, Result} ->
                unlink(SrcCommitPid),
                receive
                    {'EXIT', SrcCommitPid, _} ->
                        ok
                after 0 -> ok
                end,
                Result;
            {'EXIT', SrcCommitPid, Reason} ->
                {error, Reason}
        end,
    case TgtResult of
        {ok, TargetStartTime} ->
            case SrcResult of
                {ok, SourceStartTime} ->
                    {SourceStartTime, TargetStartTime};
                SourceError ->
                    {source_error, SourceError}
            end;
        TargetError ->
            {target_error, TargetError}
    end.

compare_replication_logs(SrcDoc, TgtDoc) ->
    #doc{body = {RepRecProps}} = SrcDoc,
    #doc{body = {RepRecPropsTgt}} = TgtDoc,
    SrcSession = get_value(<<"session_id">>, RepRecProps),
    TgtSession = get_value(<<"session_id">>, RepRecPropsTgt),
    case SrcSession == TgtSession of
        true ->
            % if the records have the same session id,
            % then we have a valid replication history
            OldSeqNum = get_value(
                <<"source_last_seq">>,
                RepRecProps,
                ?LOWEST_SEQ
            ),
            OldHistory = get_value(<<"history">>, RepRecProps, []),
            {OldSeqNum, OldHistory, true};
        false ->
            SourceHistory = get_value(<<"history">>, RepRecProps, []),
            TargetHistory = get_value(<<"history">>, RepRecPropsTgt, []),
            couch_log:notice(
                "Replication records differ. "
                "Scanning histories to find a common ancestor.",
                []
            ),
            couch_log:debug(
                "Record on source:~p~nRecord on target:~p~n",
                [RepRecProps, RepRecPropsTgt]
            ),
            {StartSeq, History} = compare_rep_history(SourceHistory, TargetHistory),
            {StartSeq, History, false}
    end.

compare_rep_history(S, T) when S =:= [] orelse T =:= [] ->
    couch_log:notice("no common ancestry -- performing full replication", []),
    {?LOWEST_SEQ, []};
compare_rep_history([{S} | SourceRest], [{T} | TargetRest] = Target) ->
    SourceId = get_value(<<"session_id">>, S),
    case has_session_id(SourceId, Target) of
        true ->
            RecordSeqNum = get_value(<<"recorded_seq">>, S, ?LOWEST_SEQ),
            couch_log:notice(
                "found a common replication record with "
                "source_seq ~p",
                [RecordSeqNum]
            ),
            {RecordSeqNum, SourceRest};
        false ->
            TargetId = get_value(<<"session_id">>, T),
            case has_session_id(TargetId, SourceRest) of
                true ->
                    RecordSeqNum = get_value(
                        <<"recorded_seq">>,
                        T,
                        ?LOWEST_SEQ
                    ),
                    couch_log:notice(
                        "found a common replication record with "
                        "source_seq ~p",
                        [RecordSeqNum]
                    ),
                    {RecordSeqNum, TargetRest};
                false ->
                    compare_rep_history(SourceRest, TargetRest)
            end
    end.

has_session_id(_SessionId, []) ->
    false;
has_session_id(SessionId, [{Props} | Rest]) ->
    case get_value(<<"session_id">>, Props, nil) of
        SessionId -> true;
        _Else -> has_session_id(SessionId, Rest)
    end.

get_pending_count(#rep_state{} = St) ->
    #rep_state{
        highest_seq_done = HighestSeqDone,
        source = #httpdb{} = Db0
    } = St,
    {_, Seq} = HighestSeqDone,
    Db = Db0#httpdb{retries = 3},
    case (catch couch_replicator_api_wrap:get_pending_count(Db, Seq)) of
        {ok, Pending} ->
            Pending;
        _ ->
            null
    end.

maybe_update_job_state(#rep_state{} = State) ->
    case State#rep_state.stats_timer of
        nil -> start_stats_timer(State);
        Ref when is_reference(Ref) -> State
    end.

update_job_state(#rep_state{} = State0) ->
    State = cancel_stats_timer(State0),
    #rep_state{
        current_through_seq = {_, ThroughSeq},
        highest_seq_done = {_, HighestSeq},
        committed_seq = {_, CommittedSeq},
        stats = Stats,
        job_data = JobData
    } = State,

    Now = erlang:system_time(second),

    RevisionsChecked = couch_replicator_stats:missing_checked(Stats),
    MissingRevisions = couch_replicator_stats:missing_found(Stats),
    DocsRead = couch_replicator_stats:docs_read(Stats),
    DocsWritten = couch_replicator_stats:docs_written(Stats),
    DocWriteFailures = couch_replicator_stats:doc_write_failures(Stats),
    PendingCount = get_pending_count(State),

    StatsMap = #{
        <<"checkpointed_source_seq">> => CommittedSeq,
        <<"source_seq">> => HighestSeq,
        <<"through_seq">> => ThroughSeq,
        <<"revisions_checked">> => RevisionsChecked,
        <<"missing_revisions_found">> => MissingRevisions,
        <<"docs_read">> => DocsRead,
        <<"docs_written">> => DocsWritten,
        <<"doc_write_failures">> => DocWriteFailures,
        <<"changes_pending">> => PendingCount
    },

    JobData1 = JobData#{
        ?REP_STATS := StatsMap,
        ?LAST_UPDATED := Now
    },

    JobData2 = maybe_heal(JobData1, Now),

    State1 = State#rep_state{job_data = JobData2},
    State2 = update_active_task_info(State1),
    update_job_data(undefined, State2).

replication_start_error({unauthorized, DbUri}) ->
    {unauthorized, <<"unauthorized to access or create database ", DbUri/binary>>};
replication_start_error({db_not_found, DbUri}) ->
    {db_not_found, <<"could not open ", DbUri/binary>>};
replication_start_error(
    {http_request_failed, _Method, Url0, {error, {error, {conn_failed, {error, nxdomain}}}}}
) ->
    Url = ?l2b(couch_util:url_strip_password(Url0)),
    {nxdomain, <<"could not resolve ", Url/binary>>};
replication_start_error({http_request_failed, Method0, Url0, {error, {code, Code}}}) when
    is_integer(Code)
->
    Url = ?l2b(couch_util:url_strip_password(Url0)),
    Method = ?l2b(Method0),
    CodeBin = integer_to_binary(Code),
    {http_error_code, <<CodeBin/binary, " ", Method/binary, " ", Url/binary>>};
replication_start_error(Error) ->
    Error.

log_replication_start(#rep_state{} = RepState) ->
    #rep_state{
        id = Id,
        doc_id = DocId,
        db_name = DbName,
        options = Options,
        source_name = Source,
        target_name = Target,
        session_id = Sid
    } = RepState,
    Workers = maps:get(<<"worker_processes">>, Options),
    BatchSize = maps:get(<<"worker_batch_size">>, Options),
    From =
        case DbName of
            Name when is_binary(Name) ->
                io_lib:format("from doc ~s:~s", [Name, DocId]);
            _ ->
                "from _replicate endpoint"
        end,
    ?LOG_NOTICE(#{
        what => starting_replication,
        in => replicator,
        source => Source,
        target => Target,
        replication_db => DbName,
        replication_doc => DocId,
        session_id => Sid,
        worker_processes => Workers,
        worker_batch_size => BatchSize
    }),
    Msg =
        "Starting replication ~s (~s -> ~s) ~s worker_procesess:~p"
        " worker_batch_size:~p session_id:~s",
    couch_log:notice(Msg, [Id, Source, Target, From, Workers, BatchSize, Sid]).

check_user_filter(#rep_state{} = State) ->
    #rep_state{
        id = RepId,
        base_id = BaseId,
        job = Job,
        job_data = JobData
    } = State,
    case get_rep_id(undefined, Job, JobData) of
        {RepId, BaseId} ->
            ok;
        {NewId, NewBaseId} when is_binary(NewId), is_binary(NewBaseId) ->
            ?LOG_ERROR(#{
                what => replication_id_updated,
                in => replicator,
                old_id => RepId,
                new_id => NewId,
                details => "replication job shutting down"
            }),
            LogMsg = "~p : Replication id was updated ~p -> ~p",
            couch_log:error(LogMsg, [?MODULE, RepId, NewId]),
            reschedule(undefined, Job, JobData),
            exit({shutdown, finished})
    end.

hist_append(Type, Now, #{} = JobData, Info) when
    is_integer(Now),
    is_binary(Type)
->
    #{?JOB_HISTORY := Hist} = JobData,
    Evt1 = #{?HIST_TYPE => Type, ?HIST_TIMESTAMP => Now},
    Evt2 =
        case Info of
            undefined ->
                Evt1;
            null ->
                Evt1#{?HIST_REASON => null};
            <<_/binary>> ->
                Evt1#{?HIST_REASON => Info};
            #{<<"error">> := Err, <<"reason">> := Reason} when
                is_binary(Err),
                is_binary(Reason)
            ->
                Evt1#{?HIST_REASON => Reason}
        end,
    Hist1 = [Evt2 | Hist],
    Hist2 = lists:sublist(Hist1, max_history()),
    JobData#{?JOB_HISTORY := Hist2}.

optimize_rate_limited_job(#{} = Options, <<"max_backoff">>) ->
    OptimizedSettings = #{
        <<"checkpoint_interval">> => 5000,
        <<"worker_processes">> => 2,
        <<"worker_batch_size">> => 100,
        <<"http_connections">> => 2
    },
    maps:merge(Options, OptimizedSettings);
optimize_rate_limited_job(#{} = Options, _Other) ->
    Options.

close_endpoints(State) ->
    State1 = cancel_timers(State),
    couch_replicator_api_wrap:db_close(State1#rep_state.source),
    couch_replicator_api_wrap:db_close(State1#rep_state.target),
    ok.

get_value(K, Props) ->
    couch_util:get_value(K, Props).

get_value(K, Props, Default) ->
    couch_util:get_value(K, Props, Default).

accept_jitter_msec() ->
    couch_rand:uniform(erlang:max(1, max_startup_jitter_msec())).

max_startup_jitter_msec() ->
    config:get_integer(
        "replicator",
        "startup_jitter",
        ?STARTUP_JITTER_DEFAULT
    ).

min_backoff_penalty_sec() ->
    config:get_integer(
        "replicator",
        "min_backoff_penalty_sec",
        ?DEFAULT_MIN_BACKOFF_PENALTY_SEC
    ).

max_backoff_penalty_sec() ->
    config:get_integer(
        "replicator",
        "max_backoff_penalty_sec",
        ?DEFAULT_MAX_BACKOFF_PENALTY_SEC
    ).

max_history() ->
    config:get_integer("replicator", "max_history", ?DEFAULT_MAX_HISTORY).

stats_update_interval_sec() ->
    config:get_integer(
        "replicator",
        "stats_update_interval_sec",
        ?DEFAULT_STATS_UPDATE_INTERVAL_SEC
    ).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("fabric/test/fabric2_test.hrl").

replication_start_error_test() ->
    ?assertEqual(
        {unauthorized, <<
            "unauthorized to access or create database"
            " http://x/y"
        >>},
        replication_start_error({unauthorized, <<"http://x/y">>})
    ),
    ?assertEqual(
        {db_not_found, <<"could not open http://x/y">>},
        replication_start_error({db_not_found, <<"http://x/y">>})
    ),
    ?assertEqual(
        {nxdomain, <<"could not resolve http://x/y">>},
        replication_start_error(
            {http_request_failed, "GET", "http://x/y",
                {error, {error, {conn_failed, {error, nxdomain}}}}}
        )
    ),
    ?assertEqual(
        {http_error_code, <<"503 GET http://x/y">>},
        replication_start_error({http_request_failed, "GET", "http://x/y", {error, {code, 503}}})
    ).

scheduler_job_format_status_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_format_status)
        ]
    }.

setup() ->
    meck:expect(config, get, fun(_, _, Default) -> Default end).

teardown(_) ->
    meck:unload().

t_format_status(_) ->
    {ok, Rep} = couch_replicator_parse:parse_rep(
        #{
            <<"source">> => <<"http://u:p@h1/d1">>,
            <<"target">> => <<"http://u:p@h2/d2">>,
            <<"create_target">> => true
        },
        null
    ),
    State = #rep_state{
        id = <<"base+ext">>,
        job_data = #{?REP => Rep},
        doc_id = <<"mydoc">>,
        db_name = <<"mydb">>,
        source = maps:get(?SOURCE, Rep),
        target = maps:get(?TARGET, Rep),
        options = maps:get(?OPTIONS, Rep),
        session_id = <<"a">>,
        start_seq = <<"1">>,
        source_seq = <<"2">>,
        committed_seq = <<"3">>,
        current_through_seq = <<"4">>,
        highest_seq_done = <<"5">>
    },
    Format = format_status(opts_ignored, [pdict, State]),
    FmtOptions = proplists:get_value(options, Format),
    ?assertEqual("http://u:*****@h1/d1/", proplists:get_value(source, Format)),
    ?assertEqual("http://u:*****@h2/d2/", proplists:get_value(target, Format)),
    ?assertEqual(<<"base+ext">>, proplists:get_value(rep_id, Format)),
    ?assertEqual(true, maps:get(<<"create_target">>, FmtOptions)),
    ?assertEqual(<<"mydoc">>, proplists:get_value(doc_id, Format)),
    ?assertEqual(<<"mydb">>, proplists:get_value(db_name, Format)),
    ?assertEqual(<<"a">>, proplists:get_value(session_id, Format)),
    ?assertEqual(<<"1">>, proplists:get_value(start_seq, Format)),
    ?assertEqual(<<"2">>, proplists:get_value(source_seq, Format)),
    ?assertEqual(<<"3">>, proplists:get_value(committed_seq, Format)),
    ?assertEqual(<<"4">>, proplists:get_value(current_through_seq, Format)),
    ?assertEqual(<<"5">>, proplists:get_value(highest_seq_done, Format)).

-endif.
