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

-module(couch_replicator_scheduler_job).

-behaviour(gen_server).

-export([
   start_link/1
]).

-export([
   init/1,
   terminate/2,
   handle_call/3,
   handle_info/2,
   handle_cast/2,
   code_change/3,
   format_status/2
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").
-include("couch_replicator_scheduler.hrl").
-include("couch_replicator.hrl").

-import(couch_util, [
    get_value/2,
    get_value/3,
    to_binary/1
]).

-import(couch_replicator_utils, [
    pp_rep_id/1
]).


-define(LOWEST_SEQ, 0).
-define(DEFAULT_CHECKPOINT_INTERVAL, 30000).
-define(STARTUP_JITTER_DEFAULT, 5000).

-record(rep_state, {
    rep_details,
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
    timer, % checkpoint timer
    changes_queue,
    changes_manager,
    changes_reader,
    workers,
    stats = couch_replicator_stats:new(),
    session_id,
    source_monitor = nil,
    target_monitor = nil,
    source_seq = nil,
    use_checkpoints = true,
    checkpoint_interval = ?DEFAULT_CHECKPOINT_INTERVAL,
    type = db,
    view = nil
}).


start_link(#rep{id = {BaseId, Ext}, source = Src, target = Tgt} = Rep) ->
    RepChildId = BaseId ++ Ext,
    Source = couch_replicator_api_wrap:db_uri(Src),
    Target = couch_replicator_api_wrap:db_uri(Tgt),
    ServerName = {global, {?MODULE, Rep#rep.id}},

    case gen_server:start_link(ServerName, ?MODULE, Rep, []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            couch_log:warning("failed to start replication `~s` (`~s` -> `~s`)",
                           [RepChildId, Source, Target]),
            {error, Reason}
    end.


init(InitArgs) ->
    {ok, InitArgs, 0}.


do_init(#rep{options = Options, id = {BaseId, Ext}, user_ctx=UserCtx} = Rep) ->
    process_flag(trap_exit, true),

    timer:sleep(startup_jitter()),

    #rep_state{
        source = Source,
        target = Target,
        source_name = SourceName,
        target_name = TargetName,
        start_seq = {_Ts, StartSeq},
        highest_seq_done = {_, HighestSeq},
        checkpoint_interval = CheckpointInterval
    } = State = init_state(Rep),

    NumWorkers = get_value(worker_processes, Options),
    BatchSize = get_value(worker_batch_size, Options),
    {ok, ChangesQueue} = couch_work_queue:new([
        {max_items, BatchSize * NumWorkers * 2},
        {max_size, 100 * 1024 * NumWorkers}
    ]),
    % This starts the _changes reader process. It adds the changes from
    % the source db to the ChangesQueue.
    {ok, ChangesReader} = couch_replicator_changes_reader:start_link(
        StartSeq, Source, ChangesQueue, Options
    ),
    % Changes manager - responsible for dequeing batches from the changes queue
    % and deliver them to the worker processes.
    ChangesManager = spawn_changes_manager(self(), ChangesQueue, BatchSize),
    % This starts the worker processes. They ask the changes queue manager for a
    % a batch of _changes rows to process -> check which revs are missing in the
    % target, and for the missing ones, it copies them from the source to the target.
    MaxConns = get_value(http_connections, Options),
    Workers = lists:map(
        fun(_) ->
            couch_stats:increment_counter([couch_replicator, workers_started]),
            {ok, Pid} = couch_replicator_worker:start_link(
                self(), Source, Target, ChangesManager, MaxConns),
            Pid
        end,
        lists:seq(1, NumWorkers)),

    couch_task_status:add_task([
        {type, replication},
        {user, UserCtx#user_ctx.name},
        {replication_id, ?l2b(BaseId ++ Ext)},
        {database, Rep#rep.db_name},
        {doc_id, Rep#rep.doc_id},
        {source, ?l2b(SourceName)},
        {target, ?l2b(TargetName)},
        {continuous, get_value(continuous, Options, false)},
        {source_seq, HighestSeq},
        {checkpoint_interval, CheckpointInterval}
    ] ++ rep_stats(State)),
    couch_task_status:set_update_frequency(1000),

    % Until OTP R14B03:
    %
    % Restarting a temporary supervised child implies that the original arguments
    % (#rep{} record) specified in the MFA component of the supervisor
    % child spec will always be used whenever the child is restarted.
    % This implies the same replication performance tunning parameters will
    % always be used. The solution is to delete the child spec (see
    % cancel_replication/1) and then start the replication again, but this is
    % unfortunately not immune to race conditions.

    log_replication_start(State),
    couch_log:debug("Worker pids are: ~p", [Workers]),

    doc_update_triggered(Rep),

    {ok, State#rep_state{
            changes_queue = ChangesQueue,
            changes_manager = ChangesManager,
            changes_reader = ChangesReader,
            workers = Workers
        }
    }.


handle_call({add_stats, Stats}, From, State) ->
    gen_server:reply(From, ok),
    NewStats = couch_replicator_utils:sum_stats(State#rep_state.stats, Stats),
    {noreply, State#rep_state{stats = NewStats}};

handle_call({report_seq_done, Seq, StatsInc}, From,
    #rep_state{seqs_in_progress = SeqsInProgress, highest_seq_done = HighestDone,
        current_through_seq = ThroughSeq, stats = Stats} = State) ->
    gen_server:reply(From, ok),
    {NewThroughSeq0, NewSeqsInProgress} = case SeqsInProgress of
    [] ->
        {Seq, []};
    [Seq | Rest] ->
        {Seq, Rest};
    [_ | _] ->
        {ThroughSeq, ordsets:del_element(Seq, SeqsInProgress)}
    end,
    NewHighestDone = lists:max([HighestDone, Seq]),
    NewThroughSeq = case NewSeqsInProgress of
    [] ->
        lists:max([NewThroughSeq0, NewHighestDone]);
    _ ->
        NewThroughSeq0
    end,
    couch_log:debug("Worker reported seq ~p, through seq was ~p, "
        "new through seq is ~p, highest seq done was ~p, "
        "new highest seq done is ~p~n"
        "Seqs in progress were: ~p~nSeqs in progress are now: ~p",
        [Seq, ThroughSeq, NewThroughSeq, HighestDone,
            NewHighestDone, SeqsInProgress, NewSeqsInProgress]),
    NewState = State#rep_state{
        stats = couch_replicator_utils:sum_stats(Stats, StatsInc),
        current_through_seq = NewThroughSeq,
        seqs_in_progress = NewSeqsInProgress,
        highest_seq_done = NewHighestDone
    },
    update_task(NewState),
    {noreply, NewState}.


handle_cast(checkpoint, State) ->
    case do_checkpoint(State) of
    {ok, NewState} ->
        couch_stats:increment_counter([couch_replicator, checkpoints, success]),
        {noreply, NewState#rep_state{timer = start_timer(State)}};
    Error ->
        couch_stats:increment_counter([couch_replicator, checkpoints, failure]),
        {stop, Error, State}
    end;

handle_cast({report_seq, Seq},
    #rep_state{seqs_in_progress = SeqsInProgress} = State) ->
    NewSeqsInProgress = ordsets:add_element(Seq, SeqsInProgress),
    {noreply, State#rep_state{seqs_in_progress = NewSeqsInProgress}}.


handle_info(shutdown, St) ->
    {stop, shutdown, St};

handle_info({'DOWN', Ref, _, _, Why}, #rep_state{source_monitor = Ref} = St) ->
    couch_log:error("Source database is down. Reason: ~p", [Why]),
    {stop, source_db_down, St};

handle_info({'DOWN', Ref, _, _, Why}, #rep_state{target_monitor = Ref} = St) ->
    couch_log:error("Target database is down. Reason: ~p", [Why]),
    {stop, target_db_down, St};

handle_info({'EXIT', Pid, max_backoff}, State) ->
    couch_log:error("Max backoff reached child process ~p", [Pid]),
    {stop, {shutdown, max_backoff}, State};

handle_info({'EXIT', Pid, {shutdown, max_backoff}}, State) ->
    couch_log:error("Max backoff reached child process ~p", [Pid]),
    {stop, {shutdown, max_backoff}, State};

handle_info({'EXIT', Pid, normal}, #rep_state{changes_reader=Pid} = State) ->
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, #rep_state{changes_reader=Pid} = State) ->
    couch_stats:increment_counter([couch_replicator, changes_reader_deaths]),
    couch_log:error("ChangesReader process died with reason: ~p", [Reason]),
    {stop, changes_reader_died, cancel_timer(State)};

handle_info({'EXIT', Pid, normal}, #rep_state{changes_manager = Pid} = State) ->
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, #rep_state{changes_manager = Pid} = State) ->
    couch_stats:increment_counter([couch_replicator, changes_manager_deaths]),
    couch_log:error("ChangesManager process died with reason: ~p", [Reason]),
    {stop, changes_manager_died, cancel_timer(State)};

handle_info({'EXIT', Pid, normal}, #rep_state{changes_queue=Pid} = State) ->
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, #rep_state{changes_queue=Pid} = State) ->
    couch_stats:increment_counter([couch_replicator, changes_queue_deaths]),
    couch_log:error("ChangesQueue process died with reason: ~p", [Reason]),
    {stop, changes_queue_died, cancel_timer(State)};

handle_info({'EXIT', Pid, normal}, #rep_state{workers = Workers} = State) ->
    case Workers -- [Pid] of
    Workers ->
        couch_log:error("unknown pid bit the dust ~p ~n",[Pid]),
        {noreply, State#rep_state{workers = Workers}};
        %% not clear why a stop was here before
        %%{stop, {unknown_process_died, Pid, normal}, State};
    [] ->
        catch unlink(State#rep_state.changes_manager),
        catch exit(State#rep_state.changes_manager, kill),
        do_last_checkpoint(State);
    Workers2 ->
        {noreply, State#rep_state{workers = Workers2}}
    end;

handle_info({'EXIT', Pid, Reason}, #rep_state{workers = Workers} = State) ->
    State2 = cancel_timer(State),
    case lists:member(Pid, Workers) of
    false ->
        {stop, {unknown_process_died, Pid, Reason}, State2};
    true ->
        couch_stats:increment_counter([couch_replicator, worker_deaths]),
        couch_log:error("Worker ~p died with reason: ~p", [Pid, Reason]),
        {stop, {worker_died, Pid, Reason}, State2}
    end;

handle_info(timeout, InitArgs) ->
    try do_init(InitArgs) of {ok, State} ->
        {noreply, State}
    catch
        exit:{http_request_failed, _, _, max_backoff} ->
            {stop, {shutdown, max_backoff}, {error, InitArgs}};
        Class:Error ->
            ShutdownReason = {error, replication_start_error(Error)},
            StackTop2 = lists:sublist(erlang:get_stacktrace(), 2),
            % Shutdown state is a hack as it is not really the state of the
            % gen_server (it failed to initialize, so it doesn't have one).
            % Shutdown state is used to pass extra info about why start failed.
            ShutdownState = {error, Class, StackTop2, InitArgs},
            {stop, {shutdown, ShutdownReason}, ShutdownState}
    end.


terminate(normal, #rep_state{rep_details = #rep{id = RepId} = Rep,
    checkpoint_history = CheckpointHistory} = State) ->
    terminate_cleanup(State),
    couch_replicator_notifier:notify({finished, RepId, CheckpointHistory}),
    doc_update_completed(Rep, rep_stats(State));

terminate(shutdown, #rep_state{rep_details = #rep{id = RepId}} = State) ->
    % Replication stopped via _scheduler_sup:terminate_child/1, which can be
    % occur during regular scheduler operation or when job is removed from
    % the scheduler.
    State1 = case do_checkpoint(State) of
        {ok, NewState} ->
            NewState;
        Error ->
            LogMsg = "~p : Failed last checkpoint. Job: ~p Error: ~p",
            couch_log:error(LogMsg, [?MODULE, RepId, Error]),
            State
    end,
    couch_replicator_notifier:notify({stopped, RepId, <<"stopped">>}),
    terminate_cleanup(State1);

terminate({shutdown, max_backoff}, {error, InitArgs}) ->
    #rep{id = {BaseId, Ext} = RepId} = InitArgs,
    couch_stats:increment_counter([couch_replicator, failed_starts]),
    couch_log:warning("Replication `~s` reached max backoff ", [BaseId ++ Ext]),
    couch_replicator_notifier:notify({error, RepId, max_backoff});

terminate({shutdown, {error, Error}}, {error, Class, Stack, InitArgs}) ->
    #rep{
        id = {BaseId, Ext} = RepId,
        source = Source0,
        target = Target0,
        doc_id = DocId,
        db_name = DbName
    } = InitArgs,
    Source = couch_replicator_api_wrap:db_uri(Source0),
    Target = couch_replicator_api_wrap:db_uri(Target0),
    RepIdStr = BaseId ++ Ext,
    Msg = "~p:~p: Replication ~s failed to start ~p -> ~p doc ~p:~p stack:~p",
    couch_log:error(Msg, [Class, Error, RepIdStr, Source, Target, DbName,
        DocId, Stack]),
    couch_stats:increment_counter([couch_replicator, failed_starts]),
    couch_replicator_notifier:notify({error, RepId, Error});

terminate({shutdown, max_backoff}, State) ->
    #rep_state{
        source_name = Source,
        target_name = Target,
        rep_details = #rep{id = {BaseId, Ext} = RepId}
    } = State,
    couch_log:error("Replication `~s` (`~s` -> `~s`) reached max backoff",
        [BaseId ++ Ext, Source, Target]),
    terminate_cleanup(State),
    couch_replicator_notifier:notify({error, RepId, max_backoff});

terminate(Reason, State) ->
#rep_state{
        source_name = Source,
        target_name = Target,
        rep_details = #rep{id = {BaseId, Ext} = RepId}
    } = State,
    couch_log:error("Replication `~s` (`~s` -> `~s`) failed: ~s",
        [BaseId ++ Ext, Source, Target, to_binary(Reason)]),
    terminate_cleanup(State),
    couch_replicator_notifier:notify({error, RepId, Reason}).

terminate_cleanup(State) ->
    update_task(State),
    couch_replicator_api_wrap:db_close(State#rep_state.source),
    couch_replicator_api_wrap:db_close(State#rep_state.target).


code_change(_OldVsn, #rep_state{}=State, _Extra) ->
    {ok, State}.


format_status(_Opt, [_PDict, State]) ->
    #rep_state{
       source = Source,
       target = Target,
       rep_details = RepDetails,
       start_seq = StartSeq,
       source_seq = SourceSeq,
       committed_seq = CommitedSeq,
       current_through_seq = ThroughSeq,
       highest_seq_done = HighestSeqDone,
       session_id = SessionId
    } = state_strip_creds(State),
    #rep{
       id = RepId,
       options = Options,
       doc_id = DocId,
       db_name = DbName
    } = RepDetails,
    [
        {rep_id, RepId},
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


startup_jitter() ->
    Jitter = config:get_integer("replicator", "startup_jitter",
        ?STARTUP_JITTER_DEFAULT),
    couch_rand:uniform(erlang:max(1, Jitter)).


headers_strip_creds([], Acc) ->
    lists:reverse(Acc);
headers_strip_creds([{Key, Value0} | Rest], Acc) ->
    Value = case string:to_lower(Key) of
    "authorization" ->
        "****";
    _ ->
        Value0
    end,
    headers_strip_creds(Rest, [{Key, Value} | Acc]).


httpdb_strip_creds(#httpdb{url = Url, headers = Headers} = HttpDb) ->
    HttpDb#httpdb{
        url = couch_util:url_strip_password(Url),
        headers = headers_strip_creds(Headers, [])
    };
httpdb_strip_creds(LocalDb) ->
    LocalDb.


rep_strip_creds(#rep{source = Source, target = Target} = Rep) ->
    Rep#rep{
        source = httpdb_strip_creds(Source),
        target = httpdb_strip_creds(Target)
    }.


state_strip_creds(#rep_state{rep_details = Rep, source = Source, target = Target} = State) ->
    % #rep_state contains the source and target at the top level and also
    % in the nested #rep_details record
    State#rep_state{
        rep_details = rep_strip_creds(Rep),
        source = httpdb_strip_creds(Source),
        target = httpdb_strip_creds(Target)
    }.


adjust_maxconn(Src = #httpdb{http_connections = 1}, RepId) ->
    Msg = "Adjusting minimum number of HTTP source connections to 2 for ~p",
    couch_log:notice(Msg, [RepId]),
    Src#httpdb{http_connections = 2};
adjust_maxconn(Src, _RepId) ->
    Src.


-spec doc_update_triggered(#rep{}) -> ok.
doc_update_triggered(#rep{db_name = null}) ->
    ok;
doc_update_triggered(#rep{id = RepId, doc_id = DocId} = Rep) ->
    case couch_replicator_doc_processor:update_docs() of
        true ->
            couch_replicator_docs:update_triggered(Rep, RepId);
        false ->
            ok
    end,
    couch_log:notice("Document `~s` triggered replication `~s`",
        [DocId, pp_rep_id(RepId)]),
    ok.


-spec doc_update_completed(#rep{}, list()) -> ok.
doc_update_completed(#rep{db_name = null}, _Stats) ->
    ok;
doc_update_completed(#rep{id = RepId, doc_id = DocId, db_name = DbName,
    start_time = StartTime}, Stats0) ->
    Stats = Stats0 ++ [{start_time, couch_replicator_utils:iso8601(StartTime)}],
    couch_replicator_docs:update_doc_completed(DbName, DocId, Stats),
    couch_log:notice("Replication `~s` completed (triggered by `~s`)",
        [pp_rep_id(RepId), DocId]),
    ok.


do_last_checkpoint(#rep_state{seqs_in_progress = [],
    highest_seq_done = {_Ts, ?LOWEST_SEQ}} = State) ->
    {stop, normal, cancel_timer(State)};
do_last_checkpoint(#rep_state{seqs_in_progress = [],
    highest_seq_done = Seq} = State) ->
    case do_checkpoint(State#rep_state{current_through_seq = Seq}) of
    {ok, NewState} ->
        couch_stats:increment_counter([couch_replicator, checkpoints, success]),
        {stop, normal, cancel_timer(NewState)};
    Error ->
        couch_stats:increment_counter([couch_replicator, checkpoints, failure]),
        {stop, Error, State}
    end.


start_timer(State) ->
    After = State#rep_state.checkpoint_interval,
    case timer:apply_after(After, gen_server, cast, [self(), checkpoint]) of
    {ok, Ref} ->
        Ref;
    Error ->
        couch_log:error("Replicator, error scheduling checkpoint:  ~p", [Error]),
        nil
    end.


cancel_timer(#rep_state{timer = nil} = State) ->
    State;
cancel_timer(#rep_state{timer = Timer} = State) ->
    {ok, cancel} = timer:cancel(Timer),
    State#rep_state{timer = nil}.


init_state(Rep) ->
    #rep{
        id = {BaseId, _Ext},
        source = Src0, target = Tgt,
        options = Options,
        type = Type, view = View,
        start_time = StartTime,
        stats = Stats
    } = Rep,
    % Adjust minimum number of http source connections to 2 to avoid deadlock
    Src = adjust_maxconn(Src0, BaseId),
    {ok, Source} = couch_replicator_api_wrap:db_open(Src),
    {CreateTargetParams} = get_value(create_target_params, Options, {[]}),
    {ok, Target} = couch_replicator_api_wrap:db_open(Tgt,
        get_value(create_target, Options, false), CreateTargetParams),

    {ok, SourceInfo} = couch_replicator_api_wrap:get_db_info(Source),
    {ok, TargetInfo} = couch_replicator_api_wrap:get_db_info(Target),

    [SourceLog, TargetLog] = find_and_migrate_logs([Source, Target], Rep),

    {StartSeq0, History} = compare_replication_logs(SourceLog, TargetLog),
    StartSeq1 = get_value(since_seq, Options, StartSeq0),
    StartSeq = {0, StartSeq1},

    SourceSeq = get_value(<<"update_seq">>, SourceInfo, ?LOWEST_SEQ),

    #doc{body={CheckpointHistory}} = SourceLog,
    State = #rep_state{
        rep_details = Rep,
        source_name = couch_replicator_api_wrap:db_uri(Source),
        target_name = couch_replicator_api_wrap:db_uri(Target),
        source = Source,
        target = Target,
        history = History,
        checkpoint_history = {[{<<"no_changes">>, true}| CheckpointHistory]},
        start_seq = StartSeq,
        current_through_seq = StartSeq,
        committed_seq = StartSeq,
        source_log = SourceLog,
        target_log = TargetLog,
        rep_starttime = StartTime,
        src_starttime = get_value(<<"instance_start_time">>, SourceInfo),
        tgt_starttime = get_value(<<"instance_start_time">>, TargetInfo),
        session_id = couch_uuids:random(),
        source_monitor = db_monitor(Source),
        target_monitor = db_monitor(Target),
        source_seq = SourceSeq,
        use_checkpoints = get_value(use_checkpoints, Options, true),
        checkpoint_interval = get_value(checkpoint_interval, Options,
                                        ?DEFAULT_CHECKPOINT_INTERVAL),
        type = Type,
        view = View,
        stats = Stats
    },
    State#rep_state{timer = start_timer(State)}.


find_and_migrate_logs(DbList, #rep{id = {BaseId, _}} = Rep) ->
    LogId = ?l2b(?LOCAL_DOC_PREFIX ++ BaseId),
    fold_replication_logs(DbList, ?REP_ID_VERSION, LogId, LogId, Rep, []).


fold_replication_logs([], _Vsn, _LogId, _NewId, _Rep, Acc) ->
    lists:reverse(Acc);

fold_replication_logs([Db | Rest] = Dbs, Vsn, LogId, NewId, Rep, Acc) ->
    case couch_replicator_api_wrap:open_doc(Db, LogId, [ejson_body]) of
    {error, <<"not_found">>} when Vsn > 1 ->
        OldRepId = couch_replicator_utils:replication_id(Rep, Vsn - 1),
        fold_replication_logs(Dbs, Vsn - 1,
            ?l2b(?LOCAL_DOC_PREFIX ++ OldRepId), NewId, Rep, Acc);
    {error, <<"not_found">>} ->
        fold_replication_logs(
            Rest, ?REP_ID_VERSION, NewId, NewId, Rep, [#doc{id = NewId} | Acc]);
    {ok, Doc} when LogId =:= NewId ->
        fold_replication_logs(
            Rest, ?REP_ID_VERSION, NewId, NewId, Rep, [Doc | Acc]);
    {ok, Doc} ->
        MigratedLog = #doc{id = NewId, body = Doc#doc.body},
        maybe_save_migrated_log(Rep, Db, MigratedLog, Doc#doc.id),
        fold_replication_logs(
            Rest, ?REP_ID_VERSION, NewId, NewId, Rep, [MigratedLog | Acc])
    end.


maybe_save_migrated_log(Rep, Db, #doc{} = Doc, OldId) ->
    case get_value(use_checkpoints, Rep#rep.options, true) of
        true ->
            update_checkpoint(Db, Doc),
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
            ReportSeq = case lists:last(ChangesOrLastSeqs) of
                {last_seq, Seq} ->
                    {Ts, Seq};
                #doc_info{high_seq = Seq} ->
                    {Ts, Seq}
            end,
            Changes = lists:filter(
                fun(#doc_info{}) ->
                    true;
                ({last_seq, _Seq}) ->
                    false
            end, ChangesOrLastSeqs),
            ok = gen_server:cast(Parent, {report_seq, ReportSeq}),
            From ! {changes, self(), Changes, ReportSeq}
        end,
        changes_manager_loop_open(Parent, ChangesQueue, BatchSize, Ts + 1)
    end.


do_checkpoint(#rep_state{use_checkpoints=false} = State) ->
    NewState = State#rep_state{checkpoint_history = {[{<<"use_checkpoints">>, false}]} },
    {ok, NewState};
do_checkpoint(#rep_state{current_through_seq=Seq, committed_seq=Seq} = State) ->
    update_task(State),
    {ok, State};
do_checkpoint(State) ->
    #rep_state{
        source_name=SourceName,
        target_name=TargetName,
        source = Source,
        target = Target,
        history = OldHistory,
        start_seq = {_, StartSeq},
        current_through_seq = {_Ts, NewSeq} = NewTsSeq,
        source_log = SourceLog,
        target_log = TargetLog,
        rep_starttime = ReplicationStartTime,
        src_starttime = SrcInstanceStartTime,
        tgt_starttime = TgtInstanceStartTime,
        stats = Stats,
        rep_details = #rep{options = Options},
        session_id = SessionId
    } = State,
    case commit_to_both(Source, Target) of
    {source_error, Reason} ->
         {checkpoint_commit_failure,
             <<"Failure on source commit: ", (to_binary(Reason))/binary>>};
    {target_error, Reason} ->
         {checkpoint_commit_failure,
             <<"Failure on target commit: ", (to_binary(Reason))/binary>>};
    {SrcInstanceStartTime, TgtInstanceStartTime} ->
        couch_log:notice("recording a checkpoint for `~s` -> `~s` at source update_seq ~p",
            [SourceName, TargetName, NewSeq]),
        LocalStartTime = calendar:now_to_local_time(ReplicationStartTime),
        StartTime = ?l2b(httpd_util:rfc1123_date(LocalStartTime)),
        EndTime = ?l2b(httpd_util:rfc1123_date()),
        NewHistoryEntry = {[
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
        BaseHistory = [
            {<<"session_id">>, SessionId},
            {<<"source_last_seq">>, NewSeq},
            {<<"replication_id_version">>, ?REP_ID_VERSION}
        ] ++ case get_value(doc_ids, Options) of
        undefined ->
            [];
        _DocIds ->
            % backwards compatibility with the result of a replication by
            % doc IDs in versions 0.11.x and 1.0.x
            % TODO: deprecate (use same history format, simplify code)
            [
                {<<"start_time">>, StartTime},
                {<<"end_time">>, EndTime},
                {<<"docs_read">>, couch_replicator_stats:docs_read(Stats)},
                {<<"docs_written">>, couch_replicator_stats:docs_written(Stats)},
                {<<"doc_write_failures">>, couch_replicator_stats:doc_write_failures(Stats)}
            ]
        end,
        % limit history to 50 entries
        NewRepHistory = {
            BaseHistory ++
            [{<<"history">>, lists:sublist([NewHistoryEntry | OldHistory], 50)}]
        },

        try
            {SrcRevPos, SrcRevId} = update_checkpoint(
                Source, SourceLog#doc{body = NewRepHistory}, source),
            {TgtRevPos, TgtRevId} = update_checkpoint(
                Target, TargetLog#doc{body = NewRepHistory}, target),
            NewState = State#rep_state{
                checkpoint_history = NewRepHistory,
                committed_seq = NewTsSeq,
                source_log = SourceLog#doc{revs={SrcRevPos, [SrcRevId]}},
                target_log = TargetLog#doc{revs={TgtRevPos, [TgtRevId]}}
            },
            update_task(NewState),
            {ok, NewState}
        catch throw:{checkpoint_commit_failure, _} = Failure ->
            Failure
        end;
    {SrcInstanceStartTime, _NewTgtInstanceStartTime} ->
        {checkpoint_commit_failure, <<"Target database out of sync. "
            "Try to increase max_dbs_open at the target's server.">>};
    {_NewSrcInstanceStartTime, TgtInstanceStartTime} ->
        {checkpoint_commit_failure, <<"Source database out of sync. "
            "Try to increase max_dbs_open at the source's server.">>};
    {_NewSrcInstanceStartTime, _NewTgtInstanceStartTime} ->
        {checkpoint_commit_failure, <<"Source and target databases out of "
            "sync. Try to increase max_dbs_open at both servers.">>}
    end.


update_checkpoint(Db, Doc, DbType) ->
    try
        update_checkpoint(Db, Doc)
    catch throw:{checkpoint_commit_failure, Reason} ->
        throw({checkpoint_commit_failure,
            <<"Error updating the ", (to_binary(DbType))/binary,
                " checkpoint document: ", (to_binary(Reason))/binary>>})
    end.


update_checkpoint(Db, #doc{id = LogId, body = LogBody} = Doc) ->
    try
        case couch_replicator_api_wrap:update_doc(Db, Doc, [delay_commit]) of
        {ok, PosRevId} ->
            PosRevId;
        {error, Reason} ->
            throw({checkpoint_commit_failure, Reason})
        end
    catch throw:conflict ->
        case (catch couch_replicator_api_wrap:open_doc(Db, LogId, [ejson_body])) of
        {ok, #doc{body = LogBody, revs = {Pos, [RevId | _]}}} ->
            % This means that we were able to update successfully the
            % checkpoint doc in a previous attempt but we got a connection
            % error (timeout for e.g.) before receiving the success response.
            % Therefore the request was retried and we got a conflict, as the
            % revision we sent is not the current one.
            % We confirm this by verifying the doc body we just got is the same
            % that we have just sent.
            {Pos, RevId};
        _ ->
            throw({checkpoint_commit_failure, conflict})
        end
    end.


commit_to_both(Source, Target) ->
    % commit the src async
    ParentPid = self(),
    SrcCommitPid = spawn_link(
        fun() ->
            Result = (catch couch_replicator_api_wrap:ensure_full_commit(Source)),
            ParentPid ! {self(), Result}
        end),

    % commit tgt sync
    TargetResult = (catch couch_replicator_api_wrap:ensure_full_commit(Target)),

    SourceResult = receive
    {SrcCommitPid, Result} ->
        unlink(SrcCommitPid),
        receive {'EXIT', SrcCommitPid, _} -> ok after 0 -> ok end,
        Result;
    {'EXIT', SrcCommitPid, Reason} ->
        {error, Reason}
    end,
    case TargetResult of
    {ok, TargetStartTime} ->
        case SourceResult of
        {ok, SourceStartTime} ->
            {SourceStartTime, TargetStartTime};
        SourceError ->
            {source_error, SourceError}
        end;
    TargetError ->
        {target_error, TargetError}
    end.


compare_replication_logs(SrcDoc, TgtDoc) ->
    #doc{body={RepRecProps}} = SrcDoc,
    #doc{body={RepRecPropsTgt}} = TgtDoc,
    case get_value(<<"session_id">>, RepRecProps) ==
            get_value(<<"session_id">>, RepRecPropsTgt) of
    true ->
        % if the records have the same session id,
        % then we have a valid replication history
        OldSeqNum = get_value(<<"source_last_seq">>, RepRecProps, ?LOWEST_SEQ),
        OldHistory = get_value(<<"history">>, RepRecProps, []),
        {OldSeqNum, OldHistory};
    false ->
        SourceHistory = get_value(<<"history">>, RepRecProps, []),
        TargetHistory = get_value(<<"history">>, RepRecPropsTgt, []),
        couch_log:notice("Replication records differ. "
                "Scanning histories to find a common ancestor.", []),
        couch_log:debug("Record on source:~p~nRecord on target:~p~n",
                [RepRecProps, RepRecPropsTgt]),
        compare_rep_history(SourceHistory, TargetHistory)
    end.


compare_rep_history(S, T) when S =:= [] orelse T =:= [] ->
    couch_log:notice("no common ancestry -- performing full replication", []),
    {?LOWEST_SEQ, []};
compare_rep_history([{S} | SourceRest], [{T} | TargetRest] = Target) ->
    SourceId = get_value(<<"session_id">>, S),
    case has_session_id(SourceId, Target) of
    true ->
        RecordSeqNum = get_value(<<"recorded_seq">>, S, ?LOWEST_SEQ),
        couch_log:notice("found a common replication record with source_seq ~p",
            [RecordSeqNum]),
        {RecordSeqNum, SourceRest};
    false ->
        TargetId = get_value(<<"session_id">>, T),
        case has_session_id(TargetId, SourceRest) of
        true ->
            RecordSeqNum = get_value(<<"recorded_seq">>, T, ?LOWEST_SEQ),
            couch_log:notice("found a common replication record with source_seq ~p",
                [RecordSeqNum]),
            {RecordSeqNum, TargetRest};
        false ->
            compare_rep_history(SourceRest, TargetRest)
        end
    end.


has_session_id(_SessionId, []) ->
    false;
has_session_id(SessionId, [{Props} | Rest]) ->
    case get_value(<<"session_id">>, Props, nil) of
    SessionId ->
        true;
    _Else ->
        has_session_id(SessionId, Rest)
    end.


db_monitor(#httpdb{}) ->
	nil;
db_monitor(Db) ->
	couch_db:monitor(Db).


get_pending_count(St) ->
    Rep = St#rep_state.rep_details,
    Timeout = get_value(connection_timeout, Rep#rep.options),
    TimeoutMicro = Timeout * 1000,
    case get(pending_count_state) of
        {LastUpdate, PendingCount} ->
            case timer:now_diff(os:timestamp(), LastUpdate) > TimeoutMicro of
                true ->
                    NewPendingCount = get_pending_count_int(St),
                    put(pending_count_state, {os:timestamp(), NewPendingCount}),
                    NewPendingCount;
                false ->
                    PendingCount
            end;
        undefined ->
            NewPendingCount = get_pending_count_int(St),
            put(pending_count_state, {os:timestamp(), NewPendingCount}),
            NewPendingCount
    end.


get_pending_count_int(#rep_state{source = #httpdb{} = Db0}=St) ->
    {_, Seq} = St#rep_state.highest_seq_done,
    Db = Db0#httpdb{retries = 3},
    case (catch couch_replicator_api_wrap:get_pending_count(Db, Seq)) of
    {ok, Pending} ->
        Pending;
    _ ->
        null
    end;
get_pending_count_int(#rep_state{source = Db}=St) ->
    {_, Seq} = St#rep_state.highest_seq_done,
    {ok, Pending} = couch_replicator_api_wrap:get_pending_count(Db, Seq),
    Pending.


update_task(State) ->
    #rep_state{
        current_through_seq = {_, ThroughSeq},
        highest_seq_done = {_, HighestSeq}
    } = State,
    update_scheduler_job_stats(State),
    couch_task_status:update(
        rep_stats(State) ++ [
        {source_seq, HighestSeq},
        {through_seq, ThroughSeq}
    ]).


update_scheduler_job_stats(#rep_state{rep_details = Rep, stats = Stats}) ->
    JobId = Rep#rep.id,
    couch_replicator_scheduler:update_job_stats(JobId, Stats).


rep_stats(State) ->
    #rep_state{
        committed_seq = {_, CommittedSeq},
        stats = Stats
    } = State,
    [
        {revisions_checked, couch_replicator_stats:missing_checked(Stats)},
        {missing_revisions_found, couch_replicator_stats:missing_found(Stats)},
        {docs_read, couch_replicator_stats:docs_read(Stats)},
        {docs_written, couch_replicator_stats:docs_written(Stats)},
        {changes_pending, get_pending_count(State)},
        {doc_write_failures, couch_replicator_stats:doc_write_failures(Stats)},
        {checkpointed_source_seq, CommittedSeq}
    ].


replication_start_error({unauthorized, DbUri}) ->
    {unauthorized, <<"unauthorized to access or create database ", DbUri/binary>>};
replication_start_error({db_not_found, DbUri}) ->
    {db_not_found, <<"could not open ", DbUri/binary>>};
replication_start_error({http_request_failed, _Method, Url0,
        {error, {error, {conn_failed, {error, nxdomain}}}}}) ->
    Url = ?l2b(couch_util:url_strip_password(Url0)),
    {nxdomain, <<"could not resolve ", Url/binary>>};
replication_start_error({http_request_failed, Method0, Url0,
        {error, {code, Code}}}) when is_integer(Code) ->
    Url = ?l2b(couch_util:url_strip_password(Url0)),
    Method = ?l2b(Method0),
    {http_error_code, Code, <<Method/binary, " ", Url/binary>>};
replication_start_error(Error) ->
    Error.


log_replication_start(#rep_state{rep_details = Rep} = RepState) ->
    #rep{
       id = {BaseId, Ext},
       doc_id = DocId,
       db_name = DbName,
       options = Options
    } = Rep,
    Id = BaseId ++ Ext,
    Workers = get_value(worker_processes, Options),
    BatchSize = get_value(worker_batch_size, Options),
    #rep_state{
       source_name = Source,  % credentials already stripped
       target_name = Target,  % credentials already stripped
       session_id = Sid
    } = RepState,
    From = case DbName of
        ShardName when is_binary(ShardName) ->
            io_lib:format("from doc ~s:~s", [mem3:dbname(ShardName), DocId]);
        _ ->
            "from _replicate endpoint"
    end,
    Msg = "Starting replication ~s (~s -> ~s) ~s worker_procesess:~p"
        " worker_batch_size:~p session_id:~s",
    couch_log:notice(Msg, [Id, Source, Target, From, Workers, BatchSize, Sid]).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


replication_start_error_test() ->
    ?assertEqual({unauthorized, <<"unauthorized to access or create database"
        " http://x/y">>}, replication_start_error({unauthorized,
        <<"http://x/y">>})),
    ?assertEqual({db_not_found, <<"could not open http://x/y">>},
        replication_start_error({db_not_found, <<"http://x/y">>})),
    ?assertEqual({nxdomain,<<"could not resolve http://x/y">>},
        replication_start_error({http_request_failed, "GET", "http://x/y",
        {error, {error, {conn_failed, {error, nxdomain}}}}})),
    ?assertEqual({http_error_code,503,<<"GET http://x/y">>},
        replication_start_error({http_request_failed, "GET", "http://x/y",
        {error, {code, 503}}})).


scheduler_job_format_status_test() ->
    Source = <<"http://u:p@h1/d1">>,
    Target = <<"http://u:p@h2/d2">>,
    Rep = #rep{
        id = {"base", "+ext"},
        source = couch_replicator_docs:parse_rep_db(Source, [], []),
        target = couch_replicator_docs:parse_rep_db(Target, [], []),
        options = [{create_target, true}],
        doc_id = <<"mydoc">>,
        db_name = <<"mydb">>
    },
    State = #rep_state{
        rep_details = Rep,
        source = Rep#rep.source,
        target = Rep#rep.target,
        session_id = <<"a">>,
        start_seq = <<"1">>,
        source_seq = <<"2">>,
        committed_seq = <<"3">>,
        current_through_seq = <<"4">>,
        highest_seq_done = <<"5">>
    },
    Format = format_status(opts_ignored, [pdict, State]),
    ?assertEqual("http://u:*****@h1/d1/", proplists:get_value(source, Format)),
    ?assertEqual("http://u:*****@h2/d2/", proplists:get_value(target, Format)),
    ?assertEqual({"base", "+ext"}, proplists:get_value(rep_id, Format)),
    ?assertEqual([{create_target, true}], proplists:get_value(options, Format)),
    ?assertEqual(<<"mydoc">>, proplists:get_value(doc_id, Format)),
    ?assertEqual(<<"mydb">>, proplists:get_value(db_name, Format)),
    ?assertEqual(<<"a">>, proplists:get_value(session_id, Format)),
    ?assertEqual(<<"1">>, proplists:get_value(start_seq, Format)),
    ?assertEqual(<<"2">>, proplists:get_value(source_seq, Format)),
    ?assertEqual(<<"3">>, proplists:get_value(committed_seq, Format)),
    ?assertEqual(<<"4">>, proplists:get_value(current_through_seq, Format)),
    ?assertEqual(<<"5">>, proplists:get_value(highest_seq_done, Format)).


-endif.
