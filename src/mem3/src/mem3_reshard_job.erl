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

-module(mem3_reshard_job).

-export([
    start_link/1,

    checkpoint_done/1,
    jobfmt/1,
    pickfun/3
]).

-export([
    init/1,

    initial_copy/1,
    initial_copy_impl/1,

    topoff/1,
    topoff_impl/1,

    build_indices/1,

    copy_local_docs/1,
    copy_local_docs_impl/1,

    update_shardmap/1,

    wait_source_close/1,
    wait_source_close_impl/1,

    source_delete/1,
    source_delete_impl/1,

    completed/1
]).

-include_lib("couch/include/couch_db.hrl").
-include("mem3_reshard.hrl").

% Batch size for internal replication topoffs
-define(INTERNAL_REP_BATCH_SIZE, 2000).

% The list of possible job states. The order of this
% list is important as a job will progress linearly
% through it. However, when starting a job we may
% have to resume from an earlier state as listed
% below in STATE_RESTART.
-define(SPLIT_STATES, [
    new,
    initial_copy,
    topoff1,
    build_indices,
    topoff2,
    copy_local_docs,
    update_shardmap,
    wait_source_close,
    topoff3,
    source_delete,
    completed
]).

% When a job starts it may be resuming from a partially
% completed state. These state pairs list the state
% we have to restart from for each possible state.
-define(STATE_RESTART, #{
    new => initial_copy,
    initial_copy => initial_copy,
    topoff1 => topoff1,
    build_indices => topoff1,
    topoff2 => topoff1,
    copy_local_docs => topoff1,
    update_shardmap => update_shardmap,
    wait_source_close => wait_source_close,
    topoff3 => wait_source_close,
    source_delete => wait_source_close,
    completed => completed
}).

% If we have a worker failing during any of these
% states we need to clean up the targets
-define(CLEAN_TARGET_STATES, [
    initial_copy,
    topoff1,
    build_indices,
    topoff2,
    copy_local_docs
]).

start_link(#job{} = Job) ->
    proc_lib:start_link(?MODULE, init, [Job]).

% This is called by the main proces after it has checkpointed the progress
% of the job. After the new state is checkpointed, we signal the job to start
% executing that state.
checkpoint_done(#job{pid = Pid} = Job) ->
    couch_log:notice(" ~p : checkpoint done for ~p", [?MODULE, jobfmt(Job)]),
    Pid ! checkpoint_done,
    ok.

% Formatting function, used for logging mostly
jobfmt(#job{} = Job) ->
    #job{
        id = Id,
        source = #shard{name = Source},
        target = Target,
        split_state = State,
        job_state = JobState,
        pid = Pid
    } = Job,
    TargetCount = length(Target),
    Msg = "#job{~s ~s /~B job_state:~s split_state:~s pid:~p}",
    Fmt = io_lib:format(Msg, [Id, Source, TargetCount, JobState, State, Pid]),
    lists:flatten(Fmt).

% This is the function which picks between various targets. It is used here as
% well as in mem3_rep internal replicator and couch_db_split bulk copy logic.
% Given a document id and list of ranges, and a hash function, it will pick one
% of the range or return not_in_range atom.
pickfun(DocId, [[B, E] | _] = Ranges, {_M, _F, _A} = HashFun) when
    is_integer(B), is_integer(E), B =< E
->
    HashKey = mem3_hash:calculate(HashFun, DocId),
    Pred = fun([Begin, End]) ->
        Begin =< HashKey andalso HashKey =< End
    end,
    case lists:filter(Pred, Ranges) of
        [] -> not_in_range;
        [Key] -> Key
    end.

init(#job{} = Job0) ->
    process_flag(trap_exit, true),
    Job1 = set_start_state(Job0#job{
        pid = self(),
        start_time = mem3_reshard:now_sec(),
        workers = [],
        retries = 0
    }),
    Job2 = update_split_history(Job1),
    proc_lib:init_ack({ok, self()}),
    couch_log:notice("~p starting job ~s", [?MODULE, jobfmt(Job2)]),
    ok = checkpoint(Job2),
    run(Job2).

run(#job{split_state = CurrState} = Job) ->
    StateFun =
        case CurrState of
            topoff1 -> topoff;
            topoff2 -> topoff;
            topoff3 -> topoff;
            _ -> CurrState
        end,
    NewJob =
        try
            Job1 = ?MODULE:StateFun(Job),
            Job2 = wait_for_workers(Job1),
            Job3 = switch_to_next_state(Job2),
            ok = checkpoint(Job3),
            Job3
        catch
            throw:{retry, RetryJob} ->
                RetryJob
        end,
    run(NewJob).

set_start_state(#job{split_state = State} = Job) ->
    case maps:get(State, ?STATE_RESTART, undefined) of
        undefined ->
            Fmt1 = "~p recover : unknown state ~s",
            couch_log:error(Fmt1, [?MODULE, jobfmt(Job)]),
            erlang:error({invalid_split_job_recover_state, Job});
        StartState ->
            Job#job{split_state = StartState}
    end.

get_next_state(#job{split_state = State}) ->
    get_next_state(State, ?SPLIT_STATES).

get_next_state(completed, _) ->
    completed;
get_next_state(CurrState, [CurrState, NextState | _]) ->
    NextState;
get_next_state(CurrState, [_ | Rest]) ->
    get_next_state(CurrState, Rest).

switch_to_next_state(#job{} = Job0) ->
    Info0 = Job0#job.state_info,
    Info1 = info_delete(error, Info0),
    Info2 = info_delete(reason, Info1),
    Job1 = Job0#job{
        split_state = get_next_state(Job0),
        update_time = mem3_reshard:now_sec(),
        retries = 0,
        state_info = Info2,
        workers = []
    },
    Job2 = update_split_history(Job1),
    check_state(Job2).

checkpoint(Job) ->
    % Ask main process to checkpoint. When it has finished it will notify us
    % by calling by checkpoint_done/1. The reason not to call the main process
    % via a gen_server:call is because the main process could be in the middle
    % of terminating the job and then it would deadlock (after sending us a
    % shutdown message) and it would end up using the whole supervisor
    % termination timeout before finally.
    ok = mem3_reshard:checkpoint(Job#job.manager, Job),
    Parent = parent(),
    receive
        {'EXIT', Parent, Reason} ->
            handle_exit(Job, Reason);
        checkpoint_done ->
            ok;
        Other ->
            handle_unknown_msg(Job, "checkpoint", Other)
    end.

wait_for_workers(#job{workers = []} = Job) ->
    Job;
wait_for_workers(#job{workers = Workers} = Job) ->
    Parent = parent(),
    receive
        {'EXIT', Parent, Reason} ->
            handle_exit(Job, Reason);
        {'EXIT', Pid, Reason} ->
            case lists:member(Pid, Workers) of
                true ->
                    NewJob = handle_worker_exit(Job, Pid, Reason),
                    wait_for_workers(NewJob);
                false ->
                    handle_unknown_msg(Job, "wait_for_workers", {Pid, Reason})
            end;
        Other ->
            handle_unknown_msg(Job, "wait_for_workers", Other)
    end.

handle_worker_exit(#job{workers = Workers} = Job, Pid, normal) ->
    Job#job{workers = Workers -- [Pid]};
handle_worker_exit(#job{} = Job, _Pid, {error, missing_source}) ->
    Msg1 = "~p stopping worker due to source missing ~p",
    couch_log:error(Msg1, [?MODULE, jobfmt(Job)]),
    kill_workers(Job),
    case lists:member(Job#job.split_state, ?CLEAN_TARGET_STATES) of
        true ->
            Msg2 = "~p cleaning target after db was deleted ~p",
            couch_log:error(Msg2, [?MODULE, jobfmt(Job)]),
            reset_target(Job),
            exit({error, missing_source});
        false ->
            exit({error, missing_source})
    end;
handle_worker_exit(#job{} = Job, _Pid, {error, missing_target}) ->
    Msg = "~p stopping worker due to target db missing ~p",
    couch_log:error(Msg, [?MODULE, jobfmt(Job)]),
    kill_workers(Job),
    exit({error, missing_target});
handle_worker_exit(#job{} = Job0, _Pid, Reason) ->
    couch_log:error("~p worker error ~p ~p", [?MODULE, jobfmt(Job0), Reason]),
    kill_workers(Job0),
    Job1 = Job0#job{workers = []},
    case Job1#job.retries =< max_retries() of
        true ->
            retry_state(Job1, Reason);
        false ->
            exit(Reason)
    end.

% Cleanup and exit when we receive an 'EXIT' message from our parent. In case
% the shard map is being updated, try to wait some time for it to finish.
handle_exit(
    #job{split_state = update_shardmap, workers = [WPid]} = Job,
    Reason
) ->
    Timeout = update_shard_map_timeout_sec(),
    Msg1 = "~p job exit ~s ~p  while shard map is updating, waiting ~p sec",
    couch_log:warning(Msg1, [?MODULE, jobfmt(Job), Reason, Timeout]),
    receive
        {'EXIT', WPid, normal} ->
            Msg2 = "~p ~s shard map finished updating successfully, exiting",
            couch_log:notice(Msg2, [?MODULE, jobfmt(Job)]),
            exit(Reason);
        {'EXIT', WPid, Error} ->
            Msg3 = "~p ~s shard map update failed with error ~p",
            couch_log:error(Msg3, [?MODULE, jobfmt(Job), Error]),
            exit(Reason)
    after Timeout * 1000 ->
        Msg4 = "~p ~s shard map update timeout exceeded ~p sec",
        couch_log:error(Msg4, [?MODULE, jobfmt(Job), Timeout]),
        kill_workers(Job),
        exit(Reason)
    end;
handle_exit(#job{} = Job, Reason) ->
    kill_workers(Job),
    exit(Reason).

retry_state(#job{retries = Retries, state_info = Info} = Job0, Error) ->
    Job1 = Job0#job{
        retries = Retries + 1,
        state_info = info_update(error, Error, Info)
    },
    couch_log:notice("~p retrying ~p ~p", [?MODULE, jobfmt(Job1), Retries]),
    Job2 = report(Job1),
    Timeout = retry_interval_sec(),
    Parent = parent(),
    receive
        {'EXIT', Parent, Reason} ->
            handle_exit(Job2, Reason);
        Other ->
            handle_unknown_msg(Job2, "retry_state", Other)
    after Timeout * 1000 ->
        ok
    end,
    throw({retry, Job2}).

report(#job{manager = ManagerPid} = Job) ->
    Job1 = Job#job{update_time = mem3_reshard:now_sec()},
    ok = mem3_reshard:report(ManagerPid, Job1),
    Job1.

kill_workers(#job{workers = Workers}) ->
    lists:foreach(
        fun(Worker) ->
            unlink(Worker),
            exit(Worker, kill)
        end,
        Workers
    ),
    flush_worker_messages().

flush_worker_messages() ->
    Parent = parent(),
    receive
        {'EXIT', Pid, _} when Pid =/= Parent ->
            flush_worker_messages()
    after 0 ->
        ok
    end.

parent() ->
    case get('$ancestors') of
        [Pid | _] when is_pid(Pid) -> Pid;
        [Name | _] when is_atom(Name) -> whereis(Name);
        _ -> undefined
    end.

handle_unknown_msg(Job, When, RMsg) ->
    LogMsg = "~p ~s received an unknown message ~p when in ~s",
    couch_log:error(LogMsg, [?MODULE, jobfmt(Job), RMsg, When]),
    erlang:error({invalid_split_job_message, Job#job.id, When, RMsg}).

initial_copy(#job{} = Job) ->
    Pid = spawn_link(?MODULE, initial_copy_impl, [Job]),
    report(Job#job{workers = [Pid]}).

initial_copy_impl(#job{source = Source, target = Targets0} = Job) ->
    #shard{name = SourceName} = Source,
    Targets = [{R, N} || #shard{range = R, name = N} <- Targets0],
    TMap = maps:from_list(Targets),
    LogMsg1 = "~p initial_copy started ~s",
    LogArgs1 = [?MODULE, shardsstr(Source, Targets0)],
    couch_log:notice(LogMsg1, LogArgs1),
    reset_target(Job),
    case couch_db_split:split(SourceName, TMap, fun pickfun/3) of
        {ok, Seq} ->
            LogMsg2 = "~p initial_copy of ~s finished @ seq:~p",
            LogArgs2 = [?MODULE, shardsstr(Source, Targets0), Seq],
            couch_log:notice(LogMsg2, LogArgs2),
            create_artificial_mem3_rep_checkpoints(Job, Seq);
        {error, Error} ->
            LogMsg3 = "~p initial_copy of ~p finished @ ~p",
            LogArgs3 = [?MODULE, shardsstr(Source, Targets0), Error],
            couch_log:notice(LogMsg3, LogArgs3),
            exit({error, Error})
    end.

topoff(#job{} = Job) ->
    Pid = spawn_link(?MODULE, topoff_impl, [Job]),
    report(Job#job{workers = [Pid]}).

topoff_impl(#job{source = #shard{} = Source, target = Targets}) ->
    couch_log:notice("~p topoff ~p", [?MODULE, shardsstr(Source, Targets)]),
    check_source_exists(Source, topoff),
    check_targets_exist(Targets, topoff),
    TMap = maps:from_list([{R, T} || #shard{range = R} = T <- Targets]),
    Opts = [{batch_size, ?INTERNAL_REP_BATCH_SIZE}, {batch_count, all}],
    case mem3_rep:go(Source, TMap, Opts) of
        {ok, Count} ->
            Args = [?MODULE, shardsstr(Source, Targets), Count],
            couch_log:notice("~p topoff done ~s, count: ~p", Args),
            ok;
        {error, Error} ->
            Args = [?MODULE, shardsstr(Source, Targets), Error],
            couch_log:error("~p topoff failed ~s, error: ~p", Args),
            exit({error, Error})
    end.

build_indices(#job{} = Job) ->
    #job{
        source = #shard{name = SourceName} = Source,
        target = Targets,
        retries = Retries,
        state_info = Info
    } = Job,
    check_source_exists(Source, build_indices),
    {ok, DDocs} = mem3_reshard_index:design_docs(SourceName),
    Indices = mem3_reshard_index:target_indices(DDocs, Targets),
    case mem3_reshard_index:spawn_builders(Indices) of
        {ok, []} ->
            % Skip the log spam if this is a no-op
            Job#job{workers = []};
        {ok, Pids} ->
            report(Job#job{workers = Pids});
        {error, Error} ->
            case Job#job.retries =< max_retries() of
                true ->
                    build_indices(Job#job{
                        retries = Retries + 1,
                        state_info = info_update(error, Error, Info)
                    });
                false ->
                    exit(Error)
            end
    end.

copy_local_docs(#job{split_state = copy_local_docs} = Job) ->
    Pid = spawn_link(?MODULE, copy_local_docs_impl, [Job]),
    report(Job#job{workers = [Pid]}).

copy_local_docs_impl(#job{source = Source, target = Targets0}) ->
    #shard{name = SourceName} = Source,
    Targets = [{R, N} || #shard{range = R, name = N} <- Targets0],
    TMap = maps:from_list(Targets),
    LogArg1 = [?MODULE, shardsstr(Source, Targets)],
    couch_log:notice("~p copy local docs start ~s", LogArg1),
    case couch_db_split:copy_local_docs(SourceName, TMap, fun pickfun/3) of
        ok ->
            couch_log:notice("~p copy local docs finished for ~s", LogArg1),
            ok;
        {error, Error} ->
            LogArg2 = [?MODULE, shardsstr(Source, Targets), Error],
            couch_log:error("~p copy local docs failed for ~s ~p", LogArg2),
            exit({error, Error})
    end.

update_shardmap(#job{} = Job) ->
    Pid = spawn_link(mem3_reshard_dbdoc, update_shard_map, [Job]),
    report(Job#job{workers = [Pid]}).

wait_source_close(#job{source = #shard{name = Name}} = Job) ->
    couch_event:notify(Name, deleted),
    Pid = spawn_link(?MODULE, wait_source_close_impl, [Job]),
    report(Job#job{workers = [Pid]}).

wait_source_close_impl(#job{source = #shard{name = Name}, target = Targets}) ->
    Timeout = config:get_integer("reshard", "source_close_timeout_sec", 600),
    check_targets_exist(Targets, wait_source_close),
    case couch_db:open_int(Name, [?ADMIN_CTX]) of
        {ok, Db} ->
            Now = mem3_reshard:now_sec(),
            case wait_source_close(Db, 1, Now + Timeout) of
                true ->
                    ok;
                false ->
                    exit({error, source_db_close_timeout, Name, Timeout})
            end;
        {not_found, _} ->
            couch_log:warning("~p source already deleted ~p", [?MODULE, Name]),
            ok
    end.

wait_source_close(Db, SleepSec, UntilSec) ->
    case couch_db:monitored_by(Db) -- [self()] of
        [] ->
            true;
        [_ | _] ->
            Now = mem3_reshard:now_sec(),
            case Now < UntilSec of
                true ->
                    LogMsg = "~p : Waiting for source shard ~p to be closed",
                    couch_log:notice(LogMsg, [?MODULE, couch_db:name(Db)]),
                    timer:sleep(SleepSec * 1000),
                    wait_source_close(Db, SleepSec, UntilSec);
                false ->
                    false
            end
    end.

source_delete(#job{} = Job) ->
    Pid = spawn_link(?MODULE, source_delete_impl, [Job]),
    report(Job#job{workers = [Pid]}).

source_delete_impl(#job{source = #shard{name = Name}, target = Targets}) ->
    check_targets_exist(Targets, source_delete),
    case config:get_boolean("mem3_reshard", "delete_source", true) of
        true ->
            case couch_server:delete(Name, [?ADMIN_CTX]) of
                ok ->
                    couch_log:notice(
                        "~p : deleted source shard ~p",
                        [?MODULE, Name]
                    );
                not_found ->
                    couch_log:warning(
                        "~p : source was already deleted ~p",
                        [?MODULE, Name]
                    )
            end;
        false ->
            % Emit deleted event even when not actually deleting the files this
            % is the second one emitted, the other one was before
            % wait_source_close. They should be idempotent. This one is just to
            % match the one that couch_server would emit had the config not
            % been set
            couch_event:notify(Name, deleted),
            LogMsg = "~p : according to configuration not deleting source ~p",
            couch_log:warning(LogMsg, [?MODULE, Name])
    end,
    TNames = [TName || #shard{name = TName} <- Targets],
    lists:foreach(fun(TName) -> couch_event:notify(TName, updated) end, TNames).

completed(#job{} = Job) ->
    couch_log:notice("~p : ~p completed, exit normal", [?MODULE, jobfmt(Job)]),
    exit(normal).

% This is for belt and suspenders really. Call periodically to validate the
% state is one of the expected states.
-spec check_state(#job{}) -> #job{} | no_return().
check_state(#job{split_state = State} = Job) ->
    case lists:member(State, ?SPLIT_STATES) of
        true ->
            Job;
        false ->
            erlang:error({invalid_shard_split_state, State, Job})
    end.

create_artificial_mem3_rep_checkpoints(#job{} = Job, Seq) ->
    #job{source = Source = #shard{name = SourceName}, target = Targets} = Job,
    check_source_exists(Source, initial_copy),
    TNames = [TN || #shard{name = TN} <- Targets],
    Timestamp = list_to_binary(mem3_util:iso8601_timestamp()),
    couch_util:with_db(SourceName, fun(SDb) ->
        [
            couch_util:with_db(TName, fun(TDb) ->
                Doc = mem3_rep_checkpoint_doc(SDb, TDb, Timestamp, Seq),
                {ok, _} = couch_db:update_doc(SDb, Doc, []),
                {ok, _} = couch_db:update_doc(TDb, Doc, []),
                ok
            end)
         || TName <- TNames
        ]
    end),
    ok.

mem3_rep_checkpoint_doc(SourceDb, TargetDb, Timestamp, Seq) ->
    Node = atom_to_binary(node(), utf8),
    SourceUUID = couch_db:get_uuid(SourceDb),
    TargetUUID = couch_db:get_uuid(TargetDb),
    History =
        {[
            {<<"source_node">>, Node},
            {<<"source_uuid">>, SourceUUID},
            {<<"source_seq">>, Seq},
            {<<"timestamp">>, Timestamp},
            {<<"target_node">>, Node},
            {<<"target_uuid">>, TargetUUID},
            {<<"target_seq">>, Seq}
        ]},
    Body =
        {[
            {<<"seq">>, Seq},
            {<<"target_uuid">>, TargetUUID},
            {<<"history">>, {[{Node, [History]}]}}
        ]},
    Id = mem3_rep:make_local_id(SourceUUID, TargetUUID),
    #doc{id = Id, body = Body}.

check_source_exists(#shard{name = Name}, StateName) ->
    case couch_server:exists(Name) of
        true ->
            ok;
        false ->
            ErrMsg = "~p source ~p is unexpectedly missing in ~p",
            couch_log:error(ErrMsg, [?MODULE, Name, StateName]),
            exit({error, missing_source})
    end.

check_targets_exist(Targets, StateName) ->
    lists:foreach(
        fun(#shard{name = Name}) ->
            case couch_server:exists(Name) of
                true ->
                    ok;
                false ->
                    ErrMsg = "~p target ~p is unexpectedly missing in ~p",
                    couch_log:error(ErrMsg, [?MODULE, Name, StateName]),
                    exit({error, missing_target})
            end
        end,
        Targets
    ).

-spec max_retries() -> integer().
max_retries() ->
    config:get_integer("reshard", "max_retries", 1).

-spec retry_interval_sec() -> integer().
retry_interval_sec() ->
    config:get_integer("reshard", "retry_interval_sec", 10).

-spec update_shard_map_timeout_sec() -> integer().
update_shard_map_timeout_sec() ->
    config:get_integer("reshard", "update_shardmap_timeout_sec", 60).

-spec info_update(atom(), any(), [tuple()]) -> [tuple()].
info_update(Key, Val, StateInfo) ->
    lists:keystore(Key, 1, StateInfo, {Key, Val}).

-spec info_delete(atom(), [tuple()]) -> [tuple()].
info_delete(Key, StateInfo) ->
    lists:keydelete(Key, 1, StateInfo).

-spec shardsstr(#shard{}, #shard{} | [#shard{}]) -> string().
shardsstr(#shard{name = SourceName}, #shard{name = TargetName}) ->
    lists:flatten(io_lib:format("~s -> ~s", [SourceName, TargetName]));
shardsstr(#shard{name = SourceName}, Targets) ->
    TNames = [TN || #shard{name = TN} <- Targets],
    TargetsStr = string:join([binary_to_list(T) || T <- TNames], ","),
    lists:flatten(io_lib:format("~s -> ~s", [SourceName, TargetsStr])).

-spec reset_target(#job{}) -> #job{}.
reset_target(#job{source = Source, target = Targets} = Job) ->
    ShardNames =
        try
            [N || #shard{name = N} <- mem3:local_shards(mem3:dbname(Source))]
        catch
            error:database_does_not_exist ->
                []
        end,
    lists:map(
        fun(#shard{name = Name}) ->
            case {couch_server:exists(Name), lists:member(Name, ShardNames)} of
                {_, true} ->
                    % Should never get here but if we do crash and don't continue
                    LogMsg = "~p : ~p target unexpectedly found in shard map ~p",
                    couch_log:error(LogMsg, [?MODULE, jobfmt(Job), Name]),
                    erlang:error({target_present_in_shard_map, Name});
                {true, false} ->
                    LogMsg = "~p : ~p resetting ~p target",
                    couch_log:warning(LogMsg, [?MODULE, jobfmt(Job), Name]),
                    couch_db_split:cleanup_target(Source#shard.name, Name);
                {false, false} ->
                    ok
            end
        end,
        Targets
    ),
    Job.

-spec update_split_history(#job{}) -> #job{}.
update_split_history(#job{split_state = St, update_time = Ts} = Job) ->
    Hist = Job#job.history,
    JobSt =
        case St of
            completed -> completed;
            failed -> failed;
            new -> new;
            stopped -> stopped;
            _ -> running
        end,
    Job#job{history = mem3_reshard:update_history(JobSt, St, Ts, Hist)}.
