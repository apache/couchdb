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

-module(couch_replicator_scheduler).

-behaviour(gen_server).
-behaviour(config_listener).

-export([
    start_link/0
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_info/2,
    handle_cast/2,
    format_status/2
]).

-export([
    add_job/1,
    remove_job/1,
    reschedule/0,
    rep_state/1,
    find_jobs_by_dbname/1,
    find_jobs_by_doc/2,
    job_summary/2,
    health_threshold/0,
    jobs/0,
    job/1,
    restart_job/1,
    update_job_stats/2,
    get_interval_msec/0
]).

%% config_listener callbacks
-export([
    handle_config_change/5,
    handle_config_terminate/3
]).

%% for status updater process to allow hot code loading
-export([
    stats_updater_loop/1
]).

-include("couch_replicator.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").
-include_lib("couch/include/couch_db.hrl").

%% definitions
-define(MAX_BACKOFF_EXPONENT, 10).
-define(BACKOFF_INTERVAL_MICROS, 30 * 1000 * 1000).
-define(DEFAULT_HEALTH_THRESHOLD_SEC, 2 * 60).
-define(RELISTEN_DELAY, 5000).
-define(STATS_UPDATE_WAIT, 5000).

-define(DEFAULT_MAX_JOBS, 500).
-define(DEFAULT_MAX_CHURN, 20).
-define(DEFAULT_MAX_HISTORY, 20).
-define(DEFAULT_SCHEDULER_INTERVAL, 60000).

% Worker children get a default 5 second shutdown timeout, so pick a value just
% a bit less than that: 4.5 seconds. In couch_replicator_sup our scheduler
% worker doesn't specify the timeout, so it up picks ups the OTP default of 5
% seconds https://www.erlang.org/doc/system/sup_princ.html#child-specification
%
-define(TERMINATE_SHUTDOWN_TIME, 4500).

-record(state, {
    interval = ?DEFAULT_SCHEDULER_INTERVAL,
    timer,
    max_jobs,
    max_churn,
    max_history,
    stats_pid
}).

-record(stats_acc, {
    pending_n = 0 :: non_neg_integer(),
    running_n = 0 :: non_neg_integer(),
    crashed_n = 0 :: non_neg_integer()
}).

%% public functions

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add_job(#rep{}) -> ok.
add_job(#rep{} = Rep) when Rep#rep.id /= undefined ->
    case existing_replication(Rep) of
        false ->
            Job = #job{
                id = Rep#rep.id,
                rep = Rep,
                history = [{added, os:timestamp()}]
            },
            gen_server:call(?MODULE, {add_job, Job}, infinity);
        true ->
            ok
    end.

-spec remove_job(job_id()) -> ok.
remove_job(Id) ->
    gen_server:call(?MODULE, {remove_job, Id}, infinity).

-spec reschedule() -> ok.
% Trigger a manual reschedule. Used for testing and/or ops.
reschedule() ->
    gen_server:call(?MODULE, reschedule, infinity).

-spec rep_state(rep_id()) -> #rep{} | nil.
rep_state(RepId) ->
    case (catch ets:lookup_element(?MODULE, RepId, #job.rep)) of
        {'EXIT', {badarg, _}} ->
            nil;
        Rep ->
            Rep
    end.

-spec job_summary(job_id(), non_neg_integer()) -> [_] | nil.
job_summary(JobId, HealthThreshold) ->
    case job_by_id(JobId) of
        {ok, #job{pid = Pid, history = History, rep = Rep}} ->
            ErrorCount = consecutive_crashes(History, HealthThreshold),
            {State, Info} =
                case {Pid, ErrorCount} of
                    {undefined, 0} ->
                        case History of
                            [{{crashed, Error}, _When} | _] ->
                                {crashing, crash_reason_json(Error)};
                            [_ | _] ->
                                {pending, Rep#rep.stats}
                        end;
                    {undefined, ErrorCount} when ErrorCount > 0 ->
                        [{{crashed, Error}, _When} | _] = History,
                        {crashing, crash_reason_json(Error)};
                    {Pid, ErrorCount} when is_pid(Pid) ->
                        {running, Rep#rep.stats}
                end,
            [
                {source, iolist_to_binary(ejson_url(Rep#rep.source))},
                {target, iolist_to_binary(ejson_url(Rep#rep.target))},
                {state, State},
                {info, couch_replicator_utils:ejson_state_info(Info)},
                {error_count, ErrorCount},
                {last_updated, last_updated(History)},
                {start_time, couch_replicator_utils:iso8601(Rep#rep.start_time)},
                {source_proxy, job_proxy_url(Rep#rep.source)},
                {target_proxy, job_proxy_url(Rep#rep.target)}
            ];
        {error, not_found} ->
            % Job might have just completed
            nil
    end.

job_proxy_url(#httpdb{proxy_url = ProxyUrl}) when is_list(ProxyUrl) ->
    list_to_binary(couch_util:url_strip_password(ProxyUrl));
job_proxy_url(_Endpoint) ->
    null.

% Health threshold is the minimum amount of time an unhealthy job should run
% crashing before it is considered to be healthy again. HealtThreashold should
% not be 0 as jobs could start and immediately crash, and it shouldn't be
% infinity, since then  consecutive crashes would accumulate forever even if
% job is back to normal.
-spec health_threshold() -> non_neg_integer().
health_threshold() ->
    config:get_integer(
        "replicator",
        "health_threshold",
        ?DEFAULT_HEALTH_THRESHOLD_SEC
    ).

-spec find_jobs_by_dbname(binary()) -> list(rep_id()).
find_jobs_by_dbname(DbName) ->
    Rep = #rep{db_name = DbName, _ = '_'},
    MatchSpec = #job{id = '$1', rep = Rep, _ = '_'},
    [RepId || [RepId] <- ets:match(?MODULE, MatchSpec)].

-spec find_jobs_by_doc(binary(), binary()) -> list(rep_id()).
find_jobs_by_doc(DbName, DocId) ->
    Rep = #rep{db_name = DbName, doc_id = DocId, _ = '_'},
    MatchSpec = #job{id = '$1', rep = Rep, _ = '_'},
    [RepId || [RepId] <- ets:match(?MODULE, MatchSpec)].

-spec restart_job(binary() | list() | rep_id()) ->
    {ok, {[_]}} | {error, not_found}.
restart_job(JobId) ->
    case rep_state(JobId) of
        nil ->
            {error, not_found};
        #rep{} = Rep ->
            ok = remove_job(JobId),
            ok = add_job(Rep),
            job(JobId)
    end.

-spec update_job_stats(job_id(), term()) -> ok.
update_job_stats(JobId, Stats) ->
    gen_server:cast(?MODULE, {update_job_stats, JobId, Stats}).

get_interval_msec() ->
    config:get_integer("replicator", "interval", ?DEFAULT_SCHEDULER_INTERVAL).

%% gen_server functions

init(_) ->
    process_flag(trap_exit, true),
    config:enable_feature('scheduler'),
    EtsOpts = [
        named_table,
        {keypos, #job.id},
        {read_concurrency, true},
        {write_concurrency, true}
    ],
    ?MODULE = ets:new(?MODULE, EtsOpts),
    ok = couch_replicator_share:init(),
    ok = config:listen_for_changes(?MODULE, nil),
    Interval = get_interval_msec(),
    MaxJobs = config:get_integer("replicator", "max_jobs", ?DEFAULT_MAX_JOBS),
    MaxChurn = config:get_integer(
        "replicator",
        "max_churn",
        ?DEFAULT_MAX_CHURN
    ),
    MaxHistory = config:get_integer(
        "replicator",
        "max_history",
        ?DEFAULT_MAX_HISTORY
    ),
    Timer = erlang:send_after(Interval, self(), reschedule),
    State = #state{
        interval = Interval,
        max_jobs = MaxJobs,
        max_churn = MaxChurn,
        max_history = MaxHistory,
        timer = Timer,
        stats_pid = start_stats_updater()
    },
    {ok, State}.

handle_call({add_job, Job}, _From, State) ->
    ok = maybe_remove_job_int(Job#job.id, State),
    true = add_job_int(Job),
    ok = maybe_start_newly_added_job(Job, State),
    couch_stats:increment_counter([couch_replicator, jobs, adds]),
    TotalJobs = ets:info(?MODULE, size),
    couch_stats:update_gauge([couch_replicator, jobs, total], TotalJobs),
    {reply, ok, State};
handle_call({remove_job, Id}, _From, State) ->
    ok = maybe_remove_job_int(Id, State),
    {reply, ok, State};
handle_call(reschedule, _From, State) ->
    ok = reschedule(State),
    {reply, ok, State};
handle_call(_, _From, State) ->
    {noreply, State}.

handle_cast({set_max_jobs, MaxJobs}, State) when
    is_integer(MaxJobs),
    MaxJobs >= 0
->
    couch_log:notice("~p: max_jobs set to ~B", [?MODULE, MaxJobs]),
    {noreply, State#state{max_jobs = MaxJobs}};
handle_cast({set_max_churn, MaxChurn}, State) when
    is_integer(MaxChurn),
    MaxChurn > 0
->
    couch_log:notice("~p: max_churn set to ~B", [?MODULE, MaxChurn]),
    {noreply, State#state{max_churn = MaxChurn}};
handle_cast({set_max_history, MaxHistory}, State) when
    is_integer(MaxHistory),
    MaxHistory > 0
->
    couch_log:notice("~p: max_history set to ~B", [?MODULE, MaxHistory]),
    {noreply, State#state{max_history = MaxHistory}};
handle_cast({set_interval, Interval}, State) when
    is_integer(Interval),
    Interval > 0
->
    couch_log:notice("~p: interval set to ~B", [?MODULE, Interval]),
    {noreply, State#state{interval = Interval}};
handle_cast({update_shares, Key, Shares}, State) when
    is_binary(Key),
    is_integer(Shares),
    Shares >= 0
->
    couch_log:notice("~p: shares for ~s set to ~B", [?MODULE, Key, Shares]),
    couch_replicator_share:update_shares(Key, Shares),
    {noreply, State};
handle_cast({reset_shares, Key}, State) when is_binary(Key) ->
    couch_log:notice("~p: shares for ~s reset to default", [?MODULE, Key]),
    couch_replicator_share:reset_shares(Key),
    {noreply, State};
handle_cast({update_job_stats, JobId, Stats}, State) ->
    case rep_state(JobId) of
        nil ->
            ok;
        #rep{} = Rep ->
            NewRep = Rep#rep{stats = Stats},
            true = ets:update_element(?MODULE, JobId, {#job.rep, NewRep})
    end,
    {noreply, State};
handle_cast(UnexpectedMsg, State) ->
    couch_log:error("~p: received un-expected cast ~p", [?MODULE, UnexpectedMsg]),
    {noreply, State}.

handle_info(reschedule, State) ->
    ok = reschedule(State),
    erlang:cancel_timer(State#state.timer),
    Timer = erlang:send_after(State#state.interval, self(), reschedule),
    {noreply, State#state{timer = Timer}};
handle_info({'EXIT', Pid, Reason}, #state{stats_pid = Pid} = State) ->
    couch_log:error("~p : stats updater died: ~p", [?MODULE, Reason]),
    {stop, {status_updater_died, Reason}, State};
handle_info({'EXIT', Pid, normal}, State) ->
    {ok, Job} = job_by_pid(Pid),
    couch_log:notice("~p: Job ~p completed normally", [?MODULE, Job#job.id]),
    Interval = State#state.interval,
    couch_replicator_share:charge(Job, Interval, os:timestamp()),
    remove_job_int(Job),
    update_running_jobs_stats(State#state.stats_pid),
    {noreply, State};
handle_info({'EXIT', Pid, Reason0}, State) ->
    {ok, Job} = job_by_pid(Pid),
    Reason =
        case Reason0 of
            {shutdown, ShutdownReason} -> ShutdownReason;
            Other -> Other
        end,
    Interval = State#state.interval,
    couch_replicator_share:charge(Job, Interval, os:timestamp()),
    ok = handle_crashed_job(Job, Reason, State),
    {noreply, State};
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    stop_clear_all_jobs(?TERMINATE_SHUTDOWN_TIME),
    couch_replicator_share:clear(),
    ok.

format_status(_Opt, [_PDict, State]) ->
    [
        {max_jobs, State#state.max_jobs},
        {running_jobs, running_job_count()},
        {pending_jobs, pending_job_count()}
    ].

%% config listener functions

handle_config_change("replicator", "max_jobs", V, _, S) ->
    ok = gen_server:cast(?MODULE, {set_max_jobs, list_to_integer(V)}),
    {ok, S};
handle_config_change("replicator", "max_churn", V, _, S) ->
    ok = gen_server:cast(?MODULE, {set_max_churn, list_to_integer(V)}),
    {ok, S};
handle_config_change("replicator", "interval", V, _, S) ->
    ok = gen_server:cast(?MODULE, {set_interval, list_to_integer(V)}),
    {ok, S};
handle_config_change("replicator", "max_history", V, _, S) ->
    ok = gen_server:cast(?MODULE, {set_max_history, list_to_integer(V)}),
    {ok, S};
handle_config_change("replicator.shares", Key, deleted, _, S) ->
    ok = gen_server:cast(?MODULE, {reset_shares, list_to_binary(Key)}),
    {ok, S};
handle_config_change("replicator.shares", Key, V, _, S) ->
    ok = gen_server:cast(?MODULE, {update_shares, list_to_binary(Key), list_to_integer(V)}),
    {ok, S};
handle_config_change(_, _, _, _, S) ->
    {ok, S}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_, _, _) ->
    Pid = whereis(?MODULE),
    erlang:send_after(?RELISTEN_DELAY, Pid, restart_config_listener).

%% Private functions

% Stop jobs in parallel
%
stop_clear_all_jobs(TimeLeftMSec) ->
    ShutdownFun = fun(#job{pid = Pid}) ->
        Pid ! shutdown,
        erlang:monitor(process, Pid)
    end,
    Refs = lists:map(ShutdownFun, running_jobs()),
    ets:delete_all_objects(?MODULE),
    wait_jobs_stop(TimeLeftMSec, Refs).

wait_jobs_stop(_, []) ->
    ok;
wait_jobs_stop(TimeLeftMSec, _) when TimeLeftMSec =< 0 ->
    % If some survive a bit longer we let them finish checkpointing.
    timeout;
wait_jobs_stop(TimeLeftMsec, [Ref | Refs]) ->
    T0 = erlang:monotonic_time(),
    receive
        {'DOWN', Ref, _, _, _} ->
            Dt = erlang:monotonic_time() - T0,
            DtMSec = erlang:convert_time_unit(Dt, native, millisecond),
            wait_jobs_stop(TimeLeftMsec - DtMSec, Refs)
    after TimeLeftMsec ->
        ok
    end.

% Handle crashed jobs. Handling differs between transient and permanent jobs.
% Transient jobs are those posted to the _replicate endpoint. They don't have a
% db associated with them. When those jobs crash, they are not restarted. That
% is also consistent with behavior when the node they run on, crashed and they
% do not migrate to other nodes. Permanent jobs are those created from
% replicator documents. Those jobs, once they pass basic validation and end up
% in the scheduler will be retried indefinitely (with appropriate exponential
% backoffs).
-spec handle_crashed_job(#job{}, any(), #state{}) -> ok.
handle_crashed_job(#job{rep = #rep{db_name = null}} = Job, Reason, State) ->
    Msg = "~p : Transient job ~p failed, removing. Error: ~p",
    ErrorBinary = couch_replicator_utils:rep_error_to_binary(Reason),
    couch_log:error(Msg, [?MODULE, Job#job.id, ErrorBinary]),
    remove_job_int(Job),
    update_running_jobs_stats(State#state.stats_pid),
    ok;
handle_crashed_job(Job, Reason, State) ->
    ok = update_state_crashed(Job, Reason, State),
    case couch_replicator_doc_processor:update_docs() of
        true ->
            couch_replicator_docs:update_error(Job#job.rep, Reason);
        false ->
            ok
    end,
    case ets:info(?MODULE, size) < State#state.max_jobs of
        true ->
            % Starting pending jobs is an O(TotalJobsCount) operation. Only do
            % it if there is a relatively small number of jobs. Otherwise
            % scheduler could be blocked if there is a cascade of lots failing
            % jobs in a row.
            start_pending_jobs(State),
            update_running_jobs_stats(State#state.stats_pid),
            ok;
        false ->
            ok
    end.

% Attempt to start a newly added job. First quickly check if total jobs
% already exceed max jobs, then do a more expensive check which runs a
% select (an O(n) operation) to check pending jobs specifically.
-spec maybe_start_newly_added_job(#job{}, #state{}) -> ok.
maybe_start_newly_added_job(Job, State) ->
    MaxJobs = State#state.max_jobs,
    TotalJobs = ets:info(?MODULE, size),
    case TotalJobs < MaxJobs andalso running_job_count() < MaxJobs of
        true ->
            start_job_int(Job, State),
            update_running_jobs_stats(State#state.stats_pid),
            ok;
        false ->
            ok
    end.

% Return up to a given number of oldest, not recently crashed jobs. Try to be
% memory efficient and use ets:foldl to accumulate jobs.
-spec pending_jobs(non_neg_integer()) -> [#job{}].
pending_jobs(0) ->
    % Handle this case as user could set max_churn to 0. If this is passed to
    % other function clause it will crash as gb_sets:largest assumes set is not
    % empty.
    [];
pending_jobs(Count) when is_integer(Count), Count > 0 ->
    % [{{Priority, LastStart}, Job},...]
    Set0 = gb_sets:new(),
    Now = os:timestamp(),
    Acc0 = {Set0, Now, Count, health_threshold()},
    {Set1, _, _, _} = ets:foldl(fun pending_fold/2, Acc0, ?MODULE),
    [Job || {_PriorityKey, Job} <- gb_sets:to_list(Set1)].

pending_fold(#job{pid = Pid}, Acc) when is_pid(Pid) ->
    Acc;
pending_fold(Job, {Set, Now, Count, HealthThreshold}) ->
    Healthy = not_recently_crashed(Job, Now, HealthThreshold),
    Set1 =
        case {Healthy, gb_sets:size(Set) >= Count} of
            {true, true} ->
                % Job is healthy but already reached accumulated limit, so might
                % have to replace one of the accumulated jobs
                pending_maybe_replace(Job, Set);
            {true, false} ->
                % Job is healthy and we haven't reached the limit, so add job
                % to accumulator
                gb_sets:add_element({start_priority_key(Job), Job}, Set);
            {false, _} ->
                % This job is not healthy (has crashed too recently), so skip it.
                Set
        end,
    {Set1, Now, Count, HealthThreshold}.

% Replace Job in the accumulator if it has a higher priority (lower priority
% value) than the lowest priority there. Job priority is indexed by
% {FairSharePiority, LastStarted} tuples. If the FairSharePriority is the same
% then last started timestamp is used to pick. The goal is to keep up to Count
% oldest jobs during the iteration. For example, if there are jobs with these
% priorities accumulated so far [5, 7, 11], and the priority of current job is
% 6. Then 6 < 11 is true, so 11 (lower priority) is dropped and 6 is inserted
% resulting in [5, 6, 7]. In the end the result might look like [1, 2, 5], for
% example.
%
pending_maybe_replace(Job, Set) ->
    Key = start_priority_key(Job),
    {LowestPKey, LowestPJob} = gb_sets:largest(Set),
    case Key < LowestPKey of
        true ->
            Set1 = gb_sets:delete({LowestPKey, LowestPJob}, Set),
            gb_sets:add_element({Key, Job}, Set1);
        false ->
            Set
    end.

% Starting priority key is used to order pending jobs such that the ones with a
% lower priority value and start time would sort first, so they would be the
% first to run.
%
start_priority_key(#job{} = Job) ->
    {couch_replicator_share:priority(Job#job.id), last_started(Job)}.

start_jobs(Count, State) ->
    [start_job_int(Job, State) || Job <- pending_jobs(Count)],
    ok.

-spec stop_jobs(non_neg_integer(), boolean(), #state{}) -> non_neg_integer().
stop_jobs(Count, _, _) when is_integer(Count), Count =< 0 ->
    0;
stop_jobs(Count, IsContinuous, State) when is_integer(Count) ->
    Running0 = running_jobs(),
    ContinuousPred = fun(Job) -> is_continuous(Job) =:= IsContinuous end,
    Running1 = lists:filter(ContinuousPred, Running0),
    Running2 = [{stop_priority_key(Job), Job} || Job <- Running1],
    Running3 = lists:sublist(lists:sort(Running2), Count),
    length([stop_job_int(Job, State) || {_SortKey, Job} <- Running3]).

% Lower priority jobs have higher priority values, so we negate them, that way
% when sorted, they'll come up first. If priorities are equal, jobs are sorted
% by the lowest starting times as jobs with lowest start time have been running
% the longest.
%
stop_priority_key(#job{} = Job) ->
    {-couch_replicator_share:priority(Job#job.id), last_started(Job)}.

not_recently_crashed(#job{history = History}, Now, HealthThreshold) ->
    case History of
        [{added, _When}] ->
            true;
        [{stopped, _When} | _] ->
            true;
        _ ->
            LatestCrashT = latest_crash_timestamp(History),
            CrashCount = consecutive_crashes(History, HealthThreshold),
            timer:now_diff(Now, LatestCrashT) >= backoff_micros(CrashCount)
    end.

% Count consecutive crashes. A crash happens when there is a `crashed` event
% within a short period of time (configurable) after any other event. It could
% be `crashed, started` for jobs crashing quickly after starting, `crashed,
% crashed`, `crashed, stopped` if job repeatedly failed to start
% being stopped. Or it could be `crashed, added` if it crashed immediately after
% being added during start.
%
% A streak of "consecutive crashes" ends when a crashed event is seen starting
% and running successfully without crashing for a period of time. That period
% of time is the HealthThreshold.
%

-spec consecutive_crashes(history(), non_neg_integer()) -> non_neg_integer().
consecutive_crashes(History, HealthThreshold) when is_list(History) ->
    consecutive_crashes(History, HealthThreshold, 0).

-spec consecutive_crashes(history(), non_neg_integer(), non_neg_integer()) ->
    non_neg_integer().
consecutive_crashes([], _HealthThreashold, Count) ->
    Count;
consecutive_crashes(
    [{{crashed, _}, CrashT}, {_, PrevT} = PrevEvent | Rest],
    HealthThreshold,
    Count
) ->
    case timer:now_diff(CrashT, PrevT) > HealthThreshold * 1000000 of
        true ->
            Count;
        false ->
            consecutive_crashes([PrevEvent | Rest], HealthThreshold, Count + 1)
    end;
consecutive_crashes(
    [{stopped, _}, {started, _} | _],
    _HealthThreshold,
    Count
) ->
    Count;
consecutive_crashes([_ | Rest], HealthThreshold, Count) ->
    consecutive_crashes(Rest, HealthThreshold, Count).

-spec latest_crash_timestamp(history()) -> erlang:timestamp().
latest_crash_timestamp([]) ->
    % Used to avoid special-casing "no crash" when doing now_diff
    {0, 0, 0};
latest_crash_timestamp([{{crashed, _Reason}, When} | _]) ->
    When;
latest_crash_timestamp([_Event | Rest]) ->
    latest_crash_timestamp(Rest).

-spec backoff_micros(non_neg_integer()) -> non_neg_integer().
backoff_micros(CrashCount) ->
    % When calculating the backoff interval treat consecutive crash count as the
    % exponent in Base * 2 ^ CrashCount to achieve an exponential backoff
    % doubling every consecutive failure, starting with the base value of
    % ?BACKOFF_INTERVAL_MICROS.
    BackoffExp = erlang:min(CrashCount - 1, ?MAX_BACKOFF_EXPONENT),
    (1 bsl BackoffExp) * ?BACKOFF_INTERVAL_MICROS.

-spec add_job_int(#job{}) -> boolean().
add_job_int(#job{} = Job) ->
    couch_replicator_share:job_added(Job),
    ets:insert_new(?MODULE, Job).

-spec maybe_remove_job_int(job_id(), #state{}) -> ok.
maybe_remove_job_int(JobId, State) ->
    case job_by_id(JobId) of
        {ok, Job} ->
            Now = os:timestamp(),
            Interval = State#state.interval,
            couch_replicator_share:charge(Job, Interval, Now),
            ok = stop_job_int(Job, State),
            true = remove_job_int(Job),
            couch_stats:increment_counter([couch_replicator, jobs, removes]),
            TotalJobs = ets:info(?MODULE, size),
            couch_stats:update_gauge(
                [couch_replicator, jobs, total],
                TotalJobs
            ),
            update_running_jobs_stats(State#state.stats_pid),
            ok;
        {error, not_found} ->
            ok
    end.

start_job_int(#job{pid = Pid}, _State) when Pid /= undefined ->
    ok;
start_job_int(#job{} = Job0, State) ->
    Job = maybe_optimize_job_for_rate_limiting(Job0),
    case couch_replicator_scheduler_job:start_link(Job#job.rep) of
        {ok, Child} ->
            ok = update_state_started(Job, Child, State),
            couch_log:notice(
                "~p: Job ~p started as ~p",
                [?MODULE, Job#job.id, Child]
            );
        {error, {already_started, OtherPid}} ->
            Node = node(OtherPid),
            CrashMsg = "Duplicate replication running on " ++ atom_to_list(Node),
            LogMsg = "~p: Job ~p already running as ~p on node ~s",
            couch_log:warning(LogMsg, [?MODULE, Job#job.id, OtherPid, Node]),
            ok = update_state_crashed(Job, CrashMsg, State);
        {error, Reason} ->
            couch_log:notice(
                "~p: Job ~p failed to start for reason ~p",
                [?MODULE, Job, Reason]
            ),
            ok = update_state_crashed(Job, Reason, State)
    end.

-spec stop_job_int(#job{}, #state{}) -> ok | {error, term()}.
stop_job_int(#job{pid = undefined}, _State) ->
    ok;
stop_job_int(#job{pid = Pid} = Job, State) when is_pid(Pid) ->
    ok = couch_replicator_scheduler_job:stop(Pid),
    ok = update_state_stopped(Job, State),
    couch_log:notice("~p: Job ~p stopped as ~p", [?MODULE, Job#job.id, Pid]).

-spec remove_job_int(#job{}) -> true.
remove_job_int(#job{} = Job) ->
    couch_replicator_share:job_removed(Job),
    ets:delete(?MODULE, Job#job.id).

-spec running_job_count() -> non_neg_integer().
running_job_count() ->
    ets:info(?MODULE, size) - pending_job_count().

-spec running_jobs() -> [#job{}].
running_jobs() ->
    ets:select(?MODULE, [{#job{pid = '$1', _ = '_'}, [{is_pid, '$1'}], ['$_']}]).

-spec pending_job_count() -> non_neg_integer().
pending_job_count() ->
    ets:select_count(?MODULE, [{#job{pid = undefined, _ = '_'}, [], [true]}]).

-spec job_by_pid(pid()) -> {ok, #job{}} | {error, not_found}.
job_by_pid(Pid) when is_pid(Pid) ->
    case ets:match_object(?MODULE, #job{pid = Pid, _ = '_'}) of
        [] ->
            {error, not_found};
        [#job{} = Job] ->
            {ok, Job}
    end.

-spec job_by_id(job_id()) -> {ok, #job{}} | {error, not_found}.
job_by_id(Id) ->
    case ets:lookup(?MODULE, Id) of
        [] ->
            {error, not_found};
        [#job{} = Job] ->
            {ok, Job}
    end.

-spec update_state_stopped(#job{}, #state{}) -> ok.
update_state_stopped(Job, State) ->
    Job1 = Job#job{pid = undefined},
    Job2 = update_history(Job1, stopped, os:timestamp(), State),
    true = ets:insert(?MODULE, Job2),
    couch_stats:increment_counter([couch_replicator, jobs, stops]),
    ok.

-spec update_state_started(#job{}, pid(), #state{}) -> ok.
update_state_started(Job, Pid, State) when is_pid(Pid) ->
    Job1 = Job#job{pid = Pid},
    Job2 = update_history(Job1, started, os:timestamp(), State),
    true = ets:insert(?MODULE, Job2),
    couch_stats:increment_counter([couch_replicator, jobs, starts]),
    ok.

-spec update_state_crashed(#job{}, any(), #state{}) -> ok.
update_state_crashed(Job, Reason, State) ->
    Job1 = Job#job{pid = undefined},
    Job2 = update_history(Job1, {crashed, Reason}, os:timestamp(), State),
    true = ets:insert(?MODULE, Job2),
    couch_stats:increment_counter([couch_replicator, jobs, crashes]),
    ok.

-spec reschedule(#state{}) -> ok.
reschedule(#state{interval = Interval} = State) ->
    couch_replicator_share:update(running_jobs(), Interval, os:timestamp()),
    StopCount = stop_excess_jobs(State, running_job_count()),
    rotate_jobs(State, StopCount),
    update_running_jobs_stats(State#state.stats_pid).

-spec stop_excess_jobs(#state{}, non_neg_integer()) -> non_neg_integer().
stop_excess_jobs(State, Running) ->
    #state{max_jobs = MaxJobs} = State,
    StopCount = max(0, Running - MaxJobs),
    Stopped = stop_jobs(StopCount, true, State),
    OneshotLeft = StopCount - Stopped,
    stop_jobs(OneshotLeft, false, State),
    StopCount.

start_pending_jobs(State) ->
    #state{max_jobs = MaxJobs} = State,
    Running = running_job_count(),
    Pending = pending_job_count(),
    if
        Running < MaxJobs, Pending > 0 ->
            start_jobs(MaxJobs - Running, State);
        true ->
            ok
    end.

-spec rotate_jobs(#state{}, non_neg_integer()) -> ok.
rotate_jobs(State, ChurnSoFar) ->
    #state{max_jobs = MaxJobs, max_churn = MaxChurn} = State,
    Running = running_job_count(),
    Pending = pending_job_count(),
    % Reduce MaxChurn by the number of already stopped jobs in the
    % current rescheduling cycle.
    Churn = max(0, MaxChurn - ChurnSoFar),
    SlotsAvailable = MaxJobs - Running,
    if
        SlotsAvailable >= 0 ->
            % If there is are enough SlotsAvailable reduce StopCount to avoid
            % unnesessarily stopping jobs. `stop_jobs/3` ignores 0 or negative
            % values so we don't worry about that here.
            StopCount = lists:min([Pending - SlotsAvailable, Running, Churn]),
            stop_jobs(StopCount, true, State),
            StartCount = max(0, MaxJobs - running_job_count()),
            start_jobs(StartCount, State);
        true ->
            ok
    end.

-spec last_started(#job{}) -> erlang:timestamp().
last_started(#job{} = Job) ->
    case lists:keyfind(started, 1, Job#job.history) of
        false ->
            {0, 0, 0};
        {started, When} ->
            When
    end.

-spec update_history(#job{}, event_type(), erlang:timestamp(), #state{}) ->
    #job{}.
update_history(Job, Type, When, State) ->
    History0 = [{Type, When} | Job#job.history],
    History1 = lists:sublist(History0, State#state.max_history),
    Job#job{history = History1}.

-spec ejson_url(#httpdb{} | binary()) -> binary().
ejson_url(#httpdb{} = Httpdb) ->
    couch_util:url_strip_password(Httpdb#httpdb.url);
ejson_url(DbName) when is_binary(DbName) ->
    DbName.

-spec job_ejson(#job{}) -> {[_ | _]} | no_return().
job_ejson(Job) ->
    Rep = Job#job.rep,
    Source = ejson_url(Rep#rep.source),
    Target = ejson_url(Rep#rep.target),
    History = lists:map(
        fun({Type, When}) ->
            EventProps =
                case Type of
                    {crashed, Reason} ->
                        [{type, crashed}, {reason, crash_reason_json(Reason)}];
                    Type ->
                        [{type, Type}]
                end,
            {[{timestamp, couch_replicator_utils:iso8601(When)} | EventProps]}
        end,
        Job#job.history
    ),
    {BaseID, Ext} = Job#job.id,
    Pid =
        case Job#job.pid of
            undefined ->
                null;
            P when is_pid(P) ->
                ?l2b(pid_to_list(P))
        end,
    {[
        {id, iolist_to_binary([BaseID, Ext])},
        {pid, Pid},
        {source, iolist_to_binary(Source)},
        {target, iolist_to_binary(Target)},
        {database, Rep#rep.db_name},
        {user, (Rep#rep.user_ctx)#user_ctx.name},
        {doc_id, Rep#rep.doc_id},
        {info, couch_replicator_utils:ejson_state_info(Rep#rep.stats)},
        {history, History},
        {node, node()},
        {start_time, couch_replicator_utils:iso8601(Rep#rep.start_time)}
    ]}.

-spec jobs() -> [[tuple()]].
jobs() ->
    ets:foldl(fun(Job, Acc) -> [job_ejson(Job) | Acc] end, [], ?MODULE).

-spec job(job_id()) -> {ok, {[_ | _]}} | {error, not_found}.
job(JobId) ->
    case job_by_id(JobId) of
        {ok, Job} ->
            {ok, job_ejson(Job)};
        Error ->
            Error
    end.

crash_reason_json({_CrashType, Info}) when is_binary(Info) ->
    Info;
crash_reason_json(Reason) when is_binary(Reason) ->
    Reason;
crash_reason_json(Error) ->
    couch_replicator_utils:rep_error_to_binary(Error).

-spec last_updated([_]) -> binary().
last_updated([{_Type, When} | _]) ->
    couch_replicator_utils:iso8601(When).

-spec is_continuous(#job{}) -> boolean().
is_continuous(#job{rep = Rep}) ->
    couch_util:get_value(continuous, Rep#rep.options, false).

% If job crashed last time because it was rate limited, try to
% optimize some options to help the job make progress.
-spec maybe_optimize_job_for_rate_limiting(#job{}) -> #job{}.
maybe_optimize_job_for_rate_limiting(
    Job = #job{
        history =
            [{{crashed, max_backoff}, _} | _]
    }
) ->
    Opts = [
        {checkpoint_interval, 5000},
        {worker_processes, 2},
        {worker_batch_size, 100},
        {http_connections, 5}
    ],
    Rep = lists:foldl(fun optimize_int_option/2, Job#job.rep, Opts),
    Job#job{rep = Rep};
maybe_optimize_job_for_rate_limiting(Job) ->
    Job.

-spec optimize_int_option({atom(), any()}, #rep{}) -> #rep{}.
optimize_int_option({Key, Val}, #rep{options = Options} = Rep) ->
    case couch_util:get_value(Key, Options) of
        CurVal when is_integer(CurVal), CurVal > Val ->
            Msg = "~p replication ~p : setting ~p = ~p due to rate limiting",
            couch_log:warning(Msg, [?MODULE, Rep#rep.id, Key, Val]),
            Options1 = lists:keyreplace(Key, 1, Options, {Key, Val}),
            Rep#rep{options = Options1};
        _ ->
            Rep
    end.

% Updater is a separate process. It receives `update_stats` messages and
% updates scheduler stats from the scheduler jobs table. Updates are
% performed no more frequently than once per ?STATS_UPDATE_WAIT milliseconds.

update_running_jobs_stats(StatsPid) when is_pid(StatsPid) ->
    StatsPid ! update_stats,
    ok.

start_stats_updater() ->
    erlang:spawn_link(?MODULE, stats_updater_loop, [undefined]).

stats_updater_loop(Timer) ->
    receive
        update_stats when Timer == undefined ->
            TRef = erlang:send_after(?STATS_UPDATE_WAIT, self(), refresh_stats),
            ?MODULE:stats_updater_loop(TRef);
        update_stats when is_reference(Timer) ->
            ?MODULE:stats_updater_loop(Timer);
        refresh_stats ->
            ok = stats_updater_refresh(),
            ?MODULE:stats_updater_loop(undefined);
        Else ->
            erlang:exit({stats_updater_bad_msg, Else})
    end.

-spec stats_updater_refresh() -> ok.
stats_updater_refresh() ->
    #stats_acc{
        pending_n = PendingN,
        running_n = RunningN,
        crashed_n = CrashedN
    } = ets:foldl(fun stats_fold/2, #stats_acc{}, ?MODULE),
    couch_stats:update_gauge([couch_replicator, jobs, pending], PendingN),
    couch_stats:update_gauge([couch_replicator, jobs, running], RunningN),
    couch_stats:update_gauge([couch_replicator, jobs, crashed], CrashedN),
    ok.

-spec stats_fold(#job{}, #stats_acc{}) -> #stats_acc{}.
stats_fold(#job{pid = undefined, history = [{added, _}]}, Acc) ->
    Acc#stats_acc{pending_n = Acc#stats_acc.pending_n + 1};
stats_fold(#job{pid = undefined, history = [{stopped, _} | _]}, Acc) ->
    Acc#stats_acc{pending_n = Acc#stats_acc.pending_n + 1};
stats_fold(#job{pid = undefined, history = [{{crashed, _}, _} | _]}, Acc) ->
    Acc#stats_acc{crashed_n = Acc#stats_acc.crashed_n + 1};
stats_fold(#job{pid = P, history = [{started, _} | _]}, Acc) when is_pid(P) ->
    Acc#stats_acc{running_n = Acc#stats_acc.running_n + 1}.

-spec existing_replication(#rep{}) -> boolean().
existing_replication(#rep{} = NewRep) ->
    case job_by_id(NewRep#rep.id) of
        {ok, #job{rep = CurRep}} ->
            NormCurRep = couch_replicator_utils:normalize_rep(CurRep),
            NormNewRep = couch_replicator_utils:normalize_rep(NewRep),
            NormCurRep == NormNewRep;
        {error, not_found} ->
            false
    end.

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

backoff_micros_test_() ->
    BaseInterval = ?BACKOFF_INTERVAL_MICROS,
    [
        ?_assertEqual(R * BaseInterval, backoff_micros(N))
     || {R, N} <- [
            {1, 1},
            {2, 2},
            {4, 3},
            {8, 4},
            {16, 5},
            {32, 6},
            {64, 7},
            {128, 8},
            {256, 9},
            {512, 10},
            {1024, 11},
            {1024, 12}
        ]
    ].

consecutive_crashes_test_() ->
    Threshold = ?DEFAULT_HEALTH_THRESHOLD_SEC,
    [
        ?_assertEqual(R, consecutive_crashes(H, Threshold))
     || {R, H} <- [
            {0, []},
            {0, [added()]},
            {0, [stopped()]},
            {0, [crashed()]},
            {1, [crashed(), added()]},
            {1, [crashed(), crashed()]},
            {1, [crashed(), stopped()]},
            {3, [crashed(), crashed(), crashed(), added()]},
            {2, [crashed(), crashed(), stopped()]},
            {1, [crashed(), started(), added()]},
            {2, [crashed(3), started(2), crashed(1), started(0)]},
            {0, [stopped(3), started(2), crashed(1), started(0)]},
            {1, [crashed(3), started(2), stopped(1), started(0)]},
            {0, [crashed(999), started(0)]},
            {1, [crashed(999), started(998), crashed(997), started(0)]}
        ]
    ].

consecutive_crashes_non_default_threshold_test_() ->
    [
        ?_assertEqual(R, consecutive_crashes(H, T))
     || {R, H, T} <- [
            {0, [crashed(11), started(0)], 10},
            {1, [crashed(10), started(0)], 10}
        ]
    ].

latest_crash_timestamp_test_() ->
    [
        ?_assertEqual({0, R, 0}, latest_crash_timestamp(H))
     || {R, H} <- [
            {0, [added()]},
            {1, [crashed(1)]},
            {3, [crashed(3), started(2), crashed(1), started(0)]},
            {1, [started(3), stopped(2), crashed(1), started(0)]}
        ]
    ].

last_started_test_() ->
    [
        ?_assertEqual({0, R, 0}, last_started(testjob(H)))
     || {R, H} <- [
            {0, [added()]},
            {0, [crashed(1)]},
            {1, [started(1)]},
            {1, [added(), started(1)]},
            {2, [started(2), started(1)]},
            {2, [crashed(3), started(2), started(1)]}
        ]
    ].

longest_running_test() ->
    J0 = testjob([crashed()]),
    J1 = testjob([started(1)]),
    J2 = testjob([started(2)]),
    SortFun = fun(A, B) -> last_started(A) =< last_started(B) end,
    Sort = fun(Jobs) -> lists:sort(SortFun, Jobs) end,
    ?assertEqual([], Sort([])),
    ?assertEqual([J1], Sort([J1])),
    ?assertEqual([J1, J2], Sort([J2, J1])),
    ?assertEqual([J0, J1, J2], Sort([J2, J1, J0])).

scheduler_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_pending_jobs_simple),
                ?TDEF_FE(t_pending_jobs_skip_crashed),
                ?TDEF_FE(t_pending_jobs_skip_running),
                ?TDEF_FE(t_one_job_starts),
                ?TDEF_FE(t_no_jobs_start_if_max_is_0),
                ?TDEF_FE(t_one_job_starts_if_max_is_1),
                ?TDEF_FE(t_max_churn_does_not_throttle_initial_start),
                ?TDEF_FE(t_excess_oneshot_only_jobs),
                ?TDEF_FE(t_excess_continuous_only_jobs),
                ?TDEF_FE(t_excess_prefer_continuous_first),
                ?TDEF_FE(t_stop_oldest_first),
                ?TDEF_FE(t_start_oldest_first),
                ?TDEF_FE(t_jobs_churn_even_if_not_all_max_jobs_are_running),
                ?TDEF_FE(t_jobs_dont_churn_if_there_are_available_running_slots),
                ?TDEF_FE(t_start_only_pending_jobs_do_not_churn_existing_ones),
                ?TDEF_FE(t_dont_stop_if_nothing_pending),
                ?TDEF_FE(t_max_churn_limits_number_of_rotated_jobs),
                ?TDEF_FE(t_existing_jobs),
                ?TDEF_FE(t_if_pending_less_than_running_start_all_pending),
                ?TDEF_FE(t_running_less_than_pending_swap_all_running),
                ?TDEF_FE(t_oneshot_dont_get_rotated),
                ?TDEF_FE(t_rotate_continuous_only_if_mixed),
                ?TDEF_FE(t_oneshot_dont_get_starting_priority),
                ?TDEF_FE(t_oneshot_will_hog_the_scheduler),
                ?TDEF_FE(t_if_excess_is_trimmed_rotation_still_happens),
                ?TDEF_FE(t_if_transient_job_crashes_it_gets_removed),
                ?TDEF_FE(t_if_permanent_job_crashes_it_stays_in_ets),
                ?TDEF_FE(t_stop_all_stops_jobs),
                ?TDEF_FE(t_job_summary_running),
                ?TDEF_FE(t_job_summary_pending),
                ?TDEF_FE(t_job_summary_crashing_once),
                ?TDEF_FE(t_job_summary_crashing_many_times),
                ?TDEF_FE(t_job_summary_proxy_fields)
            ]
        }
    }.

t_pending_jobs_simple(_) ->
    Job1 = oneshot(1),
    Job2 = oneshot(2),
    setup_jobs([Job2, Job1]),
    ?assertEqual([], pending_jobs(0)),
    ?assertEqual([Job1], pending_jobs(1)),
    ?assertEqual([Job1, Job2], pending_jobs(2)),
    ?assertEqual([Job1, Job2], pending_jobs(3)).

t_pending_jobs_skip_crashed(_) ->
    Job = oneshot(1),
    Ts = os:timestamp(),
    History = [crashed(Ts), started(Ts) | Job#job.history],
    Job1 = Job#job{history = History},
    Job2 = oneshot(2),
    Job3 = oneshot(3),
    setup_jobs([Job2, Job1, Job3]),
    ?assertEqual([Job2], pending_jobs(1)),
    ?assertEqual([Job2, Job3], pending_jobs(2)),
    ?assertEqual([Job2, Job3], pending_jobs(3)).

t_pending_jobs_skip_running(_) ->
    Job1 = continuous(1),
    Job2 = continuous_running(2),
    Job3 = oneshot(3),
    Job4 = oneshot_running(4),
    Jobs = [Job1, Job2, Job3, Job4],
    setup_jobs(Jobs),
    ?assertEqual([Job1, Job3], pending_jobs(4)).

t_one_job_starts(_) ->
    setup_jobs([oneshot(1)]),
    ?assertEqual({0, 1}, run_stop_count()),
    reschedule(mock_state(?DEFAULT_MAX_JOBS)),
    ?assertEqual({1, 0}, run_stop_count()).

t_no_jobs_start_if_max_is_0(_) ->
    setup_jobs([oneshot(1)]),
    reschedule(mock_state(0)),
    ?assertEqual({0, 1}, run_stop_count()).

t_one_job_starts_if_max_is_1(_) ->
    setup_jobs([oneshot(1), oneshot(2)]),
    reschedule(mock_state(1)),
    ?assertEqual({1, 1}, run_stop_count()).

t_max_churn_does_not_throttle_initial_start(_) ->
    setup_jobs([oneshot(1), oneshot(2)]),
    reschedule(mock_state(?DEFAULT_MAX_JOBS, 0)),
    ?assertEqual({2, 0}, run_stop_count()).

t_excess_oneshot_only_jobs(_) ->
    setup_jobs([oneshot_running(1), oneshot_running(2)]),
    ?assertEqual({2, 0}, run_stop_count()),
    reschedule(mock_state(1)),
    ?assertEqual({1, 1}, run_stop_count()),
    reschedule(mock_state(0)),
    ?assertEqual({0, 2}, run_stop_count()).

t_excess_continuous_only_jobs(_) ->
    setup_jobs([continuous_running(1), continuous_running(2)]),
    ?assertEqual({2, 0}, run_stop_count()),
    reschedule(mock_state(1)),
    ?assertEqual({1, 1}, run_stop_count()),
    reschedule(mock_state(0)),
    ?assertEqual({0, 2}, run_stop_count()).

t_excess_prefer_continuous_first(_) ->
    Jobs = [
        continuous_running(1),
        oneshot_running(2),
        continuous_running(3)
    ],
    setup_jobs(Jobs),
    ?assertEqual({3, 0}, run_stop_count()),
    ?assertEqual({1, 0}, oneshot_run_stop_count()),
    reschedule(mock_state(2)),
    ?assertEqual({2, 1}, run_stop_count()),
    ?assertEqual({1, 0}, oneshot_run_stop_count()),
    reschedule(mock_state(1)),
    ?assertEqual({1, 0}, oneshot_run_stop_count()),
    reschedule(mock_state(0)),
    ?assertEqual({0, 1}, oneshot_run_stop_count()).

t_stop_oldest_first(_) ->
    Jobs = [
        continuous_running(7),
        continuous_running(4),
        continuous_running(5)
    ],
    setup_jobs(Jobs),
    reschedule(mock_state(2, 1)),
    ?assertEqual({2, 1}, run_stop_count()),
    ?assertEqual([4], jobs_stopped()),
    reschedule(mock_state(1, 1)),
    ?assertEqual([7], jobs_running()).

t_start_oldest_first(_) ->
    setup_jobs([continuous(7), continuous(2), continuous(5)]),
    reschedule(mock_state(1)),
    ?assertEqual({1, 2}, run_stop_count()),
    ?assertEqual([2], jobs_running()),
    reschedule(mock_state(2)),
    ?assertEqual({2, 1}, run_stop_count()),
    % After rescheduling with max_jobs = 2, 2 was stopped and 5, 7 should
    % be running.
    ?assertEqual([2], jobs_stopped()).

t_jobs_churn_even_if_not_all_max_jobs_are_running(_) ->
    setup_jobs([
        continuous_running(7),
        continuous(2),
        continuous(5)
    ]),
    reschedule(mock_state(2, 2)),
    ?assertEqual({2, 1}, run_stop_count()),
    ?assertEqual([7], jobs_stopped()).

t_jobs_dont_churn_if_there_are_available_running_slots(_) ->
    setup_jobs([
        continuous_running(1),
        continuous_running(2)
    ]),
    reschedule(mock_state(2, 2)),
    ?assertEqual({2, 0}, run_stop_count()),
    ?assertEqual([], jobs_stopped()),
    ?assertEqual(0, meck:num_calls(couch_replicator_scheduler_job, start_link, 1)).

t_start_only_pending_jobs_do_not_churn_existing_ones(_) ->
    setup_jobs([
        continuous(1),
        continuous_running(2)
    ]),
    reschedule(mock_state(2, 2)),
    ?assertEqual(1, meck:num_calls(couch_replicator_scheduler_job, start_link, 1)),
    ?assertEqual([], jobs_stopped()),
    ?assertEqual({2, 0}, run_stop_count()).

t_dont_stop_if_nothing_pending(_) ->
    setup_jobs([continuous_running(1), continuous_running(2)]),
    reschedule(mock_state(2)),
    ?assertEqual({2, 0}, run_stop_count()).

t_max_churn_limits_number_of_rotated_jobs(_) ->
    Jobs = [
        continuous(1),
        continuous_running(2),
        continuous(3),
        continuous_running(4)
    ],
    setup_jobs(Jobs),
    reschedule(mock_state(2, 1)),
    ?assertEqual([2, 3], jobs_stopped()).

t_if_pending_less_than_running_start_all_pending(_) ->
    Jobs = [
        continuous(1),
        continuous_running(2),
        continuous(3),
        continuous_running(4),
        continuous_running(5)
    ],
    setup_jobs(Jobs),
    reschedule(mock_state(3)),
    ?assertEqual([1, 2, 5], jobs_running()).

t_running_less_than_pending_swap_all_running(_) ->
    Jobs = [
        continuous(1),
        continuous(2),
        continuous(3),
        continuous_running(4),
        continuous_running(5)
    ],
    setup_jobs(Jobs),
    reschedule(mock_state(2)),
    ?assertEqual([3, 4, 5], jobs_stopped()).

t_oneshot_dont_get_rotated(_) ->
    setup_jobs([oneshot_running(1), continuous(2)]),
    reschedule(mock_state(1)),
    ?assertEqual([1], jobs_running()).

t_rotate_continuous_only_if_mixed(_) ->
    setup_jobs([continuous(1), oneshot_running(2), continuous_running(3)]),
    reschedule(mock_state(2)),
    ?assertEqual([1, 2], jobs_running()).

t_oneshot_dont_get_starting_priority(_) ->
    setup_jobs([continuous(1), oneshot(2), continuous_running(3)]),
    reschedule(mock_state(1)),
    ?assertEqual([1], jobs_running()).

% This tested in other test cases, it is here to mainly make explicit a property
% of one-shot replications -- they can starve other jobs if they "take control"
% of all the available scheduler slots.
t_oneshot_will_hog_the_scheduler(_) ->
    Jobs = [
        oneshot_running(1),
        oneshot_running(2),
        oneshot(3),
        continuous(4)
    ],
    setup_jobs(Jobs),
    reschedule(mock_state(2)),
    ?assertEqual([1, 2], jobs_running()).

t_if_excess_is_trimmed_rotation_still_happens(_) ->
    Jobs = [
        continuous(1),
        continuous_running(2),
        continuous_running(3)
    ],
    setup_jobs(Jobs),
    reschedule(mock_state(1)).

t_if_transient_job_crashes_it_gets_removed(_) ->
    Pid = mock_pid(),
    Rep = continuous_rep(),
    Job = #job{
        id = job1,
        pid = Pid,
        history = [added()],
        rep = Rep#rep{db_name = null}
    },
    setup_jobs([Job]),
    ?assertEqual(1, ets:info(?MODULE, size)),
    State = #state{max_history = 3, stats_pid = self()},
    {noreply, State} = handle_info(
        {'EXIT', Pid, failed},
        State
    ),
    ?assertEqual(0, ets:info(?MODULE, size)).

t_if_permanent_job_crashes_it_stays_in_ets(_) ->
    Pid = mock_pid(),
    Rep = continuous_rep(),
    Job = #job{
        id = job1,
        pid = Pid,
        history = [added()],
        rep = Rep#rep{db_name = <<"db1">>}
    },
    setup_jobs([Job]),
    ?assertEqual(1, ets:info(?MODULE, size)),
    State = #state{
        max_jobs = 1,
        max_history = 3,
        stats_pid = self()
    },
    {noreply, State} = handle_info(
        {'EXIT', Pid, failed},
        State
    ),
    ?assertEqual(1, ets:info(?MODULE, size)),
    [Job1] = ets:lookup(?MODULE, job1),
    [Latest | _] = Job1#job.history,
    ?assertMatch({{crashed, failed}, _}, Latest).

t_stop_all_stops_jobs(_) ->
    Jobs = [
        oneshot_running(1),
        oneshot_running(2),
        oneshot(3),
        continuous(4)
    ],
    setup_jobs(Jobs),
    ?assertEqual(ok, stop_clear_all_jobs(?TERMINATE_SHUTDOWN_TIME)),
    ?assertEqual([], jobs_running()),
    ?assertEqual(0, ets:info(?MODULE, size)).

t_existing_jobs(_) ->
    Rep0 = continuous_rep(<<"s">>, <<"t">>),
    Rep = Rep0#rep{id = job1, db_name = <<"db">>},
    setup_jobs([#job{id = Rep#rep.id, rep = Rep}]),
    NewRep0 = continuous_rep(<<"s">>, <<"t">>),
    NewRep = NewRep0#rep{id = Rep#rep.id, db_name = <<"db">>},
    ?assert(existing_replication(NewRep)),
    ?assertNot(existing_replication(NewRep#rep{source = <<"s1">>})),
    ?assertNot(existing_replication(NewRep#rep{target = <<"t1">>})),
    ?assertNot(existing_replication(NewRep#rep{options = []})).

t_job_summary_running(_) ->
    Rep = rep(<<"s">>, <<"t">>),
    Job = #job{
        id = job1,
        pid = mock_pid(),
        history = [added()],
        rep = Rep#rep{db_name = <<"db1">>}
    },
    setup_jobs([Job]),
    Summary = job_summary(job1, ?DEFAULT_HEALTH_THRESHOLD_SEC),
    ?assertEqual(running, proplists:get_value(state, Summary)),
    ?assertEqual(null, proplists:get_value(info, Summary)),
    ?assertEqual(0, proplists:get_value(error_count, Summary)),

    Stats = [{source_seq, <<"1-abc">>}],
    handle_cast({update_job_stats, job1, Stats}, mock_state(1)),
    Summary1 = job_summary(job1, ?DEFAULT_HEALTH_THRESHOLD_SEC),
    ?assertEqual({Stats}, proplists:get_value(info, Summary1)).

t_job_summary_pending(_) ->
    Job = #job{
        id = job1,
        pid = undefined,
        history = [stopped(20), started(10), added()],
        rep = rep(<<"s">>, <<"t">>)
    },
    setup_jobs([Job]),
    Summary = job_summary(job1, ?DEFAULT_HEALTH_THRESHOLD_SEC),
    ?assertEqual(pending, proplists:get_value(state, Summary)),
    ?assertEqual(null, proplists:get_value(info, Summary)),
    ?assertEqual(0, proplists:get_value(error_count, Summary)),

    Stats = [{doc_write_failures, 1}],
    handle_cast({update_job_stats, job1, Stats}, mock_state(1)),
    Summary1 = job_summary(job1, ?DEFAULT_HEALTH_THRESHOLD_SEC),
    ?assertEqual({Stats}, proplists:get_value(info, Summary1)).

t_job_summary_crashing_once(_) ->
    Job = #job{
        id = job1,
        history = [crashed(?DEFAULT_HEALTH_THRESHOLD_SEC + 1), started(0)],
        rep = rep(<<"s">>, <<"t">>)
    },
    setup_jobs([Job]),
    Summary = job_summary(job1, ?DEFAULT_HEALTH_THRESHOLD_SEC),
    ?assertEqual(crashing, proplists:get_value(state, Summary)),
    Info = proplists:get_value(info, Summary),
    ?assertEqual({[{<<"error">>, <<"some_reason">>}]}, Info),
    ?assertEqual(0, proplists:get_value(error_count, Summary)).

t_job_summary_crashing_many_times(_) ->
    Job = #job{
        id = job1,
        history = [crashed(4), started(3), crashed(2), started(1)],
        rep = rep(<<"s">>, <<"t">>)
    },
    setup_jobs([Job]),
    Summary = job_summary(job1, ?DEFAULT_HEALTH_THRESHOLD_SEC),
    ?assertEqual(crashing, proplists:get_value(state, Summary)),
    Info = proplists:get_value(info, Summary),
    ?assertEqual({[{<<"error">>, <<"some_reason">>}]}, Info),
    ?assertEqual(2, proplists:get_value(error_count, Summary)).

t_job_summary_proxy_fields(_) ->
    Src = #httpdb{
        url = "https://s",
        proxy_url = "http://u:p@sproxy:12"
    },
    Tgt = #httpdb{
        url = "http://t",
        proxy_url = "socks5://u:p@tproxy:34"
    },
    Job = #job{
        id = job1,
        history = [started(10), added()],
        rep = rep(Src, Tgt)
    },
    setup_jobs([Job]),
    Summary = job_summary(job1, ?DEFAULT_HEALTH_THRESHOLD_SEC),
    ?assertEqual(
        <<"http://u:*****@sproxy:12">>,
        proplists:get_value(source_proxy, Summary)
    ),
    ?assertEqual(
        <<"socks5://u:*****@tproxy:34">>,
        proplists:get_value(target_proxy, Summary)
    ).

% Test helper functions

setup_all() ->
    catch ets:delete(?MODULE),
    meck:expect(config, get, 1, []),
    meck:expect(config, get, 2, undefined),
    meck:expect(couch_log, notice, 2, ok),
    meck:expect(couch_log, warning, 2, ok),
    meck:expect(couch_log, error, 2, ok),
    meck:expect(couch_replicator_scheduler_job, stop, 1, ok),
    meck:expect(couch_stats, increment_counter, 1, ok),
    meck:expect(couch_stats, update_gauge, 2, ok),
    Pid = mock_pid(),
    meck:expect(couch_replicator_scheduler_job, start_link, 1, {ok, Pid}),
    couch_replicator_share:init().

teardown_all(_) ->
    couch_replicator_share:clear(),
    catch ets:delete(?MODULE),
    meck:unload().

setup() ->
    meck:reset([
        couch_log,
        couch_replicator_scheduler_job,
        couch_stats,
        config
    ]).

teardown(_) ->
    ok.

setup_jobs(Jobs) when is_list(Jobs) ->
    ?MODULE = ets:new(?MODULE, [named_table, {keypos, #job.id}]),
    ets:insert(?MODULE, Jobs).

all_jobs() ->
    lists:usort(ets:tab2list(?MODULE)).

jobs_stopped() ->
    [Job#job.id || Job <- all_jobs(), Job#job.pid =:= undefined].

jobs_running() ->
    [Job#job.id || Job <- all_jobs(), Job#job.pid =/= undefined].

run_stop_count() ->
    {length(jobs_running()), length(jobs_stopped())}.

oneshot_run_stop_count() ->
    Running = [
        Job#job.id
     || Job <- all_jobs(),
        Job#job.pid =/= undefined,
        not is_continuous(Job)
    ],
    Stopped = [
        Job#job.id
     || Job <- all_jobs(),
        Job#job.pid =:= undefined,
        not is_continuous(Job)
    ],
    {length(Running), length(Stopped)}.

mock_state(MaxJobs) ->
    #state{
        max_jobs = MaxJobs,
        max_churn = ?DEFAULT_MAX_CHURN,
        max_history = ?DEFAULT_MAX_HISTORY,
        stats_pid = self()
    }.

mock_state(MaxJobs, MaxChurn) ->
    #state{
        max_jobs = MaxJobs,
        max_churn = MaxChurn,
        max_history = ?DEFAULT_MAX_HISTORY,
        stats_pid = self()
    }.

rep() ->
    #rep{options = [], user_ctx = #user_ctx{}}.

rep(Src, Tgt) ->
    Rep = rep(),
    Rep#rep{source = Src, target = Tgt}.

continuous_rep() ->
    #rep{options = [{continuous, true}], user_ctx = #user_ctx{}}.

continuous_rep(Src, Tgt) ->
    Rep = continuous_rep(),
    Rep#rep{source = Src, target = Tgt}.

continuous(Id) when is_integer(Id) ->
    Started = Id,
    Hist = [stopped(Started + 1), started(Started), added()],
    #job{
        id = Id,
        history = Hist,
        rep = continuous_rep()
    }.

continuous_running(Id) when is_integer(Id) ->
    Started = Id,
    Pid = mock_pid(),
    #job{
        id = Id,
        history = [started(Started), added()],
        rep = continuous_rep(),
        pid = Pid
    }.

oneshot(Id) when is_integer(Id) ->
    Started = Id,
    Hist = [stopped(Started + 1), started(Started), added()],
    #job{id = Id, history = Hist, rep = rep()}.

oneshot_running(Id) when is_integer(Id) ->
    Started = Id,
    Pid = mock_pid(),
    #job{
        id = Id,
        history = [started(Started), added()],
        rep = rep(),
        pid = Pid
    }.

testjob(Hist) when is_list(Hist) ->
    #job{history = Hist}.

mock_pid() ->
    list_to_pid("<0.999.999>").

crashed() ->
    crashed(0).

crashed(WhenSec) when is_integer(WhenSec) ->
    {{crashed, some_reason}, {0, WhenSec, 0}};
crashed({MSec, Sec, USec}) ->
    {{crashed, some_reason}, {MSec, Sec, USec}}.

started() ->
    started(0).

started(WhenSec) when is_integer(WhenSec) ->
    {started, {0, WhenSec, 0}};
started({MSec, Sec, USec}) ->
    {started, {MSec, Sec, USec}}.

stopped() ->
    stopped(0).

stopped(WhenSec) ->
    {stopped, {0, WhenSec, 0}}.

added() ->
    {added, {0, 0, 0}}.

-endif.
