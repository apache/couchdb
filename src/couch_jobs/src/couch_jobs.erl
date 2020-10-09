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

-module(couch_jobs).

-export([
    % Job creation
    add/4,
    add/5,
    remove/3,

    % Job monitoring
    get_types/1,
    get_job_data/3,
    get_job_state/3,
    get_active_jobs_ids/2,
    fold_jobs/4,
    pending_count/2,
    pending_count/3,

    % Job processing
    accept/1,
    accept/2,
    finish/2,
    finish/3,
    resubmit/2,
    resubmit/3,
    resubmit/4,
    is_resubmitted/1,
    update/2,
    update/3,

    % Subscriptions
    subscribe/2,
    subscribe/3,
    unsubscribe/1,
    wait/2,
    wait/3,

    % Type timeouts
    set_type_timeout/2,
    clear_type_timeout/1,
    get_type_timeout/1
]).


-include("couch_jobs.hrl").


-define(MIN_ACCEPT_WAIT_MSEC, 100).


%% Job Creation API

-spec add(jtx(), job_type(), job_id(), job_data()) -> ok | {error, any()}.
add(Tx, Type, JobId, JobData) ->
    add(Tx, Type, JobId, JobData, 0).


-spec add(jtx(), job_type(), job_id(), job_data(), scheduled_time()) ->
    ok | {error, any()}.
add(Tx, Type, JobId, JobData, ScheduledTime) when is_binary(JobId),
        is_map(JobData), is_integer(ScheduledTime) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        case couch_jobs_fdb:add(JTx, Type, JobId, JobData, ScheduledTime) of
            {ok, _, _, _} -> ok;
            {error, Error} -> {error, Error}
        end
    end).


-spec remove(jtx(), job_type(), job_id()) -> ok | {error, any()}.
remove(Tx, Type, JobId) when is_binary(JobId) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        couch_jobs_fdb:remove(JTx, job(Type, JobId))
    end).


-spec get_types(jtx()) -> [job_type()] | {error, any()}.
get_types(Tx) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        couch_jobs_fdb:get_types(JTx)
    end).


-spec get_job_data(jtx(), job_type(), job_id()) -> {ok, job_data()} | {error,
    any()}.
get_job_data(Tx, Type, JobId) when is_binary(JobId) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        case couch_jobs_fdb:get_job_state_and_data(JTx, job(Type, JobId)) of
            {ok, _Seq, _State, Data} ->
                {ok, couch_jobs_fdb:decode_data(Data)};
            {error, Error} ->
                {error, Error}
        end
    end).


-spec get_job_state(jtx(), job_type(), job_id()) -> {ok, job_state()} | {error,
    any()}.
get_job_state(Tx, Type, JobId) when is_binary(JobId) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        case couch_jobs_fdb:get_job_state_and_data(JTx, job(Type, JobId)) of
            {ok, _Seq, State, _Data} ->
                {ok, State};
            {error, Error} ->
                {error, Error}
        end
    end).


-spec get_active_jobs_ids(jtx(), job_type()) -> [job_id()] | {error,
    any()}.
get_active_jobs_ids(Tx, Type) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        Since = couch_jobs_fdb:get_active_since(JTx, Type,
            {versionstamp, 0, 0}),
        maps:keys(Since)
    end).


-spec fold_jobs(jtx(), job_type(), fun(), any()) -> any().
fold_jobs(Tx, Type, Fun, UserAcc) when is_function(Fun, 5) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        maps:fold(fun(JobId, {_Seq, JobState, DataEnc}, Acc) ->
            Data = couch_jobs_fdb:decode_data(DataEnc),
            Fun(JTx, JobId, JobState, Data, Acc)
        end, UserAcc, couch_jobs_fdb:get_jobs(JTx, Type))
    end).


-spec pending_count(jtx(), job_type()) -> integer().
pending_count(Tx, Type) ->
    pending_count(Tx, Type, #{}).


-spec pending_count(jtx(), job_type(), #{}) -> integer().
pending_count(Tx, Type, Opts) ->
    MaxSTime = maps:get(max_sched_time, Opts, ?UNDEFINED_MAX_SCHEDULED_TIME),
    Limit = maps:get(limit, Opts, 1024),
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        couch_jobs_pending:pending_count(JTx, Type, MaxSTime, Limit)
    end).


%% Job processor API

-spec accept(job_type()) -> {ok, job(), job_data()} | {error, any()}.
accept(Type) ->
    accept(Type, #{}).


-spec accept(job_type(), job_accept_opts()) -> {ok, job()} | {error, any()}.
accept(Type, #{} = Opts) ->
    NoSched = maps:get(no_schedule, Opts, false),
    MaxSchedTimeDefault = case NoSched of
        true -> 0;
        false -> ?UNDEFINED_MAX_SCHEDULED_TIME
    end,
    MaxSchedTime = maps:get(max_sched_time, Opts, MaxSchedTimeDefault),
    Timeout = maps:get(timeout, Opts, infinity),
    case NoSched andalso MaxSchedTime =/= 0 of
        true ->
            {error, no_schedule_require_0_max_sched_time};
        false ->
            accept_loop(Type, NoSched, MaxSchedTime, Timeout)
    end.


-spec finish(jtx(), job()) -> ok | {error, any()}.
finish(Tx, Job) ->
    finish(Tx, Job, undefined).


-spec finish(jtx(), job(), job_data()) -> ok | {error, any()}.
finish(Tx, #{jlock := <<_/binary>>} = Job, JobData) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        couch_jobs_fdb:finish(JTx, Job, JobData)
    end).


-spec resubmit(jtx(), job()) -> {ok, job()} | {error, any()}.
resubmit(Tx, Job) ->
    resubmit(Tx, Job, ?UNDEFINED_MAX_SCHEDULED_TIME).


-spec resubmit(jtx(), job(), scheduled_time()) -> {ok, job()} | {error, any()}.
resubmit(Tx, #{jlock := <<_/binary>>} = Job, SchedTime) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        couch_jobs_fdb:resubmit(JTx, Job, SchedTime)
    end).


-spec resubmit(jtx(), job(), scheduled_time(), job_data()) -> {ok, job()} | {error, any()}.
resubmit(Tx, #{jlock := <<_/binary>>} = Job, SchedTime, Data) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        couch_jobs_fdb:resubmit(JTx, Job, SchedTime, Data)
    end).


-spec is_resubmitted(job()) -> true | false.
is_resubmitted(#{job := true} = Job) ->
    maps:get(resubmit, Job, false).


-spec update(jtx(), job()) -> {ok, job()} | {error, any()}.
update(Tx, Job) ->
    update(Tx, Job, undefined).


-spec update(jtx(), job(), job_data()) -> {ok, job()} | {error, any()}.
update(Tx, #{jlock := <<_/binary>>} = Job, JobData) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        couch_jobs_fdb:update(JTx, Job, JobData)
    end).


%% Subscription API

% Receive events as messages. Wait for them using `wait/2,3`
% functions.
%

-spec subscribe(job_type(), job_id()) -> {ok, job_subscription(), job_state(),
    job_data()} | {ok, finished, job_data()} | {error, any()}.
subscribe(Type, JobId) ->
    subscribe(undefined, Type, JobId).


-spec subscribe(jtx(), job_type(), job_id()) -> {ok, job_subscription(),
    job_state(), job_data()} | {ok, finished, job_data()} | {error, any()}.
subscribe(Tx, Type, JobId) ->
    StateData = couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        Job = #{job => true, type => Type, id => JobId},
        couch_jobs_fdb:get_job_state_and_data(JTx, Job)
    end),
    case StateData of
        {ok, _Seq, finished, Data} ->
            {ok, finished, couch_jobs_fdb:decode_data(Data)};
        {ok, Seq, State, Data} ->
            case couch_jobs_notifier:subscribe(Type, JobId, State, Seq) of
                {ok, SubRef} ->
                    Data1 = couch_jobs_fdb:decode_data(Data),
                    {ok, SubRef, State, Data1};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


% Unsubscribe from getting notifications based on a particular subscription.
% Each subscription should be followed by its own unsubscription call. However,
% subscriber processes are also monitored and auto-unsubscribed if they exit.
% If subscribing process is exiting, calling this function is optional.
%
-spec unsubscribe(job_subscription()) -> ok.
unsubscribe({Server, Ref}) when is_pid(Server), is_reference(Ref) ->
    try
        couch_jobs_notifier:unsubscribe(Server, Ref)
    after
        flush_notifications(Ref)
    end.


% Wait to receive job state updates
%
-spec wait(job_subscription() | [job_subscription()], timeout()) ->
    {job_type(), job_id(), job_state(), job_data()} | timeout.
wait({_, Ref}, Timeout) ->
    receive
        {?COUCH_JOBS_EVENT, Ref, Type, Id, State, Data} ->
            {Type, Id, State, couch_jobs_fdb:decode_data(Data)}
    after
        Timeout -> timeout
    end;

wait(Subs, Timeout) when is_list(Subs) ->
    {Result, ResendQ} = wait_any(Subs, Timeout, []),
    lists:foreach(fun(Msg) -> self() ! Msg end, ResendQ),
    Result.


-spec wait(job_subscription() | [job_subscription()], job_state(), timeout())
    -> {job_type(), job_id(), job_state(), job_data()} | timeout.
wait({_, Ref} = Sub, State, Timeout) when is_atom(State) ->
    receive
        {?COUCH_JOBS_EVENT, Ref, Type, Id, MsgState, Data0} ->
            case MsgState =:= State of
                true ->
                    Data = couch_jobs_fdb:decode_data(Data0),
                    {Type, Id, State, Data};
                false ->
                    wait(Sub, State, Timeout)
            end
    after
        Timeout -> timeout
    end;

wait(Subs, State, Timeout) when is_list(Subs),
        is_atom(State) ->
    {Result, ResendQ} = wait_any(Subs, State, Timeout, []),
    lists:foreach(fun(Msg) -> self() ! Msg end, ResendQ),
    Result.


%% Job type timeout API

% These functions manipulate the activity timeout for each job type.

-spec set_type_timeout(job_type(), timeout()) -> ok.
set_type_timeout(Type, Timeout) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(), fun(JTx) ->
        couch_jobs_fdb:set_type_timeout(JTx, Type, Timeout)
    end),
    ok = couch_jobs_server:force_check_types().


-spec clear_type_timeout(job_type()) -> ok.
clear_type_timeout(Type) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(), fun(JTx) ->
        couch_jobs_fdb:clear_type_timeout(JTx, Type)
    end).


-spec get_type_timeout(job_type()) -> timeout().
get_type_timeout(Type) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(), fun(JTx) ->
        couch_jobs_fdb:get_type_timeout(JTx, Type)
    end).


%% Private utilities

accept_loop(Type, NoSched, MaxSchedTime, Timeout) ->
    TxFun =  fun(JTx) ->
        couch_jobs_fdb:accept(JTx, Type, MaxSchedTime, NoSched)
    end,
    AcceptResult = try
        couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(), TxFun)
    catch
        error:{timeout, _} ->
            retry;
        error:{erlfdb_error, Err} when Err =:= 1020 orelse Err =:= 1031 ->
            retry
    end,
    case AcceptResult of
        {ok, Job, Data} ->
            {ok, Job, Data};
        retry ->
            accept_loop(Type, NoSched, MaxSchedTime, Timeout);
        {not_found, PendingWatch} ->
            case wait_pending(PendingWatch, MaxSchedTime, Timeout, NoSched) of
                {error, not_found} ->
                    {error, not_found};
                retry ->
                    accept_loop(Type, NoSched, MaxSchedTime, Timeout);
                ok ->
                    accept_loop(Type, NoSched, MaxSchedTime, Timeout)
            end
    end.


job(Type, JobId) ->
    #{job => true, type => Type, id => JobId}.


wait_pending(PendingWatch, _MaxSTime, _UserTimeout = 0, _NoSched) ->
    erlfdb:cancel(PendingWatch, [flush]),
    {error, not_found};

wait_pending(PendingWatch, MaxSTime, UserTimeout, NoSched) ->
    NowMSec = erlang:system_time(millisecond),
    Timeout0 = max(?MIN_ACCEPT_WAIT_MSEC, MaxSTime * 1000 - NowMSec),
    Timeout = min(limit_timeout(Timeout0, NoSched), UserTimeout),
    try
        erlfdb:wait(PendingWatch, [{timeout, Timeout}]),
        ok
    catch
        error:{erlfdb_error, ?FUTURE_VERSION} ->
            erlfdb:cancel(PendingWatch, [flush]),
            retry;
        error:{timeout, _} ->
            erlfdb:cancel(PendingWatch, [flush]),
            {error, not_found}
    end.


wait_any(Subs, Timeout0, ResendQ) when is_list(Subs) ->
    Timeout = limit_timeout(Timeout0, false),
    receive
        {?COUCH_JOBS_EVENT, Ref, Type, Id, State, Data0} = Msg ->
            case lists:keyfind(Ref, 2, Subs) of
                false ->
                    wait_any(Subs, Timeout, [Msg | ResendQ]);
                {_, Ref} ->
                    Data = couch_jobs_fdb:decode_data(Data0),
                    {{Type, Id, State, Data}, ResendQ}
            end
    after
        Timeout -> {timeout, ResendQ}
    end.


wait_any(Subs, State, Timeout0, ResendQ) when
        is_list(Subs) ->
    Timeout = limit_timeout(Timeout0, false),
    receive
        {?COUCH_JOBS_EVENT, Ref, Type, Id, MsgState, Data0} = Msg ->
            case lists:keyfind(Ref, 2, Subs) of
                false ->
                    wait_any(Subs, Timeout, [Msg | ResendQ]);
                {_, Ref} ->
                    case MsgState =:= State of
                        true ->
                            Data = couch_jobs_fdb:decode_data(Data0),
                            {{Type, Id, State, Data}, ResendQ};
                        false ->
                            wait_any(Subs, Timeout, ResendQ)
                    end
            end
    after
        Timeout -> {timeout, ResendQ}
    end.


limit_timeout(_Timeout, true) ->
    infinity;

limit_timeout(Timeout, false) when is_integer(Timeout), Timeout < 16#FFFFFFFF ->
    Timeout;

limit_timeout(_Timeout, false) ->
    infinity.


flush_notifications(Ref) ->
    receive
        {?COUCH_JOBS_EVENT, Ref, _, _, _} ->
            flush_notifications(Ref)
    after
        0 -> ok
    end.
