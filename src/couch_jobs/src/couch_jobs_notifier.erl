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

-module(couch_jobs_notifier).

-behaviour(gen_server).

-export([
    start_link/1,
    subscribe/4,
    unsubscribe/2
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    format_status/2
]).

-include("couch_jobs.hrl").
-include_lib("kernel/include/logger.hrl").

-define(TYPE_MONITOR_HOLDOFF_DEFAULT, "50").
-define(TYPE_MONITOR_TIMEOUT_DEFAULT, "infinity").
-define(INIT_BATCH_SIZE, "1000").
-define(BATCH_FACTOR, "0.75").
-define(BATCH_INCREMENT, "100").

-record(st, {
    jtx,
    type,
    monitor_pid,
    % #{JobId => #{Ref => {Pid, State, Seq}}}
    subs,
    % #{{Jobid, Pid} => Ref}
    pidmap,
    % #{Ref => JobId}
    refmap,
    batch_size
}).

start_link(Type) ->
    gen_server:start_link(?MODULE, [Type], []).

subscribe(Type, JobId, State, Seq) ->
    case couch_jobs_server:get_notifier_server(Type) of
        {ok, Server} ->
            CallArgs = {subscribe, JobId, State, Seq, self()},
            Ref = gen_server:call(Server, CallArgs, infinity),
            {ok, {Server, Ref}};
        {error, Error} ->
            {error, Error}
    end.

unsubscribe(Server, Ref) when is_reference(Ref) ->
    gen_server:call(Server, {unsubscribe, Ref, self()}, infinity).

init([Type]) ->
    JTx = couch_jobs_fdb:get_jtx(),
    St = #st{
        jtx = JTx,
        type = Type,
        subs = #{},
        pidmap = #{},
        refmap = #{},
        batch_size = init_batch_size()
    },
    VS = get_type_vs(St),
    HoldOff = get_holdoff(),
    Timeout = get_timeout(),
    Pid = couch_jobs_type_monitor:start(Type, VS, HoldOff, Timeout),
    {ok, St#st{monitor_pid = Pid}}.

terminate(_, _St) ->
    ok.

handle_call({subscribe, JobId, State, Seq, Pid}, _From, #st{} = St) ->
    #st{pidmap = PidMap, refmap = RefMap} = St,
    case maps:get({JobId, Pid}, PidMap, not_found) of
        not_found ->
            Ref = erlang:monitor(process, Pid),
            St1 = update_sub(JobId, Ref, Pid, State, Seq, St),
            St2 = St1#st{pidmap = PidMap#{{JobId, Pid} => Ref}},
            St3 = St2#st{refmap = RefMap#{Ref => JobId}},
            {reply, Ref, St3};
        Ref when is_reference(Ref) ->
            St1 = update_sub(JobId, Ref, Pid, State, Seq, St),
            {reply, Ref, St1}
    end;
handle_call({unsubscribe, Ref, Pid}, _From, #st{} = St) ->
    {reply, ok, unsubscribe_int(Ref, Pid, St)};
handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.

handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.

handle_info({type_updated, VS}, St) ->
    VSMax = flush_type_updated_messages(VS),
    {noreply, try_notify_subscribers(VSMax, St)};
handle_info({Ref, ready}, St) when is_reference(Ref) ->
    % Don't crash out couch_jobs_server and the whole application would need to
    % eventually do proper cleanup in erlfdb:wait timeout code.
    ?LOG_ERROR(#{
        what => spurious_future_ready,
        ref => Ref
    }),
    LogMsg = "~p : spurious erlfdb future ready message ~p",
    couch_log:error(LogMsg, [?MODULE, Ref]),
    {noreply, St};
handle_info({'DOWN', Ref, process, Pid, _}, #st{} = St) ->
    {noreply, unsubscribe_int(Ref, Pid, St)};
handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

format_status(_Opt, [_PDict, State]) ->
    #st{
        jtx = JTx,
        type = Type,
        monitor_pid = MonitorPid,
        subs = Subs,
        pidmap = PidMap,
        refmap = RefMap
    } = State,
    [
        {data, [
            {"State", [
                {jtx, JTx},
                {type, Type},
                {monitor_pid, MonitorPid},
                {subs, {map_size, maps:size(Subs)}},
                {pidmap, {map_size, maps:size(PidMap)}},
                {refmap, {map_size, maps:size(RefMap)}}
            ]}
        ]}
    ].

update_subs(JobId, Refs, #st{subs = Subs} = St) when map_size(Refs) =:= 0 ->
    St#st{subs = maps:remove(JobId, Subs)};
update_subs(JobId, Refs, #st{subs = Subs} = St) when map_size(Refs) > 0 ->
    St#st{subs = Subs#{JobId => Refs}}.

update_sub(JobId, Ref, Pid, State, Seq, #st{subs = Subs} = St) ->
    Refs = maps:get(JobId, Subs, #{}),
    update_subs(JobId, Refs#{Ref => {Pid, State, Seq}}, St).

remove_sub(JobId, Ref, #st{subs = Subs} = St) ->
    case maps:get(JobId, Subs, not_found) of
        not_found -> St;
        #{} = Refs -> update_subs(JobId, maps:remove(Ref, Refs), St)
    end.

unsubscribe_int(Id, Ref, Pid, #st{pidmap = PidMap, refmap = RefMap} = St) ->
    St1 = remove_sub(Id, Ref, St),
    erlang:demonitor(Ref, [flush]),
    St1#st{
        pidmap = maps:remove({Id, Pid}, PidMap),
        refmap = maps:remove(Ref, RefMap)
    }.

unsubscribe_int(Ref, Pid, #st{refmap = RefMap} = St) ->
    case maps:get(Ref, RefMap, not_found) of
        not_found -> St;
        Id -> unsubscribe_int(Id, Ref, Pid, St)
    end.

flush_type_updated_messages(VSMax) ->
    receive
        {type_updated, VS} ->
            flush_type_updated_messages(max(VS, VSMax))
    after 0 -> VSMax
    end.

get_jobs(#st{} = St, Ids) when is_list(Ids) ->
    #st{jtx = JTx, type = Type, batch_size = BatchSize} = St,
    {Jobs, NewBatchSize} = get_jobs_iter(JTx, Type, Ids, BatchSize, #{}),
    {Jobs, St#st{batch_size = NewBatchSize}}.

get_jobs_iter(_Jtx, _Type, [], BatchSize, #{} = Acc) ->
    {Acc, BatchSize};
get_jobs_iter(JTx, Type, Ids, BatchSize, #{} = Acc0) ->
    {BatchIds, RestIds} =
        case length(Ids) < BatchSize of
            true -> {Ids, []};
            false -> lists:split(BatchSize, Ids)
        end,
    Result =
        try
            couch_jobs_fdb:tx(JTx, fun(JTx1) ->
                lists:foldl(
                    fun(JobId, #{} = Acc) ->
                        Job = #{job => true, type => Type, id => JobId},
                        case couch_jobs_fdb:get_job_state_and_data(JTx1, Job) of
                            {ok, Seq, State, Data} ->
                                Acc#{JobId => {Seq, State, Data}};
                            {error, not_found} ->
                                Acc#{JobId => {null, not_found, not_found}}
                        end
                    end,
                    Acc0,
                    BatchIds
                )
            end)
        catch
            error:{Tag, Err} when ?COUCH_JOBS_RETRYABLE(Tag, Err) ->
                failed
        end,
    case Result of
        #{} = AccF ->
            NewBatchSize = BatchSize + batch_increment(),
            get_jobs_iter(JTx, Type, RestIds, NewBatchSize, AccF);
        failed ->
            NewBatchSize = max(1, round(BatchSize * batch_factor())),
            get_jobs_iter(JTx, Type, Ids, NewBatchSize, Acc0)
    end.

get_type_vs(#st{jtx = JTx, type = Type}) ->
    couch_jobs_fdb:tx(JTx, fun(JTx1) ->
        couch_jobs_fdb:get_activity_vs(JTx1, Type)
    end).

% "Active since" is the set of jobs that have been active (running)
% and updated at least once since the given versionstamp. These are relatively
% cheap to find as it's just a range read in the ?ACTIVITY subspace.
%
get_active_since(#st{} = St, not_found) ->
    {#{}, St};
get_active_since(#st{} = St, VS) ->
    #st{jtx = JTx, type = Type, subs = Subs, batch_size = BatchSize} = St,
    {Updated, NewBatchSize} = get_active_iter(JTx, Type, VS, BatchSize, #{}),
    UpdatedSubs = maps:map(
        fun(_JobId, Data) ->
            {VS, running, Data}
        end,
        maps:with(maps:keys(Subs), Updated)
    ),
    {UpdatedSubs, St#st{batch_size = NewBatchSize}}.

get_active_iter(JTx, Type, VS, BatchSize, #{} = Acc) ->
    Opts = [{limit, BatchSize}],
    Result =
        try
            couch_jobs_fdb:tx(JTx, fun(JTx1) ->
                couch_jobs_fdb:get_active_since(JTx1, Type, VS, Opts)
            end)
        catch
            error:{Tag, Err} when ?COUCH_JOBS_RETRYABLE(Tag, Err) ->
                failed
        end,
    case Result of
        {Updated, _FinalSeq} when map_size(Updated) < BatchSize ->
            {maps:merge(Acc, Updated), BatchSize};
        {Updated, FinalSeq} when map_size(Updated) >= BatchSize ->
            Acc1 = maps:merge(Acc, Updated),
            NewBatchSize = BatchSize + batch_increment(),
            NextSeq = fabric2_fdb:next_vs(FinalSeq),
            get_active_iter(JTx, Type, NextSeq, NewBatchSize, Acc1);
        failed ->
            NewBatchSize = max(1, round(BatchSize * batch_factor())),
            get_active_iter(JTx, Type, VS, NewBatchSize, Acc)
    end.

try_notify_subscribers(ActiveVS, #st{} = St) ->
    try
        notify_subscribers(ActiveVS, St)
    catch
        error:{Tag, Err} when ?COUCH_JOBS_RETRYABLE(Tag, Err) ->
            try_notify_subscribers(ActiveVS, St)
    end.

notify_subscribers(_, #st{subs = Subs} = St) when map_size(Subs) =:= 0 ->
    St;
notify_subscribers(ActiveVS, #st{} = St1) ->
    % First gather the easy (cheap) active jobs. Then with those out of way
    % inspect each job to get its state.
    {Active, St2} = get_active_since(St1, ActiveVS),
    St3 = notify_job_ids(Active, St2),
    ActiveIds = maps:keys(Active),
    Subs = St3#st.subs,
    InactiveIds = maps:keys(maps:without(ActiveIds, Subs)),
    {Inactive, St4} = get_jobs(St3, InactiveIds),
    notify_job_ids(Inactive, St4).

notify_job_ids(#{} = Jobs, #st{type = Type} = St0) ->
    maps:fold(
        fun(Id, {VS, State, Data}, #st{} = StAcc) ->
            DoUnsub = lists:member(State, [finished, not_found]),
            maps:fold(
                fun
                    (_Ref, {_Pid, running, OldVS}, St) when
                        State =:= running,
                        OldVS >= VS
                    ->
                        St;
                    (Ref, {Pid, running, OldVS}, St) when
                        State =:= running,
                        OldVS < VS
                    ->
                        % For running state send updates even if state doesn't change
                        notify(Pid, Ref, Type, Id, State, Data),
                        update_sub(Id, Ref, Pid, running, VS, St);
                    (_Ref, {_Pid, OldState, _VS}, St) when OldState =:= State ->
                        St;
                    (Ref, {Pid, _State, _VS}, St) ->
                        notify(Pid, Ref, Type, Id, State, Data),
                        case DoUnsub of
                            true -> unsubscribe_int(Id, Ref, Pid, St);
                            false -> update_sub(Id, Ref, Pid, State, VS, St)
                        end
                end,
                StAcc,
                maps:get(Id, StAcc#st.subs, #{})
            )
        end,
        St0,
        Jobs
    ).

notify(Pid, Ref, Type, Id, State, Data) ->
    Pid ! {?COUCH_JOBS_EVENT, Ref, Type, Id, State, Data}.

get_holdoff() ->
    couch_jobs_util:get_non_neg_int(
        type_monitor_holdoff_msec,
        ?TYPE_MONITOR_HOLDOFF_DEFAULT
    ).

get_timeout() ->
    couch_jobs_util:get_timeout(
        type_monitor_timeout_msec,
        ?TYPE_MONITOR_TIMEOUT_DEFAULT
    ).

init_batch_size() ->
    couch_jobs_util:get_non_neg_int(
        notifier_init_batch_size,
        ?INIT_BATCH_SIZE
    ).

batch_increment() ->
    couch_jobs_util:get_non_neg_int(
        notifier_batch_increment,
        ?BATCH_INCREMENT
    ).

batch_factor() ->
    couch_jobs_util:get_float_0_1(
        notifier_batch_factor,
        ?BATCH_FACTOR
    ).
