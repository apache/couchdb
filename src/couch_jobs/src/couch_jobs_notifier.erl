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
    code_change/3
]).


-include("couch_jobs.hrl").


-define(TYPE_MONITOR_HOLDOFF_DEFAULT, 50).
-define(TYPE_MONITOR_TIMEOUT_DEFAULT, "infinity").
-define(GET_JOBS_RANGE_RATIO, 0.5).


-record(st, {
    jtx,
    type,
    monitor_pid,
    subs, % #{JobId => #{Ref => {Pid, State, Seq}}}
    pidmap, % #{{Jobid, Pid} => Ref}
    refmap % #{Ref => JobId}
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
        refmap = #{}
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
    {noreply, notify_subscribers(VSMax, St)};

handle_info({Ref, ready}, St) when is_reference(Ref) ->
    % Don't crash out couch_jobs_server and the whole application would need to
    % eventually do proper cleanup in erlfdb:wait timeout code.
    LogMsg = "~p : spurious erlfdb future ready message ~p",
    couch_log:error(LogMsg, [?MODULE, Ref]),
    {noreply, St};

handle_info({'DOWN', Ref, process, Pid, _}, #st{} = St) ->
    {noreply, unsubscribe_int(Ref, Pid, St)};

handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


update_subs(JobId, Refs, #st{subs = Subs} = St) when map_size(Refs) =:= 0 ->
    St#st{subs = maps:remove(JobId, Subs)};

update_subs(JobId, Refs, #st{subs = Subs} = St) when map_size(Refs) > 0 ->
    St#st{subs = Subs#{JobId => Refs}}.


update_sub(JobId, Ref, Pid, State, Seq, #st{subs = Subs} = St) ->
    Refs =  maps:get(JobId, Subs, #{}),
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
    after
        0 -> VSMax
    end.


get_jobs(#st{jtx = JTx, type = Type}, InactiveIdMap, Ratio)
        when Ratio >= ?GET_JOBS_RANGE_RATIO ->
    Filter = fun(JobId) -> maps:is_key(JobId, InactiveIdMap) end,
    JobMap = couch_jobs_fdb:tx(JTx, fun(JTx1) ->
        couch_jobs_fdb:get_jobs(JTx1, Type, Filter)
    end),
    maps:map(fun(JobId, _) ->
        case maps:is_key(JobId, JobMap) of
            true -> maps:get(JobId, JobMap);
            false -> {null, not_found, not_found}
        end
    end, InactiveIdMap);

get_jobs(#st{jtx = JTx, type = Type}, InactiveIdMap, _) ->
    couch_jobs_fdb:tx(JTx, fun(JTx1) ->
        maps:map(fun(JobId, _) ->
            Job = #{job => true, type => Type, id => JobId},
            case couch_jobs_fdb:get_job_state_and_data(JTx1, Job) of
                {ok, Seq, State, Data} ->
                    {Seq, State, Data};
                {error, not_found} ->
                    {null, not_found, not_found}
            end
        end, InactiveIdMap)
    end).


get_type_vs(#st{jtx = JTx, type = Type}) ->
    couch_jobs_fdb:tx(JTx, fun(JTx1) ->
        couch_jobs_fdb:get_activity_vs(JTx1, Type)
    end).


% "Active since" is the set of jobs that have been active (running)
% and updated at least once since the given versionstamp. These are relatively
% cheap to find as it's just a range read in the ?ACTIVITY subspace.
%
get_active_since(#st{} = _St, not_found) ->
    #{};

get_active_since(#st{jtx = JTx, type = Type, subs = Subs}, VS) ->
    AllUpdated = couch_jobs_fdb:tx(JTx, fun(JTx1) ->
        couch_jobs_fdb:get_active_since(JTx1, Type, VS)
    end),
    maps:map(fun(_JobId, Data) ->
        {VS, running, Data}
    end, maps:with(maps:keys(Subs), AllUpdated)).


notify_subscribers(_, #st{subs = Subs} = St) when map_size(Subs) =:= 0 ->
    St;

notify_subscribers(ActiveVS, #st{} = St1) ->
    % First gather the easy (cheap) active jobs. Then with those out of way
    % inspect each job to get its state.
    Active = get_active_since(St1, ActiveVS),
    St2 = notify_job_ids(Active, St1),
    ActiveIds = maps:keys(Active),
    Subs = St2#st.subs,
    InactiveIdMap = maps:without(ActiveIds, Subs),
    InactiveRatio = maps:size(InactiveIdMap) / maps:size(Subs),
    Inactive = get_jobs(St2, InactiveIdMap, InactiveRatio),
    notify_job_ids(Inactive, St2).


notify_job_ids(#{} = Jobs, #st{type = Type} = St0) ->
    maps:fold(fun(Id, {VS, State, Data}, #st{} = StAcc) ->
        DoUnsub = lists:member(State, [finished, not_found]),
        maps:fold(fun
            (_Ref, {_Pid, running, OldVS}, St) when State =:= running,
                    OldVS >= VS ->
                St;
            (Ref, {Pid, running, OldVS}, St) when State =:= running,
                    OldVS < VS ->
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
        end, StAcc, maps:get(Id, StAcc#st.subs, #{}))
    end, St0, Jobs).


notify(Pid, Ref, Type, Id, State, Data) ->
    Pid ! {?COUCH_JOBS_EVENT, Ref, Type, Id, State, Data}.


get_holdoff() ->
    config:get_integer("couch_jobs", "type_monitor_holdoff_msec",
        ?TYPE_MONITOR_HOLDOFF_DEFAULT).


get_timeout() ->
    Default =  ?TYPE_MONITOR_TIMEOUT_DEFAULT,
    case config:get("couch_jobs", "type_monitor_timeout_msec", Default) of
        "infinity" -> infinity;
        Milliseconds -> list_to_integer(Milliseconds)
    end.
