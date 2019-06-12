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

-module(couch_jobs_activity_monitor).

-behaviour(gen_server).


-export([
    start_link/1
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-record(st, {
    jtx,
    type,
    tref,
    timeout = 0,
    vs = not_found
}).


-define(MAX_JITTER_DEFAULT, 10000).
-define(MISSING_TIMEOUT_CHECK, 5000).


start_link(Type) ->
    gen_server:start_link(?MODULE, [Type], []).


%% gen_server callbacks

init([Type]) ->
    St = #st{jtx = couch_jobs_fdb:get_jtx(), type = Type},
    {ok, schedule_check(St)}.


terminate(_, _St) ->
    ok.


handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info(check_activity, St) ->
    St1 = check_activity(St),
    St2 = schedule_check(St1),
    {noreply, St2};

handle_info({Ref, ready}, St) when is_reference(Ref) ->
    % Don't crash out couch_jobs_server and the whole application would need to
    % eventually do proper cleanup in erlfdb:wait timeout code.
    LogMsg = "~p : spurious erlfdb future ready message ~p",
    couch_log:error(LogMsg, [?MODULE, Ref]),
    {noreply, St};

handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


% Private helper functions

check_activity(#st{jtx = JTx, type = Type, vs = not_found} = St) ->
    NewVS = couch_jobs_fdb:tx(JTx, fun(JTx1) ->
        couch_jobs_fdb:get_activity_vs(JTx1, Type)
    end),
    St#st{vs = NewVS};

check_activity(#st{jtx = JTx, type = Type, vs = VS} = St) ->
    NewVS = couch_jobs_fdb:tx(JTx, fun(JTx1) ->
        NewVS = couch_jobs_fdb:get_activity_vs(JTx1, Type),
        JobIds = couch_jobs_fdb:get_inactive_since(JTx1, Type, VS),
        couch_jobs_fdb:re_enqueue_inactive(JTx1, Type, JobIds),
        NewVS
    end),
    St#st{vs = NewVS}.


get_timeout_msec(JTx, Type) ->
    TimeoutVal = couch_jobs_fdb:tx(JTx, fun(JTx1) ->
        couch_jobs_fdb:get_type_timeout(JTx1, Type)
    end),
    case TimeoutVal of
        not_found -> not_found;
        ValSeconds -> timer:seconds(ValSeconds)
    end.


schedule_check(#st{jtx = JTx, type = Type, timeout = OldTimeout} = St) ->
    % Reset versionstamp if timeout changed.
    St1 = case get_timeout_msec(JTx, Type) of
        not_found ->
            St#st{vs = not_found, timeout = ?MISSING_TIMEOUT_CHECK};
        OldTimeout ->
            St;
        NewTimeout ->
            St#st{vs = not_found, timeout = NewTimeout}
    end,
    #st{timeout = Timeout} = St1,
    MaxJitter = min(Timeout div 2, get_max_jitter_msec()),
    Wait = Timeout + rand:uniform(max(1, MaxJitter)),
    St1#st{tref = erlang:send_after(Wait, self(), check_activity)}.


get_max_jitter_msec()->
    config:get_integer("couch_jobs", "activity_monitor_max_jitter_msec",
        ?MAX_JITTER_DEFAULT).
