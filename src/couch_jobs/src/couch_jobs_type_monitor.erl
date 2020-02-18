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

-module(couch_jobs_type_monitor).


-export([
    start/4
]).


-include("couch_jobs.hrl").


-record(st, {
    jtx,
    type,
    vs,
    parent,
    timestamp,
    holdoff,
    timeout
}).


start(Type, VS, HoldOff, Timeout) ->
    Parent = self(),
    spawn_link(fun() ->
        loop(#st{
            jtx = couch_jobs_fdb:get_jtx(),
            type = Type,
            vs = VS,
            parent = Parent,
            timestamp = 0,
            holdoff = HoldOff,
            timeout = Timeout
        })
    end).


loop(#st{vs = VS, timeout = Timeout} = St) ->
    {St1, Watch} = case get_vs_and_watch(St) of
        {VS1, W} when VS1 =/= VS -> {notify(St#st{vs = VS1}), W};
        {VS, W} -> {St, W}
    end,
    try
        erlfdb:wait(Watch, [{timeout, Timeout}])
    catch
        error:{erlfdb_error, ?FUTURE_VERSION} ->
            erlfdb:cancel(Watch, [flush]),
            ok;
        error:{timeout, _} ->
            erlfdb:cancel(Watch, [flush]),
            ok
    end,
    loop(St1).


notify(#st{} = St) ->
    #st{holdoff = HoldOff, parent = Pid, timestamp = Ts, vs = VS} = St,
    Now = erlang:system_time(millisecond),
    case Now - Ts of
        Dt when Dt < HoldOff ->
            timer:sleep(max(HoldOff - Dt, 0));
        _ ->
            ok
    end,
    Pid ! {type_updated, VS},
    St#st{timestamp = Now}.


get_vs_and_watch(#st{jtx = JTx, type = Type}) ->
    couch_jobs_fdb:tx(JTx, fun(JTx1) ->
        couch_jobs_fdb:get_activity_vs_and_watch(JTx1, Type)
    end).
