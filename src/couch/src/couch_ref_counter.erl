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

-module(couch_ref_counter).
-behaviour(gen_server).

-export([start/1, init/1, terminate/2, handle_call/3, handle_cast/2, code_change/3, handle_info/2]).
-export([drop/1,drop/2,add/1,add/2,count/1]).

start(ChildProcs) ->
    gen_server:start(couch_ref_counter, {self(), ChildProcs}, []).


drop(RefCounterPid) ->
    drop(RefCounterPid, self()).

drop(RefCounterPid, Pid) ->
    gen_server:call(RefCounterPid, {drop, Pid}, infinity).


add(RefCounterPid) ->
    add(RefCounterPid, self()).

add(RefCounterPid, Pid) ->
    gen_server:call(RefCounterPid, {add, Pid}, infinity).

count(RefCounterPid) ->
    gen_server:call(RefCounterPid, count).

% server functions

-record(srv,
    {
    referrers=dict:new(), % a dict of each ref counting proc.
    child_procs=[]
    }).

init({Pid, ChildProcs}) ->
    [link(ChildProc) || ChildProc <- ChildProcs],
    Referrers = dict:from_list([{Pid, {erlang:monitor(process, Pid), 1}}]),
    {ok, #srv{referrers=Referrers, child_procs=ChildProcs}}.


terminate(_Reason, #srv{child_procs=ChildProcs}) ->
    [couch_util:shutdown_sync(Pid) || Pid <- ChildProcs],
    ok.


handle_call({add, Pid},_From, #srv{referrers=Referrers}=Srv) ->
    Referrers2 =
    case dict:find(Pid, Referrers) of
    error ->
        dict:store(Pid, {erlang:monitor(process, Pid), 1}, Referrers);
    {ok, {MonRef, RefCnt}} ->
        dict:store(Pid, {MonRef, RefCnt + 1}, Referrers)
    end,
    {reply, ok, Srv#srv{referrers=Referrers2}};
handle_call(count, _From, Srv) ->
    {monitors, Monitors} =  process_info(self(), monitors),
    {reply, length(Monitors), Srv};
handle_call({drop, Pid}, _From, #srv{referrers=Referrers}=Srv) ->
    Referrers2 =
    case dict:find(Pid, Referrers) of
    {ok, {MonRef, 1}} ->
        erlang:demonitor(MonRef, [flush]),
        dict:erase(Pid, Referrers);
    {ok, {MonRef, Num}} ->
        dict:store(Pid, {MonRef, Num-1}, Referrers);
    error ->
        Referrers
    end,
    Srv2 = Srv#srv{referrers=Referrers2},
    case should_close() of
    true ->
        {stop,normal,ok,Srv2};
    false ->
        {reply, ok, Srv2}
    end.

handle_cast(Msg, _Srv)->
    exit({unknown_msg,Msg}).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({'DOWN', MonRef, _, Pid, _}, #srv{referrers=Referrers}=Srv) ->
    {ok, {MonRef, _RefCount}} = dict:find(Pid, Referrers),
    Srv2 = Srv#srv{referrers=dict:erase(Pid, Referrers)},
    case should_close() of
    true ->
        {stop,normal,Srv2};
    false ->
        {noreply,Srv2}
    end.


should_close() ->
    case process_info(self(), monitors) of
    {monitors, []} ->   true;
    _ ->                false
    end.
