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

-module(couch_event_listener_mfa).
-behavior(couch_event_listener).


-export([
    start_link/4,
    enter_loop/4,
    stop/1,
]).

-export([
    init/1,
    terminate/2,
    handle_event/3,
    handle_cast/2,
    handle_info/2
]).


-record(st, {
    mod,
    func,
    state,
    parent
}).


start_link(Mod, Func, State, Options) ->
    Arg = {self(), Mod, Func, State},
    couch_event_listener:start_link(?MODULE, Arg, Options).


enter_loop(Mod, Func, State, Options) ->
    St = #st{
        mod = Mod,
        func = Func,
        state = State
    },
    couch_event_listener:enter_loop(?MODULE, St, Options).


stop(Pid) ->
    couch_event_listener:stop(Pid).


init({Parent, Mod, Func, State}) ->
    erlang:monitor(process, Parent),
    {ok, #st{
        mod = Mod,
        func = Func,
        state = State,
        parent = Parent
    }}.


terminate(_Reason, _MFA) ->
    ok.


handle_event(DbName, Event, #st{mod=Mod, func=Func, state=State}=St) ->
    case (catch Mod:Func(DbName, Event, State)) of
        {ok, NewSt} ->
            {ok, St#st{state=NewState}};
        stop ->
            {stop, normal, St};
        Else ->
            erlang:error(Else)
    end.


handle_cast(shutdown, St) ->
    {stop, normal, St};

handle_cast(_Msg, St) ->
    {ok, St}.


handle_info({'DOWN', _Ref, process, Parent, _Reason}, #st{parent=Parent}=St) ->
    {stop, normal, St};

handle_info(_Msg, St) ->
    {ok, St}.

