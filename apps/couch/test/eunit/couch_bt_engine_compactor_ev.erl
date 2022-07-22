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

-module(couch_bt_engine_compactor_ev).

-export([
    init/0,
    terminate/0,
    clear/0,

    set_wait/1,
    set_crash/1,

    event/1
]).

-define(TAB, couch_db_updater_ev_tab).

init() ->
    ets:new(?TAB, [set, public, named_table]).

terminate() ->
    ets:delete(?TAB).

clear() ->
    ets:delete_all_objects(?TAB).

set_wait(Event) ->
    Self = self(),
    WaitFun = fun(_) ->
        receive
            {Self, go} ->
                Self ! {self(), ok}
        end,
        ets:delete(?TAB, Event)
    end,
    ContinueFun = fun(Pid) ->
        Pid ! {Self, go},
        receive
            {Pid, ok} -> ok
        end
    end,
    ets:insert(?TAB, {Event, WaitFun}),
    {ok, ContinueFun}.

set_crash(Event) ->
    Reason = {couch_db_updater_ev_crash, Event},
    CrashFun = fun(_) -> exit(Reason) end,
    ets:insert(?TAB, {Event, CrashFun}),
    {ok, Reason}.

event(Event) ->
    NewEvent =
        case Event of
            seq_init ->
                put(?MODULE, 0),
                Event;
            seq_copy ->
                Count = get(?MODULE),
                put(?MODULE, Count + 1),
                {seq_copy, Count};
            id_init ->
                put(?MODULE, 0),
                Event;
            id_copy ->
                Count = get(?MODULE),
                put(?MODULE, Count + 1),
                {id_copy, Count};
            md_copy_init ->
                put(?MODULE, 0),
                Event;
            md_copy_row ->
                Count = get(?MODULE),
                put(?MODULE, Count + 1),
                {md_copy_row, Count};
            _ ->
                Event
        end,
    handle_event(NewEvent).

handle_event(Event) ->
    try
        case ets:lookup(?TAB, Event) of
            [{Event, ActionFun}] ->
                ActionFun(Event);
            [] ->
                ok
        end
    catch
        error:badarg ->
            ok
    end.
