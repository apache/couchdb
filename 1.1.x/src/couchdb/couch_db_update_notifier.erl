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

%
% This causes an OS process to spawned and it is notified every time a database
% is updated.
%
% The notifications are in the form of a the database name sent as a line of
% text to the OS processes stdout.
%

-module(couch_db_update_notifier).

-behaviour(gen_event).

-export([start_link/1, notify/1]).
-export([init/1, terminate/2, handle_event/2, handle_call/2, handle_info/2, code_change/3,stop/1]).

-include("couch_db.hrl").

start_link(Exec) ->
    couch_event_sup:start_link(couch_db_update, {couch_db_update_notifier, make_ref()}, Exec).

notify(Event) ->
    gen_event:notify(couch_db_update, Event).

stop(Pid) ->
    couch_event_sup:stop(Pid).

init(Exec) when is_list(Exec) -> % an exe
    couch_os_process:start_link(Exec, []);
init(Else) ->
    {ok, Else}.

terminate(_Reason, Pid) when is_pid(Pid) ->
    couch_os_process:stop(Pid),
    ok;
terminate(_Reason, _State) ->
    ok.

handle_event(Event, Fun) when is_function(Fun, 1) ->
    Fun(Event),
    {ok, Fun};
handle_event(Event, {Fun, FunAcc}) ->
    FunAcc2 = Fun(Event, FunAcc),
    {ok, {Fun, FunAcc2}};
handle_event({EventAtom, DbName}, Pid) ->
    Obj = {[{type, list_to_binary(atom_to_list(EventAtom))}, {db, DbName}]},
    ok = couch_os_process:send(Pid, Obj),
    {ok, Pid}.

handle_call(_Request, State) ->
    {reply, ok, State}.

handle_info({'EXIT', Pid, Reason}, Pid) ->
    ?LOG_ERROR("Update notification process ~p died: ~p", [Pid, Reason]),
    remove_handler;
handle_info({'EXIT', _, _}, Pid) ->
    %% the db_update event manager traps exits and forwards this message to all
    %% its handlers. Just ignore as it wasn't our os_process that exited.
    {ok, Pid}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
