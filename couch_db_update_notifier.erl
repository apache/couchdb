% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
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

-define(ERR_HANDLE, {Port, {exit_status, Status}} -> {stop, {unknown_error, Status}, {unknown_error, Status}, Port}).

start_link(Exec) ->
    couch_event_sup:start_link(couch_db_update, {couch_db_update_notifier, make_ref()}, Exec).

notify(Event) ->
    gen_event:notify(couch_db_update, Event).

stop(Pid) ->
    couch_event_sup:stop(Pid).

init(Exec) when is_list(Exec) -> % an exe
    Port = open_port({spawn, Exec}, [stream, exit_status, hide]),
    {ok, Port};
init(Else) ->
    {ok, Else}.

terminate(_Reason, _Port) ->
    ok.

handle_event(Event, Fun) when is_function(Fun, 1) ->
    Fun(Event),
    {ok, Fun};
handle_event(Event, {Fun, FunAcc}) ->
    FunAcc2 = Fun(Event, FunAcc),
    {ok, {Fun, FunAcc2}};
handle_event({EventAtom, DbName}, Port) ->
    Obj = {obj, [{type, atom_to_list(EventAtom)}, {db, DbName}]},
    true = port_command(Port, cjson:encode(Obj) ++ "\n"),
    {ok, Port}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info({'EXIT', _, _Reason}, _Port) ->
    remove_handler.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
