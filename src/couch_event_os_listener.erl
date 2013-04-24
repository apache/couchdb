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

-module(couch_event_os_listener).
-behavior(gen_server).


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


start_link(Exe) when is_list(Exe) ->
    gen_server:start_link(?MODULE, Exe, []).


init(Exe) ->
    process_flag(trap_exit, true),
    ok = couch_event:register_all(self()),
    couch_os_process:start_link(Exe, []).


terminate(_Reason, Pid) when is_pid(Pid) ->
    couch_os_process:stop(Pid);
terminate(_Reason, _Pid) ->
    ok.


handle_call(Msg, From, Pid) ->
    couch_log:notice("~s ignoring call ~w from ~w", [?MODULE, Msg, From]),
    {reply, ignored, Pid, 0}.


handle_cast(Msg, Pid) ->
    couch_log:notice("~s ignoring cast ~w", [?MODULE, Msg]),
    {noreply, Pid, 0}.


handle_info({'$couch_event', DbName, Event}, Pid) ->
    Obj = {[
        {db, DbName},
        {type, list_to_binary(atom_to_list(Event))}
    ]},
    ok = couch_os_process:send(Pid, Obj),
    {noreply, Pid};

handle_info({'EXIT', Pid, Reason}, Pid) ->
    couch_log:error("Update notificatio process ~w died: ~w", [Pid, Reason]),
    {stop, normal, nil};

handle_info(Msg, Pid) ->
    couch_log:notice("~s ignoring info ~w", [?MODULE, Msg]),
    {noreply, Pid, 0}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
