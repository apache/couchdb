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

-module(couch_external_server).
-behaviour(gen_server).
-vsn(3).

-export([start_link/2, stop/1, execute/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-include_lib("couch/include/couch_db.hrl").

-define(RELISTEN_DELAY, 5000).
-define(CONFIG_SUBSCRIPTION, [{"couchdb", "os_process_timeout"}]).

% External API

start_link(Name, Command) ->
    gen_server:start_link(couch_external_server, [Name, Command], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

execute(Pid, JsonReq) ->
    {json, Json} = gen_server:call(Pid, {execute, JsonReq}, infinity),
    ?JSON_DECODE(Json).

% Gen Server Handlers

init([Name, Command]) ->
    couch_log:info("EXTERNAL: Starting process for: ~s", [Name]),
    couch_log:info("COMMAND: ~s", [Command]),
    ok = config:subscribe_for_changes(?CONFIG_SUBSCRIPTION),
    process_flag(trap_exit, true),
    Timeout = list_to_integer(config:get("couchdb", "os_process_timeout",
        "5000")),
    {ok, Pid} = couch_os_process:start_link(Command, [{timeout, Timeout}]),
    {ok, {Name, Command, Pid, whereis(config_event)}}.

terminate(_Reason, {_Name, _Command, Pid, _}) ->
    couch_os_process:stop(Pid),
    ok.

handle_call({execute, JsonReq}, _From, {Name, Command, Pid, _}) ->
    {reply, couch_os_process:prompt(Pid, JsonReq), {Name, Command, Pid}}.

handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, {Name, Command, Pid, _}) ->
    couch_log:info("EXTERNAL: Process for ~s exiting. (reason: ~w)",
                   [Name, Reason]),
    {stop, Reason, {Name, Command, Pid}};
handle_info({config_change, "couchdb", "os_process_timeout", NewTimeout, _},
        {_Name, _Command, Pid, _} = State) ->
    couch_os_process:set_timeout(Pid, list_to_integer(NewTimeout)),
    {noreply, State};
handle_info({gen_event_EXIT, _Handler, _Reason}, State) ->
    erlang:send_after(?RELISTEN_DELAY, self(), restart_config_listener),
    {noreply, State};
handle_info({'EXIT', Pid, _Reason}, {_, _, _, Pid} = State) ->
    erlang:send_after(?RELISTEN_DELAY, self(), restart_config_listener),
    {noreply, State};
handle_info(restart_config_listener, {Name, Command, Pid, _} = State) ->
    case whereis(config_event) of
        undefined ->
            erlang:send_after(?RELISTEN_DELAY, self(), restart_config_listener),
            {noreply, State};
        EventMgr ->
            ok = config:subscribe_for_changes(?CONFIG_SUBSCRIPTION),
            {noreply, {Name, Command, Pid, EventMgr}}
    end.

handle_cast(stop, {Name, _Command, Pid, _} = State) ->
    couch_log:info("EXTERNAL: Shutting down ~s", [Name]),
    exit(Pid, normal),
    {stop, normal, State};
handle_cast(_Whatever, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
