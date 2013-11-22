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
-vsn(1).
-behaviour(config_listener).

-export([start_link/2, stop/1, execute/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

% config_listener api
-export([handle_config_change/5]).

-include_lib("couch/include/couch_db.hrl").

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
    ?LOG_INFO("EXTERNAL: Starting process for: ~s", [Name]),
    ?LOG_INFO("COMMAND: ~s", [Command]),
    process_flag(trap_exit, true),
    Timeout = list_to_integer(config:get("couchdb", "os_process_timeout",
        "5000")),
    {ok, Pid} = couch_os_process:start_link(Command, [{timeout, Timeout}]),
    ok = config:listen_for_changes(?MODULE, Pid),
    {ok, {Name, Command, Pid}}.

terminate(_Reason, {_Name, _Command, Pid}) ->
    couch_os_process:stop(Pid),
    ok.

handle_call({execute, JsonReq}, _From, {Name, Command, Pid}) ->
    {reply, couch_os_process:prompt(Pid, JsonReq), {Name, Command, Pid}}.

handle_info({gen_event_EXIT, {config_listener, ?MODULE}, _Reason}, State) ->
    erlang:send_after(5000, self(), restart_config_listener),
    {noreply, State};
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, State),
    {noreply, State};
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, {Name, Command, Pid}) ->
    ?LOG_INFO("EXTERNAL: Process for ~s exiting. (reason: ~w)", [Name, Reason]),
    {stop, Reason, {Name, Command, Pid}}.

handle_cast(stop, {Name, Command, Pid}) ->
    ?LOG_INFO("EXTERNAL: Shutting down ~s", [Name]),
    exit(Pid, normal),
    {stop, normal, {Name, Command, Pid}};
handle_cast(_Whatever, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_config_change("couchdb", "os_process_timeout", NewTimeout, _, Pid) ->
    couch_os_process:set_timeout(Pid, list_to_integer(NewTimeout)),
    {ok, Pid};
handle_config_change(_, _, _, _, Pid) ->
    {ok, Pid}.

