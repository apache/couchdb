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

%% The purpose of this module is to allow event handlers to particpate in Erlang
%% supervisor trees. It provide a monitorable process that crashes if the event
%% handler fails. The process, when shutdown, deregisters the event handler.

-module(couch_event_sup).
-behaviour(gen_server).

-include("couch_db.hrl").

-export([start_link/3,start_link/4, stop/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,code_change/3]).

%
% Instead calling the
% ok = gen_event:add_sup_handler(error_logger, my_log, Args)
%
% do this:
% {ok, LinkedPid} = couch_event_sup:start_link(error_logger, my_log, Args)
%
% The benefit is the event is now part of the process tree, and can be
% started, restarted and shutdown consistently like the rest of the server
% components.
%
% And now if the "event" crashes, the supervisor is notified and can restart
% the event handler.
%
% Use this form to named process:
% {ok, LinkedPid} = couch_event_sup:start_link({local, my_log}, error_logger, my_log, Args)
%

start_link(EventMgr, EventHandler, Args) ->
    gen_server:start_link(couch_event_sup, {EventMgr, EventHandler, Args}, []).

start_link(ServerName, EventMgr, EventHandler, Args) ->
    gen_server:start_link(ServerName, couch_event_sup, {EventMgr, EventHandler, Args}, []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

init({EventMgr, EventHandler, Args}) ->
    case gen_event:add_sup_handler(EventMgr, EventHandler, Args) of
    ok ->
        {ok, {EventMgr, EventHandler}};
    {stop, Error} ->
        {stop, Error}
    end.

terminate(_Reason, _State) ->
    ok.

handle_call(_Whatever, _From, State) ->
    {ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({gen_event_EXIT, _Handler, Reason}, State) ->
    {stop, Reason, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
