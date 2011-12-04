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

-module(couch_replication_notifier).

-behaviour(gen_event).

% public API
-export([start_link/1, stop/1, notify/1]).

% gen_event callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_event/2, handle_call/2, handle_info/2]).

-include("couch_db.hrl").

start_link(FunAcc) ->
    couch_event_sup:start_link(couch_replication,
        {couch_replication_notifier, make_ref()}, FunAcc).

notify(Event) ->
    gen_event:notify(couch_replication, Event).

stop(Pid) ->
    couch_event_sup:stop(Pid).


init(FunAcc) ->
    {ok, FunAcc}.

terminate(_Reason, _State) ->
    ok.

handle_event(Event, Fun) when is_function(Fun, 1) ->
    Fun(Event),
    {ok, Fun};
handle_event(Event, {Fun, Acc}) when is_function(Fun, 2) ->
    Acc2 = Fun(Event, Acc),
    {ok, {Fun, Acc2}}.

handle_call(_Msg, State) ->
    {reply, ok, State}.

handle_info(_Msg, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
