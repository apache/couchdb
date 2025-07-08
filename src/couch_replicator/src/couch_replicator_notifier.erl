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

-module(couch_replicator_notifier).

-behaviour(gen_event).

-define(NAME, couch_replication).

% public API
-export([start_link/1, stop/1, notify/1]).

% gen_event callbacks
-export([init/1]).
-export([handle_event/2, handle_call/2, handle_info/2]).

start_link(FunAcc) ->
    couch_event_sup:start_link(?NAME, {?MODULE, make_ref()}, FunAcc).

notify(Event) ->
    try
        gen_event:notify(?NAME, Event)
    catch
        _:_ ->
            % It's possible some jobs may remain around after the notification
            % service had shut down or crashed. Avoid making a mess in the logs
            % and just ignore that. At that point nobody will notice the
            % notification anyway.
            ok
    end.

stop(Pid) ->
    couch_event_sup:stop(Pid).

init(FunAcc) ->
    {ok, FunAcc}.

handle_event(Event, Fun) when is_function(Fun, 1) ->
    Fun(Event),
    {ok, Fun};
handle_event(Event, {Fun, Acc}) when is_function(Fun, 2) ->
    Acc2 = Fun(Event, Acc),
    {ok, {Fun, Acc2}}.

handle_call(_Msg, State) ->
    {ok, ok, State}.

handle_info(_Msg, State) ->
    {ok, State}.

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

couch_replicator_notify_when_stopped_test() ->
    ?assertEqual(ok, notify({stopped, foo})).

-endif.
