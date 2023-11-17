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

-module(config_listener).

-behaviour(gen_event).

%% Public interface
-export([start/2]).
-export([start/3]).

%% Required gen_event interface
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2
]).

-callback handle_config_change(
    Sec :: string(),
    Key :: string(),
    Value :: string(),
    Persist :: boolean(),
    State :: term()
) ->
    {ok, term()} | remove_handler.

-callback handle_config_terminate(
    Subscriber :: pid(),
    Reason :: term(),
    State :: term()
) ->
    term().

start(Module, State) ->
    start(Module, Module, State).

start(Module, Id, State) ->
    gen_event:add_sup_handler(config_event, {?MODULE, Id}, {Module, State}).

init({Module, State}) ->
    {ok, {Module, State}}.

handle_event({config_change, Sec, Key, Value, Persist}, {Module, {From, State}}) ->
    case Module:handle_config_change(Sec, Key, Value, Persist, State) of
        {ok, NewState} ->
            {ok, {Module, {From, NewState}}};
        remove_handler ->
            remove_handler
    end.

handle_call(_Request, St) ->
    {ok, ignored, St}.

handle_info(_Info, St) ->
    {ok, St}.

terminate(Reason, {Module, {Subscriber, State}}) ->
    Module:handle_config_terminate(Subscriber, Reason, State).
