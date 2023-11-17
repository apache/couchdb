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

-module(config_notifier).

-behaviour(gen_event).

%% Public interface
-export([subscribe/1]).
-export([subscribe/2]).

%% gen_event interface
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2
]).

subscribe(Subscription) ->
    subscribe(self(), Subscription).

subscribe(Subscriber, Subscription) ->
    case lists:member(Subscriber, handlers()) of
        true ->
            ok;
        false ->
            gen_event:add_sup_handler(
                config_event, {?MODULE, Subscriber}, {Subscriber, Subscription}
            )
    end.

init({Subscriber, Subscription}) ->
    {ok, {Subscriber, Subscription}}.

handle_event({config_change, _, _, _, _} = Event, {Subscriber, Subscription}) ->
    maybe_notify(Event, Subscriber, Subscription),
    {ok, {Subscriber, Subscription}}.

handle_call(_Request, St) ->
    {ok, ignored, St}.

handle_info(_Info, St) ->
    {ok, St}.

maybe_notify(Event, Subscriber, all) ->
    Subscriber ! Event;
maybe_notify({config_change, Sec, Key, _, _} = Event, Subscriber, Subscription) ->
    case should_notify(Sec, Key, Subscription) of
        true ->
            Subscriber ! Event;
        false ->
            ok
    end.

should_notify(Sec, Key, Subscription) ->
    lists:any(fun(S) -> S =:= Sec orelse S =:= {Sec, Key} end, Subscription).

handlers() ->
    AllHandlers = gen_event:which_handlers(config_event),
    [Id || {?MODULE, Id} <- AllHandlers].
