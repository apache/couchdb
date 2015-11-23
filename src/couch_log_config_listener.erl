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

-module(couch_log_config_listener).
-vsn(2).
-behaviour(config_listener).

% public interface
-export([subscribe/0]).

% config_listener callback
-export([handle_config_change/5, handle_config_terminate/3]).

subscribe() ->
    Settings = [
        {backend, config:get("log", "backend", "stderr")},
        {level, config:get("log", "level", "notice")}
    ],
    ok = config:listen_for_changes(?MODULE, Settings),
    ok.

handle_config_change("log", "backend", Value, _, Settings) ->
    {level, Level} = lists:keyfind(level, 1, Settings),
    couch_log:set_level(Level),
    {ok, lists:keyreplace(backend, 1, Settings, {backend, Value})};
handle_config_change("log", "level", Value, _, Settings) ->
    couch_log:set_level(Value),
    {ok, lists:keyreplace(level, 1, Settings, {level, Value})};
handle_config_change(_, _, _, _, Settings) ->
    {ok, Settings}.

handle_config_terminate(_, stop, _) -> ok;
handle_config_terminate(_Server, _Reason, State) ->
    spawn(fun() ->
        timer:sleep(5000),
        config:listen_for_changes(?MODULE, State)
    end).
