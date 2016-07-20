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
-behaviour(config_listener).


-export([
    start/0
]).

-export([
    handle_config_change/5,
    handle_config_terminate/3
]).


-ifdef(TEST).
-define(RELISTEN_DELAY, 500).
-else.
-define(RELISTEN_DELAY, 5000).
-endif.


start() ->
    ok = config:listen_for_changes(?MODULE, nil).


handle_config_change("log", Key, _, _, _) ->
    case Key of
        "level" ->
            couch_log_config:reconfigure();
        "max_message_size" ->
            couch_log_config:reconfigure();
        _ ->
            % Someone may have changed the config for
            % the writer so we need to re-initialize.
            couch_log_server:reconfigure()
    end,
    notify_listeners(),
    {ok, nil};

handle_config_change(_, _, _, _, Settings) ->
    {ok, Settings}.


handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_, _, _) ->
    spawn(fun() ->
        timer:sleep(?RELISTEN_DELAY),
        ok = config:listen_for_changes(?MODULE, nil)
    end).


-ifdef(TEST).
notify_listeners() ->
    Listeners = application:get_env(couch_log, config_listeners, []),
    lists:foreach(fun(L) ->
        L ! couch_log_config_change_finished
    end, Listeners).
-else.
notify_listeners() ->
    ok.
-endif.
