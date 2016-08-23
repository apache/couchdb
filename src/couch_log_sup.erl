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

-module(couch_log_sup).

-behaviour(supervisor).
-vsn(1).
-behaviour(config_listener).

-export([init/1]).
-export([start_link/0]).
-export([handle_config_change/5, handle_config_terminate/3]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    ok = couch_log_config:init(),
    {ok, {{one_for_one, 1, 1}, children()}}.


children() ->
    [
        {
            couch_log_server,
            {couch_log_server, start_link, []},
            permanent,
            5000,
            worker,
            [couch_log_server]
        },
        {
            couch_log_monitor,
            {couch_log_monitor, start_link, []},
            permanent,
            5000,
            worker,
            [couch_log_monitor]
        },
        {
            config_listener_mon,
            {config_listener_mon, start_link, [?MODULE, nil]},
            permanent,
            5000,
            worker,
            [config_listener_mon]
        }
    ].

handle_config_change("log", Key, _, _, S) ->
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
    {ok, S};

handle_config_change(_, _, _, _, S) ->
    {ok, S}.

handle_config_terminate(_Server, _Reason, _State) ->
    ok.

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
