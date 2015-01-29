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

-module(chttpd_config_listener).
-vsn(2).
-behaviour(config_listener).

% public interface
-export([subscribe/0]).

% config_listener callback
-export([handle_config_change/5, handle_config_terminate/3]).

subscribe() ->
    Settings = [
        {bind_address, config:get("chttpd", "bind_address")},
        {port, config:get("chttpd", "port")},
        {backlog, config:get("chttpd", "backlog")},
        {server_options, config:get("chttpd", "server_options")}
    ],
    ok = config:listen_for_changes(?MODULE, Settings),
    ok.

handle_config_change("chttpd", "bind_address", Value, _, Settings) ->
    maybe_replace(bind_address, Value, Settings);
handle_config_change("chttpd", "port", Value, _, Settings) ->
    maybe_replace(port, Value, Settings);
handle_config_change("chttpd", "backlog", Value, _, Settings) ->
    maybe_replace(backlog, Value, Settings);
handle_config_change("chttpd", "server_options", Value, _, Settings) ->
    maybe_replace(server_options, Value, Settings);
handle_config_change(_, _, _, _, Settings) ->
    {ok, Settings}.

handle_config_terminate(_Server, _Reason, State) ->
    spawn(fun() ->
        timer:sleep(5000),
        config:listen_for_changes(?MODULE, State)
    end).

% private
maybe_replace(Key, Value, Settings) ->
    case couch_util:get_value(Key, Settings) of
    Value ->
        {ok, Settings};
    _ ->
        chttpd:stop(),
        {ok, lists:keyreplace(Key, 1, {Key, Value}, Settings)}
    end.
