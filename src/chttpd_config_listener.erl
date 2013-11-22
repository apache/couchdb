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
-behaviour(gen_server).
-vsn(1).
-behaviour(config_listener).

% public interface
-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    code_change/3, terminate/2]).

% config_listener callback
-export([handle_config_change/5]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Settings = [
        {bind_address, config:get("chttpd", "bind_address")},
        {port, config:get("chttpd", "port")},
        {backlog, config:get("chttpd", "backlog")},
        {server_options, config:get("chttpd", "server_options")}
    ],
    ok = config:listen_for_changes(?MODULE, Settings),
    {ok, Settings}.

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

handle_call(_, _, State) ->
    {reply, ignored, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({gen_event_EXIT, {config_listener, ?MODULE}, _Reason}, State) ->
    erlang:send_after(5000, self(), restart_config_listener),
    {noreply, State};
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, State),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

% private
maybe_replace(Key, Value, Settings) ->
    case couch_util:get_value(Key, Settings) of
    Value ->
        {ok, Settings};
    _ ->
        chttpd:stop(),
        {ok, lists:keyreplace(Key, 1, {Key, Value}, Settings)}
    end.
