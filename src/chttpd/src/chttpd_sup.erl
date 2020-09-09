% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(chttpd_sup).
-behaviour(supervisor).
-vsn(1).

-behaviour(config_listener).

-export([init/1]).

-export([start_link/0]).

-export([handle_config_change/5, handle_config_terminate/3]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 100, Type, [I]}).

start_link() ->
    Arg = case fabric2_node_types:is_type(api_frontend) of
        true -> normal;
        false -> disabled
    end,
    supervisor:start_link({local,?MODULE}, ?MODULE, Arg).

init(disabled) ->
    couch_log:notice("~p : api_frontend disabled", [?MODULE]),
    {ok, {{one_for_one, 3, 10}, []}};

init(normal) ->
    Children = [
        {
            config_listener_mon,
            {config_listener_mon, start_link, [?MODULE, settings()]},
            permanent,
            5000,
            worker,
            [config_listener_mon]
        },
        ?CHILD(chttpd, worker),
        ?CHILD(chttpd_auth_cache, worker),
        {chttpd_auth_cache_lru,
	 {ets_lru, start_link, [chttpd_auth_cache_lru, lru_opts()]},
	 permanent, 5000, worker, [ets_lru]}
    ],

    {ok, {{one_for_one, 3, 10},
        couch_epi:register_service(chttpd_epi, Children)}}.

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

handle_config_terminate(_Server, _Reason, _State) ->
    ok.

settings() ->
    [
        {bind_address, config:get("chttpd", "bind_address")},
        {port, config:get("chttpd", "port")},
        {backlog, config:get("chttpd", "backlog")},
        {server_options, config:get("chttpd", "server_options")}
    ].

maybe_replace(Key, Value, Settings) ->
    case couch_util:get_value(Key, Settings) of
    Value ->
        {ok, Settings};
    _ ->
        chttpd:stop(),
        {ok, lists:keyreplace(Key, 1, Settings, {Key, Value})}
    end.

lru_opts() ->
    lists:foldl(fun append_if_set/2, [], [
        {max_objects, config:get_integer("chttpd_auth_cache", "max_objects", 0)},
        {max_size, config:get_integer("chttpd_auth_cache", "max_size", 104857600)},
        {max_lifetime, config:get_integer("chttpd_auth_cache", "max_lifetime", 600000)}
    ]).

append_if_set({Key, Value}, Opts) when Value > 0 ->
    [{Key, Value} | Opts];
append_if_set({_Key, 0}, Opts) ->
    Opts;
append_if_set({Key, Value}, Opts) ->
    couch_log:error(
        "The value for `~s` should be string convertable "
        "to integer which is >= 0 (got `~p`)", [Key, Value]),
    Opts.
