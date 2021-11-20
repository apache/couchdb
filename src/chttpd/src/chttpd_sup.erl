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

-export([start_link/1]).

-export([handle_config_change/5, handle_config_terminate/3]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 100, Type, [I]}).
-define(DEFAULT_BACKLOG, 512).
-define(DEFAULT_SERVER_OPTIONS, "[{recbuf, undefined}]").

start_link(Args) ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, Args) of
        {ok, _} = Resp ->
            notify_started(),
            notify_uris(),
            write_uris(),
            Resp;
        Else ->
            notify_error(Else),
            Else
    end.

init([]) ->
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
        {chttpd_auth_cache_lru, {ets_lru, start_link, [chttpd_auth_cache_lru, lru_opts()]},
            permanent, 5000, worker, [ets_lru]}
    ],

    {ok, {{one_for_one, 3, 10}, couch_epi:register_service(chttpd_epi, Children)}}.

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
        {backlog, config:get_integer("chttpd", "backlog", ?DEFAULT_BACKLOG)},
        {server_options,
            config:get(
                "chttpd",
                "server_options",
                ?DEFAULT_SERVER_OPTIONS
            )}
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
        "to integer which is >= 0 (got `~p`)",
        [Key, Value]
    ),
    Opts.

notify_started() ->
    couch_log:info("Apache CouchDB has started. Time to relax.~n", []).

notify_error(Error) ->
    couch_log:error("Error starting Apache CouchDB:~n~n    ~p~n~n", [Error]).

notify_uris() ->
    lists:foreach(
        fun(Uri) ->
            couch_log:info("Apache CouchDB has started on ~s", [Uri])
        end,
        get_uris()
    ).

write_uris() ->
    case config:get("couchdb", "uri_file", undefined) of
        undefined ->
            ok;
        UriFile ->
            Lines = [io_lib:format("~s~n", [Uri]) || Uri <- get_uris()],
            write_file(UriFile, Lines)
    end.

get_uris() ->
    Ip = config:get("chttpd", "bind_address"),
    lists:flatmap(
        fun(Uri) ->
            case get_uri(Uri, Ip) of
                undefined -> [];
                Else -> [Else]
            end
        end,
        [chttpd, couch_httpd, https]
    ).

get_uri(Name, Ip) ->
    case get_port(Name) of
        undefined ->
            undefined;
        Port ->
            io_lib:format("~s://~s:~w/", [get_scheme(Name), Ip, Port])
    end.

get_scheme(chttpd) -> "http";
get_scheme(couch_httpd) -> "http";
get_scheme(https) -> "https".

get_port(Name) ->
    try
        mochiweb_socket_server:get(Name, port)
    catch
        exit:{noproc, _} ->
            undefined
    end.

write_file(FileName, Contents) ->
    case file:write_file(FileName, Contents) of
        ok ->
            ok;
        {error, Reason} ->
            Args = [FileName, file:format_error(Reason)],
            couch_log:error("Failed ot write ~s :: ~s", Args),
            throw({error, Reason})
    end.
