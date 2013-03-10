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

-module(couch_server_sup).
-behaviour(supervisor).
-behaviour(config_listener).


-export([start_link/1,stop/0, restart_core_server/0]).

-include_lib("couch/include/couch_db.hrl").

%% supervisor callbacks
-export([init/1]).

% config_listener api
-export([handle_config_change/5]).

start_link(IniFiles) ->
    case whereis(couch_server_sup) of
    undefined ->
        start_server(IniFiles);
    _Else ->
        {error, already_started}
    end.

restart_core_server() ->
    init:restart().

start_server(IniFiles) ->
    case init:get_argument(pidfile) of
    {ok, [PidFile]} ->
        case file:write_file(PidFile, os:getpid()) of
        ok -> ok;
        {error, Reason} ->
            io:format("Failed to write PID file ~s: ~s",
                [PidFile, file:format_error(Reason)])
        end;
    _ -> ok
    end,

    LogLevel = config:get("log", "level", "info"),
    % announce startup
    io:format("Apache CouchDB ~s (LogLevel=~s) is starting.~n", [
        couch_server:get_version(),
        LogLevel
    ]),
    case LogLevel of
    "debug" ->
        io:format("Configuration Settings ~p:~n", [IniFiles]),
        [io:format("  [~s] ~s=~p~n", [Module, Variable, Value])
            || {{Module, Variable}, Value} <- config:all()];
    _ -> ok
    end,

    BaseChildSpecs =
    {{one_for_one, 10, 60}, [
        {couch_primary_services,
            {couch_primary_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            [couch_primary_sup]},
        {couch_secondary_services,
            {couch_secondary_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            [couch_secondary_sup]}
        ]},

    % ensure these applications are running
    application:start(ibrowse),
    application:start(crypto),

    {ok, Pid} = supervisor:start_link(
        {local, couch_server_sup}, couch_server_sup, BaseChildSpecs),

    ok = config:listen_for_changes(?MODULE, nil),

    Ip = config:get("httpd", "bind_address"),
    io:format("Apache CouchDB has started. Time to relax.~n"),
    Uris = [get_uri(Name, Ip) || Name <- [couch_httpd, https]],
    [begin
        case Uri of
            undefined -> ok;
            Uri -> ?LOG_INFO("Apache CouchDB has started on ~s", [Uri])
        end
    end
    || Uri <- Uris],
    case config:get("couchdb", "uri_file", null) of 
    null -> ok;
    UriFile ->
        Lines = [begin case Uri of
            undefined -> [];
            Uri -> io_lib:format("~s~n", [Uri])
            end end || Uri <- Uris],
        case file:write_file(UriFile, Lines) of
        ok -> ok;
        {error, Reason2} = Error ->
            ?LOG_ERROR("Failed to write to URI file ~s: ~s",
                [UriFile, file:format_error(Reason2)]),
            throw(Error)
        end
    end,

    {ok, Pid}.

stop() ->
    catch exit(whereis(couch_server_sup), normal).


handle_config_change("daemons", _, _, _, _) ->
    exit(whereis(couch_server_sup), shutdown),
    remove_handler;
handle_config_change("couchdb", "util_driver_dir", _, _, _) ->
    [Pid] = [P || {collation_driver, P, _, _}
        <- supervisor:which_children(couch_primary_services)],
    Pid ! reload_driver,
    {ok, nil};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

init(ChildSpecs) ->
    {ok, ChildSpecs}.

get_uri(Name, Ip) ->
    case get_port(Name) of
        undefined ->
            undefined;
        Port ->
            io_lib:format("~s://~s:~w/", [get_scheme(Name), Ip, Port])
    end.

get_scheme(couch_httpd) -> "http";
get_scheme(https) -> "https".

get_port(Name) ->
    try
        mochiweb_socket_server:get(Name, port)
    catch
        exit:{noproc, _}->
            undefined
    end.
