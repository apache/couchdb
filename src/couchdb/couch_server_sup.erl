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

-module(couch_server_sup).
-behaviour(supervisor).


-export([start_link/1,stop/0,couch_config_start_link_wrapper/2,start_primary_services/0]).

-include("couch_db.hrl").

%% supervisor callbacks
-export([init/1]).

start_link(IniFiles) ->
    case whereis(couch_server_sup) of
    undefined ->
        start_server(IniFiles);
    _Else ->
        {error, already_started}
    end.

couch_config_start_link_wrapper(IniFiles, FirstConfigPid) ->
    case is_process_alive(FirstConfigPid) of
        true ->
            link(FirstConfigPid),
            {ok, FirstConfigPid};
        false -> couch_config:start_link(IniFiles)
    end.

start_server(IniFiles) ->
    case init:get_argument(pidfile) of
    {ok, [PidFile]} ->
        case file:write_file(PidFile, os:getpid()) of
        ok -> ok;
        Error -> io:format("Failed to write PID file ~s, error: ~p", [PidFile, Error])
        end;
    _ -> ok
    end,
    
    {ok, ConfigPid} = couch_config:start_link(IniFiles),
    
    LogLevel = couch_config:get("log", "level", "info"),
    % announce startup
    io:format("Apache CouchDB ~s (LogLevel=~s) is starting.~n", [
        couch_server:get_version(),
        LogLevel
    ]),
    case LogLevel of
    "debug" ->
        io:format("Configuration Settings ~p:~n", [IniFiles]),
        [io:format("  [~s] ~s=~p~n", [Module, Variable, Value])
            || {{Module, Variable}, Value} <- couch_config:all()];
    _ -> ok
    end,
    
    LibDir =
    case couch_config:get("couchdb", "util_driver_dir", null) of
    null ->
        filename:join(code:priv_dir(couch), "lib");
    LibDir0 -> LibDir0
    end,
    
    ok = couch_util:start_driver(LibDir),
    
    BaseServices =
    {{one_for_all, 10, 3600}, 
        [{couch_config,
            {couch_server_sup, couch_config_start_link_wrapper, [IniFiles, ConfigPid]},
            permanent,
            brutal_kill,
            worker,
            dynamic},
        {couch_primary_services,
            {couch_server_sup, start_primary_services, []},
            permanent,
            infinity,
            supervisor,
            [couch_server_sup]}
        ]},
   

    % ensure these applications are running
    application:start(inets),
    application:start(crypto),

    {ok, Pid} = supervisor:start_link(
        {local, couch_server_sup}, couch_server_sup, BaseServices),

    % launch the icu bridge
    % just restart if one of the config settings change.

    couch_config:register(
        fun("couchdb", "util_driver_dir") ->
            ?MODULE:stop()
        end, Pid),
    
    unlink(ConfigPid),
    
    io:format("Apache CouchDB has started. Time to relax.~n"),
    
    {ok, Pid}.

start_primary_services() ->
    supervisor:start_link(couch_server_sup,
        {{one_for_one, 10, 3600}, 
            [{couch_log,
                {couch_log, start_link, []},
                permanent,
                brutal_kill,
                worker,
                [couch_log]},
            {couch_db_update_event,
                {gen_event, start_link, [{local, couch_db_update}]},
                permanent,
                brutal_kill,
                supervisor,
                dynamic},
            {couch_server,
                {couch_server, sup_start_link, []},
                permanent,
                brutal_kill,
                supervisor,
                [couch_server]},
            {couch_query_servers,
                {couch_query_servers, start_link, []},
                permanent,
                brutal_kill,
                supervisor,
                [couch_query_servers]},
            {couch_view,
                {couch_view, start_link, []},
                permanent,
                brutal_kill,
                supervisor,
                [couch_view]},
            {couch_httpd,
                {couch_httpd, start_link, []},
                permanent,
                brutal_kill,
                supervisor,
                [couch_httpd]},
            {couch_ft_query,
                {couch_ft_query, start_link, []},
                permanent,
                brutal_kill,
                supervisor,
                [couch_ft_query]},
            {couch_db_update_notifier_sup,
                {couch_db_update_notifier_sup, start_link, []},
                permanent,
                brutal_kill,
                supervisor,
                [couch_db_update_notifier_sup]}]}).

stop() ->
    catch exit(whereis(couch_server_sup), normal).

init(ChildSpecs) ->
    {ok, ChildSpecs}.
