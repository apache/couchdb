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

-define(DEFAULT_INI, "couch.ini").

-export([start_link/1,stop/0]).

-include("couch_db.hrl").

%% supervisor callbacks
-export([init/1]).

start_link(IniFilename) ->
    case whereis(couch_server_sup) of
    undefined ->
        start_server(IniFilename);
    _Else ->
        {error, already_started}
    end.

start_server("") ->
        % no ini file specified, check the command line args
    IniFile =
    case init:get_argument(couchini) of
    {ok, [CmdLineIniFilename]} ->
        CmdLineIniFilename;
    _Else ->
        ?DEFAULT_INI
    end,
    start_server(IniFile);
start_server(InputIniFilename) ->

    case init:get_argument(pidfile) of
    {ok, [PidFile]} ->
        case file:write_file(PidFile, os:getpid()) of
        ok -> ok;
        Error -> io:format("Failed to write PID file ~s, error: ~p", [PidFile, Error])
        end;
    _ -> ok
    end,

    {ok, Cwd} = file:get_cwd(),
    IniFilename = couch_util:abs_pathname(InputIniFilename),
    IniBin =
    case file:read_file(IniFilename) of
    {ok, IniBin0} ->
        IniBin0;
    {error, enoent} ->
        Msg = io_lib:format("Couldn't find server configuration file ~s.", [InputIniFilename]),
        io:format("~s~n", [Msg]),
        throw({startup_error, Msg})
    end,
    {ok, Ini} = couch_util:parse_ini(binary_to_list(IniBin)),

    ConsoleStartupMsg = proplists:get_value({"Couch", "ConsoleStartupMsg"}, Ini, "Apache CouchDB is starting."),
    LogLevel = list_to_atom(proplists:get_value({"Couch", "LogLevel"}, Ini, "error")),
    DbRootDir = proplists:get_value({"Couch", "DbRootDir"}, Ini, "."),
    BindAddress = proplists:get_value({"Couch", "BindAddress"}, Ini, any),
    Port = proplists:get_value({"Couch", "Port"}, Ini, 5984),
    DocumentRoot = proplists:get_value({"Couch", "DocumentRoot"}, Ini, "share/www"),
    LogFile = proplists:get_value({"Couch", "LogFile"}, Ini, "couchdb.log"),
    UtilDriverDir = proplists:get_value({"Couch", "UtilDriverDir"}, Ini, ""),
    UpdateNotifierExes = proplists:get_all_values({"Couch", "DbUpdateNotificationProcess"}, Ini),
    FtSearchQueryServer = proplists:get_value({"Couch", "FullTextSearchQueryServer"}, Ini, ""),
    RemoteRestart = list_to_atom(proplists:get_value({"Couch", "AllowRemoteRestart"}, Ini, "undefined")),
    ServerOptions = [{remote_restart, RemoteRestart}],
    QueryServers = [{Lang, QueryExe} || {{"Couch Query Servers", Lang}, QueryExe} <- Ini],

    ChildProcesses =
        [{couch_log,
            {couch_log, start_link, [LogFile, LogLevel]},
            permanent,
            brutal_kill,
            worker,
            [couch_server]},
        {couch_db_update_event,
            {gen_event, start_link, [{local, couch_db_update}]},
            permanent,
            1000,
            supervisor,
            dynamic},
        {couch_server,
            {couch_server, sup_start_link, [DbRootDir, ServerOptions]},
            permanent,
            brutal_kill,
            worker,
            [couch_server]},
        {couch_query_servers,
            {couch_query_servers, start_link, [QueryServers]},
            permanent,
            brutal_kill,
            worker,
            [couch_query_servers]},
        {couch_view,
            {couch_view, start_link, [DbRootDir]},
            permanent,
            brutal_kill,
            worker,
            [couch_view]},
        {couch_httpd,
            {couch_httpd, start_link, [BindAddress, Port, DocumentRoot]},
            permanent,
            1000,
            supervisor,
            [couch_httpd]}
        ] ++
        lists:map(fun(UpdateNotifierExe) ->
            {UpdateNotifierExe,
                {couch_db_update_notifier, start_link, [UpdateNotifierExe]},
                permanent,
                1000,
                supervisor,
                [couch_db_update_notifier]}
            end, UpdateNotifierExes)
        ++
        case FtSearchQueryServer of
        "" ->
            [];
        _ ->
            [{couch_ft_query,
                {couch_ft_query, start_link, [FtSearchQueryServer]},
                permanent,
                1000,
                supervisor,
                [couch_ft_query]}]
        end,

    io:format("Apache CouchDB ~s (LogLevel=~s)~n", [couch_server:get_version(), LogLevel]),
    io:format("~s~n~n", [ConsoleStartupMsg]),

    couch_util:start_driver(UtilDriverDir),

    % ensure these applications are running
    application:start(inets),
    application:start(crypto),

    process_flag(trap_exit, true),
    StartResult = (catch supervisor:start_link(
        {local, couch_server_sup}, couch_server_sup, ChildProcesses)),

    ConfigInfo = io_lib:format("Config Info ~s:~n\tCurrentWorkingDir=~s~n" ++
        "\tDbRootDir=~s~n" ++
        "\tBindAddress=~p~n" ++
        "\tPort=~p~n" ++
        "\tDocumentRoot=~s~n" ++
        "\tLogFile=~s~n" ++
        "\tUtilDriverDir=~s~n" ++
        "\tDbUpdateNotificationProcesses=~s~n" ++
        "\tFullTextSearchQueryServer=~s~n" ++
        "~s",
            [IniFilename,
            Cwd,
            DbRootDir,
            BindAddress,
            Port,
            DocumentRoot,
            LogFile,
            UtilDriverDir,
            UpdateNotifierExes,
            FtSearchQueryServer,
            [lists:flatten(io_lib:format("\t~s=~s~n", [Lang, QueryExe])) || {Lang, QueryExe} <- QueryServers]]),
    ?LOG_DEBUG("~s", [ConfigInfo]),

    case StartResult of
    {ok,_} ->
        % only output when startup was successful
        %io:format("Find Futon, the management interface, at:~nhttp://~s:~s/_utils/index.html~n~n", [BindAddress, Port]),
        io:format("Apache CouchDB has started. Time to relax.~n");
    _ ->
        % Since we failed startup, unconditionally dump configuration data to console
        io:format("~s", [ConfigInfo]),
        ok
    end,
    process_flag(trap_exit, false),
    StartResult.

stop() ->
    catch exit(whereis(couch_server_sup), normal),
    couch_log:stop().

init(ChildProcesses) ->
    {ok, {{one_for_one, 10, 3600}, ChildProcesses}}.
