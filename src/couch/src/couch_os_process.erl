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

-module(couch_os_process).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([set_timeout/2, prompt/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("couch/include/couch_db.hrl").

-define(PORT_OPTIONS, [stream, {line, 4096}, binary, exit_status, hide]).

-record(os_proc, {
    command,
    port,
    timeout = 5000,
    idle,
    log_level
}).

start_link(Command) ->
    gen_server:start_link(?MODULE, [Command], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

% Read/Write API
set_timeout(Pid, TimeOut) when is_integer(TimeOut) ->
    ok = gen_server:call(Pid, {set_timeout, TimeOut}, infinity).

prompt(Pid, Data) ->
    case ioq:call(Pid, {prompt, Data}, erlang:get(io_priority)) of
        {ok, Result} ->
            Result;
        Error ->
            couch_log:error("OS Process Error ~p :: ~p", [Pid, Error]),
            throw(Error)
    end.

% Utility functions for reading lines and parsing json
%

readline(#os_proc{} = OsProc) ->
    Res = readline(OsProc, []),
    Res.
readline(#os_proc{port = Port} = OsProc, Acc) ->
    receive
        {Port, {data, {noeol, Data}}} when is_binary(Acc) ->
            readline(OsProc, <<Acc/binary, Data/binary>>);
        {Port, {data, {noeol, Data}}} when is_binary(Data) ->
            readline(OsProc, Data);
        {Port, {data, {noeol, Data}}} ->
            readline(OsProc, [Data | Acc]);
        {Port, {data, {eol, <<Data/binary>>}}} when is_binary(Acc) ->
            [<<Acc/binary, Data/binary>>];
        {Port, {data, {eol, Data}}} when is_binary(Data) ->
            [Data];
        {Port, {data, {eol, Data}}} ->
            lists:reverse(Acc, Data);
        {Port, Err} ->
            catch port_close(Port),
            throw({os_process_error, Err})
    after OsProc#os_proc.timeout ->
        catch port_close(Port),
        log(OsProc, "OS Process ~p Timed out :: ~p msec", [Port, OsProc#os_proc.timeout]),
        throw({os_process_error, "OS process timed out."})
    end.

readjson(OsProc) when is_record(OsProc, os_proc) ->
    Line = iolist_to_binary(readline(OsProc)),
    log(OsProc, "OS Process ~p Output :: ~s", [OsProc#os_proc.port, Line]),
    try
        % Don't actually parse the whole JSON. Just try to see if it's
        % a command or a doc map/reduce/filter/show/list/update output.
        % If it's a command then parse the whole JSON and execute the
        % command, otherwise return the raw JSON line to the caller.
        pick_command(Line)
    catch
        throw:abort ->
            {json, Line};
        throw:{cmd, _Cmd} ->
            case ?JSON_DECODE(Line) of
                [<<"log">>, Msg] when is_binary(Msg) ->
                    % we got a message to log. Log it and continue
                    couch_log:info(
                        "OS Process ~p Log :: ~s",
                        [OsProc#os_proc.port, Msg]
                    ),
                    readjson(OsProc);
                [<<"error">>, Id, Reason] ->
                    throw({error, {couch_util:to_existing_atom(Id), Reason}});
                [<<"fatal">>, Id, Reason] ->
                    couch_log:info(
                        "OS Process ~p Fatal Error :: ~s ~p",
                        [OsProc#os_proc.port, Id, Reason]
                    ),
                    throw({couch_util:to_existing_atom(Id), Reason});
                _Result ->
                    {json, Line}
            end
    end.

pick_command(Line) ->
    json_stream_parse:events(Line, fun pick_command0/1).

pick_command0(array_start) ->
    fun pick_command1/1;
pick_command0(_) ->
    throw(abort).

pick_command1(<<"log">> = Cmd) ->
    throw({cmd, Cmd});
pick_command1(<<"error">> = Cmd) ->
    throw({cmd, Cmd});
pick_command1(<<"fatal">> = Cmd) ->
    throw({cmd, Cmd});
pick_command1(_) ->
    throw(abort).

% gen_server API
init([Command]) ->
    V = config:get("query_server_config", "os_process_idle_limit", "300"),
    IdleLimit = list_to_integer(V) * 1000,
    LogLevel = log_level(os:getenv("COUCHDB_IO_LOG_LEVEL")),
    T0 = erlang:monotonic_time(),
    Port = open_port({spawn, Command}, ?PORT_OPTIONS),
    {os_pid, OsPid} = erlang:port_info(Port, os_pid),
    OsProc = #os_proc{
        command = Command,
        port = Port,
        idle = IdleLimit,
        log_level = LogLevel
    },
    T1 = erlang:monotonic_time(),
    DtUSec = erlang:convert_time_unit(T1 - T0, native, microsecond),
    bump_time_stat(spawn_proc, DtUSec),
    Pid = self(),
    [CmdLog | _] = string:split(Command, " "),
    CmdLog1 = filename:basename(CmdLog),
    log(OsProc, "OS Process pid:~p port:~p Started :: ~p", [OsPid, Port, CmdLog1]),
    couch_stats:increment_counter([couchdb, query_server, process_starts]),
    spawn(fun() ->
        % this ensure the real os process is killed when this process dies.
        monitor(process, Pid),
        killer(OsPid)
    end),
    {ok, OsProc, IdleLimit}.

terminate(Reason, #os_proc{port = Port} = OsProc) ->
    catch port_close(Port),
    log(OsProc, "OS Process ~p Terminated :: ~p", [Port, Reason]),
    ok.

handle_call({set_timeout, TimeOut}, _From, #os_proc{idle = Idle} = OsProc) ->
    {reply, ok, OsProc#os_proc{timeout = TimeOut}, Idle};
handle_call({prompt, Data}, _From, #os_proc{idle = Idle} = OsProc) ->
    try
        couch_stats:increment_counter([couchdb, query_server, process_prompts]),
        JsonData = ?JSON_ENCODE(Data),
        log(OsProc, "OS Process ~p Input :: ~s", [OsProc#os_proc.port, JsonData]),
        T0 = erlang:monotonic_time(),
        true = port_command(OsProc#os_proc.port, [JsonData, $\n]),
        Response = readjson(OsProc),
        T1 = erlang:monotonic_time(),
        DtUSec = erlang:convert_time_unit(T1 - T0, native, microsecond),
        bump_cmd_time_stat(Data, DtUSec),
        {reply, {ok, Response}, OsProc, Idle}
    catch
        throw:{error, OsError} ->
            couch_stats:increment_counter([couchdb, query_server, process_errors]),
            {reply, OsError, OsProc, Idle};
        throw:{fatal, OsError} ->
            couch_stats:increment_counter([couchdb, query_server, process_errors]),
            {stop, normal, OsError, OsProc};
        throw:OtherError ->
            couch_stats:increment_counter([couchdb, query_server, process_errors]),
            {stop, normal, OtherError, OsProc}
    after
        garbage_collect()
    end.

handle_cast(garbage_collect, #os_proc{idle = Idle} = OsProc) ->
    garbage_collect(),
    {noreply, OsProc, Idle};
handle_cast(stop, OsProc) ->
    {stop, normal, OsProc};
handle_cast(Msg, #os_proc{idle = Idle} = OsProc) ->
    couch_log:debug("OS Proc: Unknown cast: ~p", [Msg]),
    {noreply, OsProc, Idle}.

handle_info(timeout, #os_proc{idle = Idle} = OsProc) ->
    couch_proc_manager:os_proc_idle(self()),
    garbage_collect(),
    {noreply, OsProc, Idle};
handle_info({Port, {exit_status, 0}}, #os_proc{port = Port} = OsProc) ->
    couch_log:info("OS Process terminated normally", []),
    couch_stats:increment_counter([couchdb, query_server, process_exits]),
    {stop, normal, OsProc};
handle_info({Port, {exit_status, Status}}, #os_proc{port = Port} = OsProc) ->
    couch_log:error("OS Process died with status: ~p", [Status]),
    couch_stats:increment_counter([couchdb, query_server, process_error_exits]),
    {stop, {exit_status, Status}, OsProc};
handle_info(Msg, #os_proc{idle = Idle} = OsProc) ->
    log(OsProc, "OS Process ~p Unknown info :: ~p", [OsProc#os_proc.port, Msg]),
    {noreply, OsProc, Idle}.

kill_command(OsPid) ->
    OsPid1 = integer_to_list(OsPid),
    case os:type() of
        {win32, _} -> "taskkill /f /pid " ++ OsPid1;
        {_, _} -> "kill -9 " ++ OsPid1
    end.

killer(OsPid) when is_integer(OsPid), OsPid > 0 ->
    receive
        _ ->
            os:cmd(kill_command(OsPid))
    after 1000 ->
        killer(OsPid)
    end.

bump_cmd_time_stat(Cmd, USec) when is_list(Cmd), is_integer(USec) ->
    case Cmd of
        [<<"map_doc">> | _] ->
            bump_time_stat(map, USec);
        [<<"reduce">> | _] ->
            bump_time_stat(reduce, USec);
        [<<"rereduce">> | _] ->
            bump_time_stat(reduce, USec);
        [<<"reset">> | _] ->
            bump_time_stat(reset, USec);
        [<<"add_fun">> | _] ->
            bump_time_stat(add_fun, USec);
        [<<"ddoc">>, <<"new">> | _] ->
            bump_time_stat(ddoc_new, USec);
        [<<"ddoc">>, _, [<<"validate_doc_update">> | _] | _] ->
            bump_time_stat(ddoc_vdu, USec);
        [<<"ddoc">>, _, [<<"filters">> | _] | _] ->
            bump_time_stat(ddoc_filter, USec);
        [<<"ddoc">> | _] ->
            bump_time_stat(ddoc_other, USec);
        _ ->
            bump_time_stat(other, USec)
    end.

bump_time_stat(Stat, USec) when is_atom(Stat), is_integer(USec) ->
    couch_stats:increment_counter([couchdb, query_server, calls, Stat]),
    couch_stats:increment_counter([couchdb, query_server, time, Stat], USec).

log_level("debug") ->
    debug;
log_level("info") ->
    info;
log_level("notice") ->
    notice;
log_level("warning") ->
    warning;
log_level("error") ->
    error;
log_level(_) ->
    debug.

log(#os_proc{log_level = LogLevel}, Fmt, Args) ->
    couch_log:LogLevel(Fmt, Args).
