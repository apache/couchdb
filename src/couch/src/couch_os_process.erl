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
-export([set_timeout/2, prompt/2, killer/1]).
-export([writeline/2, readline/1, writejson/2, readjson/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-include_lib("couch/include/couch_db.hrl").

-define(PORT_OPTIONS, [stream, {line, 4096}, binary, exit_status, hide]).

-record(os_proc, {
    command,
    port,
    writer,
    reader,
    timeout = 5000,
    idle
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

% Utility functions for reading and writing
% in custom functions
writeline(OsProc, Data) when is_record(OsProc, os_proc) ->
    Res = port_command(OsProc#os_proc.port, [Data, $\n]),
    couch_io_logger:log_output(Data),
    Res.

readline(#os_proc{} = OsProc) ->
    Res = readline(OsProc, []),
    couch_io_logger:log_input(Res),
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
        throw({os_process_error, "OS process timed out."})
    end.

% Standard JSON functions
writejson(OsProc, Data) when is_record(OsProc, os_proc) ->
    JsonData = ?JSON_ENCODE(Data),
    couch_log:debug(
        "OS Process ~p Input  :: ~s",
        [OsProc#os_proc.port, JsonData]
    ),
    true = writeline(OsProc, JsonData).

readjson(OsProc) when is_record(OsProc, os_proc) ->
    Line = iolist_to_binary(readline(OsProc)),
    couch_log:debug("OS Process ~p Output :: ~s", [OsProc#os_proc.port, Line]),
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
    couch_io_logger:start(os:getenv("COUCHDB_IO_LOG_DIR")),
    PrivDir = couch_util:priv_dir(),
    Spawnkiller = "\"" ++ filename:join(PrivDir, "couchspawnkillable") ++ "\"",
    V = config:get("query_server_config", "os_process_idle_limit", "300"),
    IdleLimit = list_to_integer(V) * 1000,
    OsProc = #os_proc{
        command = Command,
        port = open_port({spawn, Spawnkiller ++ " " ++ Command}, ?PORT_OPTIONS),
        writer = fun ?MODULE:writejson/2,
        reader = fun ?MODULE:readjson/1,
        idle = IdleLimit
    },
    KillCmd = iolist_to_binary(readline(OsProc)),
    Pid = self(),
    couch_log:debug("OS Process Start :: ~p", [OsProc#os_proc.port]),
    couch_stats:increment_counter([couchdb, query_server, process_starts]),
    spawn(fun() ->
        % this ensure the real os process is killed when this process dies.
        erlang:monitor(process, Pid),
        killer(?b2l(KillCmd))
    end),
    {ok, OsProc, IdleLimit}.

terminate(Reason, #os_proc{port = Port}) ->
    catch port_close(Port),
    case Reason of
        normal ->
            couch_io_logger:stop_noerror();
        Error ->
            couch_io_logger:stop_error(Error)
    end,
    ok.

handle_call({set_timeout, TimeOut}, _From, #os_proc{idle = Idle} = OsProc) ->
    {reply, ok, OsProc#os_proc{timeout = TimeOut}, Idle};
handle_call({prompt, Data}, _From, #os_proc{idle = Idle} = OsProc) ->
    #os_proc{writer = Writer, reader = Reader} = OsProc,
    try
        Writer(OsProc, Data),
        couch_stats:increment_counter([couchdb, query_server, process_prompts]),
        {reply, {ok, Reader(OsProc)}, OsProc, Idle}
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
    erlang:garbage_collect(),
    {noreply, OsProc, Idle};
handle_cast(stop, OsProc) ->
    {stop, normal, OsProc};
handle_cast(Msg, #os_proc{idle = Idle} = OsProc) ->
    couch_log:debug("OS Proc: Unknown cast: ~p", [Msg]),
    {noreply, OsProc, Idle}.

handle_info(timeout, #os_proc{idle = Idle} = OsProc) ->
    couch_proc_manager:os_proc_idle(self()),
    erlang:garbage_collect(),
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
    couch_log:debug("OS Proc: Unknown info: ~p", [Msg]),
    {noreply, OsProc, Idle}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

killer(KillCmd) ->
    receive
        _ ->
            os:cmd(KillCmd)
    after 1000 ->
        ?MODULE:killer(KillCmd)
    end.
