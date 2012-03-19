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

-module(couch_log).
-behaviour(gen_event).

% public API
-export([start_link/0, stop/0]).
-export([debug/2, info/2, error/2]).
-export([debug_on/0, info_on/0, get_level/0, get_level_integer/0, set_level/1]).
-export([read/2]).

% gen_event callbacks
-export([init/1, handle_event/2, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/2]).

-define(LEVEL_ERROR, 3).
-define(LEVEL_INFO, 2).
-define(LEVEL_DEBUG, 1).

-record(state, {
    fd,
    level,
    sasl
}).

debug(Format, Args) ->
    {ConsoleMsg, FileMsg} = get_log_messages(self(), debug, Format, Args),
    gen_event:sync_notify(error_logger, {couch_debug, ConsoleMsg, FileMsg}).

info(Format, Args) ->
    {ConsoleMsg, FileMsg} = get_log_messages(self(), info, Format, Args),
    gen_event:sync_notify(error_logger, {couch_info, ConsoleMsg, FileMsg}).

error(Format, Args) ->
    {ConsoleMsg, FileMsg} = get_log_messages(self(), error, Format, Args),
    gen_event:sync_notify(error_logger, {couch_error, ConsoleMsg, FileMsg}).


level_integer(error)    -> ?LEVEL_ERROR;
level_integer(info)     -> ?LEVEL_INFO;
level_integer(debug)    -> ?LEVEL_DEBUG;
level_integer(_Else)    -> ?LEVEL_ERROR. % anything else default to ERROR level

level_atom(?LEVEL_ERROR) -> error;
level_atom(?LEVEL_INFO) -> info;
level_atom(?LEVEL_DEBUG) -> debug.


start_link() ->
    couch_event_sup:start_link({local, couch_log}, error_logger, couch_log, []).

stop() ->
    couch_event_sup:stop(couch_log).

init([]) ->
    % read config and register for configuration changes

    % just stop if one of the config settings change. couch_server_sup
    % will restart us and then we will pick up the new settings.
    ok = couch_config:register(
        fun("log", "file") ->
            ?MODULE:stop();
        ("log", "level") ->
            ?MODULE:stop();
        ("log", "include_sasl") ->
            ?MODULE:stop()
        end),

    Filename = couch_config:get("log", "file", "couchdb.log"),
    Level = level_integer(list_to_atom(couch_config:get("log", "level", "info"))),
    Sasl = couch_config:get("log", "include_sasl", "true") =:= "true",

    case ets:info(?MODULE) of
    undefined -> ets:new(?MODULE, [named_table]);
    _ -> ok
    end,
    ets:insert(?MODULE, {level, Level}),

    case file:open(Filename, [append]) of
    {ok, Fd} ->
        {ok, #state{fd = Fd, level = Level, sasl = Sasl}};
    {error, Reason} ->
        ReasonStr = file:format_error(Reason),
        io:format("Error opening log file ~s: ~s", [Filename, ReasonStr]),
        {stop, {error, ReasonStr, Filename}}
    end.

debug_on() ->
    get_level_integer() =< ?LEVEL_DEBUG.

info_on() ->
    get_level_integer() =< ?LEVEL_INFO.

set_level(LevelAtom) ->
    set_level_integer(level_integer(LevelAtom)).

get_level() ->
    level_atom(get_level_integer()).

get_level_integer() ->
    try
        ets:lookup_element(?MODULE, level, 2)
    catch error:badarg ->
        ?LEVEL_ERROR
    end.

set_level_integer(Int) ->
    gen_event:call(error_logger, couch_log, {set_level_integer, Int}).

handle_event({couch_error, ConMsg, FileMsg}, State) ->
    log(State, ConMsg, FileMsg),
    {ok, State};
handle_event({couch_info, ConMsg, FileMsg}, #state{level = LogLevel} = State)
when LogLevel =< ?LEVEL_INFO ->
    log(State, ConMsg, FileMsg),
    {ok, State};
handle_event({couch_debug, ConMsg, FileMsg}, #state{level = LogLevel} = State)
when LogLevel =< ?LEVEL_DEBUG ->
    log(State, ConMsg, FileMsg),
    {ok, State};
handle_event({error_report, _, {Pid, _, _}}=Event, #state{sasl = true} = St) ->
    {ConMsg, FileMsg} = get_log_messages(Pid, error, "~p", [Event]),
    log(St, ConMsg, FileMsg),
    {ok, St};
handle_event({error, _, {Pid, Format, Args}}, #state{sasl = true} = State) ->
    {ConMsg, FileMsg} = get_log_messages(Pid, error, Format, Args),
    log(State, ConMsg, FileMsg),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call({set_level_integer, NewLevel}, State) ->
    ets:insert(?MODULE, {level, NewLevel}),
    {ok, ok, State#state{level = NewLevel}}.

handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Arg, #state{fd = Fd}) ->
    file:close(Fd).

log(#state{fd = Fd}, ConsoleMsg, FileMsg) ->
    ok = io:put_chars(ConsoleMsg),
    ok = io:put_chars(Fd, FileMsg).

get_log_messages(Pid, Level, Format, Args) ->
    ConsoleMsg = unicode:characters_to_binary(io_lib:format(
        "[~s] [~p] " ++ Format ++ "~n", [Level, Pid | Args])),
    FileMsg = ["[", httpd_util:rfc1123_date(), "] ", ConsoleMsg],
    {ConsoleMsg, iolist_to_binary(FileMsg)}.


% Read Bytes bytes from the end of log file, jumping Offset bytes towards
% the beginning of the file first.
%
%  Log File    FilePos
%  ----------
% |          |  10
% |          |  20
% |          |  30
% |          |  40
% |          |  50
% |          |  60
% |          |  70 -- Bytes = 20  --
% |          |  80                 | Chunk
% |          |  90 -- Offset = 10 --
% |__________| 100

read(Bytes, Offset) ->
    LogFileName = couch_config:get("log", "file"),
    LogFileSize = filelib:file_size(LogFileName),
    MaxChunkSize = list_to_integer(
        couch_config:get("httpd", "log_max_chunk_size", "1000000")),
    case Bytes > MaxChunkSize of
    true ->
        throw({bad_request, "'bytes' cannot exceed " ++
            integer_to_list(MaxChunkSize)});
    false ->
        ok
    end,

    {ok, Fd} = file:open(LogFileName, [read]),
    Start = lists:max([LogFileSize - Bytes - Offset, 0]),

    % TODO: truncate chopped first line
    % TODO: make streaming

    {ok, Chunk} = file:pread(Fd, Start, Bytes),
    ok = file:close(Fd),
    Chunk.
