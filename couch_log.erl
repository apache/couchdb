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
    sasl,
    eol_re
}).

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
        {ok, EolRe} = re:compile("\\r\\n|\\r|\\n"),
        {ok, #state{fd = Fd, level = Level, sasl = Sasl, eol_re = EolRe}};
    {error, eacces} ->
        {stop, {file_permission_error, Filename}};
    Error ->
        {stop, Error}
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

handle_event({Pid, couch_error, {Format, Args}}, State) ->
    log(State, Pid, error, Format, Args),
    {ok, State};
handle_event({Pid, couch_info, {Format, Args}}, #state{level = LogLevel} = St)
when LogLevel =< ?LEVEL_INFO ->
    log(St, Pid, info, Format, Args),
    {ok, St};
handle_event({Pid, couch_debug, {Format, Args}}, #state{level = LogLevel} = St)
when LogLevel =< ?LEVEL_DEBUG ->
    log(St, Pid, debug, Format, Args),
    {ok, St};
handle_event({error_report, _, {Pid, _, _}}=Event, #state{sasl = true} = St) ->
    log(St, Pid, error, "~p", [Event]),
    {ok, St};
handle_event({error, _, {Pid, Format, Args}}, #state{sasl = true} = State) ->
    log(State, Pid, error, Format, Args),
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

log(#state{fd = Fd, eol_re = EolRe}, Pid, Level, Format, Args) ->
    Msg = io_lib:format(Format, Args),
    ok = io:format("[~s] [~p] ~s~n", [Level, Pid, Msg]), % dump to console too
    Msg2 = re:replace(Msg, EolRe, "\r\n", [global]),
    ok = io:format(Fd, "[~s] [~s] [~p] ~s\r~n",
        [httpd_util:rfc1123_date(), Level, Pid, Msg2]).

read(Bytes, Offset) ->
    LogFileName = couch_config:get("log", "file"),
    LogFileSize = filelib:file_size(LogFileName),

    {ok, Fd} = file:open(LogFileName, [read]),
    Start = lists:max([LogFileSize - Bytes, 0]) + Offset,

    % TODO: truncate chopped first line
    % TODO: make streaming

    {ok, Chunk} = file:pread(Fd, Start, LogFileSize),
    ok = file:close(Fd),
    Chunk.
