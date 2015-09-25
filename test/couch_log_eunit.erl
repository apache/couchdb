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

-module(couch_log_eunit).

-behaviour(couch_log).

-export([
    debug/2,
    info/2,
    notice/2,
    warning/2,
    error/2,
    critical/2,
    alert/2,
    emergency/2,
    set_level/1
]).

-export([
    setup/0,
    cleanup/0,
    debug/0,
    info/0,
    notice/0,
    warning/0,
    error/0,
    critical/0,
    alert/0,
    emergency/0,
    get_level/0
]).

debug() ->
    read_log(debug).

debug(Fmt, Args) ->
    write_log(debug, Fmt, Args).

info() ->
    read_log(info).

info(Fmt, Args) ->
    write_log(info, Fmt, Args).

notice() ->
    read_log(notice).

notice(Fmt, Args) ->
    write_log(notice, Fmt, Args).

warning() ->
    read_log(warning).

warning(Fmt, Args) ->
    write_log(warning, Fmt, Args).

error() ->
    read_log(error).

error(Fmt, Args) ->
    write_log(error, Fmt, Args).

critical() ->
    read_log(critical).

critical(Fmt, Args) ->
    write_log(critical, Fmt, Args).

alert() ->
    read_log(alert).

alert(Fmt, Args) ->
    write_log(alert, Fmt, Args).

emergency() ->
    read_log(emergency).

emergency(Fmt, Args) ->
    write_log(emergency, Fmt, Args).

get_level() ->
    read_log(level).

set_level(Level) ->
    true = ets:insert(?MODULE, {level, Level}),
    ok.


setup() ->
    ets:new(?MODULE, [public, named_table]).

cleanup() ->
    ets:delete(?MODULE).

write_log(Key, Fmt, Args) ->
    Msg = io_lib:format(Fmt, Args),
    true = ets:insert(?MODULE, {Key, Msg}),
    ok.

read_log(Key) ->
    case ets:lookup(?MODULE, Key) of
        [] -> undefined;
        [{Key, Value}] -> Value
    end.