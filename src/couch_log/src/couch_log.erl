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


-spec debug(string(), list()) -> ok.
debug(Fmt, Args) ->
    couch_stats:increment_counter([couch_log, level, debug]),
    log(debug, Fmt, Args).


-spec info(string(), list()) -> ok.
info(Fmt, Args) ->
    couch_stats:increment_counter([couch_log, level, info]),
    log(info, Fmt, Args).


-spec notice(string(), list()) -> ok.
notice(Fmt, Args) ->
    couch_stats:increment_counter([couch_log, level, notice]),
    log(notice, Fmt, Args).


-spec warning(string(), list()) -> ok.
warning(Fmt, Args) ->
    couch_stats:increment_counter([couch_log, level, warning]),
    log(warning, Fmt, Args).


-spec error(string(), list()) -> ok.
error(Fmt, Args) ->
    couch_stats:increment_counter([couch_log, level, error]),
    log(error, Fmt, Args).


-spec critical(string(), list()) -> ok.
critical(Fmt, Args) ->
    couch_stats:increment_counter([couch_log, level, critical]),
    log(critical, Fmt, Args).


-spec alert(string(), list()) -> ok.
alert(Fmt, Args) ->
    couch_stats:increment_counter([couch_log, level, alert]),
    log(alert, Fmt, Args).


-spec emergency(string(), list()) -> ok.
emergency(Fmt, Args) ->
    couch_stats:increment_counter([couch_log, level, emergency]),
    log(emergency, Fmt, Args).


-spec set_level(atom() | string() | integer()) -> true.
set_level(Level) ->
    config:set("log", "level", couch_log_util:level_to_string(Level)).


-spec log(atom(), string(), list()) -> ok.
log(Level, Fmt, Args) ->
    case couch_log_util:should_log(Level) of
        true ->
            Entry = couch_log_formatter:format(Level, self(), Fmt, Args),
            ok = couch_log_server:log(Entry);
        false ->
            ok
    end.
