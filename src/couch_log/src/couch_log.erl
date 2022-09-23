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
    report/2,
    set_level/1
]).

-spec debug(string(), list()) -> ok.
debug(Fmt, Args) -> log(debug, Fmt, Args).

-spec info(string(), list()) -> ok.
info(Fmt, Args) -> log(info, Fmt, Args).

-spec notice(string(), list()) -> ok.
notice(Fmt, Args) -> log(notice, Fmt, Args).

-spec warning(string(), list()) -> ok.
warning(Fmt, Args) -> log(warning, Fmt, Args).

-spec error(string(), list()) -> ok.
error(Fmt, Args) -> log(error, Fmt, Args).

-spec critical(string(), list()) -> ok.
critical(Fmt, Args) -> log(critical, Fmt, Args).

-spec alert(string(), list()) -> ok.
alert(Fmt, Args) -> log(alert, Fmt, Args).

-spec emergency(string(), list()) -> ok.
emergency(Fmt, Args) -> log(emergency, Fmt, Args).

-spec report(string(), map()) -> ok.
report(ReportId, Meta) when is_map(Meta) ->
    couch_stats:increment_counter([couch_log, level, report]),
    Entry = couch_log_formatter:format(report, self(), ReportId, "", [], Meta),
    ok = couch_log_server:report(Entry).

-spec set_level(atom() | string() | integer()) -> true.
set_level(Level) ->
    config:set("log", "level", couch_log_util:level_to_string(Level)).

-spec log(atom(), string(), list()) -> ok.
log(Level, Fmt, Args) ->
    log(Level, undefined, Fmt, Args, #{}).

log(Level, Type, Fmt, Args, Meta) ->
    case couch_log_util:should_log(Level) of
        true ->
            couch_stats:increment_counter([couch_log, level, Level]),
            Entry = couch_log_formatter:format(Level, self(), Type, Fmt, Args, Meta),
            ok = couch_log_server:log(Entry);
        false ->
            ok
    end.
