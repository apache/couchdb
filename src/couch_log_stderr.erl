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

-module(couch_log_stderr).

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

debug(Fmt, Args) ->
    write_log("[debug] " ++ Fmt, Args).

info(Fmt, Args) ->
    write_log("[info] " ++ Fmt, Args).

notice(Fmt, Args) ->
    write_log("[notice] " ++ Fmt, Args).

warning(Fmt, Args) ->
    write_log("[warning] " ++ Fmt, Args).

error(Fmt, Args) ->
    write_log("[error] " ++ Fmt, Args).

critical(Fmt, Args) ->
    write_log("[critical] " ++ Fmt, Args).

alert(Fmt, Args) ->
    write_log("[alert] " ++ Fmt, Args).

emergency(Fmt, Args) ->
    write_log("[emergency] " ++ Fmt, Args).

write_log(Fmt, Args) ->
    io:format(standard_error, Fmt ++ "~n", Args).

set_level(_) ->
    ok.
