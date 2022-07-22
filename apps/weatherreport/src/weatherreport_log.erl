%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

-module(weatherreport_log).
-export([
    level/1,
    log/3,
    log/4,
    should_log/1
]).

level(debug) ->
    7;
level(info) ->
    6;
level(notice) ->
    5;
level(warn) ->
    4;
level(warning) ->
    4;
level(err) ->
    3;
level(error) ->
    3;
level(crit) ->
    2;
level(alert) ->
    1;
level(emerg) ->
    0;
level(panic) ->
    0;
level(I) when is_integer(I), I >= 0, I =< 7 ->
    I;
level(_BadLevel) ->
    3.

log(Node, Level, Format, Terms) ->
    case should_log(Level) of
        true ->
            Prefix = get_prefix(Node, Level),
            Message = io_lib:format(Format, Terms),
            io:format("~s ~s~n", [Prefix, Message]);
        false ->
            ok
    end.

log(Node, Level, String) ->
    case should_log(Level) of
        true ->
            Prefix = get_prefix(Node, Level),
            io:format("~s ~s~n", [Prefix, String]);
        false ->
            ok
    end.

should_log(Level) ->
    AppLevel =
        case application:get_env(weatherreport, log_level) of
            undefined -> info;
            {ok, L0} -> L0
        end,
    level(AppLevel) >= level(Level).

get_prefix(Node, Level) ->
    io_lib:format("[~w] [~w]", [Node, Level]).
