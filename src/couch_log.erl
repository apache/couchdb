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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([debug/2, info/2, notice/2, warning/2, error/2, critical/2, alert/2,
         emergency/2]).
-export([set_level/1]).

-callback debug(Fmt::string(), Args::list()) -> ok.
-callback info(Fmt::string(), Args::list()) -> ok.
-callback notice(Fmt::string(), Args::list()) -> ok.
-callback warning(Fmt::string(), Args::list()) -> ok.
-callback error(Fmt::string(), Args::list()) -> ok.
-callback critical(Fmt::string(), Args::list()) -> ok.
-callback alert(Fmt::string(), Args::list()) -> ok.
-callback emergency(Fmt::string(), Args::list()) -> ok.
-callback set_level(Level::atom()) -> ok.

-spec level_integer(atom()) -> integer().
level_integer(debug)             -> 1;
level_integer(info)              -> 2;
level_integer(notice)            -> 3;
level_integer(warning)           -> 4;
level_integer(error)             -> 5;
level_integer(critical)          -> 6;
level_integer(alert)             -> 7;
level_integer(emergency)         -> 8;
level_integer(none)              -> 9.

-spec level_to_atom(string() | integer()) -> atom().
level_to_atom("1")                  -> debug;
level_to_atom("debug")              -> debug;
level_to_atom("2")                  -> info;
level_to_atom("info")               -> info;
level_to_atom("3")                  -> notice;
level_to_atom("notice")             -> notice;
level_to_atom("4")                  -> warning;
level_to_atom("warning")            -> warning;
level_to_atom("5")                  -> error;
level_to_atom("error")              -> error;
level_to_atom("6")                  -> critical;
level_to_atom("critical")           -> critical;
level_to_atom("7")                  -> alert;
level_to_atom("alert")              -> alert;
level_to_atom("8")                  -> emergency;
level_to_atom("emergency")          -> emergency;
level_to_atom("9")                  -> none;
level_to_atom("none")               -> none;
level_to_atom(V) when is_integer(V) -> level_to_atom(integer_to_list(V));
level_to_atom(V) when is_list(V)    -> notice.

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

-spec log(atom(), string(), list()) -> ok.
log(Level, Fmt, Args) ->
    case is_active_level(Level) of
        false -> ok;
        true ->
            {ok, Backend} = get_backend(),
            catch couch_stats:increment_counter([couch_log, level, Level]),
            apply(Backend, Level, [Fmt, Args])
    end.

-spec is_active_level(atom()) -> boolean.
is_active_level(Level) ->
    CurrentLevel = level_to_atom(config:get("log", "level", "notice")),
    level_integer(Level) >= level_integer(CurrentLevel).

-spec set_level(atom() | string() | integer()) -> ok.
set_level(Level) when is_atom(Level) ->
    {ok, Backend} = get_backend(),
    Backend:set_level(Level);
set_level(Level) ->
    set_level(level_to_atom(Level)).

-spec get_backend() -> {ok, atom()}.
get_backend() ->
    BackendName = "couch_log_" ++ config:get("log", "backend", "stderr"),
    Backend = list_to_existing_atom(BackendName),  %% yes, we need crash here
    case erlang:module_loaded(Backend) of
        true -> {ok, Backend};
        false -> {ok, couch_log_stderr}
    end.

-ifdef(TEST).

callbacks_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_assertEqual({ok, couch_log_eunit}, get_backend()),
            ?_assertEqual(ok, couch_log:set_level(info)),
            ?_assertEqual(ok, couch_log:debug("debug", [])),
            ?_assertEqual(ok, couch_log:info("info", [])),
            ?_assertEqual(ok, couch_log:notice("notice", [])),
            ?_assertEqual(ok, couch_log:warning("warning", [])),
            ?_assertEqual(ok, couch_log:error("error", [])),
            ?_assertEqual(ok, couch_log:critical("critical", [])),
            ?_assertEqual(ok, couch_log:alert("alert", [])),
            ?_assertEqual(ok, couch_log:emergency("emergency", [])),
            ?_assertEqual(stats_calls(), meck:history(couch_stats, self())),
            ?_assertEqual(log_calls(), meck:history(couch_log_eunit, self()))
        ]
    }.

setup() ->
    ok = meck:new(config),
    ok = meck:expect(config, get,
        fun("log", "backend", _) -> "eunit";
           ("log", "level", _)   -> "debug" end),
    meck:new([couch_stats, couch_log_eunit], [non_strict]),
    meck:expect(couch_stats, increment_counter, 1, ok),
    setup_couch_log_eunit().

cleanup(_) ->
    meck:unload(config),
    meck:unload([couch_stats, couch_log_eunit]).

setup_couch_log_eunit() ->
    meck:expect(couch_log_eunit, set_level, 1, ok),
    Levels = [debug, info, notice, warning, error, critical, alert, emergency],
    lists:foreach(fun(Fun) ->
        meck:expect(couch_log_eunit, Fun, 2, ok)
    end, Levels).

stats_calls() ->
    Levels = [debug, info, notice, warning, error, critical, alert, emergency],
    lists:map(fun(Level) ->
        MFA = {couch_stats, increment_counter, [[couch_log, level, Level]]},
        {self(), MFA, ok}
    end, Levels).

log_calls() ->
    Levels = [debug, info, notice, warning, error, critical, alert, emergency],
    Calls = lists:map(fun(Level) ->
        MFA = {couch_log_eunit, Level, [atom_to_list(Level),[]]},
        {self(), MFA, ok}
    end, Levels),
    [{self(), {couch_log_eunit, set_level, [info]}, ok}|Calls].

-endif.
