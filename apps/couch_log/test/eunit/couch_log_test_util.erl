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

-module(couch_log_test_util).

-export([
    start/0,
    stop/1,
    last_log/0,
    last_log_key/0,
    wait_for_config/0,
    with_config_listener/1,
    with_level/2,
    with_meck/2
]).

-include("couch_log.hrl").

start() ->
    remove_error_loggers(),
    application:set_env(config, ini_files, config_files()),
    application:start(config),
    ignore_common_loggers(),
    application:start(couch_log),
    meck:new(couch_stats),
    ok = meck:expect(couch_stats, increment_counter, ['_'], ok).

stop(_) ->
    application:stop(config),
    application:stop(couch_log),
    meck:unload(couch_stats).

with_level(Name, Fun) ->
    with_config_listener(fun() ->
        try
            LevelStr = couch_log_util:level_to_string(Name),
            config:set("log", "level", LevelStr, false),
            wait_for_config(),
            Fun()
        after
            config:delete("log", "level", false)
        end
    end).

with_config_listener(Fun) ->
    Listener = self(),
    try
        add_listener(Listener),
        Fun()
    after
        rem_listener(Listener)
    end.

wait_for_config() ->
    receive
        couch_log_config_change_finished -> ok
    after 1000 ->
        erlang:error(config_change_timeout)
    end.

with_meck(Mods, Fun) ->
    lists:foreach(
        fun(M) ->
            case M of
                {Name, Opts} -> meck:new(Name, Opts);
                Name -> meck:new(Name)
            end
        end,
        Mods
    ),
    try
        Fun()
    after
        lists:foreach(
            fun(M) ->
                case M of
                    {Name, _} -> meck:unload(Name);
                    Name -> meck:unload(Name)
                end
            end,
            Mods
        )
    end.

ignore_common_loggers() ->
    IgnoreSet = [
        application_controller,
        config,
        config_event
    ],
    lists:foreach(
        fun(Proc) ->
            disable_logs_from(Proc)
        end,
        IgnoreSet
    ).

disable_logs_from(Pid) when is_pid(Pid) ->
    Ignored =
        case application:get_env(couch_log, ignored_pids) of
            {ok, L} when is_list(L) ->
                lists:usort([Pid | L]);
            _E ->
                [Pid]
        end,
    IgnoredAlive = [P || P <- Ignored, is_process_alive(P)],
    application:set_env(couch_log, ignored_pids, IgnoredAlive);
disable_logs_from(Name) when is_atom(Name) ->
    case whereis(Name) of
        P when is_pid(P) ->
            disable_logs_from(P);
        undefined ->
            erlang:error({unknown_pid_name, Name})
    end.

last_log_key() ->
    ets:last(?COUCH_LOG_TEST_TABLE).

last_log() ->
    [{_, Entry}] = ets:lookup(?COUCH_LOG_TEST_TABLE, last_log_key()),
    Entry.

remove_error_loggers() ->
    ErrorLoggerPid = whereis(error_logger),
    if
        ErrorLoggerPid == undefined ->
            ok;
        true ->
            lists:foreach(
                fun(Handler) ->
                    error_logger:delete_report_handler(Handler)
                end,
                gen_event:which_handlers(ErrorLoggerPid)
            )
    end.

config_files() ->
    Path = filename:dirname(code:which(?MODULE)),
    Name = filename:join(Path, "couch_log_test.ini"),
    ok = file:write_file(Name, "[log]\nwriter = ets\n"),
    [Name].

add_listener(Listener) ->
    Listeners =
        case application:get_env(couch_log, config_listeners) of
            {ok, L} when is_list(L) ->
                lists:usort([Listener | L]);
            _ ->
                [Listener]
        end,
    application:set_env(couch_log, config_listeners, Listeners).

rem_listener(Listener) ->
    Listeners =
        case application:get_env(couch_lig, config_listeners) of
            {ok, L} when is_list(L) ->
                L -- [Listener];
            _ ->
                []
        end,
    application:set_env(couch_log, config_listeners, Listeners).
