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

-module(chttpd_util_test).

-include_lib("couch/include/couch_eunit.hrl").

setup() ->
    ok = lists:foreach(
        fun(Section) ->
            ok = config_delete_all_keys(Section)
        end,
        ["httpd", "chttpd", "couch_httpd_auth", "chttpd_auth"]
    ),

    Persist = false,
    ok = config:set(
        "httpd",
        "authentication_handlers",
        "{couch_httpd_auth, cookie_authentication_handler}, "
        "{couch_httpd_auth, default_authentication_handler}",
        Persist
    ),
    ok = config:set("httpd", "backlog", "512", Persist),
    ok = config:set("chttpd", "require_valid_user", "false", Persist),
    ok = config:set("httpd", "both_exist", "get_in_httpd", Persist),
    ok = config:set("chttpd", "both_exist", "get_in_chttpd", Persist),
    ok = config:set("httpd", "httpd_only", "true", Persist),
    ok = config:set("chttpd", "chttpd_only", "1", Persist),
    ok = config:set("couch_httpd_auth", "both_exist", "cha", Persist),
    ok = config:set("chttpd_auth", "both_exist", "ca", Persist),
    ok = config:set("couch_httpd_auth", "cha_only", "true", Persist),
    ok = config:set("chttpd_auth", "ca_only", "1", Persist).

teardown(_) ->
    Persist = false,
    ok = config:delete("httpd", "authentication_handlers", Persist),
    ok = config:delete("httpd", "backlog", Persist),
    ok = config:delete("chttpd", "require_valid_user", Persist),
    ok = config:delete("httpd", "both_exist", Persist),
    ok = config:delete("chttpd", "both_exist", Persist),
    ok = config:delete("httpd", "httpd_only", Persist),
    ok = config:delete("chttpd", "chttpd_only", Persist),
    ok = config:delete("couch_httpd_auth", "both_exist", Persist),
    ok = config:delete("chttpd_auth", "both_exist", Persist),
    ok = config:delete("couch_httpd_auth", "cha_only", Persist),
    ok = config:delete("chttpd_auth", "ca_only", Persist).

config_delete_all_keys(Section) ->
    lists:foreach(
        fun({Key, _Val}) ->
            ok = config:delete(Section, Key, _Persist = false)
        end,
        config:get(Section)
    ).

chttpd_util_config_test_() ->
    {
        "chttpd util config tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(test_chttpd_behavior),
                    ?TDEF_FE(test_with_undefined_option),
                    ?TDEF_FE(test_auth_behavior),
                    ?TDEF_FE(test_auth_with_undefined_option)
                ]
            }
        }
    }.

test_chttpd_behavior(_) ->
    ?assertEqual("get_in_chttpd", chttpd_util:get_chttpd_config("both_exist")),
    ?assertEqual(1, chttpd_util:get_chttpd_config_integer("chttpd_only", 0)),
    ?assert(chttpd_util:get_chttpd_config_boolean("httpd_only", false)).

test_with_undefined_option(_) ->
    ?assertEqual(undefined, chttpd_util:get_chttpd_config("undefined_option")),
    ?assertEqual(abc, chttpd_util:get_chttpd_config("undefined_option", abc)),
    ?assertEqual(123, chttpd_util:get_chttpd_config("undefined_option", 123)),
    ?assertEqual(0.2, chttpd_util:get_chttpd_config("undefined_option", 0.2)),
    ?assertEqual("a", chttpd_util:get_chttpd_config("undefined_option", "a")),
    ?assertEqual("", chttpd_util:get_chttpd_config("undefined_option", "")),
    ?assert(chttpd_util:get_chttpd_config("undefined_option", true)),
    ?assertNot(chttpd_util:get_chttpd_config("undefined_option", false)).

test_auth_behavior(_) ->
    ?assertEqual("ca", chttpd_util:get_chttpd_auth_config("both_exist")),
    ?assertEqual(1, chttpd_util:get_chttpd_auth_config_integer("ca_only", 0)),
    ?assert(chttpd_util:get_chttpd_auth_config_boolean("cha_only", false)).

test_auth_with_undefined_option(_) ->
    ?assertEqual(undefined, chttpd_util:get_chttpd_auth_config("undefine")),
    ?assertEqual(abc, chttpd_util:get_chttpd_auth_config("undefine", abc)),
    ?assertEqual(123, chttpd_util:get_chttpd_auth_config("undefine", 123)),
    ?assertEqual(0.2, chttpd_util:get_chttpd_auth_config("undefine", 0.2)),
    ?assertEqual("a", chttpd_util:get_chttpd_auth_config("undefine", "a")),
    ?assertEqual("", chttpd_util:get_chttpd_auth_config("undefine", "")),
    ?assert(chttpd_util:get_chttpd_auth_config("undefine", true)),
    ?assertNot(chttpd_util:get_chttpd_auth_config("undefine", false)).

chttpd_util_client_socker_monitor_test_() ->
    {
        setup,
        fun test_util:start_couch/0,
        fun test_util:stop_couch/1,
        with([
            ?TDEF(t_socket_set_get_clean),
            ?TDEF(t_socket_check_config),
            ?TDEF(t_closed_socket_kills_coordinator)
        ])
    }.

t_socket_set_get_clean(_) ->
    ?assertEqual(undefined, chttpd_util:mochiweb_socket_get()),
    {ok, Sock} = gen_tcp:listen(0, [{active, false}]),
    chttpd_util:mochiweb_socket_set(Sock),
    ?assertEqual(Sock, chttpd_util:mochiweb_socket_get()),
    chttpd_util:mochiweb_socket_clean(),
    ?assertEqual(undefined, chttpd_util:mochiweb_socket_get()),
    gen_tcp:close(Sock).

t_socket_check_config(_) ->
    config:set("chttpd", "disconnect_check_msec", "100", false),
    config:set("chttpd", "disconnect_check_jitter_msec", "50", false),
    lists:foreach(
        fun(_) ->
            MSec = chttpd_util:mochiweb_socket_check_msec(),
            ?assert(is_integer(MSec)),
            ?assert(MSec >= 100),
            ?assert(MSec =< 150)
        end,
        lists:seq(1, 1000)
    ),
    config:delete("chttpd", "disconnect_check_msec", false),
    config:delete("chttpd", "disconnect_check_jitter_msec", false).

t_closed_socket_kills_coordinator(_) ->
    {Pid, Ref} = spawn_coord(),
    {ok, Sock} = gen_tcp:listen(0, [{active, false}]),

    % Can call getopts many times in a row process should stay alive
    lists:foreach(
        fun(_) ->
            ok = chttpd_util:stop_client_process_if_disconnected(Pid, Sock)
        end,
        lists:seq(1, 10000)
    ),
    ?assert(is_process_alive(Pid)),

    gen_tcp:close(Sock),

    ?assertEqual(ok, chttpd_util:stop_client_process_if_disconnected(Pid, Sock)),
    case tcp_info_works() of
        true ->
            ?assertEqual({shutdown, client_disconnected}, wait_coord_death(Ref));
        false ->
            ?assert(is_process_alive(Pid)),
            % Kill it manually
            exit(Pid, kill)
    end,

    % Can call stop_client_... even if process may be dead and the socket is closed
    lists:foreach(
        fun(_) ->
            ok = chttpd_util:stop_client_process_if_disconnected(Pid, Sock)
        end,
        lists:seq(1, 10000)
    ).

spawn_coord() ->
    spawn_monitor(fun() ->
        receive
            die -> ok
        end
    end).

wait_coord_death(Ref) ->
    receive
        {'DOWN', Ref, _, _, Reason} -> Reason
    end.

tcp_info_works() ->
    case os:type() of
        {unix, OsName} ->
            lists:member(OsName, [linux, freebsd, darwin]);
        {_, _} ->
            false
    end.
