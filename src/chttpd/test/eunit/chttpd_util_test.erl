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
            ?TDEF(t_client_req_set_get_clean),
            ?TDEF(t_client_req_sensitive_headers_are_removed),
            ?TDEF(t_client_req_check_config),
            ?TDEF(t_closed_socket_kills_coordinator)
        ])
    }.

t_client_req_set_get_clean(_) ->
    ?assertEqual(undefined, chttpd_util:mochiweb_client_req_get()),
    {ok, Sock} = gen_tcp:listen(0, [{active, false}]),
    Headers = mochiweb_headers:make([]),
    ClientReq = mochiweb_request:new(Sock, 'GET', "/foo", {1, 1}, Headers),
    chttpd_util:mochiweb_client_req_set(ClientReq),
    ?assertEqual(ClientReq, chttpd_util:mochiweb_client_req_get()),
    chttpd_util:mochiweb_client_req_clean(),
    ?assertEqual(undefined, chttpd_util:mochiweb_client_req_get()),
    gen_tcp:close(Sock).

t_client_req_sensitive_headers_are_removed(_) ->
    {ok, Sock} = gen_tcp:listen(0, [{active, false}]),
    Headers = [
        {"AutHoriZatioN", "Basic s3cr3t"},
        {"COOkiE", "C00kie"},
        {"x-AUth-CouchDB-TokeN", "S3cr3tT0k3n"},
        {"other", "oth3r"}
    ],
    ClientReq = mochiweb:new_request({Sock, {'GET', "/foo", {1, 1}}, Headers}),
    chttpd_util:mochiweb_client_req_set(ClientReq),
    ResReq = chttpd_util:mochiweb_client_req_get(),
    ?assertEqual(Sock, mochiweb_request:get(socket, ResReq)),
    ?assertEqual('GET', mochiweb_request:get(method, ResReq)),
    ?assertEqual([], mochiweb_request:get(opts, ResReq)),
    ?assertEqual({1, 1}, mochiweb_request:get(version, ResReq)),
    ?assertEqual("/foo", mochiweb_request:get(raw_path, ResReq)),
    ResHeaders = mochiweb_request:get(headers, ResReq),
    ?assertEqual([{"other", "oth3r"}], mochiweb_headers:to_list(ResHeaders)),
    gen_tcp:close(Sock).

t_client_req_check_config(_) ->
    config:set("chttpd", "disconnect_check_msec", "100", false),
    config:set("chttpd", "disconnect_check_jitter_msec", "50", false),
    lists:foreach(
        fun(_) ->
            MSec = chttpd_util:mochiweb_client_req_check_msec(),
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
    Headers = mochiweb_headers:make([]),
    ClientReq = mochiweb_request:new(Sock, 'GET', "/foo", {1, 1}, Headers),
    % Can call getopts many times in a row process should stay alive
    lists:foreach(
        fun(_) ->
            ok = chttpd_util:stop_client_process_if_disconnected(Pid, ClientReq)
        end,
        lists:seq(1, 10000)
    ),
    ?assert(is_process_alive(Pid)),

    gen_tcp:close(Sock),

    ?assertEqual(ok, chttpd_util:stop_client_process_if_disconnected(Pid, ClientReq)),
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
            ok = chttpd_util:stop_client_process_if_disconnected(Pid, ClientReq)
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
            lists:member(OsName, [linux, freebsd, netbsd, openbsd, darwin]);
        {_, _} ->
            false
    end.

chttpd_util_get_active_tasks_test_() ->
    {
        "chttpd util get_active_tasks() tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(t_empty_tasks),
                    ?TDEF_FE(t_one_task),
                    ?TDEF_FE(t_multiple_tasks),
                    ?TDEF_FE(t_ignore_errors),
                    ?TDEF_FE(t_rolling_upgrade_test)
                ]
            }
        }
    }.

t_empty_tasks(_) ->
    Tasks = chttpd_util:get_active_tasks(nodes([visible, this])),
    ?assert(is_list(Tasks)),
    ?assertEqual([], Tasks).

t_one_task(_) ->
    [Pid] = spawn_link_some_tasks(1),
    [Task] = chttpd_util:get_active_tasks(nodes([visible, this])),
    ThisNode = node(),
    ?assertMatch({[{node, ThisNode}, {pid, <<_/binary>>}, {_, _} | _]}, Task),
    kill_tasks([Pid]).

t_multiple_tasks(_) ->
    N = 1000,
    Pids = spawn_link_some_tasks(N),
    Tasks = chttpd_util:get_active_tasks(nodes([visible, this])),
    ?assert(is_list(Tasks)),
    ?assertEqual(N, length(Tasks)),
    ThisNode = node(),
    lists:map(
        fun(T) ->
            ?assertMatch({[{node, ThisNode}, {pid, <<_/binary>>}, {_, _} | _]}, T)
        end,
        Tasks
    ),
    kill_tasks(Pids).

t_ignore_errors(_) ->
    [Pid] = spawn_link_some_tasks(1),
    ThisNode = node(),
    Nodes = [node(), 'bad@foobar'],
    [Task] = chttpd_util:get_active_tasks(Nodes),
    ?assertMatch({[{node, ThisNode}, {pid, <<_/binary>>}, {_, _} | _]}, Task),
    kill_tasks([Pid]).

t_rolling_upgrade_test(_) ->
    % Previously, in releases =< 3.3.2, we used to fetch active tasks with:
    %   gen_server:multi_call(couch_task_status, all)
    % Here we test the rolling upgrade case when old nodes would call the
    % the new nodes which were just upgraded. This test should be removed
    % in the future when rolling upgrades from before =< 3.3.2 is not a
    % concern any longer.
    N = 1000,
    Pids = spawn_link_some_tasks(N),
    Tasks = chttpd_util:get_active_tasks(nodes([visible, this])),
    OldTasks = compat_get_active_stats(),
    % There should be no difference between the two
    ?assertEqual(lists:sort(Tasks), lists:sort(OldTasks)),
    kill_tasks(Pids).

compat_get_active_stats() ->
    {Replies, _BadNodes} = gen_server:multi_call(couch_task_status, all),
    lists:flatmap(
        fun({Node, Tasks}) ->
            [{[{node, Node} | Task]} || Task <- Tasks]
        end,
        Replies
    ).

spawn_link_some_tasks(N) ->
    Pids = [spawn_link(fun rand_task_proc/0) || _ <- lists:seq(1, N)],
    [
        begin
            Pid ! {add_task, self()},
            receive
                task_added -> ok
            end
        end
     || Pid <- Pids
    ],
    Pids.

kill_tasks(Pids) ->
    [
        begin
            unlink(Pid),
            exit(Pid, kill)
        end
     || Pid <- Pids
    ],
    ok.

rand_task_proc() ->
    receive
        {add_task, From} ->
            couch_task_status:add_task(rand_task_props()),
            From ! task_added,
            rand_task_proc()
    end.

rand_task_props() ->
    [
        {type, indexer},
        {database, binary:encode_hex(rand:bytes(10))},
        {design_document, binary:encode_hex(rand:bytes(10))},
        {progress, rand:uniform(100)},
        {changes_done, rand:uniform(1000000)},
        {total_changes, rand:uniform(1000000)}
    ].
