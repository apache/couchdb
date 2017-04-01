% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_log_server_test).


-include("couch_log.hrl").
-include_lib("eunit/include/eunit.hrl").


couch_log_server_test_() ->
    {setup,
        fun couch_log_test_util:start/0,
        fun couch_log_test_util:stop/1,
        [
            fun check_can_reconfigure/0,
            fun check_can_restart/0,
            fun check_can_cast_log_entry/0,
            fun check_logs_ignored_messages/0
        ]
    }.


check_can_reconfigure() ->
    couch_log:error("a message", []),
    ?assertEqual(0, couch_log_test_util:last_log_key()),
    ?assertEqual(ok, couch_log_server:reconfigure()),
    ?assertEqual('$end_of_table', couch_log_test_util:last_log_key()),

    couch_log_test_util:with_config_listener(fun() ->
        couch_log:error("another message", []),
        ?assertEqual(0, couch_log_test_util:last_log_key()),
        config:set("log", "some_key", "some_val"),
        couch_log_test_util:wait_for_config(),
        ?assertEqual('$end_of_table', couch_log_test_util:last_log_key())
    end).


check_can_restart() ->
    Pid1 = whereis(couch_log_server),
    Ref = erlang:monitor(process, Pid1),
    ?assert(is_process_alive(Pid1)),

    supervisor:terminate_child(couch_log_sup, couch_log_server),
    supervisor:restart_child(couch_log_sup, couch_log_server),

    receive
        {'DOWN', Ref, _, _, _} -> ok
    after 1000 ->
        erlang:error(timeout_restarting_couch_log_server)
    end,

    ?assert(not is_process_alive(Pid1)),

    Pid2 = whereis(couch_log_server),
    ?assertNotEqual(Pid2, Pid1),
    ?assert(is_process_alive(Pid2)).


check_can_cast_log_entry() ->
    Entry = #log_entry{
        level = critical,
        pid = self(),
        msg = "this will be casted",
        msg_id = "----",
        time_stamp = "2016-07-20-almost-my-birthday"
    },
    ok = gen_server:cast(couch_log_server, {log, Entry}),
    timer:sleep(500), % totes gross
    ?assertEqual(Entry, couch_log_test_util:last_log()).


check_logs_ignored_messages() ->
    gen_server:call(couch_log_server, a_call),
    ?assertMatch(
        #log_entry{
            level = error,
            pid = couch_log_server,
            msg = "couch_log_server ignored a_call"
        },
        couch_log_test_util:last_log()
    ),

    gen_server:cast(couch_log_server, a_cast),
    timer:sleep(500), % yes gross
    ?assertMatch(
        #log_entry{
            level = error,
            pid = couch_log_server,
            msg = "couch_log_server ignored a_cast"
        },
        couch_log_test_util:last_log()
    ),

    couch_log_server ! an_info,
    timer:sleep(500), % still gross
    ?assertMatch(
        #log_entry{
            level = error,
            pid = couch_log_server,
            msg = "couch_log_server ignored an_info"
        },
        couch_log_test_util:last_log()
    ).


coverage_test() ->
    Resp = couch_log_server:code_change(foo, bazinga, baz),
    ?assertEqual({ok, bazinga}, Resp).
