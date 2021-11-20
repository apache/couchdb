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

-module(couchdb_os_proc_pool).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, 1000).

setup() ->
    ok = couch_proc_manager:reload(),
    meck:new(couch_os_process, [passthrough]),
    ok = setup_config().

teardown(_) ->
    meck:unload(),
    ok.

os_proc_pool_test_() ->
    {
        "OS processes pool tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    should_block_new_proc_on_full_pool(),
                    should_free_slot_on_proc_unexpected_exit(),
                    should_reuse_known_proc(),
                    %                    should_process_waiting_queue_as_fifo(),
                    should_reduce_pool_on_idle_os_procs(),
                    should_not_return_broken_process_to_the_pool()
                ]
            }
        }
    }.

should_block_new_proc_on_full_pool() ->
    ?_test(begin
        Client1 = spawn_client(),
        Client2 = spawn_client(),
        Client3 = spawn_client(),

        ?assertEqual(ok, ping_client(Client1)),
        ?assertEqual(ok, ping_client(Client2)),
        ?assertEqual(ok, ping_client(Client3)),

        Proc1 = get_client_proc(Client1, "1"),
        Proc2 = get_client_proc(Client2, "2"),
        Proc3 = get_client_proc(Client3, "3"),

        ?assertNotEqual(Proc1, Proc2),
        ?assertNotEqual(Proc2, Proc3),
        ?assertNotEqual(Proc3, Proc1),

        Client4 = spawn_client(),
        ?assertEqual(timeout, ping_client(Client4)),

        ?assertEqual(ok, stop_client(Client1)),
        ?assertEqual(ok, ping_client(Client4)),

        Proc4 = get_client_proc(Client4, "4"),

        ?assertEqual(Proc1#proc.pid, Proc4#proc.pid),
        ?assertNotEqual(Proc1#proc.client, Proc4#proc.client),

        lists:map(
            fun(C) ->
                ?assertEqual(ok, stop_client(C))
            end,
            [Client2, Client3, Client4]
        )
    end).

should_free_slot_on_proc_unexpected_exit() ->
    ?_test(begin
        Client1 = spawn_client(),
        Client2 = spawn_client(),
        Client3 = spawn_client(),

        ?assertEqual(ok, ping_client(Client1)),
        ?assertEqual(ok, ping_client(Client2)),
        ?assertEqual(ok, ping_client(Client3)),

        Proc1 = get_client_proc(Client1, "1"),
        Proc2 = get_client_proc(Client2, "2"),
        Proc3 = get_client_proc(Client3, "3"),

        ?assertNotEqual(Proc1#proc.pid, Proc2#proc.pid),
        ?assertNotEqual(Proc1#proc.client, Proc2#proc.client),
        ?assertNotEqual(Proc2#proc.pid, Proc3#proc.pid),
        ?assertNotEqual(Proc2#proc.client, Proc3#proc.client),
        ?assertNotEqual(Proc3#proc.pid, Proc1#proc.pid),
        ?assertNotEqual(Proc3#proc.client, Proc1#proc.client),

        ?assertEqual(ok, kill_client(Client1)),

        Client4 = spawn_client(),
        ?assertEqual(ok, ping_client(Client4)),

        Proc4 = get_client_proc(Client4, "4"),

        ?assertEqual(Proc4#proc.pid, Proc1#proc.pid),
        ?assertNotEqual(Proc4#proc.client, Proc1#proc.client),
        ?assertNotEqual(Proc2#proc.pid, Proc4#proc.pid),
        ?assertNotEqual(Proc2#proc.client, Proc4#proc.client),
        ?assertNotEqual(Proc3#proc.pid, Proc4#proc.pid),
        ?assertNotEqual(Proc3#proc.client, Proc4#proc.client),

        lists:map(
            fun(C) ->
                ?assertEqual(ok, stop_client(C))
            end,
            [Client2, Client3, Client4]
        )
    end).

should_reuse_known_proc() ->
    ?_test(begin
        Client1 = spawn_client(<<"ddoc1">>),
        Client2 = spawn_client(<<"ddoc2">>),

        ?assertEqual(ok, ping_client(Client1)),
        ?assertEqual(ok, ping_client(Client2)),

        Proc1 = get_client_proc(Client1, "1"),
        Proc2 = get_client_proc(Client2, "2"),
        ?assertNotEqual(Proc1#proc.pid, Proc2#proc.pid),

        ?assertEqual(ok, stop_client(Client1)),
        ?assertEqual(ok, stop_client(Client2)),
        ?assert(is_process_alive(Proc1#proc.pid)),
        ?assert(is_process_alive(Proc2#proc.pid)),

        Client1Again = spawn_client(<<"ddoc1">>),
        ?assertEqual(ok, ping_client(Client1Again)),
        Proc1Again = get_client_proc(Client1Again, "1-again"),
        ?assertEqual(Proc1#proc.pid, Proc1Again#proc.pid),
        ?assertNotEqual(Proc1#proc.client, Proc1Again#proc.client),
        ?assertEqual(ok, stop_client(Client1Again))
    end).

%should_process_waiting_queue_as_fifo() ->
%    ?_test(begin
%        Client1 = spawn_client(<<"ddoc1">>),
%        Client2 = spawn_client(<<"ddoc2">>),
%        Client3 = spawn_client(<<"ddoc3">>),
%        Client4 = spawn_client(<<"ddoc4">>),
%        timer:sleep(100),
%        Client5 = spawn_client(<<"ddoc5">>),
%
%        ?assertEqual(ok, ping_client(Client1)),
%        ?assertEqual(ok, ping_client(Client2)),
%        ?assertEqual(ok, ping_client(Client3)),
%        ?assertEqual(timeout, ping_client(Client4)),
%        ?assertEqual(timeout, ping_client(Client5)),
%
%        Proc1 = get_client_proc(Client1, "1"),
%        ?assertEqual(ok, stop_client(Client1)),
%        ?assertEqual(ok, ping_client(Client4)),
%        Proc4 = get_client_proc(Client4, "4"),
%
%        ?assertNotEqual(Proc4#proc.client, Proc1#proc.client),
%        ?assertEqual(Proc1#proc.pid, Proc4#proc.pid),
%        ?assertEqual(timeout, ping_client(Client5)),
%
%        ?assertEqual(ok, stop_client(Client2)),
%        ?assertEqual(ok, stop_client(Client3)),
%        ?assertEqual(ok, stop_client(Client4)),
%        ?assertEqual(ok, stop_client(Client5))
%    end).

should_reduce_pool_on_idle_os_procs() ->
    ?_test(begin
        %% os_process_idle_limit is in sec
        config:set(
            "query_server_config",
            "os_process_idle_limit",
            "1",
            false
        ),
        ok = confirm_config("os_process_idle_limit", "1"),

        Client1 = spawn_client(<<"ddoc1">>),
        Client2 = spawn_client(<<"ddoc2">>),
        Client3 = spawn_client(<<"ddoc3">>),

        ?assertEqual(ok, ping_client(Client1)),
        ?assertEqual(ok, ping_client(Client2)),
        ?assertEqual(ok, ping_client(Client3)),

        ?assertEqual(3, couch_proc_manager:get_proc_count()),

        ?assertEqual(ok, stop_client(Client1)),
        ?assertEqual(ok, stop_client(Client2)),
        ?assertEqual(ok, stop_client(Client3)),

        timer:sleep(1200),
        ?assertEqual(1, couch_proc_manager:get_proc_count())
    end).

should_not_return_broken_process_to_the_pool() ->
    ?_test(begin
        config:set(
            "query_server_config",
            "os_process_soft_limit",
            "1",
            false
        ),
        ok = confirm_config("os_process_soft_limit", "1"),

        config:set(
            "query_server_config",
            "os_process_limit",
            "1",
            false
        ),
        ok = confirm_config("os_process_limit", "1"),

        DDoc1 = ddoc(<<"_design/ddoc1">>),

        meck:reset(couch_os_process),

        ?assertEqual(0, couch_proc_manager:get_proc_count()),
        ok = couch_query_servers:with_ddoc_proc(DDoc1, fun(_) -> ok end),
        ?assertEqual(0, meck:num_calls(couch_os_process, stop, 1)),
        ?assertEqual(1, couch_proc_manager:get_proc_count()),

        ?assertError(
            bad,
            couch_query_servers:with_ddoc_proc(DDoc1, fun(_) ->
                error(bad)
            end)
        ),
        ?assertEqual(1, meck:num_calls(couch_os_process, stop, 1)),

        WaitFun = fun() ->
            case couch_proc_manager:get_proc_count() of
                0 -> ok;
                N when is_integer(N), N > 0 -> wait
            end
        end,
        case test_util:wait(WaitFun, 5000) of
            timeout -> error(timeout);
            _ -> ok
        end,
        ?assertEqual(0, couch_proc_manager:get_proc_count()),

        DDoc2 = ddoc(<<"_design/ddoc2">>),
        ok = couch_query_servers:with_ddoc_proc(DDoc2, fun(_) -> ok end),
        ?assertEqual(1, meck:num_calls(couch_os_process, stop, 1)),
        ?assertEqual(1, couch_proc_manager:get_proc_count())
    end).

ddoc(DDocId) ->
    #doc{
        id = DDocId,
        revs = {1, [<<"abc">>]},
        body =
            {[
                {<<"language">>, <<"javascript">>},
                {<<"views">>,
                    {[
                        {<<"v1">>,
                            {[
                                {<<"map">>, <<"function(doc) {emit(doc.value,1);}">>}
                            ]}}
                    ]}}
            ]}
    }.

setup_config() ->
    config:set("native_query_servers", "enable_erlang_query_server", "true", false),
    config:set("query_server_config", "os_process_limit", "3", false),
    config:set("query_server_config", "os_process_soft_limit", "2", false),
    ok = confirm_config("os_process_soft_limit", "2").

confirm_config(Key, Value) ->
    confirm_config(Key, Value, 0).

confirm_config(Key, Value, Count) ->
    case config:get("query_server_config", Key) of
        Value ->
            ok;
        _ when Count > 10 ->
            erlang:error(
                {config_setup, [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {value, timeout}
                ]}
            );
        _ ->
            %% we need to wait to let gen_server:cast finish
            timer:sleep(10),
            confirm_config(Key, Value, Count + 1)
    end.

spawn_client() ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        Proc = couch_query_servers:get_os_process(<<"erlang">>),
        loop(Parent, Ref, Proc)
    end),
    {Pid, Ref}.

spawn_client(DDocId) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        DDocKey = {DDocId, <<"1-abcdefgh">>},
        DDoc = #doc{body = {[{<<"language">>, <<"erlang">>}]}},
        Proc = couch_query_servers:get_ddoc_process(DDoc, DDocKey),
        loop(Parent, Ref, Proc)
    end),
    {Pid, Ref}.

ping_client({Pid, Ref}) ->
    Pid ! ping,
    receive
        {pong, Ref} ->
            ok
    after ?TIMEOUT ->
        timeout
    end.

get_client_proc({Pid, Ref}, ClientName) ->
    Pid ! get_proc,
    receive
        {proc, Ref, Proc} -> Proc
    after ?TIMEOUT ->
        erlang:error(
            {assertion_failed, [
                {module, ?MODULE},
                {line, ?LINE},
                {reason,
                    "Timeout getting client " ++
                        ClientName ++ " proc"}
            ]}
        )
    end.

stop_client({Pid, Ref}) ->
    Pid ! stop,
    receive
        {stop, Ref} ->
            ok
    after ?TIMEOUT ->
        timeout
    end.

kill_client({Pid, Ref}) ->
    Pid ! die,
    receive
        {die, Ref} ->
            ok
    after ?TIMEOUT ->
        timeout
    end.

loop(Parent, Ref, Proc) ->
    receive
        ping ->
            Parent ! {pong, Ref},
            loop(Parent, Ref, Proc);
        get_proc ->
            Parent ! {proc, Ref, Proc},
            loop(Parent, Ref, Proc);
        stop ->
            couch_query_servers:ret_os_process(Proc),
            Parent ! {stop, Ref};
        die ->
            Parent ! {die, Ref},
            exit(some_error)
    end.
