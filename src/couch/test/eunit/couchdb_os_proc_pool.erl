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
    Ctx = test_util:start_couch(),
    meck:new(couch_os_process, [passthrough]),
    meck:new(couch_proc_manager, [passthrough]),
    ok = setup_config(),
    Ctx.

teardown(Ctx) ->
    ok = teardown_config(),
    meck:unload(),
    test_util:stop_couch(Ctx),
    ok.

os_proc_pool_test_() ->
    {
        "OS processes pool tests",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(should_block_new_proc_on_full_pool),
                ?TDEF_FE(should_free_slot_on_proc_unexpected_exit),
                ?TDEF_FE(should_reuse_known_proc),
                ?TDEF_FE(should_process_waiting_queue_as_fifo),
                ?TDEF_FE(should_reduce_pool_on_idle_os_procs),
                ?TDEF_FE(should_reduce_pool_of_tagged_processes_on_idle),
                ?TDEF_FE(should_not_return_broken_process_to_the_pool),
                ?TDEF_FE(oldest_tagged_process_is_reaped),
                ?TDEF_FE(untagged_process_is_replenished),
                ?TDEF_FE(exact_ddoc_tagged_process_is_picked_first),
                ?TDEF_FE(db_tagged_process_is_second_choice),
                ?TDEF_FE(if_no_tagged_process_found_new_must_be_spawned),
                ?TDEF_FE(db_tag_none_works),
                ?TDEF_FE(stale_procs_are_cleaned),
                ?TDEF_FE(bad_query_language)
            ]
        }
    }.

should_block_new_proc_on_full_pool(_) ->
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

    stop_clients([Client2, Client3, Client4]).

should_free_slot_on_proc_unexpected_exit(_) ->
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

    stop_clients([Client2, Client3, Client4]).

should_reuse_known_proc(_) ->
    Db = <<"db">>,
    Client1 = spawn_client(Db, <<"ddoc1">>),
    Client2 = spawn_client(Db, <<"ddoc2">>),

    ?assertEqual(ok, ping_client(Client1)),
    ?assertEqual(ok, ping_client(Client2)),

    Proc1 = get_client_proc(Client1, "1"),
    Proc2 = get_client_proc(Client2, "2"),
    ?assertNotEqual(Proc1#proc.pid, Proc2#proc.pid),

    ?assertEqual(ok, stop_client(Client1)),
    ?assertEqual(ok, stop_client(Client2)),
    ?assert(is_process_alive(Proc1#proc.pid)),
    ?assert(is_process_alive(Proc2#proc.pid)),

    Client1Again = spawn_client(Db, <<"ddoc1">>),
    ?assertEqual(ok, ping_client(Client1Again)),
    Proc1Again = get_client_proc(Client1Again, "1-again"),
    ?assertEqual(Proc1#proc.pid, Proc1Again#proc.pid),
    ?assertNotEqual(Proc1#proc.client, Proc1Again#proc.client),
    ?assertEqual(ok, stop_client(Client1Again)).

should_process_waiting_queue_as_fifo(_) ->
    Db = <<"db">>,
    meck:reset(couch_proc_manager),
    Client1 = spawn_client(Db, <<"ddoc1">>),
    meck:wait(1, couch_proc_manager, handle_call, [{get_proc, '_'}, '_', '_'], 1000),
    Client2 = spawn_client(Db, <<"ddoc2">>),
    meck:wait(2, couch_proc_manager, handle_call, [{get_proc, '_'}, '_', '_'], 1000),
    Client3 = spawn_client(Db, <<"ddoc3">>),
    meck:wait(3, couch_proc_manager, handle_call, [{get_proc, '_'}, '_', '_'], 1000),
    Client4 = spawn_client(Db, <<"ddoc4">>),
    meck:wait(4, couch_proc_manager, handle_call, [{get_proc, '_'}, '_', '_'], 1000),
    Client5 = spawn_client(Db, <<"ddoc5">>),
    meck:wait(5, couch_proc_manager, handle_call, [{get_proc, '_'}, '_', '_'], 1000),

    ?assertEqual(ok, ping_client(Client1)),
    ?assertEqual(ok, ping_client(Client2)),
    ?assertEqual(ok, ping_client(Client3)),
    ?assertEqual(timeout, ping_client(Client4)),
    ?assertEqual(timeout, ping_client(Client5)),

    Proc1 = get_client_proc(Client1, "1"),
    ?assertEqual(ok, stop_client(Client1)),
    ?assertEqual(ok, ping_client(Client4)),
    Proc4 = get_client_proc(Client4, "4"),

    ?assertNotEqual(Proc4#proc.client, Proc1#proc.client),
    ?assertEqual(Proc1#proc.pid, Proc4#proc.pid),
    ?assertEqual(timeout, ping_client(Client5)),

    ?assertEqual(ok, stop_client(Client2)),
    ?assertEqual(ok, stop_client(Client3)),
    ?assertEqual(ok, stop_client(Client4)),
    ?assertEqual(ok, stop_client(Client5)).

should_reduce_pool_on_idle_os_procs(_) ->
    %% os_process_idle_limit is in sec
    cfg_set("os_process_idle_limit", "1"),

    Db = undefined,
    Client1 = spawn_client(Db, <<"ddoc1">>),
    Client2 = spawn_client(Db, <<"ddoc2">>),
    Client3 = spawn_client(Db, <<"ddoc3">>),

    ?assertEqual(ok, ping_client(Client1)),
    ?assertEqual(ok, ping_client(Client2)),
    ?assertEqual(ok, ping_client(Client3)),

    ?assertEqual(3, couch_proc_manager:get_proc_count()),

    ?assertEqual(ok, stop_client(Client1)),
    ?assertEqual(ok, stop_client(Client2)),
    ?assertEqual(ok, stop_client(Client3)),

    % granularity of idle limit is in seconds
    timer:sleep(1000),
    wait_process_count(1).

should_reduce_pool_of_tagged_processes_on_idle(_) ->
    %% os_process_idle_limit is in sec
    cfg_set("os_process_idle_limit", "1"),

    Db = <<"reduce_pool_on_idle_db">>,
    Client1 = spawn_client(Db, <<"ddoc1">>),
    Client2 = spawn_client(Db, <<"ddoc2">>),
    Client3 = spawn_client(Db, <<"ddoc3">>),

    ?assertEqual(ok, ping_client(Client1)),
    ?assertEqual(ok, ping_client(Client2)),
    ?assertEqual(ok, ping_client(Client3)),

    ?assertEqual(3, couch_proc_manager:get_proc_count()),

    stop_clients([Client1, Client2, Client3]),

    timer:sleep(1000),
    wait_process_count(0).

oldest_tagged_process_is_reaped(_) ->
    Client1 = spawn_client(<<"db1">>, <<"ddoc1">>),
    Client2 = spawn_client(<<"db2">>, <<"ddoc1">>),
    Client3 = spawn_client(<<"db3">>, <<"ddoc1">>),

    ?assertEqual(ok, ping_client(Client1)),
    ?assertEqual(ok, ping_client(Client2)),
    ?assertEqual(ok, ping_client(Client3)),

    Proc1 = get_client_proc(Client1, "1"),
    Proc2 = get_client_proc(Client2, "2"),
    Proc3 = get_client_proc(Client3, "3"),

    ?assert(all_alive_all_different([Proc1, Proc2, Proc3])),

    stop_clients([Client1, Client2, Client3]),

    % All procs should be released back into the pool
    wait_tagged_idle_count(3),

    % Processes should be alive
    ?assert(all_alive([Proc1, Proc2, Proc3])),

    % Spawning a new tagged proc with a different tag should kill
    % the oldest unused proc and spawn a new one
    Client4 = spawn_client(<<"db4">>, <<"ddoc1">>),
    ?assertEqual(ok, ping_client(Client4)),
    Proc4 = get_client_proc(Client4, "4"),

    ?assert(all_alive_all_different([Proc2, Proc3, Proc4])),
    ?assertNot(is_process_alive(Proc1#proc.pid)),

    ?assertEqual(ok, stop_client(Client4)).

untagged_process_is_replenished(_) ->
    Client1 = spawn_client(),
    Client2 = spawn_client(),

    ?assertEqual(ok, ping_client(Client1)),
    ?assertEqual(ok, ping_client(Client2)),

    Proc1 = get_client_proc(Client1, "1"),
    Proc2 = get_client_proc(Client2, "2"),

    ?assert(all_alive_all_different([Proc1, Proc2])),

    stop_clients([Client1, Client2]),

    % All procs should be released back into the pool
    % and they are now all untagged idle
    wait_idle_count(2),
    ?assertEqual(0, tagged_idle_count()),

    % Processes should still be alive
    ?assert(all_alive([Proc1, Proc2])),

    % Spawning a new tagged proc should tag one of the procs
    % and also asynchronously replenish the untagged pool
    Client3 = spawn_client(<<"db">>, <<"ddoc1">>),
    ?assertEqual(ok, ping_client(Client3)),

    % The process is one of the previously untagged ones
    Proc3 = get_client_proc(Client3, "3"),
    Pid3 = Proc3#proc.pid,
    ?assert(lists:member(Pid3, proc_pids([Proc1, Proc2]))),

    % wait for replinishment
    wait_idle_count(2),

    ?assertEqual(ok, stop_client(Client3)).

exact_ddoc_tagged_process_is_picked_first(_) ->
    Client1 = spawn_client(<<"db">>, <<"ddoc1">>),
    Client2 = spawn_client(<<"db">>, <<"ddoc2">>),

    ?assertEqual(ok, ping_client(Client1)),
    ?assertEqual(ok, ping_client(Client2)),

    Proc1 = get_client_proc(Client1, "1"),
    Proc2 = get_client_proc(Client2, "2"),

    ?assert(all_alive_all_different([Proc1, Proc2])),

    stop_clients([Client1, Client2]),

    % All procs should be released back into the pool
    % and they now tagged and idle
    wait_tagged_idle_count(2),
    wait_idle_count(2),

    % Processes should still be alive
    ?assert(all_alive([Proc1, Proc2])),

    % Spawning a new tagged proc should pick the one with
    % matching ddoc
    Client3 = spawn_client(<<"db">>, <<"ddoc1">>),
    ?assertEqual(ok, ping_client(Client3)),
    Proc3 = get_client_proc(Client3, "3"),
    ?assertEqual(Proc1#proc.pid, Proc3#proc.pid),

    ?assertEqual(ok, stop_client(Client3)).

db_tagged_process_is_second_choice(_) ->
    Client1 = spawn_client(<<"db1">>, <<"ddoc1">>),
    Client2 = spawn_client(<<"db2">>, <<"ddoc2">>),

    ?assertEqual(ok, ping_client(Client1)),
    ?assertEqual(ok, ping_client(Client2)),

    Proc1 = get_client_proc(Client1, "1"),
    Proc2 = get_client_proc(Client2, "2"),

    ?assert(all_alive_all_different([Proc1, Proc2])),

    stop_clients([Client1, Client2]),

    % All procs should be released back into the pool
    % and they now tagged and idle
    wait_tagged_idle_count(2),
    wait_idle_count(2),

    % Processes should still be alive
    ?assert(all_alive([Proc1, Proc2])),

    % Spawning a new tagged proc should pick the one with
    % the matching ddoc
    Client3 = spawn_client(<<"db1">>, <<"ddoc3">>),
    ?assertEqual(ok, ping_client(Client3)),
    Proc3 = get_client_proc(Client3, "3"),
    ?assertEqual(Proc1#proc.pid, Proc3#proc.pid),

    ?assertEqual(ok, stop_client(Client3)).

if_no_tagged_process_found_new_must_be_spawned(_) ->
    Client1 = spawn_client(<<"db1">>, <<"ddoc">>),
    Client2 = spawn_client(<<"db2">>, <<"ddoc">>),

    ?assertEqual(ok, ping_client(Client1)),
    ?assertEqual(ok, ping_client(Client2)),

    Proc1 = get_client_proc(Client1, "1"),
    Proc2 = get_client_proc(Client2, "2"),

    ?assert(all_alive_all_different([Proc1, Proc2])),

    stop_clients([Client1, Client2]),

    % All procs should be released back into the pool
    % and they now tagged and idle
    wait_tagged_idle_count(2),
    wait_idle_count(2),

    % Processes should still be alive
    ?assert(all_alive([Proc1, Proc2])),

    % If new tagged process with new db should spawn
    % new process never pick up an existing one
    Client3 = spawn_client(<<"db3">>, <<"ddoc">>),
    ?assertEqual(ok, ping_client(Client3)),
    Proc3 = get_client_proc(Client3, "3"),
    ?assertNotEqual(Proc1#proc.pid, Proc3#proc.pid),
    ?assertNotEqual(Proc2#proc.pid, Proc3#proc.pid),

    % db1 and db2 procs should still be sitting idle
    ?assertEqual(2, tagged_idle_count()),

    % After 3rd proc returns to the pool there should
    % be 3 tagged idle processes
    ?assertEqual(ok, stop_client(Client3)),
    wait_tagged_idle_count(3),
    ?assertEqual(3, idle_count()).

db_tag_none_works(_) ->
    cfg_set("db_tag", "none"),
    Client1 = spawn_client(undefined, <<"ddoc1">>),
    Client2 = spawn_client(undefined, <<"ddoc2">>),

    ?assertEqual(ok, ping_client(Client1)),
    ?assertEqual(ok, ping_client(Client2)),

    Proc1 = get_client_proc(Client1, "1"),
    Proc2 = get_client_proc(Client2, "2"),

    ?assert(all_alive_all_different([Proc1, Proc2])),

    stop_clients([Client1, Client2]),

    % All procs should be released back into the pool
    % they should be untagged effectively
    wait_idle_count(2),
    ?assertEqual(0, tagged_idle_count()),

    % Processes should still be alive
    ?assert(all_alive([Proc1, Proc2])),

    % If new tagged process with new db should spawn
    % new process and pick based on ddoc id matching
    Client3 = spawn_client(undefined, <<"ddoc1">>),
    ?assertEqual(ok, ping_client(Client3)),
    Proc3 = get_client_proc(Client3, "3"),
    ?assertEqual(Proc1#proc.pid, Proc3#proc.pid),
    ?assertNotEqual(Proc2#proc.pid, Proc3#proc.pid),

    wait_idle_count(1),

    % After 3rd client stop there should be 2 idle
    % untagged procs
    ?assertEqual(ok, stop_client(Client3)),
    wait_idle_count(2),
    ?assertEqual(0, tagged_idle_count()).

stale_procs_are_cleaned(_) ->
    Client1 = spawn_client(),
    Client2 = spawn_client(),

    ?assertEqual(ok, ping_client(Client1)),
    ?assertEqual(ok, ping_client(Client2)),

    Proc1 = get_client_proc(Client1, "1"),
    Proc2 = get_client_proc(Client2, "2"),

    ?assert(all_alive_all_different([Proc1, Proc2])),

    ?assertEqual(0, couch_proc_manager:get_stale_proc_count()),
    ?assertEqual(ok, couch_proc_manager:reload()),
    ?assertEqual(2, couch_proc_manager:get_stale_proc_count()),

    stop_clients([Client1, Client2]),
    ?assertEqual(ok, couch_proc_manager:terminate_stale_procs()),
    wait_idle_count(0),
    ?assertEqual(0, couch_proc_manager:get_proc_count()).

bad_query_language(_) ->
    Expect = {unknown_query_language, <<"bad">>},
    ?assertThrow(Expect, couch_query_servers:get_os_process(<<"bad">>)).

should_not_return_broken_process_to_the_pool(_) ->
    cfg_set("os_process_soft_limit", "1"),
    cfg_set("os_process_limit", "1"),

    Db = <<"thedb">>,
    DDoc1 = ddoc(<<"_design/ddoc1">>),

    meck:reset(couch_os_process),

    ?assertEqual(0, couch_proc_manager:get_proc_count()),
    ok = couch_query_servers:with_ddoc_proc(Db, DDoc1, fun(_) -> ok end),
    ?assertEqual(0, meck:num_calls(couch_os_process, stop, 1)),
    ?assertEqual(1, couch_proc_manager:get_proc_count()),

    ?assertError(
        bad,
        couch_query_servers:with_ddoc_proc(Db, DDoc1, fun(_) ->
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
    ok = couch_query_servers:with_ddoc_proc(Db, DDoc2, fun(_) -> ok end),
    ?assertEqual(1, meck:num_calls(couch_os_process, stop, 1)),
    ?assertEqual(1, couch_proc_manager:get_proc_count()).

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
    ok.

teardown_config() ->
    config:delete("native_query_servers", "enable_erlang_query_server", false),
    config:delete("query_server_config", "os_process_limit", false),
    config:delete("query_server_config", "os_process_soft_limit", false),
    config:delete("query_server_config", "db_tag", false),
    ok.

spawn_client() ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        Proc = couch_query_servers:get_os_process(<<"erlang">>),
        loop(Parent, Ref, Proc)
    end),
    {Pid, Ref}.

spawn_client(Db, DDocId) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        DDocKey = {DDocId, <<"1-abcdefgh">>},
        DDoc = #doc{body = {[{<<"language">>, <<"erlang">>}]}},
        Proc = couch_query_servers:get_ddoc_process(DDoc, Db, DDocKey),
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
    MRef = erlang:monitor(process, Pid),
    Pid ! stop,
    receive
        {stop, Ref} ->
            receive
                {'DOWN', MRef, process, Pid, _} -> ok
            end,
            ok
    after ?TIMEOUT ->
        erlang:demonitor(MRef, [flush]),
        timeout
    end.

kill_client({Pid, Ref}) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! die,
    receive
        {die, Ref} ->
            receive
                {'DOWN', MRef, process, Pid, _} -> ok
            end,
            ok
    after ?TIMEOUT ->
        erlang:demonitor(MRef, [flush]),
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

proc_pids(Procs) ->
    [P#proc.pid || P <- Procs].

all_alive(Procs) ->
    lists:all(fun is_process_alive/1, proc_pids(Procs)).

all_different(Procs) ->
    lists:usort(proc_pids(Procs)) =:= lists:sort(proc_pids(Procs)).

all_alive_all_different(Procs) ->
    all_alive(Procs) andalso all_different(Procs).

idle_count() ->
    ets:info(couch_proc_manager_idle_by_db, size).

tagged_idle_count() ->
    ets:info(couch_proc_manager_idle_access, size).

stop_clients(Clients) ->
    Fun = fun(C) -> ?assertEqual(ok, stop_client(C)) end,
    lists:map(Fun, Clients).

wait_tagged_idle_count(N) ->
    WaitFun = fun() ->
        case tagged_idle_count() == N of
            true -> ok;
            false -> wait
        end
    end,
    case test_util:wait(WaitFun, 5000) of
        timeout -> error(timeout);
        _ -> ok
    end,
    ?assertEqual(N, tagged_idle_count()).

wait_idle_count(N) ->
    WaitFun = fun() ->
        case idle_count() == N of
            true -> ok;
            false -> wait
        end
    end,
    case test_util:wait(WaitFun, 5000) of
        timeout -> error(timeout);
        _ -> ok
    end,
    ?assertEqual(N, idle_count()).

wait_process_count(N) ->
    WaitFun = fun() ->
        case couch_proc_manager:get_proc_count() == N of
            true -> ok;
            false -> wait
        end
    end,
    case test_util:wait(WaitFun, 5000) of
        timeout -> error(timeout);
        _ -> ok
    end,
    ?assertEqual(N, couch_proc_manager:get_proc_count()).

cfg_set(K, V) ->
    config:set("query_server_config", K, V, false),
    ok.
