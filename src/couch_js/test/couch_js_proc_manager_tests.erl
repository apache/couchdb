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

-module(couch_js_proc_manager_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(TDEF(A), {atom_to_list(A), fun A/0}).

-define(NUM_PROCS, 3).
-define(TIMEOUT, 1000).

-define(TIMEOUT_ERROR(Msg), erlang:error({assertion_failed, [
        {module, ?MODULE},
        {line, ?LINE},
        {reason, Msg}
    ]})).


start() ->
    ok = application:set_env(config, ini_files, ?CONFIG_CHAIN),
    {ok, Started} = application:ensure_all_started(couch_js),
    config:set("native_query_servers", "enable_erlang_query_server", "true", false),
    config:set("query_server_config", "os_process_limit", "3", false),
    config:set("query_server_config", "os_process_soft_limit", "2", false),
    config:set("query_server_config", "os_process_idle_limit", "1", false),
    ok = config_wait("os_process_idle_limit", "1"),
    Started.


stop(Apps) ->
    lists:foreach(fun(App) ->
        ok = application:stop(App)
    end, lists:reverse(Apps)).


couch_js_proc_manager_test_() ->
    {
        "couch_js_proc_manger tests",
        {
            setup,
            fun start/0,
            fun stop/1,
            [
                ?TDEF(should_block_new_proc_on_full_pool),
                ?TDEF(should_free_slot_on_proc_unexpected_exit),
                ?TDEF(should_reuse_known_proc),
                ?TDEF(should_process_waiting_queue_as_fifo),
                ?TDEF(should_reduce_pool_on_idle_os_procs)
            ]
        }
    }.


should_block_new_proc_on_full_pool() ->
    ok = couch_js_proc_manager:reload(),

    Clients = [
        spawn_client(),
        spawn_client(),
        spawn_client()
    ],

    lists:foreach(fun(Client) ->
        ?assertEqual(ok, ping_client(Client))
    end, Clients),

    % Make sure everyone got a different proc
    Procs = [get_client_proc(Client) || Client <- Clients],
    ?assertEqual(lists:sort(Procs), lists:usort(Procs)),

    % This client will be stuck waiting for someone
    % to give up their proc.
    Client4 = spawn_client(),
    ?assert(is_client_waiting(Client4)),

    Client1 = hd(Clients),
    Proc1 = hd(Procs),

    ?assertEqual(ok, stop_client(Client1)),
    ?assertEqual(ok, ping_client(Client4)),

    Proc4 = get_client_proc(Client4),

    ?assertEqual(Proc1#proc.pid, Proc4#proc.pid),
    ?assertNotEqual(Proc1#proc.client, Proc4#proc.client),

    lists:map(fun(C) ->
        ?assertEqual(ok, stop_client(C))
    end, [Client4 | tl(Clients)]).


should_free_slot_on_proc_unexpected_exit() ->
    ok = couch_js_proc_manager:reload(),

    Clients = [
        spawn_client(),
        spawn_client(),
        spawn_client()
    ],

    lists:foreach(fun(Client) ->
        ?assertEqual(ok, ping_client(Client))
    end, Clients),

    Procs1 = [get_client_proc(Client) || Client <- Clients],
    ProcClients1 = [Proc#proc.client || Proc <- Procs1],
    ?assertEqual(lists:sort(Procs1), lists:usort(Procs1)),
    ?assertEqual(lists:sort(ProcClients1), lists:usort(ProcClients1)),

    Client1 = hd(Clients),
    Proc1 = hd(Procs1),
    ?assertEqual(ok, kill_client(Client1)),

    Client4 = spawn_client(),
    ?assertEqual(ok, ping_client(Client4)),
    Proc4 = get_client_proc(Client4),

    ?assertEqual(Proc1#proc.pid, Proc4#proc.pid),
    ?assertNotEqual(Proc1#proc.client, Proc4#proc.client),

    Procs2 = [Proc4 | tl(Procs1)],
    ProcClients2 = [Proc4#proc.client | tl(ProcClients1)],
    ?assertEqual(lists:sort(Procs2), lists:usort(Procs2)),
    ?assertEqual(lists:sort(ProcClients2), lists:usort(ProcClients2)),

    lists:map(fun(C) ->
        ?assertEqual(ok, stop_client(C))
    end, [Client4 | tl(Clients)]).


should_reuse_known_proc() ->
    ok = couch_js_proc_manager:reload(),

    Clients = [
        spawn_client(<<"ddoc1">>),
        spawn_client(<<"ddoc2">>)
    ],

    lists:foreach(fun(Client) ->
        ?assertEqual(ok, ping_client(Client))
    end, Clients),

    Procs = [get_client_proc(Client) || Client <- Clients],
    ?assertEqual(lists:sort(Procs), lists:usort(Procs)),

    lists:foreach(fun(Client) ->
        ?assertEqual(ok, stop_client(Client))
    end, Clients),

    lists:foreach(fun(Proc) ->
        ?assert(is_process_alive(Proc#proc.pid))
    end, Procs),

    Client = spawn_client(<<"ddoc1">>),
    ?assertEqual(ok, ping_client(Client)),

    OldProc = hd(Procs),
    NewProc = get_client_proc(Client),

    ?assertEqual(OldProc#proc.pid, NewProc#proc.pid),
    ?assertNotEqual(OldProc#proc.client, NewProc#proc.client),
    ?assertEqual(ok, stop_client(Client)).


should_process_waiting_queue_as_fifo() ->
    Clients = [
        spawn_client(<<"ddoc1">>),
        spawn_client(<<"ddoc2">>),
        spawn_client(<<"ddoc3">>),
        spawn_client(<<"ddoc4">>),
        spawn_client(<<"ddoc5">>),
        spawn_client(<<"ddoc6">>)
    ],

    lists:foldl(fun(Client, Pos) ->
        case Pos =< ?NUM_PROCS of
            true ->
                ?assertEqual(ok, ping_client(Client));
            false ->
                ?assert(is_client_waiting(Client))
        end,
        Pos + 1
    end, 1, Clients),

    LastClients = lists:foldl(fun(_Iteration, ClientAcc) ->
        FirstClient = hd(ClientAcc),
        FirstProc = get_client_proc(FirstClient),
        ?assertEqual(ok, stop_client(FirstClient)),

        RestClients = tl(ClientAcc),

        lists:foldl(fun(Client, Pos) ->
            case Pos =< ?NUM_PROCS of
                true ->
                    ?assertEqual(ok, ping_client(Client));
                false ->
                    ?assert(is_client_waiting(Client))
            end,
            if Pos /= ?NUM_PROCS -> ok; true ->
                BubbleProc = get_client_proc(Client),
                ?assertEqual(FirstProc#proc.pid, BubbleProc#proc.pid),
                ?assertNotEqual(FirstProc#proc.client, BubbleProc#proc.client)
            end,
            Pos + 1
        end, 1, RestClients),

        RestClients
    end, Clients, lists:seq(1, 3)),

    lists:foreach(fun(Client) ->
        ?assertEqual(ok, stop_client(Client))
    end, LastClients).


should_reduce_pool_on_idle_os_procs() ->
    Clients = [
        spawn_client(<<"ddoc1">>),
        spawn_client(<<"ddoc2">>),
        spawn_client(<<"ddoc3">>)
    ],

    lists:foreach(fun(Client) ->
        ?assertEqual(ok, ping_client(Client))
    end, Clients),

    ?assertEqual(3, couch_js_proc_manager:get_proc_count()),

    lists:foreach(fun(Client) ->
        ?assertEqual(ok, stop_client(Client))
    end, Clients),

    ?assertEqual(3, couch_js_proc_manager:get_proc_count()),

    timer:sleep(1200),

    ?assertEqual(1, couch_js_proc_manager:get_proc_count()).


spawn_client() ->
    Parent = self(),
    Ref = make_ref(),
    {Pid, _} = spawn_monitor(fun() ->
        Parent ! {self(), initialized},
        Proc = couch_js_query_servers:get_os_process(<<"erlang">>),
        loop(Parent, Ref, Proc)
    end),
    receive
        {Pid, initialized} ->
            ok
    after ?TIMEOUT ->
        ?TIMEOUT_ERROR("Error creating client.")
    end,
    {Pid, Ref}.


spawn_client(DDocId) ->
    Parent = self(),
    Ref = make_ref(),
    {Pid, _} = spawn_monitor(fun() ->
        DDocKey = {DDocId, <<"1-abcdefgh">>},
        DDoc = #doc{body={[{<<"language">>, <<"erlang">>}]}},
        Parent ! {self(), initialized},
        Proc = couch_js_query_servers:get_ddoc_process(DDoc, DDocKey),
        loop(Parent, Ref, Proc)
    end),
    receive
        {Pid, initialized} ->
            ok
    after ?TIMEOUT ->
        ?TIMEOUT_ERROR("Error creating ddoc client.")
    end,
    {Pid, Ref}.


loop(Parent, Ref, Proc) ->
    receive
        ping ->
            Parent ! {pong, Ref},
            loop(Parent, Ref, Proc);
        get_proc  ->
            Parent ! {proc, Ref, Proc},
            loop(Parent, Ref, Proc);
        stop ->
            couch_js_query_servers:ret_os_process(Proc),
            Parent ! {stop, Ref};
        die ->
            Parent ! {die, Ref},
            exit(some_error)
    end.


ping_client({Pid, Ref}) ->
    Pid ! ping,
    receive
        {pong, Ref} ->
            ok
    after ?TIMEOUT ->
        ?TIMEOUT_ERROR("Timeout pinging client")
    end.


is_client_waiting({Pid, _Ref}) ->
    {status, Status} = process_info(Pid, status),
    {current_function, {M, F, A}} = process_info(Pid, current_function),
    Status == waiting andalso {M, F, A} == {gen, do_call, 4}.


get_client_proc({Pid, Ref}) ->
    Pid ! get_proc,
    receive
        {proc, Ref, Proc} -> Proc
    after ?TIMEOUT ->
        ?TIMEOUT_ERROR("Timeout getting proc from client")
    end.


stop_client({Pid, Ref}) ->
    Pid ! stop,
    receive
        {stop, Ref} ->
            ok
    after ?TIMEOUT ->
        ?TIMEOUT_ERROR("Timeout stopping client")
    end,
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        ?TIMEOUT_ERROR("Timeout waiting for stopped client 'DOWN'")
    end.


kill_client({Pid, Ref}) ->
    Pid ! die,
    receive
        {die, Ref} ->
            ok
    after ?TIMEOUT ->
        ?TIMEOUT_ERROR("Timeout killing client")
    end,
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        ?TIMEOUT_ERROR("Timeout waiting for killed client 'DOWN'")
    end.


config_wait(Key, Value) ->
    config_wait(Key, Value, 0).

config_wait(Key, Value, Count) ->
    case config:get("query_server_config", Key) of
        Value ->
            ok;
        _ when Count > 10 ->
            ?TIMEOUT_ERROR("Error waiting for config changes.");
        _ ->
            timer:sleep(10),
            config_wait(Key, Value, Count + 1)
    end.
