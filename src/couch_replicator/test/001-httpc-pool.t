#!/usr/bin/env escript
%% -*- erlang -*-
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

main(_) ->
    test_util:init_code_path(),

    etap:plan(55),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.


test() ->
    couch_server_sup:start_link(test_util:config_files()),
    ibrowse:start(),

    test_pool_full(),
    test_worker_dead_pool_non_full(),
    test_worker_dead_pool_full(),

    couch_server_sup:stop(),
    ok.


test_pool_full() ->
    Pool = spawn_pool(),
    Client1 = spawn_client(Pool),
    Client2 = spawn_client(Pool),
    Client3 = spawn_client(Pool),

    etap:diag("Check that we can spawn the max number of connections."),
    etap:is(ping_client(Client1), ok, "Client 1 started ok."),
    etap:is(ping_client(Client2), ok, "Client 2 started ok."),
    etap:is(ping_client(Client3), ok, "Client 3 started ok."),

    Worker1 = get_client_worker(Client1, "1"),
    Worker2 = get_client_worker(Client2, "2"),
    Worker3 = get_client_worker(Client3, "3"),
    etap:is(is_process_alive(Worker1), true, "Client's 1 worker is alive."),
    etap:is(is_process_alive(Worker2), true, "Client's 2 worker is alive."),
    etap:is(is_process_alive(Worker3), true, "Client's 3 worker is alive."),

    etap:isnt(Worker1, Worker2, "Clients 1 and 2 got different workers."),
    etap:isnt(Worker2, Worker3, "Clients 2 and 3 got different workers."),
    etap:isnt(Worker1, Worker3, "Clients 1 and 3 got different workers."),

    etap:diag("Check that client 4 blocks waiting for a worker."),
    Client4 = spawn_client(Pool),
    etap:is(ping_client(Client4), timeout, "Client 4 blocked while waiting."),

    etap:diag("Check that stopping a client gives up its worker."),
    etap:is(stop_client(Client1), ok, "First client stopped."),

    etap:diag("And check that our blocked client has been unblocked."),
    etap:is(ping_client(Client4), ok, "Client 4 was unblocked."),

    Worker4 = get_client_worker(Client4, "4"),
    etap:is(is_process_alive(Worker4), true, "Client's 4 worker is alive."),
    etap:is(Worker4, Worker1, "Client 4 got worker that client 1 got before."),

    lists:foreach(fun(C) -> ok = stop_client(C) end, [Client2, Client3, Client4]),
    stop_pool(Pool).


test_worker_dead_pool_non_full() ->
    Pool = spawn_pool(),
    Client1 = spawn_client(Pool),

    etap:is(ping_client(Client1), ok, "Client 1 started ok."),
    Worker1 = get_client_worker(Client1, "1"),
    etap:is(is_process_alive(Worker1), true, "Client's 1 worker is alive."),

    etap:diag("Kill client's 1 worker."),
    etap:is(kill_client_worker(Client1), ok, "Killed client's 1 worker."),
    etap:is(is_process_alive(Worker1), false, "Client's 1 worker process is dead."),

    etap:is(stop_client(Client1), ok, "First client stopped and released its worker."),

    Client2 = spawn_client(Pool),
    etap:is(ping_client(Client2), ok, "Client 2 started ok."),
    Worker2 = get_client_worker(Client2, "2"),
    etap:isnt(Worker2, Worker1, "Client 2 got a different worker from client 1"),
    etap:is(is_process_alive(Worker2), true, "Client's 2 worker is alive."),

    etap:is(stop_client(Client2), ok, "Second client stopped."),
    stop_pool(Pool).


test_worker_dead_pool_full() ->
    Pool = spawn_pool(),
    Client1 = spawn_client(Pool),
    Client2 = spawn_client(Pool),
    Client3 = spawn_client(Pool),

    etap:diag("Check that we can spawn the max number of connections."),
    etap:is(ping_client(Client1), ok, "Client 1 started ok."),
    etap:is(ping_client(Client2), ok, "Client 2 started ok."),
    etap:is(ping_client(Client3), ok, "Client 3 started ok."),

    Worker1 = get_client_worker(Client1, "1"),
    Worker2 = get_client_worker(Client2, "2"),
    Worker3 = get_client_worker(Client3, "3"),
    etap:is(is_process_alive(Worker1), true, "Client's 1 worker is alive."),
    etap:is(is_process_alive(Worker2), true, "Client's 2 worker is alive."),
    etap:is(is_process_alive(Worker3), true, "Client's 3 worker is alive."),

    etap:isnt(Worker1, Worker2, "Clients 1 and 2 got different workers."),
    etap:isnt(Worker2, Worker3, "Clients 2 and 3 got different workers."),
    etap:isnt(Worker1, Worker3, "Clients 1 and 3 got different workers."),

    etap:diag("Check that client 4 blocks waiting for a worker."),
    Client4 = spawn_client(Pool),
    etap:is(ping_client(Client4), timeout, "Client 4 blocked while waiting."),

    etap:diag("Kill client's 1 worker."),
    etap:is(kill_client_worker(Client1), ok, "Killed client's 1 worker."),
    etap:is(is_process_alive(Worker1), false, "Client's 1 worker process is dead."),

    etap:diag("Check client 4 got unblocked after first worker's death"),
    etap:is(ping_client(Client4), ok, "Client 4 not blocked anymore."),

    Worker4 = get_client_worker(Client4, "4"),
    etap:is(is_process_alive(Worker4), true, "Client's 4 worker is alive."),
    etap:isnt(Worker4, Worker1, "Client 4 got a worker different from client 1."),
    etap:isnt(Worker4, Worker2, "Client 4 got a worker different from client 2."),
    etap:isnt(Worker4, Worker3, "Client 4 got a worker different from client 3."),

    etap:diag("Check that stopping client 1 is a noop."),
    etap:is(stop_client(Client1), ok, "First client stopped."),

    etap:is(is_process_alive(Worker2), true, "Client's 2 worker still alive."),
    etap:is(is_process_alive(Worker3), true, "Client's 3 worker still alive."),
    etap:is(is_process_alive(Worker4), true, "Client's 4 worker still alive."),

    etap:diag("Check that client 5 blocks waiting for a worker."),
    Client5 = spawn_client(Pool),
    etap:is(ping_client(Client5), timeout, "Client 5 blocked while waiting."),

    etap:diag("Check that stopping client 2 gives up its worker."),
    etap:is(stop_client(Client2), ok, "Second client stopped."),

    etap:diag("Now check that client 5 has been unblocked."),
    etap:is(ping_client(Client5), ok, "Client 5 was unblocked."),

    Worker5 = get_client_worker(Client5, "5"),
    etap:is(is_process_alive(Worker5), true, "Client's 5 worker is alive."),
    etap:isnt(Worker5, Worker1, "Client 5 got a worker different from client 1."),
    etap:is(Worker5, Worker2, "Client 5 got same worker as client 2."),
    etap:isnt(Worker5, Worker3, "Client 5 got a worker different from client 3."),
    etap:isnt(Worker5, Worker4, "Client 5 got a worker different from client 4."),

    etap:is(is_process_alive(Worker3), true, "Client's 3 worker still alive."),
    etap:is(is_process_alive(Worker4), true, "Client's 4 worker still alive."),
    etap:is(is_process_alive(Worker5), true, "Client's 5 worker still alive."),

    lists:foreach(fun(C) -> ok = stop_client(C) end, [Client3, Client4, Client5]),
    stop_pool(Pool).


spawn_client(Pool) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        {ok, Worker} = couch_httpc_pool:get_worker(Pool),
        loop(Parent, Ref, Worker, Pool)
    end),
    {Pid, Ref}.


ping_client({Pid, Ref}) ->
    Pid ! ping,
    receive
        {pong, Ref} ->
            ok
    after 3000 ->
        timeout
    end.


get_client_worker({Pid, Ref}, ClientName) ->
    Pid ! get_worker,
    receive
        {worker, Ref, Worker} ->
            Worker
    after 3000 ->
        etap:bail("Timeout getting client " ++ ClientName ++ " worker.")
    end.


stop_client({Pid, Ref}) ->
    Pid ! stop,
    receive
        {stop, Ref} ->
            ok
    after 3000 ->
        timeout
    end.


kill_client_worker({Pid, Ref}) ->
    Pid ! get_worker,
    receive
        {worker, Ref, Worker} ->
            exit(Worker, kill),
            ok
    after 3000 ->
        timeout
    end.


loop(Parent, Ref, Worker, Pool) ->
    receive
        ping ->
            Parent ! {pong, Ref},
            loop(Parent, Ref, Worker, Pool);
        get_worker  ->
            Parent ! {worker, Ref, Worker},
            loop(Parent, Ref, Worker, Pool);
        stop ->
            couch_httpc_pool:release_worker(Pool, Worker),
            Parent ! {stop, Ref}
    end.


spawn_pool() ->
    Host = couch_config:get("httpd", "bind_address", "127.0.0.1"),
    Port = couch_config:get("httpd", "port", "5984"),
    {ok, Pool} = couch_httpc_pool:start_link(
        "http://" ++ Host ++ ":5984", [{max_connections, 3}]),
    Pool.


stop_pool(Pool) ->
    ok = couch_httpc_pool:stop(Pool).
