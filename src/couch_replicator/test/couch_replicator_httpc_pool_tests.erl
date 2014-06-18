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

-module(couch_replicator_httpc_pool_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(ADMIN_USER, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).
-define(TIMEOUT, 1000).


start() ->
    {ok, Pid} = couch_server_sup:start_link(?CONFIG_CHAIN),
    Pid.

stop(Pid) ->
    erlang:monitor(process, Pid),
    couch_server_sup:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, server_stop})
    end.

setup() ->
    spawn_pool().

teardown(Pool) ->
    stop_pool(Pool).


httpc_pool_test_() ->
    {
        "httpc pool tests",
        {
            setup,
            fun start/0, fun stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_block_new_clients_when_full/1,
                    fun should_replace_worker_on_death/1
                ]
            }
        }
    }.


should_block_new_clients_when_full(Pool) ->
    ?_test(begin
        Client1 = spawn_client(Pool),
        Client2 = spawn_client(Pool),
        Client3 = spawn_client(Pool),

        ?assertEqual(ok, ping_client(Client1)),
        ?assertEqual(ok, ping_client(Client2)),
        ?assertEqual(ok, ping_client(Client3)),

        Worker1 = get_client_worker(Client1, "1"),
        Worker2 = get_client_worker(Client2, "2"),
        Worker3 = get_client_worker(Client3, "3"),

        ?assert(is_process_alive(Worker1)),
        ?assert(is_process_alive(Worker2)),
        ?assert(is_process_alive(Worker3)),

        ?assertNotEqual(Worker1, Worker2),
        ?assertNotEqual(Worker2, Worker3),
        ?assertNotEqual(Worker3, Worker1),

        Client4 = spawn_client(Pool),
        ?assertEqual(timeout, ping_client(Client4)),

        ?assertEqual(ok, stop_client(Client1)),
        ?assertEqual(ok, ping_client(Client4)),

        Worker4 = get_client_worker(Client4, "4"),
        ?assertEqual(Worker1, Worker4),

        lists:foreach(
            fun(C) ->
                ?assertEqual(ok, stop_client(C))
            end, [Client2, Client3, Client4])
    end).

should_replace_worker_on_death(Pool) ->
    ?_test(begin
        Client1 = spawn_client(Pool),
        ?assertEqual(ok, ping_client(Client1)),
        Worker1 = get_client_worker(Client1, "1"),
        ?assert(is_process_alive(Worker1)),

        ?assertEqual(ok, kill_client_worker(Client1)),
        ?assertNot(is_process_alive(Worker1)),
        ?assertEqual(ok, stop_client(Client1)),

        Client2 = spawn_client(Pool),
        ?assertEqual(ok, ping_client(Client2)),
        Worker2 = get_client_worker(Client2, "2"),
        ?assert(is_process_alive(Worker2)),

        ?assertNotEqual(Worker1, Worker2),
        ?assertEqual(ok, stop_client(Client2))
    end).


spawn_client(Pool) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        {ok, Worker} = couch_replicator_httpc_pool:get_worker(Pool),
        loop(Parent, Ref, Worker, Pool)
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

get_client_worker({Pid, Ref}, ClientName) ->
    Pid ! get_worker,
    receive
        {worker, Ref, Worker} ->
            Worker
    after ?TIMEOUT ->
        erlang:error(
            {assertion_failed,
             [{module, ?MODULE}, {line, ?LINE},
              {reason, "Timeout getting client " ++ ClientName ++ " worker"}]})
    end.

stop_client({Pid, Ref}) ->
    Pid ! stop,
    receive
        {stop, Ref} ->
            ok
    after ?TIMEOUT ->
        timeout
    end.

kill_client_worker({Pid, Ref}) ->
    Pid ! get_worker,
    receive
        {worker, Ref, Worker} ->
            exit(Worker, kill),
            ok
    after ?TIMEOUT ->
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
            couch_replicator_httpc_pool:release_worker(Pool, Worker),
            Parent ! {stop, Ref}
    end.

spawn_pool() ->
    Host = couch_config:get("httpd", "bind_address", "127.0.0.1"),
    Port = couch_config:get("httpd", "port", "5984"),
    {ok, Pool} = couch_replicator_httpc_pool:start_link(
        "http://" ++ Host ++ ":" ++ Port, [{max_connections, 3}]),
    Pool.

stop_pool(Pool) ->
    ok = couch_replicator_httpc_pool:stop(Pool).
