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

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(TIMEOUT, 3000).


start() ->
    {ok, Pid} = couch_server_sup:start_link(?CONFIG_CHAIN),
    couch_config:set("query_server_config", "os_process_limit", "3", false),
    Pid.

stop(Pid) ->
    couch_server_sup:stop(),
    erlang:monitor(process, Pid),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, server_stop})
    end.


os_proc_pool_test_() ->
    {
        "OS processes pool tests",
        {
            setup,
            fun start/0, fun stop/1,
            [
                should_block_new_proc_on_full_pool(),
                should_free_slot_on_proc_unexpected_exit()
            ]
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
        ?assertEqual(Proc1, Proc4),

        lists:map(fun(C) ->
            ?assertEqual(ok, stop_client(C))
        end, [Client2, Client3, Client4])
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

        ?assertNotEqual(Proc1, Proc2),
        ?assertNotEqual(Proc2, Proc3),
        ?assertNotEqual(Proc3, Proc1),

        ?assertEqual(ok, kill_client(Client1)),

        Client4 = spawn_client(),
        ?assertEqual(ok, ping_client(Client4)),

        Proc4 = get_client_proc(Client4, "4"),
        ?assertNotEqual(Proc4, Proc1),
        ?assertNotEqual(Proc2, Proc4),
        ?assertNotEqual(Proc3, Proc4),

        lists:map(fun(C) ->
            ?assertEqual(ok, stop_client(C))
        end, [Client2, Client3, Client4])
    end).


spawn_client() ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
        Proc = couch_query_servers:get_os_process(<<"javascript">>),
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
        erlang:error({assertion_failed,
                     [{module, ?MODULE},
                      {line, ?LINE},
                      {reason, "Timeout getting client "
                               ++ ClientName ++ " proc"}]})
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
        get_proc  ->
            Parent ! {proc, Ref, Proc},
            loop(Parent, Ref, Proc);
        stop ->
            couch_query_servers:ret_os_process(Proc),
            Parent ! {stop, Ref};
        die ->
            Parent ! {die, Ref},
            exit(some_error)
    end.
