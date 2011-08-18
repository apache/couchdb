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

    etap:plan(21),
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
    couch_config:set("query_server_config", "os_process_limit", "3", false),

    test_pool_full(),
    test_client_unexpected_exit(),

    couch_server_sup:stop(),
    ok.


test_pool_full() ->
    Client1 = spawn_client(),
    Client2 = spawn_client(),
    Client3 = spawn_client(),

    etap:diag("Check that we can spawn the max number of processes."),
    etap:is(ping_client(Client1), ok, "Client 1 started ok."),
    etap:is(ping_client(Client2), ok, "Client 2 started ok."),
    etap:is(ping_client(Client3), ok, "Client 3 started ok."),

    Proc1 = get_client_proc(Client1, "1"),
    Proc2 = get_client_proc(Client2, "2"),
    Proc3 = get_client_proc(Client3, "3"),
    etap:isnt(Proc1, Proc2, "Clients 1 and 2 got different procs."),
    etap:isnt(Proc2, Proc3, "Clients 2 and 3 got different procs."),
    etap:isnt(Proc1, Proc3, "Clients 1 and 3 got different procs."),

    etap:diag("Check that client 4 blocks waiting for a process."),
    Client4 = spawn_client(),
    etap:is(ping_client(Client4), timeout, "Client 4 blocked while waiting."),

    etap:diag("Check that stopping a client gives up its process."),
    etap:is(stop_client(Client1), ok, "First client stopped."),

    etap:diag("And check that our blocked process has been unblocked."),
    etap:is(ping_client(Client4), ok, "Client was unblocked."),

    Proc4 = get_client_proc(Client4, "4"),
    etap:is(Proc4, Proc1, "Client 4 got proc that client 1 got before."),

    lists:map(fun(C) -> ok = stop_client(C) end, [Client2, Client3, Client4]).


test_client_unexpected_exit() ->
    Client1 = spawn_client(),
    Client2 = spawn_client(),
    Client3 = spawn_client(),

    etap:diag("Check that up to os_process_limit clients started."),
    etap:is(ping_client(Client1), ok, "Client 1 started ok."),
    etap:is(ping_client(Client2), ok, "Client 2 started ok."),
    etap:is(ping_client(Client3), ok, "Client 3 started ok."),

    Proc1 = get_client_proc(Client1, "1"),
    Proc2 = get_client_proc(Client2, "2"),
    Proc3 = get_client_proc(Client3, "3"),
    etap:isnt(Proc1, Proc2, "Clients 1 and 2 got different procs."),
    etap:isnt(Proc2, Proc3, "Clients 2 and 3 got different procs."),
    etap:isnt(Proc1, Proc3, "Clients 1 and 3 got different procs."),

    etap:diag("Check that killing a client frees an os_process."),
    etap:is(kill_client(Client1), ok, "Client 1 died all right."),

    etap:diag("Check that a new client is not blocked on boot."),
    Client4 = spawn_client(),
    etap:is(ping_client(Client4), ok, "New client booted without blocking."),

    Proc4 = get_client_proc(Client4, "4"),
    etap:isnt(Proc4, Proc1,
        "Client 4 got a proc different from the one client 1 got before."),
    etap:isnt(Proc4, Proc2, "Client 4's proc different from client 2's proc."),
    etap:isnt(Proc4, Proc3, "Client 4's proc different from client 3's proc."),

    lists:map(fun(C) -> ok = stop_client(C) end, [Client2, Client3, Client4]).


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
        {pong, Ref} -> ok
        after 3000 -> timeout
    end.


get_client_proc({Pid, Ref}, ClientName) ->
    Pid ! get_proc,
    receive
        {proc, Ref, Proc} -> Proc
    after 3000 ->
        etap:bail("Timeout getting client " ++ ClientName ++ " proc.")
    end.


stop_client({Pid, Ref}) ->
    Pid ! stop,
    receive
        {stop, Ref} -> ok
        after 3000 -> timeout
    end.


kill_client({Pid, Ref}) ->
    Pid ! die,
    receive
        {die, Ref} -> ok
        after 3000 -> timeout
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
