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

-module(couch_replicator_connection_tests).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


httpc_pool_test_() ->
    {
        "Replicator connection sharing tests",
        {
            setup,
            fun couch_replicator_test_helper:start_couch/0,
            fun couch_replicator_test_helper:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(connections_shared_after_release),
                    ?TDEF_FE(connections_not_shared_after_owner_death),
                    ?TDEF_FE(idle_connections_closed),
                    ?TDEF_FE(test_owner_monitors),
                    ?TDEF_FE(worker_discards_creds_on_create),
                    ?TDEF_FE(worker_discards_url_creds_after_request),
                    ?TDEF_FE(worker_discards_creds_in_headers_after_request),
                    ?TDEF_FE(worker_discards_proxy_creds_after_request)
                ]
            }
        }
    }.


setup() ->
    Host = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = config:get("chttpd", "port", "5984"),
    {Host, Port}.


teardown(_) ->
    ok.


connections_shared_after_release({Host, Port}) ->
    URL = "http://" ++ Host ++ ":" ++ Port,
    Self = self(),
    {ok, Pid} = couch_replicator_connection:acquire(URL),
    couch_replicator_connection:release(Pid),
    spawn(fun() ->
        Self ! couch_replicator_connection:acquire(URL)
    end),
    receive
        {ok, Pid2} ->
            ?assertEqual(Pid, Pid2)
    end.


connections_not_shared_after_owner_death({Host, Port}) ->
    URL = "http://" ++ Host ++ ":" ++ Port,
    Self = self(),
    spawn(fun() ->
        Self ! couch_replicator_connection:acquire(URL),
        error("simulate division by zero without compiler warning")
    end),
    receive
        {ok, Pid} ->
            {ok, Pid2} = couch_replicator_connection:acquire(URL),
            ?assertNotEqual(Pid, Pid2),
            MRef = monitor(process, Pid),
            receive
                {'DOWN', MRef, process, Pid, _Reason} ->
                    ?assert(not is_process_alive(Pid));
                Other ->
                    throw(Other)
            end
    end.


idle_connections_closed({Host, Port}) ->
    URL = "http://" ++ Host ++ ":" ++ Port,
    {ok, Pid} = couch_replicator_connection:acquire(URL),
    couch_replicator_connection ! close_idle_connections,
    ?assert(ets:member(couch_replicator_connection, Pid)),
    % block until idle connections have closed
    sys:get_status(couch_replicator_connection),
    couch_replicator_connection:release(Pid),
    couch_replicator_connection ! close_idle_connections,
    % block until idle connections have closed
    sys:get_status(couch_replicator_connection),
    ?assert(not ets:member(couch_replicator_connection, Pid)).


test_owner_monitors({Host, Port}) ->
    URL = "http://" ++ Host ++ ":" ++ Port,
    {ok, Worker0} = couch_replicator_connection:acquire(URL),
    assert_monitors_equal([{process, self()}]),
    couch_replicator_connection:release(Worker0),
    assert_monitors_equal([]),
    {Workers, Monitors}  = lists:foldl(fun(_, {WAcc, MAcc}) ->
        {ok, Worker1} = couch_replicator_connection:acquire(URL),
        MAcc1 = [{process, self()} | MAcc],
        assert_monitors_equal(MAcc1),
        {[Worker1 | WAcc], MAcc1}
    end, {[], []}, lists:seq(1, 5)),
    lists:foldl(fun(Worker2, Acc) ->
        [_ | NewAcc] = Acc,
        couch_replicator_connection:release(Worker2),
        assert_monitors_equal(NewAcc),
        NewAcc
    end, Monitors, Workers).


worker_discards_creds_on_create({Host, Port}) ->
    {User, Pass, B64Auth} = user_pass(),
    URL = "http://" ++ User ++ ":" ++ Pass ++ "@" ++ Host ++ ":" ++ Port,
    {ok, WPid} = couch_replicator_connection:acquire(URL),
    Internals = worker_internals(WPid),
    ?assert(string:str(Internals, B64Auth) =:= 0),
    ?assert(string:str(Internals, Pass) =:= 0).


worker_discards_url_creds_after_request({Host, _}) ->
    {User, Pass, B64Auth} = user_pass(),
    {Port, ServerPid} = server(),
    PortStr = integer_to_list(Port),
    URL = "http://" ++ User ++ ":" ++ Pass ++ "@" ++ Host ++ ":" ++ PortStr,
    {ok, WPid} = couch_replicator_connection:acquire(URL),
    ?assertMatch({ok, "200", _, _}, send_req(WPid, URL, [], [])),
    Internals = worker_internals(WPid),
    ?assert(string:str(Internals, B64Auth) =:= 0),
    ?assert(string:str(Internals, Pass) =:= 0),
    couch_replicator_connection:release(WPid),
    unlink(ServerPid),
    exit(ServerPid, kill).


worker_discards_creds_in_headers_after_request({Host, _}) ->
    {_User, Pass, B64Auth} = user_pass(),
    {Port, ServerPid} = server(),
    PortStr = integer_to_list(Port),
    URL = "http://" ++ Host ++ ":" ++ PortStr,
    {ok, WPid} = couch_replicator_connection:acquire(URL),
    Headers = [{"Authorization", "Basic " ++ B64Auth}],
    ?assertMatch({ok, "200", _, _}, send_req(WPid, URL, Headers, [])),
    Internals = worker_internals(WPid),
    ?assert(string:str(Internals, B64Auth) =:= 0),
    ?assert(string:str(Internals, Pass) =:= 0),
    couch_replicator_connection:release(WPid),
    unlink(ServerPid),
    exit(ServerPid, kill).


worker_discards_proxy_creds_after_request({Host, _}) ->
    {User, Pass, B64Auth} = user_pass(),
    {Port, ServerPid} = server(),
    PortStr = integer_to_list(Port),
    URL = "http://" ++ Host ++ ":" ++ PortStr,
    {ok, WPid} = couch_replicator_connection:acquire(URL),
    Opts = [
        {proxy_host, Host},
        {proxy_port, Port},
        {proxy_user, User},
        {proxy_pass, Pass}
    ],
    ?assertMatch({ok, "200", _, _}, send_req(WPid, URL, [], Opts)),
    Internals = worker_internals(WPid),
    ?assert(string:str(Internals, B64Auth) =:= 0),
    ?assert(string:str(Internals, Pass) =:= 0),
    couch_replicator_connection:release(WPid),
    unlink(ServerPid),
    exit(ServerPid, kill).


send_req(WPid, URL, Headers, Opts) ->
    ibrowse:send_req_direct(WPid, URL, Headers, get, [], Opts).


user_pass() ->
    User = "specialuser",
    Pass = "averysecretpassword",
    B64Auth = ibrowse_lib:encode_base64(User ++ ":" ++ Pass),
    {User, Pass, B64Auth}.


worker_internals(Pid) ->
    Dict = io_lib:format("~p", [erlang:process_info(Pid, dictionary)]),
    State = io_lib:format("~p", [sys:get_state(Pid)]),
    lists:flatten([Dict, State]).


server() ->
    {ok, LSock} = gen_tcp:listen(0, [{recbuf, 256}, {active, false}]),
    {ok, LPort} = inet:port(LSock),
    SPid = spawn_link(fun() -> server_responder(LSock) end),
    {LPort, SPid}.


server_responder(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            % sanity check that all the request data was received
            ?assert(lists:prefix("GET ", Data)),
            ?assert(lists:suffix("\r\n\r\n", Data)),
            Res = ["HTTP/1.1 200 OK", "Content-Length: 0", "\r\n"],
            ok = gen_tcp:send(Sock, string:join(Res, "\r\n"));
        Other ->
            gen_tcp:close(Sock),
            throw({replication_eunit_tcp_server_crashed, Other})
    end,
    server_responder(LSock).


assert_monitors_equal(ShouldBe) ->
    sys:get_status(couch_replicator_connection),
    {monitors, Monitors} = process_info(whereis(couch_replicator_connection),
        monitors),
    ?assertEqual(Monitors, ShouldBe).
