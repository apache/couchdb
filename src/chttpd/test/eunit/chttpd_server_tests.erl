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

-module(chttpd_server_tests).

-include_lib("couch/include/couch_eunit.hrl").

%% making it lower than eunit's default timeout 1000
-define(MOCK_TIMEOUT, 800).
-define(CLIENT_IP, "151.101.2.132").

setup_suite() ->
    Ctx = test_util:start_couch([chttpd]),
    meck:new(couch_log, [passthrough]),
    ok = meck:expect(couch_log, notice, fun(Format, Args) ->
        lists:flatten(io_lib:format(Format, Args))
    end),
    meck:new(mochiweb_request, [passthrough]),
    ok = meck:expect(mochiweb_request, get, fun request_get/2),
    meck:new(mochiweb_socket, [passthrough]),
    ok = meck:expect(mochiweb_socket, peername, fun peername/1),
    Ctx.

teardown_suite(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).

peername(Socket) ->
    case get(mock_ip) of
        undefined ->
            meck:passthrough([Socket]);
        {Ip, Port} ->
            {ok, {Ip, Port}}
    end.

request_get(Opp, This) ->
    Headers = meck:passthrough([headers, This]),
    case mochiweb_headers:get_value("MockIP", Headers) of
        undefined ->
            ok;
        MockIp ->
            [IpStr, Port] = string:split(MockIp, ":"),
            {ok, Ip} = inet:parse_address(IpStr),
            put(mock_ip, {Ip, list_to_integer(Port)})
    end,
    meck:passthrough([Opp, This]).

peer_detection_test_() ->
    {
        "Peer detection tests",
        {
            setup,
            fun setup_suite/0,
            fun teardown_suite/1,
            {
                foreachx,
                fun setup/1,
                fun teardown/2,
                [
                    %% Verify we log IP address as is if there is no X-Forwarded-For header present
                    {{"10.0.0.0/8", "10.0.0.1"}, fun local_network_v4/2},
                    {{"10.0.0.0/8", "10.0.1.1"}, fun local_network_v4/2},
                    {{"10.0.0.0/8", "10.1.1.1"}, fun local_network_v4/2},
                    {{"10.0.0.0/8", "10.255.255.254"}, fun local_network_v4/2},
                    {{"172.16.0.0/12", "172.16.0.1"}, fun local_network_v4/2},
                    {{"172.16.0.0/12", "172.24.0.1"}, fun local_network_v4/2},
                    {{"172.16.0.0/12", "172.31.1.128"}, fun local_network_v4/2},
                    {{"172.16.0.0/12", "172.31.255.254"}, fun local_network_v4/2},
                    {{"192.168.0.0/16", "192.168.0.1"}, fun local_network_v4/2},
                    {{"192.168.0.0/16", "192.168.1.128"}, fun local_network_v4/2},
                    {{"192.168.0.0/16", "192.168.255.254"}, fun local_network_v4/2},
                    {{"100.64.0.0/10", "100.64.0.1"}, fun shared_network_v4/2},
                    {{"100.64.0.0/10", "100.64.100.128"}, fun shared_network_v4/2},
                    {{"100.64.0.0/10", "100.127.255.254"}, fun shared_network_v4/2},

                    %% Verify we log client's IP address when X-Forwarded-For header present
                    {{"10.0.0.0/8", "10.0.0.1"}, fun local_network_v4_x_forwarded/2},
                    {{"10.0.0.0/8", "10.0.1.1"}, fun local_network_v4_x_forwarded/2},
                    {{"10.0.0.0/8", "10.1.1.1"}, fun local_network_v4_x_forwarded/2},
                    {{"10.0.0.0/8", "10.255.255.254"}, fun local_network_v4_x_forwarded/2},
                    {{"172.16.0.0/12", "172.16.0.1"}, fun local_network_v4_x_forwarded/2},
                    {{"172.16.0.0/12", "172.24.0.1"}, fun local_network_v4_x_forwarded/2},
                    {{"172.16.0.0/12", "172.31.1.128"}, fun local_network_v4_x_forwarded/2},
                    {{"172.16.0.0/12", "172.31.255.254"}, fun local_network_v4_x_forwarded/2},
                    {{"192.168.0.0/16", "192.168.0.1"}, fun local_network_v4_x_forwarded/2},
                    {{"192.168.0.0/16", "192.168.1.128"}, fun local_network_v4_x_forwarded/2},
                    {{"192.168.0.0/16", "192.168.255.254"}, fun local_network_v4_x_forwarded/2},
                    {{"100.64.0.0/10", "100.64.0.1"}, fun shared_network_v4_x_forwarded/2},
                    {{"100.64.0.0/10", "100.64.100.128"}, fun shared_network_v4_x_forwarded/2},
                    {{"100.64.0.0/10", "100.127.255.254"}, fun shared_network_v4_x_forwarded/2},

                    %% Verify the X-Forwarded-For logic doesn't affect public networks
                    {
                        {"outside of 10.0.0.0/8", "9.255.255.254"},
                        fun public_network_v4_x_forwarded/2
                    },
                    {{"outside of 10.0.0.0/8", "11.0.0.1"}, fun public_network_v4_x_forwarded/2},
                    {
                        {"outside of 172.16.0.0/12", "172.15.255.254"},
                        fun public_network_v4_x_forwarded/2
                    },
                    {
                        {"outside of 172.16.0.0/12", "172.32.0.1"},
                        fun public_network_v4_x_forwarded/2
                    },
                    {
                        {"outside of 192.168.0.0/16", "192.167.255.254"},
                        fun public_network_v4_x_forwarded/2
                    },
                    {
                        {"outside of 192.168.0.0/16", "192.169.0.1"},
                        fun public_network_v4_x_forwarded/2
                    },
                    {
                        {"outside of 100.64.0.0/10", "100.63.255.254"},
                        fun public_network_v4_x_forwarded/2
                    },
                    {
                        {"outside of 100.64.0.0/10", "100.128.0.1"},
                        fun public_network_v4_x_forwarded/2
                    },

                    %% Just in case verify the public networks return public IP
                    {{"outside of 10.0.0.0/8", "9.255.255.254"}, fun public_network_v4/2},
                    {{"outside of 10.0.0.0/8", "11.0.0.1"}, fun public_network_v4/2},
                    {{"outside of 172.16.0.0/12", "172.15.255.254"}, fun public_network_v4/2},
                    {{"outside of 172.16.0.0/12", "172.32.0.1"}, fun public_network_v4/2},
                    {{"outside of 192.168.0.0/16", "192.167.255.254"}, fun public_network_v4/2},
                    {{"outside of 192.168.0.0/16", "192.169.0.1"}, fun public_network_v4/2},
                    {{"outside of 100.64.0.0/10", "100.63.255.254"}, fun public_network_v4/2},
                    {{"outside of 100.64.0.0/10", "100.128.0.1"}, fun public_network_v4/2}
                ]
            }
        }
    }.

shared_network_v4({Net, Ip}, _) ->
    {
        Net ++ " " ++ Ip,
        ?_test(begin
            ok = request(Ip),
            Peer = maps:get(peer, log_event()),
            ?assertEqual(Ip, Peer)
        end)
    }.

local_network_v4({Net, Ip}, _) ->
    {
        Net ++ " " ++ Ip,
        ?_test(begin
            ok = request(Ip),
            Peer = maps:get(peer, log_event()),
            ?assertEqual(Ip, Peer)
        end)
    }.

public_network_v4({Net, Ip}, _) ->
    {
        Net ++ " " ++ Ip,
        ?_test(begin
            ok = request(Ip),
            Peer = maps:get(peer, log_event()),
            ?assertEqual(Ip, Peer)
        end)
    }.

shared_network_v4_x_forwarded({Net, Ip}, _) ->
    {
        Net ++ " " ++ Ip,
        ?_test(begin
            ok = request_x_forwarded(Ip),
            Peer = maps:get(peer, log_event()),
            ?assertEqual(?CLIENT_IP, Peer)
        end)
    }.

local_network_v4_x_forwarded({Net, Ip}, _) ->
    {
        Net ++ " " ++ Ip,
        ?_test(begin
            ok = request_x_forwarded(Ip),
            Peer = maps:get(peer, log_event()),
            ?assertEqual(?CLIENT_IP, Peer)
        end)
    }.

public_network_v4_x_forwarded({Net, Ip}, _) ->
    {
        Net ++ " " ++ Ip,
        ?_test(begin
            ok = request_x_forwarded(Ip),
            Peer = maps:get(peer, log_event()),
            ?assertEqual(Ip, Peer)
        end)
    }.

request(FromIp) ->
    MockIp = FromIp ++ ":" ++ port(),
    ?assertMatch({ok, _, _, _}, test_request:get(base_url(), [{"MockIp", MockIp}])),
    ok.

request_x_forwarded(FromIp) ->
    MockIp = FromIp ++ ":" ++ port(),
    ?assertMatch(
        {ok, _, _, _},
        test_request:get(base_url(), [{"MockIp", MockIp}, {"X-Forwarded-For", ?CLIENT_IP}])
    ),
    ok.

setup(_) ->
    meck:reset(couch_log),
    ok.

teardown(_, _) ->
    meck:reset(couch_log),
    ok.

base_url() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    "http://" ++ Addr ++ ":" ++ port().

port() ->
    integer_to_list(mochiweb_socket_server:get(chttpd, port)).

log_event() ->
    meck:wait(1, couch_log, notice, '_', ?MOCK_TIMEOUT),
    Args = meck:capture(first, couch_log, notice, '_', 2, '_'),
    maps:from_list(
        lists:zip([host, peer, user, method, raw_uri, code, status, request_time], Args)
    ).
