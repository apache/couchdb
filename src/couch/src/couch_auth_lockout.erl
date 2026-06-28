% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_auth_lockout).
-include_lib("couch/include/couch_db.hrl").

-define(LRU, couch_lockout_lru).

-export([is_locked_out/3, lockout/3]).

is_locked_out(#httpd{} = Req, UserName, UserSalt) ->
    case lockout_mode() of
        off ->
            false;
        Mode ->
            is_locked_out_int(Req, Mode, UserName, UserSalt)
    end.

is_locked_out_int(#httpd{} = Req, Mode, UserName, UserSalt) ->
    LockoutThreshold = lockout_threshold(),
    case peer(Req) of
        nopeer ->
            false;
        Peer ->
            case ets_lru:lookup_d(?LRU, {Peer, UserName, UserSalt}) of
                {ok, FailCount} when FailCount >= LockoutThreshold, Mode == enforce ->
                    true;
                {ok, FailCount} when FailCount >= LockoutThreshold, Mode == warn ->
                    couch_log:warning(
                        "~p: authentication failure threshold reached for ~s from ~s",
                        [?MODULE, UserName, Peer]
                    ),
                    false;
                {ok, _} ->
                    false;
                not_found ->
                    false
            end
    end.

lockout(#httpd{} = Req, UserName, UserSalt) ->
    case peer(Req) of
        nopeer ->
            ok;
        Peer ->
            case lockout_mode() of
                off ->
                    ok;
                _ ->
                    ets_lru:update_counter(?LRU, {Peer, UserName, UserSalt}, 1)
            end
    end.

lockout_mode() ->
    case config:get("chttpd_auth_lockout", "mode", "enforce") of
        "off" ->
            off;
        "warn" ->
            warn;
        "enforce" ->
            enforce;
        _ ->
            off
    end.

lockout_threshold() ->
    config:get_integer("chttpd_auth_lockout", "threshold", 5).

peer(#httpd{mochi_req = MochiReq}) ->
    Socket = mochiweb_request:get(socket, MochiReq),
    case Socket of
        {remote, _Pid, _Ref} ->
            nopeer;
        _ ->
            client_ip(MochiReq)
    end.

client_ip(MochiReq) ->
    case configured_client_ip(MochiReq) of
        undefined ->
            mochiweb_request:get(peer, MochiReq);
        Peer ->
            Peer
    end.

configured_client_ip(MochiReq) ->
    case client_ip_header() of
        undefined ->
            undefined;
        Header ->
            case mochiweb_request:get_header_value(Header, MochiReq) of
                undefined -> undefined;
                Value -> first_address(Value)
            end
    end.

first_address(Value) ->
    case string:lexemes(Value, ", ") of
        [Client | _] -> Client;
        [] -> undefined
    end.

client_ip_header() ->
    case config:get("chttpd_auth_lockout", "client_ip_header", undefined) of
        "" -> undefined;
        Header -> Header
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(SOCKET_PEER, "203.0.113.9").

peer_resolution_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun t_no_header_configured_uses_socket_peer/1,
            fun t_blank_header_config_uses_socket_peer/1,
            fun t_absent_header_falls_back_to_socket_peer/1,
            fun t_empty_header_falls_back_to_socket_peer/1,
            fun t_single_address_header_is_used/1,
            fun t_first_address_of_forwarded_for_is_used/1,
            fun t_remote_socket_has_no_peer/1
        ]
    }.

setup() ->
    meck:new(config, [passthrough]),
    meck:new(mochiweb_request, [passthrough]),
    meck:expect(mochiweb_request, get, fun
        (peer, _Req) -> ?SOCKET_PEER;
        (Key, Req) -> meck:passthrough([Key, Req])
    end),
    set_client_ip_header(undefined),
    ok.

teardown(_) ->
    meck:unload().

set_client_ip_header(Value) ->
    meck:expect(config, get, fun
        ("chttpd_auth_lockout", "client_ip_header", _Default) -> Value;
        (Section, Key, Default) -> meck:passthrough([Section, Key, Default])
    end).

req(Headers) ->
    req(fake_socket, Headers).

req(Socket, Headers) ->
    MochiReq = mochiweb_request:new(
        Socket, 'GET', "/", {1, 1}, mochiweb_headers:make(Headers)
    ),
    #httpd{mochi_req = MochiReq}.

t_no_header_configured_uses_socket_peer(_) ->
    ?_assertEqual(?SOCKET_PEER, peer(req([{"X-Forwarded-For", "198.51.100.23"}]))).

t_blank_header_config_uses_socket_peer(_) ->
    set_client_ip_header(""),
    ?_assertEqual(?SOCKET_PEER, peer(req([{"X-Forwarded-For", "198.51.100.23"}]))).

t_absent_header_falls_back_to_socket_peer(_) ->
    set_client_ip_header("X-Forwarded-For"),
    ?_assertEqual(?SOCKET_PEER, peer(req([{"Host", "example.com"}]))).

t_empty_header_falls_back_to_socket_peer(_) ->
    set_client_ip_header("X-Forwarded-For"),
    ?_assertEqual(?SOCKET_PEER, peer(req([{"X-Forwarded-For", "  "}]))).

t_single_address_header_is_used(_) ->
    set_client_ip_header("CF-Connecting-IP"),
    ?_assertEqual("198.51.100.23", peer(req([{"CF-Connecting-IP", "198.51.100.23"}]))).

t_first_address_of_forwarded_for_is_used(_) ->
    set_client_ip_header("X-Forwarded-For"),
    Req = req([{"X-Forwarded-For", "198.51.100.23, 203.0.113.9, 10.0.0.1"}]),
    ?_assertEqual("198.51.100.23", peer(Req)).

t_remote_socket_has_no_peer(_) ->
    set_client_ip_header("X-Forwarded-For"),
    Req = req({remote, self(), make_ref()}, [{"X-Forwarded-For", "198.51.100.23"}]),
    ?_assertEqual(nopeer, peer(Req)).

first_address_test_() ->
    [
        ?_assertEqual("198.51.100.23", first_address("198.51.100.23")),
        ?_assertEqual("198.51.100.23", first_address("198.51.100.23, 203.0.113.9")),
        ?_assertEqual("198.51.100.23", first_address("  198.51.100.23 , 203.0.113.9")),
        ?_assertEqual(undefined, first_address("")),
        ?_assertEqual(undefined, first_address("  "))
    ].

-endif.
