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

-module(ibrowse_socks5).

-define(VERSION, 5).
-define(CONNECT, 1).

-define(NO_AUTH, 0).
-define(USERPASS, 2).
-define(UNACCEPTABLE, 16#FF).
-define(RESERVED, 0).

-define(ATYP_IPV4, 1).
-define(ATYP_DOMAINNAME, 3).
-define(ATYP_IPV6, 4).

-define(SUCCEEDED, 0).

-export([connect/5]).

-import(ibrowse_lib, [get_value/2, get_value/3]).

connect(TargetHost, TargetPort, ProxyOptions, Options, Timeout) ->
    case gen_tcp:connect(get_value(host, ProxyOptions),
                         get_value(port, ProxyOptions),
                         Options, Timeout) of
        {ok, Socket} ->
            case handshake(Socket, Options) of
                ok ->
                    case connect(TargetHost, TargetPort, Socket) of
                        ok ->
                            maybe_ssl(Socket, ProxyOptions, Timeout);
                        Else ->
                            gen_tcp:close(Socket),
                            Else
                    end;
                Else ->
                    gen_tcp:close(Socket),
                    Else
            end;
        Else ->
            Else
    end.

handshake(Socket, ProxyOptions) when is_port(Socket) ->
    {Handshake, Success} = case get_value(user, ProxyOptions, <<>>) of
        <<>> ->
            {<<?VERSION, 1, ?NO_AUTH>>, ?NO_AUTH};
        User ->
            Password = get_value(password, ProxyOptions, <<>>),
            {<<?VERSION, 1, ?USERPASS, (byte_size(User)), User,
               (byte_size(Password)), Password>>, ?USERPASS}
    end,
    ok = gen_tcp:send(Socket, Handshake),
    case gen_tcp:recv(Socket, 0) of
        {ok, <<?VERSION, Success>>} ->
            ok;
        {ok, <<?VERSION, ?UNACCEPTABLE>>} ->
            {error, unacceptable};
        {error, Reason} ->
            {error, Reason}
    end.

connect(Host, Port, Via) when is_list(Host) ->
    connect(list_to_binary(Host), Port, Via);
connect(Host, Port, Via) when is_binary(Host), is_integer(Port),
                              is_port(Via) ->
    ok = gen_tcp:send(Via,
        <<?VERSION, ?CONNECT, ?RESERVED, ?ATYP_DOMAINNAME,
          (byte_size(Host)), Host/binary,
          (Port):16>>),
    case gen_tcp:recv(Via, 0) of
        {ok, <<?VERSION, ?SUCCEEDED, ?RESERVED, _/binary>>} ->
            ok;
        {ok, <<?VERSION, Rep, ?RESERVED, _/binary>>} ->
            {error, rep(Rep)};
        {error, Reason} ->
            {error, Reason}
    end.

maybe_ssl(Socket, ProxyOptions, Timeout) ->
    IsSsl = get_value(is_ssl, ProxyOptions, false),
    SslOpts = get_value(ssl_opts, ProxyOptions, []),
    case IsSsl of
        false ->
            {ok, Socket};
        true ->
            ssl:connect(Socket, SslOpts, Timeout)
    end.

rep(0) -> succeeded;
rep(1) -> server_fail;
rep(2) -> disallowed_by_ruleset;
rep(3) -> network_unreachable;
rep(4) -> host_unreachable;
rep(5) -> connection_refused;
rep(6) -> ttl_expired;
rep(7) -> command_not_supported;
rep(8) -> address_type_not_supported.
