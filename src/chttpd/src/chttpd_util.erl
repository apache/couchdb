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

-module(chttpd_util).

-export([
    get_chttpd_config/1,
    get_chttpd_config/2,
    get_chttpd_config_integer/2,
    get_chttpd_config_boolean/2,
    get_chttpd_auth_config/1,
    get_chttpd_auth_config/2,
    get_chttpd_auth_config_integer/2,
    get_chttpd_auth_config_boolean/2,
    maybe_add_csp_header/3,
    get_db_info/1,
    mochiweb_socket_set/1,
    mochiweb_socket_clean/0,
    mochiweb_socket_get/0,
    mochiweb_socket_check_msec/0,
    stop_client_process_if_disconnected/2
]).

-define(MOCHIWEB_SOCKET, mochiweb_connection_socket).
-define(DISCONNECT_CHECK_MSEC, 30000).

get_chttpd_config(Key) ->
    config:get("chttpd", Key, config:get("httpd", Key)).

get_chttpd_config(Key, Default) ->
    config:get("chttpd", Key, config:get("httpd", Key, Default)).

get_chttpd_config_integer(Key, Default) ->
    config:get_integer(
        "chttpd",
        Key,
        config:get_integer("httpd", Key, Default)
    ).

get_chttpd_config_boolean(Key, Default) ->
    config:get_boolean(
        "chttpd",
        Key,
        config:get_boolean("httpd", Key, Default)
    ).

get_chttpd_auth_config(Key) ->
    config:get("chttpd_auth", Key, config:get("couch_httpd_auth", Key)).

get_chttpd_auth_config(Key, Default) ->
    config:get(
        "chttpd_auth",
        Key,
        config:get("couch_httpd_auth", Key, Default)
    ).

get_chttpd_auth_config_integer(Key, Default) ->
    config:get_integer(
        "chttpd_auth",
        Key,
        config:get_integer("couch_httpd_auth", Key, Default)
    ).

get_chttpd_auth_config_boolean(Key, Default) ->
    config:get_boolean(
        "chttpd_auth",
        Key,
        config:get_boolean("couch_httpd_auth", Key, Default)
    ).

maybe_add_csp_header(Component, OriginalHeaders, DefaultHeaderValue) ->
    Enabled = config:get_boolean("csp", Component ++ "_enable", true),
    case Enabled of
        true ->
            HeaderValue = config:get("csp", Component ++ "_header_value", DefaultHeaderValue),
            % As per https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy#multiple_content_security_policies
            % The top most CSP header defines the most open policy,
            % subsequent CSP headers set by show/list functions can
            % only further restrict the policy.
            %
            % Ours goes on top and we don’t have to worry about additional
            % headers set by users.
            [{"Content-Security-Policy", HeaderValue} | OriginalHeaders];
        false ->
            % Fallback for old config vars
            case Component of
                "utils" ->
                    handle_legacy_config(OriginalHeaders, DefaultHeaderValue);
                _ ->
                    OriginalHeaders
            end
    end.

handle_legacy_config(OriginalHeaders, DefaultHeaderValue) ->
    LegacyUtilsEnabled = config:get_boolean("csp", "enable", true),
    case LegacyUtilsEnabled of
        true ->
            LegacyUtilsHeaderValue = config:get("csp", "header_value", DefaultHeaderValue),
            [{"Content-Security-Policy", LegacyUtilsHeaderValue} | OriginalHeaders];
        false ->
            OriginalHeaders
    end.

get_db_info(DbName) ->
    Timeout = fabric_util:request_timeout(),
    IsolatedFun = fun() -> fabric:get_db_info(DbName) end,
    try
        fabric_util:isolate(IsolatedFun, Timeout)
    catch
        _Tag:Error -> {error, Error}
    end.

mochiweb_socket_set(Sock) ->
    put(?MOCHIWEB_SOCKET, Sock).

mochiweb_socket_clean() ->
    erase(?MOCHIWEB_SOCKET).

mochiweb_socket_get() ->
    get(?MOCHIWEB_SOCKET).

mochiweb_socket_check_msec() ->
    MSec = config:get_integer("chttpd", "disconnect_check_msec", ?DISCONNECT_CHECK_MSEC),
    % Add some jitter to avoid a stampede in case of a larger number of concurrent connection
    MSec + rand:uniform(MSec).

stop_client_process_if_disconnected(Pid, Sock) ->
    case is_mochiweb_socket_closed(Sock) of
        true ->
            couch_log:warning("client socket ~p disconnected, terminating ~p", [Sock, Pid]),
            exit(Pid, {shutdown, client_disconnected});
        false ->
            ok
    end.

is_mochiweb_socket_closed(undefined) ->
    false;
is_mochiweb_socket_closed(Sock) ->
    OsType = os:type(),
    case tcp_info_opt(OsType) of
        {raw, _, _, _} = InfoOpt ->
            case mochiweb_socket:getopts(Sock, [InfoOpt]) of
                {ok, [{raw, _, _, <<State:8/native, _/binary>>}]} ->
                    tcp_is_closed(State, OsType);
                {ok, []} ->
                    false;
                {error, einval} ->
                    % Already cleaned up
                    true;
                {error, _} ->
                    false
            end;
        undefined ->
            false
    end.

% All OS-es have the tcpi_state (uint8) as first member of tcp_info struct

tcp_info_opt({unix, linux}) ->
    %% netinet/in.h
    %%   IPPROTO_TCP = 6
    %%
    %% netinet/tcp.h
    %%   #define TCP_INFO 11
    %%
    {raw, 6, 11, 1};
tcp_info_opt({unix, darwin}) ->
    %% netinet/in.h
    %%   #define IPPROTO_TCP   6
    %%
    %% netinet/tcp.h
    %%   #define TCP_CONNECTION_INFO  0x106
    %%
    {raw, 6, 16#106, 1};
tcp_info_opt({unix, freebsd}) ->
    %% sys/netinet/in.h
    %%   #define  IPPROTO_TCP  6
    %%
    %% sys/netinet/tcp.h
    %%   #define  TCP_INFO    32
    %%
    {raw, 6, 32, 1};
tcp_info_opt({_, _}) ->
    undefined.

tcp_is_closed(State, {unix, linux}) ->
    %% netinet/tcp.h
    %%   enum
    %%   {
    %%     TCP_ESTABLISHED = 1,
    %%     TCP_SYN_SENT,
    %%     TCP_SYN_RECV,
    %%     TCP_FIN_WAIT1,
    %%     TCP_FIN_WAIT2,
    %%     TCP_TIME_WAIT,
    %%     TCP_CLOSE,
    %%     TCP_CLOSE_WAIT,
    %%     TCP_LAST_ACK,
    %%     TCP_LISTEN,
    %%     TCP_CLOSING
    %%   }
    %%
    lists:member(State, [4, 5, 6, 7, 8, 9, 11]);
tcp_is_closed(State, {unix, Type}) when Type =:= darwin; Type =:= freebsd ->
    %% tcp_fsm.h states are the same on macos and freebsd
    %%
    %% netinet/tcp_fsm.h
    %%   #define TCPS_CLOSED             0       /* closed */
    %%   #define TCPS_LISTEN             1       /* listening for connection */
    %%   #define TCPS_SYN_SENT           2       /* active, have sent syn */
    %%   #define TCPS_SYN_RECEIVED       3       /* have send and received syn */
    %%   #define TCPS_ESTABLISHED        4       /* established */
    %%   #define TCPS_CLOSE_WAIT         5       /* rcvd fin, waiting for close */
    %%   #define TCPS_FIN_WAIT_1         6       /* have closed, sent fin */
    %%   #define TCPS_CLOSING            7       /* closed xchd FIN; await FIN ACK */
    %%   #define TCPS_LAST_ACK           8       /* had fin and close; await FIN ACK */
    %%   #define TCPS_FIN_WAIT_2         9       /* have closed, fin is acked */
    %%   #define TCPS_TIME_WAIT          10      /* in 2*msl quiet wait after close */
    %%
    lists:member(State, [0, 5, 6, 7, 8, 9, 10]).
