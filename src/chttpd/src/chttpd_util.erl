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
    mochiweb_client_req_set/1,
    mochiweb_client_req_clean/0,
    mochiweb_client_req_get/0,
    mochiweb_client_req_check_msec/0,
    stop_client_process_if_disconnected/2
]).

-define(MOCHIWEB_CLIENT_REQ, mochiweb_client_req).
-define(DISCONNECT_CHECK_MSEC, 30000).
-define(DISCONNECT_CHECK_JITTER_MSEC, 15000).

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

mochiweb_client_req_set(ClientReq) ->
    put(?MOCHIWEB_CLIENT_REQ, ClientReq).

mochiweb_client_req_clean() ->
    erase(?MOCHIWEB_CLIENT_REQ).

mochiweb_client_req_get() ->
    get(?MOCHIWEB_CLIENT_REQ).

mochiweb_client_req_check_msec() ->
    MSec = config:get_integer(
        "chttpd", "disconnect_check_msec", ?DISCONNECT_CHECK_MSEC
    ),
    JitterMSec = config:get_integer(
        "chttpd", "disconnect_check_jitter_msec", ?DISCONNECT_CHECK_JITTER_MSEC
    ),
    max(100, MSec + rand:uniform(max(1, JitterMSec))).

stop_client_process_if_disconnected(_Pid, undefined) ->
    ok;
stop_client_process_if_disconnected(Pid, ClientReq) ->
    case mochiweb_request:is_closed(ClientReq) of
        true ->
            exit(Pid, {shutdown, client_disconnected}),
            couch_stats:increment_counter([couchdb, httpd, abandoned_streaming_requests]),
            ok;
        false ->
            ok;
        undefined ->
            % Treat unsupported OS-es (ex. Windows) as `not closed`
            % so we default to the previous behavior.
            ok
    end.
