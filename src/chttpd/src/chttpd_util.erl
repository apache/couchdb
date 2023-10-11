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
    scrub_mochiweb_client_req/1,
    mochiweb_client_req_set/1,
    mochiweb_client_req_clean/0,
    mochiweb_client_req_get/0,
    mochiweb_client_req_check_msec/0,
    stop_client_process_if_disconnected/2,
    get_active_tasks/1
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

scrub_mochiweb_client_req(ClientReq) ->
    Method = mochiweb_request:get(method, ClientReq),
    Socket = mochiweb_request:get(socket, ClientReq),
    Path = mochiweb_request:get(raw_path, ClientReq),
    Version = mochiweb_request:get(version, ClientReq),
    Opts = mochiweb_request:get(opts, ClientReq),
    Headers = mochiweb_request:get(headers, ClientReq),
    Headers1 = mochiweb_headers:delete_any("Authorization", Headers),
    Headers2 = mochiweb_headers:delete_any("Cookie", Headers1),
    Headers3 = mochiweb_headers:delete_any("X-Auth-CouchDB-Token", Headers2),
    mochiweb_request:new(Socket, Opts, Method, Path, Version, Headers3).

mochiweb_client_req_set(ClientReq) ->
    % Remove any sensitive info in case process dict gets dumped
    % to the logs at some point
    put(?MOCHIWEB_CLIENT_REQ, scrub_mochiweb_client_req(ClientReq)).

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

get_active_tasks(Nodes) when is_list(Nodes) ->
    Responses = lists:zip(Nodes, erpc:multicall(Nodes, couch_task_status, all, [])),
    % Responses has this shape:
    %   [
    %      {node1, {ok, [[{pid, <<"<0.1.2>">>}, ...], [{pid, <<"<0.3.4>">>}, ...]]}},
    %      {node2, {error, foo}},
    %      {node3, {ok, [[{pid, <<"<0.5.6>">>}, ...] ...]}}
    %   ]
    lists:foldl(fun tasks_fold_fun/2, [], Responses).

% See https://www.erlang.org/doc/man/erpc#multicall-3
%
tasks_fold_fun({Node, {ok, Tasks}}, Acc) when is_atom(Node), is_list(Tasks) ->
    [{[{node, Node} | Task]} || Task <- Tasks] ++ Acc;
tasks_fold_fun({_Node, {_Class, _Reason}}, Acc) ->
    Acc.
