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

-module(couch_prometheus_e2e_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "prometheus_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(PROM_PORT, "17986").
-define(CONTENT_JSON, {"Content-Type", "application/json"}).

start() ->
    test_util:start_couch([chttpd, couch_prometheus]).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    ok = config:set_integer("stats", "interval", 2),
    ok = config:set_integer("couch_prometheus", "interval", 1),
    Port = mochiweb_socket_server:get(chttpd, port),
    construct_url(Port).

teardown(_) ->
    ok.

couch_prometheus_e2e_test_() ->
    {
        "Prometheus E2E Tests",
        {
            setup,
            fun start/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun node_call_chttpd/1,
                    fun node_call_prometheus_http/1,
                    fun deny_prometheus_http/1,
                    fun node_see_updated_metrics/1
                ]
            }
        }
    }.

% normal chttpd path via cluster port
node_call_chttpd(Url) ->
    {ok, RC1, _, _} = test_request:get(
        Url,
        [?CONTENT_JSON, ?AUTH],
        []
    ),
    ?_assertEqual(200, RC1).

% normal chttpd path via cluster port
node_see_updated_metrics(Url) ->
    TmpDb = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    DbUrl = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(TmpDb)]),
    create_db(DbUrl),
    [create_doc(DbUrl, "testdoc" ++ integer_to_binary(I)) || I <- lists:seq(1, 100)],
    delete_db(DbUrl),
    InitMetrics = wait_for_metrics(Url, "couchdb_httpd_requests_total 0", 5000),
    UpdatedMetrics = wait_for_metrics(Url, "couchdb_httpd_requests_total", 10000),
    % since the puts happen so fast, we can't have an exact
    % total requests given the scraping interval. so we just want to acknowledge
    % a change as occurred
    ?_assertNotEqual(InitMetrics, UpdatedMetrics).

% normal chttpd path via cluster port
node_call_prometheus_http(_) ->
    maybe_start_http_server("true"),
    Url = construct_url(?PROM_PORT),
    {ok, RC1, _, _} = test_request:get(
        Url,
        [?CONTENT_JSON, ?AUTH]
    ),
    % since this port doesn't require auth, this should work
    {ok, RC2, _, _} = test_request:get(
        Url,
        [?CONTENT_JSON]
    ),
    delete_db(Url),
    ?_assertEqual({200, 200}, {RC1, RC2}).

% we don't start the http server
deny_prometheus_http(_) ->
    maybe_start_http_server("false"),
    Url = construct_url(?PROM_PORT),
    Response = test_request:get(
        Url,
        [?CONTENT_JSON, ?AUTH],
        []
    ),
    ?_assertEqual({error, {conn_failed, {error, econnrefused}}}, Response).

maybe_start_http_server(Additional) ->
    test_util:stop_applications([couch_prometheus, chttpd]),
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    ok = config:set("prometheus", "additional_port", Additional),
    ok = config:set("prometheus", "port", ?PROM_PORT),
    test_util:start_applications([couch_prometheus, chttpd]).

construct_url(Port) ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    lists:concat(["http://", Addr, ":", Port, "/_node/_local/_prometheus"]).

create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

create_doc(Url, Id) ->
    test_request:put(
        Url ++ "/" ++ Id,
        [?CONTENT_JSON, ?AUTH],
        "{\"mr\": \"rockoartischocko\"}"
    ).

wait_for_metrics(Url, Value, Timeout) ->
    test_util:wait(
        fun() ->
            {ok, _, _, Body} = test_request:get(
                Url,
                [?CONTENT_JSON, ?AUTH],
                []
            ),
            case string:find(Body, Value) of
                nomatch -> wait;
                M -> M
            end
        end,
        Timeout
    ).
