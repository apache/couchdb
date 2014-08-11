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

-module(couchdb_cors_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").


-define(ADMIN_USER, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).
-define(SUPPORTED_METHODS,
        "GET, HEAD, POST, PUT, DELETE, TRACE, CONNECT, COPY, OPTIONS").
-define(TIMEOUT, 1000).


start() ->
    {ok, Pid} = couch_server_sup:start_link(?CONFIG_CHAIN),
    ok = couch_config:set("httpd", "enable_cors", "true", false),
    ok = couch_config:set("vhosts", "example.com", "/", false),
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

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_USER]),
    couch_db:close(Db),

    couch_config:set("cors", "credentials", "false", false),
    couch_config:set("cors", "origins", "http://example.com", false),

    Addr = couch_config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    Host = "http://" ++ Addr ++ ":" ++ Port,
    {Host, ?b2l(DbName)}.

setup({Mod, VHost}) ->
    {Host, DbName} = setup(),
    Url = case Mod of
        server ->
            Host;
        db ->
            Host ++ "/" ++ DbName
    end,
    DefaultHeaders = [{"Origin", "http://example.com"}]
                     ++ maybe_append_vhost(VHost),
    {Host, DbName, Url, DefaultHeaders}.

teardown(DbName) when is_list(DbName) ->
    ok = couch_server:delete(?l2b(DbName), [?ADMIN_USER]),
    ok;
teardown({_, DbName}) ->
    teardown(DbName).

teardown(_, {_, DbName, _, _}) ->
    teardown(DbName).


cors_test_() ->
    Funs = [
        fun should_not_allow_origin/2,
        fun should_not_allow_origin_with_port_mismatch/2,
        fun should_not_allow_origin_with_scheme_mismatch/2,
        fun should_not_all_origin_due_case_mismatch/2,
        fun should_make_simple_request/2,
        fun should_make_preflight_request/2,
        fun should_make_prefligh_request_with_port/2,
        fun should_make_prefligh_request_with_scheme/2,
        fun should_make_prefligh_request_with_wildcard_origin/2,
        fun should_make_request_with_credentials/2,
        fun should_make_origin_request_with_auth/2,
        fun should_make_preflight_request_with_auth/2
    ],
    {
        "CORS (COUCHDB-431)",
        {
            setup,
            fun start/0, fun stop/1,
            [
                cors_tests(Funs),
                vhost_cors_tests(Funs),
                headers_tests()
            ]
        }
    }.

headers_tests() ->
    {
        "Various headers tests",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                fun should_not_return_cors_headers_for_invalid_origin/1,
                fun should_not_return_cors_headers_for_invalid_origin_preflight/1,
                fun should_make_request_against_attachment/1,
                fun should_make_range_request_against_attachment/1,
                fun should_make_request_with_if_none_match_header/1
            ]
        }
    }.

cors_tests(Funs) ->
    {
        "CORS tests",
        [
            make_test_case(server, false, Funs),
            make_test_case(db, false, Funs)
        ]
    }.

vhost_cors_tests(Funs) ->
    {
        "Virtual Host CORS",
        [
            make_test_case(server, true, Funs),
            make_test_case(db, true, Funs)
        ]
    }.

make_test_case(Mod, UseVhost, Funs) ->
    {
        case Mod of server -> "Server"; db -> "Database" end,
        {foreachx, fun setup/1, fun teardown/2, [{{Mod, UseVhost}, Fun}
                                                 || Fun <- Funs]}
    }.


should_not_allow_origin(_, {_, _, Url, Headers0}) ->
    ?_assertEqual(undefined,
        begin
            couch_config:delete("cors", "origins", false),
            Headers1 = proplists:delete("Origin", Headers0),
            Headers = [{"Origin", "http://127.0.0.1"}]
                      ++ Headers1,
            {ok, _, Resp, _} = test_request:get(Url, Headers),
            proplists:get_value("Access-Control-Allow-Origin", Resp)
        end).

should_not_allow_origin_with_port_mismatch({_, VHost}, {_, _, Url, _}) ->
    ?_assertEqual(undefined,
        begin
            Headers = [{"Origin", "http://example.com:5984"},
                       {"Access-Control-Request-Method", "GET"}]
                      ++ maybe_append_vhost(VHost),
            {ok, _, Resp, _} = test_request:options(Url, Headers),
            proplists:get_value("Access-Control-Allow-Origin", Resp)
        end).

should_not_allow_origin_with_scheme_mismatch({_, VHost}, {_, _, Url, _}) ->
    ?_assertEqual(undefined,
        begin
            Headers = [{"Origin", "http://example.com:5984"},
                       {"Access-Control-Request-Method", "GET"}]
                      ++ maybe_append_vhost(VHost),
            {ok, _, Resp, _} = test_request:options(Url, Headers),
            proplists:get_value("Access-Control-Allow-Origin", Resp)
        end).

should_not_all_origin_due_case_mismatch({_, VHost}, {_, _, Url, _}) ->
    ?_assertEqual(undefined,
        begin
            Headers = [{"Origin", "http://ExAmPlE.CoM"},
                       {"Access-Control-Request-Method", "GET"}]
                      ++ maybe_append_vhost(VHost),
            {ok, _, Resp, _} = test_request:options(Url, Headers),
            proplists:get_value("Access-Control-Allow-Origin", Resp)
        end).

should_make_simple_request(_, {_, _, Url, DefaultHeaders}) ->
    ?_test(begin
        {ok, _, Resp, _} = test_request:get(Url, DefaultHeaders),
        ?assertEqual(
            undefined,
            proplists:get_value("Access-Control-Allow-Credentials", Resp)),
        ?assertEqual(
            "http://example.com",
            proplists:get_value("Access-Control-Allow-Origin", Resp)),
        ?assertEqual(
            "Cache-Control, Content-Type, Server",
            proplists:get_value("Access-Control-Expose-Headers", Resp))
    end).

should_make_preflight_request(_, {_, _, Url, DefaultHeaders}) ->
    ?_assertEqual(?SUPPORTED_METHODS,
        begin
            Headers = DefaultHeaders
                      ++ [{"Access-Control-Request-Method", "GET"}],
            {ok, _, Resp, _} = test_request:options(Url, Headers),
            proplists:get_value("Access-Control-Allow-Methods", Resp)
        end).

should_make_prefligh_request_with_port({_, VHost}, {_, _, Url, _}) ->
    ?_assertEqual("http://example.com:5984",
        begin
            couch_config:set("cors", "origins", "http://example.com:5984",
                             false),
            Headers = [{"Origin", "http://example.com:5984"},
                       {"Access-Control-Request-Method", "GET"}]
                      ++ maybe_append_vhost(VHost),
            {ok, _, Resp, _} = test_request:options(Url, Headers),
            proplists:get_value("Access-Control-Allow-Origin", Resp)
        end).

should_make_prefligh_request_with_scheme({_, VHost}, {_, _, Url, _}) ->
    ?_assertEqual("https://example.com:5984",
        begin
            couch_config:set("cors", "origins", "https://example.com:5984",
                             false),
            Headers = [{"Origin", "https://example.com:5984"},
                       {"Access-Control-Request-Method", "GET"}]
                      ++ maybe_append_vhost(VHost),
            {ok, _, Resp, _} = test_request:options(Url, Headers),
            proplists:get_value("Access-Control-Allow-Origin", Resp)
        end).

should_make_prefligh_request_with_wildcard_origin({_, VHost}, {_, _, Url, _}) ->
    ?_assertEqual("https://example.com:5984",
        begin
            couch_config:set("cors", "origins", "*", false),
            Headers = [{"Origin", "https://example.com:5984"},
                       {"Access-Control-Request-Method", "GET"}]
                      ++ maybe_append_vhost(VHost),
            {ok, _, Resp, _} = test_request:options(Url, Headers),
            proplists:get_value("Access-Control-Allow-Origin", Resp)
        end).

should_make_request_with_credentials(_, {_, _, Url, DefaultHeaders}) ->
    ?_assertEqual("true",
        begin
            ok = couch_config:set("cors", "credentials", "true", false),
            {ok, _, Resp, _} = test_request:options(Url, DefaultHeaders),
            proplists:get_value("Access-Control-Allow-Credentials", Resp)
        end).

should_make_origin_request_with_auth(_, {_, _, Url, DefaultHeaders}) ->
    ?_assertEqual("http://example.com",
        begin
            Hashed = couch_passwords:hash_admin_password(<<"test">>),
            couch_config:set("admins", "test", Hashed, false),
            {ok, _, Resp, _} = test_request:get(
                Url, DefaultHeaders, [{basic_auth, {"test", "test"}}]),
            couch_config:delete("admins", "test", false),
            proplists:get_value("Access-Control-Allow-Origin", Resp)
        end).

should_make_preflight_request_with_auth(_, {_, _, Url, DefaultHeaders}) ->
    ?_assertEqual(?SUPPORTED_METHODS,
        begin
            Hashed = couch_passwords:hash_admin_password(<<"test">>),
            couch_config:set("admins", "test", Hashed, false),
            Headers = DefaultHeaders
                      ++ [{"Access-Control-Request-Method", "GET"}],
            {ok, _, Resp, _} = test_request:options(
                Url, Headers, [{basic_auth, {"test", "test"}}]),
            couch_config:delete("admins", "test", false),
            proplists:get_value("Access-Control-Allow-Methods", Resp)
        end).

should_not_return_cors_headers_for_invalid_origin({Host, _}) ->
    ?_assertEqual(undefined,
        begin
            Headers = [{"Origin", "http://127.0.0.1"}],
            {ok, _, Resp, _} = test_request:get(Host, Headers),
            proplists:get_value("Access-Control-Allow-Origin", Resp)
        end).

should_not_return_cors_headers_for_invalid_origin_preflight({Host, _}) ->
    ?_assertEqual(undefined,
        begin
            Headers = [{"Origin", "http://127.0.0.1"},
                       {"Access-Control-Request-Method", "GET"}],
            {ok, _, Resp, _} = test_request:options(Host, Headers),
            proplists:get_value("Access-Control-Allow-Origin", Resp)
        end).

should_make_request_against_attachment({Host, DbName}) ->
    {"COUCHDB-1689",
     ?_assertEqual(200,
         begin
             Url = Host ++ "/" ++ DbName,
             {ok, Code0, _, _} = test_request:put(
                 Url ++ "/doc/file.txt", [{"Content-Type", "text/plain"}],
                 "hello, couch!"),
             ?assert(Code0 =:= 201),
             {ok, Code, _, _} = test_request:get(
                 Url ++ "/doc?attachments=true",
                 [{"Origin", "http://example.com"}]),
             Code
         end)}.

should_make_range_request_against_attachment({Host, DbName}) ->
    {"COUCHDB-1689",
     ?_assertEqual(206,
         begin
             Url = Host ++ "/" ++ DbName,
             {ok, Code0, _, _} = test_request:put(
                 Url ++ "/doc/file.txt",
                 [{"Content-Type", "application/octet-stream"}],
                 "hello, couch!"),
             ?assert(Code0 =:= 201),
             {ok, Code, _, _} = test_request:get(
                 Url ++ "/doc/file.txt", [{"Origin", "http://example.com"},
                                          {"Range", "bytes=0-6"}]),
             Code
         end)}.

should_make_request_with_if_none_match_header({Host, DbName}) ->
    {"COUCHDB-1697",
     ?_assertEqual(304,
         begin
             Url = Host ++ "/" ++ DbName,
             {ok, Code0, Headers0, _} = test_request:put(
                 Url ++ "/doc", [{"Content-Type", "application/json"}], "{}"),
             ?assert(Code0 =:= 201),
             ETag = proplists:get_value("ETag", Headers0),
             {ok, Code, _, _} = test_request:get(
                 Url ++ "/doc", [{"Origin", "http://example.com"},
                                 {"If-None-Match", ETag}]),
             Code
        end)}.


maybe_append_vhost(true) ->
    [{"Host", "http://example.com"}];
maybe_append_vhost(false) ->
    [].
