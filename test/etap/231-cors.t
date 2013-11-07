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

-record(user_ctx, {
    name = null,
    roles = [],
    handler
}).


-define(SUPPORTED_METHODS, "GET, HEAD, POST, PUT, DELETE, TRACE, CONNECT, COPY, OPTIONS").
server() ->
    lists:concat([
        "http://127.0.0.1:",
        mochiweb_socket_server:get(couch_httpd, port),
        "/"
    ]).


main(_) ->
    test_util:init_code_path(),

    etap:plan(30),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

dbname() -> "etap-test-db".
dbname1() -> "etap-test-db1".
dbname2() -> "etap-test-db2".

admin_user_ctx() -> {user_ctx, #user_ctx{roles=[<<"_admin">>]}}.

set_admin_password(UserName, Password) ->
    Hashed = couch_passwords:hash_admin_password(Password),
    couch_config:set("admins", UserName, Hashed, false).

cycle_db(DbName) ->
    couch_server:delete(list_to_binary(DbName), [admin_user_ctx()]),
    {ok, Db} = couch_db:create(list_to_binary(DbName), [admin_user_ctx()]),
    Db.

test() ->

    ibrowse:start(),
    crypto:start(),

    %% launch couchdb
    couch_server_sup:start_link(test_util:config_files()),

    %% initialize db
    timer:sleep(1000),
    Db = cycle_db(dbname()),
    Db1 = cycle_db(dbname1()),
    Db2 = cycle_db(dbname2()),

    % CORS is disabled by default
    test_no_headers_server(),
    test_no_headers_db(),

    % Now enable CORS
    ok = couch_config:set("httpd", "enable_cors", "true", false),
    ok = couch_config:set("cors", "origins", "http://example.com", false),

    %% do tests
    test_incorrect_origin_simple_request(),
    test_incorrect_origin_preflight_request(),

    test_preflight_request(),
    test_db_request(),
    test_doc_with_attachment_request(),
    test_doc_with_attachment_range_request(),
    test_db_preflight_request(),
    test_db_origin_request(),
    test_db1_origin_request(),
    test_preflight_with_port1(),
    test_preflight_with_scheme1(),

    ok = couch_config:set("cors", "origins", "http://example.com:5984", false),
    test_preflight_with_port2(),

    ok = couch_config:set("cors", "origins", "https://example.com:5984", false),
    test_preflight_with_scheme2(),

    ok = couch_config:set("cors", "origins", "*", false),
    test_preflight_with_wildcard(),

    ok = couch_config:set("cors", "origins", "http://example.com", false),
    test_case_sensitive_mismatch_of_allowed_origins(),

    % http://www.w3.org/TR/cors/#supports-credentials
    % 6.1.3
    % If the resource supports credentials add a single
    % Access-Control-Allow-Origin header, with the value
    % of the Origin header as value, and add a single
    % Access-Control-Allow-Credentials header with the
    % case-sensitive string "true" as value.
    % Otherwise, add a single Access-Control-Allow-Origin
    % header, with either the value of the Origin header
    % or the string "*" as value.
    % Note: The string "*" cannot be used for a resource
    % that supports credentials.
    test_db_request_credentials_header_off(),
    ok = couch_config:set("cors", "credentials", "true", false),
    test_db_request_credentials_header_on(),
    % We don’t test wildcards & credentials as that would
    % fall into the realm of validating config values
    % which we don’t do at all yet

    % test with vhosts
    ok = couch_config:set("vhosts", "example.com", "/", false),
    test_preflight_request(true),
    test_db_request(true),
    test_db_preflight_request(true),
    test_db_origin_request(true),
    test_db1_origin_request(true),
    test_preflight_with_port1(true),
    test_preflight_with_scheme1(true),

    % TBD
    % test multiple per-host configuration

    %% do tests with auth
    ok = set_admin_password("test", "test"),

    test_db_preflight_auth_request(),
    test_db_origin_auth_request(),


    %% restart boilerplate
    catch couch_db:close(Db),
    catch couch_db:close(Db1),
    catch couch_db:close(Db2),

    couch_server:delete(list_to_binary(dbname()), [admin_user_ctx()]),
    couch_server:delete(list_to_binary(dbname1()), [admin_user_ctx()]),
    couch_server:delete(list_to_binary(dbname2()), [admin_user_ctx()]),

    timer:sleep(3000),
    couch_server_sup:stop(),
    ok.

test_preflight_request() -> test_preflight_request(false).
test_db_request() -> test_db_request(false).
test_db_preflight_request() -> test_db_preflight_request(false).
test_db_origin_request() -> test_db_origin_request(false).
test_db1_origin_request() -> test_db1_origin_request(false).
test_preflight_with_port1() -> test_preflight_with_port1(false).
test_preflight_with_scheme1() -> test_preflight_with_scheme1(false).

%% Cors is disabled, should not return Access-Control-Allow-Origin
test_no_headers_server() ->
    Headers = [{"Origin", "http://127.0.0.1"}],
    {ok, _, Resp, _} = ibrowse:send_req(server(), Headers, get, []),
    etap:is(proplists:get_value("Access-Control-Allow-Origin", Resp),
            undefined, "No CORS Headers when disabled").

%% Cors is disabled, should not return Access-Control-Allow-Origin
test_no_headers_db() ->
    Headers = [{"Origin", "http://127.0.0.1"}],
    Url = server() ++ "etap-test-db",
    {ok, _, Resp, _} = ibrowse:send_req(Url, Headers, get, []),
    etap:is(proplists:get_value("Access-Control-Allow-Origin", Resp),
            undefined, "No CORS Headers when disabled").

test_incorrect_origin_simple_request() ->
    Headers = [{"Origin", "http://127.0.0.1"}],
    {ok, _, RespHeaders, _} = ibrowse:send_req(server(), Headers, get, []),
    etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            undefined,
            "Specified invalid origin, no Access").

test_incorrect_origin_preflight_request() ->
    Headers = [{"Origin", "http://127.0.0.1"},
               {"Access-Control-Request-Method", "GET"}],
    {ok, _, RespHeaders, _} = ibrowse:send_req(server(), Headers, options, []),
    etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            undefined,
            "invalid origin").

test_preflight_request(VHost) ->
    Headers = [{"Origin", "http://example.com"},
               {"Access-Control-Request-Method", "GET"}]
               ++ maybe_append_vhost(VHost),

    case ibrowse:send_req(server(), Headers, options, []) of
    {ok, _, RespHeaders, _}  ->
        etap:is(proplists:get_value("Access-Control-Allow-Methods", RespHeaders),
            ?SUPPORTED_METHODS,
            "test_preflight_request Access-Control-Allow-Methods ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_db_request(VHost) ->
    Headers = [{"Origin", "http://example.com"}]
               ++ maybe_append_vhost(VHost),
    Url = server() ++ "etap-test-db",
    case ibrowse:send_req(Url, Headers, get, []) of
    {ok, _, RespHeaders, _Body} ->
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            "http://example.com",
            "db Access-Control-Allow-Origin ok"),
        etap:is(proplists:get_value("Access-Control-Expose-Headers", RespHeaders),
            "Cache-Control, Content-Type, Server",
            "db Access-Control-Expose-Headers ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

% COUCHDB-1689
test_doc_with_attachment_request() ->
    DocUrl = server() ++ "etap-test-db/doc1",
    ibrowse:send_req(DocUrl ++ "/attachment.txt",
        [{"Content-Type", "text/plain"}], put, "this is a text attachment"),

    Headers = [{"Origin", "http://example.com"}],
    Url = DocUrl ++ "?attachments=true",
    case ibrowse:send_req(Url, Headers, get, []) of
    {ok, Code, _RespHeaders, _Body} ->
        etap:is(Code, "200", "Response without errors");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

% COUCHDB-1689
test_doc_with_attachment_range_request() ->
    AttachmentUrl = server() ++ "etap-test-db/doc2/attachment.bin",
    % Use a Content-Type that doesn't get compressed
    ibrowse:send_req(AttachmentUrl,
        [{"Content-Type", "application/octet-stream"}], put,
        "this is an attachment"),

    Headers = [{"Origin", "http://example.com"}, {"Range", "bytes=0-6"}],
    case ibrowse:send_req(AttachmentUrl, Headers, get, []) of
    {ok, Code, _RespHeaders, _Body} ->
        etap:is(Code, "206", "Response without errors");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

% COUCHDB-1697
test_if_none_match_header() ->
    Url = server() ++ "etap-test-db/doc2",
    Headers = [{"Origin", "http://example.com"}],
    {ok, _, _RespHeaders, _} = ibrowse:send_req(Url, Headers, get, []),
    ETag = proplists:get_value("ETag", _RespHeaders),
    Headers2 = [{"Origin", "http://example.com"}, {"If-None-Match", ETag}],
    case ibrowse:send_req(Url, Headers2, get, []) of
    {ok, Code, _RespHeaders2, _} ->
        etap:is(Code, "304", "Responded with Not Modified");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_db_request_credentials_header_off() ->
    Headers = [{"Origin", "http://example.com"}],
    Url = server() ++ "etap-test-db",
    case ibrowse:send_req(Url, Headers, get, []) of
    {ok, _, RespHeaders, _Body} ->
        etap:is(proplists:get_value("Access-Control-Allow-Credentials", RespHeaders),
            undefined,
            "db Access-Control-Allow-Credentials off");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_db_request_credentials_header_on() ->
    Headers = [{"Origin", "http://example.com"}],
    Url = server() ++ "etap-test-db",
    case ibrowse:send_req(Url, Headers, get, []) of
    {ok, _, RespHeaders, _Body} ->
        etap:is(proplists:get_value("Access-Control-Allow-Credentials", RespHeaders),
            "true",
            "db Access-Control-Allow-Credentials ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_db_preflight_request(VHost) ->
    Url = server() ++ "etap-test-db",
    Headers = [{"Origin", "http://example.com"},
               {"Access-Control-Request-Method", "GET"}]
               ++ maybe_append_vhost(VHost),
    case ibrowse:send_req(Url, Headers, options, []) of
    {ok, _, RespHeaders, _} ->
        etap:is(proplists:get_value("Access-Control-Allow-Methods", RespHeaders),
                ?SUPPORTED_METHODS,
                "db Access-Control-Allow-Methods ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.


test_db_origin_request(VHost) ->
    Headers = [{"Origin", "http://example.com"}]
               ++ maybe_append_vhost(VHost),
    Url = server() ++ "etap-test-db",
    case ibrowse:send_req(Url, Headers, get, []) of
    {ok, _, RespHeaders, _Body} ->
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            "http://example.com",
            "db origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_db1_origin_request(VHost) ->
    Headers = [{"Origin", "http://example.com"}]
               ++ maybe_append_vhost(VHost),
    Url = server() ++ "etap-test-db1",
    case ibrowse:send_req(Url, Headers, get, [], [{host_header, "example.com"}]) of
    {ok, _, RespHeaders, _Body} ->
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            "http://example.com",
            "db origin ok");
    _Else ->
        io:format("else ~p~n", [_Else]),
        etap:is(false, true, "ibrowse failed")
    end.

test_db_preflight_auth_request() ->
    Url = server() ++ "etap-test-db2",
    Headers = [{"Origin", "http://example.com"},
               {"Access-Control-Request-Method", "GET"}],
    case ibrowse:send_req(Url, Headers, options, []) of
    {ok, _Status, RespHeaders, _} ->
        etap:is(proplists:get_value("Access-Control-Allow-Methods", RespHeaders),
                ?SUPPORTED_METHODS,
                "db Access-Control-Allow-Methods ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.


test_db_origin_auth_request() ->
    Headers = [{"Origin", "http://example.com"}],
    Url = server() ++ "etap-test-db2",

    case ibrowse:send_req(Url, Headers, get, [],
        [{basic_auth, {"test", "test"}}]) of
    {ok, _, RespHeaders, _Body} ->
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            "http://example.com",
            "db origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_preflight_with_wildcard() ->
    Headers = [{"Origin", "http://example.com"},
               {"Access-Control-Request-Method", "GET"}],
    case ibrowse:send_req(server(), Headers, options, []) of
    {ok, _, RespHeaders, _}  ->
        % I would either expect the current origin or a wildcard to be returned
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            "http://example.com",
            "db origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_preflight_with_port1(VHost) ->
    Headers = [{"Origin", "http://example.com:5984"},
               {"Access-Control-Request-Method", "GET"}]
               ++ maybe_append_vhost(VHost),
    case ibrowse:send_req(server(), Headers, options, []) of
    {ok, _, RespHeaders, _}  ->
        % I would either expect the current origin or a wildcard to be returned
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            undefined,
            "check non defined host:port in origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_preflight_with_port2() ->
    Headers = [{"Origin", "http://example.com:5984"},
               {"Access-Control-Request-Method", "GET"}],
    case ibrowse:send_req(server(), Headers, options, []) of
    {ok, _, RespHeaders, _}  ->
        % I would either expect the current origin or a wildcard to be returned
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            "http://example.com:5984",
            "check host:port in origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_preflight_with_scheme1(VHost) ->
    Headers = [{"Origin", "https://example.com:5984"},
               {"Access-Control-Request-Method", "GET"}]
               ++ maybe_append_vhost(VHost),
    case ibrowse:send_req(server(), Headers, options, []) of
    {ok, _, RespHeaders, _}  ->
        % I would either expect the current origin or a wildcard to be returned
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            undefined,
            "check non defined scheme in origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_preflight_with_scheme2() ->
    Headers = [{"Origin", "https://example.com:5984"},
               {"Access-Control-Request-Method", "GET"}],
    case ibrowse:send_req(server(), Headers, options, []) of
    {ok, _, RespHeaders, _}  ->
        % I would either expect the current origin or a wildcard to be returned
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            "https://example.com:5984",
            "check scheme in origin ok");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

test_case_sensitive_mismatch_of_allowed_origins() ->
    Headers = [{"Origin", "http://EXAMPLE.COM"}],
    Url = server() ++ "etap-test-db",
    case ibrowse:send_req(Url, Headers, get, []) of
    {ok, _, RespHeaders, _Body} ->
        etap:is(proplists:get_value("Access-Control-Allow-Origin", RespHeaders),
            undefined,
            "db access config case mismatch");
    _ ->
        etap:is(false, true, "ibrowse failed")
    end.

maybe_append_vhost(true) ->
    [{"Host", "http://example.com"}];
maybe_append_vhost(Else) ->
    [].
