#!/usr/bin/env escript
%% -*- erlang -*-

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

%% XXX: Figure out how to -include("couch_rep.hrl")
-record(http_db, {
    url,
    auth = [],
    resource = "",
    headers = [
        {"User-Agent", "CouchDB/"++couch_server:get_version()},
        {"Accept", "application/json"},
        {"Accept-Encoding", "gzip"}
    ],
    qs = [],
    method = get,
    body = nil,
    options = [
        {response_format,binary},
        {inactivity_timeout, 30000}
    ],
    retries = 10,
    pause = 1,
    conn = nil
}).

-record(user_ctx, {
    name = null,
    roles = [],
    handler
}).

server() ->
    lists:concat([
        "http://127.0.0.1:", mochiweb_socket_server:get(couch_httpd, port), "/"
    ]).

dbname() -> "etap-test-db".
admin_user_ctx() -> {user_ctx, #user_ctx{roles=[<<"_admin">>]}}.

main(_) ->
    test_util:init_code_path(),

    etap:plan(18),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    couch_server_sup:start_link(test_util:config_files()),
    ibrowse:start(),
    crypto:start(),

    timer:sleep(1000),
    couch_server:delete(list_to_binary(dbname()), [admin_user_ctx()]),
    {ok, Db} = couch_db:create(list_to_binary(dbname()), [admin_user_ctx()]),

    Doc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"doc1">>},
        {<<"value">>, 666}
    ]}),

    Doc1 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/doc1">>},
        {<<"shows">>, {[
            {<<"test">>, <<"function(doc, req) {
    return { json: {
        requested_path: '/' + req.requested_path.join('/'),
        path: '/' + req.path.join('/')
    }};
}">>}
        ]}},
        {<<"rewrites">>, [
            {[
                {<<"from">>, <<"/">>},
                {<<"to">>, <<"_show/test">>}
            ]}
        ]}
    ]}),

    {ok, _} = couch_db:update_docs(Db, [Doc, Doc1]),

    couch_db:ensure_full_commit(Db),

    %% end boilerplate, start test

    ok = couch_config:set("vhosts", "example.com", "/etap-test-db", false),
    ok = couch_config:set("vhosts", "*.example.com",
            "/etap-test-db/_design/doc1/_rewrite", false),
    ok = couch_config:set("vhosts", "example.com/test", "/etap-test-db", false),
    ok = couch_config:set("vhosts", "example1.com",
            "/etap-test-db/_design/doc1/_rewrite/", false),
    ok = couch_config:set("vhosts",":appname.:dbname.example1.com",
            "/:dbname/_design/:appname/_rewrite/", false),
    ok = couch_config:set("vhosts", ":dbname.example1.com", "/:dbname", false),

    ok = couch_config:set("vhosts", "*.example2.com", "/*", false),
    ok = couch_config:set("vhosts", "*.example2.com/test", "/*", false),
    ok = couch_config:set("vhosts", "*/test", "/etap-test-db", false),
    ok = couch_config:set("vhosts", "*/test1",
            "/etap-test-db/_design/doc1/_show/test", false),
    ok = couch_config:set("vhosts", "example3.com", "/", false),

    %% reload rules
    couch_httpd_vhost:reload(),

    test_regular_request(),
    test_vhost_request(),
    test_vhost_request_with_qs(),
    test_vhost_request_with_global(),
    test_vhost_requested_path(),
    test_vhost_requested_path_path(),
    test_vhost_request_wildcard(),
    test_vhost_request_replace_var(),
    test_vhost_request_replace_var1(),
    test_vhost_request_replace_wildcard(),
    test_vhost_request_path(),
    test_vhost_request_path1(),
    test_vhost_request_path2(),
    test_vhost_request_path3(),
    test_vhost_request_to_root(),
    test_vhost_request_with_oauth(Db),

    %% restart boilerplate
    couch_db:close(Db),
    ok = couch_server:delete(couch_db:name(Db), [admin_user_ctx()]),
    timer:sleep(3000),
    couch_server_sup:stop(),

    ok.

test_regular_request() ->
    Result = case ibrowse:send_req(server(), [], get, []) of
        {ok, _, _, Body} ->
            {[{<<"couchdb">>, <<"Welcome">>},
              {<<"version">>,_}
            ]} = ejson:decode(Body),
            etap:is(true, true, "should return server info");
        _Else ->
            etap:is(false, true, <<"ibrowse fail">>)
    end.

test_vhost_request() ->
    case ibrowse:send_req(server(), [], get, [], [{host_header, "example.com"}]) of
        {ok, _, _, Body} ->
            {JsonBody} = ejson:decode(Body),
            HasDbNameInfo = proplists:is_defined(<<"db_name">>, JsonBody),
            etap:is(HasDbNameInfo, true, "should return database info");
        _Else ->
           etap:is(false, true, <<"ibrowse fail">>)
    end.

test_vhost_request_with_qs() ->
    Url = server() ++ "doc1?revs_info=true",
    case ibrowse:send_req(Url, [], get, [], [{host_header, "example.com"}]) of
        {ok, _, _, Body} ->
            {JsonProps} = ejson:decode(Body),
            HasRevsInfo = proplists:is_defined(<<"_revs_info">>, JsonProps),
            etap:is(HasRevsInfo, true, "should return _revs_info");
        _Else ->
            etap:is(false, true, <<"ibrowse fail">>)
    end.

test_vhost_request_with_global() ->
    Url2 = server() ++ "_utils/index.html",
    case ibrowse:send_req(Url2, [], get, [], [{host_header, "example.com"}]) of
        {ok, _, _, Body2} ->
            "<!DOCTYPE" ++ _Foo = Body2,
            etap:is(true, true, "should serve /_utils even inside vhosts");
        _Else ->
            etap:is(false, true, <<"ibrowse fail">>)
    end.

test_vhost_requested_path() ->
    case ibrowse:send_req(server(), [], get, [], [{host_header, "example1.com"}]) of
        {ok, _, _, Body} ->
            {Json} = ejson:decode(Body),
            etap:is(case proplists:get_value(<<"requested_path">>, Json) of
                <<"/">> -> true;
                _ -> false
            end, true, <<"requested path in req ok">>);
        _Else ->
            etap:is(false, true, <<"ibrowse fail">>)
    end.

test_vhost_requested_path_path() ->
    case ibrowse:send_req(server(), [], get, [], [{host_header, "example1.com"}]) of
        {ok, _, _, Body} ->
            {Json} = ejson:decode(Body),
            etap:is(case proplists:get_value(<<"path">>, Json) of
                <<"/etap-test-db/_design/doc1/_show/test">> -> true;
                _ -> false
            end, true, <<"path in req ok">>);
        _Else ->
            etap:is(false, true, <<"ibrowse fail">>)
    end.

test_vhost_request_wildcard()->
    case ibrowse:send_req(server(), [], get, [], [{host_header, "test.example.com"}]) of
        {ok, _, _, Body} ->
            {Json} = ejson:decode(Body),
            etap:is(case proplists:get_value(<<"path">>, Json) of
                <<"/etap-test-db/_design/doc1/_show/test">> -> true;
                _ -> false
            end, true, <<"wildcard  ok">>);
        _Else -> etap:is(false, true, <<"ibrowse fail">>)
    end.


test_vhost_request_replace_var() ->
    case ibrowse:send_req(server(), [], get, [], [{host_header,"etap-test-db.example1.com"}]) of
        {ok, _, _, Body} ->
            {JsonBody} = ejson:decode(Body),
            HasDbNameInfo = proplists:is_defined(<<"db_name">>, JsonBody),
            etap:is(HasDbNameInfo, true, "should return database info");
        _Else -> etap:is(false, true, <<"ibrowse fail">>)
    end.

test_vhost_request_replace_var1() ->
    case ibrowse:send_req(server(), [], get, [], [{host_header, "doc1.etap-test-db.example1.com"}]) of
        {ok, _, _, Body} ->
            {Json} = ejson:decode(Body),
            etap:is(case proplists:get_value(<<"path">>, Json) of
                <<"/etap-test-db/_design/doc1/_show/test">> -> true;
                _ -> false
            end, true, <<"wildcard  ok">>);
        _Else -> etap:is(false, true, <<"ibrowse fail">>)
    end.

test_vhost_request_replace_wildcard() ->
    case ibrowse:send_req(server(), [], get, [], [{host_header,"etap-test-db.example2.com"}]) of
        {ok, _, _, Body} ->
            {JsonBody} = ejson:decode(Body),
            HasDbNameInfo = proplists:is_defined(<<"db_name">>, JsonBody),
            etap:is(HasDbNameInfo, true, "should return database info");
        _Else -> etap:is(false, true, <<"ibrowse fail">>)
    end.

test_vhost_request_path() ->
    Uri = server() ++ "test",
    case ibrowse:send_req(Uri, [], get, [], [{host_header, "example.com"}]) of
        {ok, _, _, Body} ->
            {JsonBody} = ejson:decode(Body),
            HasDbNameInfo = proplists:is_defined(<<"db_name">>, JsonBody),
            etap:is(HasDbNameInfo, true, "should return database info");
        _Else -> etap:is(false, true, <<"ibrowse fail">>)
    end.

test_vhost_request_path1() ->
    Url = server() ++ "test/doc1?revs_info=true",
    case ibrowse:send_req(Url, [], get, [], []) of
        {ok, _, _, Body} ->
            {JsonProps} = ejson:decode(Body),
            HasRevsInfo = proplists:is_defined(<<"_revs_info">>, JsonProps),
            etap:is(HasRevsInfo, true, "should return _revs_info");
        _Else -> etap:is(false, true, <<"ibrowse fail">>)
    end.

test_vhost_request_path2() ->
    Uri = server() ++ "test",
    case ibrowse:send_req(Uri, [], get, [], [{host_header,"etap-test-db.example2.com"}]) of
        {ok, _, _, Body} ->
            {JsonBody} = ejson:decode(Body),
            HasDbNameInfo = proplists:is_defined(<<"db_name">>, JsonBody),
            etap:is(HasDbNameInfo, true, "should return database info");
        _Else -> etap:is(false, true, <<"ibrowse fail">>)
    end.

test_vhost_request_path3() ->
    Uri = server() ++ "test1",
    case ibrowse:send_req(Uri, [], get, [], []) of
        {ok, _, _, Body} ->
            {Json} = ejson:decode(Body),
            etap:is(case proplists:get_value(<<"path">>, Json) of
                <<"/etap-test-db/_design/doc1/_show/test">> -> true;
                _ -> false
            end, true, <<"path in req ok">>);
        _Else -> etap:is(false, true, <<"ibrowse fail">>)
    end.

test_vhost_request_to_root() ->
    Uri = server(),
    case ibrowse:send_req(Uri, [], get, [], []) of
        {ok, _, _, Body} ->
            {JsonBody} = ejson:decode(Body),
            HasCouchDBWelcome = proplists:is_defined(<<"couchdb">>, JsonBody),
            etap:is(HasCouchDBWelcome, true, "should allow redirect to /");
        _Else -> etap:is(false, true, <<"ibrowse fail">>)
    end.

test_vhost_request_with_oauth(Db) ->
    {ok, AuthDb} = couch_db:create(
        <<"tap_test_sec_db">>, [admin_user_ctx(), overwrite]),
    PrevAuthDbName = couch_config:get("couch_httpd_auth", "authentication_db"),
    couch_config:set("couch_httpd_auth", "authentication_db", "tap_test_sec_db", false),
    couch_config:set("oauth_token_users", "otoksec1", "joe", false),
    couch_config:set("oauth_consumer_secrets", "consec1", "foo", false),
    couch_config:set("oauth_token_secrets", "otoksec1", "foobar", false),
    couch_config:set("couch_httpd_auth", "require_valid_user", "true", false),

    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/test">>},
        {<<"language">>, <<"javascript">>},
        {<<"rewrites">>, [
            {[
                {<<"from">>, <<"foobar">>},
                {<<"to">>, <<"_info">>}
            ]}
        ]}
    ]}),
    {ok, _} = couch_db:update_doc(Db, DDoc, []),

    RewritePath = "/etap-test-db/_design/test/_rewrite/foobar",
    ok = couch_config:set("vhosts", "oauth-example.com", RewritePath, false),
    couch_httpd_vhost:reload(),

    case ibrowse:send_req(server(), [], get, [], [{host_header, "oauth-example.com"}]) of
        {ok, "401", _, Body} ->
            {JsonBody} = ejson:decode(Body),
            etap:is(
                couch_util:get_value(<<"error">>, JsonBody),
                <<"unauthorized">>,
                "Request without OAuth credentials failed");
        Error ->
           etap:bail("Request without OAuth credentials did not fail: " ++
               couch_util:to_list(Error))
    end,

    JoeDoc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"org.couchdb.user:joe">>},
        {<<"type">>, <<"user">>},
        {<<"name">>, <<"joe">>},
        {<<"roles">>, []},
        {<<"password_sha">>, <<"fe95df1ca59a9b567bdca5cbaf8412abd6e06121">>},
        {<<"salt">>, <<"4e170ffeb6f34daecfd814dfb4001a73">>}
    ]}),
    {ok, _} = couch_db:update_doc(AuthDb, JoeDoc, []),

    Url = "http://oauth-example.com/",
    Consumer = {"consec1", "foo", hmac_sha1},
    SignedParams = oauth:signed_params(
        "GET", Url, [], Consumer, "otoksec1", "foobar"),
    OAuthUrl = oauth:uri(server(), SignedParams),

    case ibrowse:send_req(OAuthUrl, [], get, [], [{host_header, "oauth-example.com"}]) of
        {ok, "200", _, Body2} ->
            {JsonBody2} = ejson:decode(Body2),
            etap:is(couch_util:get_value(<<"name">>, JsonBody2), <<"test">>,
                "should return ddoc info with OAuth credentials");
        Error2 ->
           etap:bail("Failed to access vhost with OAuth credentials: " ++
               couch_util:to_list(Error2))
    end,

    Consumer2 = {"consec1", "bad_secret", hmac_sha1},
    SignedParams2 = oauth:signed_params(
        "GET", Url, [], Consumer2, "otoksec1", "foobar"),
    OAuthUrl2 = oauth:uri(server(), SignedParams2),

    case ibrowse:send_req(OAuthUrl2, [], get, [], [{host_header, "oauth-example.com"}]) of
        {ok, "401", _, Body3} ->
            {JsonBody3} = ejson:decode(Body3),
            etap:is(
                couch_util:get_value(<<"error">>, JsonBody3),
                <<"unauthorized">>,
                "Request with bad OAuth credentials failed");
        Error3 ->
           etap:bail("Failed to access vhost with bad OAuth credentials: " ++
               couch_util:to_list(Error3))
    end,

    couch_config:set("couch_httpd_auth", "authentication_db", PrevAuthDbName, false),
    couch_config:set("couch_httpd_auth", "require_valid_user", "false", false),
    ok = couch_server:delete(couch_db:name(AuthDb), [admin_user_ctx()]).
