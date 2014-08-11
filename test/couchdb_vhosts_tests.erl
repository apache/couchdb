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

-module(couchdb_vhosts_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(ADMIN_USER, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).
-define(TIMEOUT, 1000).
-define(iofmt(S, A), lists:flatten(io_lib:format(S, A))).


start() ->
    {ok, Pid} = couch_server_sup:start_link(?CONFIG_CHAIN),
    Pid.

stop(Pid) ->
    erlang:monitor(process, Pid),
    couch_server_sup:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, server_stop})
    end.

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_USER]),
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
                    path: '/' + req.path.join('/')}};}">>}
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
    couch_db:close(Db),

    Addr = couch_config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    Url = "http://" ++ Addr ++ ":" ++ Port,
    {Url, ?b2l(DbName)}.

setup_oauth() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_USER]),

    couch_config:set("couch_httpd_auth", "authentication_db",
                     ?b2l(?tempdb()), false),
    couch_config:set("oauth_token_users", "otoksec1", "joe", false),
    couch_config:set("oauth_consumer_secrets", "consec1", "foo", false),
    couch_config:set("oauth_token_secrets", "otoksec1", "foobar", false),
    couch_config:set("couch_httpd_auth", "require_valid_user", "true", false),

    ok = couch_config:set(
        "vhosts", "oauth-example.com",
        "/" ++ ?b2l(DbName) ++ "/_design/test/_rewrite/foobar", false),

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

    couch_db:ensure_full_commit(Db),
    couch_db:close(Db),

    Addr = couch_config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    Url = "http://" ++ Addr ++ ":" ++ Port,
    {Url, ?b2l(DbName)}.

teardown({_, DbName}) ->
    ok = couch_server:delete(?l2b(DbName), []),
    ok.


vhosts_test_() ->
    {
        "Virtual Hosts rewrite tests",
        {
            setup,
            fun start/0, fun stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_return_database_info/1,
                    fun should_return_revs_info/1,
                    fun should_serve_utils_for_vhost/1,
                    fun should_return_virtual_request_path_field_in_request/1,
                    fun should_return_real_request_path_field_in_request/1,
                    fun should_match_wildcard_vhost/1,
                    fun should_return_db_info_for_wildcard_vhost_for_custom_db/1,
                    fun should_replace_rewrite_variables_for_db_and_doc/1,
                    fun should_return_db_info_for_vhost_with_resource/1,
                    fun should_return_revs_info_for_vhost_with_resource/1,
                    fun should_return_db_info_for_vhost_with_wildcard_resource/1,
                    fun should_return_path_for_vhost_with_wildcard_host/1
                ]
            }
        }
    }.

oauth_test_() ->
    {
        "Virtual Hosts OAuth tests",
        {
            setup,
            fun start/0, fun stop/1,
            {
                foreach,
                fun setup_oauth/0, fun teardown/1,
                [
                    fun should_require_auth/1,
                    fun should_succeed_oauth/1,
                    fun should_fail_oauth_with_wrong_credentials/1
                ]
            }
        }
    }.


should_return_database_info({Url, DbName}) ->
    ?_test(begin
        ok = couch_config:set("vhosts", "example.com", "/" ++ DbName, false),
        case test_request:get(Url, [], [{host_header, "example.com"}]) of
            {ok, _, _, Body} ->
                {JsonBody} = ejson:decode(Body),
                ?assert(proplists:is_defined(<<"db_name">>, JsonBody));
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).

should_return_revs_info({Url, DbName}) ->
    ?_test(begin
        ok = couch_config:set("vhosts", "example.com", "/" ++ DbName, false),
        case test_request:get(Url ++ "/doc1?revs_info=true", [],
                              [{host_header, "example.com"}]) of
            {ok, _, _, Body} ->
                {JsonBody} = ejson:decode(Body),
                ?assert(proplists:is_defined(<<"_revs_info">>, JsonBody));
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).

should_serve_utils_for_vhost({Url, DbName}) ->
    ?_test(begin
        ok = couch_config:set("vhosts", "example.com", "/" ++ DbName, false),
        case test_request:get(Url ++ "/_utils/index.html", [],
                              [{host_header, "example.com"}]) of
            {ok, _, _, Body} ->
                ?assertMatch(<<"<!DOCTYPE html>", _/binary>>, Body);
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).

should_return_virtual_request_path_field_in_request({Url, DbName}) ->
    ?_test(begin
        ok = couch_config:set("vhosts", "example1.com",
                              "/" ++ DbName ++ "/_design/doc1/_rewrite/",
                              false),
        case test_request:get(Url, [], [{host_header, "example1.com"}]) of
            {ok, _, _, Body} ->
                {Json} = ejson:decode(Body),
                ?assertEqual(<<"/">>,
                             proplists:get_value(<<"requested_path">>, Json));
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).

should_return_real_request_path_field_in_request({Url, DbName}) ->
    ?_test(begin
        ok = couch_config:set("vhosts", "example1.com",
                              "/" ++ DbName ++ "/_design/doc1/_rewrite/",
                              false),
        case test_request:get(Url, [], [{host_header, "example1.com"}]) of
            {ok, _, _, Body} ->
                {Json} = ejson:decode(Body),
                Path = ?l2b("/" ++ DbName ++ "/_design/doc1/_show/test"),
                ?assertEqual(Path, proplists:get_value(<<"path">>, Json));
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).

should_match_wildcard_vhost({Url, DbName}) ->
    ?_test(begin
        ok = couch_config:set("vhosts", "*.example.com",
                              "/" ++ DbName ++ "/_design/doc1/_rewrite", false),
        case test_request:get(Url, [], [{host_header, "test.example.com"}]) of
            {ok, _, _, Body} ->
                {Json} = ejson:decode(Body),
                Path = ?l2b("/" ++ DbName ++ "/_design/doc1/_show/test"),
                ?assertEqual(Path, proplists:get_value(<<"path">>, Json));
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).

should_return_db_info_for_wildcard_vhost_for_custom_db({Url, DbName}) ->
    ?_test(begin
        ok = couch_config:set("vhosts", ":dbname.example1.com",
                              "/:dbname", false),
        Host = DbName ++ ".example1.com",
        case test_request:get(Url, [], [{host_header, Host}]) of
            {ok, _, _, Body} ->
                {JsonBody} = ejson:decode(Body),
                ?assert(proplists:is_defined(<<"db_name">>, JsonBody));
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).

should_replace_rewrite_variables_for_db_and_doc({Url, DbName}) ->
    ?_test(begin
        ok = couch_config:set("vhosts",":appname.:dbname.example1.com",
                              "/:dbname/_design/:appname/_rewrite/", false),
        Host = "doc1." ++ DbName ++ ".example1.com",
        case test_request:get(Url, [], [{host_header, Host}]) of
            {ok, _, _, Body} ->
                {Json} = ejson:decode(Body),
                Path = ?l2b("/" ++ DbName ++ "/_design/doc1/_show/test"),
                ?assertEqual(Path, proplists:get_value(<<"path">>, Json));
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).

should_return_db_info_for_vhost_with_resource({Url, DbName}) ->
    ?_test(begin
        ok = couch_config:set("vhosts",
                              "example.com/test", "/" ++ DbName, false),
        ReqUrl = Url ++ "/test",
        case test_request:get(ReqUrl, [], [{host_header, "example.com"}]) of
            {ok, _, _, Body} ->
                {JsonBody} = ejson:decode(Body),
                ?assert(proplists:is_defined(<<"db_name">>, JsonBody));
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).


should_return_revs_info_for_vhost_with_resource({Url, DbName}) ->
    ?_test(begin
        ok = couch_config:set("vhosts",
                              "example.com/test", "/" ++ DbName, false),
        ReqUrl = Url ++ "/test/doc1?revs_info=true",
        case test_request:get(ReqUrl, [], [{host_header, "example.com"}]) of
            {ok, _, _, Body} ->
                {JsonBody} = ejson:decode(Body),
                ?assert(proplists:is_defined(<<"_revs_info">>, JsonBody));
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).

should_return_db_info_for_vhost_with_wildcard_resource({Url, DbName}) ->
    ?_test(begin
        ok = couch_config:set("vhosts", "*.example2.com/test", "/*", false),
        ReqUrl = Url ++ "/test",
        Host = DbName ++ ".example2.com",
        case test_request:get(ReqUrl, [], [{host_header, Host}]) of
            {ok, _, _, Body} ->
                {JsonBody} = ejson:decode(Body),
                ?assert(proplists:is_defined(<<"db_name">>, JsonBody));
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).

should_return_path_for_vhost_with_wildcard_host({Url, DbName}) ->
    ?_test(begin
        ok = couch_config:set("vhosts", "*/test1",
                              "/" ++ DbName ++ "/_design/doc1/_show/test",
                              false),
        case test_request:get(Url ++ "/test1") of
            {ok, _, _, Body} ->
                {Json} = ejson:decode(Body),
                Path = ?l2b("/" ++ DbName ++ "/_design/doc1/_show/test"),
                ?assertEqual(Path, proplists:get_value(<<"path">>, Json));
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).

should_require_auth({Url, _}) ->
    ?_test(begin
        case test_request:get(Url, [], [{host_header, "oauth-example.com"}]) of
            {ok, Code, _, Body} ->
                ?assertEqual(401, Code),
                {JsonBody} = ejson:decode(Body),
                ?assertEqual(<<"unauthorized">>,
                             couch_util:get_value(<<"error">>, JsonBody));
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).

should_succeed_oauth({Url, _}) ->
    ?_test(begin
        AuthDbName = couch_config:get("couch_httpd_auth", "authentication_db"),
        JoeDoc = couch_doc:from_json_obj({[
            {<<"_id">>, <<"org.couchdb.user:joe">>},
            {<<"type">>, <<"user">>},
            {<<"name">>, <<"joe">>},
            {<<"roles">>, []},
            {<<"password_sha">>, <<"fe95df1ca59a9b567bdca5cbaf8412abd6e06121">>},
            {<<"salt">>, <<"4e170ffeb6f34daecfd814dfb4001a73">>}
        ]}),
        {ok, AuthDb} = couch_db:open_int(?l2b(AuthDbName), [?ADMIN_USER]),
        {ok, _} = couch_db:update_doc(AuthDb, JoeDoc, [?ADMIN_USER]),

        Host = "oauth-example.com",
        Consumer = {"consec1", "foo", hmac_sha1},
        SignedParams = oauth:sign(
            "GET", "http://" ++ Host ++ "/", [], Consumer, "otoksec1", "foobar"),
        OAuthUrl = oauth:uri(Url, SignedParams),

        case test_request:get(OAuthUrl, [], [{host_header, Host}]) of
            {ok, Code, _, Body} ->
                ?assertEqual(200, Code),
                {JsonBody} = ejson:decode(Body),
                ?assertEqual(<<"test">>,
                             couch_util:get_value(<<"name">>, JsonBody));
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).

should_fail_oauth_with_wrong_credentials({Url, _}) ->
    ?_test(begin
        AuthDbName = couch_config:get("couch_httpd_auth", "authentication_db"),
        JoeDoc = couch_doc:from_json_obj({[
            {<<"_id">>, <<"org.couchdb.user:joe">>},
            {<<"type">>, <<"user">>},
            {<<"name">>, <<"joe">>},
            {<<"roles">>, []},
            {<<"password_sha">>, <<"fe95df1ca59a9b567bdca5cbaf8412abd6e06121">>},
            {<<"salt">>, <<"4e170ffeb6f34daecfd814dfb4001a73">>}
        ]}),
        {ok, AuthDb} = couch_db:open_int(?l2b(AuthDbName), [?ADMIN_USER]),
        {ok, _} = couch_db:update_doc(AuthDb, JoeDoc, [?ADMIN_USER]),

        Host = "oauth-example.com",
        Consumer = {"consec1", "bad_secret", hmac_sha1},
        SignedParams = oauth:sign(
            "GET", "http://" ++ Host ++ "/", [], Consumer, "otoksec1", "foobar"),
        OAuthUrl = oauth:uri(Url, SignedParams),

        case test_request:get(OAuthUrl, [], [{host_header, Host}]) of
            {ok, Code, _, Body} ->
                ?assertEqual(401, Code),
                {JsonBody} = ejson:decode(Body),
                ?assertEqual(<<"unauthorized">>,
                             couch_util:get_value(<<"error">>, JsonBody));
            Else ->
                erlang:error({assertion_failed,
                             [{module, ?MODULE},
                              {line, ?LINE},
                              {reason, ?iofmt("Request failed: ~p", [Else])}]})
        end
    end).
