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

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, 1000).
-define(iofmt(S, A), lists:flatten(io_lib:format(S, A))).

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    Doc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"doc1">>},
            {<<"value">>, 666}
        ]}
    ),

    Doc1 = couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"_design/doc1">>},
            {<<"shows">>,
                {[
                    {<<"test">>, <<
                        "function(doc, req) {\n"
                        "            return { json: {\n"
                        "                    requested_path: '/' + req.requested_path.join('/'),\n"
                        "                    path: '/' + req.path.join('/')}};}"
                    >>}
                ]}},
            {<<"rewrites">>, [
                {[
                    {<<"from">>, <<"/">>},
                    {<<"to">>, <<"_show/test">>}
                ]}
            ]}
        ]}
    ),
    {ok, _} = couch_db:update_docs(Db, [Doc, Doc1]),
    couch_db:close(Db),

    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
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
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun should_return_database_info/1,
                    fun should_return_revs_info/1,
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

should_return_database_info({Url, DbName}) ->
    ?_test(begin
        ok = config:set("vhosts", "example.com", "/" ++ DbName, false),
        case test_request:get(Url, [], [{host_header, "example.com"}]) of
            {ok, _, _, Body} ->
                {JsonBody} = jiffy:decode(Body),
                ?assert(proplists:is_defined(<<"db_name">>, JsonBody));
            Else ->
                erlang:error(
                    {assertion_failed, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {reason, ?iofmt("Request failed: ~p", [Else])}
                    ]}
                )
        end
    end).

should_return_revs_info({Url, DbName}) ->
    ?_test(begin
        ok = config:set("vhosts", "example.com", "/" ++ DbName, false),
        case
            test_request:get(
                Url ++ "/doc1?revs_info=true",
                [],
                [{host_header, "example.com"}]
            )
        of
            {ok, _, _, Body} ->
                {JsonBody} = jiffy:decode(Body),
                ?assert(proplists:is_defined(<<"_revs_info">>, JsonBody));
            Else ->
                erlang:error(
                    {assertion_failed, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {reason, ?iofmt("Request failed: ~p", [Else])}
                    ]}
                )
        end
    end).

should_return_virtual_request_path_field_in_request({Url, DbName}) ->
    ?_test(begin
        ok = config:set(
            "vhosts",
            "example1.com",
            "/" ++ DbName ++ "/_design/doc1/_rewrite/",
            false
        ),
        case test_request:get(Url, [], [{host_header, "example1.com"}]) of
            {ok, _, _, Body} ->
                {Json} = jiffy:decode(Body),
                ?assertEqual(
                    <<"/">>,
                    proplists:get_value(<<"requested_path">>, Json)
                );
            Else ->
                erlang:error(
                    {assertion_failed, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {reason, ?iofmt("Request failed: ~p", [Else])}
                    ]}
                )
        end
    end).

should_return_real_request_path_field_in_request({Url, DbName}) ->
    ?_test(begin
        ok = config:set(
            "vhosts",
            "example1.com",
            "/" ++ DbName ++ "/_design/doc1/_rewrite/",
            false
        ),
        case test_request:get(Url, [], [{host_header, "example1.com"}]) of
            {ok, _, _, Body} ->
                {Json} = jiffy:decode(Body),
                Path = ?l2b("/" ++ DbName ++ "/_design/doc1/_show/test"),
                ?assertEqual(Path, proplists:get_value(<<"path">>, Json));
            Else ->
                erlang:error(
                    {assertion_failed, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {reason, ?iofmt("Request failed: ~p", [Else])}
                    ]}
                )
        end
    end).

should_match_wildcard_vhost({Url, DbName}) ->
    ?_test(begin
        ok = config:set(
            "vhosts",
            "*.example.com",
            "/" ++ DbName ++ "/_design/doc1/_rewrite",
            false
        ),
        case test_request:get(Url, [], [{host_header, "test.example.com"}]) of
            {ok, _, _, Body} ->
                {Json} = jiffy:decode(Body),
                Path = ?l2b("/" ++ DbName ++ "/_design/doc1/_show/test"),
                ?assertEqual(Path, proplists:get_value(<<"path">>, Json));
            Else ->
                erlang:error(
                    {assertion_failed, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {reason, ?iofmt("Request failed: ~p", [Else])}
                    ]}
                )
        end
    end).

should_return_db_info_for_wildcard_vhost_for_custom_db({Url, DbName}) ->
    ?_test(begin
        ok = config:set(
            "vhosts",
            ":dbname.example1.com",
            "/:dbname",
            false
        ),
        Host = DbName ++ ".example1.com",
        case test_request:get(Url, [], [{host_header, Host}]) of
            {ok, _, _, Body} ->
                {JsonBody} = jiffy:decode(Body),
                ?assert(proplists:is_defined(<<"db_name">>, JsonBody));
            Else ->
                erlang:error(
                    {assertion_failed, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {reason, ?iofmt("Request failed: ~p", [Else])}
                    ]}
                )
        end
    end).

should_replace_rewrite_variables_for_db_and_doc({Url, DbName}) ->
    ?_test(begin
        ok = config:set(
            "vhosts",
            ":appname.:dbname.example1.com",
            "/:dbname/_design/:appname/_rewrite/",
            false
        ),
        Host = "doc1." ++ DbName ++ ".example1.com",
        case test_request:get(Url, [], [{host_header, Host}]) of
            {ok, _, _, Body} ->
                {Json} = jiffy:decode(Body),
                Path = ?l2b("/" ++ DbName ++ "/_design/doc1/_show/test"),
                ?assertEqual(Path, proplists:get_value(<<"path">>, Json));
            Else ->
                erlang:error(
                    {assertion_failed, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {reason, ?iofmt("Request failed: ~p", [Else])}
                    ]}
                )
        end
    end).

should_return_db_info_for_vhost_with_resource({Url, DbName}) ->
    ?_test(begin
        ok = config:set(
            "vhosts",
            "example.com/test",
            "/" ++ DbName,
            false
        ),
        ReqUrl = Url ++ "/test",
        case test_request:get(ReqUrl, [], [{host_header, "example.com"}]) of
            {ok, _, _, Body} ->
                {JsonBody} = jiffy:decode(Body),
                ?assert(proplists:is_defined(<<"db_name">>, JsonBody));
            Else ->
                erlang:error(
                    {assertion_failed, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {reason, ?iofmt("Request failed: ~p", [Else])}
                    ]}
                )
        end
    end).

should_return_revs_info_for_vhost_with_resource({Url, DbName}) ->
    ?_test(begin
        ok = config:set(
            "vhosts",
            "example.com/test",
            "/" ++ DbName,
            false
        ),
        ReqUrl = Url ++ "/test/doc1?revs_info=true",
        case test_request:get(ReqUrl, [], [{host_header, "example.com"}]) of
            {ok, _, _, Body} ->
                {JsonBody} = jiffy:decode(Body),
                ?assert(proplists:is_defined(<<"_revs_info">>, JsonBody));
            Else ->
                erlang:error(
                    {assertion_failed, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {reason, ?iofmt("Request failed: ~p", [Else])}
                    ]}
                )
        end
    end).

should_return_db_info_for_vhost_with_wildcard_resource({Url, DbName}) ->
    ?_test(begin
        ok = config:set("vhosts", "*.example2.com/test", "/*", false),
        ReqUrl = Url ++ "/test",
        Host = DbName ++ ".example2.com",
        case test_request:get(ReqUrl, [], [{host_header, Host}]) of
            {ok, _, _, Body} ->
                {JsonBody} = jiffy:decode(Body),
                ?assert(proplists:is_defined(<<"db_name">>, JsonBody));
            Else ->
                erlang:error(
                    {assertion_failed, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {reason, ?iofmt("Request failed: ~p", [Else])}
                    ]}
                )
        end
    end).

should_return_path_for_vhost_with_wildcard_host({Url, DbName}) ->
    ?_test(begin
        ok = config:set(
            "vhosts",
            "*/test1",
            "/" ++ DbName ++ "/_design/doc1/_show/test",
            false
        ),
        case test_request:get(Url ++ "/test1") of
            {ok, _, _, Body} ->
                {Json} = jiffy:decode(Body),
                Path = ?l2b("/" ++ DbName ++ "/_design/doc1/_show/test"),
                ?assertEqual(Path, proplists:get_value(<<"path">>, Json));
            Else ->
                erlang:error(
                    {assertion_failed, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {reason, ?iofmt("Request failed: ~p", [Else])}
                    ]}
                )
        end
    end).
