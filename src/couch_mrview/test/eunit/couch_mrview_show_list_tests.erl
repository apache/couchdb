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

-module(couch_mrview_show_list_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

setup() ->
    ok = config:set("query_server_config", "query_limit", "infinity", false),
    {ok, Db} = couch_mrview_test_util:init_db(?tempdb(), list, 5),
    Db.

teardown(Db) ->
    couch_db:close(Db),
    couch_server:delete(couch_db:name(Db), [?ADMIN_CTX]),
    config:delete("query_server_config", "query_limit", false).

list_test_() ->
    {
        "_list tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(http_query),
                    ?TDEF_FE(http_query_with_range),
                    ?TDEF_FE(http_query_with_non_string_key),
                    ?TDEF_FE(http_query_with_range_rev),
                    ?TDEF_FE(http_query_with_limit_and_skip),
                    ?TDEF_FE(http_query_with_limit_and_skip_and_query_limit),
                    ?TDEF_FE(http_query_with_query_limit_and_over_limit)
                ]
            }
        }
    }.

http_query(Db) ->
    ?assertEqual(<<"H12345_design/listsT">>, http_query(Db, "")).

http_query_with_range(Db) ->
    ?assertEqual(<<"H345T">>, http_query(Db, "?start_key=\"3\"&end_key=\"5\"")).

http_query_with_non_string_key(Db) ->
    {Code1, Res1} = http_req(Db, "?start_key=1&end_key=1"),
    ?assertEqual(200, Code1),
    ?assertEqual(<<"HT">>, Res1).

http_query_with_range_rev(Db) ->
    Params = "?descending=true&start_key=\"5\"&end_key=\"3\"&include_end=true",
    ?assertEqual(<<"H543T">>, http_query(Db, Params)).

http_query_with_limit_and_skip(Db) ->
    Params = "?start_key=\"2\"&limit=3&skip=3",
    ?assertEqual(<<"H5_design/listsT">>, http_query(Db, Params)).

http_query_with_limit_and_skip_and_query_limit(Db) ->
    config:set("query_server_config", "query_limit", "2", false),
    Params = "?start_key=\"1\"&skip=2",
    ?assertEqual(<<"H34T">>, http_query(Db, Params)).

http_query_with_query_limit_and_over_limit(Db) ->
    config:set("query_server_config", "query_limit", "2", false),
    {Code, _} = http_req(Db, "?limit=3"),
    ?assertEqual(400, Code).

db_url(Db) ->
    DbName = couch_db:name(Db),
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    "http://" ++ Addr ++ ":" ++ Port ++ "/" ++ ?b2l(DbName).

http_req(DbName, Params) ->
    Url = db_url(DbName) ++ "/_design/lists/_list/list/_all_docs",
    {ok, Code, _Headers, Body} = test_request:get(Url ++ Params),
    {Code, Body}.

http_query(DbName, Params) ->
    Url = db_url(DbName) ++ "/_design/lists/_list/list/_all_docs",
    {ok, Code, _Headers, Body} = test_request:get(Url ++ Params),
    ?assertEqual(200, Code),
    Body.
