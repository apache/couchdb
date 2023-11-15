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

-module(couchdb_access_tests).

-include_lib("couch/include/couch_eunit.hrl").

-define(CONTENT_JSON, {"Content-Type", "application/json"}).
-define(ADMIN_REQ_HEADERS, [?CONTENT_JSON, {basic_auth, {"a", "a"}}]).
-define(USERX_REQ_HEADERS, [?CONTENT_JSON, {basic_auth, {"x", "x"}}]).
-define(USERY_REQ_HEADERS, [?CONTENT_JSON, {basic_auth, {"y", "y"}}]).
-define(SECURITY_OBJECT,
    {[
        {<<"members">>, {[{<<"roles">>, [<<"_admin">>, <<"_users">>]}]}},
        {<<"admins">>, {[{<<"roles">>, [<<"_admin">>]}]}}
    ]}
).

url() ->
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    lists:concat(["http://", Addr, ":", port()]).

before_each(_) ->
    R = test_request:put(url() ++ "/db?q=1&n=1&access=true", ?ADMIN_REQ_HEADERS, ""),
    %?debugFmt("~nRequest: ~p~n", [R]),
    {ok, 201, _, _} = R,
    {ok, _, _, _} = test_request:put(
        url() ++ "/db/_security", ?ADMIN_REQ_HEADERS, jiffy:encode(?SECURITY_OBJECT)
    ),
    url().

after_each(_, Url) ->
    {ok, 200, _, _} = test_request:delete(Url ++ "/db", ?ADMIN_REQ_HEADERS),
    {_, _, _, _} = test_request:delete(Url ++ "/db2", ?ADMIN_REQ_HEADERS),
    {_, _, _, _} = test_request:delete(Url ++ "/db3", ?ADMIN_REQ_HEADERS),
    ok.

before_all() ->
    Couch = test_util:start_couch([chttpd, couch_replicator]),
    Hashed = couch_passwords:hash_admin_password("a"),
    ok = config:set("admins", "a", binary_to_list(Hashed), false),
    ok = config:set("couchdb", "uuid", "21ac467c1bc05e9d9e9d2d850bb1108f", false),
    ok = config:set("log", "level", "debug", false),
    ok = config:set("per_doc_access", "enable", "true", false),

    % cleanup and setup
    {ok, _, _, _} = test_request:delete(url() ++ "/db", ?ADMIN_REQ_HEADERS),
    % {ok, _, _, _} = test_request:put(url() ++ "/db?q=1&n=1&access=true", ?ADMIN_REQ_HEADERS, ""),

    % create users
    UserDbUrl = url() ++ "/_users?q=1&n=1",
    {ok, _, _, _} = test_request:delete(UserDbUrl, ?ADMIN_REQ_HEADERS, ""),
    {ok, 201, _, _} = test_request:put(UserDbUrl, ?ADMIN_REQ_HEADERS, ""),

    UserXDocUrl = url() ++ "/_users/org.couchdb.user:x",
    UserXDocBody = "{ \"name\":\"x\", \"roles\": [], \"password\":\"x\", \"type\": \"user\" }",
    {ok, 201, _, _} = test_request:put(UserXDocUrl, ?ADMIN_REQ_HEADERS, UserXDocBody),

    UserYDocUrl = url() ++ "/_users/org.couchdb.user:y",
    UserYDocBody = "{ \"name\":\"y\", \"roles\": [], \"password\":\"y\", \"type\": \"user\" }",
    {ok, 201, _, _} = test_request:put(UserYDocUrl, ?ADMIN_REQ_HEADERS, UserYDocBody),
    Couch.

after_all(_) ->
    UserDbUrl = url() ++ "/_users",
    {ok, _, _, _} = test_request:delete(UserDbUrl, ?ADMIN_REQ_HEADERS, ""),
    ok = test_util:stop_couch(done).

access_test_() ->
    Tests = [
        % Server config
        fun should_not_let_create_access_db_if_disabled/2,

        % Doc creation
        fun should_not_let_anonymous_user_create_doc/2,
        fun should_let_admin_create_doc_with_access/2,
        fun should_let_admin_create_doc_without_access/2,
        fun should_let_user_create_doc_for_themselves/2,
        fun should_not_let_user_create_doc_for_someone_else/2,
        fun should_let_user_create_access_ddoc/2,
        fun access_ddoc_should_have_no_effects/2,

        % Doc updates
        fun users_with_access_can_update_doc/2,
        fun users_without_access_can_not_update_doc/2,
        fun users_with_access_can_not_change_access/2,
        fun users_with_access_can_not_remove_access/2,

        % Doc reads
        fun should_let_admin_read_doc_with_access/2,
        fun user_with_access_can_read_doc/2,
        fun user_without_access_can_not_read_doc/2,
        fun user_can_not_read_doc_without_access/2,

        % Doc deletes
        fun should_let_admin_delete_doc_with_access/2,
        fun should_let_user_delete_doc_for_themselves/2,
        fun should_not_let_user_delete_doc_for_someone_else/2,

        % _all_docs with include_docs
        fun should_let_admin_fetch_all_docs/2,
        fun should_let_user_fetch_their_own_all_docs/2,

        % _changes
        fun should_let_admin_fetch_changes/2,
        fun should_let_user_fetch_their_own_changes/2,

        % views
        fun should_not_allow_admin_access_ddoc_view_request/2,
        fun should_not_allow_user_access_ddoc_view_request/2,
        fun should_allow_admin_users_access_ddoc_view_request/2,
        fun should_allow_user_users_access_ddoc_view_request/2,

        % replication
        fun should_allow_admin_to_replicate_from_access_to_access/2,
        fun should_allow_admin_to_replicate_from_no_access_to_access/2,
        fun should_allow_admin_to_replicate_from_access_to_no_access/2,
        fun should_allow_admin_to_replicate_from_no_access_to_no_access/2,

        fun should_allow_user_to_replicate_from_access_to_access/2,
        fun should_allow_user_to_replicate_from_access_to_no_access/2,
        fun should_allow_user_to_replicate_from_no_access_to_access/2,
        fun should_allow_user_to_replicate_from_no_access_to_no_access/2,

        % _revs_diff for docs you don’t have access to
        fun should_not_allow_user_to_revs_diff_other_docs/2

        % TODO: create test db with role and not _users in _security.members
        % and make sure a user in that group can access while a user not
        % in that group cant
        % % potential future feature
        % % fun should_let_user_fetch_their_own_all_docs_plus_users_ddocs/2%,
    ],
    {
        "Access tests",
        {
            setup,
            fun before_all/0,
            fun after_all/1,
            [
                make_test_cases(basic, Tests)
            ]
        }
    }.

make_test_cases(Mod, Funs) ->
    {
        lists:flatten(io_lib:format("~s", [Mod])),
        {foreachx, fun before_each/1, fun after_each/2, [{Mod, Fun} || Fun <- Funs]}
    }.

% Doc creation
% http://127.0.0.1:64903/db/a?revs=true&open_revs=%5B%221-23202479633c2b380f79507a776743d5%22%5D&latest=true

% should_do_the_thing(_PortType, Url) ->
%   ?_test(begin
%       {ok, _, _, _} = test_request:put(Url ++ "/db/a",
%           ?ADMIN_REQ_HEADERS, "{\"a\":1,\"_access\":[\"x\"]}"),
%       {ok, Code, _, _} = test_request:get(Url ++ "/db/a?revs=true&open_revs=%5B%221-23202479633c2b380f79507a776743d5%22%5D&latest=true",
%           ?USERX_REQ_HEADERS),
%       ?assertEqual(200, Code)
%   end).
%

should_not_let_create_access_db_if_disabled(_PortType, Url) ->
    ok = config:set("per_doc_access", "enable", "false", false),
    {ok, Code, _, _} = test_request:put(url() ++ "/db?q=1&n=1&access=true", ?ADMIN_REQ_HEADERS, ""),
    ok = config:set("per_doc_access", "enable", "true", false),
    ?_assertEqual(400, Code).

should_not_let_anonymous_user_create_doc(_PortType, Url) ->
    % TODO: debugging leftover
    % BulkDocsBody = {[
    %   {<<"docs">>, [
    %       {[{<<"_id">>, <<"a">>}]},
    %       {[{<<"_id">>, <<"a">>}]},
    %       {[{<<"_id">>, <<"b">>}]},
    %       {[{<<"_id">>, <<"c">>}]}
    %   ]}
    % ]},
    % Resp = test_request:post(Url ++ "/db/_bulk_docs", ?ADMIN_REQ_HEADERS, jiffy:encode(BulkDocsBody)),
    % ?debugFmt("~nResp: ~p~n", [Resp]),
    {ok, Code, _, _} = test_request:put(Url ++ "/db/a", "{\"a\":1,\"_access\":[\"x\"]}"),
    ?_assertEqual(401, Code).

should_let_admin_create_doc_with_access(_PortType, Url) ->
    {ok, Code, _, _} = test_request:put(
        Url ++ "/db/a",
        ?ADMIN_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    ?_assertEqual(201, Code).

should_let_admin_create_doc_without_access(_PortType, Url) ->
    {ok, Code, _, _} = test_request:put(
        Url ++ "/db/a",
        ?ADMIN_REQ_HEADERS,
        "{\"a\":1}"
    ),
    ?_assertEqual(201, Code).

should_let_user_create_doc_for_themselves(_PortType, Url) ->
    {ok, Code, _, _} = test_request:put(
        Url ++ "/db/b",
        ?USERX_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    ?_assertEqual(201, Code).

should_not_let_user_create_doc_for_someone_else(_PortType, Url) ->
    {ok, Code, _, _} = test_request:put(
        Url ++ "/db/c",
        ?USERY_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    ?_assertEqual(403, Code).

should_let_user_create_access_ddoc(_PortType, Url) ->
    {ok, Code, _, _} = test_request:put(
        Url ++ "/db/_design/dx",
        ?USERX_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    ?_assertEqual(201, Code).

access_ddoc_should_have_no_effects(_PortType, Url) ->
    ?_test(begin
        Ddoc =
            "{ \"_access\":[\"x\"], \"validate_doc_update\": \"function(newDoc, oldDoc, userCtx) { throw({unauthorized: 'throw error'})}\",   \"views\": {     \"foo\": {       \"map\": \"function(doc) { emit(doc._id) }\"     }   },   \"shows\": {     \"boo\": \"function() {}\"   },   \"lists\": {    \"hoo\": \"function() {}\"   },   \"update\": {     \"goo\": \"function() {}\"   },   \"filters\": {     \"loo\": \"function() {}\"   }   }",
        {ok, Code, _, _} = test_request:put(
            Url ++ "/db/_design/dx",
            ?USERX_REQ_HEADERS,
            Ddoc
        ),
        ?assertEqual(201, Code),
        {ok, Code1, _, B} = test_request:put(
            Url ++ "/db/b",
            ?USERX_REQ_HEADERS,
            "{\"a\":1,\"_access\":[\"x\"]}"
        ),
        ?assertEqual(201, Code1),
        {ok, Code2, _, _} = test_request:get(
            Url ++ "/db/_design/dx/_view/foo",
            ?USERX_REQ_HEADERS
        ),
        ?assertEqual(404, Code2),
        {ok, Code3, _, _} = test_request:get(
            Url ++ "/db/_design/dx/_show/boo/b",
            ?USERX_REQ_HEADERS
        ),
        ?assertEqual(404, Code3),
        {ok, Code4, _, _} = test_request:get(
            Url ++ "/db/_design/dx/_list/hoo/foo",
            ?USERX_REQ_HEADERS
        ),
        ?assertEqual(404, Code4),
        {ok, Code5, _, _} = test_request:post(
            Url ++ "/db/_design/dx/_update/goo",
            ?USERX_REQ_HEADERS,
            ""
        ),
        ?assertEqual(404, Code5),
        {ok, Code6, _, _} = test_request:get(
            Url ++ "/db/_changes?filter=dx/loo",
            ?USERX_REQ_HEADERS
        ),
        ?assertEqual(404, Code6),
        {ok, Code7, _, _} = test_request:get(
            Url ++ "/db/_changes?filter=_view&view=dx/foo",
            ?USERX_REQ_HEADERS
        ),
        ?assertEqual(404, Code7)
    end).

% Doc updates

users_with_access_can_update_doc(_PortType, Url) ->
    {ok, _, _, Body} = test_request:put(
        Url ++ "/db/b",
        ?USERX_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    {Json} = jiffy:decode(Body),
    Rev = couch_util:get_value(<<"rev">>, Json),
    {ok, Code, _, _} = test_request:put(
        Url ++ "/db/b",
        ?USERX_REQ_HEADERS,
        "{\"a\":2,\"_access\":[\"x\"],\"_rev\":\"" ++ binary_to_list(Rev) ++ "\"}"
    ),
    ?_assertEqual(201, Code).

users_without_access_can_not_update_doc(_PortType, Url) ->
    {ok, _, _, Body} = test_request:put(
        Url ++ "/db/b",
        ?USERX_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    {Json} = jiffy:decode(Body),
    Rev = couch_util:get_value(<<"rev">>, Json),
    {ok, Code, _, _} = test_request:put(
        Url ++ "/db/b",
        ?USERY_REQ_HEADERS,
        "{\"a\":2,\"_access\":[\"y\"],\"_rev\":\"" ++ binary_to_list(Rev) ++ "\"}"
    ),
    ?_assertEqual(403, Code).

users_with_access_can_not_change_access(_PortType, Url) ->
    {ok, _, _, Body} = test_request:put(
        Url ++ "/db/b",
        ?USERX_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    {Json} = jiffy:decode(Body),
    Rev = couch_util:get_value(<<"rev">>, Json),
    {ok, Code, _, _} = test_request:put(
        Url ++ "/db/b",
        ?USERX_REQ_HEADERS,
        "{\"a\":2,\"_access\":[\"y\"],\"_rev\":\"" ++ binary_to_list(Rev) ++ "\"}"
    ),
    ?_assertEqual(403, Code).

users_with_access_can_not_remove_access(_PortType, Url) ->
    {ok, _, _, Body} = test_request:put(
        Url ++ "/db/b",
        ?USERX_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    {Json} = jiffy:decode(Body),
    Rev = couch_util:get_value(<<"rev">>, Json),
    {ok, Code, _, _} = test_request:put(
        Url ++ "/db/b",
        ?USERX_REQ_HEADERS,
        "{\"a\":2,\"_rev\":\"" ++ binary_to_list(Rev) ++ "\"}"
    ),
    ?_assertEqual(403, Code).

% Doc reads

should_let_admin_read_doc_with_access(_PortType, Url) ->
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/a",
        ?USERX_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    {ok, Code, _, _} = test_request:get(
        Url ++ "/db/a",
        ?ADMIN_REQ_HEADERS
    ),
    ?_assertEqual(200, Code).

user_with_access_can_read_doc(_PortType, Url) ->
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/a",
        ?ADMIN_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    {ok, Code, _, _} = test_request:get(
        Url ++ "/db/a",
        ?USERX_REQ_HEADERS
    ),
    ?_assertEqual(200, Code).

user_without_access_can_not_read_doc(_PortType, Url) ->
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/a",
        ?ADMIN_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    {ok, Code, _, _} = test_request:get(
        Url ++ "/db/a",
        ?USERY_REQ_HEADERS
    ),
    ?_assertEqual(403, Code).

user_can_not_read_doc_without_access(_PortType, Url) ->
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/a",
        ?ADMIN_REQ_HEADERS,
        "{\"a\":1}"
    ),
    {ok, Code, _, _} = test_request:get(
        Url ++ "/db/a",
        ?USERX_REQ_HEADERS
    ),
    ?_assertEqual(403, Code).

% Doc deletes

should_let_admin_delete_doc_with_access(_PortType, Url) ->
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/a",
        ?USERX_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    {ok, Code, _, _} = test_request:delete(
        Url ++ "/db/a?rev=1-23202479633c2b380f79507a776743d5",
        ?ADMIN_REQ_HEADERS
    ),
    ?_assertEqual(200, Code).

should_let_user_delete_doc_for_themselves(_PortType, Url) ->
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/a",
        ?USERX_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    {ok, _, _, _} = test_request:get(
        Url ++ "/db/a",
        ?USERX_REQ_HEADERS
    ),
    {ok, Code, _, _} = test_request:delete(
        Url ++ "/db/a?rev=1-23202479633c2b380f79507a776743d5",
        ?USERX_REQ_HEADERS
    ),
    ?_assertEqual(200, Code).

should_not_let_user_delete_doc_for_someone_else(_PortType, Url) ->
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/a",
        ?USERX_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    {ok, Code, _, _} = test_request:delete(
        Url ++ "/db/a?rev=1-23202479633c2b380f79507a776743d5",
        ?USERY_REQ_HEADERS
    ),
    ?_assertEqual(403, Code).

% _all_docs with include_docs

should_let_admin_fetch_all_docs(_PortType, Url) ->
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/a",
        ?ADMIN_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/b",
        ?ADMIN_REQ_HEADERS,
        "{\"b\":2,\"_access\":[\"x\"]}"
    ),
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/c",
        ?ADMIN_REQ_HEADERS,
        "{\"c\":3,\"_access\":[\"y\"]}"
    ),
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/d",
        ?ADMIN_REQ_HEADERS,
        "{\"d\":4,\"_access\":[\"y\"]}"
    ),
    {ok, 200, _, Body} = test_request:get(
        Url ++ "/db/_all_docs?include_docs=true",
        ?ADMIN_REQ_HEADERS
    ),
    {Json} = jiffy:decode(Body),
    ?_assertEqual(4, proplists:get_value(<<"total_rows">>, Json)).

should_let_user_fetch_their_own_all_docs(_PortType, Url) ->
    ?_test(begin
        {ok, 201, _, _} = test_request:put(
            Url ++ "/db/a",
            ?ADMIN_REQ_HEADERS,
            "{\"a\":1,\"_access\":[\"x\"]}"
        ),
        {ok, 201, _, _} = test_request:put(
            Url ++ "/db/b",
            ?USERX_REQ_HEADERS,
            "{\"b\":2,\"_access\":[\"x\"]}"
        ),
        {ok, 201, _, _} = test_request:put(
            Url ++ "/db/c",
            ?ADMIN_REQ_HEADERS,
            "{\"c\":3,\"_access\":[\"y\"]}"
        ),
        {ok, 201, _, _} = test_request:put(
            Url ++ "/db/d",
            ?USERY_REQ_HEADERS,
            "{\"d\":4,\"_access\":[\"y\"]}"
        ),
        {ok, 200, _, Body} = test_request:get(
            Url ++ "/db/_all_docs?include_docs=true",
            ?USERX_REQ_HEADERS
        ),
        {Json} = jiffy:decode(Body),
        Rows = proplists:get_value(<<"rows">>, Json),
        ?assertEqual(
            [
                {[
                    {<<"id">>, <<"a">>},
                    {<<"key">>, <<"a">>},
                    {<<"value">>, <<"1-23202479633c2b380f79507a776743d5">>},
                    {<<"doc">>,
                        {[
                            {<<"_id">>, <<"a">>},
                            {<<"_rev">>, <<"1-23202479633c2b380f79507a776743d5">>},
                            {<<"a">>, 1},
                            {<<"_access">>, [<<"x">>]}
                        ]}}
                ]},
                {[
                    {<<"id">>, <<"b">>},
                    {<<"key">>, <<"b">>},
                    {<<"value">>, <<"1-d33fb05384fa65a8081da2046595de0f">>},
                    {<<"doc">>,
                        {[
                            {<<"_id">>, <<"b">>},
                            {<<"_rev">>, <<"1-d33fb05384fa65a8081da2046595de0f">>},
                            {<<"b">>, 2},
                            {<<"_access">>, [<<"x">>]}
                        ]}}
                ]}
            ],
            Rows
        ),
        ?assertEqual(2, length(Rows)),
        ?assertEqual(4, proplists:get_value(<<"total_rows">>, Json)),

        {ok, 200, _, Body1} = test_request:get(
            Url ++ "/db/_all_docs?include_docs=true",
            ?USERY_REQ_HEADERS
        ),
        {Json1} = jiffy:decode(Body1),
        ?assertEqual(
            [
                {<<"total_rows">>, 4},
                {<<"offset">>, 2},
                {<<"rows">>, [
                    {[
                        {<<"id">>, <<"c">>},
                        {<<"key">>, <<"c">>},
                        {<<"value">>, <<"1-92aef5b0e4a3f4db0aba1320869bc95d">>},
                        {<<"doc">>,
                            {[
                                {<<"_id">>, <<"c">>},
                                {<<"_rev">>, <<"1-92aef5b0e4a3f4db0aba1320869bc95d">>},
                                {<<"c">>, 3},
                                {<<"_access">>, [<<"y">>]}
                            ]}}
                    ]},
                    {[
                        {<<"id">>, <<"d">>},
                        {<<"key">>, <<"d">>},
                        {<<"value">>, <<"1-ae984f6550038b1ed1565ac4b6cd8c5d">>},
                        {<<"doc">>,
                            {[
                                {<<"_id">>, <<"d">>},
                                {<<"_rev">>, <<"1-ae984f6550038b1ed1565ac4b6cd8c5d">>},
                                {<<"d">>, 4},
                                {<<"_access">>, [<<"y">>]}
                            ]}}
                    ]}
                ]}
            ],
            Json1
        )
    end).

% _changes

should_let_admin_fetch_changes(_PortType, Url) ->
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/a",
        ?ADMIN_REQ_HEADERS,
        "{\"a\":1,\"_access\":[\"x\"]}"
    ),
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/b",
        ?ADMIN_REQ_HEADERS,
        "{\"b\":2,\"_access\":[\"x\"]}"
    ),
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/c",
        ?ADMIN_REQ_HEADERS,
        "{\"c\":3,\"_access\":[\"y\"]}"
    ),
    {ok, 201, _, _} = test_request:put(
        Url ++ "/db/d",
        ?ADMIN_REQ_HEADERS,
        "{\"d\":4,\"_access\":[\"y\"]}"
    ),
    {ok, 200, _, Body} = test_request:get(
        Url ++ "/db/_changes",
        ?ADMIN_REQ_HEADERS
    ),
    {Json} = jiffy:decode(Body),
    AmountOfDocs = length(proplists:get_value(<<"results">>, Json)),
    ?_assertEqual(4, AmountOfDocs).

should_let_user_fetch_their_own_changes(_PortType, Url) ->
    ?_test(begin
        {ok, 201, _, _} = test_request:put(
            Url ++ "/db/a",
            ?ADMIN_REQ_HEADERS,
            "{\"a\":1,\"_access\":[\"x\"]}"
        ),
        {ok, 201, _, _} = test_request:put(
            Url ++ "/db/b",
            ?ADMIN_REQ_HEADERS,
            "{\"b\":2,\"_access\":[\"x\"]}"
        ),
        {ok, 201, _, _} = test_request:put(
            Url ++ "/db/c",
            ?ADMIN_REQ_HEADERS,
            "{\"c\":3,\"_access\":[\"y\"]}"
        ),
        {ok, 201, _, _} = test_request:put(
            Url ++ "/db/d",
            ?ADMIN_REQ_HEADERS,
            "{\"d\":4,\"_access\":[\"y\"]}"
        ),
        {ok, 200, _, Body} = test_request:get(
            Url ++ "/db/_changes",
            ?USERX_REQ_HEADERS
        ),
        {Json} = jiffy:decode(Body),
        ?assertMatch(
            [
                {<<"results">>, [
                    {[
                        {<<"seq">>, <<"2-", _/binary>>},
                        {<<"id">>, <<"a">>},
                        {<<"changes">>, [{[{<<"rev">>, <<"1-23202479633c2b380f79507a776743d5">>}]}]}
                    ]},
                    {[
                        {<<"seq">>, <<"3-", _/binary>>},
                        {<<"id">>, <<"b">>},
                        {<<"changes">>, [{[{<<"rev">>, <<"1-d33fb05384fa65a8081da2046595de0f">>}]}]}
                    ]}
                ]},
                {<<"last_seq">>, <<"3-", _/binary>>},
                {<<"pending">>, 2}
            ],
            Json
        ),
        AmountOfDocs = length(proplists:get_value(<<"results">>, Json)),
        ?assertEqual(2, AmountOfDocs)
    end).

% views

should_not_allow_admin_access_ddoc_view_request(_PortType, Url) ->
    DDoc = "{\"a\":1,\"_access\":[\"x\"],\"views\":{\"foo\":{\"map\":\"function() {}\"}}}",
    {ok, Code, _, _} = test_request:put(
        Url ++ "/db/_design/a",
        ?ADMIN_REQ_HEADERS,
        DDoc
    ),
    ?assertEqual(201, Code),
    {ok, Code1, _, _} = test_request:get(
        Url ++ "/db/_design/a/_view/foo",
        ?ADMIN_REQ_HEADERS
    ),
    ?_assertEqual(404, Code1).

should_not_allow_user_access_ddoc_view_request(_PortType, Url) ->
    DDoc = "{\"a\":1,\"_access\":[\"x\"],\"views\":{\"foo\":{\"map\":\"function() {}\"}}}",
    {ok, Code, _, _} = test_request:put(
        Url ++ "/db/_design/a",
        ?ADMIN_REQ_HEADERS,
        DDoc
    ),
    ?assertEqual(201, Code),
    {ok, Code1, _, _} = test_request:get(
        Url ++ "/db/_design/a/_view/foo",
        ?USERX_REQ_HEADERS
    ),
    ?_assertEqual(404, Code1).

should_allow_admin_users_access_ddoc_view_request(_PortType, Url) ->
    DDoc = "{\"a\":1,\"_access\":[\"_users\"],\"views\":{\"foo\":{\"map\":\"function() {}\"}}}",
    {ok, Code, _, _} = test_request:put(
        Url ++ "/db/_design/a",
        ?ADMIN_REQ_HEADERS,
        DDoc
    ),
    ?assertEqual(201, Code),
    {ok, Code1, _, _} = test_request:get(
        Url ++ "/db/_design/a/_view/foo",
        ?ADMIN_REQ_HEADERS
    ),
    ?_assertEqual(200, Code1).

should_allow_user_users_access_ddoc_view_request(_PortType, Url) ->
    DDoc = "{\"a\":1,\"_access\":[\"_users\"],\"views\":{\"foo\":{\"map\":\"function() {}\"}}}",
    {ok, Code, _, _} = test_request:put(
        Url ++ "/db/_design/a",
        ?ADMIN_REQ_HEADERS,
        DDoc
    ),
    ?assertEqual(201, Code),
    {ok, Code1, _, _} = test_request:get(
        Url ++ "/db/_design/a/_view/foo",
        ?USERX_REQ_HEADERS
    ),
    ?_assertEqual(200, Code1).

% replication

should_allow_admin_to_replicate_from_access_to_access(_PortType, Url) ->
    ?_test(begin
        % create target db
        {ok, 201, _, _} = test_request:put(
            url() ++ "/db2?q=1&n=1&access=true",
            ?ADMIN_REQ_HEADERS,
            ""
        ),
        % set target db security
        {ok, _, _, _} = test_request:put(
            url() ++ "/db2/_security",
            ?ADMIN_REQ_HEADERS,
            jiffy:encode(?SECURITY_OBJECT)
        ),

        % create source docs
        {ok, _, _, _} = test_request:put(
            Url ++ "/db/a",
            ?ADMIN_REQ_HEADERS,
            "{\"a\":1,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db/b",
            ?ADMIN_REQ_HEADERS,
            "{\"b\":2,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db/c",
            ?ADMIN_REQ_HEADERS,
            "{\"c\":3,\"_access\":[\"x\"]}"
        ),

        % replicate
        AdminUrl = string:replace(Url, "http://", "http://a:a@"),
        EJRequestBody =
            {[
                {<<"source">>, list_to_binary(AdminUrl ++ "/db")},
                {<<"target">>, list_to_binary(AdminUrl ++ "/db2")}
            ]},
        {ok, ResponseCode, _, ResponseBody} = test_request:post(
            Url ++ "/_replicate",
            ?ADMIN_REQ_HEADERS,
            jiffy:encode(EJRequestBody)
        ),

        % assert replication status
        {EJResponseBody} = jiffy:decode(ResponseBody),
        ?assertEqual(ResponseCode, 200),
        ?assertEqual(true, couch_util:get_value(<<"ok">>, EJResponseBody)),
        [{History}] = couch_util:get_value(<<"history">>, EJResponseBody),

        MissingChecked = couch_util:get_value(<<"missing_checked">>, History),
        MissingFound = couch_util:get_value(<<"missing_found">>, History),
        DocsReard = couch_util:get_value(<<"docs_read">>, History),
        DocsWritten = couch_util:get_value(<<"docs_written">>, History),
        DocWriteFailures = couch_util:get_value(<<"doc_write_failures">>, History),

        ?assertEqual(3, MissingChecked),
        ?assertEqual(3, MissingFound),
        ?assertEqual(3, DocsReard),
        ?assertEqual(3, DocsWritten),
        ?assertEqual(0, DocWriteFailures),

        % assert docs in target db
        {ok, 200, _, ADBody} = test_request:get(
            Url ++ "/db2/_all_docs?include_docs=true",
            ?ADMIN_REQ_HEADERS
        ),
        {Json} = jiffy:decode(ADBody),
        ?assertEqual(3, proplists:get_value(<<"total_rows">>, Json))
    end).

should_allow_admin_to_replicate_from_no_access_to_access(_PortType, Url) ->
    ?_test(begin
        % create target db
        {ok, 201, _, _} = test_request:put(
            url() ++ "/db2?q=1&n=1",
            ?ADMIN_REQ_HEADERS,
            ""
        ),
        % set target db security
        {ok, _, _, _} = test_request:put(
            url() ++ "/db2/_security",
            ?ADMIN_REQ_HEADERS,
            jiffy:encode(?SECURITY_OBJECT)
        ),

        % create source docs
        {ok, _, _, _} = test_request:put(
            Url ++ "/db2/a",
            ?ADMIN_REQ_HEADERS,
            "{\"a\":1,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db2/b",
            ?ADMIN_REQ_HEADERS,
            "{\"b\":2,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db2/c",
            ?ADMIN_REQ_HEADERS,
            "{\"c\":3,\"_access\":[\"x\"]}"
        ),

        % replicate
        AdminUrl = string:replace(Url, "http://", "http://a:a@"),
        EJRequestBody =
            {[
                {<<"source">>, list_to_binary(AdminUrl ++ "/db2")},
                {<<"target">>, list_to_binary(AdminUrl ++ "/db")}
            ]},
        {ok, ResponseCode, _, ResponseBody} = test_request:post(
            Url ++ "/_replicate",
            ?ADMIN_REQ_HEADERS,
            jiffy:encode(EJRequestBody)
        ),

        % assert replication status
        {EJResponseBody} = jiffy:decode(ResponseBody),
        ?assertEqual(ResponseCode, 200),
        ?assertEqual(true, couch_util:get_value(<<"ok">>, EJResponseBody)),
        [{History}] = couch_util:get_value(<<"history">>, EJResponseBody),

        MissingChecked = couch_util:get_value(<<"missing_checked">>, History),
        MissingFound = couch_util:get_value(<<"missing_found">>, History),
        DocsReard = couch_util:get_value(<<"docs_read">>, History),
        DocsWritten = couch_util:get_value(<<"docs_written">>, History),
        DocWriteFailures = couch_util:get_value(<<"doc_write_failures">>, History),

        ?assertEqual(3, MissingChecked),
        ?assertEqual(3, MissingFound),
        ?assertEqual(3, DocsReard),
        ?assertEqual(3, DocsWritten),
        ?assertEqual(0, DocWriteFailures),

        % assert docs in target db
        {ok, 200, _, ADBody} = test_request:get(
            Url ++ "/db/_all_docs?include_docs=true",
            ?ADMIN_REQ_HEADERS
        ),
        {Json} = jiffy:decode(ADBody),
        ?assertEqual(3, proplists:get_value(<<"total_rows">>, Json))
    end).

should_allow_admin_to_replicate_from_access_to_no_access(_PortType, Url) ->
    ?_test(begin
        % create target db
        {ok, 201, _, _} = test_request:put(
            url() ++ "/db2?q=1&n=1",
            ?ADMIN_REQ_HEADERS,
            ""
        ),
        % set target db security
        {ok, _, _, _} = test_request:put(
            url() ++ "/db2/_security",
            ?ADMIN_REQ_HEADERS,
            jiffy:encode(?SECURITY_OBJECT)
        ),

        % create source docs
        {ok, _, _, _} = test_request:put(
            Url ++ "/db/a",
            ?ADMIN_REQ_HEADERS,
            "{\"a\":1,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db/b",
            ?ADMIN_REQ_HEADERS,
            "{\"b\":2,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db/c",
            ?ADMIN_REQ_HEADERS,
            "{\"c\":3,\"_access\":[\"x\"]}"
        ),

        % replicate
        AdminUrl = string:replace(Url, "http://", "http://a:a@"),
        EJRequestBody =
            {[
                {<<"source">>, list_to_binary(AdminUrl ++ "/db")},
                {<<"target">>, list_to_binary(AdminUrl ++ "/db2")}
            ]},
        {ok, ResponseCode, _, ResponseBody} = test_request:post(
            Url ++ "/_replicate",
            ?ADMIN_REQ_HEADERS,
            jiffy:encode(EJRequestBody)
        ),

        % assert replication status
        {EJResponseBody} = jiffy:decode(ResponseBody),
        ?assertEqual(ResponseCode, 200),
        ?assertEqual(true, couch_util:get_value(<<"ok">>, EJResponseBody)),
        [{History}] = couch_util:get_value(<<"history">>, EJResponseBody),

        MissingChecked = couch_util:get_value(<<"missing_checked">>, History),
        MissingFound = couch_util:get_value(<<"missing_found">>, History),
        DocsReard = couch_util:get_value(<<"docs_read">>, History),
        DocsWritten = couch_util:get_value(<<"docs_written">>, History),
        DocWriteFailures = couch_util:get_value(<<"doc_write_failures">>, History),

        ?assertEqual(3, MissingChecked),
        ?assertEqual(3, MissingFound),
        ?assertEqual(3, DocsReard),
        ?assertEqual(3, DocsWritten),
        ?assertEqual(0, DocWriteFailures),

        % assert docs in target db
        {ok, 200, _, ADBody} = test_request:get(
            Url ++ "/db2/_all_docs?include_docs=true",
            ?ADMIN_REQ_HEADERS
        ),
        {Json} = jiffy:decode(ADBody),
        ?assertEqual(3, proplists:get_value(<<"total_rows">>, Json))
    end).

should_allow_admin_to_replicate_from_no_access_to_no_access(_PortType, Url) ->
    ?_test(begin
        % create source and target dbs
        {ok, 201, _, _} = test_request:put(
            url() ++ "/db2?q=1&n=1",
            ?ADMIN_REQ_HEADERS,
            ""
        ),
        % set target db security
        {ok, _, _, _} = test_request:put(
            url() ++ "/db2/_security",
            ?ADMIN_REQ_HEADERS,
            jiffy:encode(?SECURITY_OBJECT)
        ),

        {ok, 201, _, _} = test_request:put(
            url() ++ "/db3?q=1&n=1",
            ?ADMIN_REQ_HEADERS,
            ""
        ),
        % set target db security
        {ok, _, _, _} = test_request:put(
            url() ++ "/db3/_security",
            ?ADMIN_REQ_HEADERS,
            jiffy:encode(?SECURITY_OBJECT)
        ),

        % create source docs
        {ok, _, _, _} = test_request:put(
            Url ++ "/db2/a",
            ?ADMIN_REQ_HEADERS,
            "{\"a\":1,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db2/b",
            ?ADMIN_REQ_HEADERS,
            "{\"b\":2,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db2/c",
            ?ADMIN_REQ_HEADERS,
            "{\"c\":3,\"_access\":[\"x\"]}"
        ),

        % replicate
        AdminUrl = string:replace(Url, "http://", "http://a:a@"),
        EJRequestBody =
            {[
                {<<"source">>, list_to_binary(AdminUrl ++ "/db2")},
                {<<"target">>, list_to_binary(AdminUrl ++ "/db3")}
            ]},
        {ok, ResponseCode, _, ResponseBody} = test_request:post(
            Url ++ "/_replicate",
            ?ADMIN_REQ_HEADERS,
            jiffy:encode(EJRequestBody)
        ),

        % assert replication status
        {EJResponseBody} = jiffy:decode(ResponseBody),
        ?assertEqual(ResponseCode, 200),
        ?assertEqual(true, couch_util:get_value(<<"ok">>, EJResponseBody)),
        [{History}] = couch_util:get_value(<<"history">>, EJResponseBody),

        MissingChecked = couch_util:get_value(<<"missing_checked">>, History),
        MissingFound = couch_util:get_value(<<"missing_found">>, History),
        DocsReard = couch_util:get_value(<<"docs_read">>, History),
        DocsWritten = couch_util:get_value(<<"docs_written">>, History),
        DocWriteFailures = couch_util:get_value(<<"doc_write_failures">>, History),

        ?assertEqual(3, MissingChecked),
        ?assertEqual(3, MissingFound),
        ?assertEqual(3, DocsReard),
        ?assertEqual(3, DocsWritten),
        ?assertEqual(0, DocWriteFailures),

        % assert docs in target db
        {ok, 200, _, ADBody} = test_request:get(
            Url ++ "/db3/_all_docs?include_docs=true",
            ?ADMIN_REQ_HEADERS
        ),
        {Json} = jiffy:decode(ADBody),
        ?assertEqual(3, proplists:get_value(<<"total_rows">>, Json))
    end).

should_allow_user_to_replicate_from_access_to_access(_PortType, Url) ->
    ?_test(begin
        % create source and target dbs
        {ok, 201, _, _} = test_request:put(
            url() ++ "/db2?q=1&n=1&access=true",
            ?ADMIN_REQ_HEADERS,
            ""
        ),
        % set target db security
        {ok, _, _, _} = test_request:put(
            url() ++ "/db2/_security",
            ?ADMIN_REQ_HEADERS,
            jiffy:encode(?SECURITY_OBJECT)
        ),

        % create source docs
        {ok, _, _, _} = test_request:put(
            Url ++ "/db/a",
            ?ADMIN_REQ_HEADERS,
            "{\"a\":1,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db/b",
            ?ADMIN_REQ_HEADERS,
            "{\"b\":2,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db/c",
            ?ADMIN_REQ_HEADERS,
            "{\"c\":3,\"_access\":[\"y\"]}"
        ),

        % replicate
        UserXUrl = string:replace(Url, "http://", "http://x:x@"),
        EJRequestBody =
            {[
                {<<"source">>, list_to_binary(UserXUrl ++ "/db")},
                {<<"target">>, list_to_binary(UserXUrl ++ "/db2")}
            ]},
        {ok, ResponseCode, _, ResponseBody} = test_request:post(
            Url ++ "/_replicate",
            ?USERX_REQ_HEADERS,
            jiffy:encode(EJRequestBody)
        ),
        % ?debugFmt("~nResponseBody: ~p~n", [ResponseBody]),

        % assert replication status
        {EJResponseBody} = jiffy:decode(ResponseBody),
        ?assertEqual(ResponseCode, 200),
        ?assertEqual(true, couch_util:get_value(<<"ok">>, EJResponseBody)),

        [{History}] = couch_util:get_value(<<"history">>, EJResponseBody),

        MissingChecked = couch_util:get_value(<<"missing_checked">>, History),
        MissingFound = couch_util:get_value(<<"missing_found">>, History),
        DocsReard = couch_util:get_value(<<"docs_read">>, History),
        DocsWritten = couch_util:get_value(<<"docs_written">>, History),
        DocWriteFailures = couch_util:get_value(<<"doc_write_failures">>, History),

        ?assertEqual(2, MissingChecked),
        ?assertEqual(2, MissingFound),
        ?assertEqual(2, DocsReard),
        ?assertEqual(2, DocsWritten),
        ?assertEqual(0, DocWriteFailures),

        % assert access in local doc
        ReplicationId = couch_util:get_value(<<"replication_id">>, EJResponseBody),
        {ok, 200, _, CheckPoint} = test_request:get(
            Url ++ "/db/_local/" ++ ReplicationId,
            ?USERX_REQ_HEADERS
        ),
        {EJCheckPoint} = jiffy:decode(CheckPoint),
        Access = couch_util:get_value(<<"_access">>, EJCheckPoint),
        ?assertEqual([<<"x">>], Access),

        % make sure others can’t read our local docs
        {ok, 403, _, _} = test_request:get(
            Url ++ "/db/_local/" ++ ReplicationId,
            ?USERY_REQ_HEADERS
        ),

        % assert docs in target db
        {ok, 200, _, ADBody} = test_request:get(
            Url ++ "/db2/_all_docs?include_docs=true",
            ?ADMIN_REQ_HEADERS
        ),
        {Json} = jiffy:decode(ADBody),
        ?assertEqual(2, proplists:get_value(<<"total_rows">>, Json))
    end).

should_allow_user_to_replicate_from_access_to_no_access(_PortType, Url) ->
    ?_test(begin
        % create source and target dbs
        {ok, 201, _, _} = test_request:put(
            url() ++ "/db2?q=1&n=1",
            ?ADMIN_REQ_HEADERS,
            ""
        ),
        % set target db security
        {ok, _, _, _} = test_request:put(
            url() ++ "/db2/_security",
            ?ADMIN_REQ_HEADERS,
            jiffy:encode(?SECURITY_OBJECT)
        ),

        % create source docs
        {ok, _, _, _} = test_request:put(
            Url ++ "/db/a",
            ?ADMIN_REQ_HEADERS,
            "{\"a\":1,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db/b",
            ?ADMIN_REQ_HEADERS,
            "{\"b\":2,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db/c",
            ?ADMIN_REQ_HEADERS,
            "{\"c\":3,\"_access\":[\"y\"]}"
        ),

        % replicate
        UserXUrl = string:replace(Url, "http://", "http://x:x@"),
        EJRequestBody =
            {[
                {<<"source">>, list_to_binary(UserXUrl ++ "/db")},
                {<<"target">>, list_to_binary(UserXUrl ++ "/db2")}
            ]},
        {ok, ResponseCode, _, ResponseBody} = test_request:post(
            Url ++ "/_replicate",
            ?USERX_REQ_HEADERS,
            jiffy:encode(EJRequestBody)
        ),

        % assert replication status
        {EJResponseBody} = jiffy:decode(ResponseBody),
        ?assertEqual(ResponseCode, 200),
        ?assertEqual(true, couch_util:get_value(<<"ok">>, EJResponseBody)),
        [{History}] = couch_util:get_value(<<"history">>, EJResponseBody),

        MissingChecked = couch_util:get_value(<<"missing_checked">>, History),
        MissingFound = couch_util:get_value(<<"missing_found">>, History),
        DocsReard = couch_util:get_value(<<"docs_read">>, History),
        DocsWritten = couch_util:get_value(<<"docs_written">>, History),
        DocWriteFailures = couch_util:get_value(<<"doc_write_failures">>, History),

        ?assertEqual(2, MissingChecked),
        ?assertEqual(2, MissingFound),
        ?assertEqual(2, DocsReard),
        ?assertEqual(2, DocsWritten),
        ?assertEqual(0, DocWriteFailures),

        % assert docs in target db
        {ok, 200, _, ADBody} = test_request:get(
            Url ++ "/db2/_all_docs?include_docs=true",
            ?ADMIN_REQ_HEADERS
        ),
        {Json} = jiffy:decode(ADBody),
        ?assertEqual(2, proplists:get_value(<<"total_rows">>, Json))
    end).

should_allow_user_to_replicate_from_no_access_to_access(_PortType, Url) ->
    ?_test(begin
        % create source and target dbs
        {ok, 201, _, _} = test_request:put(
            url() ++ "/db2?q=1&n=1",
            ?ADMIN_REQ_HEADERS,
            ""
        ),
        % set target db security
        {ok, _, _, _} = test_request:put(
            url() ++ "/db2/_security",
            ?ADMIN_REQ_HEADERS,
            jiffy:encode(?SECURITY_OBJECT)
        ),

        % leave for easier debugging
        % VduFun = <<"function(newdoc, olddoc, userctx) {if(newdoc._id == \"b\") throw({'forbidden':'fail'})}">>,
        % DDoc = {[
        %    {<<"_id">>, <<"_design/vdu">>},
        %    {<<"validate_doc_update">>, VduFun}
        % ]},
        % {ok, _, _, _} = test_request:put(Url ++ "/db/_design/vdu",
        %     ?ADMIN_REQ_HEADERS, jiffy:encode(DDoc)),
        % create source docs
        {ok, _, _, _} = test_request:put(
            Url ++ "/db2/a",
            ?ADMIN_REQ_HEADERS,
            "{\"a\":1,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db2/b",
            ?ADMIN_REQ_HEADERS,
            "{\"b\":2,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db2/c",
            ?ADMIN_REQ_HEADERS,
            "{\"c\":3,\"_access\":[\"y\"]}"
        ),

        % replicate
        UserXUrl = string:replace(Url, "http://", "http://x:x@"),
        EJRequestBody =
            {[
                {<<"source">>, list_to_binary(UserXUrl ++ "/db2")},
                {<<"target">>, list_to_binary(UserXUrl ++ "/db")}
            ]},
        {ok, ResponseCode, _, ResponseBody} = test_request:post(
            Url ++ "/_replicate",
            ?USERX_REQ_HEADERS,
            jiffy:encode(EJRequestBody)
        ),

        % assert replication status
        {EJResponseBody} = jiffy:decode(ResponseBody),
        ?assertEqual(ResponseCode, 200),
        ?assertEqual(true, couch_util:get_value(<<"ok">>, EJResponseBody)),
        [{History}] = couch_util:get_value(<<"history">>, EJResponseBody),

        MissingChecked = couch_util:get_value(<<"missing_checked">>, History),
        MissingFound = couch_util:get_value(<<"missing_found">>, History),
        DocsRead = couch_util:get_value(<<"docs_read">>, History),
        DocsWritten = couch_util:get_value(<<"docs_written">>, History),
        DocWriteFailures = couch_util:get_value(<<"doc_write_failures">>, History),
        % ?debugFmt("~n History: ~p ~n", [History]),
        ?assertEqual(3, MissingChecked),
        ?assertEqual(3, MissingFound),
        ?assertEqual(3, DocsRead),
        ?assertEqual(2, DocsWritten),
        ?assertEqual(1, DocWriteFailures),

        % assert docs in target db
        {ok, 200, _, ADBody} = test_request:get(
            Url ++ "/db/_all_docs?include_docs=true",
            ?ADMIN_REQ_HEADERS
        ),
        {Json} = jiffy:decode(ADBody),
        ?assertEqual(2, proplists:get_value(<<"total_rows">>, Json))
    end).

should_allow_user_to_replicate_from_no_access_to_no_access(_PortType, Url) ->
    ?_test(begin
        % create source and target dbs
        {ok, 201, _, _} = test_request:put(
            url() ++ "/db2?q=1&n=1",
            ?ADMIN_REQ_HEADERS,
            ""
        ),
        % set target db security
        {ok, _, _, _} = test_request:put(
            url() ++ "/db2/_security",
            ?ADMIN_REQ_HEADERS,
            jiffy:encode(?SECURITY_OBJECT)
        ),

        {ok, 201, _, _} = test_request:put(
            url() ++ "/db3?q=1&n=1",
            ?ADMIN_REQ_HEADERS,
            ""
        ),
        % set target db security
        {ok, _, _, _} = test_request:put(
            url() ++ "/db3/_security",
            ?ADMIN_REQ_HEADERS,
            jiffy:encode(?SECURITY_OBJECT)
        ),
        % create source docs
        {ok, _, _, _} = test_request:put(
            Url ++ "/db2/a",
            ?ADMIN_REQ_HEADERS,
            "{\"a\":1,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db2/b",
            ?ADMIN_REQ_HEADERS,
            "{\"b\":2,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db2/c",
            ?ADMIN_REQ_HEADERS,
            "{\"c\":3,\"_access\":[\"y\"]}"
        ),

        % replicate
        UserXUrl = string:replace(Url, "http://", "http://x:x@"),
        EJRequestBody =
            {[
                {<<"source">>, list_to_binary(UserXUrl ++ "/db2")},
                {<<"target">>, list_to_binary(UserXUrl ++ "/db3")}
            ]},
        {ok, ResponseCode, _, ResponseBody} = test_request:post(
            Url ++ "/_replicate",
            ?USERX_REQ_HEADERS,
            jiffy:encode(EJRequestBody)
        ),

        % assert replication status
        {EJResponseBody} = jiffy:decode(ResponseBody),
        ?assertEqual(ResponseCode, 200),
        ?assertEqual(true, couch_util:get_value(<<"ok">>, EJResponseBody)),
        [{History}] = couch_util:get_value(<<"history">>, EJResponseBody),

        MissingChecked = couch_util:get_value(<<"missing_checked">>, History),
        MissingFound = couch_util:get_value(<<"missing_found">>, History),
        DocsReard = couch_util:get_value(<<"docs_read">>, History),
        DocsWritten = couch_util:get_value(<<"docs_written">>, History),
        DocWriteFailures = couch_util:get_value(<<"doc_write_failures">>, History),

        ?assertEqual(3, MissingChecked),
        ?assertEqual(3, MissingFound),
        ?assertEqual(3, DocsReard),
        ?assertEqual(3, DocsWritten),
        ?assertEqual(0, DocWriteFailures),

        % assert docs in target db
        {ok, 200, _, ADBody} = test_request:get(
            Url ++ "/db3/_all_docs?include_docs=true",
            ?ADMIN_REQ_HEADERS
        ),
        {Json} = jiffy:decode(ADBody),
        ?assertEqual(3, proplists:get_value(<<"total_rows">>, Json))
    end).

% revs_diff
should_not_allow_user_to_revs_diff_other_docs(_PortType, Url) ->
    ?_test(begin
        % create test docs
        {ok, _, _, _} = test_request:put(
            Url ++ "/db/a",
            ?ADMIN_REQ_HEADERS,
            "{\"a\":1,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, _} = test_request:put(
            Url ++ "/db/b",
            ?ADMIN_REQ_HEADERS,
            "{\"b\":2,\"_access\":[\"x\"]}"
        ),
        {ok, _, _, V} = test_request:put(
            Url ++ "/db/c",
            ?ADMIN_REQ_HEADERS,
            "{\"c\":3,\"_access\":[\"y\"]}"
        ),

        % nothing missing
        RevsDiff =
            {[
                {<<"a">>, [
                    <<"1-23202479633c2b380f79507a776743d5">>
                ]}
            ]},
        {ok, GoodCode, _, GoodBody} = test_request:post(
            Url ++ "/db/_revs_diff",
            ?USERX_REQ_HEADERS,
            jiffy:encode(RevsDiff)
        ),
        EJGoodBody = jiffy:decode(GoodBody),
        ?assertEqual(200, GoodCode),
        ?assertEqual({[]}, EJGoodBody),

        % something missing
        MissingRevsDiff =
            {[
                {<<"a">>, [
                    <<"1-missing">>
                ]}
            ]},
        {ok, MissingCode, _, MissingBody} = test_request:post(
            Url ++ "/db/_revs_diff",
            ?USERX_REQ_HEADERS,
            jiffy:encode(MissingRevsDiff)
        ),
        EJMissingBody = jiffy:decode(MissingBody),
        ?assertEqual(200, MissingCode),
        MissingExpect =
            {[
                {<<"a">>,
                    {[
                        {<<"missing">>, [<<"1-missing">>]}
                    ]}}
            ]},
        ?assertEqual(MissingExpect, EJMissingBody),

        % other doc
        OtherRevsDiff =
            {[
                {<<"c">>, [
                    <<"1-92aef5b0e4a3f4db0aba1320869bc95d">>
                ]}
            ]},
        {ok, OtherCode, _, OtherBody} = test_request:post(
            Url ++ "/db/_revs_diff",
            ?USERX_REQ_HEADERS,
            jiffy:encode(OtherRevsDiff)
        ),
        EJOtherBody = jiffy:decode(OtherBody),
        ?assertEqual(200, OtherCode),
        ?assertEqual({[]}, EJOtherBody)
    end).
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

port() ->
    integer_to_list(mochiweb_socket_server:get(chttpd, port)).

% Potential future feature:%
% should_let_user_fetch_their_own_all_docs_plus_users_ddocs(_PortType, Url) ->
%     {ok, 201, _, _} = test_request:put(Url ++ "/db/a",
%         ?ADMIN_REQ_HEADERS, "{\"a\":1,\"_access\":[\"x\"]}"),
%     {ok, 201, _, _} = test_request:put(Url ++ "/db/_design/foo",
%         ?ADMIN_REQ_HEADERS, "{\"a\":1,\"_access\":[\"_users\"]}"),
%     {ok, 201, _, _} = test_request:put(Url ++ "/db/_design/bar",
%         ?ADMIN_REQ_HEADERS, "{\"a\":1,\"_access\":[\"houdini\"]}"),
%     {ok, 201, _, _} = test_request:put(Url ++ "/db/b",
%         ?USERX_REQ_HEADERS, "{\"b\":2,\"_access\":[\"x\"]}"),
%
%     % % TODO: add allowing non-admin users adding non-admin ddocs
%     {ok, 201, _, _} = test_request:put(Url ++ "/db/_design/x",
%         ?ADMIN_REQ_HEADERS, "{\"b\":2,\"_access\":[\"x\"]}"),
%
%     {ok, 201, _, _} = test_request:put(Url ++ "/db/c",
%         ?ADMIN_REQ_HEADERS, "{\"c\":3,\"_access\":[\"y\"]}"),
%     {ok, 201, _, _} = test_request:put(Url ++ "/db/d",
%         ?USERY_REQ_HEADERS, "{\"d\":4,\"_access\":[\"y\"]}"),
%     {ok, 200, _, Body} = test_request:get(Url ++ "/db/_all_docs?include_docs=true",
%         ?USERX_REQ_HEADERS),
%     {Json} = jiffy:decode(Body),
%     ?_assertEqual(3, length(proplists:get_value(<<"rows">>, Json))).
%     ?debugFmt("~nHSOIN: ~p~n", [Json]),
