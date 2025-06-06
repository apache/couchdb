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

-module(couch_mrview_all_docs_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

setup() ->
    ok = config:set("query_server_config", "query_limit", "infinity", false),
    {ok, Db} = couch_mrview_test_util:init_db(?tempdb(), map),
    Db.

teardown(Db) ->
    couch_db:close(Db),
    couch_server:delete(couch_db:name(Db), [?ADMIN_CTX]),
    config:delete("query_server_config", "query_limit", false).

all_docs_test_() ->
    {
        "_all_docs view tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(should_query),
                    ?TDEF_FE(should_query_with_range),
                    ?TDEF_FE(should_query_with_non_string_key),
                    ?TDEF_FE(should_query_with_non_string_keys),
                    ?TDEF_FE(should_query_with_range_and_same_keys),
                    ?TDEF_FE(raise_error_query_with_range_and_different_keys),
                    ?TDEF_FE(should_query_with_range_rev),
                    ?TDEF_FE(should_query_with_limit_and_skip),
                    ?TDEF_FE(should_query_with_include_docs),
                    ?TDEF_FE(should_query_empty_views),
                    ?TDEF_FE(http_query),
                    ?TDEF_FE(http_query_with_range),
                    ?TDEF_FE(http_query_with_non_string_key),
                    ?TDEF_FE(http_query_with_range_rev),
                    ?TDEF_FE(http_query_with_limit_and_skip),
                    ?TDEF_FE(http_query_with_limit_and_skip_and_query_limit),
                    ?TDEF_FE(http_query_with_query_limit_and_over_limit),
                    ?TDEF_FE(http_query_with_include_docs)
                ]
            }
        }
    }.

should_query(Db) ->
    Result = run_query(Db, ""),
    Expect =
        {ok, [
            {meta, [{total, 11}, {offset, 0}]},
            mk_row(<<"1">>, <<"1-08d53a5760b95fce6df2e2c5b008be39">>),
            mk_row(<<"10">>, <<"1-a05b6ea2bc0243949f103d5b4f15f71e">>),
            mk_row(<<"2">>, <<"1-b57c77a9e6f7574ca6469f0d6dcd78bb">>),
            mk_row(<<"3">>, <<"1-7fbf84d56f8017880974402d60f5acd6">>),
            mk_row(<<"4">>, <<"1-fcaf5852c08ffb239ac8ce16c409f253">>),
            mk_row(<<"5">>, <<"1-aaac5d460fd40f9286e57b9bf12e23d2">>),
            mk_row(<<"6">>, <<"1-aca21c2e7bc5f8951424fcfc5d1209d8">>),
            mk_row(<<"7">>, <<"1-4374aeec17590d82f16e70f318116ad9">>),
            mk_row(<<"8">>, <<"1-55b9a29311341e07ec0a7ca13bc1b59f">>),
            mk_row(<<"9">>, <<"1-558c8487d9aee25399a91b5d31d90fe2">>),
            mk_row(<<"_design/bar">>, <<"1-a44e1dd1994a7717bf89c894ebd1f081">>)
        ]},
    ?assertEqual(Expect, Result).

should_query_with_range(Db) ->
    Result = run_query(Db, [{start_key, <<"3">>}, {end_key, <<"5">>}]),
    Expect =
        {ok, [
            {meta, [{total, 11}, {offset, 3}]},
            mk_row(<<"3">>, <<"1-7fbf84d56f8017880974402d60f5acd6">>),
            mk_row(<<"4">>, <<"1-fcaf5852c08ffb239ac8ce16c409f253">>),
            mk_row(<<"5">>, <<"1-aaac5d460fd40f9286e57b9bf12e23d2">>)
        ]},
    ?assertEqual(Expect, Result).

should_query_with_non_string_key(Db) ->
    Expect = {ok, [{meta, [{total, 11}, {offset, 0}]}]},
    [
        ?assertEqual(Expect, run_query(Db, [{start_key, Key}, {end_key, Key}]))
     || Key <- [1, a, [1, "2"], #{id => 1}]
    ].

should_query_with_non_string_keys(Db) ->
    [
        ?assertEqual(
            {ok, [
                {meta, [{total, 11}, {offset, 0}]},
                {row, [{id, error}, {key, Key}, {value, not_found}]}
            ]},
            run_query(Db, [{keys, [Key]}])
        )
     || Key <- [1, a, [1, "2"], #{id => 1}]
    ].

should_query_with_range_and_same_keys(Db) ->
    Result = run_query(Db, [{keys, [<<"3">>]}, {start_key, <<"3">>}, {end_key, <<"3">>}]),
    Expect =
        {ok, [
            {meta, [{total, 11}, {offset, 0}]},
            mk_row(<<"3">>, <<"1-7fbf84d56f8017880974402d60f5acd6">>)
        ]},
    ?assertEqual(Expect, Result).

raise_error_query_with_range_and_different_keys(Db) ->
    Error = {query_parse_error, <<"`keys` is incompatible with `key`, `start_key` and `end_key`">>},
    ?assertThrow(Error, run_query(Db, [{keys, [<<"1">>]}, {start_key, <<"5">>}])),
    ?assertThrow(Error, run_query(Db, [{keys, [<<"5">>]}, {start_key, <<"5">>}])).

should_query_with_range_rev(Db) ->
    Result = run_query(Db, [
        {direction, rev},
        {start_key, <<"5">>},
        {end_key, <<"3">>},
        {inclusive_end, true}
    ]),
    Expect =
        {ok, [
            {meta, [{total, 11}, {offset, 5}]},
            mk_row(<<"5">>, <<"1-aaac5d460fd40f9286e57b9bf12e23d2">>),
            mk_row(<<"4">>, <<"1-fcaf5852c08ffb239ac8ce16c409f253">>),
            mk_row(<<"3">>, <<"1-7fbf84d56f8017880974402d60f5acd6">>)
        ]},
    ?assertEqual(Expect, Result).

should_query_with_limit_and_skip(Db) ->
    Result = run_query(Db, [
        {start_key, <<"2">>},
        {limit, 3},
        {skip, 3}
    ]),
    Expect =
        {ok, [
            {meta, [{total, 11}, {offset, 5}]},
            mk_row(<<"5">>, <<"1-aaac5d460fd40f9286e57b9bf12e23d2">>),
            mk_row(<<"6">>, <<"1-aca21c2e7bc5f8951424fcfc5d1209d8">>),
            mk_row(<<"7">>, <<"1-4374aeec17590d82f16e70f318116ad9">>)
        ]},
    ?assertEqual(Expect, Result).

should_query_with_include_docs(Db) ->
    Result = run_query(Db, [
        {start_key, <<"8">>},
        {end_key, <<"8">>},
        {include_docs, true}
    ]),
    Doc =
        {[
            {<<"_id">>, <<"8">>},
            {<<"_rev">>, <<"1-55b9a29311341e07ec0a7ca13bc1b59f">>},
            {<<"val">>, 8}
        ]},
    Val = {[{rev, <<"1-55b9a29311341e07ec0a7ca13bc1b59f">>}]},
    Expect =
        {ok, [
            {meta, [{total, 11}, {offset, 8}]},
            {row, [{id, <<"8">>}, {key, <<"8">>}, {value, Val}, {doc, Doc}]}
        ]},
    ?assertEqual(Expect, Result).

should_query_empty_views(Db) ->
    Result = couch_mrview:query_view(Db, <<"_design/bar">>, <<"bing">>),
    Expect =
        {ok, [
            {meta, [{total, 0}, {offset, 0}]}
        ]},
    ?assertEqual(Expect, Result).

mk_row(Id, Rev) ->
    {row, [{id, Id}, {key, Id}, {value, {[{rev, Rev}]}}]}.

run_query(Db, Opts) ->
    couch_mrview:query_all_docs(Db, Opts).

http_query(Db) ->
    ?assertMatch(
        [
            #{<<"id">> := <<"1">>, <<"key">> := <<"1">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"10">>, <<"key">> := <<"10">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"2">>, <<"key">> := <<"2">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"3">>, <<"key">> := <<"3">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"4">>, <<"key">> := <<"4">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"5">>, <<"key">> := <<"5">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"6">>, <<"key">> := <<"6">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"7">>, <<"key">> := <<"7">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"8">>, <<"key">> := <<"8">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"9">>, <<"key">> := <<"9">>, <<"value">> := #{<<"rev">> := _}},
            #{
                <<"id">> := <<"_design/bar">>,
                <<"key">> := <<"_design/bar">>,
                <<"value">> := #{<<"rev">> := _}
            }
        ],
        http_query(Db, "")
    ).

http_query_with_range(Db) ->
    ?assertMatch(
        [
            #{<<"id">> := <<"3">>, <<"key">> := <<"3">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"4">>, <<"key">> := <<"4">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"5">>, <<"key">> := <<"5">>, <<"value">> := #{<<"rev">> := _}}
        ],
        http_query(Db, "?start_key=\"3\"&end_key=\"5\"")
    ).

http_query_with_non_string_key(Db) ->
    {Code1, Res1} = http_req(Db, "?start_key=1&end_key=1"),
    ?assertEqual(200, Code1),
    ?assertEqual(#{<<"offset">> => 0, <<"rows">> => [], <<"total_rows">> => 11}, Res1),

    {Code2, Res2} = http_req(Db, "?start_key=a&end_key=a"),
    ?assertEqual(400, Code2),
    ?assertEqual(#{<<"error">> => <<"bad_request">>, <<"reason">> => <<"invalid_json">>}, Res2),

    {Code3, Res3} = http_req(Db, "?start_key=\[1,\"2\"\]&end_key=\[1,\"2\"\]"),
    ?assertEqual(200, Code3),
    ?assertEqual(#{<<"offset">> => 0, <<"rows">> => [], <<"total_rows">> => 11}, Res3),

    {Code4, Res4} = http_req(Db, "?start_key={\"id\":1}&end_key={\"id\":1}"),
    ?assertEqual(200, Code4),
    ?assertEqual(#{<<"offset">> => 0, <<"rows">> => [], <<"total_rows">> => 11}, Res4).

http_query_with_range_rev(Db) ->
    Params = "?descending=true&start_key=\"5\"&end_key=\"3\"&include_end=true",
    ?assertMatch(
        [
            #{<<"id">> := <<"5">>, <<"key">> := <<"5">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"4">>, <<"key">> := <<"4">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"3">>, <<"key">> := <<"3">>, <<"value">> := #{<<"rev">> := _}}
        ],
        http_query(Db, Params)
    ).

http_query_with_limit_and_skip(Db) ->
    Params = "?start_key=\"2\"&limit=3&skip=3",
    ?assertMatch(
        [
            #{<<"id">> := <<"5">>, <<"key">> := <<"5">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"6">>, <<"key">> := <<"6">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"7">>, <<"key">> := <<"7">>, <<"value">> := #{<<"rev">> := _}}
        ],
        http_query(Db, Params)
    ).

http_query_with_limit_and_skip_and_query_limit(Db) ->
    config:set("query_server_config", "query_limit", "2", false),
    Params = "?start_key=\"3\"&skip=2",
    ?assertMatch(
        [
            #{<<"id">> := <<"5">>, <<"key">> := <<"5">>, <<"value">> := #{<<"rev">> := _}},
            #{<<"id">> := <<"6">>, <<"key">> := <<"6">>, <<"value">> := #{<<"rev">> := _}}
        ],
        http_query(Db, Params)
    ).

http_query_with_query_limit_and_over_limit(Db) ->
    config:set("query_server_config", "query_limit", "2", false),
    {Code, Res} = http_req(Db, "?limit=3"),
    ?assertEqual(400, Code),
    ?assertMatch(
        #{
            <<"error">> := <<"query_parse_error">>,
            <<"reason">> := <<"Limit is too large", _/binary>>
        },
        Res
    ).

http_query_with_include_docs(Db) ->
    Params = "?start_key=\"8\"&end_key=\"8\"&limit=8&include_docs=true",
    ?assertMatch(
        [
            #{
                <<"id">> := Id,
                <<"key">> := Id,
                <<"value">> := #{<<"rev">> := Rev},
                <<"doc">> := #{
                    <<"_id">> := Id,
                    <<"_rev">> := Rev,
                    <<"val">> := 8
                }
            }
        ],
        http_query(Db, Params)
    ).

db_url(Db) ->
    DbName = couch_db:name(Db),
    Addr = config:get("httpd", "bind_address", "127.0.0.1"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    "http://" ++ Addr ++ ":" ++ Port ++ "/" ++ ?b2l(DbName).

http_req(DbName, Params) ->
    Url = db_url(DbName) ++ "/_all_docs",
    {ok, Code, _Headers, Body} = test_request:get(Url ++ Params),
    Res = #{} = jiffy:decode(Body, [return_maps]),
    {Code, Res}.

http_query(DbName, Params) ->
    Url = db_url(DbName) ++ "/_all_docs",
    {ok, Code, _Headers, Body} = test_request:get(Url ++ Params),
    ?assertEqual(200, Code),
    #{<<"rows">> := Rows} = jiffy:decode(Body, [return_maps]),
    Rows.
