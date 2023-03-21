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

-module(chttpd_view_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_view_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).

-define(DOCS, #{
    <<"docs">> => [
        #{<<"_id">> => <<"a">>, <<"key">> => <<"a">>, <<"value">> => 1},
        #{<<"_id">> => <<"b">>, <<"key">> => <<"b">>, <<"value">> => 2},
        #{<<"_id">> => <<"c">>, <<"key">> => <<"c">>, <<"value">> => 3},
        #{<<"_id">> => <<"d">>, <<"key">> => <<"d">>, <<"_deleted">> => true}
    ]
}).
-define(DDOC, #{
    <<"_id">> => <<"_design/ddoc">>,
    <<"views">> => #{
        <<"map">> => #{<<"map">> => <<"function(doc) { emit(doc.key, doc.value) }">>},
        <<"map_reduce">> => #{
            <<"map">> => <<"function(doc) { emit(doc.key, doc.value) }">>,
            <<"reduce">> => <<"_sum">>
        }
    }
}).
-define(ERROR_KEYS_INCOMPATIBLE, #{
    <<"error">> := <<"query_parse_error">>,
    <<"reason">> := <<"`keys` is incompatible with `key`, `start_key` and `end_key`">>
}).

% seconds
-define(TIMEOUT, 60).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    Db = ?tempdb(),
    ok = create_db(Db),
    ok = create_docs(Db),
    ok = create_ddoc(Db),
    Db.

teardown(Db) ->
    ok = fabric:delete_db(Db),
    ok = config:delete("admins", ?USER, _Persist = false).

view_test_() ->
    {
        "chttpd view tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(t_view_with_queries_keys, ?TIMEOUT),
                    ?TDEF_FE(t_view_with_queries_limit_skip, ?TIMEOUT),
                    ?TDEF_FE(t_view_with_multiple_queries, ?TIMEOUT),
                    ?TDEF_FE(t_view_with_key_and_start_key),
                    ?TDEF_FE(t_view_with_key_and_end_key),
                    ?TDEF_FE(t_view_with_single_keys_and_start_key),
                    ?TDEF_FE(t_view_with_keys_and_start_key),
                    ?TDEF_FE(t_view_with_key_non_existent_docs),
                    ?TDEF_FE(t_view_with_keys_non_existent_docs),
                    ?TDEF_FE(t_view_with_key_deleted_docs),
                    ?TDEF_FE(t_view_with_keys_deleted_docs),
                    ?TDEF_FE(t_view_map_reduce_with_key),
                    ?TDEF_FE(t_view_map_reduce_with_single_keys),
                    ?TDEF_FE(t_view_map_reduce_with_single_keys_and_group),
                    ?TDEF_FE(t_view_map_reduce_with_keys),
                    ?TDEF_FE(t_view_map_reduce_with_keys_and_group)
                ]
            }
        }
    }.

all_docs_test_() ->
    {
        "chttpd all docs tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(t_all_docs_with_key_and_start_key),
                    ?TDEF_FE(t_all_docs_with_key_and_end_key),
                    ?TDEF_FE(t_all_docs_with_single_keys_and_start_key),
                    ?TDEF_FE(t_all_docs_with_keys_and_start_key),
                    ?TDEF_FE(t_all_docs_with_key_non_existent_docs),
                    ?TDEF_FE(t_all_docs_with_keys_non_existent_docs),
                    ?TDEF_FE(t_all_docs_with_key_deleted_docs),
                    ?TDEF_FE(t_all_docs_with_keys_deleted_docs)
                ]
            }
        }
    }.

t_view_with_queries_keys(Db) ->
    QueryDoc = #{<<"queries">> => [#{<<"keys">> => [<<"a">>, <<"c">>]}]},
    {Code, Res} = req(post, url(Db, "_design/ddoc/_view/map/queries"), QueryDoc),
    ?assertMatch(
        #{
            <<"results">> := [
                #{
                    <<"total_rows">> := 3,
                    <<"offset">> := 1,
                    <<"rows">> := [#{<<"id">> := <<"a">>}, #{<<"id">> := <<"c">>}]
                }
            ]
        },
        Res
    ),
    ?assertEqual(200, Code).

t_view_with_queries_limit_skip(Db) ->
    QueryDoc = #{<<"queries">> => [#{<<"limit">> => 1, <<"skip">> => 1}]},
    {Code, Res} = req(post, url(Db, "_design/ddoc/_view/map/queries/"), QueryDoc),
    ?assertMatch(
        #{
            <<"results">> := [
                #{<<"total_rows">> := 3, <<"offset">> := 1, <<"rows">> := [#{<<"id">> := <<"b">>}]}
            ]
        },
        Res
    ),
    ?assertEqual(200, Code).

t_view_with_multiple_queries(Db) ->
    QueryDoc = #{
        <<"queries">> => [#{<<"keys">> => [<<"a">>, <<"c">>], <<"limit">> => 1, <<"skip">> => 1}]
    },
    {Code, Res} = req(post, url(Db, "_design/ddoc/_view/map/queries/"), QueryDoc),
    ?assertMatch(
        #{
            <<"results">> := [
                #{<<"total_rows">> := 3, <<"offset">> := 2, <<"rows">> := [#{<<"id">> := <<"c">>}]}
            ]
        },
        Res
    ),
    ?assertEqual(200, Code).

t_view_with_key_and_start_key(Db) ->
    {Code1, Res1} = req(get, url(Db, "_design/ddoc/_view/map", "key=\"a\"&startkey=\"b\"")),
    {Code2, Res2} = req(get, url(Db, "_design/ddoc/_view/map", "startkey=\"b\"&key=\"a\"")),
    ?assertMatch(
        #{
            <<"error">> := <<"query_parse_error">>,
            <<"reason">> :=
                <<"No rows can match your key range, reverse your start_key and end_key or set descending=true">>
        },
        Res1
    ),
    ?assertMatch(#{<<"rows">> := [#{<<"id">> := <<"a">>}]}, Res2),
    ?assertEqual(400, Code1),
    ?assertEqual(200, Code2).

t_all_docs_with_key_and_start_key(Db) ->
    {Code1, Res1} = req(get, url(Db, "_all_docs", "key=\"a\"&startkey=\"b\"")),
    {Code2, Res2} = req(get, url(Db, "_all_docs", "startkey=\"b\"&key=\"a\"")),
    ?assertMatch(#{<<"rows">> := []}, Res1),
    ?assertMatch(#{<<"rows">> := [#{<<"id">> := <<"a">>}]}, Res2),
    ?assertEqual(200, Code1),
    ?assertEqual(200, Code2).

t_view_with_key_and_end_key(Db) ->
    test_helper_key_and_end_key(Db, "_design/ddoc/_view/map").

t_all_docs_with_key_and_end_key(Db) ->
    test_helper_key_and_end_key(Db, "_all_docs").

test_helper_key_and_end_key(Db, Path) ->
    {Code1, Res1} = req(get, url(Db, Path, "key=\"a\"&endkey=\"b\"")),
    {Code2, Res2} = req(get, url(Db, Path, "endkey=\"b\"&key=\"a\"")),
    ?assertMatch(#{<<"rows">> := [#{<<"id">> := <<"a">>}, #{<<"id">> := <<"b">>}]}, Res1),
    ?assertMatch(#{<<"rows">> := [#{<<"id">> := <<"a">>}]}, Res2),
    ?assertEqual(200, Code1),
    ?assertEqual(200, Code2).

t_view_with_single_keys_and_start_key(Db) ->
    {Code, Res} = req(get, url(Db, "_design/ddoc/_view/map?keys=[\"a\"]&startkey=\"b\"")),
    ?assertMatch(
        #{
            <<"error">> := <<"query_parse_error">>,
            <<"reason">> :=
                <<"No rows can match your key range, reverse your start_key and end_key or set descending=true">>
        },
        Res
    ),
    ?assertEqual(400, Code).

t_all_docs_with_single_keys_and_start_key(Db) ->
    {Code, Res} = req(get, url(Db, "_all_docs?keys=[\"a\"]&startkey=\"b\"")),
    ?assertMatch(?ERROR_KEYS_INCOMPATIBLE, Res),
    ?assertEqual(400, Code).

t_view_with_keys_and_start_key(Db) ->
    {Code, Res} = req(get, url(Db, "_design/ddoc/_view/map", "keys=[\"a\",\"b\"]&start_key=\"b\"")),
    ?assertMatch(?ERROR_KEYS_INCOMPATIBLE, Res),
    ?assertEqual(400, Code).

t_all_docs_with_keys_and_start_key(Db) ->
    {Code, Res} = req(get, url(Db, "_all_docs", "keys=[\"a\",\"b\"]&start_key=\"b\"")),
    ?assertMatch(?ERROR_KEYS_INCOMPATIBLE, Res),
    ?assertEqual(400, Code).

t_view_with_key_non_existent_docs(Db) ->
    {Code, Res} = req(get, url(Db, "_design/ddoc/_view/map", "key=\"not_exist\"")),
    ?assertMatch(#{<<"total_rows">> := 3, <<"offset">> := 3, <<"rows">> := []}, Res),
    ?assertEqual(200, Code),
    {Code1, Res1} = req(post, url(Db, "_design/ddoc/_view/map"), #{<<"key">> => <<"not_exist">>}),
    ?assertEqual(Res, Res1),
    ?assertEqual(Code, Code1).

t_all_docs_with_key_non_existent_docs(Db) ->
    {Code, Res} = req(get, url(Db, "_all_docs", "key=\"not_exist\"")),
    ?assertMatch(#{<<"total_rows">> := 4, <<"offset">> := 4, <<"rows">> := []}, Res),
    ?assertEqual(200, Code),
    {Code1, Res1} = req(post, url(Db, "_all_docs"), #{<<"key">> => <<"not_exist">>}),
    ?assertEqual(Res, Res1),
    ?assertEqual(Code, Code1).

t_view_with_keys_non_existent_docs(Db) ->
    {Code, Res} = req(get, url(Db, "_design/ddoc/_view/map", "keys=[\"not_exist\"]")),
    ?assertMatch(#{<<"total_rows">> := 3, <<"offset">> := 3, <<"rows">> := []}, Res),
    ?assertEqual(200, Code),
    {Code1, Res1} = req(post, url(Db, "_design/ddoc/_view/map"), #{<<"keys">> => [<<"not_exist">>]}),
    ?assertEqual(Res, Res1),
    ?assertEqual(Code, Code1).

t_all_docs_with_keys_non_existent_docs(Db) ->
    {Code, Res} = req(get, url(Db, "_all_docs", "keys=[\"not_exist\"]")),
    ?assertMatch(
        #{
            <<"total_rows">> := 4,
            <<"offset">> := null,
            <<"rows">> := [#{<<"key">> := <<"not_exist">>, <<"error">> := <<"not_found">>}]
        },
        Res
    ),
    ?assertEqual(200, Code),
    {Code1, Res1} = req(post, url(Db, "_all_docs"), #{<<"keys">> => [<<"not_exist">>]}),
    ?assertEqual(Res, Res1),
    ?assertEqual(Code, Code1).

t_view_with_key_deleted_docs(Db) ->
    {Code, Res} = req(get, url(Db, "_design/ddoc/_view/map", "key=\"d\"")),
    ?assertMatch(#{<<"total_rows">> := 3, <<"offset">> := 3, <<"rows">> := []}, Res),
    ?assertEqual(200, Code),
    {Code1, Res1} = req(post, url(Db, "_design/ddoc/_view/map"), #{<<"key">> => <<"d">>}),
    ?assertEqual(Res, Res1),
    ?assertEqual(Code, Code1).

t_all_docs_with_key_deleted_docs(Db) ->
    {Code, Res} = req(get, url(Db, "_all_docs", "key=\"d\"")),
    ?assertMatch(#{<<"total_rows">> := 4, <<"offset">> := 4, <<"rows">> := []}, Res),
    ?assertEqual(200, Code),
    {Code1, Res1} = req(post, url(Db, "_all_docs"), #{<<"key">> => <<"d">>}),
    ?assertEqual(Res, Res1),
    ?assertEqual(Code, Code1).

t_view_with_keys_deleted_docs(Db) ->
    {Code, Res} = req(get, url(Db, "_design/ddoc/_view/map", "keys=[\"d\"]")),
    ?assertMatch(#{<<"total_rows">> := 3, <<"offset">> := 3, <<"rows">> := []}, Res),
    ?assertEqual(200, Code),
    {Code1, Res1} = req(post, url(Db, "_design/ddoc/_view/map"), #{<<"keys">> => [<<"d">>]}),
    ?assertEqual(Res, Res1),
    ?assertEqual(Code, Code1).

t_all_docs_with_keys_deleted_docs(Db) ->
    {Code, Res} = req(get, url(Db, "_all_docs", "keys=[\"d\"]")),
    ?assertMatch(
        #{
            <<"total_rows">> := 4,
            <<"offset">> := null,
            <<"rows">> := [
                #{
                    <<"id">> := <<"d">>,
                    <<"key">> := <<"d">>,
                    <<"value">> := #{<<"deleted">> := true}
                }
            ]
        },
        Res
    ),
    ?assertEqual(200, Code),
    {Code1, Res1} = req(post, url(Db, "_all_docs"), #{<<"keys">> => [<<"d">>]}),
    ?assertEqual(Res, Res1),
    ?assertEqual(Code, Code1).

t_view_map_reduce_with_key(Db) ->
    {Code, Res} = req(get, url(Db, "_design/ddoc/_view/map_reduce", "key=\"a\"")),
    ?assertMatch(#{<<"rows">> := [#{<<"key">> := null, <<"value">> := 1}]}, Res),
    ?assertEqual(200, Code),
    {Code1, Res1} = req(post, url(Db, "_design/ddoc/_view/map_reduce"), #{<<"key">> => <<"a">>}),
    ?assertEqual(Res, Res1),
    ?assertEqual(Code, Code1).

t_view_map_reduce_with_single_keys(Db) ->
    {Code, Res} = req(get, url(Db, "_design/ddoc/_view/map_reduce", "keys=[\"a\"]")),
    ?assertMatch(#{<<"rows">> := [#{<<"key">> := null, <<"value">> := 1}]}, Res),
    ?assertEqual(200, Code),
    {Code1, Res1} = req(post, url(Db, "_design/ddoc/_view/map_reduce"), #{<<"keys">> => [<<"a">>]}),
    ?assertEqual(Res, Res1),
    ?assertEqual(Code, Code1).

t_view_map_reduce_with_single_keys_and_group(Db) ->
    {Code, Res} = req(get, url(Db, "_design/ddoc/_view/map_reduce?keys=[\"a\"]&group=true")),
    ?assertMatch(#{<<"rows">> := [#{<<"key">> := <<"a">>, <<"value">> := 1}]}, Res),
    ?assertEqual(200, Code),
    {Code1, Res1} = req(post, url(Db, "_design/ddoc/_view/map_reduce"), #{
        <<"keys">> => [<<"a">>], <<"group">> => <<"true">>
    }),
    ?assertEqual(Res, Res1),
    ?assertEqual(Code, Code1).

t_view_map_reduce_with_keys(Db) ->
    {Code, Res} = req(get, url(Db, "_design/ddoc/_view/map_reduce?keys=[\"a\",\"b\"]")),
    ?assertMatch(
        #{
            <<"error">> := <<"query_parse_error">>,
            <<"reason">> := <<"Multi-key fetches for reduce views must use `group=true`">>
        },
        Res
    ),
    ?assertEqual(400, Code),
    {Code1, Res1} = req(post, url(Db, "_design/ddoc/_view/map_reduce"), #{
        <<"keys">> => [<<"a">>, <<"b">>]
    }),
    ?assertEqual(Res, Res1),
    ?assertEqual(Code, Code1).

t_view_map_reduce_with_keys_and_group(Db) ->
    {Code, Res} = req(get, url(Db, "_design/ddoc/_view/map_reduce?keys=[\"a\",\"b\"]&group=true")),
    ?assertMatch(
        #{
            <<"rows">> := [
                #{<<"key">> := <<"a">>, <<"value">> := 1},
                #{<<"key">> := <<"b">>, <<"value">> := 2}
            ]
        },
        Res
    ),
    ?assertEqual(200, Code),
    {Code1, Res1} = req(post, url(Db, "_design/ddoc/_view/map_reduce"), #{
        <<"keys">> => [<<"a">>, <<"b">>], <<"group">> => true
    }),
    ?assertEqual(Res, Res1),
    ?assertEqual(Code, Code1).

%%%%%%%%%%%%%%%%%%%% Utility Functions %%%%%%%%%%%%%%%%%%%%
url(Db) ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    lists:concat(["http://", Addr, ":", Port, "/", ?b2l(Db)]).

url(Db, Path) ->
    url(Db) ++ "/" ++ Path.

url(Db, Path, Parameters) ->
    url(Db) ++ "/" ++ Path ++ "?" ++ Parameters.

create_db(Db) ->
    case req(put, url(Db)) of
        {201, #{}} -> ok;
        Error -> error({failed_to_create_test_db, Db, Error})
    end.

create_docs(Db) ->
    case req(post, url(Db) ++ "/_bulk_docs", ?DOCS) of
        {201, _} -> ok;
        Error -> error({failed_to_create_docs, Db, Error})
    end.

create_ddoc(Db) ->
    case req(post, url(Db), ?DDOC) of
        {201, _} -> ok;
        Error -> error({failed_to_create_ddocs, Db, Error})
    end.

req(Method, Url) ->
    Headers = [?CONTENT_JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers),
    {Code, jiffy:decode(Res, [return_maps])}.

req(Method, Url, #{} = Body) ->
    req(Method, Url, jiffy:encode(Body));
req(Method, Url, Body) ->
    Headers = [?CONTENT_JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, jiffy:decode(Res, [return_maps])}.
