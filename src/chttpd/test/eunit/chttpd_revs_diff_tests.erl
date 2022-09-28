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

-module(chttpd_revs_diff_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_revs_diff_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(JSON, {"Content-Type", "application/json"}).

-define(DOC1, <<"doc1">>).
-define(DOC2, <<"doc2">>).
-define(REVA, <<"reva">>).
-define(REVB, <<"revb">>).
-define(REVC, <<"revc">>).
-define(REVD, <<"revd">>).

test_docs() ->
    [
        {?DOC1, [?REVB, ?REVA]},
        {?DOC1, [?REVC, ?REVA]},
        {?DOC2, [?REVD]}
    ].

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Db = binary_to_list(?tempdb()),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    ok = create_db(Url, Db),
    ok = create_docs(Url, Db, test_docs()),
    {Url, Db}.

teardown({Url, Db}) ->
    delete_db(Url, Db),
    ok = config:delete("admins", ?USER, _Persist = false).

start_couch() ->
    test_util:start_couch([chttpd]).

stop_couch(Ctx) ->
    test_util:stop_couch(Ctx).

chttpd_revs_diff_test_() ->
    {
        "chttpd _revs_diff tests",
        {
            setup,
            fun start_couch/0,
            fun stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(t_empty_revs_diff),
                    ?TDEF_FE(t_revs_diff_no_revs),
                    ?TDEF_FE(t_revs_diff_non_existent_doc),
                    ?TDEF_FE(t_revs_diff_all_revs),
                    ?TDEF_FE(t_revs_diff_some_missing_some_not),
                    ?TDEF_FE(t_empty_missing_revs),
                    ?TDEF_FE(t_missing_revs_no_revs),
                    ?TDEF_FE(t_missing_revs_non_existent_doc),
                    ?TDEF_FE(t_missing_revs_all_revs),
                    ?TDEF_FE(t_missing_revs_some_missing_some_not)
                ]
            }
        }
    }.

t_empty_revs_diff({Top, Db}) ->
    {Code, Res} = req(post, Top ++ Db ++ "/_revs_diff", #{}),
    ?assertEqual(200, Code),
    ?assertEqual(#{}, Res).

t_revs_diff_no_revs({Top, Db}) ->
    Body = #{?DOC1 => [], <<"non_existent_doc">> => []},
    {Code, Res} = req(post, Top ++ Db ++ "/_revs_diff", Body),
    ?assertEqual(200, Code),
    ?assertEqual(#{}, Res).

t_revs_diff_non_existent_doc({Top, Db}) ->
    Body = #{<<"non_existent_doc">> => [<<"1-rev">>]},
    {Code, Res} = req(post, Top ++ Db ++ "/_revs_diff", Body),
    ?assertEqual(200, Code),
    ?assertEqual(
        #{
            <<"non_existent_doc">> => #{
                <<"missing">> => [<<"1-rev">>]
            }
        },
        Res
    ).

t_revs_diff_all_revs({Top, Db}) ->
    Body = #{
        ?DOC1 => [<<"2-", ?REVB/binary>>, <<"2-", ?REVC/binary>>],
        ?DOC2 => [<<"1-", ?REVD/binary>>]
    },
    {Code, Res} = req(post, Top ++ Db ++ "/_revs_diff", Body),
    ?assertEqual(200, Code),
    ?assertEqual(#{}, Res).

t_revs_diff_some_missing_some_not({Top, Db}) ->
    Body = #{
        ?DOC1 => [<<"2-", ?REVB/binary>>, <<"1-xyz">>, <<"2-def">>, <<"3-klm">>],
        ?DOC2 => [<<"1-pqr">>]
    },
    {Code, Res} = req(post, Top ++ Db ++ "/_revs_diff", Body),
    ?assertEqual(200, Code),

    ?assert(maps:is_key(?DOC1, Res)),
    ?assert(maps:is_key(?DOC2, Res)),
    #{?DOC1 := Doc1, ?DOC2 := Doc2} = Res,

    ?assert(maps:is_key(<<"missing">>, Doc1)),
    ?assert(maps:is_key(<<"missing">>, Doc2)),
    #{<<"missing">> := Missing1} = Doc1,
    #{<<"missing">> := Missing2} = Doc2,
    ?assertEqual([<<"1-xyz">>, <<"2-def">>, <<"3-klm">>], lists:sort(Missing1)),
    ?assertEqual([<<"1-pqr">>], Missing2),

    ?assert(maps:is_key(<<"possible_ancestors">>, Doc1)),
    ?assert(not maps:is_key(<<"possible_ancestors">>, Doc2)),

    #{<<"possible_ancestors">> := PAs1} = Doc1,
    ?assertEqual([<<"2-revb">>, <<"2-revc">>], lists:sort(PAs1)).

t_empty_missing_revs({Top, Db}) ->
    {Code, Res} = req(post, Top ++ Db ++ "/_missing_revs", #{}),
    ?assertEqual(200, Code),
    ?assertEqual(#{<<"missing_revs">> => #{}}, Res).

t_missing_revs_no_revs({Top, Db}) ->
    Body = #{?DOC1 => [], <<"non_existent_doc">> => []},
    {Code, Res} = req(post, Top ++ Db ++ "/_missing_revs", Body),
    ?assertEqual(200, Code),
    ?assertEqual(#{<<"missing_revs">> => #{}}, Res).

t_missing_revs_non_existent_doc({Top, Db}) ->
    Body = #{<<"non_existent_doc">> => [<<"1-rev">>]},
    {Code, Res} = req(post, Top ++ Db ++ "/_missing_revs", Body),
    ?assertEqual(200, Code),
    ?assertEqual(
        #{
            <<"missing_revs">> => #{
                <<"non_existent_doc">> => [<<"1-rev">>]
            }
        },
        Res
    ).

t_missing_revs_all_revs({Top, Db}) ->
    Body = #{
        ?DOC1 => [<<"2-", ?REVB/binary>>, <<"2-", ?REVC/binary>>],
        ?DOC2 => [<<"1-", ?REVD/binary>>]
    },
    {Code, Res} = req(post, Top ++ Db ++ "/_missing_revs", Body),
    ?assertEqual(200, Code),
    ?assertEqual(#{<<"missing_revs">> => #{}}, Res).

t_missing_revs_some_missing_some_not({Top, Db}) ->
    Body = #{
        ?DOC1 => [<<"2-", ?REVB/binary>>, <<"1-xyz">>, <<"2-def">>, <<"3-klm">>],
        ?DOC2 => [<<"1-pqr">>]
    },
    {Code, Res} = req(post, Top ++ Db ++ "/_missing_revs", Body),
    ?assertEqual(200, Code),
    ?assertEqual(
        #{
            <<"missing_revs">> => #{
                ?DOC1 => [<<"1-xyz">>, <<"2-def">>, <<"3-klm">>],
                ?DOC2 => [<<"1-pqr">>]
            }
        },
        Res
    ).

create_db(Top, Db) ->
    case req(put, Top ++ Db) of
        {201, #{}} ->
            ok;
        Error ->
            error({failed_to_create_test_db, Db, Error})
    end.

delete_db(Top, Db) ->
    case req(delete, Top ++ Db) of
        {200, #{}} ->
            ok;
        Error ->
            error({failed_to_delete_test_db, Db, Error})
    end.

create_docs(Top, Db, DocRevs) ->
    Docs = lists:map(
        fun({Id, Revs}) ->
            #{
                <<"_id">> => Id,
                <<"_revisions">> => #{
                    <<"ids">> => Revs,
                    <<"start">> => length(Revs)
                }
            }
        end,
        DocRevs
    ),
    Body = #{
        <<"docs">> => Docs,
        <<"new_edits">> => false
    },
    {Code, Res} = req(post, Top ++ Db ++ "/_bulk_docs", Body),
    ?assertEqual(201, Code),
    ?assertEqual([], Res),
    ok.

req(Method, Url) ->
    Headers = [?JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers),
    {Code, jiffy:decode(Res, [return_maps])}.

req(Method, Url, #{} = Body) ->
    req(Method, Url, jiffy:encode(Body));
req(Method, Url, Body) ->
    Headers = [?JSON, ?AUTH],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, jiffy:decode(Res, [return_maps])}.
