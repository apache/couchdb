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

-module(chttpd_local_docs_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_local_docs_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(JSON, {"Content-Type", "application/json"}).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Db = binary_to_list(?tempdb()),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    ok = create_db(Url, Db),
    {Url, Db}.

teardown({Url, Db}) ->
    delete_db(Url, Db),
    ok = config:delete("admins", ?USER, _Persist = false).

start_couch() ->
    test_util:start_couch([chttpd]).

stop_couch(Ctx) ->
    test_util:stop_couch(Ctx).

chttpd_local_docs_test_() ->
    {
        "chttpd _local_docs tests",
        {
            setup,
            fun start_couch/0,
            fun stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(t_local_docs_doesnt_include_system_docs),
                    ?TDEF_FE(t_local_docs_does_include_system_docs_w_param)
                ]
            }
        }
    }.

t_local_docs_doesnt_include_system_docs({Top, Db}) ->
    Path = Top ++ Db,
    Docs = [
        #{
            <<"_id">> => <<"_local/shard-sync-foo-bar">>,
            <<"_rev">> => <<"0-1">>,
            <<"seq">> => 1,
            <<"target_uuid">> => <<"4f3eb3181db904d62176fb81626f84d0">>
        },
        #{
            <<"_id">> => <<"_local/purge-mrview-foo-bar">>,
            <<"_rev">> => <<"0-1">>,
            <<"ddoc_id">> => <<"_design/ddoc1">>,
            <<"purge_seq">> => 0,
            <<"signature">> => [1, 2, 3],
            <<"type">> => <<"mrview">>,
            <<"updated_on">> => 1676995502
        },
        #{
            <<"_id">> => <<"_local/1-foo-bar">>
        },
        #{
            <<"_id">> => <<"_local/2-foo-bar">>
        }
    ],
    Body = #{<<"docs">> => Docs},
    {Code, _} = req(post, Path ++ "/_bulk_docs", Body),
    ?assertEqual(201, Code),
    {Code1, Res} = req(get, Path ++ "/_local_docs"),
    ?assertEqual(200, Code1),
    ?assertEqual(
        #{
            <<"total_rows">> => null,
            <<"offset">> => null,
            <<"rows">> => [
                #{
                    <<"id">> => <<"_local/1-foo-bar">>,
                    <<"key">> => <<"_local/1-foo-bar">>,
                    <<"value">> => #{<<"rev">> => <<"0-1">>}
                },
                #{
                    <<"id">> => <<"_local/2-foo-bar">>,
                    <<"key">> => <<"_local/2-foo-bar">>,
                    <<"value">> => #{<<"rev">> => <<"0-1">>}
                }
            ]
        },
        Res
    ).

t_local_docs_does_include_system_docs_w_param({Top, Db}) ->
    Path = Top ++ Db,
    Docs = [
        #{
            <<"_id">> => <<"_local/shard-sync-foo-bar">>,
            <<"_rev">> => <<"0-1">>,
            <<"seq">> => 1,
            <<"target_uuid">> => <<"4f3eb3181db904d62176fb81626f84d0">>
        },
        #{
            <<"_id">> => <<"_local/purge-mrview-foo-bar">>,
            <<"_rev">> => <<"0-1">>,
            <<"ddoc_id">> => <<"_design/ddoc1">>,
            <<"purge_seq">> => 0,
            <<"signature">> => [1, 2, 3],
            <<"type">> => <<"mrview">>,
            <<"updated_on">> => 1676995502
        },
        #{
            <<"_id">> => <<"_local/1-foo-bar">>
        },
        #{
            <<"_id">> => <<"_local/2-foo-bar">>
        }
    ],
    Body = #{<<"docs">> => Docs},
    {Code, _} = req(post, Path ++ "/_bulk_docs", Body),
    ?assertEqual(201, Code),
    {Code1, Res} = req(get, Path ++ "/_local_docs?include_system=true"),
    ?assertEqual(200, Code1),
    ?assertEqual(
        #{
            <<"total_rows">> => null,
            <<"offset">> => null,
            <<"rows">> => [
                #{
                    <<"id">> => <<"_local/1-foo-bar">>,
                    <<"key">> => <<"_local/1-foo-bar">>,
                    <<"value">> => #{<<"rev">> => <<"0-1">>}
                },
                #{
                    <<"id">> => <<"_local/2-foo-bar">>,
                    <<"key">> => <<"_local/2-foo-bar">>,
                    <<"value">> => #{<<"rev">> => <<"0-1">>}
                },
                #{
                    <<"id">> => <<"_local/purge-mrview-foo-bar">>,
                    <<"key">> => <<"_local/purge-mrview-foo-bar">>,
                    <<"value">> => #{<<"rev">> => <<"0-2">>}
                },
                #{
                    <<"id">> => <<"_local/shard-sync-foo-bar">>,
                    <<"key">> => <<"_local/shard-sync-foo-bar">>,
                    <<"value">> => #{<<"rev">> => <<"0-2">>}
                }
            ]
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
