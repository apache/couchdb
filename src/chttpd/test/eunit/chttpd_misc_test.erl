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

-module(chttpd_misc_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_misc_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    Url.

teardown(_Url) ->
    meck:unload(),
    Persist = false,
    ok = config:delete("couchdb", "maintenance_mode", Persist),
    ok = config:delete("cluster", "seedlist", Persist),
    ok = config:delete("couchdb", "js_engine", Persist),
    ok = config:delete("admins", ?USER, Persist).

welcome_test_() ->
    {
        "chttpd welcome endpoint tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(should_have_version),
                    ?TDEF_FE(should_have_features),
                    ?TDEF_FE(should_have_uuid)
                ]
            }
        }
    }.

should_have_uuid(Url) ->
    {ok, Status, _, Body} = req_get(Url),
    ?assertEqual(200, Status),
    #{
        <<"couchdb">> := CouchDB,
        <<"uuid">> := Uuid
    } = Body,
    ?assertEqual(<<"Welcome">>, CouchDB),
    RealUuid = couch_server:get_uuid(),
    ?assertEqual(RealUuid, Uuid).

should_have_version(Url) ->
    {ok, Status, _, Body} = req_get(Url),
    ?assertEqual(200, Status),
    #{
        <<"couchdb">> := CouchDB,
        <<"version">> := Version,
        <<"git_sha">> := Sha
    } = Body,
    ?assertNotEqual(Sha, undefined),
    ?assertEqual(<<"Welcome">>, CouchDB),
    RealVersion = list_to_binary(couch_server:get_version()),
    ?assertEqual(RealVersion, Version).

should_have_features(Url) ->
    config:enable_feature(snek),
    {ok, 200, _, Body1} = req_get(Url),
    #{<<"features">> := Features1} = Body1,
    ?assert(is_list(Features1)),
    ?assert(lists:member(<<"snek">>, Features1)),
    config:disable_feature(snek),
    {ok, 200, _, Body2} = req_get(Url),
    #{<<"features">> := Features2} = Body2,
    ?assert(is_list(Features2)),
    ?assertNot(lists:member(<<"snek">>, Features2)),
    config:set("couchdb", "js_engine", "quickjs", false),
    {ok, 200, _, Body3} = req_get(Url),
    #{<<"features">> := Features3} = Body3,
    ?assert(lists:member(<<"quickjs">>, Features3)),
    config:delete("couchdb", "js_engine", false).

up_test_() ->
    {
        "chttpd _up endpoint tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(t_basic),
                    ?TDEF_FE(t_maintenance_mode),
                    ?TDEF_FE(t_nolb_mode),
                    ?TDEF_FE(t_with_seeds)
                ]
            }
        }
    }.

t_basic(Url) ->
    {ok, Code, _, Body} = req_get(Url ++ "/_up"),
    ?assertEqual(200, Code),
    ?assertMatch(#{<<"status">> := <<"ok">>, <<"seeds">> := #{}}, Body).

t_maintenance_mode(Url) ->
    config:set("couchdb", "maintenance_mode", "true", _Persist = false),
    {ok, Code, _, Body} = req_get(Url ++ "/_up"),
    ?assertEqual(404, Code),
    ?assertMatch(#{<<"status">> := <<"maintenance_mode">>}, Body).

t_nolb_mode(Url) ->
    config:set("couchdb", "maintenance_mode", "nolb", _Persist = false),
    {ok, Code, _, Body} = req_get(Url ++ "/_up"),
    ?assertEqual(404, Code),
    ?assertMatch(#{<<"status">> := <<"nolb">>}, Body).

t_with_seeds(Url) ->
    CfgSeeds = "couchdb@node1.example.com,couchdb@node2.example.com",
    config:set("cluster", "seedlist", CfgSeeds, false),
    ok = gen_server:stop(mem3_seeds),
    WaitFun = fun() ->
        case catch mem3_seeds:get_status() of
            {ok, #{}} -> ok;
            _ -> wait
        end
    end,
    test_util:wait(WaitFun),
    {ok, Code, _, Body} = req_get(Url ++ "/_up"),
    ?assertEqual(404, Code),
    ?assertMatch(#{<<"seeds">> := #{}}, Body),
    ?assertMatch(#{<<"status">> := <<"seeding">>}, Body),
    #{<<"seeds">> := Seeds} = Body,
    ?assertMatch(#{<<"couchdb@node1.example.com">> := #{}}, Seeds),
    ?assertMatch(#{<<"couchdb@node2.example.com">> := #{}}, Seeds).

uuid_test_() ->
    {
        "chttpd _uuid endpoint tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(t_one_uuid),
                    ?TDEF_FE(t_multiple_uuids)
                ]
            }
        }
    }.

t_one_uuid(Url) ->
    {ok, Code, _, Body} = req_get(Url ++ "/_uuids"),
    ?assertEqual(200, Code),
    ?assertMatch(#{<<"uuids">> := [_]}, Body),
    #{<<"uuids">> := [Uuid]} = Body,
    ?assertEqual(32, byte_size(Uuid)).

t_multiple_uuids(Url) ->
    {ok, Code, _, Body} = req_get(Url ++ "/_uuids?count=3"),
    ?assertEqual(200, Code),
    ?assertMatch(#{<<"uuids">> := [_, _, _]}, Body),
    #{<<"uuids">> := [Uuid1, Uuid2, Uuid3]} = Body,
    ?assert(Uuid1 /= Uuid2),
    ?assert(Uuid1 /= Uuid3).

-define(MOCKED_CLOUSEAU_VERSION, <<"2.24.0">>).

node_test_() ->
    {
        "chttpd _node/{node_name}/* endpoint tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(t_system),
                    ?TDEF_FE(t_versions_with_clouseau),
                    ?TDEF_FE(t_versions_without_clouseau)
                ]
            }
        }
    }.

t_system(Url) ->
    {ok, Code, _, Body} = req_get(Url ++ "/_node/_local/_system"),
    ?assertEqual(200, Code),
    % The body is quite large, so test a general subset of functionality:
    %   - Metrics from the VM (context_switches), some simple, and some histograms
    %   - Custom computed metrics like internal_replication_jobs
    %   - Simple, histogrammed and aggregated message queues, from the vm/dependencies/couch services
    %
    ?assertMatch(
        #{
            <<"context_switches">> := _,
            <<"distribution">> := #{},
            <<"internal_replication_jobs">> := _,
            <<"memory">> := #{
                <<"binary">> := _,
                <<"processes">> := _
            },
            <<"message_queues">> := #{
                <<"couch_db_updater">> := #{},
                <<"couch_event_server">> := _,
                <<"couch_file">> := #{},
                <<"couch_server">> := _,
                <<"couch_server_1">> := _,
                <<"ibrowse">> := _,
                <<"index_server">> := _,
                <<"index_server_1">> := _,
                <<"init">> := _,
                <<"logger">> := _,
                <<"rexi_buffer">> := _,
                <<"rexi_server">> := _
            }
        },
        Body
    ).

t_versions_with_clouseau(Url) ->
    ok = meck:expect(clouseau_rpc, version, 0, {ok, ?MOCKED_CLOUSEAU_VERSION}),
    {ok, Code, _, Body} = req_get(Url ++ "/_node/_local/_versions"),
    ?assertEqual(200, Code),
    ?assertMatch(
        #{
            <<"javascript_engine">> := #{<<"name">> := _},
            <<"collation_driver">> := #{<<"name">> := <<"libicu">>},
            <<"erlang">> := #{
                <<"supported_hashes">> := [_ | _],
                <<"version">> := _
            },
            <<"clouseau">> := #{
                <<"version">> := ?MOCKED_CLOUSEAU_VERSION
            }
        },
        Body
    ).

t_versions_without_clouseau(Url) ->
    ok = meck:expect(clouseau_rpc, version, 0, {'EXIT', noconnection}),
    {ok, Code, _, Body} = req_get(Url ++ "/_node/_local/_versions"),
    ?assertEqual(200, Code),
    ?assertNot(maps:is_key(<<"search">>, Body)),
    ?assertMatch(
        #{
            <<"javascript_engine">> := #{<<"name">> := _},
            <<"collation_driver">> := #{<<"name">> := <<"libicu">>},
            <<"erlang">> := #{
                <<"supported_hashes">> := [_ | _],
                <<"version">> := _
            }
        },
        Body
    ).

req_get(Url) ->
    {ok, Code, Headers, Body} = test_request:get(Url, [?CONTENT_JSON, ?AUTH]),
    {ok, Code, Headers, jiffy:decode(Body, [return_maps])}.
