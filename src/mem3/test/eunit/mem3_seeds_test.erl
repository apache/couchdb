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

-module(mem3_seeds_test).

-include_lib("couch/include/couch_eunit.hrl").

-define(FAKE_NODES, ['couchdb@node1.example.com', 'couchdb@node2.example.com']).

mem3_seeds_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_empty_seedlist_status_ok),
            ?TDEF_FE(t_seedlist_misconfiguration),
            ?TDEF_FE(t_seedlist_replicate_all),
            ?TDEF_FE(t_seedlist_replicate_partial),
            ?TDEF_FE(t_check_nodelist),
            ?TDEF_FE(t_check_local_dbs)
        ]
    }.

t_empty_seedlist_status_ok(_) ->
    ok = application:start(mem3),
    {ok, Result = #{}} = mem3_seeds:get_status(),
    ?assertMatch(#{seeds := _, status := ok}, Result).

t_seedlist_misconfiguration(_) ->
    CfgSeeds = string:join([atom_to_list(N) || N <- ?FAKE_NODES], ","),
    config:set("cluster", "seedlist", CfgSeeds, false),
    ok = application:start(mem3),
    {ok, Result = #{seeds := Seeds}} = mem3_seeds:get_status(),
    ?assertEqual(2, map_size(Seeds)),
    ?assertMatch(#{'couchdb@node1.example.com' := _}, Seeds),
    ?assertMatch(#{'couchdb@node2.example.com' := _}, Seeds),
    ?assertMatch(#{status := seeding}, Result).

t_seedlist_replicate_all(_) ->
    CfgSeeds = string:join([atom_to_list(N) || N <- ?FAKE_NODES], ","),
    config:set("cluster", "seedlist", CfgSeeds, false),
    mock_rep(?FAKE_NODES, {ok, 0}),
    meck:reset(mem3_seeds),
    ok = application:start(mem3),
    meck:wait(mem3_seeds, handle_info, [{'DOWN', '_', '_', '_', '_'}, '_'], 4000),
    {ok, Result = #{seeds := Seeds}} = mem3_seeds:get_status(),
    ?assertEqual(2, map_size(Seeds)),
    ?assertMatch(#{'couchdb@node1.example.com' := _}, Seeds),
    ?assertMatch(#{'couchdb@node2.example.com' := _}, Seeds),
    ?assertMatch(#{status := ok}, Result).

t_seedlist_replicate_partial(_) ->
    CfgSeeds = string:join([atom_to_list(N) || N <- ?FAKE_NODES], ","),
    config:set("cluster", "seedlist", CfgSeeds, false),
    mock_rep(?FAKE_NODES, {ok, 42}),
    meck:reset(mem3_seeds),
    ok = application:start(mem3),
    meck:wait(mem3_seeds, handle_info, [{'DOWN', '_', '_', '_', '_'}, '_'], 4000),
    {ok, Result = #{seeds := Seeds}} = mem3_seeds:get_status(),
    ?assertEqual(2, map_size(Seeds)),
    ?assertMatch(#{'couchdb@node1.example.com' := _}, Seeds),
    ?assertMatch(#{'couchdb@node2.example.com' := _}, Seeds),
    ?assertMatch(#{status := seeding}, Result).

t_check_nodelist(_) ->
    CfgSeeds = string:join([atom_to_list(N) || N <- ?FAKE_NODES], ","),
    config:set("cluster", "seedlist", CfgSeeds, false),
    ok = application:start(mem3),
    Nodes = mem3:nodes(),
    ?assert(lists:member('couchdb@node1.example.com', Nodes)),
    ?assert(lists:member('couchdb@node2.example.com', Nodes)).

t_check_local_dbs(_) ->
    LocalDbs = mem3_sync:local_dbs(),
    {ok, _} = couch_server:create(<<"_users">>, []),
    ?assertEqual(
        lists:append(LocalDbs, [<<"_users">>]),
        mem3_sync:local_dbs()
    ).

mock_rep(Nodes, Result) ->
    ResultDbs = [{DbName, Result} || DbName <- mem3_sync:local_dbs()],
    meck:expect(mem3_rpc, pull_replication, fun(Node) ->
        case lists:member(Node, Nodes) of
            true -> ResultDbs;
            false -> meck:passthrough()
        end
    end).

setup() ->
    meck:new(mem3_seeds, [passthrough]),
    meck:new(mem3_rpc, [passthrough]),
    test_util:start_couch([rexi]).

teardown(Ctx) ->
    catch application:stop(mem3),
    config:delete("cluster", "seedlist", false),
    Filename = config:get("mem3", "nodes_db", "_nodes") ++ ".couch",
    file:delete(filename:join([?BUILDDIR(), "tmp", "data", Filename])),
    case config:get("couch_httpd_auth", "authentication_db") of
        undefined -> ok;
        DbName -> couch_server:delete(list_to_binary(DbName), [])
    end,
    meck:unload(),
    test_util:stop_couch(Ctx).
