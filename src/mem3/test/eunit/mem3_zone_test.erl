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

-module(mem3_zone_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").

mem3_zone_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_empty_zone),
            ?TDEF_FE(t_set_zone_from_env),
            ?TDEF_FE(t_set_zone_when_node_in_seedlist),
            ?TDEF_FE(t_zone_already_set)
        ]
    }.

assertZoneEqual(Expected) ->
    [Node | _] = mem3:nodes(),
    Actual = mem3:node_info(Node, <<"zone">>),
    ?assertEqual(Expected, Actual).

t_empty_zone(_) ->
    ok = application:start(mem3),
    assertZoneEqual(undefined).

t_set_zone_from_env(_) ->
    Zone = "zone1",
    os:putenv("COUCHDB_ZONE", Zone),
    ok = application:start(mem3),
    assertZoneEqual(iolist_to_binary(Zone)).

t_set_zone_when_node_in_seedlist(_) ->
    CfgSeeds = "nonode@nohost",
    config:set("cluster", "seedlist", CfgSeeds, false),
    Zone = "zone1",
    os:putenv("COUCHDB_ZONE", Zone),
    ok = application:start(mem3),
    assertZoneEqual(iolist_to_binary(Zone)).

t_zone_already_set(_) ->
    Zone = "zone1",
    os:putenv("COUCHDB_ZONE", Zone),
    ok = application:start(mem3),
    application:stop(mem3),
    ok = application:start(mem3),
    assertZoneEqual(iolist_to_binary(Zone)).

setup() ->
    meck:new(mem3_seeds, [passthrough]),
    meck:new(mem3_rpc, [passthrough]),
    test_util:start_couch([rexi]).

teardown(Ctx) ->
    catch application:stop(mem3),
    os:unsetenv("COUCHDB_ZONE"),
    Filename = config:get("mem3", "nodes_db", "_nodes") ++ ".couch",
    file:delete(filename:join([?BUILDDIR(), "tmp", "data", Filename])),
    case config:get("couch_httpd_auth", "authentication_db") of
        undefined -> ok;
        DbName -> couch_server:delete(list_to_binary(DbName), [])
    end,
    meck:unload(),
    test_util:stop_couch(Ctx).
