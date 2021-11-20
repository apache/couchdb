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

a_test_() ->
    Tests = [
        {"empty seedlist should set status ok", fun empty_seedlist_status_ok/0},
        {"all seedlist nodes unreachable keeps status seeding", fun seedlist_misconfiguration/0},
        {"seedlist entries should be present in _nodes", fun check_nodelist/0},
        {"optional local _users db in mem3_sync:local_dbs()", fun check_local_dbs/0}
    ],
    {setup, fun setup/0, fun teardown/1, Tests}.

empty_seedlist_status_ok() ->
    ok = application:start(mem3),
    try
        {ok, {Result}} = mem3_seeds:get_status(),
        ?assertEqual({[]}, couch_util:get_value(seeds, Result)),
        ?assertEqual(ok, couch_util:get_value(status, Result))
    after
        cleanup()
    end.

seedlist_misconfiguration() ->
    config:set("cluster", "seedlist", "couchdb@node1.example.com,couchdb@node2.example.com", false),
    ok = application:start(mem3),
    try
        {ok, {Result}} = mem3_seeds:get_status(),
        {Seeds} = couch_util:get_value(seeds, Result),
        ?assertEqual(2, length(Seeds)),
        ?assertMatch({_}, couch_util:get_value('couchdb@node1.example.com', Seeds)),
        ?assertMatch({_}, couch_util:get_value('couchdb@node2.example.com', Seeds)),
        ?assertEqual(seeding, couch_util:get_value(status, Result))
    after
        cleanup()
    end.

check_nodelist() ->
    config:set("cluster", "seedlist", "couchdb@node1.example.com,couchdb@node2.example.com", false),
    ok = application:start(mem3),
    try
        Nodes = mem3:nodes(),
        ?assert(lists:member('couchdb@node1.example.com', Nodes)),
        ?assert(lists:member('couchdb@node2.example.com', Nodes))
    after
        cleanup()
    end.

check_local_dbs() ->
    LocalDbs = mem3_sync:local_dbs(),
    {ok, _} = couch_server:create(<<"_users">>, []),
    ?assertEqual(
        lists:append(LocalDbs, [<<"_users">>]),
        mem3_sync:local_dbs()
    ).

cleanup() ->
    application:stop(mem3),
    Filename = config:get("mem3", "nodes_db", "_nodes") ++ ".couch",
    file:delete(filename:join([?BUILDDIR(), "tmp", "data", Filename])),
    case config:get("couch_httpd_auth", "authentication_db") of
        undefined -> ok;
        DbName -> couch_server:delete(list_to_binary(DbName), [])
    end.

setup() ->
    test_util:start_couch([rexi]).

teardown(Ctx) ->
    test_util:stop_couch(Ctx).
