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

-module(fabric_meta_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

fabric_meta_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        with([
            ?TDEF(t_set_revs_limit),
            ?TDEF(t_set_purge_revs_limit),
            ?TDEF(t_update_props),
            ?TDEF(t_security)
        ])
    }.

setup() ->
    test_util:start_couch([fabric]).

teardown(Ctx) ->
    test_util:stop_couch(Ctx).

t_update_props(_) ->
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, 2}, {n, 1}]),

    {ok, Info} = fabric:get_db_info(DbName),
    Props = couch_util:get_value(props, Info),
    ?assertEqual({[]}, Props),

    ?assertEqual(ok, fabric:update_props(DbName, <<"foo">>, 100)),
    {ok, Info1} = fabric:get_db_info(DbName),
    Props1 = couch_util:get_value(props, Info1),
    % 200 because q=2 and we're using get_db_info which sums
    % the info object integers when merging them
    ?assertEqual({[{<<"foo">>, 200}]}, Props1),

    ?assertEqual(ok, fabric:update_props(DbName, bar, 101)),
    {ok, Info2} = fabric:get_db_info(DbName),
    Props2 = couch_util:get_value(props, Info2),
    ?assertEqual(
        {[
            {<<"foo">>, 200},
            {bar, 202}
        ]},
        Props2
    ),

    ?assertEqual(ok, fabric:update_props(DbName, <<"foo">>, undefined)),
    {ok, Info3} = fabric:get_db_info(DbName),
    ?assertEqual({[{bar, 202}]}, couch_util:get_value(props, Info3)),

    Res = fabric:update_props(DbName, partitioned, true),
    ?assertMatch({error, {bad_request, _}}, Res),
    {ok, Info4} = fabric:get_db_info(DbName),
    ?assertEqual({[{bar, 202}]}, couch_util:get_value(props, Info4)),

    ok = fabric:delete_db(DbName, []).

t_set_revs_limit(_) ->
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, 2}, {n, 1}]),
    ?assertEqual(ok, fabric:set_revs_limit(DbName, 3, [?ADMIN_CTX])),
    Check = fun(Db) ->
        ?assertEqual(3, couch_db:get_revs_limit(Db))
    end,
    [check_shard(S, Check) || S <- mem3:shards(DbName)].

t_set_purge_revs_limit(_) ->
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, 2}, {n, 1}]),
    ?assertEqual(ok, fabric:set_purge_infos_limit(DbName, 3, [?ADMIN_CTX])),
    Check = fun(Db) ->
        ?assertEqual(3, couch_db:get_purge_infos_limit(Db))
    end,
    [check_shard(S, Check) || S <- mem3:shards(DbName)].

t_security(_) ->
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, 2}, {n, 1}]),
    SecObj = #{
        <<"admins">> => #{<<"names">> => [<<"n1">>], <<"roles">> => [<<"r1">>]},
        <<"members">> => #{<<"names">> => [<<"n2">>], <<"roles">> => [<<"r2">>]}
    },
    Ejson = ?JSON_DECODE(?JSON_ENCODE(SecObj)),
    ?assertEqual(ok, fabric:set_security(DbName, Ejson)),
    Check = fun(Db) ->
        ?assertEqual(SecObj, couch_util:ejson_to_map(couch_db:get_security(Db)))
    end,
    [check_shard(S, Check) || S <- mem3:shards(DbName)],

    AllSec = fabric:get_all_security(DbName),
    ?assertMatch({ok, _}, AllSec),
    {ok, ShardSec} = AllSec,
    ?assert(is_list(ShardSec)),
    ?assertEqual(2, length(ShardSec)),
    % ShardSec result is [{#shard{}, SecObj}, ...]
    {_, SecObjs} = lists:unzip(ShardSec),
    [?assertEqual(SecObj, couch_util:ejson_to_map(O)) || O <- SecObjs].

check_shard(Shard, Check) ->
    Name = mem3:name(Shard),
    couch_util:with_db(Name, Check).
