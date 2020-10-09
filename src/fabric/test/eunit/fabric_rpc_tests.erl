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

-module(fabric_rpc_tests).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(TDEF(A), {A, fun A/1}).


main_test_() ->
    {
        setup,
        spawn,
        fun setup_all/0,
        fun teardown_all/1,
        [
            {
                foreach,
                fun setup_no_db_or_config/0,
                fun teardown_db/1,
                lists:map(fun wrap/1, [
                    ?TDEF(t_no_config_non_shard_db_create_succeeds)
                ])
            },
            {
                foreach,
                fun setup_shard/0,
                fun teardown_noop/1,
                lists:map(fun wrap/1, [
                    ?TDEF(t_no_db),
                    ?TDEF(t_no_config_db_create_fails_for_shard),
                    ?TDEF(t_no_config_db_create_fails_for_shard_rpc)
                ])
            },
            {
                foreach,
                fun setup_shard/0,
                fun teardown_db/1,
                lists:map(fun wrap/1, [
                    ?TDEF(t_db_create_with_config)
                ])
            }

        ]
    }.


setup_all() ->
    test_util:start_couch([rexi, mem3, fabric]).


teardown_all(Ctx) ->
    test_util:stop_couch(Ctx).


setup_no_db_or_config() ->
    ?tempdb().


setup_shard() ->
    ?tempshard().


teardown_noop(_DbName) ->
    ok.

teardown_db(DbName) ->
    ok = couch_server:delete(DbName, []).


wrap({Name, Fun}) ->
    fun(Arg) ->
        {timeout, 60, {atom_to_list(Name), fun() ->
            process_flag(trap_exit, true),
            Fun(Arg)
        end}}
    end.


t_no_db(DbName) ->
    ?assertEqual({not_found, no_db_file}, couch_db:open_int(DbName, [?ADMIN_CTX])).


t_no_config_non_shard_db_create_succeeds(DbName) ->
    ?assertEqual({not_found, no_db_file}, couch_db:open_int(DbName, [?ADMIN_CTX])),
    ?assertEqual(DbName, mem3:dbname(DbName)),
    ?assertMatch({ok, _}, mem3_util:get_or_create_db(DbName, [?ADMIN_CTX])).


t_no_config_db_create_fails_for_shard(DbName) ->
    ?assertEqual({not_found, no_db_file}, couch_db:open_int(DbName, [?ADMIN_CTX])),
    ?assertException(throw, {error, missing_target}, mem3_util:get_or_create_db(DbName, [?ADMIN_CTX])).


t_no_config_db_create_fails_for_shard_rpc(DbName) ->
    ?assertEqual({not_found, no_db_file}, couch_db:open_int(DbName, [?ADMIN_CTX])),
    ?assertException(throw, {error, missing_target}, mem3_util:get_or_create_db(DbName, [?ADMIN_CTX])),
    MFA = {fabric_rpc, get_db_info, [DbName]},
    Ref = rexi:cast(node(), self(), MFA),
    Resp = receive
        Resp0 -> Resp0
    end,
    ?assertMatch({Ref, {'rexi_EXIT', {{error, missing_target}, _}}}, Resp).


t_db_create_with_config(DbName) ->
    MDbName = mem3:dbname(DbName),
    DbDoc = #doc{id = MDbName, body = test_db_doc()},

    ?assertEqual({not_found, no_db_file}, couch_db:open_int(DbName, [?ADMIN_CTX])),

    %% Write the dbs db config
    couch_util:with_db(mem3_sync:shards_db(), fun(Db) ->
        ?assertEqual({not_found, missing}, couch_db:open_doc(Db, MDbName, [ejson_body])),
        ?assertMatch({ok, _}, couch_db:update_docs(Db, [DbDoc]))
    end),

    %% Test get_or_create_db loads the properties as expected
    couch_util:with_db(mem3_sync:shards_db(), fun(Db) ->
        ?assertMatch({ok, _}, couch_db:open_doc(Db, MDbName, [ejson_body])),
        ?assertEqual({not_found, no_db_file}, couch_db:open_int(DbName, [?ADMIN_CTX])),
        Resp = mem3_util:get_or_create_db(DbName, [?ADMIN_CTX]),
        ?assertMatch({ok, _}, Resp),
        {ok, LDb} = Resp,

        {Body} = test_db_doc(),
        DbProps = mem3_util:get_shard_opts(Body),
        {Props} = case couch_db_engine:get_props(LDb) of
            undefined -> {[]};
            Else -> {Else}
        end,
        %% We don't normally store the default engine name
        EngineProps = case couch_db_engine:get_engine(LDb) of
            couch_bt_engine ->
                [];
            EngineName ->
                [{engine, EngineName}]
        end,
        ?assertEqual([{props, Props} | EngineProps], DbProps)
    end).


test_db_doc() ->
    {[
        {<<"shard_suffix">>, ".1584997648"},
        {<<"changelog">>, [
            [<<"add">>, <<"00000000-7fffffff">>, <<"node1@127.0.0.1">>],
            [<<"add">>, <<"00000000-7fffffff">>, <<"node2@127.0.0.1">>],
            [<<"add">>, <<"00000000-7fffffff">>, <<"node3@127.0.0.1">>],
            [<<"add">>, <<"80000000-ffffffff">>, <<"node1@127.0.0.1">>],
            [<<"add">>, <<"80000000-ffffffff">>, <<"node2@127.0.0.1">>],
            [<<"add">>, <<"80000000-ffffffff">>, <<"node3@127.0.0.1">>]
        ]},
        {<<"by_node">>, {[
            {<<"node1@127.0.0.1">>, [<<"00000000-7fffffff">>, <<"80000000-ffffffff">>]},
            {<<"node2@127.0.0.1">>, [<<"00000000-7fffffff">>, <<"80000000-ffffffff">>]},
            {<<"node3@127.0.0.1">>, [<<"00000000-7fffffff">>, <<"80000000-ffffffff">>]}
        ]}},
        {<<"by_range">>, {[
            {<<"00000000-7fffffff">>, [<<"node1@127.0.0.1">>, <<"node2@127.0.0.1">>, <<"node3@127.0.0.1">>]},
            {<<"80000000-ffffffff">>, [<<"node1@127.0.0.1">>, <<"node2@127.0.0.1">>, <<"node3@127.0.0.1">>]}
        ]}},
        {<<"props">>, {[
            {partitioned, true},
            {hash, [couch_partition, hash, []]}
        ]}}
    ]}.

