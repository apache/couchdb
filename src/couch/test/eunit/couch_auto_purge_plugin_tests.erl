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

-module(couch_auto_purge_plugin_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(PLUGIN, couch_auto_purge_plugin).

couch_quickjs_scanner_plugin_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_no_auto_purge_by_default, 10),
            ?TDEF_FE(t_auto_purge_after_config_ttl, 10),
            ?TDEF_FE(t_auto_purge_after_db_ttl, 10),
            ?TDEF_FE(t_min_batch_size_1, 10),
            ?TDEF_FE(t_min_batch_size_2, 10),
            ?TDEF_FE(t_max_batch_size_1, 10),
            ?TDEF_FE(t_max_batch_size_2, 10)
        ]
    }.

setup() ->
    {module, _} = code:ensure_loaded(?PLUGIN),
    meck:new(?PLUGIN, [passthrough]),
    meck:new(couch_scanner_server, [passthrough]),
    meck:new(couch_scanner_util, [passthrough]),
    Ctx = test_util:start_couch([fabric, couch_scanner]),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, "2"}, {n, "1"}]),
    config:set(atom_to_list(?PLUGIN), "max_batch_items", "1", false),
    reset_stats(),
    {Ctx, DbName}.

teardown({Ctx, DbName}) ->
    config_delete_section("couch_scanner"),
    config_delete_section("couch_scanner_plugins"),
    config_delete_section(atom_to_list(?PLUGIN)),
    couch_scanner:reset_checkpoints(),
    couch_scanner:resume(),
    fabric:delete_db(DbName),
    test_util:stop_couch(Ctx),
    meck:unload().

t_no_auto_purge_by_default({_, DbName}) ->
    ok = add_doc(DbName, <<"doc1">>, #{<<"_deleted">> => true}),
    ?assertEqual(1, doc_del_count(DbName)),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, doc_del_count(DbName)),
    ok.

t_auto_purge_after_config_ttl({_, DbName}) ->
    config:set(atom_to_list(?PLUGIN), "deleted_document_ttl", "-3_hour", false),
    ok = add_doc(DbName, <<"doc1">>, #{<<"_deleted">> => true}),
    ?assertEqual(1, doc_del_count(DbName)),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(0, doc_del_count(DbName)),
    ok.

t_auto_purge_after_db_ttl({_, DbName}) ->
    ok = fabric:set_auto_purge_props(DbName, [{<<"deleted_document_ttl">>, "-3_hour"}]),
    ok = add_doc(DbName, <<"doc1">>, #{<<"_deleted">> => true}),
    ?assertEqual(1, doc_del_count(DbName)),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(0, doc_del_count(DbName)),
    ok.

t_min_batch_size_1({_, DbName}) ->
    meck:new(fabric, [passthrough]),
    config:set_integer(atom_to_list(?PLUGIN), "min_batch_size", 5),
    ok = fabric:set_auto_purge_props(DbName, [{<<"deleted_document_ttl">>, "-3_hour"}]),
    [
        add_doc(DbName, <<"doc", (integer_to_binary(I))/binary>>, #{<<"_deleted">> => true})
     || I <- lists:seq(1, 10)
    ],
    ?assertEqual(10, doc_del_count(DbName)),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(2, meck:num_calls(fabric, purge_docs, '_')),
    ?assertEqual(0, doc_del_count(DbName)),
    ok.

t_min_batch_size_2({_, DbName}) ->
    meck:new(fabric, [passthrough]),
    config:set_integer(atom_to_list(?PLUGIN), "min_batch_size", 5),
    ok = fabric:set_auto_purge_props(DbName, [{<<"deleted_document_ttl">>, "-3_hour"}]),
    [
        add_doc(DbName, <<"doc", (integer_to_binary(I))/binary>>, #{<<"_deleted">> => true})
     || I <- lists:seq(1, 11)
    ],
    ?assertEqual(11, doc_del_count(DbName)),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(3, meck:num_calls(fabric, purge_docs, '_')),
    ?assertEqual(0, doc_del_count(DbName)),
    ok.

t_max_batch_size_1({_, DbName}) ->
    meck:new(fabric, [passthrough]),
    config:set_integer(atom_to_list(?PLUGIN), "min_batch_size", 1),
    config:set_integer(atom_to_list(?PLUGIN), "max_batch_size", 5),
    ok = fabric:set_auto_purge_props(DbName, [{<<"deleted_document_ttl">>, "-3_hour"}]),
    [
        add_replicated_doc(
            DbName,
            <<"doc">>,
            #{
                <<"_rev">> => <<"1-", (couch_uuids:random())/binary>>,
                <<"foo">> => I,
                <<"_deleted">> => true
            }
        )
     || I <- lists:seq(1, 10)
    ],
    ?assertEqual(1, doc_del_count(DbName)),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(2, meck:num_calls(fabric, purge_docs, '_')),
    ?assertEqual(0, doc_del_count(DbName)),
    ok.

t_max_batch_size_2({_, DbName}) ->
    meck:new(fabric, [passthrough]),
    config:set_integer(atom_to_list(?PLUGIN), "min_batch_size", 1),
    config:set_integer(atom_to_list(?PLUGIN), "max_batch_size", 5),
    ok = fabric:set_auto_purge_props(DbName, [{<<"deleted_document_ttl">>, "-3_hour"}]),
    [
        add_replicated_doc(
            DbName,
            <<"doc">>,
            #{
                <<"_rev">> => <<"1-", (couch_uuids:random())/binary>>,
                <<"foo">> => I,
                <<"_deleted">> => true
            }
        )
     || I <- lists:seq(1, 11)
    ],
    ?assertEqual(1, doc_del_count(DbName)),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(3, meck:num_calls(fabric, purge_docs, '_')),
    ?assertEqual(0, doc_del_count(DbName)),
    ok.

reset_stats() ->
    Counters = [
        [couchdb, query_server, process_error_exits],
        [couchdb, query_server, process_errors],
        [couchdb, query_server, process_exits]
    ],
    [reset_counter(C) || C <- Counters].

reset_counter(Counter) ->
    case couch_stats:sample(Counter) of
        0 ->
            ok;
        N when is_integer(N), N > 0 ->
            couch_stats:decrement_counter(Counter, N)
    end.

config_delete_section(Section) ->
    [config:delete(K, V, false) || {K, V} <- config:get(Section)].

add_doc(DbName, DocId, Body) ->
    {ok, _} = fabric:update_doc(DbName, mkdoc(DocId, Body), [?ADMIN_CTX]),
    ok.

add_replicated_doc(DbName, DocId, Body) ->
    {ok, _} = fabric:update_doc(DbName, mkdoc(DocId, Body), [?ADMIN_CTX, ?REPLICATED_CHANGES]),
    ok.

mkdoc(Id, #{} = Body) ->
    Body1 = Body#{<<"_id">> => Id},
    jiffy:decode(jiffy:encode(Body1)).

wait_exit(MSec) ->
    meck:wait(couch_scanner_server, handle_info, [{'EXIT', '_', '_'}, '_'], MSec).

doc_del_count(DbName) ->
    {ok, DbInfo} = fabric:get_db_info(DbName),
    couch_util:get_value(doc_del_count, DbInfo).
