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

-module(nouveau_index_upgrader_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("nouveau/include/nouveau.hrl").

-define(PLUGIN, nouveau_index_upgrader).

nouveau_index_upgrader_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_upgrade_legacy_index, 10),
            ?TDEF_FE(t_dont_upgrade_latest_index, 10)
        ]
    }.

setup() ->
    {module, _} = code:ensure_loaded(?PLUGIN),
    meck:new(?PLUGIN, [passthrough]),
    meck:new(couch_scanner_server, [passthrough]),
    meck:new(couch_scanner_util, [passthrough]),
    meck:new(nouveau_api, [passthrough]),
    meck:expect(nouveau_api, supported_lucene_versions, fun() ->
        {ok, [?LEGACY_LUCENE_VERSION, ?TARGET_LUCENE_VERSION]}
    end),
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

t_upgrade_legacy_index({_, DbName}) ->
    DDocId = <<"_design/foo">>,
    IndexName = <<"bar">>,
    ok = add_ddoc(DbName, DDocId, IndexName, ?LEGACY_LUCENE_VERSION),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    meck:new(nouveau_fabric_search, [passthrough]),
    meck:expect(nouveau_fabric_search, go, fun(_DbName, _Args, _Index) -> {ok, []} end),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    ?assertEqual(1, num_calls(complete, 1)),
    ?assertEqual(?TARGET_LUCENE_VERSION, get_lucene_version(DbName, DDocId, IndexName)),
    ok.

t_dont_upgrade_latest_index({_, DbName}) ->
    DDocId = <<"_design/foo">>,
    IndexName = <<"bar">>,
    ok = add_ddoc(DbName, DDocId, IndexName, ?TARGET_LUCENE_VERSION),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    ?assertEqual(1, num_calls(complete, 1)),
    ?assertEqual(?TARGET_LUCENE_VERSION, get_lucene_version(DbName, DDocId, IndexName)),
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

add_ddoc(DbName, DDocId, IndexName, LuceneVersion) ->
    {ok, _} = fabric:update_doc(DbName, mkddoc(DDocId, IndexName, LuceneVersion), [?ADMIN_CTX]),
    ok.

get_lucene_version(DbName, DDocId, IndexName) ->
    {ok, #doc{body = {Props}}} = fabric:open_doc(DbName, DDocId, [?ADMIN_CTX]),
    {Indexes} = couch_util:get_value(<<"nouveau">>, Props),
    {Index} = couch_util:get_value(IndexName, Indexes),
    couch_util:get_value(<<"lucene_version">>, Index).

mkddoc(DocId, IndexName, LuceneVersion) ->
    Body = #{
        <<"_id">> => DocId,
        <<"nouveau">> => #{
            IndexName => #{
                <<"lucene_version">> => LuceneVersion,
                <<"index">> => <<"function(doc){}">>
            }
        }
    },
    jiffy:decode(jiffy:encode(Body)).

num_calls(Fun, Args) ->
    meck:num_calls(?PLUGIN, Fun, Args).

wait_exit(MSec) ->
    meck:wait(couch_scanner_server, handle_info, [{'EXIT', '_', '_'}, '_'], MSec).
