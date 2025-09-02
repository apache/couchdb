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

-module(couch_tombstone_remover_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(PLUGIN, couch_tombstone_remover).

couch_quickjs_scanner_plugin_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_removes_tombstone, 10)
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
    ok = add_doc(DbName, <<"_local/_tombstone_ttl">>, #{<<"ttl">> => -1_000_000}),
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

t_removes_tombstone({_, DbName}) ->
    ok = add_doc(DbName, <<"doc1">>, #{<<"_deleted">> => true}),
    ?assertEqual(1, doc_del_count(DbName)),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
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

mkdoc(Id, #{} = Body) ->
    Body1 = Body#{<<"_id">> => Id},
    jiffy:decode(jiffy:encode(Body1)).

num_calls(Fun, Args) ->
    meck:num_calls(?PLUGIN, Fun, Args).

wait_exit(MSec) ->
    meck:wait(couch_scanner_server, handle_info, [{'EXIT', '_', '_'}, '_'], MSec).

doc_del_count(DbName) ->
    {ok, DbInfo} = fabric:get_db_info(DbName),
    couch_util:get_value(doc_del_count, DbInfo).
