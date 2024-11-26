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

-module(couch_scanner_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

couch_scanner_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_top_level_api),
            ?TDEF_FE(t_start_stop),
            ?TDEF_FE(t_run_through_all_callbacks_basic, 10),
            ?TDEF_FE(t_find_reporting_works, 10),
            ?TDEF_FE(t_ddoc_features_works, 20),
            ?TDEF_FE(t_config_skips, 10),
            ?TDEF_FE(t_resume_after_error, 10),
            ?TDEF_FE(t_reset, 10),
            ?TDEF_FE(t_schedule_repeat, 10),
            ?TDEF_FE(t_schedule_after, 15)
        ]
    }.

-define(DOC1, <<"scanner_test_doc1">>).
-define(DOC2, <<"_design/scanner_test_doc2">>).
-define(DOC3, <<"scanner_test_doc3">>).
-define(DOC4, <<"_design/scanner_test_doc4">>).

-define(FIND_PLUGIN, couch_scanner_plugin_find).
-define(FEATURES_PLUGIN, couch_scanner_plugin_ddoc_features).

setup() ->
    {module, _} = code:ensure_loaded(?FIND_PLUGIN),
    meck:new(?FIND_PLUGIN, [passthrough]),
    meck:new(couch_scanner_server, [passthrough]),
    meck:new(couch_scanner_util, [passthrough]),
    Ctx = test_util:start_couch([fabric, couch_scanner]),
    DbName1 = <<"dbname1", (?tempdb())/binary>>,
    DbName2 = <<"dbname2", (?tempdb())/binary>>,
    ok = fabric:create_db(DbName1, [{q, "2"}, {n, "1"}]),
    ok = fabric:create_db(DbName2, [{q, "2"}, {n, "1"}]),
    ok = add_doc(DbName1, ?DOC1, #{foo1 => bar}),
    ok = add_doc(DbName1, ?DOC2, #{
        foo2 => baz,
        views => #{
            v1 => #{
                map => <<"function(doc) {emit(1,2);}">>,
                reduce => <<"function(ks, vs, rereduce) {return {};}">>
            },
            v2 => #{
                map => <<"function(doc) {emit(3,4);}">>,
                reduce => <<"_count">>
            }
        },
        filters => #{f1 => <<"function(d){return true;}">>},
        shows => #{s1 => <<"function(d,r){return " ";}">>},
        lists => #{l1 => <<"function(h,r){return " ";}">>},
        rewrites => [
            #{from => <<"/a/b">>, to => <<"/c/d">>},
            #{from => <<"x">>, to => <<"y">>}
        ],
        updates => #{u1 => <<"function(d,r){return [];}">>},
        validate_doc_update => <<"function(n,o,u,s){return true;">>
    }),
    ok = add_doc(DbName2, ?DOC3, #{foo3 => bax}),
    ok = add_doc(DbName2, ?DOC4, #{foo4 => baw, <<>> => this_is_ok_apparently}),
    couch_scanner:reset_checkpoints(),
    {Ctx, {DbName1, DbName2}}.

teardown({Ctx, {DbName1, DbName2}}) ->
    config:delete("couch_scanner", "maintenance_mode", false),
    config_delete_section("couch_scanner"),
    config_delete_section("couch_scanner_plugins"),
    config_delete_section(atom_to_list(?FEATURES_PLUGIN)),
    config_delete_section(atom_to_list(?FIND_PLUGIN)),
    lists:foreach(
        fun(Subsection) ->
            config_delete_section(atom_to_list(?FIND_PLUGIN) ++ "." ++ Subsection)
        end,
        ["skip_dbs", "skip_ddoc", "skip_docs", "regexes"]
    ),
    couch_scanner:reset_checkpoints(),
    couch_scanner:resume(),
    fabric:delete_db(DbName1),
    fabric:delete_db(DbName2),
    test_util:stop_couch(Ctx),
    meck:unload().

t_top_level_api(_) ->
    ?assertMatch(#{}, couch_scanner:checkpoints()),
    ?assertMatch(#{stopped := false}, couch_scanner:status()),
    ?assertMatch(#{}, couch_scanner:reset_checkpoints()),
    ?assertEqual(#{}, couch_scanner:checkpoints()),
    ?assertEqual(ok, couch_scanner:resume()).

t_start_stop(_) ->
    ?assertMatch(#{stopped := false}, couch_scanner:status()),
    ?assertEqual(ok, couch_scanner:stop()),
    ?assertMatch(#{stopped := true}, couch_scanner:status()),
    ?assertEqual(ok, couch_scanner:stop()),
    ?assertMatch(#{stopped := true}, couch_scanner:status()),
    ?assertEqual(ok, couch_scanner_server:resume()),
    ?assertMatch(#{stopped := false}, couch_scanner:status()),
    ?assertEqual(ok, couch_scanner_server:resume()),
    ?assertMatch(#{stopped := false}, couch_scanner:status()).

t_run_through_all_callbacks_basic({_, {DbName1, DbName2}}) ->
    % Run the "find" plugin without any regexes
    meck:reset(couch_scanner_server),
    config:set("couch_scanner_plugins", atom_to_list(?FIND_PLUGIN), "true", false),
    wait_exit(10000),
    % Check that all callbacks we expected to be called were called
    ?assertEqual(1, num_calls(start, 2)),
    ?assertEqual(0, num_calls(resume, 2)),
    ?assertEqual(1, num_calls(complete, 1)),
    ?assertEqual(2, num_calls(checkpoint, 1)),
    ?assertEqual(1, num_calls(db, ['_', DbName1])),
    ?assertEqual(1, num_calls(db, ['_', DbName2])),
    ?assertEqual(1, num_calls(ddoc, ['_', DbName1, '_'])),
    ?assertEqual(1, num_calls(ddoc, ['_', DbName2, '_'])),
    ?assert(num_calls(shards, 2) >= 2),
    DbOpenedCount = num_calls(db_opened, 2),
    ?assert(DbOpenedCount >= 4),
    ?assertEqual(1, num_calls(doc_id, ['_', ?DOC1, '_'])),
    ?assertEqual(1, num_calls(doc_id, ['_', ?DOC2, '_'])),
    ?assertEqual(1, num_calls(doc_id, ['_', ?DOC3, '_'])),
    ?assertEqual(1, num_calls(doc_id, ['_', ?DOC4, '_'])),
    ?assert(num_calls(doc, 3) >= 4),
    DbClosingCount = num_calls(db_closing, 2),
    ?assertEqual(DbOpenedCount, DbClosingCount),
    ?assertEqual(0, log_calls(warning)).

t_find_reporting_works(_) ->
    % Run the "find" plugin with some regexes
    Plugin = atom_to_list(?FIND_PLUGIN),
    config:set(Plugin ++ ".regexes", "foo14", "foo(1|4)", false),
    config:set(Plugin ++ ".regexes", "baz", "baz", false),
    meck:reset(couch_scanner_server),
    config:set("couch_scanner_plugins", Plugin, "true", false),
    wait_exit(10000),
    % doc2 should have a baz and doc1 and doc4 matches foo14
    ?assertEqual(3, log_calls(warning)).

t_ddoc_features_works({_, {_, DbName2}}) ->
    % Run the "ddoc_features" plugin
    Plugin = atom_to_list(?FEATURES_PLUGIN),
    config:set(Plugin, "filters", "true", false),
    config:set(Plugin, "reduce", "true", false),
    config:set(Plugin, "validate_doc_update", "true", false),
    meck:reset(couch_scanner_server),
    meck:reset(couch_scanner_util),
    config:set("couch_scanner_plugins", Plugin, "true", false),
    wait_exit(10000),
    LogArgs = [warning, ?FEATURES_PLUGIN, '_', '_', '_'],
    ?assertEqual(1, meck:num_calls(couch_scanner_util, log, LogArgs)),
    % Add a detectable feature to the second db.
    % Expect two reports written
    ok = add_doc(DbName2, <<"_design/doc42">>, #{
        rewrites => <<"function(r) {return r;}">>
    }),
    config:set("couch_scanner", "interval_sec", "1", false),
    couch_scanner:resume(),
    meck:reset(couch_scanner_server),
    meck:reset(couch_scanner_util),
    Now = erlang:system_time(second),
    TStamp = calendar:system_time_to_rfc3339(Now + 1, [{offset, "Z"}]),
    config:set(Plugin, "after", TStamp, false),
    wait_exit(10000),
    ?assertEqual(2, meck:num_calls(couch_scanner_util, log, LogArgs)).

t_config_skips({_, {DbName1, DbName2}}) ->
    Plugin = atom_to_list(?FIND_PLUGIN),
    config:set(Plugin ++ ".skip_dbs", "x", binary_to_list(DbName2), false),
    config:set(Plugin ++ ".skip_docs", "y", binary_to_list(?DOC1), false),
    config:set(Plugin ++ ".skip_ddocs", "z", binary_to_list(?DOC2), false),
    meck:reset(couch_scanner_server),
    config:set("couch_scanner_plugins", Plugin, "true", false),
    wait_exit(10000),
    % Some should not be called we skipped DbName2 and some docs
    ?assertEqual(1, num_calls(start, 2)),
    ?assertEqual(0, num_calls(resume, 2)),
    ?assertEqual(1, num_calls(complete, 1)),
    ?assertEqual(2, num_calls(checkpoint, 1)),
    ?assertEqual(1, num_calls(db, ['_', DbName1])),
    ?assertEqual(0, num_calls(db, ['_', DbName2])),
    ?assertEqual(0, num_calls(ddoc, ['_', DbName1, '_'])),
    ?assertEqual(0, num_calls(ddoc, ['_', DbName2, '_'])),
    DbOpenedCount = num_calls(db_opened, 2),
    ?assert(DbOpenedCount >= 2),
    ?assertEqual(0, num_calls(doc_id, ['_', ?DOC1, '_'])),
    ?assertEqual(1, num_calls(doc_id, ['_', ?DOC2, '_'])),
    ?assertEqual(0, num_calls(doc_id, ['_', ?DOC3, '_'])),
    ?assertEqual(0, num_calls(doc_id, ['_', ?DOC4, '_'])),
    DbClosingCount = num_calls(db_closing, 2),
    ?assertEqual(DbOpenedCount, DbClosingCount).

t_resume_after_error(_) ->
    meck:reset(?FIND_PLUGIN),
    meck:expect(
        ?FIND_PLUGIN,
        shards,
        2,
        meck:seq([
            meck:passthrough(),
            meck:raise(error, oops),
            meck:passthrough()
        ])
    ),
    Plugin = atom_to_list(?FIND_PLUGIN),
    config:set("couch_scanner", "min_penalty_sec", "1", false),
    config:set("couch_scanner", "interval_sec", "1", false),
    couch_scanner:resume(),
    config:set("couch_scanner_plugins", Plugin, "true", false),
    meck:wait(?FIND_PLUGIN, resume, 2, 10000).

t_reset(_) ->
    meck:reset(?FIND_PLUGIN),
    meck:expect(
        ?FIND_PLUGIN,
        checkpoint,
        1,
        meck:seq([
            meck:passthrough(),
            meck:val(reset),
            meck:passthrough()
        ])
    ),
    Plugin = atom_to_list(?FIND_PLUGIN),
    config:set("couch_scanner", "interval_sec", "1", false),
    couch_scanner:resume(),
    config:set("couch_scanner_plugins", Plugin, "true", false),
    % Start called twice because of the reset
    meck:wait(2, ?FIND_PLUGIN, start, 2, 10000).

t_schedule_repeat(_) ->
    meck:reset(?FIND_PLUGIN),
    Plugin = atom_to_list(?FIND_PLUGIN),
    config:set("couch_scanner", "interval_sec", "1", false),
    couch_scanner:resume(),
    config:set(Plugin, "repeat", "2_sec", false),
    config:set("couch_scanner_plugins", Plugin, "true", false),
    meck:wait(2, ?FIND_PLUGIN, start, 2, 10000).

t_schedule_after(_) ->
    meck:reset(?FIND_PLUGIN),
    Plugin = atom_to_list(?FIND_PLUGIN),
    config:set("couch_scanner", "interval_sec", "1", false),
    couch_scanner:resume(),
    Now = erlang:system_time(second),
    TStamp = calendar:system_time_to_rfc3339(Now + 3, [{offset, "Z"}]),
    config:set(Plugin, "after", TStamp, false),
    config:set("couch_scanner_plugins", Plugin, "true", false),
    meck:wait(?FIND_PLUGIN, start, 2, 10000).

config_delete_section(Section) ->
    [config:delete(K, V, false) || {K, V} <- config:get(Section)].

add_doc(DbName, DocId, Body) ->
    {ok, _} = fabric:update_doc(DbName, mkdoc(DocId, Body), [?ADMIN_CTX]),
    ok.

mkdoc(Id, #{} = Body) ->
    Body1 = Body#{<<"_id">> => Id},
    jiffy:decode(jiffy:encode(Body1)).

num_calls(Fun, Args) ->
    meck:num_calls(?FIND_PLUGIN, Fun, Args).

log_calls(Level) ->
    meck:num_calls(couch_scanner_util, log, [Level, ?FIND_PLUGIN, '_', '_', '_']).

wait_exit(MSec) ->
    meck:wait(couch_scanner_server, handle_info, [{'EXIT', '_', '_'}, '_'], MSec).
