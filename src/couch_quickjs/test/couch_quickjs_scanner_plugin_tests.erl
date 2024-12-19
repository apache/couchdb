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

-module(couch_quickjs_scanner_plugin_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

couch_quickjs_scanner_plugin_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_vdu_filter_map, 10),
            ?TDEF_FE(t_filter_map, 10),
            ?TDEF_FE(t_map_only, 10),
            ?TDEF_FE(t_vdu_only, 10),
            ?TDEF_FE(t_non_deterministic_vdu, 10),
            ?TDEF_FE(t_filter_only, 10),
            ?TDEF_FE(t_filter_with_expected_error, 10),
            ?TDEF_FE(t_empty_ddoc, 10),
            ?TDEF_FE(t_multi_emit_map, 10),
            ?TDEF_FE(t_non_deterministic_views, 10),
            ?TDEF_FE(t_handle_list_functions_in_maps, 10),
            ?TDEF_FE(t_doc_updates, 10),
            ?TDEF_FE(t_clouseau, 10),
            ?TDEF_FE(t_nouveau, 10)
        ]
    }.

-define(DOC1, <<"doc1">>).
-define(DOC2, <<"doc2">>).
-define(DOC3, <<"doc3">>).
-define(DOC4, <<"doc4">>).
-define(DOC5, <<"doc5">>).
-define(DDOC1, <<"_design/ddoc1">>).

-define(PLUGIN, couch_quickjs_scanner_plugin).

setup() ->
    {module, _} = code:ensure_loaded(?PLUGIN),
    meck:new(?PLUGIN, [passthrough]),
    meck:new(couch_scanner_server, [passthrough]),
    meck:new(couch_scanner_util, [passthrough]),
    Ctx = test_util:start_couch([fabric, couch_scanner]),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, "2"}, {n, "1"}]),
    ok = add_doc(DbName, ?DOC1, #{a => x}),
    ok = add_doc(DbName, ?DOC2, #{a => y}),
    ok = add_doc(DbName, ?DOC3, #{a => z}),
    ok = add_doc(DbName, ?DOC4, #{a => w}),
    ok = add_doc(DbName, ?DOC5, #{a => u}),
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

t_vdu_filter_map({_, DbName}) ->
    ok = add_doc(DbName, ?DDOC1, ddoc_vdu(ddoc_filter(ddoc_view(#{})))),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    case couch_server:with_spidermonkey() of
        true ->
            ?assertEqual(1, num_calls(complete, 1)),
            ?assertEqual(2, num_calls(checkpoint, 1)),
            ?assertEqual(1, num_calls(db, ['_', DbName])),
            ?assertEqual(1, num_calls(ddoc, ['_', DbName, '_'])),
            ?assert(num_calls(shards, 2) >= 1),
            DbOpenedCount = num_calls(db_opened, 2),
            ?assert(DbOpenedCount >= 2),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC1, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC2, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC3, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC4, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC5, '_'])),
            ?assert(num_calls(doc, 3) >= 5),
            DbClosingCount = num_calls(db_closing, 2),
            ?assertEqual(DbOpenedCount, DbClosingCount),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_error_exits])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_errors])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_exits])),
            % start, complete and each of the 5 docs should fail = 7 total
            ?assertEqual(7, log_calls(warning));
        false ->
            ok
    end.

t_filter_map({_, DbName}) ->
    ok = add_doc(DbName, ?DDOC1, ddoc_filter(ddoc_view(#{}))),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    case couch_server:with_spidermonkey() of
        true ->
            ?assertEqual(1, num_calls(complete, 1)),
            ?assertEqual(2, num_calls(checkpoint, 1)),
            ?assertEqual(1, num_calls(db, ['_', DbName])),
            ?assertEqual(1, num_calls(ddoc, ['_', DbName, '_'])),
            ?assert(num_calls(shards, 2) >= 1),
            DbOpenedCount = num_calls(db_opened, 2),
            ?assert(DbOpenedCount >= 2),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC1, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC2, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC3, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC4, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC5, '_'])),
            ?assert(num_calls(doc, 3) >= 5),
            DbClosingCount = num_calls(db_closing, 2),
            ?assertEqual(DbOpenedCount, DbClosingCount),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_error_exits])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_errors])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_exits])),
            % start, complete and each of the 4 docs should fail = 6 total
            ?assertEqual(6, log_calls(warning));
        false ->
            ok
    end.

t_map_only({_, DbName}) ->
    ok = add_doc(DbName, ?DDOC1, ddoc_view(#{})),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    case couch_server:with_spidermonkey() of
        true ->
            ?assertEqual(1, num_calls(complete, 1)),
            ?assertEqual(2, num_calls(checkpoint, 1)),
            ?assertEqual(1, num_calls(db, ['_', DbName])),
            ?assertEqual(1, num_calls(ddoc, ['_', DbName, '_'])),
            ?assert(num_calls(shards, 2) >= 1),
            DbOpenedCount = num_calls(db_opened, 2),
            ?assert(DbOpenedCount >= 2),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC1, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC2, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC3, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC4, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC5, '_'])),
            ?assert(num_calls(doc, 3) >= 5),
            DbClosingCount = num_calls(db_closing, 2),
            ?assertEqual(DbOpenedCount, DbClosingCount),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_error_exits])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_errors])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_exits])),
            % start, complete and each of the 3 docs should fail = 5 total
            ?assertEqual(5, log_calls(warning));
        false ->
            ok
    end.

t_vdu_only({_, DbName}) ->
    ok = add_doc(DbName, ?DDOC1, ddoc_vdu(#{})),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    case couch_server:with_spidermonkey() of
        true ->
            ?assertEqual(1, num_calls(complete, 1)),
            ?assertEqual(2, num_calls(checkpoint, 1)),
            ?assertEqual(1, num_calls(db, ['_', DbName])),
            ?assertEqual(1, num_calls(ddoc, ['_', DbName, '_'])),
            ?assert(num_calls(shards, 2) >= 1),
            DbOpenedCount = num_calls(db_opened, 2),
            ?assert(DbOpenedCount >= 2),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC1, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC2, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC3, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC4, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC5, '_'])),
            ?assert(num_calls(doc, 3) >= 5),
            DbClosingCount = num_calls(db_closing, 2),
            ?assertEqual(DbOpenedCount, DbClosingCount),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_error_exits])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_errors])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_exits])),
            % start, complete and 1 vdu only = 3 total
            ?assertEqual(3, log_calls(warning));
        false ->
            ok
    end.

t_non_deterministic_vdu({_, DbName}) ->
    ok = add_doc(DbName, ?DDOC1, ddoc_non_deterministic_vdu(#{})),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    case couch_server:with_spidermonkey() of
        true ->
            ?assertEqual(1, num_calls(complete, 1)),
            ?assertEqual(2, num_calls(checkpoint, 1)),
            ?assertEqual(1, num_calls(db, ['_', DbName])),
            ?assertEqual(1, num_calls(ddoc, ['_', DbName, '_'])),
            ?assert(num_calls(shards, 2) >= 1),
            DbOpenedCount = num_calls(db_opened, 2),
            ?assert(DbOpenedCount >= 2),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC1, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC2, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC3, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC4, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC5, '_'])),
            ?assert(num_calls(doc, 3) >= 5),
            DbClosingCount = num_calls(db_closing, 2),
            ?assertEqual(DbOpenedCount, DbClosingCount),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_error_exits])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_errors])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_exits])),
            % start, complete and no vdu warning because it has a Math.random(), so 2 total
            ?assertEqual(2, log_calls(warning));
        false ->
            ok
    end.

t_filter_only({_, DbName}) ->
    ok = add_doc(DbName, ?DDOC1, ddoc_filter(#{})),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    case couch_server:with_spidermonkey() of
        true ->
            ?assertEqual(1, num_calls(complete, 1)),
            ?assertEqual(2, num_calls(checkpoint, 1)),
            ?assertEqual(1, num_calls(db, ['_', DbName])),
            ?assertEqual(1, num_calls(ddoc, ['_', DbName, '_'])),
            ?assert(num_calls(shards, 2) >= 1),
            DbOpenedCount = num_calls(db_opened, 2),
            ?assert(DbOpenedCount >= 2),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC1, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC2, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC3, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC4, '_'])),
            ?assertEqual(1, num_calls(doc_id, ['_', ?DOC5, '_'])),
            ?assert(num_calls(doc, 3) >= 5),
            DbClosingCount = num_calls(db_closing, 2),
            ?assertEqual(DbOpenedCount, DbClosingCount),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_error_exits])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_errors])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_exits])),
            % start, complete and one filter only = 3 total
            ?assertEqual(3, log_calls(warning));
        false ->
            ok
    end.

t_filter_with_expected_error({_, DbName}) ->
    % Put filters with errors in different ddocs so than each be tested in isolation
    ok = add_doc(DbName, <<"_design/ddoc_custom_error">>, ddoc_filter_custom_error(#{})),
    ok = add_doc(DbName, <<"_design/ddoc_type_error">>, ddoc_filter_type_error(#{})),
    ok = add_doc(DbName, <<"_design/ddoc_syntax_error">>, ddoc_filter_syntax_error(#{})),
    ok = add_doc(DbName, <<"_design/ddoc_reference_error">>, ddoc_filter_reference_error(#{})),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    case couch_server:with_spidermonkey() of
        true ->
            ?assertEqual(1, num_calls(complete, 1)),
            ?assertEqual(1, num_calls(db, ['_', DbName])),
            ?assertEqual(4, num_calls(ddoc, ['_', DbName, '_'])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_error_exits])),
            ?assertEqual(40, couch_stats:sample([couchdb, query_server, process_errors])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_exits])),
            % start, complete and no warning as we expected the error, so 2 total only
            ?assertEqual(2, log_calls(warning));
        false ->
            ok
    end.

t_empty_ddoc({_, DbName}) ->
    ok = add_doc(DbName, ?DDOC1, #{}),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    case couch_server:with_spidermonkey() of
        true ->
            ?assertEqual(1, num_calls(complete, 1)),
            ?assertEqual(2, num_calls(checkpoint, 1)),
            ?assertEqual(1, num_calls(db, ['_', DbName])),
            ?assertEqual(1, num_calls(ddoc, ['_', DbName, '_'])),
            ?assert(num_calls(shards, 2) >= 1),
            ?assertEqual(0, num_calls(db_opened, 2)),
            ?assertEqual(0, num_calls(doc_id, ['_', ?DOC1, '_'])),
            ?assertEqual(0, num_calls(doc_id, ['_', ?DOC2, '_'])),
            ?assertEqual(0, num_calls(doc_id, ['_', ?DOC3, '_'])),
            ?assertEqual(0, num_calls(doc_id, ['_', ?DOC4, '_'])),
            ?assertEqual(0, num_calls(doc_id, ['_', ?DOC5, '_'])),
            ?assertEqual(0, num_calls(db_closing, 2)),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_error_exits])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_errors])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_exits])),
            % start and complete only = 2 total
            ?assertEqual(2, log_calls(warning));
        false ->
            ok
    end.

t_multi_emit_map({_, DbName}) ->
    ok = add_doc(DbName, ?DDOC1, ddoc_view_multi_emit(#{})),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    case couch_server:with_spidermonkey() of
        true ->
            ?assertEqual(1, num_calls(complete, 1)),
            ?assert(num_calls(doc, 3) >= 5),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_error_exits])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_errors])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_exits])),
            % start and complete = 2, no errors
            ?assertEqual(2, log_calls(warning));
        false ->
            ok
    end.

t_non_deterministic_views({_, DbName}) ->
    ok = add_doc(DbName, ?DDOC1, ddoc_view_non_determinism(#{})),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    case couch_server:with_spidermonkey() of
        true ->
            ?assertEqual(1, num_calls(complete, 1)),
            ?assert(num_calls(doc, 3) >= 5),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_error_exits])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_errors])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_exits])),
            % start and complete = 2, no errors
            ?assertEqual(2, log_calls(warning));
        false ->
            ok
    end.

t_handle_list_functions_in_maps({_, DbName}) ->
    ok = add_doc(DbName, ?DDOC1, ddoc_use_list_funs_in_maps(#{})),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    case couch_server:with_spidermonkey() of
        true ->
            ?assertEqual(1, num_calls(complete, 1)),
            ?assert(num_calls(doc, 3) >= 5),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_error_exits])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_errors])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_exits])),
            % start and complete = 2, no errors
            ?assertEqual(2, log_calls(warning));
        false ->
            ok
    end.

t_doc_updates({_, DbName}) ->
    ok = add_doc(DbName, ?DDOC1, ddoc_update(#{})),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    case couch_server:with_spidermonkey() of
        true ->
            ?assertEqual(1, num_calls(complete, 1)),
            ?assert(num_calls(doc, 3) >= 5),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_error_exits])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_errors])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_exits])),
            % start and complete = 2 + 5 warnings = 7
            ?assertEqual(7, log_calls(warning));
        false ->
            ok
    end.

t_clouseau({_, DbName}) ->
    ok = add_doc(DbName, ?DDOC1, ddoc_clouseau(#{})),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    case couch_server:with_spidermonkey() of
        true ->
            ?assertEqual(1, num_calls(complete, 1)),
            ?assert(num_calls(doc, 3) >= 5),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_error_exits])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_errors])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_exits])),
            % start and complete = 2 + 3 warnings = 5
            ?assertEqual(5, log_calls(warning));
        false ->
            ok
    end.

t_nouveau({_, DbName}) ->
    ok = add_doc(DbName, ?DDOC1, ddoc_nouveau(#{})),
    meck:reset(couch_scanner_server),
    meck:reset(?PLUGIN),
    config:set("couch_scanner_plugins", atom_to_list(?PLUGIN), "true", false),
    wait_exit(10000),
    ?assertEqual(1, num_calls(start, 2)),
    case {couch_server:with_spidermonkey(), nouveau:enabled()} of
        {true, true} ->
            ?assertEqual(1, num_calls(complete, 1)),
            ?assert(num_calls(doc, 3) >= 5),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_error_exits])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_errors])),
            ?assertEqual(0, couch_stats:sample([couchdb, query_server, process_exits])),
            % start and complete = 2 + 3 warnings = 5
            ?assertEqual(5, log_calls(warning));
        {_, _} ->
            ok
    end.

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

log_calls(Level) ->
    meck:num_calls(couch_scanner_util, log, [Level, ?PLUGIN, '_', '_', '_']).

wait_exit(MSec) ->
    meck:wait(couch_scanner_server, handle_info, [{'EXIT', '_', '_'}, '_'], MSec).

ddoc_vdu(Doc) ->
    Doc#{
        validate_doc_update => <<
            "function(newdoc, olddoc, userctx, sec){\n"
            " if(newdoc.a == 'w') {\n"
            "    newdoc.a.search(/(w+)/);\n"
            "    if(RegExp.$1 == 'w') {return true} else {throw('forbidden')}\n"
            " } else {\n"
            "    return true\n"
            " }\n"
            "}"
        >>
    }.

ddoc_non_deterministic_vdu(Doc) ->
    Doc#{
        validate_doc_update => <<
            "function(newdoc, olddoc, userctx, sec){\n"
            " if (Math.random() > 0.5) {\n"
            "    throw('forbidden');\n"
            " } else {\n"
            "    return true\n"
            " }\n"
            "}"
        >>
    }.

ddoc_filter(Doc) ->
    Doc#{
        filters => #{
            f => <<
                "function(doc, req) {\n"
                " if(doc.a == 'z') {\n"
                "   doc.a.search(/(z+)/);\n"
                "   if(RegExp.$1 == 'z') {return true} else {return false};\n"
                " } else {\n"
                "   return true;\n"
                " }\n"
                "}"
            >>,
            f_use_req => <<
                "function (doc, req) {\n"
                "    if (doc.a == req.query.foo) {return true;} \n"
                "    return false; \n"
                "}\n"
            >>,
            f_non_deterministic => <<"function(doc, req) {return new Date();}}">>
        }
    }.

ddoc_filter_custom_error(Doc) ->
    Doc#{filters => #{f => <<"function(doc, req) {throw(\"foo\");}">>}}.

ddoc_filter_type_error(Doc) ->
    Doc#{filters => #{f => <<"function(doc, req){if(doc.missing.foo){return false;}}">>}}.

ddoc_filter_syntax_error(Doc) ->
    Doc#{filters => #{f => <<"function(doc, req){JSON.parse([]); return true;}">>}}.

ddoc_filter_reference_error(Doc) ->
    Doc#{filters => #{f => <<"function(doc, req){if(potato){return true;}}">>}}.

ddoc_view(Doc) ->
    Doc#{
        views => #{
            lib => #{
                baz => <<"exports.baz = 'bam';">>,
                foo => #{
                    fuzz => <<"exports.foo = 'bar';">>
                }
            },
            v1 => #{
                map => <<
                    "function(doc) {\n"
                    "  if(doc.a == 'x') {\n"
                    "    r = doc.a.search(/(x+)/); emit(r, RegExp.$1)\n"
                    "  } else {\n"
                    "    emit(doc.a, doc.a);\n"
                    "  }\n"
                    "}"
                >>,
                reduce => <<
                    "function(ks, vs, rereduce) {\n"
                    "  v0 = vs[0];\n"
                    "  if (!rereduce) {\n"
                    "    k0 = ks[0];\n"
                    "    if (k0 == 'y') {\n"
                    "      k0.search(/(y+)/);\n"
                    "      return RegExp.$1;\n"
                    "    };\n"
                    "    return v0;\n"
                    " } else {\n"
                    "    if (v0 == 'u') {\n"
                    "      v0.search(/(u+)/);\n"
                    "      return RegExp.$1;\n"
                    "    };\n"
                    "    return v0;\n"
                    " }\n"
                    "}"
                >>
            },
            v_large_reduce => #{
                map => <<
                    "function(doc) {\n"
                    "  if(doc.a == 'x') {\n"
                    "    r = doc.a.search(/(x+)/); emit(r, RegExp.$1)\n"
                    "  } else {\n"
                    "    for(i=0;i<10000;i++) {emit(doc.a, doc.a)};\n"
                    "  }\n"
                    "}"
                >>,
                reduce => <<
                    "function(ks, vs, rereduce) {\n"
                    "  return {'ks1': ks, 'ks2': k2, 'vs1': v2, 'v2':v2};\n"
                    "}"
                >>
            },
            v_type_error => #{map => <<"function(doc){emit(doc.missing.foo,1);}">>},
            v_syntax_error => #{map => <<"function(doc){emit(JSON.parse([]),1);}">>},
            v_reference_error => #{map => <<"function(doc){emit(potato,1);}">>},
            v_expr_fun1 => #{map => <<"(function(doc) {emit(1,2)})\n">>},
            v_expr_fun2 => #{map => <<"(function(doc) {emit(3,4)});">>},
            v_expr_fun3 => #{map => <<"y=9;\n(function(doc) {emit(5,y)})">>},
            v_expr_fun4 => #{map => <<"x=10; function(doc) {emit(x,6)}\n">>},
            v_emit_map1 => #{
                map => <<
                    "function(doc){ \n",
                    "  emit(42, {NaN:1, \"2\": 1}); \n"
                    "};\n"
                >>
            }
        }
    }.

ddoc_view_multi_emit(Doc) ->
    % String.prototype.startsWith used as a differentiator between
    % SM and QuickJS. Make both emit items in different order. But at the
    % end of the day, that doesn't matter as those are sorted anyway
    Doc#{
        views => #{
            v1 => #{
                map => <<
                    "function(doc) {\n"
                    "  if(String.prototype.startsWith === undefined) {\n"
                    "    emit(42,1); emit(40,2); emit(42,3); emit(39,4);\n"
                    "  } else {\n"
                    "    emit(40,2); emit(39,4); emit(42,1); emit(42,3)\n"
                    "  }\n"
                    "}"
                >>
            },
            v2 => #{
                map => <<
                    "function(doc) {\n"
                    "  if(String.prototype.startsWith === undefined) {\n"
                    "    emit(99,1); emit(98,2);\n"
                    "  } else {\n"
                    "    emit(98,2); emit(99,1)\n"
                    "  }\n"
                    "}"
                >>
            }
        }
    }.

ddoc_view_non_determinism(Doc) ->
    % Test some functions with random and date values.
    Doc#{
        views => #{
            v1 => #{
                map => <<"function(doc) {emit(Math.random(), 1);}">>
            },
            v2 => #{
                map => <<"function(doc) {emit(Date.now(), 1);}">>
            },
            v3 => #{
                map => <<"function(doc) {emit(new Date(), 1);}">>
            },
            v4 => #{
                map => <<"function(doc) {emit(1,2);}">>,
                reduce => <<"function(ks, vs, rr){return Date.now();}">>
            }
        }
    }.

ddoc_use_list_funs_in_maps(Doc) ->
    % If users call list functions from their maps, we used to crash
    % the scanner process with a function_clause.
    Doc#{
        views => #{
            v1 => #{
                map => <<
                    "function(head, req) { \n"
                    "var row;\n"
                    "start({headers: {'Content-Type': 'text/plain'}});\n"
                    "while(row = getRow()) {send('x'); send('y');}\n"
                    "}"
                >>
            }
        }
    }.

ddoc_update(Doc) ->
    % If users call list functions from their maps, we used to crash
    % the scanner process with a function_clause.
    Doc#{
        updates => #{
            u1 => <<
                "function(doc, req) {\n"
                "  doc.a.search(/(x+)/); \n"
                "  if (RegExp.$1 === undefined) {\n"
                "    return [null, 'no_dollar_one']; \n"
                "  } else { \n"
                "    return [null, 'got_dollar_one'];\n"
                "  }\n"
                "}"
            >>,
            u2 => <<
                "function(doc, req) {\n"
                "  if (typeof(req.form) === 'object') {\n"
                "    return [null, 'has_form']; \n"
                "  } else { \n"
                "    return [null, 'no_form'];\n"
                "  }\n"
                "}"
            >>,
            u3 => <<"function(doc, req) {throw('Potato')}">>,
            u4_non_deterministic => <<"function(doc, req) {throw(Date.now())}">>
        }
    }.

ddoc_clouseau(Doc) ->
    Doc#{
        indexes => #{
            idx1 => #{
                <<"default_analyzer">> => <<"english">>,
                <<"index">> => <<
                    "function(doc) {\n"
                    "  index('a', doc.a, {'store': true}); \n"
                    "  index('fourtytwo', 42, {'store': false}); \n"
                    "}"
                >>
            },
            idx2 => #{
                <<"index">> => <<
                    "function(doc) {\n"
                    "  doc.a.search(/(x+)/); \n"
                    "  if (RegExp.$1 === undefined) {\n"
                    "      index('dollar_one', 'nope') \n"
                    "  } else { \n"
                    "      index('dollar_one', 'yup') \n"
                    "  }\n"
                    "}"
                >>
            }
        }
    }.

ddoc_nouveau(Doc) ->
    Doc#{
        nouveau => #{
            idx1 => #{
                <<"index">> => <<
                    "function(doc) {\n"
                    "  index('string', 'a', doc.a, {'store': true}); \n"
                    "  index('double', 'fourtytwo', 42, {'store': false}); \n"
                    "}"
                >>
            },
            idx2 => #{
                <<"index">> => <<
                    "function(doc) {\n"
                    "  doc.a.search(/(x+)/); \n"
                    "  if (RegExp.$1 === undefined) {\n"
                    "      index('string', 'dollar_one', 'nope') \n"
                    "  } else { \n"
                    "      index('string', 'dollar_one', 'yup') \n"
                    "  }\n"
                    "}"
                >>
            }
        }
    }.
