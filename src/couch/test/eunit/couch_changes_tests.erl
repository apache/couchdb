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

-module(couch_changes_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, 6000).

-record(row, {
    id,
    seq,
    deleted = false,
    doc = nil,
    revs = []
}).

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = create_db(DbName),
    Revs = [
        R
     || {ok, R} <- [
            save_doc(Db, {[{<<"_id">>, <<"doc1">>}]}),
            save_doc(Db, {[{<<"_id">>, <<"doc2">>}]}),
            save_doc(Db, {[{<<"_id">>, <<"doc3">>}]}),
            save_doc(Db, {[{<<"_id">>, <<"doc4">>}]}),
            save_doc(Db, {[{<<"_id">>, <<"doc5">>}]})
        ]
    ],
    Rev = lists:nth(3, Revs),
    {ok, Db1} = couch_db:reopen(Db),

    {ok, Rev1} = save_doc(Db1, {[{<<"_id">>, <<"doc3">>}, {<<"_rev">>, Rev}]}),
    Revs1 = Revs ++ [Rev1],
    Revs2 =
        Revs1 ++
            [
                R
             || {ok, R} <- [
                    save_doc(Db1, {[{<<"_id">>, <<"doc6">>}]}),
                    save_doc(Db1, {[{<<"_id">>, <<"_design/foo">>}]}),
                    save_doc(Db1, {[{<<"_id">>, <<"doc7">>}]}),
                    save_doc(Db1, {[{<<"_id">>, <<"doc8">>}]})
                ]
            ],
    config:set(
        "native_query_servers", "erlang", "{couch_native_process, start_link, []}", _Persist = false
    ),
    {DbName, list_to_tuple(Revs2)}.

teardown({DbName, _}) ->
    config:delete("native_query_servers", "erlang", _Persist = false),
    delete_db(DbName),
    ok.

changes_test_() ->
    {
        "Changes feed",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            [
                filter_by_selector(),
                filter_by_doc_id(),
                filter_by_design(),
                continuous_feed(),
                filter_by_custom_function(),
                filter_by_filter_function(),
                filter_by_view(),
                style_and_include_docs(),
                style_and_include_docs_with_revtree()
            ]
        }
    }.

filter_by_doc_id() ->
    {
        "Filter _doc_id",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_filter_by_specific_doc_ids),
                ?TDEF_FE(t_filter_by_specific_doc_ids_descending),
                ?TDEF_FE(t_filter_by_specific_doc_ids_no_result),
                ?TDEF_FE(t_filter_by_specific_doc_ids_with_since),
                ?TDEF_FE(t_handle_deleted_docs)
            ]
        }
    }.

filter_by_selector() ->
    {
        "Filter _selector",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_select_basic),
                ?TDEF_FE(t_select_with_since),
                ?TDEF_FE(t_select_when_no_result),
                ?TDEF_FE(t_select_with_deleted_docs),
                ?TDEF_FE(t_select_with_continuous),
                ?TDEF_FE(t_stop_selector_when_db_deleted),
                ?TDEF_FE(t_select_with_empty_fields),
                ?TDEF_FE(t_select_with_fields)
            ]
        }
    }.

filter_by_design() ->
    {
        "Filter _design",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [?TDEF_FE(t_emit_only_design_documents)]
        }
    }.

filter_by_custom_function() ->
    {
        "Filter function",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [?TDEF_FE(t_receive_heartbeats, 10)]
        }
    }.

filter_by_filter_function() ->
    {
        "Filter by filters",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_filter_by_doc_attribute),
                ?TDEF_FE(t_filter_by_user_ctx)
            ]
        }
    }.

filter_by_view() ->
    {
        "Filter _view",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_filter_by_view),
                ?TDEF_FE(t_filter_by_erlang_view)
            ]
        }
    }.

continuous_feed() ->
    {
        "Continuous Feed",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_filter_continuous_feed_by_specific_doc_ids),
                ?TDEF_FE(t_end_changes_when_db_deleted)
            ]
        }
    }.

style_and_include_docs() ->
    {
        "Style and include_docs",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_style_main_only),
                ?TDEF_FE(t_style_main_only_with_include_docs),
                ?TDEF_FE(t_style_all_docs),
                ?TDEF_FE(t_style_all_docs_with_include_docs)
            ]
        }
    }.

style_and_include_docs_with_revtree() ->
    {
        "Style and include_docs with revtree",
        {
            foreach,
            fun setup_with_revtree/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_style_main_only_with_revtree),
                ?TDEF_FE(t_style_main_only_with_include_docs_with_revtree),
                ?TDEF_FE(t_style_all_docs_with_revtree),
                ?TDEF_FE(t_style_all_docs_with_include_docs_with_revtree)
            ]
        }
    }.

t_filter_by_specific_doc_ids({DbName, _}) ->
    ChArgs = #changes_args{filter = "_doc_ids"},
    DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
    Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(2, length(Rows)),
    ?assertMatch([#row{seq = 4, id = <<"doc4">>}, #row{seq = 6, id = <<"doc3">>}], Rows),
    ?assertEqual(UpSeq, LastSeq).

t_filter_by_specific_doc_ids_descending({DbName, _}) ->
    ChArgs = #changes_args{filter = "_doc_ids", dir = rev},
    DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
    Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
    {Rows, LastSeq, _} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(2, length(Rows)),
    ?assertMatch([#row{seq = 6, id = <<"doc3">>}, #row{seq = 4, id = <<"doc4">>}], Rows),
    ?assertEqual(4, LastSeq).

t_filter_by_specific_doc_ids_with_since({DbName, _}) ->
    ChArgs = #changes_args{filter = "_doc_ids", since = 5},
    DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
    Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(1, length(Rows)),
    ?assertMatch([#row{seq = 6, id = <<"doc3">>}], Rows),
    ?assertEqual(UpSeq, LastSeq).

t_filter_by_specific_doc_ids_no_result({DbName, _}) ->
    ChArgs = #changes_args{filter = "_doc_ids", since = 6},
    DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
    Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(0, length(Rows)),
    ?assertEqual(UpSeq, LastSeq).

t_handle_deleted_docs({DbName, Revs}) ->
    Rev3_2 = element(6, Revs),
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, _} = save_doc(
        Db,
        {[
            {<<"_id">>, <<"doc3">>},
            {<<"_deleted">>, true},
            {<<"_rev">>, Rev3_2}
        ]}
    ),
    ChArgs = #changes_args{filter = "_doc_ids", since = 9},
    DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
    Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
    {Rows, LastSeq, _} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(1, length(Rows)),
    ?assertMatch([#row{seq = LastSeq, id = <<"doc3">>, deleted = true}], Rows),
    ?assertEqual(11, LastSeq).

t_filter_continuous_feed_by_specific_doc_ids({DbName, Revs}) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    ChangesArgs = #changes_args{feed = "continuous", filter = "_doc_ids"},
    DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
    Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
    reset_row_notifications(),
    Consumer = spawn_consumer(DbName, ChangesArgs, Req),
    ?assertEqual(ok, wait_row_notifications(2)),
    ok = pause(Consumer),

    Rows = get_rows(Consumer),
    ?assertEqual(2, length(Rows)),
    ?assertMatch([#row{seq = 4, id = <<"doc4">>}, #row{seq = 6, id = <<"doc3">>}], Rows),

    clear_rows(Consumer),
    {ok, _Rev9} = save_doc(Db, {[{<<"_id">>, <<"doc9">>}]}),
    {ok, _Rev10} = save_doc(Db, {[{<<"_id">>, <<"doc10">>}]}),
    ok = unpause(Consumer),
    timer:sleep(100),
    ok = pause(Consumer),
    ?assertEqual([], get_rows(Consumer)),

    Rev4 = element(4, Revs),
    Rev3_2 = element(6, Revs),
    {ok, Rev4_2} = save_doc(Db, {[{<<"_id">>, <<"doc4">>}, {<<"_rev">>, Rev4}]}),
    {ok, _} = save_doc(Db, {[{<<"_id">>, <<"doc11">>}]}),
    {ok, _} = save_doc(Db, {[{<<"_id">>, <<"doc4">>}, {<<"_rev">>, Rev4_2}]}),
    {ok, _} = save_doc(Db, {[{<<"_id">>, <<"doc12">>}]}),
    {ok, Rev3_3} = save_doc(Db, {[{<<"_id">>, <<"doc3">>}, {<<"_rev">>, Rev3_2}]}),
    reset_row_notifications(),
    ok = unpause(Consumer),
    ?assertEqual(ok, wait_row_notifications(2)),
    ok = pause(Consumer),

    NewRows = get_rows(Consumer),
    ?assertEqual(2, length(NewRows)),
    ?assertMatch([#row{seq = 15, id = <<"doc4">>}, #row{seq = 17, id = <<"doc3">>}], NewRows),

    clear_rows(Consumer),
    {ok, _Rev3_4} = save_doc(Db, {[{<<"_id">>, <<"doc3">>}, {<<"_rev">>, Rev3_3}]}),
    reset_row_notifications(),
    ok = unpause(Consumer),
    ?assertEqual(ok, wait_row_notifications(1)),
    ok = pause(Consumer),

    FinalRows = get_rows(Consumer),

    ok = unpause(Consumer),
    stop_consumer(Consumer),
    couch_db:close(Db),
    ?assertMatch([#row{seq = 18, id = <<"doc3">>}], FinalRows).

t_end_changes_when_db_deleted({DbName, _Revs}) ->
    {ok, _Db} = couch_db:open_int(DbName, []),
    ChangesArgs = #changes_args{feed = "continuous", filter = "_doc_ids"},
    DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
    Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
    Consumer = spawn_consumer(DbName, ChangesArgs, Req),
    ok = pause(Consumer),
    ok = delete_db(DbName),
    ok = unpause(Consumer),
    {_Rows, _LastSeq} = wait_finished(Consumer),
    ok = stop_consumer(Consumer).

t_select_basic({DbName, _}) ->
    ChArgs = #changes_args{filter = "_selector"},
    Selector = {[{<<"_id">>, <<"doc3">>}]},
    Req = {json_req, {[{<<"selector">>, Selector}]}},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(1, length(Rows)),
    ?assertMatch([#row{seq = 6, id = <<"doc3">>}], Rows),
    ?assertEqual(UpSeq, LastSeq).

t_select_with_since({DbName, _}) ->
    ChArgs = #changes_args{filter = "_selector", since = 9},
    GteDoc2 = {[{<<"$gte">>, <<"doc1">>}]},
    Selector = {[{<<"_id">>, GteDoc2}]},
    Req = {json_req, {[{<<"selector">>, Selector}]}},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(1, length(Rows)),
    ?assertMatch([#row{seq = 10, id = <<"doc8">>}], Rows),
    ?assertEqual(UpSeq, LastSeq).

t_select_when_no_result({DbName, _}) ->
    ChArgs = #changes_args{filter = "_selector"},
    Selector = {[{<<"_id">>, <<"nopers">>}]},
    Req = {json_req, {[{<<"selector">>, Selector}]}},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(0, length(Rows)),
    ?assertEqual(UpSeq, LastSeq).

t_select_with_deleted_docs({DbName, Revs}) ->
    Rev3_2 = element(6, Revs),
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, _} = save_doc(
        Db,
        {[
            {<<"_id">>, <<"doc3">>},
            {<<"_deleted">>, true},
            {<<"_rev">>, Rev3_2}
        ]}
    ),
    ChArgs = #changes_args{filter = "_selector"},
    Selector = {[{<<"_id">>, <<"doc3">>}]},
    Req = {json_req, {[{<<"selector">>, Selector}]}},
    {Rows, LastSeq, _} = run_changes_query(DbName, ChArgs, Req),
    ?assertMatch([#row{seq = LastSeq, id = <<"doc3">>, deleted = true}], Rows),
    ?assertEqual(11, LastSeq).

t_select_with_continuous({DbName, Revs}) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    ChArgs = #changes_args{filter = "_selector", feed = "continuous"},
    GteDoc8 = {[{<<"$gte">>, <<"doc8">>}]},
    Selector = {[{<<"_id">>, GteDoc8}]},
    Req = {json_req, {[{<<"selector">>, Selector}]}},
    reset_row_notifications(),
    Consumer = spawn_consumer(DbName, ChArgs, Req),
    ?assertEqual(ok, wait_row_notifications(1)),
    ok = pause(Consumer),
    Rows = get_rows(Consumer),
    ?assertMatch([#row{seq = 10, id = <<"doc8">>, deleted = false}], Rows),
    clear_rows(Consumer),
    {ok, _} = save_doc(Db, {[{<<"_id">>, <<"doc01">>}]}),
    ok = unpause(Consumer),
    timer:sleep(100),
    ok = pause(Consumer),
    ?assertEqual([], get_rows(Consumer)),
    Rev4 = element(4, Revs),
    Rev8 = element(10, Revs),
    {ok, _} = save_doc(Db, {[{<<"_id">>, <<"doc8">>}, {<<"_rev">>, Rev8}]}),
    {ok, _} = save_doc(Db, {[{<<"_id">>, <<"doc4">>}, {<<"_rev">>, Rev4}]}),
    reset_row_notifications(),
    ok = unpause(Consumer),
    ?assertEqual(ok, wait_row_notifications(1)),
    ok = pause(Consumer),
    NewRows = get_rows(Consumer),
    ?assertMatch([#row{seq = _, id = <<"doc8">>, deleted = false}], NewRows),
    ?assertMatch([#row{seq = 12, id = <<"doc8">>, deleted = false}], NewRows).

t_stop_selector_when_db_deleted({DbName, _Revs}) ->
    {ok, _Db} = couch_db:open_int(DbName, []),
    ChArgs = #changes_args{filter = "_selector", feed = "continuous"},
    Selector = {[{<<"_id">>, <<"doc3">>}]},
    Req = {json_req, {[{<<"selector">>, Selector}]}},
    Consumer = spawn_consumer(DbName, ChArgs, Req),
    ok = pause(Consumer),
    ok = delete_db(DbName),
    ok = unpause(Consumer),
    {_Rows, _LastSeq} = wait_finished(Consumer),
    ok = stop_consumer(Consumer).

t_select_with_empty_fields({DbName, Revs}) ->
    ChArgs = #changes_args{filter = "_selector", include_docs = true},
    Selector = {[{<<"_id">>, <<"doc3">>}]},
    Req = {json_req, {[{<<"selector">>, Selector}, {<<"fields">>, []}]}},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(1, length(Rows)),
    Rev3_2 = element(6, Revs),
    Doc = {[{<<"_id">>, <<"doc3">>}, {<<"_rev">>, Rev3_2}]},
    ?assertMatch([#row{seq = 6, id = <<"doc3">>, doc = Doc}], Rows),
    ?assertEqual(UpSeq, LastSeq).

t_select_with_fields({DbName, _}) ->
    ChArgs = #changes_args{filter = "_selector", include_docs = true},
    Selector = {[{<<"_id">>, <<"doc3">>}]},
    Req = {json_req, {[{<<"selector">>, Selector}, {<<"fields">>, [<<"_id">>, <<"nope">>]}]}},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(1, length(Rows)),
    ?assertMatch([#row{seq = 6, id = <<"doc3">>, doc = {[{<<"_id">>, <<"doc3">>}]}}], Rows),
    ?assertEqual(UpSeq, LastSeq).

t_emit_only_design_documents({DbName, Revs}) ->
    ChArgs = #changes_args{filter = "_design"},
    Req = {json_req, null},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),

    ?assertEqual(1, length(Rows)),
    ?assertEqual(UpSeq, LastSeq),
    ?assertMatch([#row{seq = 8, id = <<"_design/foo">>}], Rows),

    {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX]),
    {ok, _} = save_doc(
        Db,
        {[
            {<<"_id">>, <<"_design/foo">>},
            {<<"_rev">>, element(8, Revs)},
            {<<"_deleted">>, true}
        ]}
    ),

    couch_db:close(Db),
    {Rows2, LastSeq2, UpSeq2} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(1, length(Rows2)),
    ?assertEqual(UpSeq2, LastSeq2),
    ?assertMatch([#row{seq = 11, id = <<"_design/foo">>, deleted = true}], Rows2).

t_receive_heartbeats(_) ->
    DbName = ?tempdb(),
    Timeout = 100,
    {ok, Db} = create_db(DbName),

    {ok, _} = save_doc(
        Db,
        {[
            {<<"_id">>, <<"_design/filtered">>},
            {<<"language">>, <<"javascript">>},
            {<<"filters">>,
                {[
                    {<<"foo">>, <<
                        "function(doc) {"
                        " return ['doc10', 'doc11', 'doc12'].indexOf(doc._id) != -1; }"
                    >>}
                ]}}
        ]}
    ),

    ChangesArgs = #changes_args{
        filter = "filtered/foo",
        feed = "continuous",
        timeout = 10000,
        heartbeat = 1000
    },
    Consumer = spawn_consumer(DbName, ChangesArgs, {json_req, null}),

    {ok, _Rev1} = save_doc(Db, {[{<<"_id">>, <<"doc1">>}]}),
    timer:sleep(Timeout),
    {ok, _Rev2} = save_doc(Db, {[{<<"_id">>, <<"doc2">>}]}),
    timer:sleep(Timeout),
    {ok, _Rev3} = save_doc(Db, {[{<<"_id">>, <<"doc3">>}]}),
    timer:sleep(Timeout),
    {ok, _Rev4} = save_doc(Db, {[{<<"_id">>, <<"doc4">>}]}),
    timer:sleep(Timeout),
    {ok, _Rev5} = save_doc(Db, {[{<<"_id">>, <<"doc5">>}]}),
    timer:sleep(Timeout),
    {ok, _Rev6} = save_doc(Db, {[{<<"_id">>, <<"doc6">>}]}),
    timer:sleep(Timeout),
    {ok, _Rev7} = save_doc(Db, {[{<<"_id">>, <<"doc7">>}]}),
    timer:sleep(Timeout),
    {ok, _Rev8} = save_doc(Db, {[{<<"_id">>, <<"doc8">>}]}),
    timer:sleep(Timeout),
    {ok, _Rev9} = save_doc(Db, {[{<<"_id">>, <<"doc9">>}]}),

    Heartbeats = get_heartbeats(Consumer),
    ?assert(Heartbeats > 0),

    {ok, _Rev10} = save_doc(Db, {[{<<"_id">>, <<"doc10">>}]}),
    timer:sleep(Timeout),
    {ok, _Rev11} = save_doc(Db, {[{<<"_id">>, <<"doc11">>}]}),
    timer:sleep(Timeout),
    {ok, _Rev12} = save_doc(Db, {[{<<"_id">>, <<"doc12">>}]}),

    Heartbeats2 = get_heartbeats(Consumer),
    ?assert(Heartbeats2 > Heartbeats),

    Rows = get_rows(Consumer),
    ?assertEqual(3, length(Rows)),

    {ok, _Rev13} = save_doc(Db, {[{<<"_id">>, <<"doc13">>}]}),
    timer:sleep(Timeout),
    {ok, _Rev14} = save_doc(Db, {[{<<"_id">>, <<"doc14">>}]}),
    timer:sleep(Timeout),

    Heartbeats3 = get_heartbeats(Consumer),
    ?assert(Heartbeats3 > Heartbeats2).

t_filter_by_doc_attribute({DbName, _}) ->
    DDocId = <<"_design/app">>,
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DDocId},
            {<<"language">>, <<"javascript">>},
            {<<"filters">>,
                {[
                    {<<"valid">>, <<
                        "function(doc, req) {"
                        " if (doc._id == 'doc3') {"
                        " return true; "
                        "} }"
                    >>}
                ]}}
        ]}
    ),
    ChArgs = #changes_args{filter = "app/valid"},
    Req = {json_req, null},
    ok = update_ddoc(DbName, DDoc),
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(1, length(Rows)),
    ?assertMatch([#row{seq = 6, id = <<"doc3">>}], Rows),
    ?assertEqual(UpSeq, LastSeq).

t_filter_by_user_ctx({DbName, _}) ->
    DDocId = <<"_design/app">>,
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DDocId},
            {<<"language">>, <<"javascript">>},
            {<<"filters">>,
                {[
                    {<<"valid">>, <<
                        "function(doc, req) {"
                        " if (req.userCtx.name == doc._id) {"
                        " return true; "
                        "} }"
                    >>}
                ]}}
        ]}
    ),
    ChArgs = #changes_args{filter = "app/valid"},
    UserCtx = #user_ctx{name = <<"doc3">>, roles = []},
    {ok, DbRec} = couch_db:clustered_db(DbName, UserCtx),
    Req = {json_req, {[{<<"userCtx">>, couch_util:json_user_ctx(DbRec)}]}},
    ok = update_ddoc(DbName, DDoc),
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(1, length(Rows)),
    ?assertMatch([#row{seq = 6, id = <<"doc3">>}], Rows),
    ?assertEqual(UpSeq, LastSeq).

t_filter_by_view({DbName, _}) ->
    DDocId = <<"_design/app">>,
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DDocId},
            {<<"language">>, <<"javascript">>},
            {<<"views">>,
                {[
                    {<<"valid">>,
                        {[
                            {<<"map">>, <<
                                "function(doc) {"
                                " if (doc._id == 'doc3') {"
                                " emit(doc); "
                                "} }"
                            >>}
                        ]}}
                ]}}
        ]}
    ),
    ChArgs = #changes_args{filter = "_view"},
    Req = {json_req, {[{<<"query">>, {[{<<"view">>, <<"app/valid">>}]}}]}},
    ok = update_ddoc(DbName, DDoc),
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(1, length(Rows)),
    ?assertMatch([#row{seq = 6, id = <<"doc3">>}], Rows),
    ?assertEqual(UpSeq, LastSeq).

t_filter_by_erlang_view({DbName, _}) ->
    DDocId = <<"_design/app">>,
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DDocId},
            {<<"language">>, <<"erlang">>},
            {<<"views">>,
                {[
                    {<<"valid">>,
                        {[
                            {<<"map">>, <<
                                "fun({Doc}) ->"
                                " case lists:keyfind(<<\"_id\">>, 1, Doc) of"
                                "   {<<\"_id\">>, <<\"doc3\">>} ->  Emit(Doc, null);"
                                "   false -> ok"
                                " end "
                                "end."
                            >>}
                        ]}}
                ]}}
        ]}
    ),
    ChArgs = #changes_args{filter = "_view"},
    Req = {json_req, {[{<<"query">>, {[{<<"view">>, <<"app/valid">>}]}}]}},
    ok = update_ddoc(DbName, DDoc),
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(1, length(Rows)),
    ?assertMatch([#row{seq = 6, id = <<"doc3">>}], Rows),
    ?assertEqual(UpSeq, LastSeq).

t_style_main_only({DbName, _}) ->
    ChArgs = #changes_args{style = main_only},
    Req = {json_req, null},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(9, length(Rows)),
    ?assertEqual(UpSeq, LastSeq),
    ?assertMatch(
        [
            #row{seq = 1, id = <<"doc1">>},
            #row{seq = 2, id = <<"doc2">>},
            #row{seq = 4, id = <<"doc4">>},
            #row{seq = 5, id = <<"doc5">>},
            #row{seq = 6, id = <<"doc3">>},
            #row{seq = 7, id = <<"doc6">>},
            #row{seq = 8, id = <<"_design/foo">>},
            #row{seq = 9, id = <<"doc7">>},
            #row{seq = 10, id = <<"doc8">>}
        ],
        Rows
    ).

t_style_main_only_with_include_docs({DbName, Revs}) ->
    ChArgs = #changes_args{style = main_only, include_docs = true},
    Req = {json_req, null},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(9, length(Rows)),
    ?assertEqual(UpSeq, LastSeq),
    FirstRev = element(1, Revs),
    [FirstRow | _] = Rows,
    ?assertMatch(
        #row{
            seq = 1,
            id = <<"doc1">>,
            doc = {[{<<"_id">>, <<"doc1">>}, {<<"_rev">>, FirstRev}]}
        },
        FirstRow
    ).

t_style_all_docs({DbName, _}) ->
    ChArgs = #changes_args{style = all_docs},
    Req = {json_req, null},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(9, length(Rows)),
    ?assertEqual(UpSeq, LastSeq),
    ?assertMatch(
        [
            #row{seq = 1, id = <<"doc1">>},
            #row{seq = 2, id = <<"doc2">>},
            #row{seq = 4, id = <<"doc4">>},
            #row{seq = 5, id = <<"doc5">>},
            #row{seq = 6, id = <<"doc3">>},
            #row{seq = 7, id = <<"doc6">>},
            #row{seq = 8, id = <<"_design/foo">>},
            #row{seq = 9, id = <<"doc7">>},
            #row{seq = 10, id = <<"doc8">>}
        ],
        Rows
    ).

t_style_all_docs_with_include_docs({DbName, Revs}) ->
    ChArgs = #changes_args{style = all_docs, include_docs = true},
    Req = {json_req, null},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(9, length(Rows)),
    ?assertEqual(UpSeq, LastSeq),
    FirstRev = element(1, Revs),
    [FirstRow | _] = Rows,
    ?assertMatch(
        #row{
            seq = 1,
            id = <<"doc1">>,
            doc = {[{<<"_id">>, <<"doc1">>}, {<<"_rev">>, FirstRev}]}
        },
        FirstRow
    ).

t_style_main_only_with_revtree({DbName, _}) ->
    ChArgs = #changes_args{style = main_only},
    Req = {json_req, null},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(2, length(Rows)),
    ?assertEqual(4, LastSeq),
    ?assertEqual(4, UpSeq),
    ?assertMatch(
        [
            #row{
                seq = 3,
                id = <<"doc1">>,
                doc = nil,
                revs = [<<"2-y">>]
            },
            #row{
                seq = 4,
                id = <<"doc2">>,
                deleted = true,
                doc = nil,
                revs = [<<"1-m">>]
            }
        ],
        Rows
    ).

t_style_main_only_with_include_docs_with_revtree({DbName, _}) ->
    ChArgs = #changes_args{
        style = main_only,
        include_docs = true,
        conflicts = true
    },
    Req = {json_req, null},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(2, length(Rows)),
    ?assertEqual(4, LastSeq),
    ?assertEqual(4, UpSeq),
    ?assertMatch(
        [
            #row{
                seq = 3,
                id = <<"doc1">>,
                doc =
                    {[
                        {<<"_id">>, <<"doc1">>},
                        {<<"_rev">>, <<"2-y">>},
                        {<<"_conflicts">>, [<<"2-x">>]}
                    ]},
                revs = [<<"2-y">>]
            },
            #row{
                seq = 4,
                id = <<"doc2">>,
                deleted = true,
                doc =
                    {[
                        {<<"_id">>, <<"doc2">>},
                        {<<"_rev">>, <<"1-m">>},
                        {<<"_deleted">>, true}
                    ]},
                revs = [<<"1-m">>]
            }
        ],
        Rows
    ).

t_style_all_docs_with_revtree({DbName, _}) ->
    ChArgs = #changes_args{style = all_docs},
    Req = {json_req, null},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(2, length(Rows)),
    ?assertEqual(4, UpSeq),
    ?assertEqual(4, LastSeq),
    ?assertMatch(
        [
            #row{
                seq = 3,
                id = <<"doc1">>,
                revs = [<<"2-y">>, <<"2-x">>]
            },
            #row{
                seq = 4,
                id = <<"doc2">>,
                deleted = true,
                revs = [<<"1-m">>, <<"1-l">>]
            }
        ],
        Rows
    ).

t_style_all_docs_with_include_docs_with_revtree({DbName, _}) ->
    ChArgs = #changes_args{
        style = all_docs,
        include_docs = true,
        conflicts = true
    },
    Req = {json_req, null},
    {Rows, LastSeq, UpSeq} = run_changes_query(DbName, ChArgs, Req),
    ?assertEqual(2, length(Rows)),
    ?assertEqual(4, LastSeq),
    ?assertEqual(4, UpSeq),
    ?assertMatch(
        [
            #row{
                seq = 3,
                id = <<"doc1">>,
                doc =
                    {[
                        {<<"_id">>, <<"doc1">>},
                        {<<"_rev">>, <<"2-y">>},
                        {<<"_conflicts">>, [<<"2-x">>]}
                    ]},
                revs = [<<"2-y">>, <<"2-x">>]
            },
            #row{
                seq = 4,
                id = <<"doc2">>,
                deleted = true,
                doc =
                    {[
                        {<<"_id">>, <<"doc2">>},
                        {<<"_rev">>, <<"1-m">>},
                        {<<"_deleted">>, true}
                    ]},
                revs = [<<"1-m">>, <<"1-l">>]
            }
        ],
        Rows
    ).

%%%%%%%%%%%%%%%%%%%% Utility Functions %%%%%%%%%%%%%%%%%%%%
update_ddoc(DbName, DDoc) ->
    {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX]),
    {ok, _} = couch_db:update_doc(Db, DDoc, []),
    couch_db:close(Db).

run_changes_query(DbName, ChangesArgs, Opts) ->
    Consumer = spawn_consumer(DbName, ChangesArgs, Opts),
    {Rows, LastSeq} = wait_finished(Consumer),
    {ok, Db} = couch_db:open_int(DbName, []),
    UpSeq = couch_db:get_update_seq(Db),
    couch_db:close(Db),
    stop_consumer(Consumer),
    {Rows, LastSeq, UpSeq}.

save_doc(Db, Json) ->
    Doc = couch_doc:from_json_obj(Json),
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    {ok, couch_doc:rev_to_str(Rev)}.

get_rows({Consumer, _}) ->
    Ref = make_ref(),
    Consumer ! {get_rows, Ref},
    Resp =
        receive
            {rows, Ref, Rows} ->
                Rows
        after ?TIMEOUT ->
            timeout
        end,
    ?assertNotEqual(timeout, Resp),
    Resp.

get_heartbeats({Consumer, _}) ->
    Ref = make_ref(),
    Consumer ! {get_heartbeats, Ref},
    Resp =
        receive
            {hearthbeats, Ref, HeartBeats} ->
                HeartBeats
        after ?TIMEOUT ->
            timeout
        end,
    ?assertNotEqual(timeout, Resp),
    Resp.

clear_rows({Consumer, _}) ->
    Ref = make_ref(),
    Consumer ! {reset, Ref},
    Resp =
        receive
            {ok, Ref} ->
                ok
        after ?TIMEOUT ->
            timeout
        end,
    ?assertNotEqual(timeout, Resp),
    Resp.

stop_consumer({Consumer, _}) ->
    Ref = make_ref(),
    Consumer ! {stop, Ref},
    Resp =
        receive
            {ok, Ref} ->
                ok
        after ?TIMEOUT ->
            timeout
        end,
    ?assertNotEqual(timeout, Resp),
    Resp.

pause({Consumer, _}) ->
    Ref = make_ref(),
    Consumer ! {pause, Ref},
    Resp =
        receive
            {paused, Ref} ->
                ok
        after ?TIMEOUT ->
            timeout
        end,
    ?assertNotEqual(timeout, Resp),
    Resp.

unpause({Consumer, _}) ->
    Ref = make_ref(),
    Consumer ! {continue, Ref},
    Resp =
        receive
            {ok, Ref} ->
                ok
        after ?TIMEOUT ->
            timeout
        end,
    ?assertNotEqual(timeout, Resp),
    Resp.

wait_finished({_, ConsumerRef}) ->
    receive
        {consumer_finished, Rows, LastSeq} ->
            {Rows, LastSeq};
        {'DOWN', ConsumerRef, _, _, Msg} when Msg == normal; Msg == ok ->
            ok;
        {'DOWN', ConsumerRef, _, _, Msg} ->
            erlang:error(
                {consumer_died, [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {value, Msg}
                ]}
            )
    after ?TIMEOUT ->
        erlang:error(
            {consumer_died, [
                {module, ?MODULE},
                {line, ?LINE},
                {value, timeout}
            ]}
        )
    end.

reset_row_notifications() ->
    receive
        row ->
            reset_row_notifications()
    after 0 ->
        ok
    end.

wait_row_notifications(N) ->
    receive
        row when N == 1 ->
            ok;
        row when N > 1 ->
            wait_row_notifications(N - 1)
    after ?TIMEOUT ->
        timeout
    end.

spawn_consumer(DbName, ChangesArgs0, Req) ->
    Parent = self(),
    spawn_monitor(fun() ->
        put(heartbeat_count, 0),
        Callback = fun
            ({change, {Change}, _}, _, Acc) ->
                Id = couch_util:get_value(<<"id">>, Change),
                Seq = couch_util:get_value(<<"seq">>, Change),
                Del = couch_util:get_value(<<"deleted">>, Change, false),
                Doc = couch_util:get_value(doc, Change, nil),
                Revs = couch_util:get_value(<<"changes">>, Change, []),
                Revs1 = lists:map(fun({[{<<"rev">>, Rev}]}) -> Rev end, Revs),
                Parent ! row,
                [#row{id = Id, seq = Seq, deleted = Del, doc = Doc, revs = Revs1} | Acc];
            ({stop, LastSeq}, _, Acc) ->
                Parent ! {consumer_finished, lists:reverse(Acc), LastSeq},
                stop_loop(Parent, Acc);
            (timeout, _, Acc) ->
                put(heartbeat_count, get(heartbeat_count) + 1),
                maybe_pause(Parent, Acc);
            (_, _, Acc) ->
                maybe_pause(Parent, Acc)
        end,
        {ok, Db} = couch_db:open_int(DbName, []),
        ChangesArgs =
            case
                (ChangesArgs0#changes_args.timeout =:= undefined) andalso
                    (ChangesArgs0#changes_args.heartbeat =:= undefined)
            of
                true ->
                    ChangesArgs0#changes_args{timeout = 1000, heartbeat = 100};
                false ->
                    ChangesArgs0
            end,
        FeedFun = couch_changes:handle_db_changes(ChangesArgs, Req, Db),
        try
            FeedFun({Callback, []})
        catch
            throw:{stop, _} -> ok;
            _:Error -> exit(Error)
        after
            couch_db:close(Db)
        end
    end).

maybe_pause(Parent, Acc) ->
    receive
        {get_rows, Ref} ->
            Parent ! {rows, Ref, lists:reverse(Acc)},
            maybe_pause(Parent, Acc);
        {get_heartbeats, Ref} ->
            Parent ! {hearthbeats, Ref, get(heartbeat_count)},
            maybe_pause(Parent, Acc);
        {reset, Ref} ->
            Parent ! {ok, Ref},
            maybe_pause(Parent, []);
        {pause, Ref} ->
            Parent ! {paused, Ref},
            pause_loop(Parent, Acc);
        {stop, Ref} ->
            Parent ! {ok, Ref},
            throw({stop, Acc});
        V when V /= updated ->
            erlang:error(
                {assertion_failed, [
                    {module, ?MODULE},
                    {line, ?LINE},
                    {value, V},
                    {reason, "Received unexpected message"}
                ]}
            )
    after 0 ->
        Acc
    end.

pause_loop(Parent, Acc) ->
    receive
        {stop, Ref} ->
            Parent ! {ok, Ref},
            throw({stop, Acc});
        {reset, Ref} ->
            Parent ! {ok, Ref},
            pause_loop(Parent, []);
        {continue, Ref} ->
            Parent ! {ok, Ref},
            Acc;
        {get_rows, Ref} ->
            Parent ! {rows, Ref, lists:reverse(Acc)},
            pause_loop(Parent, Acc)
    end.

stop_loop(Parent, Acc) ->
    receive
        {get_rows, Ref} ->
            Parent ! {rows, Ref, lists:reverse(Acc)},
            stop_loop(Parent, Acc);
        {stop, Ref} ->
            Parent ! {ok, Ref},
            Acc
    end.

create_db(DbName) ->
    couch_db:create(DbName, [?ADMIN_CTX, overwrite]).

delete_db(DbName) ->
    couch_server:delete(DbName, [?ADMIN_CTX]).

setup_with_revtree() ->
    DbName = ?tempdb(),
    {ok, Db} = create_db(DbName),
    update_replicated(Db, [
        doc(<<"doc1">>, [<<"x">>, <<"z">>]),
        doc(<<"doc2">>, [<<"l">>], true)
    ]),
    {ok, Db1} = couch_db:reopen(Db),
    update_replicated(Db1, [
        doc(<<"doc1">>, [<<"y">>, <<"z">>]),
        doc(<<"doc2">>, [<<"m">>], true)
    ]),
    ok = couch_db:close(Db1),
    {DbName, []}.

doc(Id, Revs) ->
    doc(Id, Revs, false).

doc(Id, Revs, Deleted) ->
    #doc{
        id = Id,
        revs = {length(Revs), Revs},
        deleted = Deleted
    }.

update_replicated(Db, Docs) ->
    {ok, []} = couch_db:update_docs(Db, Docs, [], ?REPLICATED_CHANGES),
    ok.
