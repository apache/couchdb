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

-module(couch_views_map_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include("couch_views.hrl").


-define(TDEF(A), {atom_to_list(A), fun A/0}).


setup() ->
    test_util:start_couch([
            fabric,
            couch_jobs,
            couch_js,
            couch_views
        ]).


teardown(State) ->
    test_util:stop_couch(State).


map_views_test_() ->
    {
        "Map views",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF(should_map),
                ?TDEF(should_map_with_startkey),
                ?TDEF(should_map_with_endkey),
                ?TDEF(should_map_with_endkey_not_inclusive),
                ?TDEF(should_map_reverse_and_limit),
                ?TDEF(should_map_with_range_reverse),
                ?TDEF(should_map_with_limit_and_skip),
                ?TDEF(should_map_with_limit_and_skip_reverse),
                ?TDEF(should_map_with_include_docs),
                ?TDEF(should_map_with_include_docs_reverse),
                ?TDEF(should_map_with_startkey_with_key_array),
                ?TDEF(should_map_with_startkey_and_endkey_with_key_array),
                ?TDEF(should_map_empty_views),
                ?TDEF(should_map_duplicate_keys),
                ?TDEF(should_map_with_doc_emit),
                ?TDEF(should_map_update_is_false),
                ?TDEF(should_map_update_is_lazy),
                ?TDEF(should_map_wait_for_interactive),
                ?TDEF(should_map_local_seq)
                % fun should_give_ext_size_seq_indexed_test/1
            ]
        }
    }.


should_map() ->
    Result = run_query(<<"baz">>, #{}),
    Expect = {ok, [
        {row, [{id, <<"1">>}, {key, 1}, {value, 1}]},
        {row, [{id, <<"2">>}, {key, 2}, {value, 2}]},
        {row, [{id, <<"3">>}, {key, 3}, {value, 3}]},
        {row, [{id, <<"4">>}, {key, 4}, {value, 4}]},
        {row, [{id, <<"5">>}, {key, 5}, {value, 5}]},
        {row, [{id, <<"6">>}, {key, 6}, {value, 6}]},
        {row, [{id, <<"7">>}, {key, 7}, {value, 7}]},
        {row, [{id, <<"8">>}, {key, 8}, {value, 8}]},
        {row, [{id, <<"9">>}, {key, 9}, {value, 9}]},
        {row, [{id, <<"10">>}, {key, 10}, {value, 10}]}
    ]},
    ?assertEqual(Expect, Result).


should_map_with_startkey() ->
    Result = run_query(<<"baz">>, #{start_key => 4}),
    Expect = {ok, [
        {row, [{id, <<"4">>}, {key, 4}, {value, 4}]},
        {row, [{id, <<"5">>}, {key, 5}, {value, 5}]},
        {row, [{id, <<"6">>}, {key, 6}, {value, 6}]},
        {row, [{id, <<"7">>}, {key, 7}, {value, 7}]},
        {row, [{id, <<"8">>}, {key, 8}, {value, 8}]},
        {row, [{id, <<"9">>}, {key, 9}, {value, 9}]},
        {row, [{id, <<"10">>}, {key, 10}, {value, 10}]}
    ]},
    ?assertEqual(Expect, Result).


should_map_with_endkey() ->
    Result = run_query(<<"baz">>, #{end_key => 5}),
    Expect = {ok, [
        {row, [{id, <<"1">>}, {key, 1}, {value, 1}]},
        {row, [{id, <<"2">>}, {key, 2}, {value, 2}]},
        {row, [{id, <<"3">>}, {key, 3}, {value, 3}]},
        {row, [{id, <<"4">>}, {key, 4}, {value, 4}]},
        {row, [{id, <<"5">>}, {key, 5}, {value, 5}]}
    ]},
    ?assertEqual(Expect, Result).


should_map_with_endkey_not_inclusive() ->
    Result = run_query(<<"baz">>, #{
        end_key => 5,
        inclusive_end => false
    }),
    Expect = {ok, [
        {row, [{id, <<"1">>}, {key, 1}, {value, 1}]},
        {row, [{id, <<"2">>}, {key, 2}, {value, 2}]},
        {row, [{id, <<"3">>}, {key, 3}, {value, 3}]},
        {row, [{id, <<"4">>}, {key, 4}, {value, 4}]}
    ]},
    ?assertEqual(Expect, Result).


should_map_reverse_and_limit() ->
    Result = run_query(<<"baz">>, #{
        direction => rev,
        limit => 3
    }),
    Expect = {ok, [
        {row, [{id, <<"10">>}, {key, 10}, {value, 10}]},
        {row, [{id, <<"9">>}, {key, 9}, {value, 9}]},
        {row, [{id, <<"8">>}, {key, 8}, {value, 8}]}
    ]},
    ?assertEqual(Expect, Result).


should_map_with_range_reverse() ->
    Result = run_query(<<"baz">>, #{
        direction => rev,
        start_key => 5,
        end_key => 3,
        inclusive_end => true
    }),
    Expect = {ok, [
        {row, [{id, <<"5">>}, {key, 5}, {value, 5}]},
        {row, [{id, <<"4">>}, {key, 4}, {value, 4}]},
        {row, [{id, <<"3">>}, {key, 3}, {value, 3}]}
    ]},
    ?assertEqual(Expect, Result).


should_map_with_limit_and_skip() ->
    Result = run_query(<<"baz">>, #{
        start_key => 2,
        limit => 3,
        skip => 3
    }),
    Expect = {ok, [
        {row, [{id, <<"5">>}, {key, 5}, {value, 5}]},
        {row, [{id, <<"6">>}, {key, 6}, {value, 6}]},
        {row, [{id, <<"7">>}, {key, 7}, {value, 7}]}
    ]},
    ?assertEqual(Expect, Result).


should_map_with_limit_and_skip_reverse() ->
    Result = run_query(<<"baz">>, #{
        start_key => 10,
        limit => 3,
        skip => 3,
        direction => rev
    }),
    Expect = {ok, [
        {row, [{id, <<"7">>}, {key, 7}, {value, 7}]},
        {row, [{id, <<"6">>}, {key, 6}, {value, 6}]},
        {row, [{id, <<"5">>}, {key, 5}, {value, 5}]}
    ]},
    ?assertEqual(Expect, Result).


should_map_with_include_docs() ->
    Result = run_query(<<"baz">>, #{
        start_key => 8,
        end_key => 8,
        include_docs => true
    }),
    Doc = {[
        {<<"_id">>, <<"8">>},
        {<<"_rev">>, <<"1-55b9a29311341e07ec0a7ca13bc1b59f">>},
        {<<"val">>, 8}
    ]},
    Expect = {ok, [
        {row, [{id, <<"8">>}, {key, 8}, {value, 8}, {doc, Doc}]}
    ]},
    ?assertEqual(Expect, Result).


should_map_with_include_docs_reverse() ->
    Result = run_query(<<"baz">>, #{
        start_key => 8,
        end_key => 8,
        include_docs => true,
        direction => rev
    }),
    Doc = {[
        {<<"_id">>, <<"8">>},
        {<<"_rev">>, <<"1-55b9a29311341e07ec0a7ca13bc1b59f">>},
        {<<"val">>, 8}
    ]},
    Expect = {ok, [
        {row, [{id, <<"8">>}, {key, 8}, {value, 8}, {doc, Doc}]}
    ]},
    ?assertEqual(Expect, Result).


should_map_with_startkey_with_key_array() ->
    Rows = [
        {row, [{id, <<"4">>}, {key, [<<"4">>, 4]}, {value, 4}]},
        {row, [{id, <<"5">>}, {key, [<<"5">>, 5]}, {value, 5}]},
        {row, [{id, <<"6">>}, {key, [<<"6">>, 6]}, {value, 6}]},
        {row, [{id, <<"7">>}, {key, [<<"7">>, 7]}, {value, 7}]},
        {row, [{id, <<"8">>}, {key, [<<"8">>, 8]}, {value, 8}]},
        {row, [{id, <<"9">>}, {key, [<<"9">>, 9]}, {value, 9}]}
    ],

    Result = run_query(<<"boom">>, #{
        start_key => [<<"4">>]
    }),

    ?assertEqual({ok, Rows}, Result),

    ResultRev = run_query(<<"boom">>, #{
        start_key => [<<"9">>, 9],
        direction => rev,
        limit => 6
    }),

    ?assertEqual({ok, lists:reverse(Rows)}, ResultRev).


should_map_with_startkey_and_endkey_with_key_array() ->
    Rows1 = [
        {row, [{id, <<"4">>}, {key, [<<"4">>, 4]}, {value, 4}]},
        {row, [{id, <<"5">>}, {key, [<<"5">>, 5]}, {value, 5}]},
        {row, [{id, <<"6">>}, {key, [<<"6">>, 6]}, {value, 6}]},
        {row, [{id, <<"7">>}, {key, [<<"7">>, 7]}, {value, 7}]},
        {row, [{id, <<"8">>}, {key, [<<"8">>, 8]}, {value, 8}]}
    ],

    Rows2 = [
        {row, [{id, <<"4">>}, {key, [<<"4">>, 4]}, {value, 4}]},
        {row, [{id, <<"5">>}, {key, [<<"5">>, 5]}, {value, 5}]},
        {row, [{id, <<"6">>}, {key, [<<"6">>, 6]}, {value, 6}]},
        {row, [{id, <<"7">>}, {key, [<<"7">>, 7]}, {value, 7}]},
        {row, [{id, <<"8">>}, {key, [<<"8">>, 8]}, {value, 8}]},
        {row, [{id, <<"9">>}, {key, [<<"9">>, 9]}, {value, 9}]}
    ],

    Result = run_query(<<"boom">>, #{
        start_key => [<<"4">>],
        end_key => [<<"8">>, []]
    }),

    ?assertEqual({ok, Rows1}, Result),

    ResultRev = run_query(<<"boom">>, #{
        start_key => [<<"8">>, []],
        end_key => [<<"4">>],
        direction => rev
    }),

    ?assertEqual({ok, lists:reverse(Rows1)}, ResultRev),

    ResultRev2 = run_query(<<"boom">>, #{
        start_key => [<<"9">>, 9],
        end_key => [<<"4">>],
        direction => rev,
        inclusive_end => false
    }),

    % Here, [<<"4">>] is less than [<<"4">>, 4] so we
    % expect rows 9-4
    ?assertEqual({ok, lists:reverse(Rows2)}, ResultRev2),

    ResultRev3 = run_query(<<"boom">>, #{
        start_key => [<<"9">>, 9],
        end_key => [<<"4">>, 4],
        direction => rev,
        inclusive_end => false
    }),

    % Here, specifying [<<"4">>, 4] as the key will prevent
    % us from including that row which leaves rows 9-5
    ?assertEqual({ok, lists:reverse(lists:nthtail(1, Rows2))}, ResultRev3).


should_map_empty_views() ->
    Result = run_query(<<"bing">>, #{}),
    Expect = {ok, []},
    ?assertEqual(Expect, Result).


should_map_with_doc_emit() ->
    Result = run_query(<<"doc_emit">>, #{
        start_key => 8,
        limit => 1
    }),
    Doc = {[
        {<<"_id">>, <<"8">>},
        {<<"_rev">>, <<"1-55b9a29311341e07ec0a7ca13bc1b59f">>},
        {<<"val">>, 8}
    ]},
    Expect = {ok, [
        {row, [{id, <<"8">>}, {key, 8}, {value, Doc}]}
    ]},
    ?assertEqual(Expect, Result).


should_map_duplicate_keys() ->
    Result = run_query(<<"duplicate_keys">>, #{
        limit => 6
    }),
    Expect = {ok, [
        {row, [{id, <<"1">>}, {key, <<"1">>}, {value, 1}]},
        {row, [{id, <<"1">>}, {key, <<"1">>}, {value, 2}]},
        {row, [{id, <<"10">>}, {key, <<"10">>}, {value, 10}]},
        {row, [{id, <<"10">>}, {key, <<"10">>}, {value, 11}]},
        {row, [{id, <<"2">>}, {key, <<"2">>}, {value, 2}]},
        {row, [{id, <<"2">>}, {key, <<"2">>}, {value, 3}]}
    ]},
    ?assertEqual(Expect, Result).


should_map_update_is_false() ->
    Expect = {ok, [
        {row, [{id, <<"8">>}, {key, 8}, {value, 8}]},
        {row, [{id, <<"9">>}, {key, 9}, {value, 9}]},
        {row, [{id, <<"10">>}, {key, 10}, {value, 10}]}
    ]},

    Expect1 = {ok, [
        {row, [{id, <<"8">>}, {key, 8}, {value, 8}]},
        {row, [{id, <<"9">>}, {key, 9}, {value, 9}]},
        {row, [{id, <<"10">>}, {key, 10}, {value, 10}]},
        {row, [{id, <<"11">>}, {key, 11}, {value, 11}]}
    ]},

    Idx = <<"baz">>,
    DbName = ?tempdb(),

    {ok, Db} = fabric2_db:create(DbName, [{user_ctx, ?ADMIN_USER}]),

    DDoc = create_ddoc(),
    Docs = make_docs(10),
    fabric2_db:update_docs(Db, [DDoc | Docs]),

    Args1 = #{
        start_key => 8
    },

    Result1 = couch_views:query(Db, DDoc, Idx, fun default_cb/2,
        [], Args1),
    ?assertEqual(Expect, Result1),

    Doc = doc(11),
    fabric2_db:update_doc(Db, Doc),

    Args2 = #{
        start_key => 8,
        update => false
    },

    Result2 = couch_views:query(Db, DDoc, Idx, fun default_cb/2,
        [], Args2),
    ?assertEqual(Expect, Result2),

    Result3 = couch_views:query(Db, DDoc, Idx, fun default_cb/2,
        [], Args1),
    ?assertEqual(Expect1, Result3).


should_map_update_is_lazy() ->
    Expect = {ok, [
        {row, [{id, <<"8">>}, {key, 8}, {value, 8}]},
        {row, [{id, <<"9">>}, {key, 9}, {value, 9}]},
        {row, [{id, <<"10">>}, {key, 10}, {value, 10}]}
    ]},

    Idx = <<"baz">>,
    DbName = ?tempdb(),

    {ok, Db} = fabric2_db:create(DbName, [{user_ctx, ?ADMIN_USER}]),

    DDoc = create_ddoc(),
    Docs = make_docs(10),

    fabric2_db:update_docs(Db, [DDoc | Docs]),

    Args1 = #{
        start_key => 8,
        update => lazy
    },

    Result1 = couch_views:query(Db, DDoc, Idx, fun default_cb/2,
        [], Args1),
    ?assertEqual({ok, []}, Result1),

    {ok, Mrst} = couch_views_util:ddoc_to_mrst(DbName, DDoc),
    JobId = couch_views_jobs:job_id(Db, Mrst),
    UpdateSeq = fabric2_db:get_update_seq(Db),
    ok = couch_views_jobs:wait_for_job(JobId, DDoc#doc.id, UpdateSeq),

    Args2 = #{
        start_key => 8,
        update => false
    },

    Result2 = couch_views:query(Db, DDoc, Idx, fun default_cb/2,
        [], Args2),
    ?assertEqual(Expect, Result2).


should_map_wait_for_interactive() ->
    DbName = ?tempdb(),
    {ok, Db} = fabric2_db:create(DbName, [{user_ctx, ?ADMIN_USER}]),

    DDoc = create_interactive_ddoc(),
    Docs = make_docs(101),

    fabric2_db:update_docs(Db, Docs),
    fabric2_db:update_docs(Db, [DDoc]),

    Result = couch_views:query(Db, DDoc, <<"idx_01">>, fun default_cb/2, [],
        #{limit => 3}),
    ?assertEqual({ok, [
        {row, [{id, <<"1">>}, {key, 1}, {value, 1}]},
        {row, [{id, <<"2">>}, {key, 2}, {value, 2}]},
        {row, [{id, <<"3">>}, {key, 3}, {value, 3}]}
    ]}, Result).


should_map_local_seq() ->
    ExpectedTrue = [
        {row, [{id, <<"1">>}, {key, 1}, {value, 1}]},
        {row, [{id, <<"2">>}, {key, 2}, {value, 2}]},
        {row, [{id, <<"3">>}, {key, 3}, {value, 3}]}
    ],
    check_local_seq(true, ExpectedTrue),

    ExpectedFalse = [],
    check_local_seq(false, ExpectedFalse),

    Error = {bad_request,invalid_design_doc,
        <<"`options.local_seq` field must have boolean type">>},
    ?assertThrow(Error, check_local_seq(something_else, null)).


check_local_seq(Val, Expected) ->
    DbName = ?tempdb(),
    {ok, Db} = fabric2_db:create(DbName, [{user_ctx, ?ADMIN_USER}]),

    DDoc = create_local_seq_ddoc(Val),
    Docs = make_docs(5),
    fabric2_db:update_docs(Db, [DDoc | Docs]),

    {ok, Result} = couch_views:query(Db, DDoc, <<"idx_01">>, fun default_cb/2, [],
        #{limit => 3}),

    ?assertEqual(Expected, Result).


% should_give_ext_size_seq_indexed_test(Db) ->
%     DDoc = couch_doc:from_json_obj({[
%         {<<"_id">>, <<"_design/seqdoc">>},
%         {<<"options">>, {[{<<"seq_indexed">>, true}]}},
%         {<<"views">>, {[
%                 {<<"view1">>, {[
%                     {<<"map">>, <<"function(doc){emit(doc._id, doc._id);}">>}
%                 ]}}
%             ]}
%         }
%     ]}),
%     {ok, _} = couch_db:update_doc(Db, DDoc, []),
%     {ok, Db1} = couch_db:open_int(couch_db:name(Db), []),
%     {ok, DDoc1} = couch_db:open_doc(Db1, <<"_design/seqdoc">>, [ejson_body]),
%     couch_mrview:query_view(Db1, DDoc1, <<"view1">>, [{update, true}]),
%     {ok, Info} = couch_mrview:get_info(Db1, DDoc),
%     Size = couch_util:get_nested_json_value({Info}, [sizes, external]),
%     ok = couch_db:close(Db1),
%     ?assert(is_number(Size)).


run_query(Idx, Args) ->
    run_query(Idx, Args, false).


run_query(Idx, Args, DebugCluster) ->
    DbName = ?tempdb(),
    {ok, Db} = fabric2_db:create(DbName, [{user_ctx, ?ADMIN_USER}]),
    DDoc = create_ddoc(),
    Docs = make_docs(10),
    fabric2_db:update_docs(Db, [DDoc | Docs]),
    if not DebugCluster -> ok; true ->
        couch_views:query(Db, DDoc, Idx, fun default_cb/2, [], #{}),
        fabric2_fdb:debug_cluster(),
        ok
    end,
    couch_views:query(Db, DDoc, Idx, fun default_cb/2, [], Args).


default_cb(complete, Acc) ->
    {ok, lists:reverse(Acc)};
default_cb({final, Info}, []) ->
    {ok, [Info]};
default_cb({final, _}, Acc) ->
    {ok, Acc};
default_cb({meta, _}, Acc) ->
    {ok, Acc};
default_cb(ok, ddoc_updated) ->
    {ok, ddoc_updated};
default_cb(Row, Acc) ->
    {ok, [Row | Acc]}.


create_ddoc() ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/bar">>},
        {<<"views">>, {[
            {<<"baz">>, {[
                {<<"map">>, <<"function(doc) {emit(doc.val, doc.val);}">>}
            ]}},
            {<<"boom">>, {[
                {<<"map">>, <<
                    "function(doc) {\n"
                    "   emit([doc.val.toString(), doc.val], doc.val);\n"
                    "}"
                >>}
            ]}},
            {<<"bing">>, {[
                {<<"map">>, <<"function(doc) {}">>}
            ]}},
            {<<"doc_emit">>, {[
                {<<"map">>, <<"function(doc) {emit(doc.val, doc)}">>}
            ]}},
            {<<"duplicate_keys">>, {[
                {<<"map">>, <<
                    "function(doc) {\n"
                    "   emit(doc._id, doc.val);\n"
                    "   emit(doc._id, doc.val + 1);\n"
                    "}">>}
            ]}},
            {<<"zing">>, {[
                {<<"map">>, <<
                    "function(doc) {\n"
                    "  if(doc.foo !== undefined)\n"
                    "    emit(doc.foo, 0);\n"
                    "}"
                >>}
            ]}}
        ]}}
    ]}).

create_interactive_ddoc() ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/ddoc_interactive">>},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
            {<<"idx_01">>, {[
                {<<"map">>, <<
                    "function(doc) {"
                        "if (doc.val) {"
                            "emit(doc.val, doc.val);"
                        "}"
                    "}">>}
            ]}}
        ]}},
        {<<"autoupdate">>, false},
        {<<"interactive">>, true}
    ]}).


create_local_seq_ddoc(Val) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/ddoc_local_seq">>},
        {<<"options">>, {[{<<"local_seq">>, Val}]}},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, {[
            {<<"idx_01">>, {[
                {<<"map">>, <<
                    "function(doc) {"
                        "if (doc._local_seq) {"
                            "emit(doc.val, doc.val);"
                        "}"
                    "}">>}
            ]}}
        ]}}
    ]}).


make_docs(Count) ->
    [doc(I) || I <- lists:seq(1, Count)].


doc(Id) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, list_to_binary(integer_to_list(Id))},
        {<<"val">>, Id}
    ]}).
