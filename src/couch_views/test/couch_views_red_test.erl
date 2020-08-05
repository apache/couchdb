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

-module(couch_views_red_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include("couch_views.hrl").


-define(TDEF(A), {atom_to_list(A), fun A/1}).
-define(TDEFI(A), {atom_to_list(A), fun A/0}).


with(Tests) ->
    fun(ArgsTuple) ->
        lists:map(fun({Name, Fun}) ->
            {Name, ?_test(Fun(ArgsTuple))}
        end, Tests)
    end.


-define(NUM_DOCS, 2000).


reduce_views_shraed_db_test_() ->
    {
        "Reduce views",
        {
            setup,
            fun setup_shared_db/0,
            fun teardown_shared_db/1,
            with([
                ?TDEF(should_reduce),
                ?TDEF(should_reduce_rev),
                ?TDEF(should_reduce_start_key),
                ?TDEF(should_reduce_start_key_rev),
                ?TDEF(should_reduce_end_key),
                ?TDEF(should_reduce_end_key_rev),
                ?TDEF(should_reduce_inclusive_end_false),
                ?TDEF(should_reduce_inclusive_end_false_rev),
                ?TDEF(should_reduce_start_and_end_key),
                ?TDEF(should_reduce_start_and_end_key_rev),
                ?TDEF(should_reduce_empty_range),
                ?TDEF(should_reduce_empty_range_rev),
                ?TDEF(should_reduce_grouped),
                ?TDEF(should_reduce_grouped_rev),
                ?TDEF(should_reduce_grouped_start_key),
                ?TDEF(should_reduce_grouped_start_key_rev),
                ?TDEF(should_reduce_grouped_end_key),
                ?TDEF(should_reduce_grouped_end_key_rev),
                ?TDEF(should_reduce_grouped_inclusive_end_false),
                ?TDEF(should_reduce_grouped_inclusive_end_false_rev),
                ?TDEF(should_reduce_grouped_start_and_end_key),
                ?TDEF(should_reduce_grouped_start_and_end_key_rev),
                ?TDEF(should_reduce_grouped_empty_range),
                ?TDEF(should_reduce_grouped_empty_range_rev),

                ?TDEF(should_reduce_array_keys),
                ?TDEF(should_reduce_grouped_array_keys),
                ?TDEF(should_reduce_group_1_array_keys),
                ?TDEF(should_reduce_group_1_array_keys_start_key),
                ?TDEF(should_reduce_group_1_array_keys_start_key_rev),
                ?TDEF(should_reduce_group_1_array_keys_end_key),
                ?TDEF(should_reduce_group_1_array_keys_end_key_rev),
                ?TDEF(should_reduce_group_1_array_keys_inclusive_end_false),
                ?TDEF(should_reduce_group_1_array_keys_inclusive_end_false_rev),
                ?TDEF(should_reduce_group_1_array_keys_start_and_end_key),
                ?TDEF(should_reduce_group_1_array_keys_start_and_end_key_rev),
                ?TDEF(should_reduce_group_1_array_keys_sub_array_select),
                ?TDEF(should_reduce_group_1_array_keys_sub_array_select_rev),
                ?TDEF(should_reduce_group_1_array_keys_sub_array_inclusive_end),
                ?TDEF(should_reduce_group_1_array_keys_empty_range),
                ?TDEF(should_reduce_group_1_array_keys_empty_range_rev)
            ])
        }
    }.


reduce_views_individual_test_() ->
    {
        "Reduce views",
        {
            setup,
            fun setup_individual/0,
            fun teardown_individual/1,
            [
                ?TDEFI(should_collate_group_keys)
            ]
        }
    }.


setup_shared_db() ->
    Ctx = test_util:start_couch([
            fabric,
            couch_jobs,
            couch_js,
            couch_views
        ]),
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    fabric2_db:update_docs(Db, [create_ddoc()]),
    make_docs(Db, ?NUM_DOCS),
    run_query(Db, <<"baz">>, #{limit => 0}),
    {Db, Ctx}.


teardown_shared_db({Db, Ctx}) ->
    fabric2_db:delete(fabric2_db:name(Db), [{user_ctx, ?ADMIN_USER}]),
    test_util:stop_couch(Ctx).


setup_individual() ->
    test_util:start_couch([
            fabric,
            couch_jobs,
            couch_js,
            couch_views
        ]).


teardown_individual(Ctx) ->
    test_util:stop_couch(Ctx).


should_reduce({Db, _}) ->
    Result = run_query(Db, <<"baz_count">>, #{}),
    Expect = {ok, [row(null, ?NUM_DOCS)]},
    ?assertEqual(Expect, Result).


should_reduce_rev({Db, _}) ->
    Args = #{
        direction => rev
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [row(null, ?NUM_DOCS)]},
    ?assertEqual(Expect, Result).


should_reduce_start_key({Db, _}) ->
    Args = #{
        start_key => 4
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [row(null, ?NUM_DOCS - 3)]},
    ?assertEqual(Expect, Result).


should_reduce_start_key_rev({Db, _}) ->
    Args = #{
        direction => rev,
        start_key => 4
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [row(null, 4)]},
    ?assertEqual(Expect, Result).


should_reduce_end_key({Db, _}) ->
    Args = #{
        end_key => 6
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [row(null, 6)]},
    ?assertEqual(Expect, Result).


should_reduce_end_key_rev({Db, _}) ->
    Args = #{
        direction => rev,
        end_key => 6
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [row(null, ?NUM_DOCS - 5)]},
    ?assertEqual(Expect, Result).


should_reduce_inclusive_end_false({Db, _}) ->
    Args = #{
        end_key => 6,
        inclusive_end => false
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [row(null, 5)]},
    ?assertEqual(Expect, Result).


should_reduce_inclusive_end_false_rev({Db, _}) ->
    Args = #{
        direction => rev,
        end_key => 6,
        inclusive_end => false
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [row(null, ?NUM_DOCS - 6)]},
    ?assertEqual(Expect, Result).


should_reduce_start_and_end_key({Db, _}) ->
    Args = #{
        start_key => 3,
        end_key => 5
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [row(null, 3)]},
    ?assertEqual(Expect, Result).


should_reduce_start_and_end_key_rev({Db, _}) ->
    Args = #{
        direction => rev,
        start_key => 5,
        end_key => 3
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [row(null, 3)]},
    ?assertEqual(Expect, Result).


should_reduce_empty_range({Db, _}) ->
    Args = #{
        start_key => 100000,
        end_key => 100001
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [row(null, 0)]},
    ?assertEqual(Expect, Result).


should_reduce_empty_range_rev({Db, _}) ->
    Args = #{
        direction => rev,
        start_key => 100001,
        end_key => 100000
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [row(null, 0)]},
    ?assertEqual(Expect, Result).


should_reduce_grouped({Db, _}) ->
    Args = #{
        group_level => exact
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [
        row(I, 1) || I <- lists:seq(1, ?NUM_DOCS)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_grouped_rev({Db, _}) ->
    Args = #{
        direction => rev,
        group_level => exact
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [
        row(I, 1) || I <- lists:seq(?NUM_DOCS, 1, -1)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_grouped_start_key({Db, _}) ->
    Args = #{
        group_level => exact,
        start_key => 3
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [
        row(I, 1) || I <- lists:seq(3, ?NUM_DOCS)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_grouped_start_key_rev({Db, _}) ->
    Args = #{
        direction => rev,
        group_level => exact,
        start_key => 3
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [
        row(3, 1),
        row(2, 1),
        row(1, 1)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_grouped_end_key({Db, _}) ->
    Args = #{
        group_level => exact,
        end_key => 6
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [
        row(I, 1) || I <- lists:seq(1, 6)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_grouped_end_key_rev({Db, _}) ->
    Args = #{
        direction => rev,
        group_level => exact,
        end_key => 6
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [
        row(I, 1) || I <- lists:seq(?NUM_DOCS, 6, -1)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_grouped_inclusive_end_false({Db, _}) ->
    Args = #{
        group_level => exact,
        end_key => 4,
        inclusive_end => false
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [
        row(I, 1) || I <- lists:seq(1, 3)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_grouped_inclusive_end_false_rev({Db, _}) ->
    Args = #{
        direction => rev,
        group_level => exact,
        end_key => 4,
        inclusive_end => false
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [
        row(I, 1) || I <- lists:seq(?NUM_DOCS, 5, -1)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_grouped_start_and_end_key({Db, _}) ->
    Args = #{
        group_level => exact,
        start_key => 2,
        end_key => 4
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [
        row(I, 1) || I <- lists:seq(2, 4)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_grouped_start_and_end_key_rev({Db, _}) ->
    Args = #{
        direction => rev,
        group_level => exact,
        start_key => 4,
        end_key => 2
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, [
        row(I, 1) || I <- lists:seq(4, 2, -1)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_grouped_empty_range({Db, _}) ->
    Args = #{
        group_level => exact,
        start_key => 100000,
        end_key => 100001
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, []},
    ?assertEqual(Expect, Result).


should_reduce_grouped_empty_range_rev({Db, _}) ->
    Args = #{
        direction => rev,
        group_level => exact,
        start_key => 100001,
        end_key => 100000
    },
    Result = run_query(Db, <<"baz_count">>, Args),
    Expect = {ok, []},
    ?assertEqual(Expect, Result).


should_reduce_array_keys({Db, _}) ->
    Result = run_query(Db, <<"boom">>, #{}),
    Expect = {ok, [row(null, 1.5 * ?NUM_DOCS)]},
    ?assertEqual(Expect, Result).


should_reduce_grouped_array_keys({Db, _}) ->
    Args = #{
        group_level => exact
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, lists:sort([
        row([I rem 3, I], 1.5) || I <- lists:seq(1, ?NUM_DOCS)
    ])},
    ?assertEqual(Expect, Result).


should_reduce_group_1_array_keys({Db, _}) ->
    Args = #{
        group_level => 1
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, [
        row([0], rem_count(0, ?NUM_DOCS) * 1.5),
        row([1], rem_count(1, ?NUM_DOCS) * 1.5),
        row([2], rem_count(2, ?NUM_DOCS) * 1.5)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_group_1_array_keys_start_key({Db, _}) ->
    Args = #{
        group_level => 1,
        start_key => [1]
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, [
        row([1], rem_count(1, ?NUM_DOCS) * 1.5),
        row([2], rem_count(2, ?NUM_DOCS) * 1.5)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_group_1_array_keys_start_key_rev({Db, _}) ->
    Args = #{
        direction => rev,
        group_level => 1,
        start_key => [1, ?NUM_DOCS + 1]
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, [
        row([1], rem_count(1, ?NUM_DOCS) * 1.5),
        row([0], rem_count(0, ?NUM_DOCS) * 1.5)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_group_1_array_keys_end_key({Db, _}) ->
    Args = #{
        group_level => 1,
        end_key => [1, ?NUM_DOCS + 1]
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, [
        row([0], rem_count(0, ?NUM_DOCS) * 1.5),
        row([1], rem_count(1, ?NUM_DOCS) * 1.5)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_group_1_array_keys_end_key_rev({Db, _}) ->
    Args = #{
        direction => rev,
        group_level => 1,
        end_key => [1]
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, [
        row([2], rem_count(2, ?NUM_DOCS) * 1.5),
        row([1], rem_count(1, ?NUM_DOCS) * 1.5)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_group_1_array_keys_inclusive_end_false({Db, _}) ->
    Args = #{
        group_level => 1,
        end_key => [1],
        inclusive_end => false
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, [
        row([0], rem_count(0, ?NUM_DOCS) * 1.5)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_group_1_array_keys_inclusive_end_false_rev({Db, _}) ->
    Args = #{
        direction => rev,
        group_level => 1,
        end_key => [1, ?NUM_DOCS + 1],
        inclusive_end => false
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, [
        row([2], rem_count(2, ?NUM_DOCS) * 1.5)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_group_1_array_keys_start_and_end_key({Db, _}) ->
    Args = #{
        group_level => 1,
        start_key => [1],
        end_key => [1, ?NUM_DOCS + 1]
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, [
        row([1], rem_count(1, ?NUM_DOCS) * 1.5)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_group_1_array_keys_start_and_end_key_rev({Db, _}) ->
    Args = #{
        direction => rev,
        group_level => 1,
        start_key => [1, ?NUM_DOCS + 1],
        end_key => [1]
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, [
        row([1], rem_count(1, ?NUM_DOCS) * 1.5)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_group_1_array_keys_sub_array_select({Db, _}) ->
    % Test that keys are applied below the key grouping
    Args = #{
        group_level => 1,
        start_key => [0, ?NUM_DOCS - 6],
        end_key => [1, 4]
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, [
        row([0], 3.0),
        row([1], 3.0)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_group_1_array_keys_sub_array_select_rev({Db, _}) ->
    % Test that keys are applied below the key grouping
    Args = #{
        direction => rev,
        group_level => 1,
        start_key => [1, 4],
        end_key => [0, ?NUM_DOCS - 6]
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, [
        row([1], 3.0),
        row([0], 3.0)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_group_1_array_keys_sub_array_inclusive_end({Db, _}) ->
    % Test that keys are applied below the key grouping
    Args = #{
        group_level => 1,
        start_key => [0, ?NUM_DOCS - 6],
        end_key => [1, 4],
        inclusive_end => false
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, [
        row([0], 3.0),
        row([1], 1.5)
    ]},
    ?assertEqual(Expect, Result).


should_reduce_group_1_array_keys_empty_range({Db, _}) ->
    Args = #{
        group_level => 1,
        start_key => [100],
        end_key => [101]
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, []},
    ?assertEqual(Expect, Result).


should_reduce_group_1_array_keys_empty_range_rev({Db, _}) ->
    Args = #{
        direction => rev,
        group_level => 1,
        start_key => [101],
        end_key => [100]
    },
    Result = run_query(Db, <<"boom">>, Args),
    Expect = {ok, []},
    ?assertEqual(Expect, Result).


should_collate_group_keys() ->
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    DDoc = couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/bar">>},
        {<<"views">>, {[
            {<<"group">>, {[
                {<<"map">>, <<"function(doc) {emit([doc.val], 1);}">>},
                {<<"reduce">>, <<"_count">>}
            ]}}
        ]}}
    ]}),
    % val is "föö" without combining characters
    Doc1 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"a">>},
        {<<"val">>, <<16#66, 16#C3, 16#B6, 16#C3, 16#B6>>}
    ]}),
    % val is "föö" without combining characters
    Doc2 = couch_doc:from_json_obj({[
        {<<"_id">>, <<"b">>},
        {<<"val">>, <<16#66, 16#6F, 16#CC, 16#88, 16#6F, 16#CC, 16#88>>}
    ]}),
    {ok, _} = fabric2_db:update_docs(Db, [DDoc, Doc1, Doc2]),

    % An implementation detail we have is that depending on
    % the direction of the view read we'll get the first
    % or last key to represent a group. In this particular
    % implementation the document ID breaks the sort tie
    % in the map view data.

    ArgsFwd = #{
        group_level => exact
    },
    ResultFwd = run_query(Db, DDoc, <<"group">>, ArgsFwd),
    ExpectFwd = {ok, [
        row([<<16#66, 16#C3, 16#B6, 16#C3, 16#B6>>], 2)
    ]},
    ?assertEqual(ExpectFwd, ResultFwd),

    ArgsRev = #{
        direction => rev,
        group_level => exact
    },
    ResultRev = run_query(Db, DDoc, <<"group">>, ArgsRev),
    ExpectRev = {ok, [
        row([<<16#66, 16#6F, 16#CC, 16#88, 16#6F, 16#CC, 16#88>>], 2)
    ]},
    ?assertEqual(ExpectRev, ResultRev).


rem_count(Rem, Count) ->
    Members = [I || I <- lists:seq(1, Count), I rem 3 == Rem],
    length(Members).


run_query(Db, Idx, Args) ->
    DDoc = create_ddoc(),
    run_query(Db, DDoc, Idx, Args).


run_query(Db, DDoc, Idx, Args) ->
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


row(Key, Value) ->
    {row, [{key, Key}, {value, Value}]}.


create_ddoc() ->
    couch_doc:from_json_obj({[
        {<<"_id">>, <<"_design/bar">>},
        {<<"views">>, {[
            {<<"baz">>, {[
                {<<"map">>, <<"function(doc) {emit(doc.val, doc.val);}">>}
            ]}},
            {<<"baz_count">>, {[
                {<<"map">>, <<"function(doc) {emit(doc.val, doc.val);}">>},
                {<<"reduce">>, <<"_count">>}
            ]}},
            {<<"baz_size">>, {[
                {<<"map">>, <<"function(doc) {emit(doc.val, doc.val);}">>},
                {<<"reduce">>, <<"_sum">>}
            ]}},
            {<<"boom">>, {[
                {<<"map">>, <<
                    "function(doc) {\n"
                    "   emit([doc.val % 3, doc.val], 1.5);\n"
                    "}"
                >>},
                {<<"reduce">>, <<"_sum">>}
            ]}},
            {<<"bing">>, {[
                {<<"map">>, <<"function(doc) {}">>},
                {<<"reduce">>, <<"_count">>}
            ]}},
            {<<"bing_hyper">>, {[
                {<<"map">>, <<"function(doc) {}">>},
                {<<"reduce">>, <<"_approx_count_distinct">>}
            ]}},
            {<<"doc_emit">>, {[
                {<<"map">>, <<"function(doc) {emit(doc.val, doc)}">>}
            ]}},
            {<<"duplicate_keys">>, {[
                {<<"map">>, <<
                    "function(doc) {\n"
                    "   emit(doc._id, doc.val);\n"
                    "   emit(doc._id, doc.val + 1);\n"
                    "}">>},
                {<<"reduce">>, <<"_count">>}
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


make_docs(Db, TotalDocs) when TotalDocs > 0 ->
    make_docs(Db, TotalDocs, 0).


make_docs(Db, TotalDocs, DocsMade) when TotalDocs > DocsMade ->
    DocCount = min(TotalDocs - DocsMade, 500),
    Docs = [doc(I + DocsMade) || I <- lists:seq(1, DocCount)],
    fabric2_db:update_docs(Db, Docs),
    make_docs(Db, TotalDocs, DocsMade + DocCount);

make_docs(_Db, TotalDocs, DocsMade) when TotalDocs =< DocsMade ->
    ok.


doc(Id) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, list_to_binary(integer_to_list(Id))},
        {<<"val">>, Id}
    ]}).
