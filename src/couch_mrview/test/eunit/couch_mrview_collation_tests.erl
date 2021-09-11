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

-module(couch_mrview_collation_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, 1000).
-define(VALUES, [
    null,
    false,
    true,

    1,
    2,
    3.0,
    4,

    <<"a">>,
    <<"A">>,
    <<"aa">>,
    <<"b">>,
    <<"B">>,
    <<"ba">>,
    <<"bb">>,

    % U+200B is a zero-width space, which will be ignored by ICU but will cause
    % the raw collator to treat these as three distinct keys
    <<"c">>,
    unicode:characters_to_binary([$c, 16#200B]),
    unicode:characters_to_binary([$c, 16#200B, 16#200B]),

    [<<"a">>],
    [<<"b">>],
    [<<"b">>, <<"c">>],
    [<<"b">>, <<"c">>, <<"a">>],
    [<<"b">>, <<"d">>],
    [<<"b">>, <<"d">>, <<"e">>],

    {[{<<"a">>, 1}]},
    {[{<<"a">>, 2}]},
    {[{<<"b">>, 1}]},
    {[{<<"b">>, 2}]},
    {[{<<"b">>, 2}, {<<"a">>, 1}]},
    {[{<<"b">>, 2}, {<<"c">>, 2}]},

    % Values with depth > 10 trigger the erlang collation fallback in couch_ejson_compare
    {[{<<"x">>, [[[[[[[[[[[<<"y">>]]]]]]]]]]]}]}

]).


setup() ->
    {ok, Db1} = couch_mrview_test_util:new_db(?tempdb(), map),
    Docs = [couch_mrview_test_util:ddoc(red) | make_docs()],
    {ok, Db2} = couch_mrview_test_util:save_docs(Db1, Docs),
    Db2.

teardown(Db) ->
    couch_db:close(Db),
    couch_server:delete(couch_db:name(Db), [?ADMIN_CTX]),
    ok.


collation_test_() ->
    {
        "Collation tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_collate_fwd/1,
                    fun should_collate_rev/1,
                    fun should_collate_range_/1,
                    fun should_collate_with_inclusive_end_fwd/1,
                    fun should_collate_with_inclusive_end_rev/1,
                    fun should_collate_without_inclusive_end_fwd/1,
                    fun should_collate_without_inclusive_end_rev/1,
                    fun should_collate_with_endkey_docid/1,
                    fun should_use_collator_for_reduce_grouping/1
                ]
            }
        }
    }.


should_collate_fwd(Db) ->
    {ok, Results} = run_query(Db, []),
    Expect = [{meta, [{total, length(?VALUES)}, {offset, 0}]}] ++ rows(),
    ?_assertEquiv(Expect, Results).

should_collate_rev(Db) ->
    {ok, Results} = run_query(Db, [{direction, rev}]),
    Expect = [{meta, [{total, length(?VALUES)}, {offset, 0}]}] ++ lists:reverse(rows()),
    ?_assertEquiv(Expect, Results).

should_collate_range_(Db) ->
    Index = lists:zip(lists:seq(0, length(?VALUES)-1), ?VALUES),
    lists:map(fun(V) ->
        {ok, Results} = run_query(Db, [{start_key, V}, {end_key, V}]),
        Expect = [
            {meta, [{total, length(?VALUES)}, find_offset(Index, V)]} |
            find_matching_rows(Index, V)
        ],
        ?_assertEquiv(Expect, Results)
    end, ?VALUES).

find_offset(Index, Value) ->
    [{Offset, _} | _] = lists:dropwhile(fun({_, V}) ->
        couch_ejson_compare:less(Value, V) =/= 0
    end, Index),
    {offset, Offset}.

find_matching_rows(Index, Value) ->
    Matches = lists:filter(fun({_, V}) ->
        couch_ejson_compare:less(Value, V) =:= 0
    end, Index),
    lists:map(fun({Id, V}) ->
        {row, [{id, list_to_binary(integer_to_list(Id))}, {key, V}, {value, 0}]}
    end, Matches).

should_collate_with_inclusive_end_fwd(Db) ->
    Opts = [{end_key, <<"b">>}, {inclusive_end, true}],
    {ok, Rows0} = run_query(Db, Opts),
    LastRow = lists:last(Rows0),
    Expect = {row, [{id,<<"10">>}, {key,<<"b">>}, {value,0}]},
    ?_assertEqual(Expect, LastRow).

should_collate_with_inclusive_end_rev(Db) ->
    Opts = [{end_key, <<"b">>}, {inclusive_end, true}, {direction, rev}],
    {ok, Rows} = run_query(Db, Opts),
    LastRow = lists:last(Rows),
    Expect = {row, [{id,<<"10">>}, {key,<<"b">>}, {value,0}]},
    ?_assertEqual(Expect, LastRow).

should_collate_without_inclusive_end_fwd(Db) ->
    Opts = [{end_key, <<"b">>}, {inclusive_end, false}],
    {ok, Rows0} = run_query(Db, Opts),
    LastRow = lists:last(Rows0),
    Expect = {row, [{id,<<"9">>}, {key,<<"aa">>}, {value,0}]},
    ?_assertEqual(Expect, LastRow).

should_collate_without_inclusive_end_rev(Db) ->
    Opts = [{end_key, <<"b">>}, {inclusive_end, false}, {direction, rev}],
    {ok, Rows} = run_query(Db, Opts),
    LastRow = lists:last(Rows),
    Expect = {row, [{id,<<"11">>}, {key,<<"B">>}, {value,0}]},
    ?_assertEqual(Expect, LastRow).

should_collate_with_endkey_docid(Db) ->
    ?_test(begin
        {ok, Rows0} = run_query(Db, [
            {end_key, <<"b">>}, {end_key_docid, <<"10">>},
            {inclusive_end, false}
        ]),
        Result0 = lists:last(Rows0),
        Expect0 = {row, [{id,<<"9">>}, {key,<<"aa">>}, {value,0}]},
        ?assertEqual(Expect0, Result0),

        {ok, Rows1} = run_query(Db, [
            {end_key, <<"b">>}, {end_key_docid, <<"11">>},
            {inclusive_end, false}
        ]),
        Result1 = lists:last(Rows1),
        Expect1 = {row, [{id,<<"10">>}, {key,<<"b">>}, {value,0}]},
        ?assertEqual(Expect1, Result1)
    end).

should_use_collator_for_reduce_grouping(Db) ->
    UniqueKeys = lists:usort(fun(A, B) ->
        not couch_ejson_compare:less_json(B, A)
    end, ?VALUES),
    {ok, [{meta,_} | Rows]} = reduce_query(Db, [{group_level, exact}]),
    ?_assertEqual(length(UniqueKeys), length(Rows)).

make_docs() ->
    {Docs, _} = lists:foldl(fun(V, {Docs0, Count}) ->
        Doc = couch_doc:from_json_obj({[
            {<<"_id">>, list_to_binary(integer_to_list(Count))},
            {<<"foo">>, V}
        ]}),
        {[Doc | Docs0], Count+1}
    end, {[], 0}, ?VALUES),
    Docs.

rows() ->
    {Rows, _} = lists:foldl(fun(V, {Rows0, Count}) ->
        Id = list_to_binary(integer_to_list(Count)),
        Row = {row, [{id, Id}, {key, V}, {value, 0}]},
        {[Row | Rows0], Count+1}
    end, {[], 0}, ?VALUES),
    lists:reverse(Rows).

run_query(Db, Opts) ->
    couch_mrview:query_view(Db, <<"_design/bar">>, <<"zing">>, Opts).

reduce_query(Db, Opts) ->
    couch_mrview:query_view(Db, <<"_design/red">>, <<"zing">>, Opts).
