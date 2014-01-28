#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./deps/*/ebin -pa ./apps/*/ebin -pa ./test/etap

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

main(_) ->
    etap:plan(14),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    timer:sleep(300),
    ok.

test() ->
    test_util:start_couch(),

    {ok, Db} = couch_mrview_test_util:init_db(<<"foo">>, changes),

    test_basic(Db),
    test_range(Db),
    test_basic_since(Db),
    test_range_since(Db),
    test_basic_count(Db),
    test_range_count(Db),
    test_basic_count_since(Db),
    test_range_count_since(Db),
    test_compact(Db),
    test_remove_key(Db),
    catch test_util:stop_couch(),
    ok.


test_basic(Db) ->
    Result = run_query(Db, 0, []),
    Expect = {ok, [
                {{2, 1, <<"1">>}, 1},
                {{3, 10, <<"10">>}, 10},
                {{4, 2, <<"2">>}, 2},
                {{5, 3, <<"3">>}, 3},
                {{6, 4, <<"4">>}, 4},
                {{7, 5, <<"5">>}, 5},
                {{8, 6, <<"6">>}, 6},
                {{9, 7, <<"7">>}, 7},
                {{10, 8, <<"8">>}, 8},
                {{11, 9, <<"9">>}, 9}
    ]},
    etap:is(Result, Expect, "Simple view query worked.").


test_range(Db) ->
    Result = run_query(Db, 0, [{start_key, 3}, {end_key, 5}]),
    Expect = {ok, [
                {{5, 3, <<"3">>}, 3},
                {{6, 4, <<"4">>}, 4},
                {{7, 5, <<"5">>}, 5}
    ]},
    etap:is(Result, Expect, "Query with range works.").

test_basic_since(Db) ->
    Result = run_query(Db, 5, []),
    Expect = {ok, [
                {{6, 4, <<"4">>}, 4},
                {{7, 5, <<"5">>}, 5},
                {{8, 6, <<"6">>}, 6},
                {{9, 7, <<"7">>}, 7},
                {{10, 8, <<"8">>}, 8},
                {{11, 9, <<"9">>}, 9}
    ]},
    etap:is(Result, Expect, "Simple view query since 5 worked.").

test_range_since(Db) ->
    Result = run_query(Db, 5, [{start_key, 3}, {end_key, 5}]),
    Expect = {ok, [
                {{6, 4, <<"4">>}, 4},
                {{7, 5, <<"5">>}, 5}
    ]},
    etap:is(Result, Expect, "Query with range since 5 works.").

test_basic_count(Db) ->
    Result = run_count_query(Db, 0, []),
    etap:is(Result, 10, "Simple view count worked.").

test_range_count(Db) ->
    Result = run_count_query(Db, 0, [{start_key, 3}, {end_key, 5}]),
    etap:is(Result, 3, "Count with range works.").

test_basic_count_since(Db) ->
    Result = run_count_query(Db, 5, []),
    etap:is(Result, 6, "Simple view count since 5 worked.").

test_range_count_since(Db) ->
    Result = run_count_query(Db, 5, [{start_key, 3}, {end_key, 5}]),
    etap:is(Result, 2, "Count with range since 5 works.").

test_compact(Db) ->
    Result = couch_mrview:compact(Db, <<"_design/bar">>),
    etap:is(Result, ok, "compact view is OK"),
    Count = run_count_query(Db, 0, []),
    etap:is(Count, 10, "compact view worked.").

test_remove_key(Db) ->
    %% add new doc
    Doc = couch_mrview_test_util:doc(11),
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    RevStr = couch_doc:rev_to_str(Rev),
    {ok, _} =  couch_db:ensure_full_commit(Db),
    {ok, Db1} = couch_db:reopen(Db),
    Result = run_count_query(Db1, 0, []),
    etap:is(Result, 11, "Add new doc worked."),
    %% check new view key
    Result1 = run_query(Db1, 0, [{start_key, 11}, {end_key, 11}]),
    Expect = {ok, [
                {{12, 11, <<"11">>}, 11}
    ]},
    etap:is(Result1, Expect, "added key OK."),

    %% delete doc
    Doc2 = couch_doc:from_json_obj({[
                {<<"_id">>, <<"11">>},
                {<<"_rev">>, RevStr},
                {<<"_deleted">>, true}
    ]}),
    {ok, _} = couch_db:update_doc(Db1, Doc2, []),
    {ok, Db2} = couch_db:reopen(Db1),
    Result2 = run_count_query(Db2, 0, []),
    etap:is(Result2, 11, "removed key saved."),
    %% check new view key
    Result3 = run_query(Db2, 0, [{start_key, 11}, {end_key, 11}]),
    Expect2 = {ok, [
                {{13, 11, <<"11">>}, {[{<<"_removed">>, true}]}}
    ]},
    etap:is(Result3, Expect2, "removed key OK.").

run_query(Db, Since, Opts) ->
    Fun = fun(KV, Acc) -> {ok, [KV | Acc]} end,
    {ok, R} = couch_mrview:view_changes_since(Db, <<"_design/bar">>, <<"baz">>,
                                              Since, Fun, Opts, []),
    {ok, lists:reverse(R)}.

run_count_query(Db, Since, Opts) ->
    couch_mrview:count_view_changes_since(Db, <<"_design/bar">>, <<"baz">>,
                                          Since, Opts).
