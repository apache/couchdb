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

-module(couch_mrview_changes_since_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, 1000).



setup() ->
    {ok, Db} = couch_mrview_test_util:init_db(?tempdb(), changes),
    Db.

teardown(Db) ->
    couch_db:close(Db),
    couch_server:delete(couch_db:name(Db), [?ADMIN_CTX]),
    ok.


changes_since_test() ->
    {
        "changes_since tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun test_basic/1,
                    fun test_range/1,
                    fun test_basic_since/1,
                    fun test_range_since/1,
                    fun test_basic_count/1,
                    fun test_range_count/1,
                    fun test_basic_count_since/1,
                    fun test_range_count_since/1,
                    fun test_compact/1,
                    fun test_remove_key/1
                ]
            }
        }
    }.

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
    ?_assertEqual(Result, Expect).


test_range(Db) ->
    Result = run_query(Db, 0, [{start_key, 3}, {end_key, 5}]),
    Expect = {ok, [
                {{5, 3, <<"3">>}, 3},
                {{6, 4, <<"4">>}, 4},
                {{7, 5, <<"5">>}, 5}
    ]},
    ?_assertEqual(Result, Expect).

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
    ?_assertEqual(Result, Expect).

test_range_since(Db) ->
    Result = run_query(Db, 5, [{start_key, 3}, {end_key, 5}]),
    Expect = {ok, [
                {{6, 4, <<"4">>}, 4},
                {{7, 5, <<"5">>}, 5}
    ]},
    ?_assertEqual(Result, Expect).

test_basic_count(Db) ->
    Result = run_count_query(Db, 0, []),
    ?_assertEqual(Result, 10).

test_range_count(Db) ->
    Result = run_count_query(Db, 0, [{start_key, 3}, {end_key, 5}]),
    ?_assertEqual(Result, 3).

test_basic_count_since(Db) ->
    Result = run_count_query(Db, 5, []),
    ?_assertEqual(Result, 6).

test_range_count_since(Db) ->
    Result = run_count_query(Db, 5, [{start_key, 3}, {end_key, 5}]),
    ?_assertEqual(Result, 2).

test_compact(Db) ->
    Result = couch_mrview:compact(Db, <<"_design/bar">>),
    ?_assertEqual(Result, ok),
    Count = run_count_query(Db, 0, []),
    ?_assertEqual(Count, 10).

test_remove_key(Db) ->
    %% add new doc
    Doc = couch_mrview_test_util:doc(11),
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    RevStr = couch_doc:rev_to_str(Rev),
    {ok, _} =  couch_db:ensure_full_commit(Db),
    {ok, Db1} = couch_db:reopen(Db),
    Result = run_count_query(Db1, 0, []),
    ?_assertEqual(Result, 11),
    %% check new view key
    Result1 = run_query(Db1, 0, [{start_key, 11}, {end_key, 11}]),
    Expect = {ok, [
                {{12, 11, <<"11">>}, 11}
    ]},
    ?_assertEqual(Result1, Expect),

    %% delete doc
    Doc2 = couch_doc:from_json_obj({[
                {<<"_id">>, <<"11">>},
                {<<"_rev">>, RevStr},
                {<<"_deleted">>, true}
    ]}),
    {ok, _} = couch_db:update_doc(Db1, Doc2, []),
    {ok, Db2} = couch_db:reopen(Db1),
    Result2 = run_count_query(Db2, 0, []),
    ?_assertEqual(Result2, 11),
    %% check new view key
    Result3 = run_query(Db2, 0, [{start_key, 11}, {end_key, 11}]),
    Expect2 = {ok, [
                {{13, 11, <<"11">>}, {[{<<"_removed">>, true}]}}
    ]},
    ?_assertEqual(Result3, Expect2).

run_query(Db, Since, Opts) ->
    Fun = fun(KV, Acc) -> {ok, [KV | Acc]} end,
    {ok, R} = couch_mrview:view_changes_since(Db, <<"_design/bar">>, <<"baz">>,
                                              Since, Fun, Opts, []),
    {ok, lists:reverse(R)}.

run_count_query(Db, Since, Opts) ->
    couch_mrview:count_view_changes_since(Db, <<"_design/bar">>, <<"baz">>,
                                          Since, Opts).
