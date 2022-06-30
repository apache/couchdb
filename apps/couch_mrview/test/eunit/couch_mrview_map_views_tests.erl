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

-module(couch_mrview_map_views_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, 1000).

setup() ->
    {ok, Db} = couch_mrview_test_util:init_db(?tempdb(), map),
    Db.

teardown(Db) ->
    couch_db:close(Db),
    couch_server:delete(couch_db:name(Db), [?ADMIN_CTX]),
    ok.

map_views_test_() ->
    {
        "Map views",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun should_map/1,
                    fun should_map_with_range/1,
                    fun should_map_with_limit_and_skip/1,
                    fun should_map_with_include_docs/1,
                    fun should_map_empty_views/1,
                    fun should_give_ext_size_seq_indexed_test/1
                ]
            }
        }
    }.

should_map(Db) ->
    Result = run_query(Db, []),
    Expect =
        {ok, [
            {meta, [{total, 10}, {offset, 0}]},
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
    ?_assertEqual(Expect, Result).

should_map_with_range(Db) ->
    Result = run_query(Db, [
        {direction, rev},
        {start_key, 5},
        {end_key, 3},
        {inclusive_end, true}
    ]),
    Expect =
        {ok, [
            {meta, [{total, 10}, {offset, 5}]},
            {row, [{id, <<"5">>}, {key, 5}, {value, 5}]},
            {row, [{id, <<"4">>}, {key, 4}, {value, 4}]},
            {row, [{id, <<"3">>}, {key, 3}, {value, 3}]}
        ]},
    ?_assertEqual(Expect, Result).

should_map_with_limit_and_skip(Db) ->
    Result = run_query(Db, [
        {start_key, 2},
        {limit, 3},
        {skip, 3}
    ]),
    Expect =
        {ok, [
            {meta, [{total, 10}, {offset, 4}]},
            {row, [{id, <<"5">>}, {key, 5}, {value, 5}]},
            {row, [{id, <<"6">>}, {key, 6}, {value, 6}]},
            {row, [{id, <<"7">>}, {key, 7}, {value, 7}]}
        ]},
    ?_assertEqual(Expect, Result).

should_map_with_include_docs(Db) ->
    Result = run_query(Db, [
        {start_key, 8},
        {end_key, 8},
        {include_docs, true}
    ]),
    Doc =
        {[
            {<<"_id">>, <<"8">>},
            {<<"_rev">>, <<"1-55b9a29311341e07ec0a7ca13bc1b59f">>},
            {<<"val">>, 8}
        ]},
    Expect =
        {ok, [
            {meta, [{total, 10}, {offset, 7}]},
            {row, [{id, <<"8">>}, {key, 8}, {value, 8}, {doc, Doc}]}
        ]},
    ?_assertEqual(Expect, Result).

should_map_empty_views(Db) ->
    Result = couch_mrview:query_view(Db, <<"_design/bar">>, <<"bing">>),
    Expect =
        {ok, [
            {meta, [{total, 0}, {offset, 0}]}
        ]},
    ?_assertEqual(Expect, Result).

should_give_ext_size_seq_indexed_test(Db) ->
    DDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"_design/seqdoc">>},
            {<<"options">>, {[{<<"seq_indexed">>, true}]}},
            {<<"views">>,
                {[
                    {<<"view1">>,
                        {[
                            {<<"map">>, <<"function(doc){emit(doc._id, doc._id);}">>}
                        ]}}
                ]}}
        ]}
    ),
    {ok, _} = couch_db:update_doc(Db, DDoc, []),
    {ok, Db1} = couch_db:open_int(couch_db:name(Db), []),
    {ok, DDoc1} = couch_db:open_doc(Db1, <<"_design/seqdoc">>, [ejson_body]),
    couch_mrview:query_view(Db1, DDoc1, <<"view1">>, [{update, true}]),
    {ok, Info} = couch_mrview:get_info(Db1, DDoc),
    Size = couch_util:get_nested_json_value({Info}, [sizes, external]),
    ok = couch_db:close(Db1),
    ?_assert(is_number(Size)).

run_query(Db, Opts) ->
    couch_mrview:query_view(Db, <<"_design/bar">>, <<"baz">>, Opts).
