#!/usr/bin/env escript
%% -*- erlang -*-

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
    test_util:init_code_path(),

    etap:plan(6),
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
    couch_server_sup:start_link(test_util:config_files()),

    {ok, Db} = couch_mrview_test_util:init_db(<<"foo">>, map),

    test_basic(Db),
    test_range(Db),
    test_rev_range(Db),
    test_limit_and_skip(Db),
    test_include_docs(Db),
    test_empty_view(Db),

    ok.


test_basic(Db) ->
    Result = run_query(Db, []),
    Expect = {ok, [
        {meta, [{total, 10}, {offset, 0}]},
        {row, [{id, <<"1">>}, {key, 1}, {val, 1}]},
        {row, [{id, <<"2">>}, {key, 2}, {val, 2}]},
        {row, [{id, <<"3">>}, {key, 3}, {val, 3}]},
        {row, [{id, <<"4">>}, {key, 4}, {val, 4}]},
        {row, [{id, <<"5">>}, {key, 5}, {val, 5}]},
        {row, [{id, <<"6">>}, {key, 6}, {val, 6}]},
        {row, [{id, <<"7">>}, {key, 7}, {val, 7}]},
        {row, [{id, <<"8">>}, {key, 8}, {val, 8}]},
        {row, [{id, <<"9">>}, {key, 9}, {val, 9}]},
        {row, [{id, <<"10">>}, {key, 10}, {val, 10}]}
    ]},
    etap:is(Result, Expect, "Simple view query worked.").


test_range(Db) ->
    Result = run_query(Db, [{start_key, 3}, {end_key, 5}]),
    Expect = {ok, [
        {meta, [{total, 10}, {offset, 2}]},
        {row, [{id, <<"3">>}, {key, 3}, {val, 3}]},
        {row, [{id, <<"4">>}, {key, 4}, {val, 4}]},
        {row, [{id, <<"5">>}, {key, 5}, {val, 5}]}
    ]},
    etap:is(Result, Expect, "Query with range works.").


test_rev_range(Db) ->
    Result = run_query(Db, [
        {direction, rev},
        {start_key, 5}, {end_key, 3},
        {inclusive_end, true}
    ]),
    Expect = {ok, [
        {meta, [{total, 10}, {offset, 5}]},
        {row, [{id, <<"5">>}, {key, 5}, {val, 5}]},
        {row, [{id, <<"4">>}, {key, 4}, {val, 4}]},
        {row, [{id, <<"3">>}, {key, 3}, {val, 3}]}
    ]},
    etap:is(Result, Expect, "Query with reversed range works.").


test_limit_and_skip(Db) ->
    Result = run_query(Db, [
        {start_key, 2},
        {limit, 3},
        {skip, 3}
    ]),
    Expect = {ok, [
        {meta, [{total, 10}, {offset, 4}]},
        {row, [{id, <<"5">>}, {key, 5}, {val, 5}]},
        {row, [{id, <<"6">>}, {key, 6}, {val, 6}]},
        {row, [{id, <<"7">>}, {key, 7}, {val, 7}]}
    ]},
    etap:is(Result, Expect, "Query with limit and skip works.").


test_include_docs(Db) ->
    Result = run_query(Db, [
        {start_key, 8},
        {end_key, 8},
        {include_docs, true}
    ]),
    Doc = {[
        {<<"_id">>,<<"8">>},
        {<<"_rev">>, <<"1-55b9a29311341e07ec0a7ca13bc1b59f">>},
        {<<"val">>,8}
    ]},
    Expect = {ok, [
        {meta, [{total, 10}, {offset, 7}]},
        {row, [{id, <<"8">>}, {key, 8}, {val, 8}, {doc, Doc}]}
    ]},
    etap:is(Result, Expect, "Query with include docs works.").


test_empty_view(Db) ->
    Result = couch_mrview:query_view(Db, <<"_design/bar">>, <<"bing">>),
    Expect = {ok, [
        {meta, [{total, 0}, {offset, 0}]}
    ]},
    etap:is(Result, Expect, "Empty views are correct.").


run_query(Db, Opts) ->
    couch_mrview:query_view(Db, <<"_design/bar">>, <<"baz">>, Opts).
