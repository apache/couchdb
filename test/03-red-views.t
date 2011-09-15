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
    test_util:run(4, fun() -> test() end).

test() ->
    couch_server_sup:start_link(test_util:config_files()),

    {ok, Db} = couch_mrview_test_util:init_db(<<"foo">>, red),

    test_basic(Db),
    test_key_range(Db),
    test_group_level(Db),
    test_group_exact(Db),

    ok.


test_basic(Db) ->
    Result = run_query(Db, []),
    Expect = {ok, [
        {meta, []},
        {row, [{key, null}, {val, 55}]}
    ]},
    etap:is(Result, Expect, "Simple reduce view works.").


test_key_range(Db) ->
    Result = run_query(Db, [{start_key, [0, 2]}, {end_key, [0, 4]}]),
    Expect = {ok, [
        {meta, []},
        {row, [{key, null}, {val, 6}]}
    ]},
    etap:is(Result, Expect, "Reduce with key range works.").


test_group_level(Db) ->
    Result = run_query(Db, [{group_level, 1}]),
    Expect = {ok, [
        {meta, []},
        {row, [{key, [0]}, {val, 30}]},
        {row, [{key, [1]}, {val, 25}]}
    ]},
    etap:is(Result, Expect, "Group level works.").

test_group_exact(Db) ->
    Result = run_query(Db, [{group_level, exact}]),
    Expect = {ok, [
        {meta, []},
        {row, [{key, [0, 2]}, {val, 2}]},
        {row, [{key, [0, 4]}, {val, 4}]},
        {row, [{key, [0, 6]}, {val, 6}]},
        {row, [{key, [0, 8]}, {val, 8}]},
        {row, [{key, [0, 10]}, {val, 10}]},
        {row, [{key, [1, 1]}, {val, 1}]},
        {row, [{key, [1, 3]}, {val, 3}]},
        {row, [{key, [1, 5]}, {val, 5}]},
        {row, [{key, [1, 7]}, {val, 7}]},
        {row, [{key, [1, 9]}, {val, 9}]}
    ]},
    etap:is(Result, Expect, "Group exact works.").


run_query(Db, Opts) ->
    couch_mrview:query_view(Db, <<"_design/bar">>, <<"baz">>, Opts).
