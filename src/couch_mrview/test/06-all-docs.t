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
    test_util:run(6, fun() -> test() end).


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
        {meta, [{total, 11}, {offset, 0}]},
        mk_row(<<"1">>, <<"1-08d53a5760b95fce6df2e2c5b008be39">>),
        mk_row(<<"10">>, <<"1-a05b6ea2bc0243949f103d5b4f15f71e">>),
        mk_row(<<"2">>, <<"1-b57c77a9e6f7574ca6469f0d6dcd78bb">>),
        mk_row(<<"3">>, <<"1-7fbf84d56f8017880974402d60f5acd6">>),
        mk_row(<<"4">>, <<"1-fcaf5852c08ffb239ac8ce16c409f253">>),
        mk_row(<<"5">>, <<"1-aaac5d460fd40f9286e57b9bf12e23d2">>),
        mk_row(<<"6">>, <<"1-aca21c2e7bc5f8951424fcfc5d1209d8">>),
        mk_row(<<"7">>, <<"1-4374aeec17590d82f16e70f318116ad9">>),
        mk_row(<<"8">>, <<"1-55b9a29311341e07ec0a7ca13bc1b59f">>),
        mk_row(<<"9">>, <<"1-558c8487d9aee25399a91b5d31d90fe2">>),
        mk_row(<<"_design/bar">>, <<"1-a44e1dd1994a7717bf89c894ebd1f081">>)
    ]},
    etap:is(Result, Expect, "Simple view query worked.").


test_range(Db) ->
    Result = run_query(Db, [{start_key, <<"3">>}, {end_key, <<"5">>}]),
    Expect = {ok, [
        {meta, [{total, 11}, {offset, 3}]},
        mk_row(<<"3">>, <<"1-7fbf84d56f8017880974402d60f5acd6">>),
        mk_row(<<"4">>, <<"1-fcaf5852c08ffb239ac8ce16c409f253">>),
        mk_row(<<"5">>, <<"1-aaac5d460fd40f9286e57b9bf12e23d2">>)
    ]},
    etap:is(Result, Expect, "Query with range works.").


test_rev_range(Db) ->
    Result = run_query(Db, [
        {direction, rev},
        {start_key, <<"5">>}, {end_key, <<"3">>},
        {inclusive_end, true}
    ]),
    Expect = {ok, [
        {meta, [{total, 11}, {offset, 5}]},
        mk_row(<<"5">>, <<"1-aaac5d460fd40f9286e57b9bf12e23d2">>),
        mk_row(<<"4">>, <<"1-fcaf5852c08ffb239ac8ce16c409f253">>),
        mk_row(<<"3">>, <<"1-7fbf84d56f8017880974402d60f5acd6">>)
    ]},
    etap:is(Result, Expect, "Query with reversed range works.").


test_limit_and_skip(Db) ->
    Result = run_query(Db, [
        {start_key, <<"2">>},
        {limit, 3},
        {skip, 3}
    ]),
    Expect = {ok, [
        {meta, [{total, 11}, {offset, 5}]},
        mk_row(<<"5">>, <<"1-aaac5d460fd40f9286e57b9bf12e23d2">>),
        mk_row(<<"6">>, <<"1-aca21c2e7bc5f8951424fcfc5d1209d8">>),
        mk_row(<<"7">>, <<"1-4374aeec17590d82f16e70f318116ad9">>)
    ]},
    etap:is(Result, Expect, "Query with limit and skip works.").


test_include_docs(Db) ->
    Result = run_query(Db, [
        {start_key, <<"8">>},
        {end_key, <<"8">>},
        {include_docs, true}
    ]),
    Doc = {[
        {<<"_id">>,<<"8">>},
        {<<"_rev">>, <<"1-55b9a29311341e07ec0a7ca13bc1b59f">>},
        {<<"val">>, 8}
    ]},
    Val = {[{rev, <<"1-55b9a29311341e07ec0a7ca13bc1b59f">>}]},
    Expect = {ok, [
        {meta, [{total, 11}, {offset, 8}]},
        {row, [{id, <<"8">>}, {key, <<"8">>}, {value, Val}, {doc, Doc}]}
    ]},
    etap:is(Result, Expect, "Query with include docs works.").


test_empty_view(Db) ->
    Result = couch_mrview:query_view(Db, <<"_design/bar">>, <<"bing">>),
    Expect = {ok, [
        {meta, [{total, 0}, {offset, 0}]}
    ]},
    etap:is(Result, Expect, "Empty views are correct.").


mk_row(Id, Rev) ->
    {row, [{id, Id}, {key, Id}, {value, {[{rev, Rev}]}}]}.


run_query(Db, Opts) ->
    couch_mrview:query_all_docs(Db, Opts).
