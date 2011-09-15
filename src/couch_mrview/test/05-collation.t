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
    test_util:run(9, fun() -> test() end).


test() ->
    couch_server_sup:start_link(test_util:config_files()),
    {ok, Db0} = couch_mrview_test_util:new_db(<<"foo">>, map),
    {ok, Db1} = couch_mrview_test_util:save_docs(Db0, docs()),

    test_collated_fwd(Db1),
    test_collated_rev(Db1),
    test_range_collation(Db1),
    test_inclusive_end(Db1),
    test_uninclusive_end(Db1),
    test_with_endkey_docid(Db1),

    ok.

test_collated_fwd(Db) ->
    {ok, Results} = run_query(Db, []),
    Expect = [{meta, [{total, 26}, {offset, 0}]}] ++ rows(),
    etap:is(Results, Expect, "Values were collated correctly.").


test_collated_rev(Db) ->
    {ok, Results} = run_query(Db, [{direction, rev}]),
    Expect = [{meta, [{total, 26}, {offset, 0}]}] ++ lists:reverse(rows()),
    etap:is(Results, Expect, "Values were collated correctly descending.").


test_range_collation(Db) ->
    {_, Error} = lists:foldl(fun(V, {Count, Error}) ->
        {ok, Results} = run_query(Db, [{start_key, V}, {end_key, V}]),
        Id = list_to_binary(integer_to_list(Count)),
        Expect = [
            {meta, [{total, 26}, {offset, Count}]},
            {row, [{id, Id}, {key, V}, {val, 0}]}
        ],
        case Results == Expect of
            true -> {Count+1, Error};
            _ -> {Count+1, true}
        end
    end, {0, false}, vals()),
    etap:is(Error, false, "Found each individual key correctly.").


test_inclusive_end(Db) ->
    Opts = [{end_key, <<"b">>}, {inclusive_end, true}],
    {ok, Rows0} = run_query(Db, Opts),
    LastRow0 = lists:last(Rows0),
    Expect0 = {row, [{id,<<"10">>}, {key,<<"b">>}, {val,0}]},
    etap:is(LastRow0, Expect0, "Inclusive end is correct."),

    {ok, Rows1} = run_query(Db, Opts ++ [{direction, rev}]),
    LastRow1 = lists:last(Rows1),
    Expect1 = {row, [{id,<<"10">>}, {key,<<"b">>}, {val,0}]},
    etap:is(LastRow1, Expect1,
            "Inclusive end is correct with descending=true").

test_uninclusive_end(Db) ->
    Opts = [{end_key, <<"b">>}, {inclusive_end, false}],
    {ok, Rows0} = run_query(Db, Opts),
    LastRow0 = lists:last(Rows0),
    Expect0 = {row, [{id,<<"9">>}, {key,<<"aa">>}, {val,0}]},
    etap:is(LastRow0, Expect0, "Uninclusive end is correct."),

    {ok, Rows1} = run_query(Db, Opts ++ [{direction, rev}]),
    LastRow1 = lists:last(Rows1),
    Expect1 = {row, [{id,<<"11">>}, {key,<<"B">>}, {val,0}]},
    etap:is(LastRow1, Expect1,
            "Uninclusive end is correct with descending=true").


test_with_endkey_docid(Db) ->
    {ok, Rows0} = run_query(Db, [
        {end_key, <<"b">>}, {end_key_docid, <<"10">>},
        {inclusive_end, false}
    ]),
    Result0 = lists:last(Rows0),
    Expect0 = {row, [{id,<<"9">>}, {key,<<"aa">>}, {val,0}]},
    etap:is(Result0, Expect0, "Uninclsuive end with endkey_docid set is ok."),

    {ok, Rows1} = run_query(Db, [
        {end_key, <<"b">>}, {end_key_docid, <<"11">>},
        {inclusive_end, false}
    ]),
    Result1 = lists:last(Rows1),
    Expect1 = {row, [{id,<<"10">>}, {key,<<"b">>}, {val,0}]},
    etap:is(Result1, Expect1, "Uninclsuive end with endkey_docid set is ok.").


run_query(Db, Opts) ->
    couch_mrview:query_view(Db, <<"_design/bar">>, <<"zing">>, Opts).


docs() ->
    {Docs, _} = lists:foldl(fun(V, {Docs0, Count}) ->
        Doc = couch_doc:from_json_obj({[
            {<<"_id">>, list_to_binary(integer_to_list(Count))},
            {<<"foo">>, V}
        ]}),
        {[Doc | Docs0], Count+1}
    end, {[], 0}, vals()),
    Docs.


rows() ->
    {Rows, _} = lists:foldl(fun(V, {Rows0, Count}) ->
        Id = list_to_binary(integer_to_list(Count)),
        Row = {row, [{id, Id}, {key, V}, {val, 0}]},
        {[Row | Rows0], Count+1}
    end, {[], 0}, vals()),
    lists:reverse(Rows).


vals() ->
    [
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
        {[{<<"b">>, 2}, {<<"c">>, 2}]}
    ].
