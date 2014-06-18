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

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(ADMIN_USER, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).
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
]).


start() ->
    {ok, Pid} = couch_server_sup:start_link(?CONFIG_CHAIN),
    Pid.

stop(Pid) ->
    erlang:monitor(process, Pid),
    couch_server_sup:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, server_stop})
    end.

setup() ->
    {ok, Db1} = couch_mrview_test_util:new_db(?tempdb(), map),
    {ok, Db2} = couch_mrview_test_util:save_docs(Db1, make_docs()),
    Db2.

teardown(Db) ->
    couch_db:close(Db),
    couch_server:delete(Db#db.name, [?ADMIN_USER]),
    ok.


collation_test_() ->
    {
        "Collation tests",
        {
            setup,
            fun start/0, fun stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_collate_fwd/1,
                    fun should_collate_rev/1,
                    fun should_collate_range/1,
                    fun should_collate_with_inclusive_end_fwd/1,
                    fun should_collate_with_inclusive_end_rev/1,
                    fun should_collate_without_inclusive_end_fwd/1,
                    fun should_collate_without_inclusive_end_rev/1,
                    fun should_collate_with_endkey_docid/1
                ]
            }
        }
    }.


should_collate_fwd(Db) ->
    {ok, Results} = run_query(Db, []),
    Expect = [{meta, [{total, 26}, {offset, 0}]}] ++ rows(),
    %% cannot use _assertEqual since mrview converts
    %% value 3.0 to 3 making assertion fail
    ?_assert(Expect == Results).

should_collate_rev(Db) ->
    {ok, Results} = run_query(Db, [{direction, rev}]),
    Expect = [{meta, [{total, 26}, {offset, 0}]}] ++ lists:reverse(rows()),
    %% cannot use _assertEqual since mrview converts
    %% value 3.0 to 3 making assertion fail
    ?_assert(Expect == Results).

should_collate_range(Db) ->
    ?_assertNot(
        begin
            {_, Error} = lists:foldl(fun(V, {Count, Error}) ->
                {ok, Results} = run_query(Db, [{start_key, V}, {end_key, V}]),
                Id = list_to_binary(integer_to_list(Count)),
                Expect = [
                    {meta, [{total, 26}, {offset, Count}]},
                    {row, [{id, Id}, {key, V}, {value, 0}]}
                ],
                case Results == Expect of
                    true -> {Count+1, Error};
                    _ -> {Count+1, true}
                end
            end, {0, false}, ?VALUES),
            Error
        end).

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
