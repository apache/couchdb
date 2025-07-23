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

-module(couch_srt_query_tests).

-include_lib("couch/include/couch_eunit.hrl").

-include_lib("stdlib/include/ms_transform.hrl").
-include("../../src/couch_srt.hrl").

-define(MATCHERS_THRESHOLD, 1000).
csrt_query_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_group_by_multiple_keys),
            ?TDEF_FE(t_group_by_single_key),
            ?TDEF_FE(t_group_by_binary_key),
            ?TDEF_FE(t_group_by_detect_unsafe_query),
            ?TDEF_FE(t_group_by_run_unsafe_query),
            ?TDEF_FE(t_group_by_run_unsafe_correctness),
            ?TDEF_FE(t_group_by_bad_request),
            ?TDEF_FE(t_count_by_multiple_keys),
            ?TDEF_FE(t_count_by_single_key),
            ?TDEF_FE(t_count_by_binary_key),
            ?TDEF_FE(t_count_by_bad_request),
            ?TDEF_FE(t_sort_by_multiple_keys),
            ?TDEF_FE(t_sort_by_single_key),
            ?TDEF_FE(t_sort_by_binary_key),
            ?TDEF_FE(t_sort_by_bad_request)
        ]
    }.

setup() ->
    Rctxs = [
        rctx(#{dbname => <<"db1">>, ioq_calls => 123, username => <<"user_foo">>}),
        rctx(#{dbname => <<"db1">>, ioq_calls => 321, username => <<"user_foo">>}),
        rctx(#{dbname => <<"db2">>, ioq_calls => 345, username => <<"user_bar">>}),
        rctx(#{dbname => <<"db2">>, ioq_calls => 543, username => <<"user_bar">>}),
        rctx(#{dbname => <<"db1">>, ioq_calls => 678, username => <<"user_bar">>}),
        rctx(#{dbname => <<"db2">>, ioq_calls => 987, username => <<"user_foo">>})
    ],
    ets:new(?CSRT_ETS, [
        named_table,
        public,
        {keypos, #rctx.pid_ref}
    ]),
    ets:insert(?CSRT_ETS, Rctxs),
    add_matcher("docs_read", couch_srt_logger:matcher_on_docs_read(?MATCHERS_THRESHOLD)),
    #{rctxs => Rctxs}.

teardown(_) ->
    ets:delete(?CSRT_ETS).

rctx(Opts) ->
    % Update `docs_read` to make standard `{docs_read, fun matcher_on_docs_read/1, 1000}`
    % matcher match.
    BaseOpts = #{docs_read => ?MATCHERS_THRESHOLD + 1, username => <<"user_foo">>},
    couch_srt_test_helper:rctx_gen(maps:merge(BaseOpts, Opts)).

dummy_key_fun(#rctx{username = Username}) ->
    Username.

dummy_value_fun(#rctx{ioq_calls = IoqCalls}) ->
    IoqCalls.

t_group_by_multiple_keys(#{rctxs := Rctxs}) ->
    Aggregated = aggregate([username, dbname], ioq_calls, Rctxs),
    Grouped = group(Aggregated),
    V1 = maps:get({<<"user_bar">>, <<"db1">>}, Grouped),
    V2 = maps:get({<<"user_bar">>, <<"db2">>}, Grouped),
    V3 = maps:get({<<"user_foo">>, <<"db1">>}, Grouped),
    V4 = maps:get({<<"user_foo">>, <<"db2">>}, Grouped),
    Q = couch_srt:query([
        couch_srt:from("docs_read"),
        couch_srt:group_by([<<"username">>, <<"dbname">>], <<"ioq_calls">>)
    ]),
    ?assertMatch(
        {ok, #{
            {<<"user_bar">>, <<"db1">>} := V1,
            {<<"user_bar">>, <<"db2">>} := V2,
            {<<"user_foo">>, <<"db1">>} := V3,
            {<<"user_foo">>, <<"db2">>} := V4
        }},
        couch_srt:run(Q)
    ),
    ok.

t_group_by_single_key(#{rctxs := Rctxs}) ->
    Aggregated = aggregate([username], ioq_calls, Rctxs),
    Grouped = group(Aggregated),
    V1 = maps:get({<<"user_bar">>}, Grouped),
    V2 = maps:get({<<"user_foo">>}, Grouped),
    Q = couch_srt:query([
        couch_srt:from("docs_read"),
        couch_srt:group_by([<<"username">>], <<"ioq_calls">>)
    ]),
    ?assertMatch(
        {ok, #{
            {<<"user_bar">>} := V1,
            {<<"user_foo">>} := V2
        }},
        couch_srt:run(Q)
    ),
    ok.

t_group_by_binary_key(#{rctxs := Rctxs}) ->
    Aggregated = aggregate([username], ioq_calls, Rctxs),
    Grouped = group(Aggregated),
    V1 = maps:get({<<"user_bar">>}, Grouped),
    V2 = maps:get({<<"user_foo">>}, Grouped),
    Q = couch_srt:query([
        couch_srt:from("docs_read"),
        couch_srt:group_by(<<"username">>, <<"ioq_calls">>)
    ]),
    ?assertMatch(
        {ok, #{
            <<"user_bar">> := V1,
            <<"user_foo">> := V2
        }},
        couch_srt:run(Q)
    ),
    ok.

t_group_by_detect_unsafe_query(_) ->
    ?assertMatch(
        {error, {unsafe_query, _}},
        couch_srt:run(
            couch_srt:query([
                couch_srt:from(all),
                couch_srt:group_by(<<"username">>, <<"ioq_calls">>)
            ])
        ),
        "Should detect `unsafe` when `all` matcher is used"
    ),
    ?assertMatch(
        {error, {unsafe_query, _}},
        couch_srt:run(
            couch_srt:query([
                couch_srt:from("docs_read"),
                couch_srt:group_by(fun dummy_key_fun/1, <<"ioq_calls">>)
            ])
        ),
        "Should detect `unsafe` when `AggregationKey` is a function()"
    ),
    ?assertMatch(
        {error, {unsafe_query, _}},
        couch_srt:run(
            couch_srt:query([
                couch_srt:from("docs_read"),
                couch_srt:group_by(<<"username">>, fun dummy_value_fun/1)
            ])
        ),
        "Should detect `unsafe` when `ValueKey` is a function()"
    ),
    ?assertMatch(
        {error, {unsafe_query, _}},
        couch_srt:run(
            couch_srt:query([
                couch_srt:from("docs_read"),
                couch_srt:group_by(<<"username">>, <<"ioq_calls">>),
                couch_srt:options([
                    couch_srt:unlimited()
                ])
            ])
        ),
        "Should detect `unsafe` when `unlimited()` is used"
    ),
    ok.

t_group_by_run_unsafe_query(_) ->
    ?assertMatch(
        {ok, _},
        couch_srt:unsafe_run(
            couch_srt:query([
                couch_srt:from(all),
                couch_srt:group_by(<<"username">>, <<"ioq_calls">>)
            ])
        ),
        "Should be able to use `unsafe_run` when `all` matcher is used"
    ),
    ?assertMatch(
        {ok, _},
        couch_srt:unsafe_run(
            couch_srt:query([
                couch_srt:from("docs_read"),
                couch_srt:group_by(fun dummy_key_fun/1, <<"ioq_calls">>)
            ])
        ),
        "Should be able to use `unsafe_run`  when `AggregationKey` is a function()"
    ),
    ?assertMatch(
        {ok, _},
        couch_srt:unsafe_run(
            couch_srt:query([
                couch_srt:from("docs_read"),
                couch_srt:group_by(<<"username">>, fun dummy_value_fun/1)
            ])
        ),
        "Should be able to use `unsafe_run`  when `ValueKey` is a function()"
    ),
    ?assertMatch(
        {ok, _},
        couch_srt:unsafe_run(
            couch_srt:query([
                couch_srt:from("docs_read"),
                couch_srt:group_by(<<"username">>, <<"ioq_calls">>),
                couch_srt:options([
                    couch_srt:unlimited()
                ])
            ])
        ),
        "Should be able to use `unsafe_run` when `unlimited()` is used"
    ),
    ok.

t_group_by_run_unsafe_correctness(_) ->
    % we are checking that safe analog of the query return same result
    ?assertEqual(
        couch_srt:run(
            couch_srt:query([
                couch_srt:from("docs_read"),
                couch_srt:group_by(<<"username">>, <<"ioq_calls">>)
            ])
        ),
        couch_srt:unsafe_run(
            couch_srt:query([
                couch_srt:from(all),
                couch_srt:group_by(<<"username">>, <<"ioq_calls">>)
            ])
        ),
        "Should get correct result from `unsafe_run` when `all` matcher is used"
    ),
    ?assertEqual(
        couch_srt:run(
            couch_srt:query([
                couch_srt:from("docs_read"),
                couch_srt:group_by(<<"username">>, <<"ioq_calls">>)
            ])
        ),
        couch_srt:unsafe_run(
            couch_srt:query([
                couch_srt:from("docs_read"),
                couch_srt:group_by(fun dummy_key_fun/1, <<"ioq_calls">>)
            ])
        ),
        "Should get correct result from `unsafe_run`  when `AggregationKey` is a function()"
    ),
    ?assertEqual(
        couch_srt:run(
            couch_srt:query([
                couch_srt:from("docs_read"),
                couch_srt:group_by(<<"username">>, ioq_calls)
            ])
        ),
        couch_srt:unsafe_run(
            couch_srt:query([
                couch_srt:from("docs_read"),
                couch_srt:group_by(<<"username">>, fun dummy_value_fun/1)
            ])
        ),
        "Should get correct result from `unsafe_run`  when `ValueKey` is a function()"
    ),
    ?assertEqual(
        couch_srt:run(
            couch_srt:query([
                couch_srt:from("docs_read"),
                couch_srt:group_by(<<"username">>, <<"ioq_calls">>)
            ])
        ),
        couch_srt:unsafe_run(
            couch_srt:query([
                couch_srt:from("docs_read"),
                couch_srt:group_by(<<"username">>, <<"ioq_calls">>),
                couch_srt:options([
                    couch_srt:unlimited()
                ])
            ])
        ),
        "Should get correct result from `unsafe_run` when `unlimited()` is used"
    ),
    ok.

t_group_by_bad_request(_) ->
    ?assertMatch(
        {error, [{unknown_matcher, "unknown_matcher"}]},
        couch_srt:query([
            couch_srt:from("unknown_matcher"),
            couch_srt:group_by(<<"username">>, <<"ioq_calls">>)
        ]),
        "Should return error if 'matcher' is unknown"
    ),
    ?assertMatch(
        {error, [{unknown_matcher, rows_read}]},
        couch_srt:query([
            couch_srt:from(rows_read),
            couch_srt:group_by([username, dbname], ioq_calls)
        ]),
        "Should return error if 'matcher' is not a string()"
    ),
    ?assertMatch(
        {error, [{invalid_key, "unknown_field"}]},
        couch_srt:query([
            couch_srt:from("docs_read"),
            couch_srt:group_by("unknown_field", ioq_calls)
        ]),
        "Should return error if 'AggregationKeys' contain unknown field"
    ),
    ?assertMatch(
        {error, [{invalid_key, "unknown_field"}]},
        couch_srt:query([
            couch_srt:from("docs_read"),
            couch_srt:group_by("username", "unknown_field")
        ]),
        "Should return error if 'ValueKey' contain unknown field"
    ),
    ?assertMatch(
        {error, [{beyond_limit, ?QUERY_LIMIT + 1}]},
        couch_srt:query([
            couch_srt:from("docs_read"),
            couch_srt:group_by("username", ioq_calls),
            couch_srt:options([
                couch_srt:with_limit(?QUERY_LIMIT + 1)
            ])
        ]),
        "Should return error when 'limit' is greater than configured"
    ),
    ok.

t_count_by_multiple_keys(#{rctxs := Rctxs}) ->
    Aggregated = aggregate([username, dbname], ioq_calls, Rctxs),
    Grouped = count(Aggregated),
    V1 = maps:get({<<"user_bar">>, <<"db1">>}, Grouped),
    V2 = maps:get({<<"user_bar">>, <<"db2">>}, Grouped),
    V3 = maps:get({<<"user_foo">>, <<"db1">>}, Grouped),
    V4 = maps:get({<<"user_foo">>, <<"db2">>}, Grouped),
    Q = couch_srt:query([
        couch_srt:from("docs_read"),
        couch_srt:count_by([<<"username">>, <<"dbname">>])
    ]),
    ?assertMatch(
        {ok, #{
            {<<"user_bar">>, <<"db1">>} := V1,
            {<<"user_bar">>, <<"db2">>} := V2,
            {<<"user_foo">>, <<"db1">>} := V3,
            {<<"user_foo">>, <<"db2">>} := V4
        }},
        couch_srt:run(Q)
    ),
    ok.

t_count_by_single_key(#{rctxs := Rctxs}) ->
    Aggregated = aggregate([username], ioq_calls, Rctxs),
    Grouped = count(Aggregated),
    V1 = maps:get({<<"user_bar">>}, Grouped),
    V2 = maps:get({<<"user_foo">>}, Grouped),
    Q = couch_srt:query([
        couch_srt:from("docs_read"),
        couch_srt:count_by([<<"username">>])
    ]),
    ?assertMatch(
        {ok, #{
            {<<"user_bar">>} := V1,
            {<<"user_foo">>} := V2
        }},
        couch_srt:run(Q)
    ),
    ok.

t_count_by_binary_key(#{rctxs := Rctxs}) ->
    Aggregated = aggregate([username], ioq_calls, Rctxs),
    Grouped = count(Aggregated),
    V1 = maps:get({<<"user_bar">>}, Grouped),
    V2 = maps:get({<<"user_foo">>}, Grouped),
    Q = couch_srt:query([
        couch_srt:from("docs_read"),
        couch_srt:count_by(<<"username">>)
    ]),
    ?assertMatch(
        {ok, #{
            <<"user_bar">> := V1,
            <<"user_foo">> := V2
        }},
        couch_srt:run(Q)
    ),
    ok.

t_count_by_bad_request(_) ->
    ?assertMatch(
        {error, [{unknown_matcher, "unknown_matcher"}]},
        couch_srt:query([
            couch_srt:from("unknown_matcher"),
            couch_srt:count_by(<<"username">>)
        ]),
        "Should return error if 'matcher' is unknown"
    ),
    ?assertMatch(
        {error, [{unknown_matcher, rows_read}]},
        couch_srt:query([
            couch_srt:from(rows_read),
            couch_srt:count_by([username, dbname])
        ]),
        "Should return error if 'matcher' is not a string()"
    ),
    ?assertMatch(
        {error, [{invalid_key, "unknown_field"}]},
        couch_srt:query([
            couch_srt:from("docs_read"),
            couch_srt:count_by("unknown_field")
        ]),
        "Should return error if 'AggregationKeys' contain unknown field"
    ),
    ?assertMatch(
        {error, [{beyond_limit, ?QUERY_LIMIT + 1}]},
        couch_srt:query([
            couch_srt:from("docs_read"),
            couch_srt:count_by("username"),
            couch_srt:options([
                couch_srt:with_limit(?QUERY_LIMIT + 1)
            ])
        ]),
        "Should return error when 'limit' is greater than configured"
    ),
    ok.

t_sort_by_multiple_keys(#{rctxs := Rctxs}) ->
    Aggregated = aggregate([username, dbname], ioq_calls, Rctxs),
    Grouped = group(Aggregated),
    Ordered = order_by_value(Grouped),
    [
        {{<<"user_foo">>, <<"db2">>}, V1},
        {{<<"user_bar">>, <<"db2">>}, V2},
        {{<<"user_bar">>, <<"db1">>}, V3},
        {{<<"user_foo">>, <<"db1">>}, V4}
    ] = Ordered,
    Q = couch_srt:query([
        couch_srt:from("docs_read"),
        couch_srt:sort_by([<<"username">>, <<"dbname">>], <<"ioq_calls">>)
    ]),
    ?assertMatch(
        {ok, [
            {{<<"user_foo">>, <<"db2">>}, V1},
            {{<<"user_bar">>, <<"db2">>}, V2},
            {{<<"user_bar">>, <<"db1">>}, V3},
            {{<<"user_foo">>, <<"db1">>}, V4}
        ]},
        couch_srt:run(Q)
    ),
    ok.

t_sort_by_single_key(#{rctxs := Rctxs}) ->
    Aggregated = aggregate([username], ioq_calls, Rctxs),
    Grouped = group(Aggregated),
    Ordered = order_by_value(Grouped),
    [
        {{<<"user_bar">>}, V1},
        {{<<"user_foo">>}, V2}
    ] = Ordered,
    Q = couch_srt:query([
        couch_srt:from("docs_read"),
        couch_srt:sort_by([<<"username">>], <<"ioq_calls">>)
    ]),
    ?assertMatch(
        {ok, [
            {{<<"user_bar">>}, V1},
            {{<<"user_foo">>}, V2}
        ]},
        couch_srt:run(Q)
    ),
    ok.

t_sort_by_binary_key(#{rctxs := Rctxs}) ->
    Aggregated = aggregate([username], ioq_calls, Rctxs),
    Grouped = group(Aggregated),
    Ordered = order_by_value(Grouped),
    [
        {{<<"user_bar">>}, V1},
        {{<<"user_foo">>}, V2}
    ] = Ordered,
    Q = couch_srt:query([
        couch_srt:from("docs_read"),
        couch_srt:sort_by(<<"username">>, <<"ioq_calls">>)
    ]),
    ?assertMatch(
        {ok, [
            {<<"user_bar">>, V1},
            {<<"user_foo">>, V2}
        ]},
        couch_srt:run(Q)
    ),
    ok.

t_sort_by_bad_request(_) ->
    ?assertMatch(
        {error, [{unknown_matcher, "unknown_matcher"}]},
        couch_srt:query([
            couch_srt:from("unknown_matcher"),
            couch_srt:sort_by(<<"username">>, <<"ioq_calls">>)
        ]),
        "Should return error if 'matcher' is unknown"
    ),
    ?assertMatch(
        {error, [{unknown_matcher, "unknown_matcher"}]},
        couch_srt:query([
            couch_srt:from("unknown_matcher"),
            couch_srt:sort_by(<<"username">>)
        ]),
        "Should return error if 'matcher' is unknown"
    ),
    ?assertMatch(
        {error, [{unknown_matcher, rows_read}]},
        couch_srt:query([
            couch_srt:from(rows_read),
            couch_srt:sort_by([username, dbname], ioq_calls)
        ]),
        "Should return error if 'matcher' is not a string()"
    ),
    ?assertMatch(
        {error, [{unknown_matcher, rows_read}]},
        couch_srt:query([
            couch_srt:from(rows_read),
            couch_srt:sort_by([username, dbname])
        ]),
        "Should return error if 'matcher' is not a string()"
    ),
    ?assertMatch(
        {error, [{invalid_key, "unknown_field"}]},
        couch_srt:query([
            couch_srt:from("docs_read"),
            couch_srt:sort_by("unknown_field", ioq_calls)
        ]),
        "Should return error if 'AggregationKeys' contain unknown field"
    ),
    ?assertMatch(
        {error, [{invalid_key, "unknown_field"}]},
        couch_srt:query([
            couch_srt:from("docs_read"),
            couch_srt:sort_by("unknown_field")
        ]),
        "Should return error if 'AggregationKeys' contain unknown field"
    ),
    ?assertMatch(
        {error, [{invalid_key, "unknown_field"}]},
        couch_srt:query([
            couch_srt:from("docs_read"),
            couch_srt:sort_by("username", "unknown_field")
        ]),
        "Should return error if 'ValueKey' contain unknown field"
    ),
    ?assertMatch(
        {error, [{beyond_limit, ?QUERY_LIMIT + 1}]},
        couch_srt:query([
            couch_srt:from("docs_read"),
            couch_srt:sort_by("username", ioq_calls),
            couch_srt:options([
                couch_srt:with_limit(?QUERY_LIMIT + 1)
            ])
        ]),
        "Should return error when 'limit' is greater than configured"
    ),
    ?assertMatch(
        {error, [{beyond_limit, ?QUERY_LIMIT + 1}]},
        couch_srt:query([
            couch_srt:from("docs_read"),
            couch_srt:sort_by("username"),
            couch_srt:options([
                couch_srt:with_limit(?QUERY_LIMIT + 1)
            ])
        ]),
        "Should return error when 'limit' is greater than configured"
    ),
    ok.

add_matcher(Name, MSpec) ->
    persistent_term:put({csrt_logger, all_csrt_matchers}, #{
        Name => {MSpec, ets:match_spec_compile(MSpec)}
    }).

aggregate(AggregationKeys, ValField, Records) ->
    lists:foldl(
        fun(Rctx, Acc) ->
            Key = list_to_tuple([couch_srt_entry:value(Field, Rctx) || Field <- AggregationKeys]),
            CurrVal = maps:get(Key, Acc, []),
            maps:put(Key, [couch_srt_entry:value(ValField, Rctx) | CurrVal], Acc)
        end,
        #{},
        Records
    ).

group(Aggregated) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            maps:put(Key, lists:foldl(fun erlang:'+'/2, 0, Val), Acc)
        end,
        #{},
        Aggregated
    ).

count(Aggregated) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            maps:put(Key, lists:foldl(fun(_, A) -> A + 1 end, 0, Val), Acc)
        end,
        #{},
        Aggregated
    ).

order_by_value(Grouped) ->
    lists:reverse(lists:keysort(2, maps:to_list(Grouped))).
