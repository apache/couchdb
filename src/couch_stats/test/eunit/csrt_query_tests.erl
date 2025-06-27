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

-module(csrt_query_tests).

-include_lib("stdlib/include/ms_transform.hrl").

-import(
    csrt_test_helper,
    [
        rctx_gen/0,
        rctx_gen/1,
        rctxs/0,
        jrctx/1
    ]
).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include("../../src/couch_stats_resource_tracker.hrl").

%% Use different values than default configs to ensure they're picked up
-define(THRESHOLD_DBNAME_IO, 91).
-define(THRESHOLD_DOCS_READ, 123).
-define(THRESHOLD_DOCS_WRITTEN, 12).
-define(THRESHOLD_IOQ_CALLS, 439).
-define(THRESHOLD_ROWS_READ, 43).
-define(THRESHOLD_CHANGES, 79).
-define(THRESHOLD_LONG_REQS, 432).

-define(TEST_QUERY_LIMIT, 98).

csrt_query_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_query_group_by),
            ?TDEF_FE(t_query_count_by),
            ?TDEF_FE(t_query_sort_by)
        ]
    }.

setup() ->
    Ctx = test_util:start_couch([fabric, couch_stats]),
    config:set_boolean(?CSRT, "randomize_testing", false, false),
    config:set_boolean(?CSRT, "enable_reporting", true, false),
    config:set_boolean(?CSRT, "enable_rpc_reporting", true, false),

    ok = meck:new(ioq, [passthrough]),
    ok = meck:expect(ioq, bypass, fun(_, _) -> false end),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, 8}, {n, 1}]),
    Docs = make_docs(100),
    Opts = [],
    {ok, _} = fabric:update_docs(DbName, Docs, Opts),
    Method = 'GET',
    Path = "/" ++ ?b2l(DbName) ++ "/_all_docs",
    Nonce = couch_util:to_hex(crypto:strong_rand_bytes(5)),
    Req = #httpd{method = Method, nonce = Nonce},
    {_, _} = PidRef = csrt:create_coordinator_context(Req, Path),
    csrt:set_context_username(<<"user_foo">>),
    csrt:set_context_dbname(DbName),
    MArgs = #mrargs{include_docs = false},
    _Res = fabric:all_docs(DbName, [?ADMIN_CTX], fun view_cb/2, [], MArgs),
    Rctx = load_rctx(PidRef),
    ok = config:set(
        "csrt_logger.matchers_threshold", "docs_read", integer_to_list(?THRESHOLD_DOCS_READ), false
    ),
    ok = config:set(
        "csrt_logger.matchers_threshold",
        "docs_written",
        integer_to_list(?THRESHOLD_DOCS_WRITTEN),
        false
    ),
    ok = config:set(
        "csrt_logger.matchers_threshold", "ioq_calls", integer_to_list(?THRESHOLD_IOQ_CALLS), false
    ),
    ok = config:set(
        "csrt_logger.matchers_threshold", "rows_read", integer_to_list(?THRESHOLD_ROWS_READ), false
    ),
    ok = config:set(
        "csrt_logger.matchers_threshold",
        "changes_processed",
        integer_to_list(?THRESHOLD_CHANGES),
        false
    ),
    ok = config:set(
        "csrt_logger.matchers_threshold", "long_reqs", integer_to_list(?THRESHOLD_LONG_REQS), false
    ),
    ok = config:set("csrt_logger.dbnames_io", "foo", integer_to_list(?THRESHOLD_DBNAME_IO), false),
    ok = config:set("csrt_logger.dbnames_io", "bar", integer_to_list(?THRESHOLD_DBNAME_IO), false),
    ok = config:set(
        "csrt_logger.dbnames_io", "foo/bar", integer_to_list(?THRESHOLD_DBNAME_IO), false
    ),
    config:set(?CSRT, "query_limit", integer_to_list(?TEST_QUERY_LIMIT)),
    csrt_logger:reload_matchers(),
    Rctxs = rctxs(),
    #{ctx => Ctx, dbname => DbName, rctx => Rctx, rctxs => Rctxs}.

teardown(#{ctx := Ctx, dbname := DbName}) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]),
    ok = meck:unload(ioq),
    test_util:stop_couch(Ctx).

load_rctx(PidRef) ->
    %% Add slight delay to accumulate RPC response deltas
    timer:sleep(50),
    csrt:get_resource(PidRef).

make_docs(Count) ->
    lists:map(
        fun(I) ->
            #doc{
                id = ?l2b("foo_" ++ integer_to_list(I)),
                body = {[{<<"value">>, I}]}
            }
        end,
        lists:seq(1, Count)
    ).

view_cb({row, Row}, Acc) ->
    {ok, [Row | Acc]};
view_cb(_Msg, Acc) ->
    {ok, Acc}.

t_query_group_by(#{rctx := Rctx, dbname := DbName}) ->
    IoqCalls = Rctx#rctx.ioq_calls,
    ?assertMatch(
        {ok, #{{<<"user_foo">>, DbName} := IoqCalls}},
        csrt_query:query_group_by("rows_read", [username, dbname], <<"ioq_calls">>, #{}),
        "Should handle 'AggregationKeys :: [atom(), ...]'"
    ),
    ?assertMatch(
        {ok, #{{<<"user_foo">>} := IoqCalls}},
        csrt_query:query_group_by("rows_read", [username], <<"ioq_calls">>, #{}),
        "Should handle 'AggregationKeys :: [atom()]'"
    ),
    ?assertMatch(
        {ok, #{{<<"user_foo">>} := IoqCalls}},
        csrt_query:query_group_by("rows_read", ["username"], <<"ioq_calls">>, #{}),
        "Should handle 'AggregationKeys :: [string()]'"
    ),
    ?assertMatch(
        {ok, #{<<"user_foo">> := IoqCalls}},
        csrt_query:query_group_by("rows_read", username, <<"ioq_calls">>, #{}),
        "Should handle 'AggregationKeys :: atom()'"
    ),
    ?assertMatch(
        {ok, #{<<"user_foo">> := IoqCalls}},
        csrt_query:query_group_by("rows_read", "username", <<"ioq_calls">>, #{}),
        "Should handle 'AggregationKeys :: string()'"
    ),
    ?assertMatch(
        {ok, #{{<<"user_foo">>, DbName} := IoqCalls}},
        csrt_query:query_group_by("rows_read", [username, dbname], "ioq_calls", #{}),
        "Should handle 'ValueKey :: string()'"
    ),
    ?assertMatch(
        {ok, #{{<<"user_foo">>, DbName} := IoqCalls}},
        csrt_query:query_group_by("rows_read", [username, dbname], ioq_calls, #{}),
        "Should handle 'ValueKey :: atom()'"
    ),
    ?assertMatch(
        {ok, #{{<<"user_foo">>, DbName} := IoqCalls}},
        csrt_query:query_group_by("rows_read", [username, dbname], ioq_calls, #{limit => ?TEST_QUERY_LIMIT - 1}),
        "Should handle 'limit' option"
    ),
    ?assertMatch(
        {error,{unknown_matcher,"unknown_matcher"}},
        csrt_query:query_group_by("unknown_matcher", [username, dbname], ioq_calls, #{}),
        "Should return error if 'matcher' is unknown"
    ),
    ?assertMatch(
        {error,{unknown_matcher, rows_read}},
        csrt_query:query_group_by(rows_read, [username, dbname], ioq_calls, #{}),
        "Should return error if 'matcher' is not a string()"
    ),
    ?assertMatch(
        {error, {invalid_key, "unknown_field"}},
        csrt_query:query_group_by("rows_read", "unknown_field", ioq_calls, #{}),
        "Should return error if 'AggregationKeys' contain unknown field"
    ),
    ?assertMatch(
        {error, {invalid_key, "unknown_field"}},
        csrt_query:query_group_by("rows_read", "username", "unknown_field", #{}),
        "Should return error if 'ValueKey' contain unknown field"
    ),
    ?assertMatch(
        {error, {beyond_limit, ?TEST_QUERY_LIMIT}},
        csrt_query:query_group_by("rows_read", [username, dbname], ioq_calls, #{limit => ?TEST_QUERY_LIMIT + 1}),
        "Should return error when 'limit' is greater than configured"
    ),
    ok.

t_query_count_by(#{dbname := DbName}) ->
    IoqCount = 1,
    ?assertMatch(
        {ok, #{{<<"user_foo">>, DbName} := IoqCount}},
        csrt_query:query_count_by("rows_read", [username, dbname], #{}),
        "Should handle 'AggregationKeys :: [atom(), ...]'"
    ),
    ?assertMatch(
        {ok, #{{<<"user_foo">>} := IoqCount}},
        csrt_query:query_count_by("rows_read", [username], #{}),
        "Should handle 'AggregationKeys :: [atom()]'"
    ),
    ?assertMatch(
        {ok, #{{<<"user_foo">>} := IoqCount}},
        csrt_query:query_count_by("rows_read", ["username"], #{}),
        "Should handle 'AggregationKeys :: [string()]'"
    ),
    ?assertMatch(
        {ok, #{<<"user_foo">> := IoqCount}},
        csrt_query:query_count_by("rows_read", username, #{}),
        "Should handle 'AggregationKeys :: atom()'"
    ),
    ?assertMatch(
        {ok, #{<<"user_foo">> := IoqCount}},
        csrt_query:query_count_by("rows_read", "username", #{}),
        "Should handle 'AggregationKeys :: string()'"
    ),
    ?assertMatch(
        {ok, #{<<"user_foo">> := IoqCount}},
        csrt_query:query_count_by("rows_read", "username", #{limit => ?TEST_QUERY_LIMIT - 1}),
        "Should handle 'limit' option"
    ),
    ?assertMatch(
        {error,{unknown_matcher,"unknown_matcher"}},
        csrt_query:query_count_by("unknown_matcher", [username, dbname], #{}),
        "Should return error if 'matcher' is unknown"
    ),
    ?assertMatch(
        {error,{unknown_matcher, rows_read}},
        csrt_query:query_count_by(rows_read, [username, dbname], #{}),
        "Should return error if 'matcher' is not a string()"
    ),
    ?assertMatch(
        {error, {invalid_key, "unknown_field"}},
        csrt_query:query_count_by("rows_read", "unknown_field", #{}),
        "Should return error if 'AggregationKeys' contain unknown field"
    ),
    ?assertMatch(
        {error, {beyond_limit, ?TEST_QUERY_LIMIT}},
        csrt_query:query_count_by("rows_read", [username, dbname], #{limit => ?TEST_QUERY_LIMIT + 1}),
        "Should return error when 'limit' is greater than configured"
    ),
    ok.

t_query_sort_by(#{rctx := Rctx, dbname := DbName}) ->
    IoqCalls = Rctx#rctx.ioq_calls,
    ?assertMatch(
        {ok, [{{<<"user_foo">>, DbName}, IoqCalls}]},
        csrt_query:query_sort_by("rows_read", [username, dbname], <<"ioq_calls">>, #{}),
        "Should handle 'AggregationKeys :: [atom(), ...]'"
    ),
    ?assertMatch(
        {ok, [{{<<"user_foo">>}, IoqCalls}]},
        csrt_query:query_sort_by("rows_read", [username], <<"ioq_calls">>, #{}),
        "Should handle 'AggregationKeys :: [atom()]'"
    ),
    ?assertMatch(
        {ok, [{{<<"user_foo">>}, IoqCalls}]},
        csrt_query:query_sort_by("rows_read", ["username"], <<"ioq_calls">>, #{}),
        "Should handle 'AggregationKeys :: [string()]'"
    ),
    ?assertMatch(
        {ok, [{<<"user_foo">>, IoqCalls}]},
        csrt_query:query_sort_by("rows_read", username, <<"ioq_calls">>, #{}),
        "Should handle 'AggregationKeys :: atom()'"
    ),
    ?assertMatch(
        {ok, [{<<"user_foo">>, IoqCalls}]},
        csrt_query:query_sort_by("rows_read", "username", <<"ioq_calls">>, #{}),
        "Should handle 'AggregationKeys :: string()'"
    ),
    ?assertMatch(
        {ok, [{{<<"user_foo">>, DbName}, IoqCalls}]},
        csrt_query:query_sort_by("rows_read", [username, dbname], "ioq_calls", #{}),
        "Should handle 'ValueKey :: string()'"
    ),
    ?assertMatch(
        {ok, [{{<<"user_foo">>, DbName}, IoqCalls}]},
        csrt_query:query_sort_by("rows_read", [username, dbname], ioq_calls, #{}),
        "Should handle 'ValueKey :: atom()'"
    ),
    ?assertMatch(
        {ok, [{{<<"user_foo">>, DbName}, IoqCalls}]},
        csrt_query:query_sort_by("rows_read", [username, dbname], ioq_calls, #{limit => ?TEST_QUERY_LIMIT - 1}),
        "Should handle 'limit' option"
    ),
    ?assertMatch(
        {error,{unknown_matcher,"unknown_matcher"}},
        csrt_query:query_sort_by("unknown_matcher", [username, dbname], ioq_calls, #{}),
        "Should return error if 'matcher' is unknown"
    ),
    ?assertMatch(
        {error,{unknown_matcher, rows_read}},
        csrt_query:query_sort_by(rows_read, [username, dbname], ioq_calls, #{}),
        "Should return error if 'matcher' is not a string()"
    ),
    ?assertMatch(
        {error, {invalid_key, "unknown_field"}},
        csrt_query:query_sort_by("rows_read", "unknown_field", ioq_calls, #{}),
        "Should return error if 'AggregationKeys' contain unknown field"
    ),
    ?assertMatch(
        {error, {invalid_key, "unknown_field"}},
        csrt_query:query_sort_by("rows_read", "username", "unknown_field", #{}),
        "Should return error if 'ValueKey' contain unknown field"
    ),
    ?assertMatch(
        {error, {beyond_limit, ?TEST_QUERY_LIMIT}},
        csrt_query:query_sort_by("rows_read", [username, dbname], ioq_calls, #{limit => ?TEST_QUERY_LIMIT + 1}),
        "Should return error when 'limit' is greater than configured"
    ),
    ok.
