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

-module(csrt_httpd_tests).

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

-define(USER, ?MODULE_STRING ++ "_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).

-define(JSON, "application/json").
-define(JSON_CT, {"Content-Type", ?JSON}).
-define(ACCEPT_JSON, {"Accept", ?JSON}).

%% Use different values than default configs to ensure they're picked up
-define(THRESHOLD_DBNAME_IO, 91).
-define(THRESHOLD_DOCS_READ, 123).
-define(THRESHOLD_DOCS_WRITTEN, 12).
-define(THRESHOLD_IOQ_CALLS, 439).
-define(THRESHOLD_ROWS_READ, 43).
-define(THRESHOLD_CHANGES, 79).
-define(THRESHOLD_LONG_REQS, 432).

-define(TEST_QUERY_LIMIT, 98).

csrt_httpd_test_() ->
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

setup_ctx() ->
    Ctx = test_util:start_couch([chttpd, fabric, couch_stats]),
    Hashed = couch_passwords:hash_admin_password(?PASS),
    HashedList = binary_to_list(Hashed),
    ok = config:set("admins", ?USER, HashedList, false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    DbName = binary_to_list(?tempdb()),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    {Ctx, Url, DbName}.

teardown(#{dbname := DbName, ctx := Ctx}) ->
    meck:unload(ioq),
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]),
    Persist = false,
    ok = config:delete("admins", ?USER, Persist),
    test_util:stop_couch(Ctx).

create_db(Top, Db) ->
    case req(put, Top ++ Db) of
        {201, #{}} -> ok;
        Error -> error({failed_to_create_test_db, Db, Error})
    end.

req(Method, Url) ->
    Headers = [?JSON_CT, ?AUTH, ?ACCEPT_JSON],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers),
    {Code, json_decode(Res)}.

req(Method, Url, #{} = Body) ->
    req(Method, Url, jiffy:encode(Body));
req(Method, Url, Body) ->
    Headers = [?JSON_CT, ?AUTH, ?ACCEPT_JSON],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, json_decode(Res)}.

json_decode(Bin) when is_binary(Bin) ->
    jiffy:decode(Bin, [return_maps]).

setup() ->
    {Ctx, Url, DbName} = setup_ctx(),
    configure(),
    ok = create_db(Url, DbName),
    create_docs(DbName),
    PidRef = mock_all_docs_req(DbName),
    MArgs = #mrargs{include_docs = false},
    _Res = fabric:all_docs(DbName, [?ADMIN_CTX], fun view_cb/2, [], MArgs),
    Rctx = load_rctx(PidRef),
    Rctxs = rctxs(),
    #{ctx => Ctx, dbname => DbName, rctx => Rctx, rctxs => Rctxs, url => Url}.


create_docs(DbName) ->
    Docs = make_docs(100),
    Opts = [],
    {ok, _} = fabric:update_docs(DbName, Docs, Opts),
    ok.

configure() ->
    config:set_boolean(?CSRT, "randomize_testing", false, false),
    config:set_boolean(?CSRT, "enable_reporting", true, false),
    config:set_boolean(?CSRT, "enable_rpc_reporting", true, false),

    ok = meck:new(ioq, [passthrough]),
    ok = meck:expect(ioq, bypass, fun(_, _) -> false end),

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
    ok = config:set(?CSRT, "query_limit", integer_to_list(?TEST_QUERY_LIMIT)),
    csrt_logger:reload_matchers(),
    ok.

%% we cannot use normal http request, because the `chttpd` calls
%% `csrt:destroy_context()` and we would remove entries from `ets`.
mock_all_docs_req(DbName) ->
    Method = 'GET',
    Path = "/" ++ DbName ++ "/_all_docs",
    Nonce = couch_util:to_hex(crypto:strong_rand_bytes(5)),
    Req = #httpd{method = Method, nonce = Nonce},
    {_, _} = PidRef = csrt:create_coordinator_context(Req, Path),
    csrt:set_context_username(<<"user_foo">>),
    csrt:set_context_dbname(?l2b(DbName)),
    PidRef.

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

active_resources(Url, MatchName, Body) ->
    req(post, Url ++ "/_active_resources/_match/" ++ MatchName, Body).


t_query_group_by(#{rctx := Rctx, dbname := DbName, url := Url}) ->
    DbNameBin = ?l2b(DbName),
    IoqCalls = Rctx#rctx.ioq_calls,
    Req = fun(AggregationKeys, CounterKey) ->
        #{
            <<"group_by">> => #{
                <<"aggregate_keys">> => AggregationKeys,
                <<"counter_key">> => CounterKey
            }
        }
    end,
    ?assertMatch(
        {200, [#{
            <<"key">> := #{<<"dbname">> := DbNameBin, <<"username">> := <<"user_foo">>},
            <<"value">> := IoqCalls}]},
        active_resources(Url, "rows_read", Req([<<"username">>, <<"dbname">>], <<"ioq_calls">>)),
        "Should handle 'AggregationKeys :: [binary(), ...]'"
    ),
    ?assertMatch(
        {200, [#{
            <<"key">> := #{<<"username">> := <<"user_foo">>},
            <<"value">> := IoqCalls}]},
        active_resources(Url, "rows_read", Req([<<"username">>], <<"ioq_calls">>)),
        "Should handle 'AggregationKeys :: [binary()]'"
    ),
    ?assertMatch(
        {200, [#{
            <<"key">> := #{<<"username">> := <<"user_foo">>},
            <<"value">> := IoqCalls}]},
        active_resources(Url, "rows_read", Req(<<"username">>, <<"ioq_calls">>)),
        "Should handle 'AggregationKeys :: binary()'"
    ),
    ?assertMatch(
        {400,
            #{<<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown matcher 'unknown_matcher'">>}},
        active_resources(Url, "unknown_matcher", Req([<<"username">>], <<"ioq_calls">>)),
        "Should return error if 'matcher' is unknown"
    ),
    ?assertMatch(
        {400,
            #{<<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>}},
        active_resources(Url, "rows_read", Req([<<"unknown_field">>], <<"ioq_calls">>)),
        "Should return error if 'AggregationKeys' contain unknown field"
    ),
    ?assertMatch(
        {400,
            #{<<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>}},
        active_resources(Url, "rows_read", Req(<<"unknown_field">>, <<"ioq_calls">>)),
        "Should return error if 'AggregationKeys' is unknown field"
    ),
    ?assertMatch(
        {400,
            #{<<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>}},
        active_resources(Url, "rows_read", Req(<<"username">>, <<"unknown_field">>)),
        "Should return error if 'ValueKey' contain unknown field"
    ),
    ok.

t_query_count_by(#{dbname := DbName, url := Url}) ->
    DbNameBin = ?l2b(DbName),
    IoqCount = 1,
    Req = fun(AggregationKeys) ->
        #{
            <<"count_by">> => #{
                <<"aggregate_keys">> => AggregationKeys
            }
        }
    end,
    ?assertMatch(
        {200, [#{
            <<"key">> := #{<<"dbname">> := DbNameBin, <<"username">> := <<"user_foo">>},
            <<"value">> := IoqCount}]},
        active_resources(Url, "rows_read", Req([<<"username">>, <<"dbname">>])),
        "Should handle 'AggregationKeys :: [binary(), ...]'"
    ),
    ?assertMatch(
        {200, [#{
            <<"key">> := #{<<"username">> := <<"user_foo">>},
            <<"value">> := IoqCount}]},
        active_resources(Url, "rows_read", Req([<<"username">>])),
        "Should handle 'AggregationKeys :: [binary()]'"
    ),
    ?assertMatch(
        {200, [#{
            <<"key">> := #{<<"username">> := <<"user_foo">>},
            <<"value">> := IoqCount}]},
        active_resources(Url, "rows_read", Req(<<"username">>)),
        "Should handle 'AggregationKeys :: binary()'"
    ),
    ?assertMatch(
        {400,
            #{<<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown matcher 'unknown_matcher'">>}},
        active_resources(Url, "unknown_matcher", Req([<<"username">>])),
        "Should return error if 'matcher' is unknown"
    ),
    ?assertMatch(
        {400,
            #{<<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>}},
        active_resources(Url, "rows_read", Req([<<"unknown_field">>])),
        "Should return error if 'AggregationKeys' contain unknown field"
    ),
    ?assertMatch(
        {400,
            #{<<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>}},
        active_resources(Url, "rows_read", Req(<<"unknown_field">>)),
        "Should return error if 'AggregationKeys' is unknown field"
    ),
    ok.

t_query_sort_by(#{rctx := Rctx, dbname := DbName, url := Url}) ->
    DbNameBin = ?l2b(DbName),
    IoqCalls = Rctx#rctx.ioq_calls,
    Req = fun(AggregationKeys, CounterKey) ->
        #{
            <<"sort_by">> => #{
                <<"aggregate_keys">> => AggregationKeys,
                <<"counter_key">> => CounterKey
            }
        }
    end,
    ?assertMatch(
        {200, [#{
            <<"key">> := #{<<"dbname">> := DbNameBin, <<"username">> := <<"user_foo">>},
            <<"value">> := IoqCalls}]},
        active_resources(Url, "rows_read", Req([<<"username">>, <<"dbname">>], <<"ioq_calls">>)),
        "Should handle 'AggregationKeys :: [binary(), ...]'"
    ),
    ?assertMatch(
        {200, [#{
            <<"key">> := #{<<"username">> := <<"user_foo">>},
            <<"value">> := IoqCalls}]},
        active_resources(Url, "rows_read", Req([<<"username">>], <<"ioq_calls">>)),
        "Should handle 'AggregationKeys :: [binary()]'"
    ),
    ?assertMatch(
        {200, [#{
            <<"key">> := #{<<"username">> := <<"user_foo">>},
            <<"value">> := IoqCalls}]},
        active_resources(Url, "rows_read", Req(<<"username">>, <<"ioq_calls">>)),
        "Should handle 'AggregationKeys :: binary()'"
    ),
    ?assertMatch(
        {400,
            #{<<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown matcher 'unknown_matcher'">>}},
        active_resources(Url, "unknown_matcher", Req([<<"username">>], <<"ioq_calls">>)),
        "Should return error if 'matcher' is unknown"
    ),
    ?assertMatch(
        {400,
            #{<<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>}},
        active_resources(Url, "rows_read", Req([<<"unknown_field">>], <<"ioq_calls">>)),
        "Should return error if 'AggregationKeys' contain unknown field"
    ),
    ?assertMatch(
        {400,
            #{<<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>}},
        active_resources(Url, "rows_read", Req(<<"unknown_field">>, <<"ioq_calls">>)),
        "Should return error if 'AggregationKeys' is unknown field"
    ),
    ?assertMatch(
        {400,
            #{<<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>}},
        active_resources(Url, "rows_read", Req(<<"username">>, <<"unknown_field">>)),
        "Should return error if 'ValueKey' contain unknown field"
    ),
    ok.
