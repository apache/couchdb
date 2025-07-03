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

-include_lib("couch/include/couch_eunit.hrl").
-include("../../src/couch_stats_resource_tracker.hrl").

-define(USER, ?MODULE_STRING ++ "_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).

-define(JSON, "application/json").
-define(JSON_CT, {"Content-Type", ?JSON}).
-define(ACCEPT_JSON, {"Accept", ?JSON}).

csrt_httpd_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_query_group_by_multiple_keys),
            ?TDEF_FE(t_query_group_by_single_key),
            ?TDEF_FE(t_query_group_by_binary_key),
            ?TDEF_FE(t_query_group_by_bad_request),
            ?TDEF_FE(t_query_count_by_multiple_keys),
            ?TDEF_FE(t_query_count_by_single_key),
            ?TDEF_FE(t_query_count_by_binary_key),
            ?TDEF_FE(t_query_count_by_bad_request),
            ?TDEF_FE(t_query_sort_by_multiple_keys),
            ?TDEF_FE(t_query_sort_by_single_key),
            ?TDEF_FE(t_query_sort_by_binary_key),
            ?TDEF_FE(t_query_sort_by_bad_request)
        ]
    }.

setup_ctx() ->
    Ctx = test_util:start_couch([chttpd, fabric, couch_stats]),
    Hashed = couch_passwords:hash_admin_password(?PASS),
    HashedList = binary_to_list(Hashed),
    ok = config:set("admins", ?USER, HashedList, false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/"]),
    {Ctx, Url}.

setup() ->
    {Ctx, Url} = setup_ctx(),
    Rctxs = [
        rctx(#{dbname => <<"db1">>, ioq_calls => 123, username => <<"user_foo">>}),
        rctx(#{dbname => <<"db1">>, ioq_calls => 321, username => <<"user_foo">>}),
        rctx(#{dbname => <<"db2">>, ioq_calls => 345, username => <<"user_bar">>}),
        rctx(#{dbname => <<"db2">>, ioq_calls => 543, username => <<"user_bar">>}),
        rctx(#{dbname => <<"db1">>, ioq_calls => 678, username => <<"user_bar">>}),
        rctx(#{dbname => <<"db2">>, ioq_calls => 987, username => <<"user_foo">>})
    ],
    ets:insert(?CSRT_ETS, Rctxs),
    #{ctx => Ctx, url => Url, rctxs => Rctxs}.

teardown(#{ctx := Ctx}) ->
    Persist = false,
    ok = config:delete("admins", ?USER, Persist),
    test_util:stop_couch(Ctx).

active_resources_group_by(Url, AggregationKeys, CounterKey) ->
    active_resources_group_by("docs_read", Url, AggregationKeys, CounterKey).

active_resources_group_by(MatcherName, Url, AggregationKeys, CounterKey) ->
    Body = #{
        <<"group_by">> => #{
            <<"aggregate_keys">> => AggregationKeys,
            <<"counter_key">> => CounterKey
        }
    },
    active_resources(Url, MatcherName, Body).

t_query_group_by_multiple_keys(#{rctxs := Rctxs, url := Url}) ->
    Aggregated = aggregate([username, dbname], ioq_calls, Rctxs),
    Grouped = group(Aggregated),
    {RC, Result} = active_resources_group_by(Url, [<<"username">>, <<"dbname">>], <<"ioq_calls">>),
    ?assertEqual(200, RC, format("Should have '200' return code, got ~p~n  ~p~n", [RC, Result])),
    ?assert(is_list(Result), format("Expected list of entries, got ~p~n", [Result])),
    ?assertEqual(
        4, length(Result), format("Expected four entries, got ~p~n  ~p~n", [length(Result), Result])
    ),
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := _, <<"dbname">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _, <<"dbname">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _, <<"dbname">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _, <<"dbname">> := _}, <<"value">> := _}
        ],
        Result,
        "Unexpected shape of the result"
    ),
    OrderedByKey = order_by_key([username, dbname], Result),
    V1 = maps:get({<<"user_bar">>, <<"db1">>}, Grouped),
    V2 = maps:get({<<"user_bar">>, <<"db2">>}, Grouped),
    V3 = maps:get({<<"user_foo">>, <<"db1">>}, Grouped),
    V4 = maps:get({<<"user_foo">>, <<"db2">>}, Grouped),
    ?assertMatch(
        [
            #{
                <<"key">> := #{<<"username">> := <<"user_bar">>, <<"dbname">> := <<"db1">>},
                <<"value">> := V1
            },
            #{
                <<"key">> := #{<<"username">> := <<"user_bar">>, <<"dbname">> := <<"db2">>},
                <<"value">> := V2
            },
            #{
                <<"key">> := #{<<"username">> := <<"user_foo">>, <<"dbname">> := <<"db1">>},
                <<"value">> := V3
            },
            #{
                <<"key">> := #{<<"username">> := <<"user_foo">>, <<"dbname">> := <<"db2">>},
                <<"value">> := V4
            }
        ],
        OrderedByKey
    ),
    ok.

t_query_group_by_single_key(#{rctxs := Rctxs, url := Url}) ->
    Aggregated = aggregate([username], ioq_calls, Rctxs),
    Grouped = group(Aggregated),
    {RC, Result} = active_resources_group_by(Url, [<<"username">>], <<"ioq_calls">>),
    ?assertEqual(200, RC, format("Should have '200' return code, got ~p~n  ~p~n", [RC, Result])),
    ?assert(is_list(Result), format("Expected list of entries, got ~p~n", [Result])),
    ?assertEqual(
        2, length(Result), format("Expected two entries, got ~p~n  ~p~n", [length(Result), Result])
    ),
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _}, <<"value">> := _}
        ],
        Result,
        "Unexpected shape of the result"
    ),
    OrderedByKey = order_by_key([username], Result),
    V1 = maps:get({<<"user_bar">>}, Grouped),
    V2 = maps:get({<<"user_foo">>}, Grouped),
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := <<"user_bar">>}, <<"value">> := V1},
            #{<<"key">> := #{<<"username">> := <<"user_foo">>}, <<"value">> := V2}
        ],
        OrderedByKey
    ),
    ok.

t_query_group_by_binary_key(#{rctxs := Rctxs, url := Url}) ->
    Aggregated = aggregate([username], ioq_calls, Rctxs),
    Grouped = group(Aggregated),
    {RC, Result} = active_resources_group_by(Url, <<"username">>, <<"ioq_calls">>),
    ?assertEqual(200, RC, format("Should have '200' return code, got ~p~n  ~p~n", [RC, Result])),
    ?assert(is_list(Result), format("Expected list of entries, got ~p~n", [Result])),
    ?assertEqual(
        2, length(Result), format("Expected two entries, got ~p~n  ~p~n", [length(Result), Result])
    ),
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _}, <<"value">> := _}
        ],
        Result,
        format("Unexpected shape of the result~n  ~p~n", [Result])
    ),
    OrderedByKey = order_by_key([username], Result),
    V1 = maps:get({<<"user_bar">>}, Grouped),
    V2 = maps:get({<<"user_foo">>}, Grouped),
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := <<"user_bar">>}, <<"value">> := V1},
            #{<<"key">> := #{<<"username">> := <<"user_foo">>}, <<"value">> := V2}
        ],
        OrderedByKey
    ),
    ok.

t_query_group_by_bad_request(#{url := Url}) ->
    ?assertMatch(
        {400, #{
            <<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown matcher 'unknown_matcher'">>
        }},
        active_resources_group_by("unknown_matcher", Url, <<"username">>, <<"ioq_calls">>),
        "Should return error if 'matcher' is unknown"
    ),
    ?assertMatch(
        {400, #{
            <<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>
        }},
        active_resources_group_by(Url, [<<"unknown_field">>], <<"ioq_calls">>),
        "Should return error if 'AggregationKeys' contain unknown field"
    ),
    ?assertMatch(
        {400, #{
            <<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>
        }},
        active_resources_group_by(Url, <<"unknown_field">>, <<"ioq_calls">>),
        "Should return error if 'AggregationKeys' is unknown field"
    ),
    ?assertMatch(
        {400, #{
            <<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>
        }},
        active_resources_group_by(Url, <<"username">>, <<"unknown_field">>),
        "Should return error if 'ValueKey' contain unknown field"
    ),
    ok.

active_resources_count_by(Url, AggregationKeys) ->
    active_resources_count_by("docs_read", Url, AggregationKeys).

active_resources_count_by(MatcherName, Url, AggregationKeys) ->
    Body = #{
        <<"count_by">> => #{
            <<"aggregate_keys">> => AggregationKeys
        }
    },
    active_resources(Url, MatcherName, Body).

t_query_count_by_multiple_keys(#{rctxs := Rctxs, url := Url}) ->
    Aggregated = aggregate([username, dbname], ioq_calls, Rctxs),
    Grouped = count(Aggregated),
    {RC, Result} = active_resources_count_by(Url, [<<"username">>, <<"dbname">>]),
    ?assertEqual(200, RC, format("Should have '200' return code, got ~p~n  ~p~n", [RC, Result])),
    ?assert(is_list(Result), format("Expected list of entries, got ~p~n", [Result])),
    ?assertEqual(
        4, length(Result), format("Expected four entries, got ~p~n  ~p~n", [length(Result), Result])
    ),
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := _, <<"dbname">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _, <<"dbname">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _, <<"dbname">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _, <<"dbname">> := _}, <<"value">> := _}
        ],
        Result,
        "Unexpected shape of the result"
    ),
    OrderedByKey = order_by_key([username, dbname], Result),
    V1 = maps:get({<<"user_bar">>, <<"db1">>}, Grouped),
    V2 = maps:get({<<"user_bar">>, <<"db2">>}, Grouped),
    V3 = maps:get({<<"user_foo">>, <<"db1">>}, Grouped),
    V4 = maps:get({<<"user_foo">>, <<"db2">>}, Grouped),
    ?assertMatch(
        [
            #{
                <<"key">> := #{<<"username">> := <<"user_bar">>, <<"dbname">> := <<"db1">>},
                <<"value">> := V1
            },
            #{
                <<"key">> := #{<<"username">> := <<"user_bar">>, <<"dbname">> := <<"db2">>},
                <<"value">> := V2
            },
            #{
                <<"key">> := #{<<"username">> := <<"user_foo">>, <<"dbname">> := <<"db1">>},
                <<"value">> := V3
            },
            #{
                <<"key">> := #{<<"username">> := <<"user_foo">>, <<"dbname">> := <<"db2">>},
                <<"value">> := V4
            }
        ],
        OrderedByKey
    ),
    ok.

t_query_count_by_single_key(#{rctxs := Rctxs, url := Url}) ->
    Aggregated = aggregate([username], ioq_calls, Rctxs),
    Grouped = count(Aggregated),
    {RC, Result} = active_resources_count_by(Url, [<<"username">>]),
    ?assertEqual(200, RC, format("Should have '200' return code, got ~p~n  ~p~n", [RC, Result])),
    ?assert(is_list(Result), format("Expected list of entries, got ~p~n", [Result])),
    ?assertEqual(
        2, length(Result), format("Expected two entries, got ~p~n  ~p~n", [length(Result), Result])
    ),
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _}, <<"value">> := _}
        ],
        Result,
        "Unexpected shape of the result"
    ),
    OrderedByKey = order_by_key([username], Result),
    V1 = maps:get({<<"user_bar">>}, Grouped),
    V2 = maps:get({<<"user_foo">>}, Grouped),
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := <<"user_bar">>}, <<"value">> := V1},
            #{<<"key">> := #{<<"username">> := <<"user_foo">>}, <<"value">> := V2}
        ],
        OrderedByKey
    ),
    ok.

t_query_count_by_binary_key(#{rctxs := Rctxs, url := Url}) ->
    Aggregated = aggregate([username], ioq_calls, Rctxs),
    Grouped = count(Aggregated),
    {RC, Result} = active_resources_count_by(Url, <<"username">>),
    ?assertEqual(200, RC, format("Should have '200' return code, got ~p~n  ~p~n", [RC, Result])),
    ?assert(is_list(Result), format("Expected list of entries, got ~p~n", [Result])),
    ?assertEqual(
        2, length(Result), format("Expected two entries, got ~p~n  ~p~n", [length(Result), Result])
    ),
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _}, <<"value">> := _}
        ],
        Result,
        "Unexpected shape of the result"
    ),
    OrderedByKey = order_by_key([username], Result),
    V1 = maps:get({<<"user_bar">>}, Grouped),
    V2 = maps:get({<<"user_foo">>}, Grouped),
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := <<"user_bar">>}, <<"value">> := V1},
            #{<<"key">> := #{<<"username">> := <<"user_foo">>}, <<"value">> := V2}
        ],
        OrderedByKey
    ),
    ok.

t_query_count_by_bad_request(#{url := Url}) ->
    ?assertMatch(
        {400, #{
            <<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown matcher 'unknown_matcher'">>
        }},
        active_resources_count_by("unknown_matcher", Url, <<"username">>),
        "Should return error if 'matcher' is unknown"
    ),
    ?assertMatch(
        {400, #{
            <<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>
        }},
        active_resources_count_by(Url, [<<"unknown_field">>]),
        "Should return error if 'AggregationKeys' contain unknown field"
    ),
    ?assertMatch(
        {400, #{
            <<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>
        }},
        active_resources_count_by(Url, <<"unknown_field">>),
        "Should return error if 'AggregationKeys' is unknown field"
    ),
    ok.

active_resources_sort_by(Url, AggregationKeys, CounterKey) ->
    active_resources_sort_by("docs_read", Url, AggregationKeys, CounterKey).

active_resources_sort_by(MatcherName, Url, AggregationKeys, CounterKey) ->
    Body = #{
        <<"sort_by">> => #{
            <<"aggregate_keys">> => AggregationKeys,
            <<"counter_key">> => CounterKey
        }
    },
    active_resources(Url, MatcherName, Body).

t_query_sort_by_multiple_keys(#{rctxs := Rctxs, url := Url}) ->
    Aggregated = aggregate([username, dbname], ioq_calls, Rctxs),
    Grouped = group(Aggregated),
    Ordered = order_by_value(Grouped),
    {RC, Result} = active_resources_sort_by(Url, [<<"username">>, <<"dbname">>], <<"ioq_calls">>),
    ?assertEqual(200, RC, format("Should have '200' return code, got ~p~n  ~p~n", [RC, Result])),
    ?assert(is_list(Result), format("Expected list of entries, got ~p~n", [Result])),
    ?assertEqual(
        4, length(Result), format("Expected four entries, got ~p~n  ~p~n", [length(Result), Result])
    ),
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := _, <<"dbname">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _, <<"dbname">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _, <<"dbname">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _, <<"dbname">> := _}, <<"value">> := _}
        ],
        Result,
        "Unexpected shape of the result"
    ),
    [
        {{<<"user_foo">>, <<"db2">>}, V1},
        {{<<"user_bar">>, <<"db2">>}, V2},
        {{<<"user_bar">>, <<"db1">>}, V3},
        {{<<"user_foo">>, <<"db1">>}, V4}
    ] = Ordered,
    ?assertMatch(
        [
            #{
                <<"key">> := #{<<"username">> := <<"user_foo">>, <<"dbname">> := <<"db2">>},
                <<"value">> := V1
            },
            #{
                <<"key">> := #{<<"username">> := <<"user_bar">>, <<"dbname">> := <<"db2">>},
                <<"value">> := V2
            },
            #{
                <<"key">> := #{<<"username">> := <<"user_bar">>, <<"dbname">> := <<"db1">>},
                <<"value">> := V3
            },
            #{
                <<"key">> := #{<<"username">> := <<"user_foo">>, <<"dbname">> := <<"db1">>},
                <<"value">> := V4
            }
        ],
        Result
    ),
    ok.

t_query_sort_by_single_key(#{rctxs := Rctxs, url := Url}) ->
    Aggregated = aggregate([username], ioq_calls, Rctxs),
    Grouped = group(Aggregated),
    Ordered = order_by_value(Grouped),
    {RC, Result} = active_resources_sort_by(Url, [<<"username">>], <<"ioq_calls">>),
    ?assertEqual(200, RC, format("Should have '200' return code, got ~p~n  ~p~n", [RC, Result])),
    ?assert(is_list(Result), format("Expected list of entries, got ~p~n", [Result])),
    ?assertEqual(
        2, length(Result), format("Expected two entries, got ~p~n  ~p~n", [length(Result), Result])
    ),
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _}, <<"value">> := _}
        ],
        Result,
        "Unexpected shape of the result"
    ),
    [
        {{<<"user_bar">>}, V1},
        {{<<"user_foo">>}, V2}
    ] = Ordered,
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := <<"user_bar">>}, <<"value">> := V1},
            #{<<"key">> := #{<<"username">> := <<"user_foo">>}, <<"value">> := V2}
        ],
        Result
    ),
    ok.

t_query_sort_by_binary_key(#{rctxs := Rctxs, url := Url}) ->
    Aggregated = aggregate([username], ioq_calls, Rctxs),
    Grouped = group(Aggregated),
    Ordered = order_by_value(Grouped),
    {RC, Result} = active_resources_sort_by(Url, <<"username">>, <<"ioq_calls">>),
    ?assertEqual(200, RC, format("Should have '200' return code, got ~p~n  ~p~n", [RC, Result])),
    ?assert(is_list(Result), format("Expected list of entries, got ~p~n", [Result])),
    ?assertEqual(
        2, length(Result), format("Expected two entries, got ~p~n  ~p~n", [length(Result), Result])
    ),
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := _}, <<"value">> := _},
            #{<<"key">> := #{<<"username">> := _}, <<"value">> := _}
        ],
        Result,
        "Unexpected shape of the result"
    ),
    [
        {{<<"user_bar">>}, V1},
        {{<<"user_foo">>}, V2}
    ] = Ordered,
    ?assertMatch(
        [
            #{<<"key">> := #{<<"username">> := <<"user_bar">>}, <<"value">> := V1},
            #{<<"key">> := #{<<"username">> := <<"user_foo">>}, <<"value">> := V2}
        ],
        Result
    ),
    ok.

t_query_sort_by_bad_request(#{url := Url}) ->
    ?assertMatch(
        {400, #{
            <<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown matcher 'unknown_matcher'">>
        }},
        active_resources_sort_by("unknown_matcher", Url, <<"username">>, <<"ioq_calls">>),
        "Should return error if 'matcher' is unknown"
    ),
    ?assertMatch(
        {400, #{
            <<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>
        }},
        active_resources_sort_by(Url, [<<"unknown_field">>], <<"ioq_calls">>),
        "Should return error if 'AggregationKeys' contain unknown field"
    ),
    ?assertMatch(
        {400, #{
            <<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>
        }},
        active_resources_sort_by(Url, <<"unknown_field">>, <<"ioq_calls">>),
        "Should return error if 'AggregationKeys' is unknown field"
    ),
    ?assertMatch(
        {400, #{
            <<"error">> := <<"bad_request">>,
            <<"reason">> := <<"Unknown field name 'unknown_field'">>
        }},
        active_resources_sort_by(Url, <<"username">>, <<"unknown_field">>),
        "Should return error if 'ValueKey' contain unknown field"
    ),
    ok.

format(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).

aggregate(AggregationKeys, ValField, Records) ->
    lists:foldl(
        fun(Rctx, Acc) ->
            Key = list_to_tuple([csrt_entry:value(Field, Rctx) || Field <- AggregationKeys]),
            CurrVal = maps:get(Key, Acc, []),
            maps:put(Key, [csrt_entry:value(ValField, Rctx) | CurrVal], Acc)
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

% This function handles both representations of entries of the result
% #{<<"key">> => #{<<"dbname">> => <<"db2">>, <<"username">> => <<"user_foo">>}, <<"value">> => 1}
% and
% {{<<"db2">>, <<"user_foo">>}, 1}
order_by_key(AggregationKeys, Entries) when is_list(AggregationKeys) andalso is_list(Entries) ->
    lists:sort(
        fun(A, B) ->
            get_key(AggregationKeys, A) =< get_key(AggregationKeys, B)
        end,
        Entries
    ).

% This function handles both representations of entries of the result
% #{<<"key">> => #{<<"dbname">> => <<"db2">>, <<"username">> => <<"user_foo">>}, <<"value">> => 1}
% and
% {{<<"db2">>, <<"user_foo">>}, 1}
get_key(AggregationKeys, #{<<"key">> := Key}) ->
    list_to_tuple([maps:get(atom_to_binary(Field), Key) || Field <- AggregationKeys]);
get_key(_AggregationKeys, {Key, _}) ->
    Key.

active_resources(Url, MatchName, Body) ->
    EndpointUrl = Url ++ "/_active_resources/_match/" ++ MatchName,
    Headers = [?JSON_CT, ?AUTH, ?ACCEPT_JSON],
    {ok, Code, _, Res} = test_request:request(post, EndpointUrl, Headers, jiffy:encode(Body)),
    {Code, jiffy:decode(Res, [return_maps])}.

rctx(Opts) ->
    % Update `docs_read` to make standard `{docs_read, fun matcher_on_docs_read/1, 1000}`
    % matcher match.
    Threshold = config:get("csrt_logger.matchers_threshold", "rows_read", 1000),
    BaseOpts = #{docs_read => Threshold + 1, username => <<"user_foo">>},
    csrt_test_helper:rctx_gen(maps:merge(BaseOpts, Opts)).
