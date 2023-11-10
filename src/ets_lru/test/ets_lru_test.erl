-module(ets_lru_test).

-include_lib("eunit/include/eunit.hrl").

lifecyle_test_() ->
    {
        "Test LRU lifecycle",
        {setup, fun() -> ets_lru:start_link(?MODULE, []) end,
            fun({ok, LRU}) -> is_process_alive(LRU) == false end, fun({ok, LRU}) ->
                [
                    {
                        "ets_lru:start_link/2 returned an LRU",
                        ?_assert(is_pid(LRU))
                    },
                    {
                        "Destroyed the LRU ok",
                        ?_assertEqual(ok, ets_lru:stop(LRU))
                    }
                ]
            end}
    }.

table_names_test_() ->
    {
        "Test tables names",
        {setup, fun() -> ets_lru:start_link(foo, []) end, fun({ok, LRU}) ->
            [
                {
                    "foo_objects exists",
                    ?_assertEqual(0, ets:info(foo_objects, size))
                },
                {
                    "foo_atimes exists",
                    ?_assertEqual(0, ets:info(foo_atimes, size))
                },
                {
                    "foo_ctimes exists",
                    ?_assertEqual(0, ets:info(foo_ctimes, size))
                },
                {
                    "LRU stopped normally",
                    ?_test(begin
                        Reason = stop_lru({ok, LRU}),
                        ?assertEqual(normal, Reason)
                    end)
                },
                {
                    "foo_objects doesn't exists",
                    ?_assertEqual(undefined, ets:info(foo_objects, size))
                },
                {
                    "foo_atimes doesn't exists",
                    ?_assertEqual(undefined, ets:info(foo_atimes, size))
                },
                {
                    "foo_ctimes doesn't exists",
                    ?_assertEqual(undefined, ets:info(foo_ctimes, size))
                }
            ]
        end}
    }.

basic_behavior_test_() ->
    {
        "Test basic behavior",
        {foreach,
            fun() ->
                {ok, LRU} = ets_lru:start_link(test_lru, []),
                ok = ets_lru:insert(LRU, foo, bar),
                {ok, LRU}
            end,
            fun stop_lru/1, [
                fun({ok, LRU}) ->
                    {
                        "Lookup returned the inserted value",
                        ?_assertEqual({ok, bar}, ets_lru:lookup(LRU, foo))
                    }
                end,
                fun({ok, _LRU}) ->
                    {
                        "Dirty lookup returned the inserted value",
                        ?_assertEqual(
                            {ok, bar},
                            ets_lru:lookup_d(test_lru, foo)
                        )
                    }
                end,
                fun({ok, LRU}) ->
                    [
                        {
                            "Lookup returned the inserted value",
                            ?_assertEqual({ok, bar}, ets_lru:lookup(LRU, foo))
                        },
                        {
                            "Insert new value",
                            ?_assertEqual(ok, ets_lru:insert(LRU, foo, bam))
                        },
                        {
                            "Lookup returned the newly inserted value",
                            ?_assertEqual({ok, bam}, ets_lru:lookup(LRU, foo))
                        }
                    ]
                end,
                fun({ok, LRU}) ->
                    [
                        {
                            "Lookup returned the inserted value",
                            ?_assertEqual({ok, bar}, ets_lru:lookup(LRU, foo))
                        },
                        {
                            "Remove value",
                            ?_assertEqual(ok, ets_lru:remove(LRU, foo))
                        },
                        {
                            "Lookup returned not_found for removed value",
                            ?_assertEqual(not_found, ets_lru:lookup(LRU, foo))
                        }
                    ]
                end,
                fun({ok, LRU}) ->
                    [
                        {
                            "Lookup returned the inserted value",
                            ?_assertEqual({ok, bar}, ets_lru:lookup(LRU, foo))
                        },
                        {
                            "Clear LRU",
                            ?_assertEqual(ok, ets_lru:clear(LRU))
                        },
                        {
                            "Lookup returned not_found after a clear",
                            ?_assertEqual(not_found, ets_lru:lookup(LRU, foo))
                        }
                    ]
                end,
                fun({ok, LRU}) ->
                    [
                        {
                            "Insert the value twice",
                            ?_assertEqual(ok, ets_lru:insert(LRU, foo, bar))
                        },
                        {
                            "Objects table size should be 1",
                            ?_assertEqual(1, ets:info(test_lru_objects, size))
                        },
                        {
                            "ATimes table size should be 1",
                            ?_assertEqual(1, ets:info(test_lru_atimes, size))
                        },
                        {
                            "CTimes table size should be 1",
                            ?_assertEqual(1, ets:info(test_lru_ctimes, size))
                        },
                        {
                            "Clear LRU after duplicate insert",
                            ?_assertEqual(ok, ets_lru:clear(LRU))
                        },
                        {
                            "Lookup returned not_found after a clear after a duplicate insert",
                            ?_assertEqual(not_found, ets_lru:lookup(LRU, foo))
                        }
                    ]
                end
            ]}
    }.

lru_good_options_test_() ->
    {
        "Test good LRU options",
        {foreachx,
            fun(Opts) ->
                process_flag(trap_exit, true),
                ets_lru:start_link(?MODULE, Opts)
            end,
            fun(_, Cfg) -> stop_lru(Cfg) end, [
                {[{max_objects, 1}], fun test_good_opts/2},
                {[{max_objects, 5}], fun test_good_opts/2},
                {[{max_objects, 923928342098203942}], fun test_good_opts/2},
                {[{max_size, 1}], fun test_good_opts/2},
                {[{max_size, 5}], fun test_good_opts/2},
                {[{max_size, 2342923423942309423094}], fun test_good_opts/2},
                {[{max_lifetime, 1}], fun test_good_opts/2},
                {[{max_lifetime, 5}], fun test_good_opts/2},
                {[{max_lifetime, 1244209909180928348}], fun test_good_opts/2},
                {[{max_idle, 1}], fun test_good_opts/2},
                {[{max_idle, 5}], fun test_good_opts/2},
                {[{max_idle, 1244209909180928348}], fun test_good_opts/2}
            ]}
    }.

lru_bad_options_test_() ->
    {
        "Test bad LRU options",
        {foreachx,
            fun(Opts) ->
                process_flag(trap_exit, true),
                ets_lru:start_link(?MODULE, Opts)
            end,
            fun(_, _) ->
                case whereis(?MODULE) of
                    Pid when is_pid(Pid) ->
                        stop_lru({ok, Pid});
                    undefined ->
                        ok
                end
            end,
            [
                {[{bingo, bango}], fun test_bad_opts/2},
                {[12], fun test_bad_opts/2},
                {[true], fun test_bad_opts/2}
            ]}
    }.

lru_limits_test_() ->
    {
        "Test LRU limits",
        {foreachx, fun(Opts) -> ets_lru:start_link(lru, Opts) end, fun(_, Cfg) -> stop_lru(Cfg) end,
            [
                {[{max_objects, 25}], fun test_limits/2},
                {[{max_size, 1024}], fun test_limits/2},
                {[{max_lifetime, 100}], fun test_limits/2},
                {[{max_idle, 100}], fun test_limits/2}
            ]}
    }.

lru_match_test_() ->
    {
        "Test match functions",
        {foreach, fun() -> ets_lru:start_link(test_lru, []) end, fun stop_lru/1, [
            fun({ok, LRU}) ->
                {
                    "Empty match",
                    ?_assertEqual([], ets_lru:match(LRU, a, '$1'))
                }
            end,
            fun({ok, LRU}) ->
                ets_lru:insert(LRU, b, {x, y}),
                {
                    "Single match",
                    ?_assertEqual(
                        [[x, y]],
                        ets_lru:match(LRU, b, {'$1', '$2'})
                    )
                }
            end,
            fun({ok, LRU}) ->
                ets_lru:insert(LRU, boston, {state, "MA"}),
                ets_lru:insert(LRU, new_york, {state, "NY"}),
                Values = ets_lru:match(LRU, '_', {state, '$1'}),
                {
                    "Multiple match",
                    ?_assertEqual([["MA"], ["NY"]], lists:sort(Values))
                }
            end,
            fun({ok, LRU}) ->
                {
                    "Empty match_object",
                    ?_assertEqual([], ets_lru:match_object(LRU, a, '$1'))
                }
            end,
            fun({ok, LRU}) ->
                ets_lru:insert(LRU, ans, 42),
                [
                    {
                        "Single match_object (registered)",
                        ?_assertEqual(
                            [42],
                            ets_lru:match_object(test_lru, ans, '$1')
                        )
                    },
                    {
                        "Single match_object (pid)",
                        ?_assertEqual(
                            [42],
                            ets_lru:match_object(LRU, ans, '$1')
                        )
                    }
                ]
            end,
            fun({ok, LRU}) ->
                ets_lru:insert(LRU, {color, blue}, a),
                ets_lru:insert(LRU, {color, red}, b),
                Values = ets_lru:match_object(LRU, {color, '_'}, '_'),
                {
                    "Multiple match_object",
                    ?_assertEqual(lists:sort(Values), [a, b])
                }
            end
        ]}
    }.

test_good_opts(Opts, {ok, LRU}) ->
    Msg = io_lib:format("LRU created ok with options: ~w", [Opts]),
    {lists:flatten(Msg), ?_assert(is_pid(LRU))};
test_good_opts(Opts, ErrorMsg) ->
    Msg = io_lib:format("LRU created ok with options: ~w", [Opts]),
    {lists:flatten(Msg), ?_assertEqual(ok, ErrorMsg)}.

test_bad_opts([Opts], {error, {bad_return_value, {invalid_option, Opts2}}}) ->
    Msg = io_lib:format("LRU failed with bad options: ~w", [Opts]),
    {lists:flatten(Msg), ?_assertEqual(Opts, Opts2)}.

test_limits([{max_objects, N}], {ok, LRU}) ->
    {
        "Max object count ok",
        ?_assert(insert_kvs(size, LRU, 100 * N, N))
    };
test_limits([{max_size, N}], {ok, LRU}) ->
    {
        "Max size ok",
        ?_assert(insert_kvs(memory, LRU, 10 * N, N))
    };
test_limits([{Max, N}], {ok, LRU}) when Max == max_lifetime; Max == max_idle ->
    [
        {
            "Expire leaves new entries",
            ?_test(begin
                ets_lru:insert(LRU, foo, bar),
                ?assertEqual({ok, bar}, ets_lru:lookup(LRU, foo))
            end)
        },
        {
            "Entry was expired",
            ?_test(begin
                timer:sleep(round(N * 1.5)),
                test_util:wait(fun() ->
                    case ets_lru:lookup(LRU, foo) of
                        not_found -> ok;
                        _ -> wait
                    end
                end),
                ?assertEqual(not_found, ets_lru:lookup(LRU, foo))
            end)
        }
    ].

insert_kvs(_, _, 0, _) ->
    true;
insert_kvs(Info, LRU, Count, Limit) ->
    ets_lru:insert(LRU, Count, 1.5234),
    case ets:info(lru_objects, Info) > Limit of
        true ->
            % Retry again as eviction statistics
            % returned by ets:info() can be delayed.
            timer:sleep(1),
            case ets:info(lru_objects, Info) > Limit of
                true ->
                    erlang:error(exceeded_limit);
                false ->
                    true
            end;
        false ->
            true
    end,
    insert_kvs(Info, LRU, Count - 1, Limit).

stop_lru({ok, LRU}) ->
    Ref = erlang:monitor(process, LRU),
    ets_lru:stop(LRU),
    receive
        {'DOWN', Ref, process, LRU, Reason} -> Reason
    end;
stop_lru({error, _}) ->
    ok.

valid_parameterized_time_unit_test() ->
    Opts = [{time_unit, microsecond}],
    {ok, LRU} = ets_lru:start_link(lru_test, Opts),
    ?assert(is_process_alive(LRU)),
    ok = ets_lru:insert(LRU, foo, bar),
    ?assertEqual({ok, bar}, ets_lru:lookup(LRU, foo)),
    ?assertEqual(ok, ets_lru:stop(LRU)).

invalid_parameterized_time_unit_test() ->
    Opts = [{time_unit, invalid}],
    {ok, LRU} = ets_lru:start_link(lru_test, Opts),
    ?assertExit(_, ets_lru:insert(LRU, foo, bar)).
