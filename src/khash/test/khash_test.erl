-module(khash_test).

-export([
    khash_fetch/0,
    khash_store/0,
    run_keys/1
]).

-include_lib("eunit/include/eunit.hrl").

-define(NUM_RAND_CYCLES, 10000).
-define(NUM_CYCLES, 1000000).
-define(NUM_KVS, 5000).
-define(TIMEOUT, 15).

load_test_() ->
    {
        "Loaded khash",
        ?_assertEqual({module, khash}, code:load_file(khash))
    }.

basic_test_() ->
    {
        "khash basic operations",
        {setup, local, fun() -> khash:new() end, fun({ok, _}) -> ok end, fun({ok, C}) ->
            [
                {
                    "Lookup missing is ok",
                    ?_assertEqual(not_found, khash:lookup(C, <<"foo">>))
                },
                {
                    "Get missing is ok",
                    ?_assertEqual(undefined, khash:get(C, <<"foo">>))
                },
                {
                    "Del missing is ok",
                    ?_assertEqual(not_found, khash:del(C, <<"foo">>))
                },
                {
                    "Stored a key",
                    ?_assertEqual(ok, khash:put(C, <<"foo">>, bar))
                },
                {
                    "Lookuped a key",
                    ?_assertEqual({value, bar}, khash:lookup(C, <<"foo">>))
                },
                {
                    "Retrieved a key",
                    ?_assertEqual(bar, khash:get(C, <<"foo">>))
                },
                {
                    "Stored a key",
                    ?_assertEqual(ok, khash:put(C, <<"bar">>, foo))
                },
                {
                    "Correct size for hash",
                    ?_assertEqual(2, khash:size(C))
                },
                {
                    "Deleted a key",
                    ?_assertEqual(ok, khash:del(C, <<"foo">>))
                },
                {
                    "Correct size after delete",
                    ?_assertEqual(1, khash:size(C))
                },
                {
                    "Cleared the hash",
                    ?_assertEqual(ok, khash:clear(C))
                },
                {
                    "Correct size after clear",
                    ?_assertEqual(0, khash:size(C))
                }
            ]
        end}
    }.

randomized_test_() ->
    {
        "khash randomized test",
        {setup, local,
            fun() ->
                Dict = dict:new(),
                {ok, KHash} = khash:new(),
                Actions = [
                    {0.1, fun run_clear/1},
                    {1.0, fun run_get2/1},
                    {1.0, fun run_get3/1},
                    {1.0, fun run_put/1},
                    {1.0, fun run_del/1},
                    {0.5, fun run_size/1},
                    % {0.3, fun run_keys/1},
                    {0.3, fun run_to_list/1}
                ],
                {ok, Actions, ?NUM_RAND_CYCLES, {Dict, KHash}}
            end,
            fun(State) ->
                {timeout, ?TIMEOUT, {
                    "State matches dict implementation",
                    ?_assert(run_randomized(State, true))
                }}
            end}
    }.

basic_iterators_test_() ->
    {
        "khash itrators basics operations",
        {setup, local,
            fun() ->
                {ok, H} = khash:new(),
                khash:put(H, foo, bar),
                {ok, I} = khash:iter(H),
                {ok, H, I}
            end,
            fun({ok, H, I}) ->
                [
                    {
                        "Got only kv pair as first element",
                        ?_assertEqual(khash:iter_next(I), {foo, bar})
                    },
                    {
                        "Only the one kv pair exists",
                        ?_assertEqual(khash:iter_next(I), end_of_table)
                    },
                    {
                        "Fold works",
                        ?_test(begin
                            Fold = fun(K, V, Acc) -> [{K, V} | Acc] end,
                            ?assertEqual(khash:fold(H, Fold, []), [{foo, bar}])
                        end)
                    }
                ]
            end}
    }.

multiread_iterators_test_() ->
    {
        "khash iterators multi-read test",
        {setup, local,
            fun() ->
                {ok, H} = khash:new(),
                KVs = kv_data(10),
                lists:foreach(fun({K, V}) -> khash:put(H, K, V) end, KVs),
                {ok, I} = khash:iter(H),
                {ok, I, KVs}
            end,
            fun({ok, I, KVs}) ->
                {
                    "Read the same exact key/val pairs",
                    ?_assertEqual(khash_iterate(I, []), KVs)
                }
            end}
    }.

expiration_iterators_test_() ->
    {
        "khash iterators exipiration functions",
        {foreach, local,
            fun() ->
                Err = {error, expired_iterator},
                {ok, H} = khash:new(),
                khash:put(H, foo, bar),
                {ok, I} = khash:iter(H),
                {ok, H, I, Err}
            end,
            [
                fun({ok, H, I, Err}) ->
                    khash:put(H, foo, bar2),
                    {
                        "put expires iterators",
                        ?_assertEqual(khash:iter_next(I), Err)
                    }
                end,
                fun({ok, H, I, Err}) ->
                    khash:del(H, foo),
                    {
                        "del expires iterators",
                        ?_assertEqual(khash:iter_next(I), Err)
                    }
                end,
                fun({ok, H, I, Err}) ->
                    khash:clear(H),
                    {
                        "clear expires iterators",
                        ?_assertEqual(khash:iter_next(I), Err)
                    }
                end
            ]}
    }.

no_expiration_iterators_test_() ->
    {
        "khash iterators no exipiration functions",
        {foreach, local,
            fun() ->
                Err = {error, expired_iterator},
                {ok, H} = khash:new(),
                khash:put(H, foo, bar),
                {ok, I} = khash:iter(H),
                {ok, H, I, Err}
            end,
            [
                fun({ok, H, I, Err}) ->
                    [{foo, bar}] = khash:to_list(H),
                    {
                        "to_list doesn't expire iterators",
                        ?_assertNot(khash:iter_next(I) == Err)
                    }
                end,
                fun({ok, H, I, Err}) ->
                    {value, bar} = khash:lookup(H, foo),
                    {
                        "lookup doesn't expire iterators",
                        ?_assertNot(khash:iter_next(I) == Err)
                    }
                end,
                fun({ok, H, I, Err}) ->
                    bar = khash:get(H, foo),
                    {
                        "get doesn't expire iterators",
                        ?_assertNot(khash:iter_next(I) == Err)
                    }
                end,
                fun({ok, H, I, Err}) ->
                    1 = khash:size(H),
                    {
                        "size doesn't expire iterators",
                        ?_assertNot(khash:iter_next(I) == Err)
                    }
                end,
                fun({ok, H, I, Err}) ->
                    {ok, _OtherI} = khash:iter(H),
                    {foo, bar} = khash:iter_next(I),
                    end_of_table = khash:iter_next(I),
                    {
                        "iteration doesn't expire iterators",
                        ?_assertNot(khash:iter_next(I) == Err)
                    }
                end
            ]}
    }.

khash_fetch() ->
    erlang:garbage_collect(),
    List = kv_data(?NUM_KVS),
    {ok, KHash} = khash:from_list(List),
    khash_fetch(KHash, ?NUM_CYCLES * 10).

khash_fetch(_, 0) ->
    ok;
khash_fetch(KHash, NumCycles) ->
    ?assertMatch(1, khash:get(KHash, 1)),
    khash_fetch(KHash, NumCycles - 1).

khash_store() ->
    List = kv_data(?NUM_KVS * 2),
    {ok, KHash} = khash:from_list(List),
    khash_store(KHash, ?NUM_CYCLES).

khash_store(_, 0) ->
    ok;
khash_store(KHash, NumCycles) ->
    khash:put(KHash, 1, 1),
    khash_store(KHash, NumCycles - 1).

khash_iterate(I, Acc) ->
    case khash:iter_next(I) of
        {K, V} ->
            khash_iterate(I, [{K, V} | Acc]);
        end_of_table ->
            lists:sort(Acc)
    end.

kv_data(NumKVs) ->
    [{I, I} || I <- lists:seq(1, NumKVs)].

run_randomized({ok, _, N, _}, Valid) when N =< 0 ->
    Valid;
run_randomized({ok, Actions, N, S0}, Valid) ->
    Action = weighted_choice(Actions),
    S1 = Action(S0),
    Assertion = Valid andalso validate_randomized_state(S1),
    run_randomized({ok, Actions, N - 1, S1}, Assertion).

validate_randomized_state({D, H}) ->
    DKVs = lists:sort(dict:to_list(D)),
    HKVs = lists:sort(khash:to_list(H)),
    DKVs == HKVs.

run_clear({_D0, H}) ->
    ?assertMatch(ok, khash:clear(H)),
    {dict:new(), H}.

run_get2({D, H}) ->
    K = random_key(D),
    case dict:find(K, D) of
        {ok, Value} ->
            ?assertMatch({value, Value}, khash:lookup(H, K)),
            ?assertMatch(Value, khash:get(H, K));
        error ->
            ?assertMatch(not_found, khash:lookup(H, K)),
            ?assertMatch(undefined, khash:get(H, K))
    end,
    {D, H}.

run_get3({D, H}) ->
    K = random_key(D),
    case dict:find(K, D) of
        {ok, Value} ->
            ?assertMatch({value, Value}, khash:lookup(H, K)),
            ?assertMatch(Value, khash:get(H, K));
        error ->
            Val = random_val(),
            ?assertMatch(Val, khash:get(H, K, Val))
    end,
    {D, H}.

run_put({D0, H}) ->
    K = random_key(D0),
    V = random_val(),
    D1 = dict:store(K, V, D0),
    ?assertMatch(ok, khash:put(H, K, V)),
    {D1, H}.

run_del({D0, H}) ->
    K = random_key(D0),
    D1 =
        case dict:is_key(K, D0) of
            true ->
                ?assertMatch(ok, khash:del(H, K)),
                dict:erase(K, D0);
            false ->
                ?assertMatch(not_found, khash:del(H, K)),
                D0
        end,
    {D1, H}.

run_size({D, H}) ->
    ?assertEqual(dict:size(D), khash:size(H)),
    {D, H}.

run_keys({D, H}) ->
    DKeys = dict:fetch_keys(D),
    {ok, HKeys0} = khash:keys(H),
    HKeys = lists:sort(HKeys0),
    ?assertEqual(lists:sort(DKeys), lists:sort(HKeys)),
    {D, H}.

run_to_list({D, H}) ->
    DKVs = dict:to_list(D),
    HKVs = khash:to_list(H),
    ?assertEqual(lists:sort(DKVs), lists:sort(HKVs)),
    {D, H}.

weighted_choice(Items0) ->
    Items = lists:sort(Items0),
    Sum = lists:sum([W || {W, _} <- Items]),
    Choice = rand:uniform() * Sum,
    weighted_choice(Items, 0.0, Choice).

weighted_choice([], _, _) ->
    throw(bad_choice);
weighted_choice([{W, _} | Rest], S, C) when W + S < C ->
    weighted_choice(Rest, W + S, C);
weighted_choice([{_, I} | _], _, _) ->
    I.

random_key(D) ->
    Keys = lists:usort(dict:fetch_keys(D) ++ [foo]),
    lists:nth(rand:uniform(length(Keys)), Keys).

random_val() ->
    gen_term:any().
