#! /usr/bin/env escript

lifetime() -> 100.

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    tutil:run(2, fun() -> test() end).


test() ->
    {ok, LRU} = ets_lru:start_link(lru, [{max_lifetime, lifetime()}]),
    ok = test_single_entry(LRU),
    ok = ets_lru:stop(LRU).


test_single_entry(LRU) ->
    ets_lru:insert(LRU, foo, bar),
    etap:is(ets_lru:lookup(LRU, foo), {ok, bar}, "Expire leaves new entries"),
    timer:sleep(round(lifetime() * 1.5)),
    etap:is(ets_lru:lookup(LRU, foo), not_found, "Entry was expired"),
    ok.
