#! /usr/bin/env escript

-define(WITH_LRU(F), tutil:with_lru(fun(LRU) -> F(LRU) end)).

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    tutil:run(12, fun() -> test() end).


test() ->
    test_lifecycle(),
    ?WITH_LRU(test_insert_lookup),
    ?WITH_LRU(test_insert_overwrite),
    ?WITH_LRU(test_insert_remove),
    ?WITH_LRU(test_member),
    ?WITH_LRU(test_clear),

    ok.


test_lifecycle() ->
    Resp = ets_lru:create(?MODULE, []),
    etap:fun_is(
        fun({ok, _LRU}) -> true; (_) -> false end,
        Resp,
        "ets_lru:create/2 returned an LRU"
    ),
    {ok, LRU} = Resp,
    etap:is(ok, ets_lru:destroy(LRU), "Destroyed the LRU ok").


test_insert_lookup(LRU) ->
    ok = ets_lru:insert(LRU, foo, bar),
    Resp = ets_lru:lookup(LRU, foo),
    etap:is(Resp, {ok, bar}, "Lookup returned the inserted value").


test_insert_overwrite(LRU) ->
    ok = ets_lru:insert(LRU, foo, bar),
    Resp1 = ets_lru:lookup(LRU, foo),
    etap:is(Resp1, {ok, bar}, "Lookup returned the inserted value"),
    ok = ets_lru:insert(LRU, foo, bam),
    Resp2 = ets_lru:lookup(LRU, foo),
    etap:is(Resp2, {ok, bam}, "Lookup returned the newly inserted value").


test_insert_remove(LRU) ->
    ok = ets_lru:insert(LRU, foo, bar),
    Resp1 = ets_lru:lookup(LRU, foo),
    etap:is(Resp1, {ok, bar}, "Lookup returned the inserted value"),
    ok = ets_lru:remove(LRU, foo),
    Resp2 = ets_lru:lookup(LRU, foo),
    etap:is(Resp2, not_found, "Lookup returned not_found for removed value").


test_member(LRU) ->
    etap:is(false, ets_lru:member(LRU, foo), "Not yet a member: foo"),
    ok = ets_lru:insert(LRU, foo, bar),
    etap:is(true, ets_lru:member(LRU, foo), "Now a member: foo"),
    ok = ets_lru:remove(LRU, foo),
    etap:is(false, ets_lru:member(LRU, foo), "No longer a member: foo").
    

test_clear(LRU) ->
    ok = ets_lru:insert(LRU, foo, bar),
    Resp1 = ets_lru:lookup(LRU, foo),
    etap:is(Resp1, {ok, bar}, "Lookup returned the inserted value"),
    ok = ets_lru:clear(LRU),
    Resp2 = ets_lru:lookup(LRU, foo),
    etap:is(Resp2, not_found, "Lookup returned not_found after a clear").
