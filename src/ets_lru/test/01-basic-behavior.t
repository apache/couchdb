#! /usr/bin/env escript

-define(WITH_LRU(F), tutil:with_lru(fun(LRU) -> F(LRU) end)).

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    tutil:run(16, fun() -> test() end).


test() ->
    test_lifecycle(),
    test_table_names(),
    ?WITH_LRU(test_insert_lookup),
    ?WITH_LRU(test_insert_overwrite),
    ?WITH_LRU(test_insert_remove),
    ?WITH_LRU(test_clear),

    ok.


test_lifecycle() ->
    Resp = ets_lru:start_link(?MODULE, []),
    etap:fun_is(
        fun({ok, LRU}) when is_pid(LRU) -> true; (_) -> false end,
        Resp,
        "ets_lru:start_link/2 returned an LRU"
    ),
    {ok, LRU} = Resp,
    etap:is(ok, ets_lru:stop(LRU), "Destroyed the LRU ok").


test_table_names() ->
    {ok, LRU} = ets_lru:start_link(foo, []),
    Exists = fun(Name) -> ets:info(Name, size) == 0 end,
    NExists = fun(Name) -> ets:info(Name, size) == undefined end,
    etap:is(Exists(foo_objects), true, "foo_objects exists"),
    etap:is(Exists(foo_atimes), true, "foo_atimes exists"),
    etap:is(Exists(foo_ctimes), true, "foo_ctimes exists"),

    Ref = erlang:monitor(process, LRU),
    ets_lru:stop(LRU),

    receive {'DOWN', Ref, process, LRU, Reason} -> ok end,
    etap:is(Reason, normal, "LRU stopped normally"),

    etap:is(NExists(foo_objects), true, "foo_objects doesn't exist"),
    etap:is(NExists(foo_atimes), true, "foo_atimes doesn't exist"),
    etap:is(NExists(foo_ctimes), true, "foo_ctimes doesn't exist"),

    ok.


test_insert_lookup(LRU) ->
    ok = ets_lru:insert(LRU, foo, bar),
    Resp = ets_lru:lookup(LRU, foo),
    etap:is(Resp, {ok, bar}, "Lookup returned the inserted value").


test_insert_lookup_d(LRU) ->
    ok = ets_lru:insert(LRU, foo, bar),
    Resp = ets_lru:lookup_d(test_lru, foo),
    etap:is(Resp, {ok, bar}, "Dirty lookup returned the inserted value").


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


test_clear(LRU) ->
    ok = ets_lru:insert(LRU, foo, bar),
    Resp1 = ets_lru:lookup(LRU, foo),
    etap:is(Resp1, {ok, bar}, "Lookup returned the inserted value"),
    ok = ets_lru:clear(LRU),
    Resp2 = ets_lru:lookup(LRU, foo),
    etap:is(Resp2, not_found, "Lookup returned not_found after a clear").
