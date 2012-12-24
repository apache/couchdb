#! /usr/bin/env escript

lifetime() -> 1024.

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    tutil:run(unknown, fun() -> test() end).


test() ->
    {ok, LRU} = ets_lru:create(lru, [named_tables, {lifetime, lifetime()}]),
    % Figure out how to test this.
    ok = ets_lru:destroy(LRU).
