#! /usr/bin/env escript

max_size() -> 1024.

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    tutil:run(1, fun() -> test() end).


test() ->
    {ok, LRU} = ets_lru:create(lru, [named_tables, {max_size, max_size()}]),
    etap:is(insert_kvs(LRU, 10000), ok, "Max size ok"),
    ok = ets_lru:destroy(LRU).


insert_kvs(LRU, 0) ->
    ok;
insert_kvs(LRU, Count) ->
    ets_lru:insert(LRU, Count, 1.5234),
    case ets:info(lru_objects, memory) > max_size() of
        true -> erlang:error(exceeded_max_size);
        false -> ok
    end,
    insert_kvs(LRU, Count-1).
