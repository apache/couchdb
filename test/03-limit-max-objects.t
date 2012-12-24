#! /usr/bin/env escript

objs() -> 25.

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    tutil:run(1, fun() -> test() end).


test() ->
    {ok, LRU} = ets_lru:create(lru, [named_tables, {max_objects, objs()}]),
    etap:is(insert_kvs(LRU, 100 * objs()), ok, "Max object count ok"),
    ok = ets_lru:destroy(LRU).


insert_kvs(LRU, 0) ->
    ok;
insert_kvs(LRU, Count) ->
    ets_lru:insert(LRU, Count, bar),
    case ets:info(lru_objects, size) > objs() of
        true -> erlang:error(exceeded_max_objects);
        false -> ok
    end,
    insert_kvs(LRU, Count-1).
