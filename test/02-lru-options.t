#! /usr/bin/env escript

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    tutil:run(unknown, fun() -> test() end).


test() ->
    test_named_tables(),
    test_max_objects(),
    test_max_size(),
    test_lifetime(),
    test_bad_option(),
    
    ok.


test_named_tables() ->
    {ok, LRU} = ets_lru:create(foo, [named_tables]),
    etap:is(ets:info(foo_objects, size), 0, "foo_objects table exists"),
    etap:is(ets:info(foo_atimes, size), 0, "foo_atimes table exists"),
    ok = ets_lru:destroy(LRU),
    etap:isnt(catch ets:info(foo_objects, size), 0, "foo_objects is gone"),
    etap:isnt(catch ets:info(foo_atimes, size), 0, "foo_atimes is gone"),
    ok.


test_max_objects() ->
    % See also: 03-limit-max-objects.t
    test_good([{max_objects, 5}]),
    test_good([{max_objects, 1}]),
    test_good([{max_objects, 923928342098203942}]).


test_max_size() ->
    % See also: 04-limit-max-size.t
    test_good([{max_size, 1}]),
    test_good([{max_size, 5}]),
    test_good([{max_size, 23423409090923423942309423094}]).


test_lifetime() ->
    % See also: 05-limit-lifetime.t
    test_good([{lifetime, 1}]),
    test_good([{lifetime, 5}]),
    test_good([{lifetime, 1244209909182409328409283409238}]).


test_bad_option() ->
    test_bad([{bingo, bango}]),
    test_bad([12]),
    test_bad([true]).
        

test_good(Options) ->
    etap:fun_is(fun
        ({ok, LRU}) -> ets_lru:destroy(LRU), true;
        (_) -> false
    end, ets_lru:create(?MODULE, Options), "LRU created ok with options").


test_bad(Options) ->
    etap:fun_is(fun
        ({invalid_option, _}) -> true;
        ({ok, LRU}) -> ets_lru:destroy(LRU), false;
        (_) -> false
    end, catch ets_lru:create(?MODULE, Options), "LRU error with options").