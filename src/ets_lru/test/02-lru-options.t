#! /usr/bin/env escript

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    tutil:run(9, fun() -> test() end).


test() ->
    test_max_objects(),
    test_max_size(),
    test_lifetime(),
    test_bad_option(),

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
    test_good([{max_size, 2342923423942309423094}]).


test_lifetime() ->
    % See also: 05-limit-lifetime.t
    test_good([{max_lifetime, 1}]),
    test_good([{max_lifetime, 5}]),
    test_good([{max_lifetime, 1244209909180928348}]).


test_bad_option() ->
    % Figure out a test for these.
    %test_bad([{bingo, bango}]),
    %test_bad([12]),
    %test_bad([true]).
    ok.


test_good(Options) ->
    Msg = io_lib:format("LRU created ok with options: ~w", [Options]),
    etap:fun_is(fun
        ({ok, LRU}) when is_pid(LRU) -> ets_lru:stop(LRU), true;
        (_) -> false
    end, ets_lru:start_link(?MODULE, Options), lists:flatten(Msg)).


% test_bad(Options) ->
%     etap:fun_is(fun
%         ({invalid_option, _}) -> true;
%         ({ok, LRU}) -> ets_lru:stop(LRU), false;
%         (_) -> false
%     end, catch ets_lru:start_link(?MODULE, Options), "LRU bad options").
