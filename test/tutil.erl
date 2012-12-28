-module(tutil).

-export([
    run/2,
    with_lru/1
]).


run(Plan, Fun) ->
    etap:plan(Plan),
    case (catch Fun()) of
        ok ->
            etap:end_tests();
        Error ->
            Msg = lists:flatten(io_lib:format("Error: ~p", [Error])),
            etap:bail(Msg)
    end.


with_lru(Fun) ->
    {ok, LRU} = ets_lru:start_link(test_lru, []),
    Ref = erlang:monitor(process, LRU),
    try
        Fun(LRU)
    after
        ets_lru:stop(LRU),
        receive {'DOWN', Ref, process, LRU, _} -> ok end
    end.

