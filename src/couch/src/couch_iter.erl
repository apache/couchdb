-module(couch_iter).
-export([start/1, next/1, counter/2]).

start(Max) ->
    couch_log:info("~nstarting iter with Max: ~p~n", [Max]),
    P = spawn_link(couch_iter, counter, [0, Max]),
    couch_log:info("~ndd_iter_start: ~p~n", [P]),
    P.

counter(N, N) ->
    couch_log:info("~ndd_counterNN: ~p~n", [N]),
    receive {From, next} ->
        From ! {self(), done}
    end;
counter(N, Max) ->
    couch_log:info("~ndd_counterN: ~p Max: ~p ~n", [N, Max]),
    receive {From, next} ->
        From ! {self(), N + 1},
        counter(N + 1, Max)
    end.

next(Iter) ->
    couch_log:info("~nnext~n", []),
    Iter ! {self(), next},
    receive
        {Iter, Answer} -> Answer
    end.
