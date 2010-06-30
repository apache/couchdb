-module(mem3).
-author('Brad Anderson <brad@cloudant.com>').

-export([start/0, stop/0, restart/0]).


start() ->
    application:start(mem3).

stop() ->
    application:stop(mem3).

restart() ->
    stop(),
    start().
