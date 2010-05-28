-module(membership).
-author('Brad Anderson <brad@cloudant.com>').

-export([start/0, stop/0, restart/0]).


start() ->
    application:start(membership).

stop() ->
    application:stop(membership).

restart() ->
    stop(),
    start().
