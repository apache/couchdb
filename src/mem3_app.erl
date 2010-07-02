-module(mem3_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, []) ->
    mem3_sup:start_link().

stop([]) ->
    ok.
