-module(mango_app).
-behavior(application).


-export([
    start/2,
    stop/1
]).


start(_Type, []) ->
    mango_sup:start_link().


stop([]) ->
    ok.

