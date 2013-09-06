%% Copyright 2013 Cloudant

-module(global_changes_app).
-behavior(application).


-export([
    start/2,
    stop/1
]).


start(_StartType, _StartArgs) ->
    global_changes_sup:start_link().


stop(_State) ->
    ok.
