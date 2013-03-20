% Copyright 2012 Cloudant. All rights reserved.

-module(ddoc_cache_app).
-behaviour(application).


-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    ddoc_cache_sup:start_link().


stop(_State) ->
    ok.
