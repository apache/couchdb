% Copyright 2012 Cloudant. All rights reserved.

-module(ddoc_cache).


-export([
    start/0,
    stop/0
]).


start() ->
    application:start(ddoc_cache).


stop() ->
    application:stop(ddoc_cache).
