% Copyright 2012 Cloudant. All rights reserved.

-module(ddoc_cache).


-export([
    start/0,
    stop/0,
    
    open/2
]).


-define(CACHE, ddoc_cache_lru).
-define(OPENER, ddoc_cache_opener).


start() ->
    application:start(ddoc_cache).


stop() ->
    application:stop(ddoc_cache).


open(DbName, <<"_design/", _/binary>>=DDocId) when is_binary(DbName) ->
    case ets_lru:lookup_d(?CACHE, {DbName, DDocId}) of
        {ok, Doc} ->
            {ok, Doc};
        _ ->
            gen_server:call(?OPENER, {open, {DbName, DDocId}}, infinity)
    end;
open(DbName, DDocId) when is_binary(DDocId) ->
    open(DbName, <<"_design/", DDocId/binary>>).
