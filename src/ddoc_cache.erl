% Copyright 2012 Cloudant. All rights reserved.

-module(ddoc_cache).


-export([
    start/0,
    stop/0,
    
    open/2,
    evict/2
]).


-define(CACHE, ddoc_cache_lru).
-define(OPENER, ddoc_cache_opener).


start() ->
    application:start(ddoc_cache).


stop() ->
    application:stop(ddoc_cache).


open(DbName, validation_funs) ->
    open({DbName, validation_funs});
open(DbName, <<"_design/", _/binary>>=DDocId) when is_binary(DbName) ->
    open({DbName, DDocId});
open(DbName, DDocId) when is_binary(DDocId) ->
    open({DbName, <<"_design/", DDocId/binary>>}).


open(Key) ->
    case ets_lru:lookup_d(?CACHE, Key) of
        {ok, _} = Resp ->
            Resp;
        _ ->
            gen_server:call(?OPENER, {open, Key}, infinity)
    end.


evict(ShardDbName, DDocIds) ->
    DbName = mem3:dbname(ShardDbName),
    gen_server:cast(?OPENER, {evict, DbName, DDocIds}).

