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
    try ets_lru:lookup_d(?CACHE, Key) of
        {ok, _} = Resp ->
            margaret_counter:increment([ddoc_cache, hit]),
            Resp;
        _ ->
            margaret_counter:increment([ddoc_cache, miss]),
            case gen_server:call(?OPENER, {open, Key}, infinity) of
                {open_ok, Resp} ->
                    Resp;
                {open_error, throw, Error} ->
                    throw(Error);
                {open_error, error, Error} ->
                    erlang:error(Error);
                {open_error, exit, Error} ->
                    exit(Error)
            end
    catch
        error:badarg ->
            margaret_counter:increment([ddoc_cache, recovery]),
            recover(Key)
    end.


evict(ShardDbName, DDocIds) ->
    DbName = mem3:dbname(ShardDbName),
    gen_server:cast(?OPENER, {evict, DbName, DDocIds}).


recover({DbName, validation_funs}) ->
    {ok, DDocs} = fabric:design_docs(mem3:dbname(DbName)),
    Funs = lists:flatmap(fun(DDoc) ->
        case couch_doc:get_validate_doc_fun(DDoc) of
            nil -> [];
            Fun -> [Fun]
        end
    end, DDocs),
    {ok, Funs};
recover({DbName, DDocId}) ->
    fabric:open_doc(DbName, DDocId, []).
