% Copyright 2012 Cloudant. All rights reserved.

-module(ddoc_cache_sup).
-behaviour(supervisor).


-export([
    start_link/0,
    init/1
]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Children = [
        {
            ddoc_cache_lru,
            {ets_lru, start_link, [ddoc_cache_lru, lru_opts()]},
            permanent,
            5000,
            worker,
            [ets_lru]
        },
        {
            ddoc_cache_opener,
            {ddoc_cache_opener, start_link, []},
            permanent,
            5000,
            worker,
            [ddoc_cache_opener]
        }
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.


lru_opts() ->
    case application:get_env(ddoc_cache, max_objects) of
        {ok, MxObjs} when is_integer(MxObjs), MxObjs > 0 ->
            [{max_objects, MxObjs}];
        _ ->
            []
    end ++
    case application:get_env(ddoc_cache, max_size) of
        {ok, MxSize} when is_integer(MxSize), MxSize > 0 ->
            [{max_size, MxSize}];
        _ ->
            []
    end ++
    case application:get_env(ddoc_cache, max_lifetime) of
        {ok, MxLT} when is_integer(MxLT), MxLT > 0 ->
            [{max_lifetime, MxLT}];
        _ ->
            []
    end.
