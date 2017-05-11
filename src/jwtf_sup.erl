%%%-------------------------------------------------------------------
%% @doc epep top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(jwtf_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Children = [
        {jwks_cache_lru,
            {ets_lru, start_link, [jwks_cache_lru, lru_opts()]},
            permanent, 5000, worker, [ets_lru]}
    ],
    {ok, { {one_for_all, 0, 1}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================

lru_opts() ->
    case config:get_integer("jwtf_cache", "max_objects", 50) of
        MxObjs when MxObjs > 0 ->
            [{max_objects, MxObjs}];
        _ ->
            []
    end ++
    case config:get_integer("jwtf_cache", "max_size", 0) of
        MxSize when MxSize > 0 ->
            [{max_size, MxSize}];
        _ ->
            []
    end ++
    case config:get_integer("jwtf_cache", "max_lifetime", 0) of
        MxLT when MxLT > 0 ->
            [{max_lifetime, MxLT}];
        _ ->
            []
    end.
