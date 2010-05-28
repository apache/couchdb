-module(membership_app).
-author('brad@cloudant.com').

-behaviour(application).

-include("membership.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% @doc start required apps, join cluster, start supervisor
start(_Type, _StartArgs) ->
    % start dynomite supervisor
    membership_sup:start_link().

stop({_, Sup}) ->
    ?LOG_ALERT("dynomite application stopped", []),
    exit(Sup, normal),
    ok.
