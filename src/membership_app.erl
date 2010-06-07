-module(membership_app).
-author('brad@cloudant.com').

-behaviour(application).

-include("membership.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% @doc start required apps, join cluster, start supervisor
start(_Type, _StartArgs) ->
    couch_api:create_db(<<"dbs">>, []), % all nodes have local 'dbs' db
    % start membership supervisor
    membership_sup:start_link().

stop({_, Sup}) ->
    ?LOG_ALERT("membership application stopped", []),
    exit(Sup, normal),
    ok.
