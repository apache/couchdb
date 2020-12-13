% Copyright 2013 Cloudant. All rights reserved.

-module(custodian_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    custodian_sup:start_link().

stop(_State) ->
    ok.
