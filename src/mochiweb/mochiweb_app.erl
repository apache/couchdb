%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Callbacks for the mochiweb application.

-module(mochiweb_app).
-author('bob@mochimedia.com').

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mochiweb.
start(_Type, _StartArgs) ->
    mochiweb_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mochiweb.
stop(_State) ->
    ok.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
