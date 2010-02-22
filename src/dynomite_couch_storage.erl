%%%-------------------------------------------------------------------
%%% File:      dynomite_couch_storage.erl
%%% @author    Brad Anderson
%%% @copyright 2009 Brad Anderson
%%% @doc
%%%
%%% @end
%%%
%%% @since 2009-07-14
%%%-------------------------------------------------------------------
-module(dynomite_couch_storage).
-author('brad@cloudant.com').

%% API
-export([name/1, open/2, close/1, create/2]).
%% , close/1, get/2, put/4, has_key/2, delete/2, fold/3

-include_lib("../include/common.hrl").

%% -record(row, {key, context, values}).

%%====================================================================
%% API
%%====================================================================

name(Boundary) ->
    showroom_utils:int_to_hexstr(Boundary).

open(Directory, Name) ->
%%     ?debugFmt("~nDirectory: ~p~nName     : ~p~n", [Directory,Name]),
    {ok, {Directory, Name}}.

close(_Table) -> ok.

create(_Directory, _Name) ->
    ok.


%%====================================================================
%% Internal functions
%%====================================================================
