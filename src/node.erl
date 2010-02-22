%%%-------------------------------------------------------------------
%%% File:      node.erl
%%% @author    Cliff Moon <> []
%%% @copyright 2009 Cliff Moon
%%% @doc
%%%
%%% @end
%%%
%%% @since 2009-05-11 by Cliff Moon
%%%-------------------------------------------------------------------
-module(node).
-author('cliff@powerset.com').

%% API
-export([name/1, attributes/1]).

-include("../include/common.hrl").

%% -ifdef(TEST).
%% -include("../etest/node_test.erl").
%% -endif.

%%====================================================================
%% API
%%====================================================================

name(Name) when is_atom(Name) ->
  Name;
name(Node) when is_tuple(Node) ->
  element(1, Node);
name(Node) ->
  Node.

attributes(Name) when is_atom(Name) ->
  [];
attributes(Node) when is_tuple(Node) ->
  element(2, Node);
attributes(_) ->
  [].
