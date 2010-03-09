%%% -*-  erlang-indent-level:2  -*-
%%%-------------------------------------------------------------------
%%% File:      configuration.erl
%%% @author    Cliff Moon <cliff@powerset.com>
%%% @author    Brad Anderson <brad@cloudant.com>
%%% @copyright 2008 Cliff Moon
%%% @doc
%%%      This module keeps Dynomite source relatively unchanged, but
%%%      reads from couchdb config stuffs
%%% @end
%%%
%%% @since 2008-07-18 by Cliff Moon
%%%-------------------------------------------------------------------
-module(configuration).
-author('cliff@powerset.com').
-author('brad@cloudant.com').

%%-behaviour(gen_server).

%% API
-export([start_link/1, get_config/1, get_config/0, set_config/1, stop/0]).

-include_lib("eunit/include/eunit.hrl").

-include("../include/config.hrl").
-include("../include/common.hrl").

-define(SERVER, couch_config).
-define(i2l(V), integer_to_list(V)).
-define(l2i(V), list_to_integer(V)).


%% -----------------------------------------------------------------
%%  API
%% -----------------------------------------------------------------

%% @doc starts couch_config gen_server if it's not already started
start_link(DynomiteConfig) ->
  couch_config_event:start_link(),
  couch_config:start_link([]),
  set_config(DynomiteConfig).


%% @doc get the config for a remote node
get_config(Node) ->
  ClusterConfig = rpc:call(Node, couch_config, get, ["cluster"]),
  Directory = rpc:call(Node, couch_config, get, ["couchdb", "database_dir"]),
  couch2dynomite_config(ClusterConfig, Directory).


%% @doc get the config for the local node
get_config() ->
  get_config(node()).


%% @doc given a Dynomite config record, put the values into the Couch config
set_config(DynomiteConfig) ->
  dynomite2couch_config(DynomiteConfig).


%% @doc stop the config server (nothing to do until after couch_config refactor)
stop() ->
  couch_config:stop().


%% -----------------------------------------------------------------
%%  Internal functions
%% -----------------------------------------------------------------

%% @doc turn a couch config proplist into a dynomite configuration record
couch2dynomite_config(ClusterConfig, Directory) ->
  Q = ?l2i(proplists:get_value("q", ClusterConfig, "3")),
  R = ?l2i(proplists:get_value("r", ClusterConfig, "2")),
  W = ?l2i(proplists:get_value("w", ClusterConfig, "1")),
  N = ?l2i(proplists:get_value("n", ClusterConfig, "4")),
  %% use couch's database_dir here, to avoid /tmp/data not existing
  Webport = ?l2i(proplists:get_value("webport", ClusterConfig, "8080")),
  Meta = proplists:get_value("meta", ClusterConfig, []),
  StorageMod = proplists:get_value("storage_mod", ClusterConfig, []),
  #config{q=Q, r=R, w=W, n=N, directory=Directory, web_port=Webport,
          meta=Meta, storage_mod=StorageMod}.


%% @doc workhorse for set_config/1 above
dynomite2couch_config(DynomiteConfig) ->
  couch_config:set("cluster", "q", ?i2l(DynomiteConfig#config.q), false),
  couch_config:set("cluster", "r", ?i2l(DynomiteConfig#config.r), false),
  couch_config:set("cluster", "w", ?i2l(DynomiteConfig#config.w), false),
  couch_config:set("cluster", "n", ?i2l(DynomiteConfig#config.n), false),
  couch_config:set("couchdb", "database_dir", DynomiteConfig#config.directory,
                   false),
  couch_config:set("cluster", "webport",
                   case DynomiteConfig#config.web_port of
                     undefined -> "8080";
                     _ -> ?i2l(DynomiteConfig#config.web_port)
                   end, false),
  couch_config:set("cluster", "meta", DynomiteConfig#config.meta, false),
  couch_config:set("cluster", "storage_mod",
                   DynomiteConfig#config.storage_mod, false),
  ok.
