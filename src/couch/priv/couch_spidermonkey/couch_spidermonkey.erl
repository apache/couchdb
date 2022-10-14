%%%-------------------------------------------------------------------
%%% @author big-r
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Okt 2022 19:40
%%%-------------------------------------------------------------------
-module(couch_spidermonkey).

-export([get_spidermonkey_version/0]).
-nifs([get_spidermonkey_version/0]).
-on_load(init/0).

init() ->
  ok = erlang:load_nif("./couch_spidermonkey", 0).

get_spidermonkey_version() ->
  exit(nif_library_not_loaded).