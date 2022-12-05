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
  Dir = code:priv_dir(couch),
  couch_log:info("Priv-Dir: ~p", [filename:join(Dir, ?MODULE)]),
  ok = erlang:load_nif(filename:join(Dir, ?MODULE), 0).

get_spidermonkey_version() ->
  exit(nif_library_not_loaded).