-module(couch_config_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    couch_config_sup:start_link(get_ini_files()).

stop(_State) ->
    ok.

get_ini_files() ->
    Etc = filename:join(code:root_dir(), "etc"),
    Default = [filename:join(Etc,"default.ini"), filename:join(Etc,"local.ini")],
    case init:get_argument(couch_ini) of
    error ->
        Default;
    {ok, [[]]} ->
        Default;
    {ok, [Values]} ->
        Values
    end.
