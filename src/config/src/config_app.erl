% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(config_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    config_sup:start_link(get_ini_files()).

stop(_State) ->
    ok.

get_ini_files() ->
    IniFiles = hd([L || L <- [command_line(), env(), default()], L =/= skip]),
    lists:flatmap(fun expand_dirs/1, IniFiles).

env() ->
    case application:get_env(config, ini_files) of
        undefined ->
            skip;
        {ok, IniFiles} ->
            IniFiles
    end.

command_line() ->
    case init:get_argument(couch_ini) of
        error ->
            skip;
        {ok, [IniFiles]} ->
            IniFiles
    end.

default() ->
    Etc = filename:join(code:root_dir(), "etc"),
    Default = [
        filename:join(Etc, "default.ini"),
        filename:join(Etc, "default.d"),
        filename:join(Etc, "local.ini"),
        filename:join(Etc, "local.d")
    ],
    lists:filter(fun filelib:is_file/1, Default).

expand_dirs(File) ->
    case filelib:is_dir(File) of
        true ->
            lists:sort(filelib:wildcard(File ++ "/*.ini"));
        false ->
            [File]
    end.
