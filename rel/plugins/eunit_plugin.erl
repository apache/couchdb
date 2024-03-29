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

-module(eunit_plugin).

-export([setup_eunit/2]).

setup_eunit(Config, AppFile) ->
    case is_base_dir(Config) of
        false -> ok;
        true -> build_eunit_config(Config, AppFile)
    end.

%% from https://github.com/ChicagoBoss/ChicagoBoss/blob/master/skel/priv/rebar/boss_plugin.erl
is_base_dir(RebarConf) ->
    filename:absname(rebar_utils:get_cwd()) =:=
        rebar_config:get_xconf(RebarConf, base_dir, undefined).

build_eunit_config(Config0, AppFile) ->
    Cwd = filename:absname(rebar_utils:get_cwd()),
    DataDir = Cwd ++ "/tmp/data",
    ViewIndexDir = Cwd ++ "/tmp/data",
    StateDir = Cwd ++ "/tmp/data",
    TmpDataDir = Cwd ++ "/tmp/tmp_data",
    LogDir = Cwd ++ "/tmp",
    cleanup_dirs([DataDir, TmpDataDir]),
    Config1 = rebar_config:set_global(Config0, template, "setup_eunit"),
    Config2 = rebar_config:set_global(Config1, prefix, Cwd),
    Config3 = rebar_config:set_global(Config2, data_dir, DataDir),
    Config4 = rebar_config:set_global(Config3, view_index_dir, ViewIndexDir),
    Config5 = rebar_config:set_global(Config4, log_dir, LogDir),
    Config = rebar_config:set_global(Config5, state_dir, StateDir),
    rebar_templater:create(Config, AppFile).

cleanup_dirs(Dirs) ->
    lists:foreach(
        fun(Dir) ->
            case filelib:is_dir(Dir) of
                true -> del_dir(Dir);
                false -> ok
            end
        end,
        Dirs
    ).

del_dir(Dir) ->
    All = filelib:wildcard(Dir ++ "/**"),
    {Dirs, Files} = lists:partition(fun filelib:is_dir/1, All),
    ok = lists:foreach(fun file:delete/1, Files),
    SortedDirs = lists:sort(fun(A, B) -> length(A) > length(B) end, Dirs),
    ok = lists:foreach(fun file:del_dir/1, SortedDirs),
    ok = file:del_dir(Dir).
