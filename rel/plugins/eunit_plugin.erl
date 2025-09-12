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

build_eunit_config(Config, AppFile) ->
    Cwd = filename:absname(rebar_utils:get_cwd()),
    App = rebar_config:get_global(Config, app, undefined),
    case is_list(App) of
        true -> ok;
        false -> error(app_parameter_must_be_defined)
    end,
    case re:run(App, "^[_a-z0-9]+$") of
        nomatch ->
            error({app_parameter_must_be_just_one_app, App});
        {match, _} ->
            Prefix = Cwd ++ "/tmp/" ++ App,
            DataDir = Prefix ++ "/data",
            ViewIndexDir = Prefix ++ "/data",
            StateDir = Prefix ++ "/data",
            TmpDataDir = Prefix ++ "/tmp_data",
            EtcDir = Prefix ++ "/etc",
            LogDir = Prefix,
            build_config(
                Config,
                AppFile,
                Prefix,
                DataDir,
                ViewIndexDir,
                StateDir,
                TmpDataDir,
                EtcDir,
                LogDir
            )
    end.

build_config(Config, AppFile, Prefix, DataDir, ViewIndexDir, StateDir, TmpDataDir, EtcDir, LogDir) ->
    cleanup_dirs([DataDir, TmpDataDir, EtcDir]),
    Config1 = rebar_config:set_global(Config, template, "setup_eunit"),
    Config2 = rebar_config:set_global(Config1, prefix, Prefix),
    Config3 = rebar_config:set_global(Config2, data_dir, DataDir),
    Config4 = rebar_config:set_global(Config3, view_index_dir, ViewIndexDir),
    Config5 = rebar_config:set_global(Config4, log_dir, LogDir),
    Config6 = rebar_config:set_global(Config5, etc_dir, EtcDir),
    Config7 = rebar_config:set_global(Config6, tmp_data, TmpDataDir),
    Config8 = rebar_config:set_global(Config7, state_dir, StateDir),
    rebar_templater:create(Config8, AppFile).

cleanup_dirs(Dirs) ->
    lists:foreach(
        fun(Dir) ->
            case filelib:is_dir(Dir) of
                true -> file:del_dir_r(Dir);
                false -> ok
            end
        end,
        Dirs
    ).
