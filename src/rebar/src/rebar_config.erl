%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_config).

-export([new/0, new/1, base_config/1, consult_file/1,
         get/3, get_local/3, get_list/3,
         get_all/2,
         set/3,
         set_global/3, get_global/3,
         is_verbose/1,
         save_env/3, get_env/2, reset_envs/1,
         set_skip_dir/2, is_skip_dir/2, reset_skip_dirs/1,
         clean_config/2,
         set_xconf/3, get_xconf/2, get_xconf/3, erase_xconf/2]).

-include("rebar.hrl").

-record(config, { dir :: file:filename(),
                  opts = [] :: list(),
                  globals = new_globals() :: dict(),
                  envs = new_env() :: dict(),
                  %% cross-directory/-command config
                  skip_dirs = new_skip_dirs() :: dict(),
                  xconf = new_xconf() :: dict() }).

-export_type([config/0]).

-opaque config() :: #config{}.

-define(DEFAULT_NAME, "rebar.config").

%% ===================================================================
%% Public API
%% ===================================================================

base_config(GlobalConfig) ->
    ConfName = rebar_config:get_global(GlobalConfig, config, ?DEFAULT_NAME),
    new(GlobalConfig, ConfName).

new() ->
    #config{dir = rebar_utils:get_cwd()}.

new(ConfigFile) when is_list(ConfigFile) ->
    case consult_file(ConfigFile) of
        {ok, Opts} ->
            #config { dir = rebar_utils:get_cwd(),
                      opts = Opts };
        Other ->
            ?ABORT("Failed to load ~s: ~p~n", [ConfigFile, Other])
    end;
new(_ParentConfig=#config{opts=Opts0, globals=Globals, skip_dirs=SkipDirs,
                          xconf=Xconf}) ->
    new(#config{opts=Opts0, globals=Globals, skip_dirs=SkipDirs, xconf=Xconf},
        ?DEFAULT_NAME).

get(Config, Key, Default) ->
    proplists:get_value(Key, Config#config.opts, Default).

get_list(Config, Key, Default) ->
    get(Config, Key, Default).

get_local(Config, Key, Default) ->
    proplists:get_value(Key, local_opts(Config#config.opts, []), Default).

get_all(Config, Key) ->
    proplists:get_all_values(Key, Config#config.opts).

set(Config, Key, Value) ->
    Opts = proplists:delete(Key, Config#config.opts),
    Config#config { opts = [{Key, Value} | Opts] }.

set_global(Config, jobs=Key, Value) when is_list(Value) ->
    set_global(Config, Key, list_to_integer(Value));
set_global(Config, jobs=Key, Value) when is_integer(Value) ->
    NewGlobals = dict:store(Key, erlang:max(1, Value), Config#config.globals),
    Config#config{globals = NewGlobals};
set_global(Config, Key, Value) ->
    NewGlobals = dict:store(Key, Value, Config#config.globals),
    Config#config{globals = NewGlobals}.

get_global(Config, Key, Default) ->
    case dict:find(Key, Config#config.globals) of
        error ->
            Default;
        {ok, Value} ->
            Value
    end.

is_verbose(Config) ->
    DefaulLevel = rebar_log:default_level(),
    get_global(Config, verbose, DefaulLevel) > DefaulLevel.

consult_file(File) ->
    case filename:extension(File) of
        ".script" ->
            consult_and_eval(remove_script_ext(File), File);
        _ ->
            Script = File ++ ".script",
            case filelib:is_regular(Script) of
                true ->
                    consult_and_eval(File, Script);
                false ->
                    ?DEBUG("Consult config file ~p~n", [File]),
                    file:consult(File)
            end
    end.

save_env(Config, Mod, Env) ->
    NewEnvs = dict:store(Mod, Env, Config#config.envs),
    Config#config{envs = NewEnvs}.

get_env(Config, Mod) ->
    dict:fetch(Mod, Config#config.envs).

reset_envs(Config) ->
    Config#config{envs = new_env()}.

set_skip_dir(Config, Dir) ->
    OldSkipDirs = Config#config.skip_dirs,
    NewSkipDirs = case is_skip_dir(Config, Dir) of
                      false ->
                          ?DEBUG("Adding skip dir: ~s\n", [Dir]),
                          dict:store(Dir, true, OldSkipDirs);
                      true ->
                          OldSkipDirs
                  end,
    Config#config{skip_dirs = NewSkipDirs}.

is_skip_dir(Config, Dir) ->
    dict:is_key(Dir, Config#config.skip_dirs).

reset_skip_dirs(Config) ->
    Config#config{skip_dirs = new_skip_dirs()}.

set_xconf(Config, Key, Value) ->
    NewXconf = dict:store(Key, Value, Config#config.xconf),
    Config#config{xconf=NewXconf}.

get_xconf(Config, Key) ->
    {ok, Value} = dict:find(Key, Config#config.xconf),
    Value.

get_xconf(Config, Key, Default) ->
    case dict:find(Key, Config#config.xconf) of
        error ->
            Default;
        {ok, Value} ->
            Value
    end.

erase_xconf(Config, Key) ->
    NewXconf = dict:erase(Key, Config#config.xconf),
    Config#config{xconf = NewXconf}.

%% TODO: reconsider after config inheritance removal/redesign
clean_config(Old, New) ->
    New#config{opts=Old#config.opts}.

%% ===================================================================
%% Internal functions
%% ===================================================================

new(ParentConfig, ConfName) ->
    %% Load terms from rebar.config, if it exists
    Dir = rebar_utils:get_cwd(),
    ConfigFile = filename:join([Dir, ConfName]),
    Opts0 = ParentConfig#config.opts,
    Opts = case consult_file(ConfigFile) of
               {ok, Terms} ->
                   %% Found a config file with some terms. We need to
                   %% be able to distinguish between local definitions
                   %% (i.e. from the file in the cwd) and inherited
                   %% definitions. To accomplish this, we use a marker
                   %% in the proplist (since order matters) between
                   %% the new and old defs.
                   Terms ++ [local] ++
                       [Opt || Opt <- Opts0, Opt /= local];
               {error, enoent} ->
                   [local] ++
                       [Opt || Opt <- Opts0, Opt /= local];
               Other ->
                   ?ABORT("Failed to load ~s: ~p\n", [ConfigFile, Other])
           end,

    ParentConfig#config{dir = Dir, opts = Opts}.

consult_and_eval(File, Script) ->
    ?DEBUG("Evaluating config script ~p~n", [Script]),
    ConfigData = try_consult(File),
    file:script(Script, bs([{'CONFIG', ConfigData}, {'SCRIPT', Script}])).

remove_script_ext(F) ->
    "tpircs." ++ Rev = lists:reverse(F),
    lists:reverse(Rev).

try_consult(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            ?DEBUG("Consult config file ~p~n", [File]),
            Terms;
        {error, enoent} ->
            [];
        {error, Reason} ->
            ?ABORT("Failed to read config file ~s: ~p~n", [File, Reason])
    end.

bs(Vars) ->
    lists:foldl(fun({K,V}, Bs) ->
                        erl_eval:add_binding(K, V, Bs)
                end, erl_eval:new_bindings(), Vars).

local_opts([], Acc) ->
    lists:reverse(Acc);
local_opts([local | _Rest], Acc) ->
    lists:reverse(Acc);
local_opts([Item | Rest], Acc) ->
    local_opts(Rest, [Item | Acc]).

new_globals() -> dict:new().

new_env() -> dict:new().

new_skip_dirs() -> dict:new().

new_xconf() -> dict:new().
