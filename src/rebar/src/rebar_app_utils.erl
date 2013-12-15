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
-module(rebar_app_utils).

-export([is_app_dir/0, is_app_dir/1,
         is_app_src/1,
         app_src_to_app/1,
         app_name/2,
         app_applications/2,
         app_vsn/2,
         is_skipped_app/2]).

-export([load_app_file/2]). % TEMPORARY

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

is_app_dir() ->
    is_app_dir(rebar_utils:get_cwd()).

is_app_dir(Dir) ->
    SrcDir = filename:join([Dir, "src"]),
    AppSrc = filename:join([SrcDir, "*.app.src"]),
    case filelib:wildcard(AppSrc) of
        [AppSrcFile] ->
            {true, AppSrcFile};
        [] ->
            EbinDir = filename:join([Dir, "ebin"]),
            App = filename:join([EbinDir, "*.app"]),
            case filelib:wildcard(App) of
                [AppFile] ->
                    {true, AppFile};
                [] ->
                    false;
                _ ->
                    ?ERROR("More than one .app file in ~s~n", [EbinDir]),
                    false
            end;
        _ ->
            ?ERROR("More than one .app.src file in ~s~n", [SrcDir]),
            false
    end.


is_app_src(Filename) ->
    %% If removing the extension .app.src yields a shorter name,
    %% this is an .app.src file.
    Filename =/= filename:rootname(Filename, ".app.src").

app_src_to_app(Filename) ->
    filename:join("ebin", filename:basename(Filename, ".app.src") ++ ".app").

app_name(Config, AppFile) ->
    case load_app_file(Config, AppFile) of
        {ok, NewConfig, AppName, _} ->
            {NewConfig, AppName};
        {error, Reason} ->
            ?ABORT("Failed to extract name from ~s: ~p\n",
                   [AppFile, Reason])
    end.

app_applications(Config, AppFile) ->
    case load_app_file(Config, AppFile) of
        {ok, NewConfig, _, AppInfo} ->
            {NewConfig, get_value(applications, AppInfo, AppFile)};
        {error, Reason} ->
            ?ABORT("Failed to extract applications from ~s: ~p\n",
                   [AppFile, Reason])
    end.

app_vsn(Config, AppFile) ->
    case load_app_file(Config, AppFile) of
        {ok, Config1, _, AppInfo} ->
            AppDir = filename:dirname(filename:dirname(AppFile)),
            rebar_utils:vcs_vsn(Config1, get_value(vsn, AppInfo, AppFile),
                                AppDir);
        {error, Reason} ->
            ?ABORT("Failed to extract vsn from ~s: ~p\n",
                   [AppFile, Reason])
    end.

is_skipped_app(Config, AppFile) ->
    {Config1, ThisApp} = app_name(Config, AppFile),
    %% Check for apps global parameter; this is a comma-delimited list
    %% of apps on which we want to run commands
    Skipped =
        case get_apps(Config) of
            undefined ->
                %% No apps parameter specified, check the skip_apps list..
                case get_skip_apps(Config) of
                    undefined ->
                        %% No skip_apps list, run everything..
                        false;
                    SkipApps ->
                        TargetApps = [list_to_atom(A) ||
                                         A <- string:tokens(SkipApps, ",")],
                        is_skipped(ThisApp, TargetApps)
                end;
            Apps ->
                %% run only selected apps
                TargetApps = [list_to_atom(A) || A <- string:tokens(Apps, ",")],
                is_selected(ThisApp, TargetApps)
        end,
    {Config1, Skipped}.

%% ===================================================================
%% Internal functions
%% ===================================================================

load_app_file(Config, Filename) ->
    AppFile = {app_file, Filename},
    case rebar_config:get_xconf(Config, {appfile, AppFile}, undefined) of
        undefined ->
            case consult_app_file(Filename) of
                {ok, [{application, AppName, AppData}]} ->
                    Config1 = rebar_config:set_xconf(Config,
                                                     {appfile, AppFile},
                                                     {AppName, AppData}),
                    {ok, Config1, AppName, AppData};
                {error, _} = Error ->
                    Error;
                Other ->
                    {error, {unexpected_terms, Other}}
            end;
        {AppName, AppData} ->
            {ok, Config, AppName, AppData}
    end.

%% In the case of *.app.src we want to give the user the ability to
%% dynamically script the application resource file (think dynamic version
%% string, etc.), in a way similar to what can be done with the rebar
%% config. However, in the case of *.app, rebar should not manipulate
%% that file. This enforces that dichotomy between app and app.src.
consult_app_file(Filename) ->
    case lists:suffix(".app.src", Filename) of
        false ->
            file:consult(Filename);
        true ->
            %% TODO: EXPERIMENTAL For now let's warn the user if a
            %% script is going to be run.
            case filelib:is_regular([Filename, ".script"]) of
                true ->
                    ?CONSOLE("NOTICE: Using experimental *.app.src.script "
                             "functionality on ~s ~n", [Filename]);
                _ ->
                    ok
            end,
            rebar_config:consult_file(Filename)
    end.

get_value(Key, AppInfo, AppFile) ->
    case proplists:get_value(Key, AppInfo) of
        undefined ->
            ?ABORT("Failed to get app value '~p' from '~s'~n", [Key, AppFile]);
        Value ->
            Value
    end.

%% apps= for selecting apps
is_selected(ThisApp, TargetApps) ->
    case lists:member(ThisApp, TargetApps) of
        false ->
            {true, ThisApp};
        true ->
            false
    end.

%% skip_apps= for filtering apps
is_skipped(ThisApp, TargetApps) ->
    case lists:member(ThisApp, TargetApps) of
        false ->
            false;
        true ->
            {true, ThisApp}
    end.

get_apps(Config) ->
    rebar_config:get_global(Config, apps, undefined).

get_skip_apps(Config) ->
    rebar_config:get_global(Config, skip_apps, undefined).
