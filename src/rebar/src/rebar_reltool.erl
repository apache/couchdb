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
-module(rebar_reltool).

-export([generate/2,
         overlay/2,
         clean/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").
-include_lib("kernel/include/file.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

generate(Config0, ReltoolFile) ->
    %% Make sure we have decent version of reltool available
    check_vsn(),

    %% Load the reltool configuration from the file
    {Config, ReltoolConfig} = rebar_rel_utils:load_config(Config0, ReltoolFile),

    Sys = rebar_rel_utils:get_sys_tuple(ReltoolConfig),

    %% Spin up reltool server and load our config into it
    {ok, Server} = reltool:start_server([Sys]),

    %% Do some validation of the reltool configuration; error messages out of
    %% reltool are still pretty cryptic
    validate_rel_apps(Server, Sys),

    %% Finally, run reltool
    case catch(run_reltool(Server, Config, ReltoolConfig)) of
        ok ->
            {ok, Config};
        {error, failed} ->
            ?FAIL;
        Other2 ->
            ?ERROR("Unexpected error: ~p\n", [Other2]),
            ?FAIL
    end.

overlay(Config, ReltoolFile) ->
    %% Load the reltool configuration from the file
    {Config1, ReltoolConfig} = rebar_rel_utils:load_config(Config, ReltoolFile),
    {process_overlay(Config, ReltoolConfig), Config1}.

clean(Config, ReltoolFile) ->
    {Config1, ReltoolConfig} = rebar_rel_utils:load_config(Config, ReltoolFile),
    TargetDir = rebar_rel_utils:get_target_dir(Config, ReltoolConfig),
    rebar_file_utils:rm_rf(TargetDir),
    rebar_file_utils:delete_each(["reltool.spec"]),
    {ok, Config1}.

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, generate) ->
    info_help("Build release with reltool");
info(help, clean) ->
    info_help("Delete release");
info(help, overlay) ->
    info_help("Run reltool overlays only").

info_help(Description) ->
    ?CONSOLE(
       "~s.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  ~n"
       "Valid reltool.config options:~n"
       "  {sys, []}~n"
       "  {target_dir, \"target\"}~n"
       "  {overlay_vars, \"overlay\"}~n"
       "  {overlay, []}~n"
       "Valid command line options:~n"
       "  target_dir=target~n"
       "  overlay_vars=VarsFile~n"
       "  dump_spec=1 (write reltool target spec to reltool.spec)~n",
       [
        Description
       ]).

check_vsn() ->
    %% TODO: use application:load and application:get_key once we require
    %%       R14A or newer. There's no reltool.app before R14A.
    case code:lib_dir(reltool) of
        {error, bad_name} ->
            ?ABORT("Reltool support requires the reltool application "
                   "to be installed!", []);
        Path ->
            ReltoolVsn = filename:basename(Path),
            case ReltoolVsn < "reltool-0.5.2" of
                true ->
                    ?ABORT("Reltool support requires at least reltool-0.5.2; "
                           "this VM is using ~s\n", [ReltoolVsn]);
                false ->
                    ok
            end
    end.

process_overlay(Config, ReltoolConfig) ->
    TargetDir = rebar_rel_utils:get_target_dir(Config, ReltoolConfig),

    {_BootRelName, BootRelVsn} =
        rebar_rel_utils:get_reltool_release_info(ReltoolConfig),

    %% Initialize overlay vars with some basics
    %% (that can get overwritten)
    OverlayVars0 =
        dict:from_list([{erts_vsn, "erts-" ++ erlang:system_info(version)},
                        {rel_vsn, BootRelVsn},
                        {target_dir, TargetDir},
                        {hostname, net_adm:localhost()}]),

    %% Load up any variables specified by overlay_vars
    OverlayVars1 = overlay_vars(Config, OverlayVars0, ReltoolConfig),
    OverlayVars = rebar_templater:resolve_variables(dict:to_list(OverlayVars1),
                                                    OverlayVars1),

    %% Finally, overlay the files specified by the overlay section
    case lists:keyfind(overlay, 1, ReltoolConfig) of
        {overlay, Overlay} when is_list(Overlay) ->
            execute_overlay(Overlay, OverlayVars, rebar_utils:get_cwd(),
                            TargetDir);
        false ->
            ?INFO("No {overlay, [...]} found in reltool.config.\n", []);
        _ ->
            ?ABORT("{overlay, [...]} entry in reltool.config "
                   "must be a list.\n", [])
    end.

%%
%% Look for overlay_vars file reference. If the user provides an overlay_vars on
%% the command line (i.e. a global), the terms from that file OVERRIDE the one
%% listed in reltool.config. To re-iterate, this means you can specify a
%% variable in the file from reltool.config and then override that value by
%% providing an additional file on the command-line.
%%
overlay_vars(Config, Vars0, ReltoolConfig) ->
    BaseVars = load_vars_file([proplists:get_value(overlay_vars, ReltoolConfig)]),
    OverlayVars = rebar_config:get_global(Config, overlay_vars, []),
    OverrideVars = load_vars_file(string:tokens(OverlayVars, ",")),
    M = fun merge_overlay_var/3, 
    dict:merge(M, dict:merge(M, Vars0, BaseVars), OverrideVars).

merge_overlay_var(_Key, _Base, Override) -> Override.

%%
%% If a filename is provided, construct a dict of terms
%%
load_vars_file([undefined]) ->
    dict:new();
load_vars_file([]) ->
    dict:new();
load_vars_file(Files) ->
    load_vars_file(Files, dict:new()).

load_vars_file([], Dict) ->
    Dict;
load_vars_file([File | Files], BaseVars) ->
    case rebar_config:consult_file(File) of
        {ok, Terms} ->
            OverrideVars = dict:from_list(Terms),
            M = fun merge_overlay_var/3,
            load_vars_file(Files, dict:merge(M, BaseVars, OverrideVars));
        {error, Reason} ->
            ?ABORT("Unable to load overlay_vars from ~p: ~p\n", [File, Reason])
    end.

validate_rel_apps(ReltoolServer, {sys, ReltoolConfig}) ->
    case lists:keyfind(rel, 1, ReltoolConfig) of
        false ->
            ok;
        {rel, _Name, _Vsn, Apps} ->
            %% Identify all the apps that do NOT exist, based on
            %% what's available from the reltool server
            Missing = lists:sort(
                        [App || App <- Apps,
                                app_exists(App, ReltoolServer) == false]),
            case Missing of
                [] ->
                    ok;
                _ ->
                    ?ABORT("Apps in {rel, ...} section not found by "
                           "reltool: ~p\n", [Missing])
            end;
        Rel ->
            %% Invalid release format!
            ?ABORT("Invalid {rel, ...} section in reltools.config: ~p\n", [Rel])
    end.

app_exists(App, Server) when is_atom(App) ->
    case reltool_server:get_app(Server, App) of
        {ok, _} ->
            true;
        _ ->
            false
    end;
app_exists(AppTuple, Server) when is_tuple(AppTuple) ->
    app_exists(element(1, AppTuple), Server).

run_reltool(Server, Config, ReltoolConfig) ->
    case reltool:get_target_spec(Server) of
        {ok, Spec} ->
            %% Pull the target dir and make sure it exists
            TargetDir = rebar_rel_utils:get_target_dir(Config, ReltoolConfig),
            mk_target_dir(Config, TargetDir),

            %% Determine the otp root dir to use
            RootDir = rebar_rel_utils:get_root_dir(Config, ReltoolConfig),

            %% Dump the spec, if necessary
            dump_spec(Config, Spec),

            %% Have reltool actually run
            case reltool:eval_target_spec(Spec, RootDir, TargetDir) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?ABORT("Failed to generate target from spec: ~p\n",
                           [Reason])
            end,

            {BootRelName, BootRelVsn} =
                rebar_rel_utils:get_reltool_release_info(ReltoolConfig),

            ok = create_RELEASES(TargetDir, BootRelName, BootRelVsn),

            process_overlay(Config, ReltoolConfig);

        {error, Reason} ->
            ?ABORT("Unable to generate spec: ~s\n", [Reason])
    end.

mk_target_dir(Config, TargetDir) ->
    case filelib:ensure_dir(filename:join(TargetDir, "dummy")) of
        ok ->
            ok;
        {error, eexist} ->
            %% Output directory already exists; if force=1, wipe it out
            case rebar_config:get_global(Config, force, "0") of
                "1" ->
                    rebar_file_utils:rm_rf(TargetDir),
                    ok = file:make_dir(TargetDir);
                _ ->
                    ?ERROR("Release target directory ~p already exists!\n",
                           [TargetDir]),
                    ?FAIL
            end;
        {error, Reason} ->
            ?ERROR("Failed to make target dir ~p: ~s\n",
                   [TargetDir, file:format_error(Reason)]),
            ?FAIL
    end.

dump_spec(Config, Spec) ->
    case rebar_config:get_global(Config, dump_spec, "0") of
        "1" ->
            SpecBin = list_to_binary(io_lib:print(Spec, 1, 120, -1)),
            ok = file:write_file("reltool.spec", SpecBin);
        _ ->
            ok
    end.


%% TODO: Merge functionality here with rebar_templater

execute_overlay([], _Vars, _BaseDir, _TargetDir) ->
    ok;
execute_overlay([{mkdir, Out} | Rest], Vars, BaseDir, TargetDir) ->
    OutFile = rebar_templater:render(
                filename:join([TargetDir, Out, "dummy"]), Vars),
    ok = filelib:ensure_dir(OutFile),
    ?DEBUG("Created dir ~s\n", [filename:dirname(OutFile)]),
    execute_overlay(Rest, Vars, BaseDir, TargetDir);
execute_overlay([{copy, In} | Rest], _Vars, BaseDir, TargetDir) ->
    execute_overlay([{copy, In, ""} | Rest], _Vars, BaseDir, TargetDir);
execute_overlay([{copy, In, Out} | Rest], Vars, BaseDir, TargetDir) ->
    InFile = rebar_templater:render(filename:join(BaseDir, In), Vars),
    OutFile = rebar_templater:render(filename:join(TargetDir, Out), Vars),
    case filelib:is_dir(InFile) of
        true ->
            ok;
        false ->
            ok = filelib:ensure_dir(OutFile)
    end,
    rebar_file_utils:cp_r([InFile], OutFile),
    execute_overlay(Rest, Vars, BaseDir, TargetDir);
execute_overlay([{template_wildcard, Wildcard, OutDir} | Rest], Vars,
                BaseDir, TargetDir) ->
    %% Generate a series of {template, In, Out} instructions from the wildcard
    %% that will get processed per normal
    Ifun = fun(F, Acc0) ->
                   [{template, F,
                     filename:join(OutDir, filename:basename(F))} | Acc0]
           end,
    NewInstrs = lists:foldl(Ifun, Rest, filelib:wildcard(Wildcard, BaseDir)),
    case length(NewInstrs) =:= length(Rest) of
        true ->
            ?WARN("template_wildcard: ~s did not match any files!\n",
                  [Wildcard]);
        false ->
            ok
    end,
    ?DEBUG("template_wildcard: ~s expanded to ~p\n", [Wildcard, NewInstrs]),
    execute_overlay(NewInstrs, Vars, BaseDir, TargetDir);
execute_overlay([{template, In, Out} | Rest], Vars, BaseDir, TargetDir) ->
    InFile = rebar_templater:render(filename:join(BaseDir, In), Vars),
    {ok, InFileData} = file:read_file(InFile),
    OutFile = rebar_templater:render(filename:join(TargetDir, Out), Vars),
    ok = filelib:ensure_dir(OutFile),
    case file:write_file(OutFile, rebar_templater:render(InFileData, Vars)) of
        ok ->
            ok = apply_file_info(InFile, OutFile),
            ?DEBUG("Templated ~p\n", [OutFile]),
            execute_overlay(Rest, Vars, BaseDir, TargetDir);
        {error, Reason} ->
            ?ABORT("Failed to template ~p: ~p\n", [OutFile, Reason])
    end;
execute_overlay([{create, Out, Contents} | Rest], Vars, BaseDir, TargetDir) ->
    OutFile = rebar_templater:render(filename:join(TargetDir, Out), Vars),
    ok = filelib:ensure_dir(OutFile),
    case file:write_file(OutFile, Contents) of
        ok ->
            ?DEBUG("Created ~p\n", [OutFile]),
            execute_overlay(Rest, Vars, BaseDir, TargetDir);
        {error, Reason} ->
            ?ABORT("Failed to create ~p: ~p\n", [OutFile, Reason])
    end;
execute_overlay([{replace, Out, Regex, Replacement} | Rest],
                Vars, BaseDir, TargetDir) ->
    execute_overlay([{replace, Out, Regex, Replacement, []} | Rest],
                    Vars, BaseDir, TargetDir);
execute_overlay([{replace, Out, Regex, Replacement, Opts} | Rest],
                Vars, BaseDir, TargetDir) ->
    Filename = rebar_templater:render(filename:join(TargetDir, Out), Vars),
    {ok, OrigData} = file:read_file(Filename),
    Data = re:replace(OrigData, Regex,
                      rebar_templater:render(Replacement, Vars),
                      [global, {return, binary}] ++ Opts),
    case file:write_file(Filename, Data) of
        ok ->
            ?DEBUG("Edited ~s: s/~s/~s/\n", [Filename, Regex, Replacement]),
            execute_overlay(Rest, Vars, BaseDir, TargetDir);
        {error, Reason} ->
            ?ABORT("Failed to edit ~p: ~p\n", [Filename, Reason])
    end;
execute_overlay([Other | _Rest], _Vars, _BaseDir, _TargetDir) ->
    {error, {unsupported_operation, Other}}.


apply_file_info(InFile, OutFile) ->
    {ok, FileInfo} = file:read_file_info(InFile),
    ok = file:write_file_info(OutFile, FileInfo).

create_RELEASES(TargetDir, RelName, RelVsn) ->
    ReleasesDir = filename:join(TargetDir, "releases"),
    RelFile = filename:join([ReleasesDir, RelVsn, RelName ++ ".rel"]),
    Apps = rebar_rel_utils:get_rel_apps(RelFile),
    TargetLib = filename:join(TargetDir,"lib"),

    AppDirs =
        [ {App, Vsn, TargetLib}
          || {App, Vsn} <- Apps,
             filelib:is_dir(
               filename:join(TargetLib,
                             lists:concat([App, "-", Vsn]))) ],

    case release_handler:create_RELEASES(
           code:root_dir(),
           ReleasesDir,
           RelFile,
           AppDirs) of
        ok ->
            ok;
        {error, Reason} ->
            ?ABORT("Failed to create RELEASES file: ~p\n",
                   [Reason])
    end.
