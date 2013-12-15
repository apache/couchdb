%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2011 Joe Williams (joe@joetify.com)
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

-module(rebar_upgrade).

-include("rebar.hrl").
-include_lib("kernel/include/file.hrl").

-export(['generate-upgrade'/2]).

%% for internal use only
-export([info/2]).

-define(TMP, "_tmp").

%% ====================================================================
%% Public API
%% ====================================================================

'generate-upgrade'(Config0, ReltoolFile) ->
    %% Get the old release path
    {Config, ReltoolConfig} = rebar_rel_utils:load_config(Config0, ReltoolFile),
    TargetParentDir = rebar_rel_utils:get_target_parent_dir(Config,
                                                            ReltoolConfig),
    TargetDir = rebar_rel_utils:get_target_dir(Config, ReltoolConfig),

    PrevRelPath = rebar_rel_utils:get_previous_release_path(Config),
    OldVerPath = filename:join([TargetParentDir, PrevRelPath]),

    %% Run checks to make sure that building a package is possible
    {NewVerPath, NewName, NewVer} = run_checks(Config, OldVerPath,
                                               ReltoolConfig),
    NameVer = NewName ++ "_" ++ NewVer,

    %% Save the code path prior to doing anything
    OrigPath = code:get_path(),

    %% Prepare the environment for building the package
    ok = setup(OldVerPath, NewVerPath, NewName, NewVer, NameVer),

    %% Build the package
    run_systools(NameVer, NewName),

    %% Boot file changes
    {ok, _} = boot_files(TargetDir, NewVer, NewName),

    %% Extract upgrade and tar it back up with changes
    make_tar(NameVer, NewVer, NewName),

    %% Clean up files that systools created
    ok = cleanup(NameVer),

    %% Restore original path
    true = code:set_path(OrigPath),

    {ok, Config}.

%% ===================================================================
%% Internal functions
%% ==================================================================

info(help, 'generate-upgrade') ->
    ?CONSOLE("Build an upgrade package.~n"
             "~n"
             "Valid command line options:~n"
             "  previous_release=path~n",
             []).

run_checks(Config, OldVerPath, ReltoolConfig) ->
    true = rebar_utils:prop_check(filelib:is_dir(OldVerPath),
                                  "Release directory doesn't exist (~p)~n",
                                  [OldVerPath]),

    {Name, Ver} = rebar_rel_utils:get_reltool_release_info(ReltoolConfig),

    NewVerPath =
        filename:join(
          [rebar_rel_utils:get_target_parent_dir(Config, ReltoolConfig),
           Name]),
    true = rebar_utils:prop_check(filelib:is_dir(NewVerPath),
                                  "Release directory doesn't exist (~p)~n",
                                  [NewVerPath]),

    {NewName, NewVer} = rebar_rel_utils:get_rel_release_info(Name, NewVerPath),
    {OldName, OldVer} = rebar_rel_utils:get_rel_release_info(Name, OldVerPath),

    true =
        rebar_utils:prop_check(NewName == OldName,
                               "New and old .rel release names do not match~n",
                               []),
    true =
        rebar_utils:prop_check(Name == NewName,
                               "Reltool and .rel release names do not match~n",
                               []),
    true =
        rebar_utils:prop_check(NewVer =/= OldVer,
                               "New and old .rel contain the same version~n",
                               []),
    true =
        rebar_utils:prop_check(Ver == NewVer,
                               "Reltool and .rel versions do not match~n", []),

    {NewVerPath, NewName, NewVer}.

setup(OldVerPath, NewVerPath, NewName, NewVer, NameVer) ->
    Src = filename:join([NewVerPath, "releases",
                         NewVer, NewName ++ ".rel"]),
    Dst = filename:join([".", NameVer ++ ".rel"]),
    {ok, _} = file:copy(Src, Dst),
    ok = code:add_pathsa(
           lists:append([
                         filelib:wildcard(filename:join([NewVerPath,
                                                         "lib", "*", "ebin"])),
                         filelib:wildcard(filename:join([OldVerPath,
                                                         "releases", "*"])),
                         filelib:wildcard(filename:join([OldVerPath,
                                                         "lib", "*", "ebin"]))
                        ])).

run_systools(NewVer, Name) ->
    Opts = [silent],
    NameList = [Name],
    case systools:make_relup(NewVer, NameList, NameList, Opts) of
        {error, _, Msg} ->
            ?ABORT("Systools [systools:make_relup/4] aborted with: ~p~n",
                   [Msg]);
        _ ->
            ?DEBUG("Relup created~n", []),
            case systools:make_script(NewVer, Opts) of
                {error, _, Msg1} ->
                    ?ABORT("Systools [systools:make_script/2] "
                           "aborted with: ~p~n", [Msg1]);
                _ ->
                    ?DEBUG("Script created~n", []),
                    case systools:make_tar(NewVer, Opts) of
                        {error, _, Msg2} ->
                            ?ABORT("Systools [systools:make_tar/2] "
                                   "aborted with: ~p~n", [Msg2]);
                        _ ->
                            ?DEBUG("Tarball created~n", []),
                            ok
                    end
            end
    end.

boot_files(TargetDir, Ver, Name) ->
    ok = file:make_dir(filename:join([".", ?TMP])),
    ok = file:make_dir(filename:join([".", ?TMP, "releases"])),
    ok = file:make_dir(filename:join([".", ?TMP, "releases", Ver])),
    case os:type() of
        {win32,_} ->
            ok;
        _ ->
            ok = file:make_symlink(
                   filename:join(["start.boot"]),
                   filename:join([".", ?TMP, "releases", Ver, Name ++ ".boot"]))
    end,
    {ok, _} =
        file:copy(
          filename:join([TargetDir, "releases", Ver, "start_clean.boot"]),
          filename:join([".", ?TMP, "releases", Ver, "start_clean.boot"])),

    SysConfig = filename:join([TargetDir, "releases", Ver, "sys.config"]),
    _ = case filelib:is_regular(SysConfig) of
            true ->
                {ok, _} = file:copy(
                            SysConfig,
                            filename:join([".", ?TMP, "releases", Ver,
                                           "sys.config"]));
            false -> ok
        end,

    VmArgs = filename:join([TargetDir, "releases", Ver, "vm.args"]),
    case filelib:is_regular(VmArgs) of
        true ->
            {ok, _} = file:copy(
                        VmArgs,
                        filename:join([".", ?TMP, "releases", Ver, "vm.args"]));
        false -> {ok, 0}
    end.

make_tar(NameVer, NewVer, NewName) ->
    Filename = NameVer ++ ".tar.gz",
    {ok, Cwd} = file:get_cwd(),
    Absname = filename:join([Cwd, Filename]),
    ok = file:set_cwd(?TMP),
    ok = erl_tar:extract(Absname, [compressed]),
    ok = file:delete(Absname),
    case os:type() of
        {win32,_} ->
            {ok, _} =
                file:copy(
                  filename:join([".", "releases", NewVer, "start.boot"]),
                  filename:join([".", "releases", NewVer, NewName ++ ".boot"])),
            ok;
        _ ->
            ok
    end,
    {ok, Tar} = erl_tar:open(Absname, [write, compressed]),
    ok = erl_tar:add(Tar, "lib", []),
    ok = erl_tar:add(Tar, "releases", []),
    ok = erl_tar:close(Tar),
    ok = file:set_cwd(Cwd),
    ?CONSOLE("~s upgrade package created~n", [NameVer]).

cleanup(NameVer) ->
    ?DEBUG("Removing files needed for building the upgrade~n", []),
    Files = [
             filename:join([".", NameVer ++ ".rel"]),
             filename:join([".", NameVer ++ ".boot"]),
             filename:join([".", NameVer ++ ".script"]),
             filename:join([".", "relup"])
            ],
    lists:foreach(fun(F) -> ok = file:delete(F) end, Files),

    ok = remove_dir_tree(?TMP).

%% adapted from http://www.erlang.org/doc/system_principles/create_target.html
remove_dir_tree(Dir) ->
    remove_all_files(".", [Dir]).
remove_all_files(Dir, Files) ->
    lists:foreach(fun(File) ->
                          FilePath = filename:join([Dir, File]),
                          {ok, FileInfo, Link} = file_info(FilePath),
                          case {Link, FileInfo#file_info.type} of
                              {false, directory} ->
                                  {ok, DirFiles} = file:list_dir(FilePath),
                                  remove_all_files(FilePath, DirFiles),
                                  file:del_dir(FilePath);
                              _ ->
                                  file:delete(FilePath)
                          end
                  end, Files).

file_info(Path) ->
    case file:read_file_info(Path) of
        {ok, Info} ->
            {ok, Info, false};
        {error, enoent} ->
            {ok, Info} = file:read_link_info(Path),
            {ok, Info, true};
        Error ->
            Error
    end.
