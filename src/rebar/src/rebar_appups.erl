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
%% ------------------------------------------------------------------

-module(rebar_appups).

-include("rebar.hrl").

-export(['generate-appups'/2]).

%% for internal use only
-export([info/2]).

-define(APPUPFILEFORMAT, "%% appup generated for ~p by rebar (~p)~n"
        "{~p, [{~p, ~p}], [{~p, []}]}.~n").

%% ====================================================================
%% Public API
%% ====================================================================

'generate-appups'(Config, ReltoolFile) ->
    %% Get the old release path
    {Config1, ReltoolConfig} = rebar_rel_utils:load_config(Config, ReltoolFile),
    TargetParentDir = rebar_rel_utils:get_target_parent_dir(Config,
                                                            ReltoolConfig),

    PrevRelPath = rebar_rel_utils:get_previous_release_path(Config),
    OldVerPath = filename:join([TargetParentDir, PrevRelPath]),

    %% Get the new and old release name and versions
    {Name, _Ver} = rebar_rel_utils:get_reltool_release_info(ReltoolConfig),
    NewVerPath = filename:join([TargetParentDir, Name]),
    {NewName, NewVer} = rebar_rel_utils:get_rel_release_info(Name, NewVerPath),
    {OldName, OldVer} = rebar_rel_utils:get_rel_release_info(Name, OldVerPath),

    %% Run some simple checks
    true = rebar_utils:prop_check(NewVer =/= OldVer,
                                  "New and old .rel versions match~n", []),
    true = rebar_utils:prop_check(
             NewName == OldName,
             "Reltool and .rel release names do not match~n", []),

    %% Find all the apps that have been upgraded
    {_Added, _Removed, Upgraded} = get_apps(Name, OldVerPath, NewVerPath),

    %% Get a list of any appup files that exist in the new release
    NewAppUpFiles = rebar_utils:find_files(
                      filename:join([NewVerPath, "lib"]), "^.*.appup$"),

    %% Convert the list of appup files into app names
    AppUpApps = [file_to_name(File) || File <- NewAppUpFiles],

    %% Create a list of apps that don't already have appups
    UpgradeApps = genappup_which_apps(Upgraded, AppUpApps),

    %% Generate appup files for upgraded apps
    generate_appup_files(NewVerPath, OldVerPath, UpgradeApps),

    {ok, Config1}.

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, 'generate-appups') ->
    ?CONSOLE("Generate appup files.~n"
             "~n"
             "Valid command line options:~n"
             "  previous_release=path~n",
             []).

get_apps(Name, OldVerPath, NewVerPath) ->
    OldApps = rebar_rel_utils:get_rel_apps(Name, OldVerPath),
    ?DEBUG("Old Version Apps: ~p~n", [OldApps]),

    NewApps = rebar_rel_utils:get_rel_apps(Name, NewVerPath),
    ?DEBUG("New Version Apps: ~p~n", [NewApps]),

    Added = app_list_diff(NewApps, OldApps),
    ?DEBUG("Added: ~p~n", [Added]),

    Removed = app_list_diff(OldApps, NewApps),
    ?DEBUG("Removed: ~p~n", [Removed]),

    PossiblyUpgraded = proplists:get_keys(NewApps),

    UpgradedApps = [upgraded_app(AppName,
                                 proplists:get_value(AppName, OldApps),
                                 proplists:get_value(AppName, NewApps))
                    || AppName <- PossiblyUpgraded],

    Upgraded = lists:dropwhile(fun(Elem) ->
                                       Elem == false
                               end, lists:sort(UpgradedApps)),

    ?DEBUG("Upgraded: ~p~n", [Upgraded]),

    {Added, Removed, Upgraded}.

upgraded_app(AppName, OldAppVer, NewAppVer) when OldAppVer /= NewAppVer ->
    {AppName, {OldAppVer, NewAppVer}};
upgraded_app(_, _, _) ->
    false.

app_list_diff(List1, List2) ->
    List3 = lists:umerge(lists:sort(proplists:get_keys(List1)),
                         lists:sort(proplists:get_keys(List2))),
    List3 -- proplists:get_keys(List2).

file_to_name(File) ->
    filename:rootname(filename:basename(File)).

genappup_which_apps(UpgradedApps, [First|Rest]) ->
    List = proplists:delete(list_to_atom(First), UpgradedApps),
    genappup_which_apps(List, Rest);
genappup_which_apps(Apps, []) ->
    Apps.

generate_appup_files(NewVerPath, OldVerPath, [{_App, {undefined, _}}|Rest]) ->
    generate_appup_files(NewVerPath, OldVerPath, Rest);
generate_appup_files(NewVerPath, OldVerPath, [{App, {OldVer, NewVer}}|Rest]) ->
    OldEbinDir = filename:join([OldVerPath, "lib",
                                atom_to_list(App) ++ "-" ++ OldVer, "ebin"]),
    NewEbinDir = filename:join([NewVerPath, "lib",
                                atom_to_list(App) ++ "-" ++ NewVer, "ebin"]),

    {AddedFiles, DeletedFiles, ChangedFiles} = beam_lib:cmp_dirs(NewEbinDir,
                                                                 OldEbinDir),

    Added = [generate_instruction(added, File) || File <- AddedFiles],
    Deleted = [generate_instruction(deleted, File) || File <- DeletedFiles],
    Changed = [generate_instruction(changed, File) || File <- ChangedFiles],

    Inst = lists:append([Added, Deleted, Changed]),

    AppUpFile = filename:join([NewEbinDir, atom_to_list(App) ++ ".appup"]),

    ok = file:write_file(AppUpFile,
                         io_lib:fwrite(?APPUPFILEFORMAT,
                                       [App, rebar_utils:now_str(), NewVer,
                                        OldVer, Inst, OldVer])),

    ?CONSOLE("Generated appup for ~p~n", [App]),
    generate_appup_files(NewVerPath, OldVerPath, Rest);
generate_appup_files(_, _, []) ->
    ?CONSOLE("Appup generation complete~n", []).

generate_instruction(added, File) ->
    Name = list_to_atom(file_to_name(File)),
    {add_module, Name};
generate_instruction(deleted, File) ->
    Name = list_to_atom(file_to_name(File)),
    {delete_module, Name};
generate_instruction(changed, {File, _}) ->
    {ok, {Name, List}} = beam_lib:chunks(File, [attributes, exports]),
    Behavior = get_behavior(List),
    CodeChange = is_code_change(List),
    generate_instruction_advanced(Name, Behavior, CodeChange).

generate_instruction_advanced(Name, undefined, undefined) ->
    %% Not a behavior or code change, assume purely functional
    {load_module, Name};
generate_instruction_advanced(Name, [supervisor], _) ->
    %% Supervisor
    {update, Name, supervisor};
generate_instruction_advanced(Name, _, code_change) ->
    %% Includes code_change export
    {update, Name, {advanced, []}};
generate_instruction_advanced(Name, _, _) ->
    %% Anything else
    {load_module, Name}.

get_behavior(List) ->
    Attributes = proplists:get_value(attributes, List),
    case proplists:get_value(behavior, Attributes) of
        undefined -> proplists:get_value(behaviour, Attributes);
        Else -> Else
    end.

is_code_change(List) ->
    Exports = proplists:get_value(exports, List),
    case proplists:is_defined(code_change, Exports) of
        true ->
            code_change;
        false ->
            undefined
    end.
