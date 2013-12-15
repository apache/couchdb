%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2010 Dave Smith (dizzyd@dizzyd.com)
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
%% @author Dave Smith <dizzyd@dizzyd.com>
%% @doc rebar_edoc supports the following command:
%% <ul>
%%   <li>doc (essentially erl -noshell -run edoc_run application
%% "'$(&lt;app_name&gt;)'"
%% '"."' '[&lt;options&gt;]')</li>
%% </ul>
%% EDoc options can be given in the <code>edoc_opts</code> option in
%% <code>rebar.config</code>.
%% @copyright 2010 Dave Smith
%% -------------------------------------------------------------------
-module(rebar_edoc).

-export([doc/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

doc(Config, File) ->
    %% Save code path
    CodePath = setup_code_path(),

    %% Get the edoc_opts and app file info
    EDocOpts = rebar_config:get(Config, edoc_opts, []),
    {ok, Config1, AppName, _AppData} =
        rebar_app_utils:load_app_file(Config, File),

    case needs_regen(EDocOpts) of
        true ->
            ?INFO("Regenerating edocs for ~p\n", [AppName]),
            ok = edoc:application(AppName, ".", EDocOpts);
        false ->
            ?INFO("Skipping regeneration of edocs for ~p\n", [AppName]),
            ok
    end,

    %% Restore code path
    true = code:set_path(CodePath),
    {ok, Config1}.

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, doc) ->
    ?CONSOLE(
       "Generate Erlang program documentation.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  {edoc_opts, []} (see edoc:application/3 documentation)~n",
       []).

setup_code_path() ->
    %% Setup code path prior to calling edoc so that edown, asciiedoc,
    %% and the like can work properly when generating their own
    %% documentation.
    CodePath = code:get_path(),
    true = code:add_patha(rebar_utils:ebin_dir()),
    CodePath.

-type path_spec() :: {'file', file:filename()} | file:filename().
-spec newer_file_exists(Paths::[path_spec()], OldFile::string()) -> boolean().
newer_file_exists(Paths, OldFile) ->
    OldModTime = filelib:last_modified(OldFile),

    ThrowIfNewer = fun(Fn, _Acc) ->
                           FModTime = filelib:last_modified(Fn),
                           (FModTime > OldModTime) andalso
                               throw({newer_file_exists, {Fn, FModTime}})
                   end,

    try
        lists:foldl(fun({file, F}, _) ->
                            ThrowIfNewer(F, false);
                       (P, _) ->
                            filelib:fold_files(P, ".*.erl", true,
                                               ThrowIfNewer, false)
                    end, undefined, Paths)
    catch
        throw:{newer_file_exists, {Filename, FMod}} ->
            ?DEBUG("~p is more recent than ~p: "
                   "~120p > ~120p\n",
                   [Filename, OldFile, FMod, OldModTime]),
            true
    end.

%% Needs regen if any dependent file is changed since the last
%% edoc run. Dependent files are the erlang source files,
%% and the overview file, if it exists.
-spec needs_regen(proplists:proplist()) -> boolean().
needs_regen(EDocOpts) ->
    DocDir = proplists:get_value(dir, EDocOpts, "doc"),
    EDocInfoName = filename:join(DocDir, "edoc-info"),
    OverviewFile = proplists:get_value(overview, EDocOpts, "overview.edoc"),
    EDocOverviewName = filename:join(DocDir, OverviewFile),
    SrcPaths = proplists:get_value(source_path, EDocOpts, ["src"]),

    newer_file_exists([{file, EDocOverviewName} | SrcPaths], EDocInfoName).
