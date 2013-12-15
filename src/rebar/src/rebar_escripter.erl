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
-module(rebar_escripter).

-export([escriptize/2,
         clean/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").
-include_lib("kernel/include/file.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

escriptize(Config0, AppFile) ->
    %% Extract the application name from the archive -- this is the default
    %% name of the generated script
    {Config, AppName} = rebar_app_utils:app_name(Config0, AppFile),
    AppNameStr = atom_to_list(AppName),

    %% Get the output filename for the escript -- this may include dirs
    Filename = rebar_config:get_local(Config, escript_name, AppName),
    ok = filelib:ensure_dir(Filename),

    %% Look for a list of other applications (dependencies) to include
    %% in the output file. We then use the .app files for each of these
    %% to pull in all the .beam files.
    InclBeams = get_app_beams(
                  rebar_config:get_local(Config, escript_incl_apps, []), []),

    %% Look for a list of extra files to include in the output file.
    %% For internal rebar-private use only. Do not use outside rebar.
    InclExtra = get_extra(Config),

    %% Construct the archive of everything in ebin/ dir -- put it on the
    %% top-level of the zip file so that code loading works properly.
    EbinPrefix = filename:join(AppNameStr, "ebin"),
    EbinFiles = usort(load_files(EbinPrefix, "*", "ebin")),
    ExtraFiles = usort(InclBeams ++ InclExtra),
    Files = EbinFiles ++ ExtraFiles,

    case zip:create("mem", Files, [memory]) of
        {ok, {"mem", ZipBin}} ->
            %% Archive was successfully created. Prefix that binary with our
            %% header and write to our escript file
            Shebang = rebar_config:get(Config, escript_shebang,
                                       "#!/usr/bin/env escript\n"),
            Comment = rebar_config:get(Config, escript_comment, "%%\n"),
            DefaultEmuArgs = ?FMT("%%! -pa ~s/~s/ebin\n",
                                  [AppNameStr, AppNameStr]),
            EmuArgs = rebar_config:get(Config, escript_emu_args,
                                       DefaultEmuArgs),
            Script = iolist_to_binary([Shebang, Comment, EmuArgs, ZipBin]),
            case file:write_file(Filename, Script) of
                ok ->
                    ok;
                {error, WriteError} ->
                    ?ERROR("Failed to write ~p script: ~p\n",
                           [AppName, WriteError]),
                    ?FAIL
            end;
        {error, ZipError} ->
            ?ERROR("Failed to construct ~p escript: ~p\n",
                   [AppName, ZipError]),
            ?FAIL
    end,

    %% Finally, update executable perms for our script
    {ok, #file_info{mode = Mode}} = file:read_file_info(Filename),
    ok = file:change_mode(Filename, Mode bor 8#00111),
    {ok, Config}.

clean(Config0, AppFile) ->
    %% Extract the application name from the archive -- this is the default
    %% name of the generated script
    {Config, AppName} = rebar_app_utils:app_name(Config0, AppFile),

    %% Get the output filename for the escript -- this may include dirs
    Filename = rebar_config:get_local(Config, escript_name, AppName),
    rebar_file_utils:delete_each([Filename]),
    {ok, Config}.

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, escriptize) ->
    info_help("Generate escript archive");
info(help, clean) ->
    info_help("Delete generated escript archive").

info_help(Description) ->
    ?CONSOLE(
       "~s.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  ~p~n"
       "  ~p~n"
       "  ~p~n"
       "  ~p~n"
       "  ~p~n",
       [
        Description,
        {escript_name, "application"},
        {escript_incl_apps, []},
        {escript_shebang, "#!/usr/bin/env escript\n"},
        {escript_comment, "%%\n"},
        {escript_emu_args, "%%! -pa application/application/ebin\n"}
       ]).

get_app_beams([], Acc) ->
    Acc;
get_app_beams([App | Rest], Acc) ->
    case code:lib_dir(App, ebin) of
        {error, bad_name} ->
            ?ABORT("Failed to get ebin/ directory for "
                   "~p escript_incl_apps.", [App]);
        Path ->
            Prefix = filename:join(atom_to_list(App), "ebin"),
            Acc2 = load_files(Prefix, "*", Path),
            get_app_beams(Rest, Acc2 ++ Acc)
    end.

get_extra(Config) ->
    Extra = rebar_config:get_local(Config, escript_incl_extra, []),
    lists:foldl(fun({Wildcard, Dir}, Files) ->
                        load_files(Wildcard, Dir) ++ Files
                end, [], Extra).

load_files(Wildcard, Dir) ->
    load_files("", Wildcard, Dir).

load_files(Prefix, Wildcard, Dir) ->
    [read_file(Prefix, Filename, Dir)
     || Filename <- filelib:wildcard(Wildcard, Dir)].

read_file(Prefix, Filename, Dir) ->
    Filename1 = case Prefix of
                    "" ->
                        Filename;
                    _ ->
                        filename:join([Prefix, Filename])
                end,
    [dir_entries(filename:dirname(Filename1)),
     {Filename1, file_contents(filename:join(Dir, Filename))}].

file_contents(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Bin.

%% Given a filename, return zip archive dir entries for each sub-dir.
%% Required to work around issues fixed in OTP-10071.
dir_entries(File) ->
    Dirs = dirs(File),
    [{Dir ++ "/", <<>>} || Dir <- Dirs].

%% Given "foo/bar/baz", return ["foo", "foo/bar", "foo/bar/baz"].
dirs(Dir) ->
    dirs1(filename:split(Dir), "", []).

dirs1([], _, Acc) ->
    lists:reverse(Acc);
dirs1([H|T], "", []) ->
    dirs1(T, H, [H]);
dirs1([H|T], Last, Acc) ->
    Dir = filename:join(Last, H),
    dirs1(T, Dir, [Dir|Acc]).

usort(List) ->
    lists:ukeysort(1, lists:flatten(List)).
