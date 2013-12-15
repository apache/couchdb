%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
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
-module(rebar_dia_compiler).

-export([compile/2, clean/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(rebar_config:config(), file:filename()) -> 'ok'.
compile(Config, _AppFile) ->
    rebar_base_compiler:run(Config, filelib:wildcard("dia/*.dia"),
                            "dia", ".dia", "src", ".erl",
                            fun compile_dia/3).

-spec clean(rebar_config:config(), file:filename()) -> 'ok'.
clean(_Config, _AppFile) ->
    GeneratedFiles = dia_generated_files("dia", "src", "include"),
    ok = rebar_file_utils:delete_each(GeneratedFiles),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, compile) ->
    info_help("Build Diameter (*.dia) sources");
info(help, clean) ->
    info_help("Delete generated Diameter files").

info_help(Description) ->
    ?CONSOLE(
       "~s.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  {dia_opts, []} (see diameter_codegen:from_dict/4 documentation)~n",
       [Description]).

-spec compile_dia(file:filename(), file:filename(),
                   rebar_config:config()) -> ok.
compile_dia(Source, Target, Config) ->
    ok = filelib:ensure_dir(Target),
    ok = filelib:ensure_dir(filename:join("include", "dummy.hrl")),
    Opts = [{outdir, "src"}] ++ rebar_config:get(Config, dia_opts, []),
    case diameter_dict_util:parse({path, Source}, []) of
        {ok, Spec} ->
            FileName = dia_filename(Source, Spec),
            diameter_codegen:from_dict(FileName, Spec, Opts, erl),
            diameter_codegen:from_dict(FileName, Spec, Opts, hrl),
            HrlFile = filename:join("src", FileName ++ ".hrl"),
            case filelib:is_regular(HrlFile) of
                true ->
                    ok = rebar_file_utils:mv(HrlFile, "include");
                false ->
                    ok
            end;
        {error, Reason} ->
            ?ERROR("~s~n", [diameter_dict_util:format_error(Reason)])
    end.

dia_generated_files(DiaDir, SrcDir, IncDir) ->
    F = fun(File, Acc) ->
            {ok, Spec} = diameter_dict_util:parse({path, File}, []),
            FileName = dia_filename(File, Spec),
            [filename:join([IncDir, FileName ++ ".hrl"]) |
             filelib:wildcard(filename:join([SrcDir, FileName ++ ".*"]))] ++ Acc
    end,
    lists:foldl(F, [], filelib:wildcard(filename:join([DiaDir, "*.dia"]))).

dia_filename(File, Spec) ->
    case proplists:get_value(name, Spec) of
        undefined ->
            filename:rootname(filename:basename(File));
        Name ->
            Name
    end.
