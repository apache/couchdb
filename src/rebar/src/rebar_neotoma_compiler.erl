%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2010 Cliff Moon (cliff@moonpolysoft.com)
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

%% The rebar_neotoma module is a plugin for rebar that compiles
%% neotoma peg files.  By default, it compiles all src/*.peg to src/*.erl
%%
%% Configuration options should be placed in rebar.config under
%% neotoma_opts.  Available options include:
%%
%% doc_root: where to find the peg files to compile.
%%           "src" by default
%% out_dir: where to put the generated erl files.
%%          "src" by defualt
%% module_ext: characters to append to the module's name.
%%             "" by default
%% source_ext: extension of peg source files
-module(rebar_neotoma_compiler).

-export([compile/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").

%% ============================================================================
%% Public API
%% ============================================================================

compile(Config, _AppFile) ->
    NeoOpts = neotoma_opts(Config),
    rebar_base_compiler:run(Config, [],
                            option(doc_root, NeoOpts), ".peg",
                            option(out_dir, NeoOpts),
                            option(module_ext, NeoOpts) ++ ".erl",
                            fun compile_neo/3, [{check_last_mod, true}]).

%% ============================================================================
%% Internal functions
%% ============================================================================

info(help, compile) ->
    ?CONSOLE(
       "Build Neotoma (*.peg) sources.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  ~p~n",
       [
        {neotoma_opts, [{doc_root, "src"},
                        {out_dir, "src"},
                        {source_ext, ".peg"},
                        {module_ext, ""}]}
       ]).

neotoma_opts(Config) ->
    rebar_config:get(Config, neotoma_opts, []).

option(Opt, Options) ->
    proplists:get_value(Opt, Options, default(Opt)).

default(doc_root) -> "src";
default(out_dir) -> "src";
default(module_ext) -> "";
default(source_ext) -> ".peg".

compile_neo(Source, Target, Config) ->
    case code:which(neotoma) of
        non_existing ->
            ?ERROR("~n===============================================~n"
                   " You need to install neotoma to compile PEG grammars~n"
                   " Download the latest tarball release from github~n"
                   "    https://github.com/seancribbs/neotoma~n"
                   " and install it into your erlang library dir~n"
                   "===============================================~n~n", []),
            ?FAIL;
        _ ->
            case needs_compile(Source, Target, Config) of
                true ->
                    do_compile(Source, Target, Config);
                false ->
                    skipped
            end
    end.

do_compile(Source, _Target, Config) ->
    %% TODO: Check last mod on target and referenced DTLs here..
    NeoOpts = neotoma_opts(Config),
    %% ensure that doc_root and out_dir are defined,
    %% using defaults if necessary
    Opts = [{output, option(out_dir, NeoOpts)},
            {module, list_to_atom(filename:basename(Source, ".peg")
                                  ++ option(module_ext, NeoOpts))}],
    case neotoma:file(Source, Opts ++ NeoOpts) of
        ok ->
            ok;
        Reason ->
            ?ERROR("Compiling peg ~s failed:~n  ~p~n",
                   [Source, Reason]),
            ?FAIL
    end.

needs_compile(Source, Target, Config) ->
    LM = filelib:last_modified(Target),
    LM < filelib:last_modified(Source) orelse
        lists:any(fun(D) -> LM < filelib:last_modified(D) end,
                  referenced_pegs(Source, Config)).

referenced_pegs(Source, Config) ->
    Set = referenced_pegs1([Source], Config,
                           sets:add_element(Source, sets:new())),
    sets:to_list(sets:del_element(Source, Set)).

referenced_pegs1(Step, Config, Seen) ->
    NeoOpts = neotoma_opts(Config),
    ExtMatch = re:replace(option(source_ext, NeoOpts), "\.", "\\\\\\\\.",
                          [{return, list}]),

    ShOpts = [{use_stdout, false}, return_on_error],
    AllRefs =
        lists:append(
          [begin
               Cmd = lists:flatten(["grep -o [^\\\"]*",
                                    ExtMatch, " ", F]),
               case rebar_utils:sh(Cmd, ShOpts) of
                   {ok, Res} ->
                       string:tokens(Res, "\n");
                   {error, _} ->
                       ""
               end
           end || F <- Step]),
    DocRoot = option(doc_root, NeoOpts),
    WithPaths = [ filename:join([DocRoot, F]) || F <- AllRefs ],
    Existing = [F || F <- WithPaths, filelib:is_regular(F)],
    New = sets:subtract(sets:from_list(Existing), Seen),
    case sets:size(New) of
        0 -> Seen;
        _ -> referenced_pegs1(sets:to_list(New), Config,
                              sets:union(New, Seen))
    end.
