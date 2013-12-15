%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2010 Anthony Ramine (nox@dev-extend.eu),
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

%% The rebar_abnfc_compiler module is a plugin for rebar that compiles
%% ABNF grammars into parsers.  By default, it compiles all src/*.abnf
%% to src/*.erl.
%%
%% Configuration options should be placed in rebar.config under
%% 'abnfc_opts'.  Available options include:
%%
%%  doc_root: where to find the ABNF grammars to compile
%%            "src" by default
%%
%%  out_dir: where to put the generated files.
%%           "src" by default
%%
%%  source_ext: the file extension the ABNF grammars have.
%%              ".abnf" by default
%%
%%  module_ext: characters to append to the parser's module name
%%              "" by default
-module(rebar_abnfc_compiler).

-export([compile/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    DtlOpts = abnfc_opts(Config),
    rebar_base_compiler:run(Config, [],
                            option(doc_root, DtlOpts),
                            option(source_ext, DtlOpts),
                            option(out_dir, DtlOpts),
                            option(module_ext, DtlOpts) ++ ".erl",
                            fun compile_abnfc/3).

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, compile) ->
    ?CONSOLE(
       "Build ABNF (*.abnf) sources.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  ~p~n",
       [
        {abnfc_opts, [{doc_root, "src"},
                      {out_dir, "src"},
                      {source_ext, ".abnfc"},
                      {module_ext, ""}]}
       ]).

abnfc_opts(Config) ->
    rebar_config:get(Config, abnfc_opts, []).

option(Opt, DtlOpts) ->
    proplists:get_value(Opt, DtlOpts, default(Opt)).

default(doc_root) -> "src";
default(out_dir)  -> "src";
default(source_ext) -> ".abnf";
default(module_ext) -> "".

abnfc_is_present() ->
    code:which(abnfc) =/= non_existing.

compile_abnfc(Source, _Target, Config) ->
    case abnfc_is_present() of
        false ->
            ?ERROR("~n===============================================~n"
                   " You need to install abnfc to compile ABNF grammars~n"
                   " Download the latest tarball release from github~n"
                   "    https://github.com/nygge/abnfc~n"
                   " and install it into your erlang library dir~n"
                   "===============================================~n~n", []),
            ?FAIL;
        true ->
            AbnfcOpts = abnfc_opts(Config),
            SourceExt = option(source_ext, AbnfcOpts),
            Opts = [noobj,
                    {o, option(out_dir, AbnfcOpts)},
                    {mod, filename:basename(Source, SourceExt) ++
                         option(module_ext, AbnfcOpts)}],
            case abnfc:file(Source, Opts) of
                ok -> ok;
                Error ->
                    ?ERROR("Compiling grammar ~s failed:~n  ~p~n",
                           [Source, Error]),
                    ?FAIL
            end
    end.
