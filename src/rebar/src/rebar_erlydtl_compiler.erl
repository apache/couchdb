%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com),
%%                    Bryan Fink (bryan@basho.com)
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

%% The rebar_erlydtl_compiler module is a plugin for rebar that compiles
%% ErlyDTL templates.  By default, it compiles all templates/*.dtl
%% to ebin/*_dtl.beam.
%%
%% Configuration options should be placed in rebar.config under
%% 'erlydtl_opts'.  It can be a list of name-value tuples or a list of
%% lists of name-value tuples if you have multiple template directories
%% that need to have different settings (see example below).
%%
%% Available options include:
%%
%%  doc_root: where to find templates to compile
%%            "templates" by default
%%
%%  out_dir: where to put compiled template beam files
%%           "ebin" by default
%%
%%  source_ext: the file extension the template sources have
%%              ".dtl" by default
%%
%%  module_ext: characters to append to the template's module name
%%              "_dtl" by default
%%
%%  recursive: boolean that determines if doc_root(s) need to be
%%             scanned recursively for matching template file names
%%             (default: true).
%% For example, if you had:
%%   /t_src/
%%          base.html
%%          foo.html
%%
%% And you wanted them compiled to:
%%   /priv/
%%         base.beam
%%         foo.beam
%%
%% You would add to your rebar.config:
%%   {erlydtl_opts, [
%%               {doc_root,   "t_src"},
%%               {out_dir,    "priv"},
%%               {source_ext, ".html"},
%%               {module_ext, ""}
%%              ]}.
%%
%% The default settings are the equivalent of:
%%   {erlydtl_opts, [
%%               {doc_root,   "templates"},
%%               {out_dir,    "ebin"},
%%               {source_ext, ".dtl"},
%%               {module_ext, "_dtl"}
%%              ]}.
%%
%% The following example will compile the following templates:
%% "src/*.dtl" files into "ebin/*_dtl.beam" and
%% "templates/*.html" into "ebin/*.beam". Note that any tuple option
%% (such as 'out_dir') in the outer list is added to each inner list:
%%   {erlydtl_opts, [
%%      {out_dir, "ebin"},
%%      {recursive, false},
%%      [
%%          {doc_root, "src"}, {module_ext, "_dtl"}
%%      ],
%%      [
%%          {doc_root, "templates", {module_ext, ""}, {source_ext, ".html"}
%%      ]
%%   ]}.
-module(rebar_erlydtl_compiler).

-export([compile/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    MultiDtlOpts = erlydtl_opts(Config),
    OrigPath = code:get_path(),
    true = code:add_path(rebar_utils:ebin_dir()),

    Result = lists:foldl(fun(DtlOpts, _) ->
            rebar_base_compiler:run(Config, [],
                                     option(doc_root, DtlOpts),
                                     option(source_ext, DtlOpts),
                                     option(out_dir, DtlOpts),
                                     option(module_ext, DtlOpts) ++ ".beam",
                                     fun(S, T, C) ->
                                        compile_dtl(C, S, T, DtlOpts)
                                     end,
                                     [{check_last_mod, false},
                                      {recursive, option(recursive, DtlOpts)}])
        end, ok, MultiDtlOpts),

    true = code:set_path(OrigPath),
    Result.

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, compile) ->
    ?CONSOLE(
       "Build ErlyDtl (*.dtl) sources.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  ~p~n",
       [
        {erlydtl_opts, [{doc_root,   "templates"},
                        {out_dir,    "ebin"},
                        {source_ext, ".dtl"},
                        {module_ext, "_dtl"},
                        {recursive, true}]}
       ]).

erlydtl_opts(Config) ->
    Opts = rebar_config:get(Config, erlydtl_opts, []),
    Tuples = [{K,V} || {K,V} <- Opts],
    case [L || L <- Opts, is_list(L), not io_lib:printable_list(L)] of
        [] ->
            [lists:keysort(1, Tuples)];
        Lists ->
            lists:map(
              fun(L) ->
                      lists:keysort(1,
                                    lists:foldl(
                                      fun({K,T}, Acc) ->
                                              lists:keystore(K, 1, Acc, {K, T})
                                      end, Tuples, L))
              end, Lists)
    end.

option(Opt, DtlOpts) ->
    proplists:get_value(Opt, DtlOpts, default(Opt)).

default(doc_root) -> "templates";
default(out_dir)  -> "ebin";
default(source_ext) -> ".dtl";
default(module_ext) -> "_dtl";
default(custom_tags_dir) -> "";
default(compiler_options) -> [return];
default(recursive) -> true.

compile_dtl(Config, Source, Target, DtlOpts) ->
    case code:which(erlydtl) of
        non_existing ->
            ?ERROR("~n===============================================~n"
                   " You need to install erlydtl to compile DTL templates~n"
                   " Download the latest tarball release from github~n"
                   "    http://code.google.com/p/erlydtl/~n"
                   " and install it into your erlang library dir~n"
                   "===============================================~n~n", []),
            ?FAIL;
        _ ->
            case needs_compile(Source, Target, DtlOpts) of
                true ->
                    do_compile(Config, Source, Target, DtlOpts);
                false ->
                    skipped
            end
    end.

do_compile(Config, Source, Target, DtlOpts) ->
    %% TODO: Check last mod on target and referenced DTLs here..

    %% ensure that doc_root and out_dir are defined,
    %% using defaults if necessary
    Opts = lists:ukeymerge(1,
            DtlOpts,
            lists:sort(
                [{out_dir, option(out_dir, DtlOpts)},
                 {doc_root, option(doc_root, DtlOpts)},
                 {custom_tags_dir, option(custom_tags_dir, DtlOpts)},
                 {compiler_options, option(compiler_options, DtlOpts)}])),
    ?INFO("Compiling \"~s\" -> \"~s\" with options:~n    ~s~n",
        [Source, Target, io_lib:format("~p", [Opts])]),
    case erlydtl:compile(Source,
                         module_name(Target),
                         Opts) of
        ok ->
            ok;
        error ->
            rebar_base_compiler:error_tuple(Config, Source, [], [], Opts);
        {error, {_File, _Msgs} = Error} ->
            rebar_base_compiler:error_tuple(Config, Source, [Error], [], Opts);
        {error, Msg} ->
            Es = [{Source, [{erlydtl_parser, Msg}]}],
            rebar_base_compiler:error_tuple(Config, Source, Es, [], Opts)
    end.

module_name(Target) ->
    F = filename:basename(Target),
    string:substr(F, 1, length(F)-length(".beam")).

needs_compile(Source, Target, DtlOpts) ->
    LM = filelib:last_modified(Target),
    LM < filelib:last_modified(Source) orelse
        lists:any(fun(D) -> LM < filelib:last_modified(D) end,
                  referenced_dtls(Source, DtlOpts)).

referenced_dtls(Source, DtlOpts) ->
    DtlOpts1 = lists:keyreplace(doc_root, 1, DtlOpts,
        {doc_root, filename:dirname(Source)}),
    Set = referenced_dtls1([Source], DtlOpts1,
                           sets:add_element(Source, sets:new())),
    sets:to_list(sets:del_element(Source, Set)).

referenced_dtls1(Step, DtlOpts, Seen) ->
    ExtMatch = re:replace(option(source_ext, DtlOpts), "\.", "\\\\\\\\.",
                          [{return, list}]),

    ShOpts = [{use_stdout, false}, return_on_error],
    AllRefs =
        lists:append(
          [begin
               Cmd = lists:flatten(["grep -o [^\\\"]*\\",
                                    ExtMatch, "[^\\\"]* ", F]),
               case rebar_utils:sh(Cmd, ShOpts) of
                   {ok, Res} ->
                       string:tokens(Res, "\n");
                   {error, _} ->
                       ""
               end
           end || F <- Step]),
    DocRoot = option(doc_root, DtlOpts),
    WithPaths = [ filename:join([DocRoot, F]) || F <- AllRefs ],
    ?DEBUG("All deps: ~p\n", [WithPaths]),
    Existing = [F || F <- WithPaths, filelib:is_regular(F)],
    New = sets:subtract(sets:from_list(Existing), Seen),
    case sets:size(New) of
        0 -> Seen;
        _ -> referenced_dtls1(sets:to_list(New), DtlOpts,
                              sets:union(New, Seen))
    end.
