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
-module(rebar_asn1_compiler).
-author('ruslan@babayev.com').

-export([compile/2,
         clean/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(rebar_config:config(), file:filename()) -> 'ok'.
compile(Config, _AppFile) ->
    rebar_base_compiler:run(Config, filelib:wildcard("asn1/*.asn1"),
                            "asn1", ".asn1", "src", ".erl",
                            fun compile_asn1/3).

-spec clean(rebar_config:config(), file:filename()) -> 'ok'.
clean(_Config, _AppFile) ->
    GeneratedFiles = asn_generated_files("asn1", "src", "include"),
    ok = rebar_file_utils:delete_each(GeneratedFiles),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, compile) ->
    info_help("Build ASN.1 (*.asn1) sources");
info(help, clean) ->
    info_help("Delete ASN.1 (*.asn1) results").

info_help(Description) ->
    ?CONSOLE(
       "~s.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  {asn1_opts, []} (see asn1ct:compile/2 documentation)~n",
       [Description]).

-spec compile_asn1(file:filename(), file:filename(),
                   rebar_config:config()) -> ok.
compile_asn1(Source, Target, Config) ->
    ok = filelib:ensure_dir(Target),
    ok = filelib:ensure_dir(filename:join("include", "dummy.hrl")),
    Opts = [{outdir, "src"}, noobj] ++ rebar_config:get(Config, asn1_opts, []),
    case asn1ct:compile(Source, Opts) of
        ok ->
            Asn1 = filename:basename(Source, ".asn1"),
            HrlFile = filename:join("src", Asn1 ++ ".hrl"),
            case filelib:is_regular(HrlFile) of
                true ->
                    ok = rebar_file_utils:mv(HrlFile, "include");
                false ->
                    ok
            end;
        {error, _Reason} ->
            ?FAIL
    end.

asn_generated_files(AsnDir, SrcDir, IncDir) ->
    lists:foldl(
      fun(AsnFile, Acc) ->
              Base = filename:rootname(filename:basename(AsnFile)),
              [filename:join([IncDir, Base ++ ".hrl"])|
               filelib:wildcard(filename:join([SrcDir, Base ++ ".*"]))] ++ Acc
      end,
      [],
      filelib:wildcard(filename:join([AsnDir, "*.asn1"]))
     ).
