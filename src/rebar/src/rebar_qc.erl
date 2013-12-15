%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2011-2012 Tuncer Ayaz
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
-module(rebar_qc).

-export([qc/2, triq/2, eqc/2, clean/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").

-define(QC_DIR, ".qc").

%% ===================================================================
%% Public API
%% ===================================================================

qc(Config, _AppFile) ->
    ?CONSOLE("NOTICE: Using experimental 'qc' command~n", []),
    run_qc(Config, qc_opts(Config)).

triq(Config, _AppFile) ->
    ?CONSOLE("NOTICE: Using experimental 'triq' command~n", []),
    ok = load_qc_mod(triq),
    run_qc(Config, qc_opts(Config), triq).

eqc(Config, _AppFile) ->
    ?CONSOLE("NOTICE: Using experimental 'eqc' command~n", []),
    ok = load_qc_mod(eqc),
    run_qc(Config, qc_opts(Config), eqc).

clean(_Config, _File) ->
    rebar_file_utils:rm_rf(?QC_DIR).

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, qc) ->
    ?CONSOLE(
       "Test QuickCheck properties.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  {qc_opts, [{qc_mod, module()}, Options]}~n"
       "  ~p~n"
       "  ~p~n",
       [
        {qc_compile_opts, []},
        {qc_first_files, []}
       ]).

-define(TRIQ_MOD, triq).
-define(EQC_MOD, eqc).

qc_opts(Config) ->
    rebar_config:get(Config, qc_opts, []).

run_qc(Config, QCOpts) ->
    run_qc(Config, QCOpts, select_qc_mod(QCOpts)).

run_qc(Config, RawQCOpts, QC) ->
    ?DEBUG("Selected QC module: ~p~n", [QC]),
    QCOpts = lists:filter(fun({qc_mod, _}) -> false;
                             (_) -> true
                          end, RawQCOpts),
    run(Config, QC, QCOpts).

select_qc_mod(QCOpts) ->
    case proplists:get_value(qc_mod, QCOpts) of
        undefined ->
            detect_qc_mod();
        QC ->
            case code:ensure_loaded(QC) of
                {module, QC} ->
                    QC;
                {error, nofile} ->
                    ?ABORT("Configured QC library '~p' not available~n", [QC])
            end
    end.

detect_qc_mod() ->
    case code:ensure_loaded(?TRIQ_MOD) of
        {module, ?TRIQ_MOD} ->
            ?TRIQ_MOD;
        {error, nofile} ->
            case code:ensure_loaded(?EQC_MOD) of
                {module, ?EQC_MOD} ->
                    ?EQC_MOD;
                {error, nofile} ->
                    ?ABORT("No QC library available~n", [])
            end
    end.

load_qc_mod(Mod) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            ok;
        {error, nofile} ->
            ?ABORT("Failed to load QC lib '~p'~n", [Mod])
    end.

ensure_dirs() ->
    ok = filelib:ensure_dir(filename:join(qc_dir(), "dummy")),
    ok = filelib:ensure_dir(filename:join(rebar_utils:ebin_dir(), "dummy")).

setup_codepath() ->
    CodePath = code:get_path(),
    true = code:add_patha(qc_dir()),
    true = code:add_pathz(rebar_utils:ebin_dir()),
    CodePath.

qc_dir() ->
    filename:join(rebar_utils:get_cwd(), ?QC_DIR).

run(Config, QC, QCOpts) ->
    ?DEBUG("qc_opts: ~p~n", [QCOpts]),

    ok = ensure_dirs(),
    CodePath = setup_codepath(),

    CompileOnly = rebar_utils:get_experimental_global(Config, compile_only,
                                                      false),
    %% Compile erlang code to ?QC_DIR, using a tweaked config
    %% with appropriate defines, and include all the test modules
    %% as well.
    {ok, _SrcErls} = rebar_erlc_compiler:test_compile(Config, "qc", ?QC_DIR),

    case CompileOnly of
        "true" ->
            true = code:set_path(CodePath),
            ?CONSOLE("Compiled modules for qc~n", []);
        false ->
            run1(QC, QCOpts, CodePath)
    end.

run1(QC, QCOpts, CodePath) ->
    TestModule = fun(M) -> qc_module(QC, QCOpts, M) end,
    case lists:flatmap(TestModule, find_prop_mods()) of
        [] ->
            true = code:set_path(CodePath),
            ok;
        Errors ->
            ?ABORT("One or more QC properties didn't hold true:~n~p~n",
                   [Errors])
    end.

qc_module(QC=triq, _QCOpts, M) ->
    case QC:module(M) of
        true ->
            [];
        Failed ->
            [Failed]
    end;
qc_module(QC=eqc, [], M) -> QC:module(M);
qc_module(QC=eqc, QCOpts, M) -> QC:module(QCOpts, M).

find_prop_mods() ->
    Beams = rebar_utils:find_files(?QC_DIR, ".*\\.beam\$"),
    [M || M <- [rebar_utils:erl_to_mod(Beam) || Beam <- Beams], has_prop(M)].

has_prop(Mod) ->
    lists:any(fun({F,_A}) -> lists:prefix("prop_", atom_to_list(F)) end,
              Mod:module_info(exports)).
