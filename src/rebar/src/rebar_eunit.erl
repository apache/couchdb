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
%% @author Dave Smith <dizzyd@dizzyd.com>
%% @doc rebar_eunit supports the following commands:
%% <ul>
%%   <li>eunit - runs eunit tests</li>
%%   <li>clean - remove ?EUNIT_DIR directory</li>
%%   <li>reset_after_eunit::boolean() - default = true.
%%       If true, try to "reset" VM state to approximate state prior to
%%       running the EUnit tests:
%%       <ul>
%%        <li>Stop net_kernel if it was started</li>
%%        <li>Stop OTP applications not running before EUnit tests were run</li>
%%        <li>Kill processes not running before EUnit tests were run</li>
%%        <li>Reset OTP application environment variables</li>
%%       </ul>
%%   </li>
%% </ul>
%% The following Global options are supported:
%% <ul>
%%   <li>verbose=1 - show extra output from the eunit test</li>
%%   <li>
%%      suites="foo,bar" - runs tests in foo.erl, test/foo_tests.erl and
%%      tests in bar.erl, test/bar_tests.erl
%%   </li>
%%   <li>
%%      suites="foo,bar" tests="baz"- runs first test with name starting
%%      with 'baz' in foo.erl, test/foo_tests.erl and tests in bar.erl,
%%      test/bar_tests.erl
%%   </li>
%%   <li>
%%      tests="baz"- For every existing suite, run the first test whose
%%      name starts with bar and, if no such test exists, run the test
%%      whose name starts with bar in the suite's _tests module
%%   </li>
%% </ul>
%% Additionally, for projects that have separate folders for the core
%% implementation, and for the unit tests, then the following
%% <code>rebar.config</code> option can be provided:
%% <code>{eunit_compile_opts, [{src_dirs, ["src", "dir"]}]}.</code>.
%% @copyright 2009, 2010 Dave Smith
%% -------------------------------------------------------------------
-module(rebar_eunit).

-export([eunit/2,
         clean/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").

-define(EUNIT_DIR, ".eunit").

%% ===================================================================
%% Public API
%% ===================================================================

eunit(Config, _AppFile) ->
    ok = ensure_dirs(),
    %% Save code path
    CodePath = setup_code_path(),
    CompileOnly = rebar_utils:get_experimental_global(Config, compile_only,
                                                      false),
    {ok, SrcErls} = rebar_erlc_compiler:test_compile(Config, "eunit",
                                                     ?EUNIT_DIR),
    case CompileOnly of
        "true" ->
            true = code:set_path(CodePath),
            ?CONSOLE("Compiled modules for eunit~n", []);
        false ->
            run_eunit(Config, CodePath, SrcErls)
    end.

clean(_Config, _File) ->
    rebar_file_utils:rm_rf(?EUNIT_DIR).

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, eunit) ->
    info_help("Run eunit tests");
info(help, clean) ->
    Description = ?FMT("Delete eunit test dir (~s)", [?EUNIT_DIR]),
    info_help(Description).

info_help(Description) ->
    ?CONSOLE(
       "~s.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  ~p~n"
       "  ~p~n"
       "  ~p~n"
       "  ~p~n"
       "  ~p~n"
       "  ~p~n"
       "Valid command line options:~n"
       "  suites=\"foo,bar\" (Run tests in foo.erl, test/foo_tests.erl and~n"
       "                    tests in bar.erl, test/bar_tests.erl)~n"
       "  tests=\"baz\" (For every existing suite, run the first test whose~n"
       "               name starts with bar and, if no such test exists,~n"
       "               run the test whose name starts with bar in the~n"
       "               suite's _tests module)~n",
       [
        Description,
        {eunit_opts, []},
        {eunit_compile_opts, []},
        {eunit_first_files, []},
        {cover_enabled, false},
        {cover_print_enabled, false},
        {cover_export_enabled, false}
       ]).

run_eunit(Config, CodePath, SrcErls) ->
    %% Build a list of all the .beams in ?EUNIT_DIR -- use this for
    %% cover and eunit testing. Normally you can just tell cover
    %% and/or eunit to scan the directory for you, but eunit does a
    %% code:purge in conjunction with that scan and causes any cover
    %% compilation info to be lost.

    AllBeamFiles = rebar_utils:beams(?EUNIT_DIR),
    {BeamFiles, TestBeamFiles} =
        lists:partition(fun(N) -> string:str(N, "_tests.beam") =:= 0 end,
                        AllBeamFiles),
    OtherBeamFiles = TestBeamFiles --
        [filename:rootname(N) ++ "_tests.beam" || N <- AllBeamFiles],
    ModuleBeamFiles = BeamFiles ++ OtherBeamFiles,

    %% Get modules to be run in eunit
    AllModules = [rebar_utils:beam_to_mod(?EUNIT_DIR, N) || N <- AllBeamFiles],
    {SuitesProvided, FilteredModules} = filter_suites(Config, AllModules),

    %% Get matching tests
    Tests = get_tests(Config, SuitesProvided, ModuleBeamFiles, FilteredModules),

    SrcModules = [rebar_utils:erl_to_mod(M) || M <- SrcErls],

    {ok, CoverLog} = cover_init(Config, ModuleBeamFiles),

    StatusBefore = status_before_eunit(),
    EunitResult = perform_eunit(Config, Tests),

    perform_cover(Config, FilteredModules, SrcModules),
    cover_close(CoverLog),

    case proplists:get_value(reset_after_eunit, get_eunit_opts(Config),
                             true) of
        true ->
            reset_after_eunit(StatusBefore);
        false ->
            ok
    end,

    %% Stop cover to clean the cover_server state. This is important if we want
    %% eunit+cover to not slow down when analyzing many Erlang modules.
    ok = cover:stop(),

    case EunitResult of
        ok ->
            ok;
        _ ->
            ?ABORT("One or more eunit tests failed.~n", [])
    end,

    %% Restore code path
    true = code:set_path(CodePath),
    ok.

ensure_dirs() ->
    %% Make sure ?EUNIT_DIR/ and ebin/ directory exists (append dummy module)
    ok = filelib:ensure_dir(filename:join(eunit_dir(), "dummy")),
    ok = filelib:ensure_dir(filename:join(rebar_utils:ebin_dir(), "dummy")).

eunit_dir() ->
    filename:join(rebar_utils:get_cwd(), ?EUNIT_DIR).

setup_code_path() ->
    %% Setup code path prior to compilation so that parse_transforms
    %% and the like work properly. Also, be sure to add ebin_dir()
    %% to the END of the code path so that we don't have to jump
    %% through hoops to access the .app file
    CodePath = code:get_path(),
    true = code:add_patha(eunit_dir()),
    true = code:add_pathz(rebar_utils:ebin_dir()),
    CodePath.

%%
%% == filter suites ==
%%

filter_suites(Config, Modules) ->
    RawSuites = rebar_config:get_global(Config, suites, ""),
    SuitesProvided = RawSuites =/= "",
    Suites = [list_to_atom(Suite) || Suite <- string:tokens(RawSuites, ",")],
    {SuitesProvided, filter_suites1(Modules, Suites)}.

filter_suites1(Modules, []) ->
    Modules;
filter_suites1(Modules, Suites) ->
    [M || M <- Suites, lists:member(M, Modules)].

%%
%% == get matching tests ==
%%
get_tests(Config, SuitesProvided, ModuleBeamFiles, FilteredModules) ->
    Modules = case SuitesProvided of
                  false ->
                      %% No specific suites have been provided, use
                      %% ModuleBeamFiles which filters out "*_tests" modules
                      %% so eunit won't doubly run them and cover only
                      %% calculates coverage on production code. However,
                      %% keep "*_tests" modules that are not automatically
                      %% included by eunit.
                      %%
                      %% From 'Primitives' in the EUnit User's Guide
                      %% http://www.erlang.org/doc/apps/eunit/chapter.html
                      %% "In addition, EUnit will also look for another
                      %% module whose name is ModuleName plus the suffix
                      %% _tests, and if it exists, all the tests from that
                      %% module will also be added. (If ModuleName already
                      %% contains the suffix _tests, this is not done.) E.g.,
                      %% the specification {module, mymodule} will run all
                      %% tests in the modules mymodule and mymodule_tests.
                      %% Typically, the _tests module should only contain
                      %% test cases that use the public interface of the main
                      %% module (and no other code)."
                      [rebar_utils:beam_to_mod(?EUNIT_DIR, N) ||
                          N <- ModuleBeamFiles];
                  true ->
                      %% Specific suites have been provided, return the
                      %% filtered modules
                      FilteredModules
              end,
    get_matching_tests(Config, Modules).

get_matching_tests(Config, Modules) ->
    RawFunctions = rebar_utils:get_experimental_global(Config, tests, ""),
    Tests = [list_to_atom(F1) || F1 <- string:tokens(RawFunctions, ",")],
    case Tests of
        [] ->
            Modules;
        Functions ->
            case get_matching_tests1(Modules, Functions, []) of
                [] ->
                    [];
                RawTests ->
                    make_test_primitives(RawTests)
            end
    end.

get_matching_tests1([], _Functions, TestFunctions) ->
    TestFunctions;

get_matching_tests1([Module|TModules], Functions, TestFunctions) ->
    %% Get module exports
    ModuleStr = atom_to_list(Module),
    ModuleExports = get_beam_test_exports(ModuleStr),
    %% Get module _tests exports
    TestModuleStr = string:concat(ModuleStr, "_tests"),
    TestModuleExports = get_beam_test_exports(TestModuleStr),
    %% Build tests {M, F} list
    Tests = get_matching_tests2(Functions, {Module, ModuleExports},
                                {list_to_atom(TestModuleStr),
                                 TestModuleExports}),
    get_matching_tests1(TModules, Functions,
                        lists:merge([TestFunctions, Tests])).

get_matching_tests2(Functions, {Mod, ModExports}, {TestMod, TestModExports}) ->
    %% Look for matching functions into ModExports
    ModExportsStr = [atom_to_list(E1) || E1 <- ModExports],
    TestModExportsStr = [atom_to_list(E2) || E2 <- TestModExports],
    get_matching_exports(Functions, {Mod, ModExportsStr},
                         {TestMod, TestModExportsStr}, []).

get_matching_exports([], _, _, Matched) ->
    Matched;
get_matching_exports([Function|TFunctions], {Mod, ModExportsStr},
                     {TestMod, TestModExportsStr}, Matched) ->

    FunctionStr = atom_to_list(Function),
    %% Get matching Function in module, otherwise look in _tests module
    NewMatch = case get_matching_export(FunctionStr, ModExportsStr) of
                   [] ->
                       {TestMod, get_matching_export(FunctionStr,
                                                     TestModExportsStr)};
                   MatchingExport ->
                       {Mod, MatchingExport}
               end,
    case NewMatch of
        {_, []} ->
            get_matching_exports(TFunctions, {Mod, ModExportsStr},
                                 {TestMod, TestModExportsStr}, Matched);
        _ ->
            get_matching_exports(TFunctions, {Mod, ModExportsStr},
                                 {TestMod, TestModExportsStr},
                                 [NewMatch|Matched])
    end.

get_matching_export(_FunctionStr, []) ->
    [];
get_matching_export(FunctionStr, [ExportStr|TExportsStr]) ->
    case string:str(ExportStr, FunctionStr) of
        1 ->
            list_to_atom(ExportStr);
        _ ->
            get_matching_export(FunctionStr, TExportsStr)
    end.

get_beam_test_exports(ModuleStr) ->
    FilePath = filename:join(eunit_dir(),
                             string:concat(ModuleStr, ".beam")),
    case filelib:is_regular(FilePath) of
        true ->
            {beam_file, _, Exports0, _, _, _} = beam_disasm:file(FilePath),
            Exports1 = [FunName || {FunName, FunArity, _} <- Exports0,
                                   FunArity =:= 0],
            F = fun(FName) ->
                        FNameStr = atom_to_list(FName),
                        re:run(FNameStr, "_test(_)?") =/= nomatch
                end,
            lists:filter(F, Exports1);
        _ ->
            []
    end.

make_test_primitives(RawTests) ->
    %% Use {test,M,F} and {generator,M,F} if at least R15B02. Otherwise,
    %% use eunit_test:function_wrapper/2 fallback.
    %% eunit_test:function_wrapper/2 was renamed to eunit_test:mf_wrapper/2
    %% in R15B02; use that as >= R15B02 check.
    %% TODO: remove fallback and use only {test,M,F} and {generator,M,F}
    %% primitives once at least R15B02 is required.
    {module, eunit_test} = code:ensure_loaded(eunit_test),
    MakePrimitive = case erlang:function_exported(eunit_test, mf_wrapper, 2) of
                        true  -> fun eunit_primitive/3;
                        false -> fun pre15b02_eunit_primitive/3
                    end,

    ?CONSOLE("    Running test function(s):~n", []),
    F = fun({M, F2}, Acc) ->
                ?CONSOLE("      ~p:~p/0~n", [M, F2]),
                FNameStr = atom_to_list(F2),
                NewFunction =
                    case re:run(FNameStr, "_test_") of
                        nomatch ->
                            %% Normal test
                            MakePrimitive(test, M, F2);
                        _ ->
                            %% Generator
                            MakePrimitive(generator, M, F2)
                    end,
                [NewFunction|Acc]
        end,
    lists:foldl(F, [], RawTests).

eunit_primitive(Type, M, F) ->
    {Type, M, F}.

pre15b02_eunit_primitive(test, M, F) ->
    eunit_test:function_wrapper(M, F);
pre15b02_eunit_primitive(generator, M, F) ->
    {generator, eunit_test:function_wrapper(M, F)}.

%%
%% == run tests ==
%%

perform_eunit(Config, Tests) ->
    EunitOpts = get_eunit_opts(Config),

    %% Move down into ?EUNIT_DIR while we run tests so any generated files
    %% are created there (versus in the source dir)
    Cwd = rebar_utils:get_cwd(),
    ok = file:set_cwd(?EUNIT_DIR),

    EunitResult = (catch eunit:test(Tests, EunitOpts)),

    %% Return to original working dir
    ok = file:set_cwd(Cwd),

    EunitResult.

get_eunit_opts(Config) ->
    %% Enable verbose in eunit if so requested..
    BaseOpts = case rebar_config:is_verbose(Config) of
                   true ->
                       [verbose];
                   false ->
                       []
               end,

    BaseOpts ++ rebar_config:get_list(Config, eunit_opts, []).

%%
%% == code coverage ==
%%

perform_cover(Config, BeamFiles, SrcModules) ->
    perform_cover(rebar_config:get(Config, cover_enabled, false),
                  Config, BeamFiles, SrcModules).

perform_cover(false, _Config, _BeamFiles, _SrcModules) ->
    ok;
perform_cover(true, Config, BeamFiles, SrcModules) ->
    cover_analyze(Config, BeamFiles, SrcModules).

cover_analyze(_Config, [], _SrcModules) ->
    ok;
cover_analyze(Config, FilteredModules, SrcModules) ->
    %% Generate coverage info for all the cover-compiled modules
    Coverage = lists:flatten([cover_analyze_mod(M)
                              || M <- FilteredModules,
                                 cover:is_compiled(M) =/= false]),

    %% Write index of coverage info
    cover_write_index(lists:sort(Coverage), SrcModules),

    %% Write coverage details for each file
    lists:foreach(fun({M, _, _}) ->
                          {ok, _} = cover:analyze_to_file(M, cover_file(M),
                                                          [html])
                  end, Coverage),

    Index = filename:join([rebar_utils:get_cwd(), ?EUNIT_DIR, "index.html"]),
    ?CONSOLE("Cover analysis: ~s\n", [Index]),

    %% Export coverage data, if configured
    case rebar_config:get(Config, cover_export_enabled, false) of
        true ->
            cover_export_coverdata();
        false ->
            ok
    end,

    %% Print coverage report, if configured
    case rebar_config:get(Config, cover_print_enabled, false) of
        true ->
            cover_print_coverage(lists:sort(Coverage));
        false ->
            ok
    end.

cover_close(not_enabled) ->
    ok;
cover_close(F) ->
    ok = file:close(F).

cover_init(false, _BeamFiles) ->
    {ok, not_enabled};
cover_init(true, BeamFiles) ->
    %% Attempt to start the cover server, then set its group leader to
    %% .eunit/cover.log, so all cover log messages will go there instead of
    %% to stdout. If the cover server is already started, we'll kill that
    %% server and start a new one in order not to inherit a polluted
    %% cover_server state.
    {ok, CoverPid} = case whereis(cover_server) of
                         undefined ->
                             cover:start();
                         _         ->
                             cover:stop(),
                             cover:start()
                     end,

    {ok, F} = OkOpen = file:open(
                         filename:join([?EUNIT_DIR, "cover.log"]),
                         [write]),

    group_leader(F, CoverPid),

    ?INFO("Cover compiling ~s\n", [rebar_utils:get_cwd()]),

    Compiled = [{Beam, cover:compile_beam(Beam)} || Beam <- BeamFiles],
    case [Module || {_, {ok, Module}} <- Compiled] of
        [] ->
            %% No modules compiled successfully...fail
            ?ERROR("Cover failed to compile any modules; aborting.~n", []),
            ?FAIL;
        _ ->
            %% At least one module compiled successfully

            %% It's not an error for cover compilation to fail partially,
            %% but we do want to warn about them
            PrintWarning =
                fun(Beam, Desc) ->
                        ?CONSOLE("Cover compilation warning for ~p: ~p",
                                 [Beam, Desc])
                end,
            _ = [PrintWarning(Beam, Desc) || {Beam, {error, Desc}} <- Compiled],
            OkOpen
    end;
cover_init(Config, BeamFiles) ->
    cover_init(rebar_config:get(Config, cover_enabled, false), BeamFiles).

cover_analyze_mod(Module) ->
    case cover:analyze(Module, coverage, module) of
        {ok, {Module, {Covered, NotCovered}}} ->
            %% Modules that include the eunit header get an implicit
            %% test/0 fun, which cover considers a runnable line, but
            %% eunit:test(TestRepresentation) never calls.  Decrement
            %% NotCovered in this case.
            [align_notcovered_count(Module, Covered, NotCovered,
                                    is_eunitized(Module))];
        {error, Reason} ->
            ?ERROR("Cover analyze failed for ~p: ~p ~p\n",
                   [Module, Reason, code:which(Module)]),
            []
    end.

is_eunitized(Mod) ->
    has_eunit_test_fun(Mod) andalso
        has_header(Mod, "include/eunit.hrl").

has_eunit_test_fun(Mod) ->
    [F || {exports, Funs} <- Mod:module_info(),
          {F, 0} <- Funs, F =:= test] =/= [].

has_header(Mod, Header) ->
    Mod1 = case code:which(Mod) of
               cover_compiled ->
                   {file, File} = cover:is_compiled(Mod),
                   File;
               non_existing -> Mod;
               preloaded -> Mod;
               L -> L
           end,
    {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(Mod1,
                                                            [abstract_code]),
    [F || {attribute, 1, file, {F, 1}} <- AC,
          string:str(F, Header) =/= 0] =/= [].

align_notcovered_count(Module, Covered, NotCovered, false) ->
    {Module, Covered, NotCovered};
align_notcovered_count(Module, Covered, NotCovered, true) ->
    {Module, Covered, NotCovered - 1}.

cover_write_index(Coverage, SrcModules) ->
    {ok, F} = file:open(filename:join([?EUNIT_DIR, "index.html"]), [write]),
    ok = file:write(F, "<!DOCTYPE HTML><html>\n"
                        "<head><meta charset=\"utf-8\">"
                        "<title>Coverage Summary</title></head>\n"
                        "<body>\n"),
    IsSrcCoverage = fun({Mod,_C,_N}) -> lists:member(Mod, SrcModules) end,
    {SrcCoverage, TestCoverage} = lists:partition(IsSrcCoverage, Coverage),
    cover_write_index_section(F, "Source", SrcCoverage),
    cover_write_index_section(F, "Test", TestCoverage),
    ok = file:write(F, "</body></html>"),
    ok = file:close(F).

cover_write_index_section(_F, _SectionName, []) ->
    ok;
cover_write_index_section(F, SectionName, Coverage) ->
    %% Calculate total coverage
    {Covered, NotCovered} = lists:foldl(fun({_Mod, C, N}, {CAcc, NAcc}) ->
                                                {CAcc + C, NAcc + N}
                                        end, {0, 0}, Coverage),
    TotalCoverage = percentage(Covered, NotCovered),

    %% Write the report
    ok = file:write(F, ?FMT("<h1>~s Summary</h1>\n", [SectionName])),
    ok = file:write(F, ?FMT("<h3>Total: ~s</h3>\n", [TotalCoverage])),
    ok = file:write(F, "<table><tr><th>Module</th><th>Coverage %</th></tr>\n"),

    FmtLink =
        fun(Module, Cov, NotCov) ->
                ?FMT("<tr><td><a href='~s.COVER.html'>~s</a></td><td>~s</td>\n",
                     [Module, Module, percentage(Cov, NotCov)])
        end,
    lists:foreach(fun({Module, Cov, NotCov}) ->
                          ok = file:write(F, FmtLink(Module, Cov, NotCov))
                  end, Coverage),
    ok = file:write(F, "</table>\n").

cover_print_coverage(Coverage) ->
    {Covered, NotCovered} = lists:foldl(fun({_Mod, C, N}, {CAcc, NAcc}) ->
                                                {CAcc + C, NAcc + N}
                                        end, {0, 0}, Coverage),
    TotalCoverage = percentage(Covered, NotCovered),

    %% Determine the longest module name for right-padding
    Width = lists:foldl(fun({Mod, _, _}, Acc) ->
                                case length(atom_to_list(Mod)) of
                                    N when N > Acc ->
                                        N;
                                    _ ->
                                        Acc
                                end
                        end, 0, Coverage) * -1,

    %% Print the output the console
    ?CONSOLE("~nCode Coverage:~n", []),
    lists:foreach(fun({Mod, C, N}) ->
                          ?CONSOLE("~*s : ~3s~n",
                                   [Width, Mod, percentage(C, N)])
                  end, Coverage),
    ?CONSOLE("~n~*s : ~s~n", [Width, "Total", TotalCoverage]).

cover_file(Module) ->
    filename:join([?EUNIT_DIR, atom_to_list(Module) ++ ".COVER.html"]).

cover_export_coverdata() ->
    ExportFile = filename:join(eunit_dir(), "eunit.coverdata"),
    case cover:export(ExportFile) of
        ok ->
            ?CONSOLE("Coverdata export: ~s~n", [ExportFile]);
        {error, Reason} ->
            ?ERROR("Coverdata export failed: ~p~n", [Reason])
    end.

percentage(0, 0) ->
    "not executed";
percentage(Cov, NotCov) ->
    integer_to_list(trunc((Cov / (Cov + NotCov)) * 100)) ++ "%".

%%
%% == reset_after_eunit ==
%%

status_before_eunit() ->
    Apps = get_app_names(),
    AppEnvs = [{App, application:get_all_env(App)} || App <- Apps],
    {erlang:processes(), erlang:is_alive(), AppEnvs, ets:tab2list(ac_tab)}.

get_app_names() ->
    [AppName || {AppName, _, _} <- application:loaded_applications()].

reset_after_eunit({OldProcesses, WasAlive, OldAppEnvs, _OldACs}) ->
    IsAlive = erlang:is_alive(),
    if not WasAlive andalso IsAlive ->
            ?DEBUG("Stopping net kernel....\n", []),
            erl_epmd:stop(),
            _ = net_kernel:stop(),
            pause_until_net_kernel_stopped();
       true ->
            ok
    end,

    OldApps = [App || {App, _} <- OldAppEnvs],
    Apps = get_app_names(),
    _ = [begin
             _ = case lists:member(App, OldApps) of
                     true  -> ok;
                     false -> application:stop(App)
                 end,
             ok = application:unset_env(App, K)
         end || App <- Apps, App /= rebar,
                {K, _V} <- application:get_all_env(App),
                K =/= included_applications],

    reconstruct_app_env_vars(Apps),

    Processes = erlang:processes(),
    _ = kill_extras(Processes -- OldProcesses),

    ok.

kill_extras(Pids) ->
    %% Killing any of the procs below will either:
    %% 1. Interfere with stuff that we don't want interfered with, or
    %% 2. May/will force the 'kernel' app to shutdown, which *will*
    %%    interfere with rebar's ability To Do Useful Stuff(tm).
    %% This list may require changes as OTP versions and/or
    %% rebar use cases change.
    KeepProcs = [cover_server, eunit_server,
                 eqc, eqc_license, eqc_locked,
                 %% inet_gethost_native is started on demand, when
                 %% doing name lookups. It is under kernel_sup, under
                 %% a supervisor_bridge.
                 inet_gethost_native],
    Killed = [begin
                  Info = case erlang:process_info(Pid) of
                             undefined -> [];
                             Else      -> Else
                         end,
                  Keep1 = case proplists:get_value(registered_name, Info) of
                              undefined ->
                                  false;
                              Name ->
                                  lists:member(Name, KeepProcs)
                          end,
                  Keep2 = case proplists:get_value(dictionary, Info) of
                              undefined ->
                                  false;
                              Ds ->
                                  case proplists:get_value('$ancestors', Ds) of
                                      undefined ->
                                          false;
                                      As ->
                                          lists:member(kernel_sup, As)
                                  end
                          end,
                  if Keep1 orelse Keep2 ->
                          ok;
                     true ->
                          ?DEBUG("Kill ~p ~p\n", [Pid, Info]),
                          exit(Pid, kill),
                          Pid
                  end
              end || Pid <- Pids],
    case lists:usort(Killed) -- [ok] of
        [] ->
            ?DEBUG("No processes to kill\n", []),
            [];
        Else ->
            lists:foreach(fun(Pid) -> wait_until_dead(Pid) end, Else),
            Else
    end.

reconstruct_app_env_vars([App|Apps]) ->
    CmdLine0 = proplists:get_value(App, init:get_arguments(), []),
    CmdVars = [{list_to_atom(K), list_to_atom(V)} || {K, V} <- CmdLine0],
    AppFile = (catch filename:join([code:lib_dir(App),
                                    "ebin",
                                    atom_to_list(App) ++ ".app"])),
    AppVars = case file:consult(AppFile) of
                  {ok, [{application, App, Ps}]} ->
                      proplists:get_value(env, Ps, []);
                  _ ->
                      []
              end,

    %% App vars specified in config files override those in the .app file.
    %% Config files later in the args list override earlier ones.
    AppVars1 = case init:get_argument(config) of
                   {ok, ConfigFiles} ->
                       {App, MergedAppVars} = lists:foldl(fun merge_app_vars/2,
                                                          {App, AppVars},
                                                          ConfigFiles),
                       MergedAppVars;
                   error ->
                       AppVars
               end,
    AllVars = CmdVars ++ AppVars1,
    ?DEBUG("Reconstruct ~p ~p\n", [App, AllVars]),
    lists:foreach(fun({K, V}) -> application:set_env(App, K, V) end, AllVars),
    reconstruct_app_env_vars(Apps);
reconstruct_app_env_vars([]) ->
    ok.

merge_app_vars(ConfigFile, {App, AppVars}) ->
    File = ensure_config_extension(ConfigFile),
    FileAppVars = app_vars_from_config_file(File, App),
    Dict1 = dict:from_list(AppVars),
    Dict2 = dict:from_list(FileAppVars),
    Dict3 = dict:merge(fun(_Key, _Value1, Value2) -> Value2 end, Dict1, Dict2),
    {App, dict:to_list(Dict3)}.

ensure_config_extension(File) ->
    %% config files must end with .config on disk but when specifying them
    %% via the -config option the extension is optional
    BaseFileName = filename:basename(File, ".config"),
    DirName = filename:dirname(File),
    filename:join(DirName, BaseFileName ++ ".config").

app_vars_from_config_file(File, App) ->
    case file:consult(File) of
        {ok, [Env]} ->
            proplists:get_value(App, Env, []);
        _ ->
            []
    end.

wait_until_dead(Pid) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, _Obj, Info} ->
            Info
    after 10*1000 ->
            exit({timeout_waiting_for, Pid})
    end;
wait_until_dead(_) ->
    ok.

pause_until_net_kernel_stopped() ->
    pause_until_net_kernel_stopped(10).

pause_until_net_kernel_stopped(0) ->
    exit(net_kernel_stop_failed);
pause_until_net_kernel_stopped(N) ->
    case node() of
        'nonode@nohost' ->
            ?DEBUG("Stopped net kernel.\n", []),
            ok;
        _ ->
            timer:sleep(100),
            pause_until_net_kernel_stopped(N - 1)
    end.
