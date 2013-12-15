-module(rebar_xref_eunit).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(REBAR_SCRIPT, "../rebar").

-define(TMP_DIR, "tmp_xref_eunit/").

xref_test_() ->
    {"Test the various xref warnings",
     setup, fun() -> setup_project(false), rebar("compile"), rebar("skip_deps=true xref") end,
     fun teardown/1,
     fun(RebarOut) ->
             [
              {"Undefined function", ?_assert(string:str(RebarOut, 
                "myapp_somemod:notavailable/1 is undefined function") =/= 0)},
              {"Undefined function call", ?_assert(string:str(RebarOut, 
                "myapp_othermod:somefunc/0 calls undefined function myapp_somemod:notavailable/1") =/= 0)},
              {"Deprecated function", ?_assert(string:str(RebarOut, 
                "myapp_mymod:fdeprecated/0 is deprecated function") =/= 0)},
              {"Deprecated function call", ?_assert(string:str(RebarOut, 
                "myapp_othermod:somefunc/0 calls deprecated function myapp_mymod:fdeprecated/0") =/= 0)},
              {"Unused local", ?_assert(string:str(RebarOut, 
                "myapp_mymod:localfunc2/0 is unused local function") =/= 0)},
              {"Unused export 1", ?_assert(string:str(RebarOut, 
                "myapp_behaviour1:behaviour_info/1 is unused export") =/= 0)},
              {"Unused export 2", ?_assert(string:str(RebarOut, 
                "myapp_behaviour2:behaviour_info/1 is unused export") =/= 0)},
              {"Unused export 3", ?_assert(string:str(RebarOut, 
                "myapp_mymod:other2/1 is unused export") =/= 0)},
              {"Unused export 4", ?_assert(string:str(RebarOut, 
                "myapp_othermod:somefunc/0 is unused export") =/= 0)},
              {"Suppressed behaviour export 1", ?_assert(string:str(RebarOut, 
                "myapp_mymod:bh1_a/1 is unused export") =:= 0)},
              {"Suppressed behaviour export 2", ?_assert(string:str(RebarOut, 
                "myapp_mymod:bh1_b/1 is unused export") =:= 0)},
              {"Suppressed behaviour export 3", ?_assert(string:str(RebarOut, 
                "myapp_mymod:bh2_a/1 is unused export") =:= 0)},
              {"Suppressed behaviour export 4", ?_assert(string:str(RebarOut, 
                "myapp_mymod:bh2_b/1 is unused export") =:= 0)}
            ]
     end}.

xref_ignore_test_() ->
    {"Test the suppression of xref warnings",
     setup, fun() -> setup_project(ignore_xref), rebar("compile"), rebar("skip_deps=true xref") end,
     fun teardown/1,
     fun(RebarOut) ->
             [
              {"Undefined function can not be suppressed.", ?_assert(string:str(RebarOut, 
                "myapp_somemod:notavailable/1 is undefined function") =/= 0)},
              {"Supppressed undefined function call", ?_assert(string:str(RebarOut, 
                "myapp_othermod:somefunc/0 calls undefined function myapp_somemod:notavailable/1") =:= 0)},
              {"Supppressed deprecated function", ?_assert(string:str(RebarOut, 
                "myapp_mymod:fdeprecated/0 is deprecated function") =:= 0)},
              {"Supppressed deprecated function call", ?_assert(string:str(RebarOut, 
                "myapp_othermod:somefunc/0 calls deprecated function myapp_mymod:fdeprecated/0") =:= 0)},
              {"Supppressed unused local", ?_assert(string:str(RebarOut, 
                "myapp_mymod:localfunc2/0 is unused local function") =:= 0)},
              {"Supppressed unused export 1", ?_assert(string:str(RebarOut, 
                "myapp_behaviour1:behaviour_info/1 is unused export") =:= 0)},
              {"Supppressed unused export 2", ?_assert(string:str(RebarOut, 
                "myapp_behaviour2:behaviour_info/1 is unused export") =:= 0)},
              {"Supppressed unused export 3", ?_assert(string:str(RebarOut, 
                "myapp_mymod:other2/1 is unused export") =:= 0)},
              {"Supppressed unused export 4", ?_assert(string:str(RebarOut, 
                "myapp_othermod:somefunc/0 is unused export") =:= 0)},
              {"Suppressed behaviour export 1", ?_assert(string:str(RebarOut, 
                "myapp_mymod:bh1_a/1 is unused export") =:= 0)},
              {"Suppressed behaviour export 2", ?_assert(string:str(RebarOut, 
                "myapp_mymod:bh1_b/1 is unused export") =:= 0)},
              {"Suppressed behaviour export 3", ?_assert(string:str(RebarOut, 
                "myapp_mymod:bh2_a/1 is unused export") =:= 0)},
              {"Suppressed behaviour export 4", ?_assert(string:str(RebarOut, 
                "myapp_mymod:bh2_b/1 is unused export") =:= 0)}
            ]

     end}.


%% ====================================================================
%% Setup and Teardown
%% ====================================================================

-define(myapp_behaviour1,
        ["-module(myapp_behaviour1).\n",
         "-export([behaviour_info/1]).\n"]).
-define(myapp_behaviour1_body,
        ["behaviour_info(callbacks) -> [{bh1_a,1},{bh1_b,1}];\n",
         "behaviour_info(_Other) -> undefined.\n"]).
-define(myapp_behaviour1_ignorexref,
        ["-ignore_xref({behaviour_info,1}).\n"]).

-define(myapp_behaviour2,
        ["-module(myapp_behaviour2).\n",
         "-export([behaviour_info/1]).\n"]).
-define(myapp_behaviour2_body,
        ["behaviour_info(callbacks) -> [{bh2_a,1},{bh2_b,1}];\n",
         "behaviour_info(_Other) -> undefined.\n"]).
-define(myapp_behaviour2_ignorexref,
        ["-ignore_xref({behaviour_info,1}).\n"]).

-define(myapp_mymod,
        ["-module(myapp_mymod).\n",
         "-export([bh1_a/1,bh1_b/1,bh2_a/1,bh2_b/1,other1/1,other2/1,fdeprecated/0]).\n",
         "-behaviour(myapp_behaviour1).\n",     % 2 behaviours
         "-behaviour(myapp_behaviour2).\n",
         "-deprecated({fdeprecated,0}).\n"]).      % deprecated function
-define(myapp_mymod_body,
        ["bh1_a(A) -> localfunc1(bh1_a, A).\n", % behaviour functions
         "bh1_b(A) -> localfunc1(bh1_b, A).\n",
         "bh2_a(A) -> localfunc1(bh2_a, A).\n",
         "bh2_b(A) -> localfunc1(bh2_b, A).\n",
         "other1(A) -> localfunc1(other1, A).\n", % regular exported functions
         "other2(A) -> localfunc1(other2, A).\n", 
         "localfunc1(A, B) -> {A, B}.\n",       % used local
         "localfunc2() -> ok.\n",               % unused local
         "fdeprecated() -> ok.\n"              % deprecated function
         ]).
-define(myapp_mymod_ignorexref,
        ["-ignore_xref([{other2,1},{localfunc2,0},{fdeprecated,0}]).\n"]).



-define(myapp_othermod,
        ["-module(myapp_othermod).\n",
         "-export([somefunc/0]).\n"]).
-define(myapp_othermod_body,
        ["somefunc() ->\n",
         "   myapp_mymod:other1(arg),\n",
         "   myapp_somemod:notavailable(arg),\n",
         "   myapp_mymod:fdeprecated().\n"
         ]).
-define(myapp_othermod_ignorexref,
        ["-ignore_xref([{myapp_somemod,notavailable,1},{somefunc,0}]).\n",
         "-ignore_xref({myapp_mymod,fdeprecated,0}).\n"]).


-define(myapp_rebarconfig,
        ["{erl_opts, [debug_info]}.\n",
         "{xref_checks, [deprecated_function_calls,deprecated_functions,\n",
         "               undefined_function_calls,undefined_functions,\n",
         "               exports_not_used,locals_not_used]}.\n"
         ]).

setup_environment() ->
    ok = file:make_dir(?TMP_DIR),
    prepare_rebar_script(),
    ok = file:set_cwd(?TMP_DIR).

prepare_project() ->
    setup_environment(),
    rebar("create-app appid=myapp"),
    ok = file:make_dir("ebin").  

setup_project(ignore_xref) ->
    prepare_project(),
    ok = file:write_file("src/myapp_behaviour1.erl", ?myapp_behaviour1 ++ ?myapp_behaviour1_ignorexref ++ ?myapp_behaviour1_body),
    ok = file:write_file("src/myapp_behaviour2.erl", ?myapp_behaviour2 ++ ?myapp_behaviour2_ignorexref++ ?myapp_behaviour2_body),
    ok = file:write_file("src/myapp_mymod.erl", ?myapp_mymod ++ ?myapp_mymod_ignorexref ++ ?myapp_mymod_body),
    ok = file:write_file("src/myapp_othermod.erl", ?myapp_othermod ++ ?myapp_othermod_ignorexref ++ ?myapp_othermod_body),
    ok = file:write_file("rebar.config", ?myapp_rebarconfig);

setup_project(_) ->
    prepare_project(),
    ok = file:write_file("src/myapp_behaviour1.erl", ?myapp_behaviour1 ++ ?myapp_behaviour1_body),
    ok = file:write_file("src/myapp_behaviour2.erl", ?myapp_behaviour2 ++ ?myapp_behaviour2_body),
    ok = file:write_file("src/myapp_mymod.erl", ?myapp_mymod ++ ?myapp_mymod_body),
    ok = file:write_file("src/myapp_othermod.erl", ?myapp_othermod ++ ?myapp_othermod_body),
    ok = file:write_file("rebar.config", ?myapp_rebarconfig).


teardown(_) ->
    ok = file:set_cwd(".."),
    ok = remove_tmp_dir().

remove_tmp_dir() ->
    ok = rebar_file_utils:rm_rf(?TMP_DIR).

%% ====================================================================
%% Helper Functions
%% ====================================================================

prepare_rebar_script() ->
    Rebar = ?TMP_DIR ++ "rebar",
    {ok, _} = file:copy(?REBAR_SCRIPT, Rebar),
    case os:type() of
        {unix, _} ->
            [] = os:cmd("chmod u+x " ++ Rebar);
        {win32, _} ->
            {ok, _} = file:copy(?REBAR_SCRIPT ++ ".bat",
                                ?TMP_DIR ++ "rebar.bat")
    end.

rebar() ->
    rebar([]).

rebar(Args) when is_list(Args) ->
    Out = os:cmd(filename:nativename("./rebar") ++ " " ++ Args),
    %% ?debugMsg("**** Begin"), ?debugMsg(Out), ?debugMsg("**** End"),
    Out.
