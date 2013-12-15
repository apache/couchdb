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
-module(rebar_erlc_compiler).

-export([compile/2,
         clean/2]).

%% for internal use only
-export([test_compile/3,
         info/2]).

-include("rebar.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

%% Supported configuration variables:
%%
%% * erl_opts - Erlang list of options passed to compile:file/2
%%              It is also possible to specify platform specific
%%              options by specifying a pair or a triplet where the
%%              first string is a regex that is checked against the
%%              string
%%
%%                OtpRelease ++ "-" ++ SysArch ++ "-" ++ Words.
%%
%%              where
%%
%%                OtpRelease = erlang:system_info(otp_release).
%%                SysArch = erlang:system_info(system_architecture).
%%                Words = integer_to_list(8 *
%%                            erlang:system_info({wordsize, external})).
%%
%%              E.g. to define HAVE_SENDFILE only on systems with
%%              sendfile(), to define BACKLOG on Linux/FreeBSD as 128,
%%              and to define 'old_inets' for R13 OTP release do:
%%
%%              {erl_opts, [{platform_define,
%%                           "(linux|solaris|freebsd|darwin)",
%%                           'HAVE_SENDFILE'},
%%                          {platform_define, "(linux|freebsd)",
%%                           'BACKLOG', 128},
%%                          {platform_define, "R13",
%%                           'old_inets'}]}.
%%

-spec compile(rebar_config:config(), file:filename()) -> 'ok'.
compile(Config, _AppFile) ->
    rebar_base_compiler:run(Config,
                            check_files(rebar_config:get_local(
                                          Config, xrl_first_files, [])),
                            "src", ".xrl", "src", ".erl",
                            fun compile_xrl/3),
    rebar_base_compiler:run(Config,
                            check_files(rebar_config:get_local(
                                          Config, yrl_first_files, [])),
                            "src", ".yrl", "src", ".erl",
                            fun compile_yrl/3),
    rebar_base_compiler:run(Config,
                            check_files(rebar_config:get_local(
                                          Config, mib_first_files, [])),
                            "mibs", ".mib", "priv/mibs", ".bin",
                            fun compile_mib/3),
    doterl_compile(Config, "ebin").

-spec clean(rebar_config:config(), file:filename()) -> 'ok'.
clean(_Config, _AppFile) ->
    MibFiles = rebar_utils:find_files("mibs", "^.*\\.mib\$"),
    MIBs = [filename:rootname(filename:basename(MIB)) || MIB <- MibFiles],
    rebar_file_utils:delete_each(
      [filename:join(["include",MIB++".hrl"]) || MIB <- MIBs]),
    lists:foreach(fun(F) -> ok = rebar_file_utils:rm_rf(F) end,
                  ["ebin/*.beam", "priv/mibs/*.bin"]),

    YrlFiles = rebar_utils:find_files("src", "^.*\\.[x|y]rl\$"),
    rebar_file_utils:delete_each(
      [ binary_to_list(iolist_to_binary(re:replace(F, "\\.[x|y]rl$", ".erl")))
        || F <- YrlFiles ]),

    %% Erlang compilation is recursive, so it's possible that we have a nested
    %% directory structure in ebin with .beam files within. As such, we want
    %% to scan whatever is left in the ebin/ directory for sub-dirs which
    %% satisfy our criteria.
    BeamFiles = rebar_utils:find_files("ebin", "^.*\\.beam\$"),
    rebar_file_utils:delete_each(BeamFiles),
    lists:foreach(fun(Dir) -> delete_dir(Dir, dirs(Dir)) end, dirs("ebin")),
    ok.

%% ===================================================================
%% .erl Compilation API (externally used by only eunit and qc)
%% ===================================================================

test_compile(Config, Cmd, OutDir) ->
    %% Obtain all the test modules for inclusion in the compile stage.
    TestErls = rebar_utils:find_files("test", ".*\\.erl\$"),

    %% Copy source files to eunit dir for cover in case they are not directly
    %% in src but in a subdirectory of src. Cover only looks in cwd and ../src
    %% for source files. Also copy files from src_dirs.
    ErlOpts = rebar_utils:erl_opts(Config),

    SrcDirs = rebar_utils:src_dirs(proplists:append_values(src_dirs, ErlOpts)),
    SrcErls = lists:foldl(
                fun(Dir, Acc) ->
                        Files = rebar_utils:find_files(Dir, ".*\\.erl\$"),
                        lists:append(Acc, Files)
                end, [], SrcDirs),

    %% If it is not the first time rebar eunit is executed, there will be source
    %% files already present in OutDir. Since some SCMs (like Perforce) set
    %% the source files as being read only (unless they are checked out), we
    %% need to be sure that the files already present in OutDir are writable
    %% before doing the copy. This is done here by removing any file that was
    %% already present before calling rebar_file_utils:cp_r.

    %% Get the full path to a file that was previously copied in OutDir
    ToCleanUp = fun(F, Acc) ->
                        F2 = filename:basename(F),
                        F3 = filename:join([OutDir, F2]),
                        case filelib:is_regular(F3) of
                            true -> [F3|Acc];
                            false -> Acc
                        end
                end,

    ok = rebar_file_utils:delete_each(lists:foldl(ToCleanUp, [], TestErls)),
    ok = rebar_file_utils:delete_each(lists:foldl(ToCleanUp, [], SrcErls)),

    ok = rebar_file_utils:cp_r(SrcErls ++ TestErls, OutDir),

    %% Compile erlang code to OutDir, using a tweaked config
    %% with appropriate defines for eunit, and include all the test modules
    %% as well.
    ok = doterl_compile(test_compile_config(Config, ErlOpts, Cmd),
                        OutDir, TestErls),

    {ok, SrcErls}.

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, compile) ->
    info_help("Build *.erl, *.yrl, *.xrl, and *.mib sources");
info(help, clean) ->
    info_help("Delete *.erl, *.yrl, *.xrl, and *.mib build results").

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
       "  ~p~n"
       "  ~p~n",
       [
        Description,
        {erl_opts, [no_debug_info,
                    {i, "myinclude"},
                    {src_dirs, ["src", "src2", "src3"]},
                    {platform_define,
                     "(linux|solaris|freebsd|darwin)", 'HAVE_SENDFILE'},
                    {platform_define, "(linux|freebsd)", 'BACKLOG', 128},
                    {platform_define, "R13", 'old_inets'}]},
        {erl_first_files, ["mymib1", "mymib2"]},
        {mib_opts, []},
        {mib_first_files, []},
        {xrl_opts, []},
        {xrl_first_files, []},
        {yrl_opts, []},
        {yrl_first_files, []}
       ]).

test_compile_config(Config, ErlOpts, Cmd) ->
    {Config1, TriqOpts} = triq_opts(Config),
    {Config2, PropErOpts} = proper_opts(Config1),
    {Config3, EqcOpts} = eqc_opts(Config2),

    OptsAtom = list_to_atom(Cmd ++ "_compile_opts"),
    EunitOpts = rebar_config:get_list(Config3, OptsAtom, []),
    Opts0 = [{d, 'TEST'}] ++
        ErlOpts ++ EunitOpts ++ TriqOpts ++ PropErOpts ++ EqcOpts,
    Opts = [O || O <- Opts0, O =/= no_debug_info],
    Config4 = rebar_config:set(Config3, erl_opts, Opts),

    FirstFilesAtom = list_to_atom(Cmd ++ "_first_files"),
    FirstErls = rebar_config:get_list(Config4, FirstFilesAtom, []),
    rebar_config:set(Config4, erl_first_files, FirstErls).

triq_opts(Config) ->
    {NewConfig, IsAvail} = is_lib_avail(Config, is_triq_avail, triq,
                                        "triq.hrl", "Triq"),
    Opts = define_if('TRIQ', IsAvail),
    {NewConfig, Opts}.

proper_opts(Config) ->
    {NewConfig, IsAvail} = is_lib_avail(Config, is_proper_avail, proper,
                                        "proper.hrl", "PropEr"),
    Opts = define_if('PROPER', IsAvail),
    {NewConfig, Opts}.

eqc_opts(Config) ->
    {NewConfig, IsAvail} = is_lib_avail(Config, is_eqc_avail, eqc,
                                        "eqc.hrl", "QuickCheck"),
    Opts = define_if('EQC', IsAvail),
    {NewConfig, Opts}.

define_if(Def, true) -> [{d, Def}];
define_if(_Def, false) -> [].

is_lib_avail(Config, DictKey, Mod, Hrl, Name) ->
    case rebar_config:get_xconf(Config, DictKey, undefined) of
        undefined ->
            IsAvail = case code:lib_dir(Mod, include) of
                          {error, bad_name} ->
                              false;
                          Dir ->
                              filelib:is_regular(filename:join(Dir, Hrl))
                      end,
            NewConfig = rebar_config:set_xconf(Config, DictKey, IsAvail),
            ?DEBUG("~s availability: ~p\n", [Name, IsAvail]),
            {NewConfig, IsAvail};
        IsAvail ->
            {Config, IsAvail}
    end.

-spec doterl_compile(rebar_config:config(), file:filename()) -> 'ok'.
doterl_compile(Config, OutDir) ->
    doterl_compile(Config, OutDir, []).

doterl_compile(Config, OutDir, MoreSources) ->
    FirstErls = rebar_config:get_list(Config, erl_first_files, []),
    ErlOpts = rebar_utils:erl_opts(Config),
    ?DEBUG("erl_opts ~p~n", [ErlOpts]),
    %% Support the src_dirs option allowing multiple directories to
    %% contain erlang source. This might be used, for example, should
    %% eunit tests be separated from the core application source.
    SrcDirs = rebar_utils:src_dirs(proplists:append_values(src_dirs, ErlOpts)),
    RestErls  = [Source || Source <- gather_src(SrcDirs, []) ++ MoreSources,
                           not lists:member(Source, FirstErls)],

    %% Split RestErls so that parse_transforms and behaviours are instead added
    %% to erl_first_files, parse transforms first.
    %% This should probably be somewhat combined with inspect_epp
    [ParseTransforms, Behaviours, OtherErls] =
        lists:foldl(fun(F, [A, B, C]) ->
                            case compile_priority(F) of
                                parse_transform ->
                                    [[F | A], B, C];
                                behaviour ->
                                    [A, [F | B], C];
                                callback ->
                                    [A, [F | B], C];
                                _ ->
                                    [A, B, [F | C]]
                            end
                    end, [[], [], []], RestErls),

    NewFirstErls = FirstErls ++ ParseTransforms ++ Behaviours,

    %% Make sure that ebin/ exists and is on the path
    ok = filelib:ensure_dir(filename:join("ebin", "dummy.beam")),
    CurrPath = code:get_path(),
    true = code:add_path(filename:absname("ebin")),
    OutDir1 = proplists:get_value(outdir, ErlOpts, OutDir),
    rebar_base_compiler:run(Config, NewFirstErls, OtherErls,
                            fun(S, C) ->
                                    internal_erl_compile(C, S, OutDir1, ErlOpts)
                            end),
    true = code:set_path(CurrPath),
    ok.

-spec include_path(file:filename(),
                   rebar_config:config()) -> [file:filename(), ...].
include_path(Source, Config) ->
    ErlOpts = rebar_config:get(Config, erl_opts, []),
    ["include", filename:dirname(Source)]
        ++ proplists:get_all_values(i, ErlOpts).

-spec inspect(file:filename(),
              [file:filename(), ...]) -> {string(), [string()]}.
inspect(Source, IncludePath) ->
    ModuleDefault = filename:basename(Source, ".erl"),
    case epp:open(Source, IncludePath) of
        {ok, Epp} ->
            inspect_epp(Epp, Source, ModuleDefault, []);
        {error, Reason} ->
            ?DEBUG("Failed to inspect ~s: ~p\n", [Source, Reason]),
            {ModuleDefault, []}
    end.

-spec inspect_epp(pid(), file:filename(), file:filename(),
                  [string()]) -> {string(), [string()]}.
inspect_epp(Epp, Source, Module, Includes) ->
    case epp:parse_erl_form(Epp) of
        {ok, {attribute, _, module, ModInfo}} ->
            ActualModuleStr =
                case ModInfo of
                    %% Typical module name, single atom
                    ActualModule when is_atom(ActualModule) ->
                        atom_to_list(ActualModule);
                    %% Packag-ized module name, list of atoms
                    ActualModule when is_list(ActualModule) ->
                        string:join([atom_to_list(P) ||
                                        P <- ActualModule], ".");
                    %% Parameterized module name, single atom
                    {ActualModule, _} when is_atom(ActualModule) ->
                        atom_to_list(ActualModule);
                    %% Parameterized and packagized module name, list of atoms
                    {ActualModule, _} when is_list(ActualModule) ->
                        string:join([atom_to_list(P) ||
                                        P <- ActualModule], ".")
                end,
            inspect_epp(Epp, Source, ActualModuleStr, Includes);
        {ok, {attribute, 1, file, {Module, 1}}} ->
            inspect_epp(Epp, Source, Module, Includes);
        {ok, {attribute, 1, file, {Source, 1}}} ->
            inspect_epp(Epp, Source, Module, Includes);
        {ok, {attribute, 1, file, {IncFile, 1}}} ->
            inspect_epp(Epp, Source, Module, [IncFile | Includes]);
        {eof, _} ->
            epp:close(Epp),
            {Module, Includes};
        _ ->
            inspect_epp(Epp, Source, Module, Includes)
    end.

-spec needs_compile(file:filename(), file:filename(),
                    [string()]) -> boolean().
needs_compile(Source, Target, Hrls) ->
    TargetLastMod = filelib:last_modified(Target),
    lists:any(fun(I) -> TargetLastMod < filelib:last_modified(I) end,
              [Source] ++ Hrls).

-spec internal_erl_compile(rebar_config:config(), file:filename(),
                           file:filename(), list()) -> 'ok' | 'skipped'.
internal_erl_compile(Config, Source, Outdir, ErlOpts) ->
    %% Determine the target name and includes list by inspecting the source file
    {Module, Hrls} = inspect(Source, include_path(Source, Config)),

    %% Construct the target filename
    Target = filename:join([Outdir | string:tokens(Module, ".")]) ++ ".beam",
    ok = filelib:ensure_dir(Target),

    %% If the file needs compilation, based on last mod date of includes or
    %% the target
    case needs_compile(Source, Target, Hrls) of
        true ->
            Opts = [{outdir, filename:dirname(Target)}] ++
                ErlOpts ++ [{i, "include"}, return],
            case compile:file(Source, Opts) of
                {ok, _Mod} ->
                    ok;
                {ok, _Mod, Ws} ->
                    rebar_base_compiler:ok_tuple(Config, Source, Ws);
                {error, Es, Ws} ->
                    rebar_base_compiler:error_tuple(Config, Source,
                                                    Es, Ws, Opts)
            end;
        false ->
            skipped
    end.

-spec compile_mib(file:filename(), file:filename(),
                  rebar_config:config()) -> 'ok'.
compile_mib(Source, Target, Config) ->
    ok = rebar_utils:ensure_dir(Target),
    ok = rebar_utils:ensure_dir(filename:join("include", "dummy.hrl")),
    Opts = [{outdir, "priv/mibs"}, {i, ["priv/mibs"]}] ++
        rebar_config:get(Config, mib_opts, []),
    case snmpc:compile(Source, Opts) of
        {ok, _} ->
            Mib = filename:rootname(Target),
            MibToHrlOpts =
                case proplists:get_value(verbosity, Opts, undefined) of
                    undefined ->
                        #options{specific = []};
                    Verbosity ->
                        #options{specific = [{verbosity, Verbosity}]}
                end,
            ok = snmpc:mib_to_hrl(Mib, Mib, MibToHrlOpts),
            Hrl_filename = Mib ++ ".hrl",
            rebar_file_utils:mv(Hrl_filename, "include"),
            ok;
        {error, compilation_failed} ->
            ?FAIL
    end.

-spec compile_xrl(file:filename(), file:filename(),
                  rebar_config:config()) -> 'ok'.
compile_xrl(Source, Target, Config) ->
    Opts = [{scannerfile, Target} | rebar_config:get(Config, xrl_opts, [])],
    compile_xrl_yrl(Config, Source, Target, Opts, leex).

-spec compile_yrl(file:filename(), file:filename(),
                  rebar_config:config()) -> 'ok'.
compile_yrl(Source, Target, Config) ->
    Opts = [{parserfile, Target} | rebar_config:get(Config, yrl_opts, [])],
    compile_xrl_yrl(Config, Source, Target, Opts, yecc).

-spec compile_xrl_yrl(rebar_config:config(), file:filename(),
                      file:filename(), list(), module()) -> 'ok'.
compile_xrl_yrl(Config, Source, Target, Opts, Mod) ->
    case needs_compile(Source, Target, []) of
        true ->
            case Mod:file(Source, Opts ++ [{return, true}]) of
                {ok, _} ->
                    ok;
                {ok, _Mod, Ws} ->
                    rebar_base_compiler:ok_tuple(Config, Source, Ws);
                {error, Es, Ws} ->
                    rebar_base_compiler:error_tuple(Config, Source,
                                                    Es, Ws, Opts)
            end;
        false ->
            skipped
    end.

gather_src([], Srcs) ->
    Srcs;
gather_src([Dir|Rest], Srcs) ->
    gather_src(Rest, Srcs ++ rebar_utils:find_files(Dir, ".*\\.erl\$")).

-spec dirs(file:filename()) -> [file:filename()].
dirs(Dir) ->
    [F || F <- filelib:wildcard(filename:join([Dir, "*"])), filelib:is_dir(F)].

-spec delete_dir(file:filename(), [string()]) -> 'ok' | {'error', atom()}.
delete_dir(Dir, []) ->
    file:del_dir(Dir);
delete_dir(Dir, Subdirs) ->
    lists:foreach(fun(D) -> delete_dir(D, dirs(D)) end, Subdirs),
    file:del_dir(Dir).

-spec compile_priority(file:filename()) -> 'normal' | 'behaviour' |
                                           'callback' |
                                           'parse_transform'.
compile_priority(File) ->
    case epp_dodger:parse_file(File) of
        {error, _} ->
            normal; % couldn't parse the file, default priority
        {ok, Trees} ->
            F2 = fun({tree,arity_qualifier,_,
                      {arity_qualifier,{tree,atom,_,behaviour_info},
                       {tree,integer,_,1}}}, _) ->
                         behaviour;
                    ({tree,arity_qualifier,_,
                      {arity_qualifier,{tree,atom,_,parse_transform},
                       {tree,integer,_,2}}}, _) ->
                         parse_transform;
                    (_, Acc) ->
                         Acc
                 end,

            F = fun({tree, attribute, _,
                     {attribute, {tree, atom, _, export},
                      [{tree, list, _, {list, List, none}}]}}, Acc) ->
                        lists:foldl(F2, Acc, List);
                   ({tree, attribute, _,
                     {attribute, {tree, atom, _, callback},_}}, _Acc) ->
                        callback;
                   (_, Acc) ->
                        Acc
                end,

            lists:foldl(F, normal, Trees)
    end.

%%
%% Ensure all files in a list are present and abort if one is missing
%%
-spec check_files([file:filename()]) -> [file:filename()].
check_files(FileList) ->
    [check_file(F) || F <- FileList].

check_file(File) ->
    case filelib:is_regular(File) of
        false -> ?ABORT("File ~p is missing, aborting\n", [File]);
        true -> File
    end.
