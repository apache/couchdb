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
-module(rebar_utils).

-export([get_cwd/0,
         is_arch/1,
         get_arch/0,
         wordsize/0,
         sh/2,
         find_files/2, find_files/3,
         now_str/0,
         ensure_dir/1,
         beam_to_mod/2, beams/1,
         erl_to_mod/1,
         abort/0, abort/2,
         escript_foldl/3,
         find_executable/1,
         prop_check/3,
         expand_code_path/0,
         expand_env_variable/3,
         vcs_vsn/3,
         deprecated/3, deprecated/4,
         get_deprecated_global/4, get_deprecated_global/5,
         get_experimental_global/3, get_experimental_local/3,
         get_deprecated_list/4, get_deprecated_list/5,
         get_deprecated_local/4, get_deprecated_local/5,
         delayed_halt/1,
         erl_opts/1,
         src_dirs/1,
         ebin_dir/0,
         processing_base_dir/1, processing_base_dir/2]).

-include("rebar.hrl").

%% ====================================================================
%% Public API
%% ====================================================================

get_cwd() ->
    {ok, Dir} = file:get_cwd(),
    Dir.

is_arch(ArchRegex) ->
    case re:run(get_arch(), ArchRegex, [{capture, none}]) of
        match ->
            true;
        nomatch ->
            false
    end.

get_arch() ->
    Words = wordsize(),
    erlang:system_info(otp_release) ++ "-"
        ++ erlang:system_info(system_architecture) ++ "-" ++ Words.

wordsize() ->
    try erlang:system_info({wordsize, external}) of
        Val ->
            integer_to_list(8 * Val)
    catch
        error:badarg ->
            integer_to_list(8 * erlang:system_info(wordsize))
    end.

%%
%% Options = [Option] -- defaults to [use_stdout, abort_on_error]
%% Option = ErrorOption | OutputOption | {cd, string()} | {env, Env}
%% ErrorOption = return_on_error | abort_on_error | {abort_on_error, string()}
%% OutputOption = use_stdout | {use_stdout, bool()}
%% Env = [{string(), Val}]
%% Val = string() | false
%%
sh(Command0, Options0) ->
    ?INFO("sh info:\n\tcwd: ~p\n\tcmd: ~s\n", [get_cwd(), Command0]),
    ?DEBUG("\topts: ~p\n", [Options0]),

    DefaultOptions = [use_stdout, abort_on_error],
    Options = [expand_sh_flag(V)
               || V <- proplists:compact(Options0 ++ DefaultOptions)],

    ErrorHandler = proplists:get_value(error_handler, Options),
    OutputHandler = proplists:get_value(output_handler, Options),

    Command = patch_on_windows(Command0, proplists:get_value(env, Options, [])),
    PortSettings = proplists:get_all_values(port_settings, Options) ++
        [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide],
    ?DEBUG("Port Cmd: ~p\nPort Opts: ~p\n", [Command, PortSettings]),
    Port = open_port({spawn, Command}, PortSettings),

    case sh_loop(Port, OutputHandler, []) of
        {ok, _Output} = Ok ->
            Ok;
        {error, {_Rc, _Output}=Err} ->
            ErrorHandler(Command, Err)
    end.

find_files(Dir, Regex) ->
    find_files(Dir, Regex, true).

find_files(Dir, Regex, Recursive) ->
    filelib:fold_files(Dir, Regex, Recursive,
                       fun(F, Acc) -> [F | Acc] end, []).

now_str() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4b/~2..0b/~2..0b ~2..0b:~2..0b:~2..0b",
                                [Year, Month, Day, Hour, Minute, Second])).

%% TODO: filelib:ensure_dir/1 corrected in R13B04. Remove when we drop
%% support for OTP releases older than R13B04.
ensure_dir(Path) ->
    case filelib:ensure_dir(Path) of
        ok ->
            ok;
        {error,eexist} ->
            ok;
        Error ->
            Error
    end.

-spec abort() -> no_return().
abort() ->
    throw(rebar_abort).

-spec abort(string(), [term()]) -> no_return().
abort(String, Args) ->
    ?ERROR(String, Args),
    abort().

%% TODO: Rename emulate_escript_foldl to escript_foldl and remove
%% this function when the time is right. escript:foldl/3 was an
%% undocumented exported fun and has been removed in R14.
escript_foldl(Fun, Acc, File) ->
    {module, zip} = code:ensure_loaded(zip),
    case erlang:function_exported(zip, foldl, 3) of
        true ->
            emulate_escript_foldl(Fun, Acc, File);
        false ->
            escript:foldl(Fun, Acc, File)
    end.

find_executable(Name) ->
    case os:find_executable(Name) of
        false -> false;
        Path ->
            "\"" ++ filename:nativename(Path) ++ "\""
    end.

%% Helper function for checking values and aborting when needed
prop_check(true, _, _) -> true;
prop_check(false, Msg, Args) -> ?ABORT(Msg, Args).

%% Convert all the entries in the code path to absolute paths.
expand_code_path() ->
    CodePath = lists:foldl(fun(Path, Acc) ->
                                   [filename:absname(Path) | Acc]
                           end, [], code:get_path()),
    code:set_path(lists:reverse(CodePath)).

%%
%% Given env. variable FOO we want to expand all references to
%% it in InStr. References can have two forms: $FOO and ${FOO}
%% The end of form $FOO is delimited with whitespace or eol
%%
expand_env_variable(InStr, VarName, RawVarValue) ->
    case string:chr(InStr, $$) of
        0 ->
            %% No variables to expand
            InStr;
        _ ->
            ReOpts = [global, unicode, {return, list}],
            VarValue = re:replace(RawVarValue, "\\\\", "\\\\\\\\", ReOpts),
            %% Use a regex to match/replace:
            %% Given variable "FOO": match $FOO\s | $FOOeol | ${FOO}
            RegEx = io_lib:format("\\\$(~s(\\s|$)|{~s})", [VarName, VarName]),
            re:replace(InStr, RegEx, [VarValue, "\\2"], ReOpts)
    end.

vcs_vsn(Config, Vsn, Dir) ->
    Key = {Vsn, Dir},
    Cache = rebar_config:get_xconf(Config, vsn_cache),
    case dict:find(Key, Cache) of
        error ->
            VsnString = vcs_vsn_1(Vsn, Dir),
            Cache1 = dict:store(Key, VsnString, Cache),
            Config1 = rebar_config:set_xconf(Config, vsn_cache, Cache1),
            {Config1, VsnString};
        {ok, VsnString} ->
            {Config, VsnString}
    end.

get_deprecated_global(Config, OldOpt, NewOpt, When) ->
    get_deprecated_global(Config, OldOpt, NewOpt, undefined, When).

get_deprecated_global(Config, OldOpt, NewOpt, Default, When) ->
    get_deprecated_3(fun rebar_config:get_global/3,
                     Config, OldOpt, NewOpt, Default, When).

get_experimental_global(Config, Opt, Default) ->
    get_experimental_3(fun rebar_config:get_global/3, Config, Opt, Default).

get_experimental_local(Config, Opt, Default) ->
    get_experimental_3(fun rebar_config:get_local/3, Config, Opt, Default).

get_deprecated_list(Config, OldOpt, NewOpt, When) ->
    get_deprecated_list(Config, OldOpt, NewOpt, undefined, When).

get_deprecated_list(Config, OldOpt, NewOpt, Default, When) ->
    get_deprecated_3(fun rebar_config:get_list/3,
                     Config, OldOpt, NewOpt, Default, When).

get_deprecated_local(Config, OldOpt, NewOpt, When) ->
    get_deprecated_local(Config, OldOpt, NewOpt, undefined, When).

get_deprecated_local(Config, OldOpt, NewOpt, Default, When) ->
    get_deprecated_3(fun rebar_config:get_local/3,
                     Config, OldOpt, NewOpt, Default, When).

deprecated(Old, New, Opts, When) when is_list(Opts) ->
    case lists:member(Old, Opts) of
        true ->
            deprecated(Old, New, When);
        false ->
            ok
    end;
deprecated(Old, New, Config, When) ->
    case rebar_config:get(Config, Old, undefined) of
        undefined ->
            ok;
        _ ->
            deprecated(Old, New, When)
    end.

deprecated(Old, New, When) ->
    io:format(
      <<"WARNING: deprecated ~p option used~n"
        "Option '~p' has been deprecated~n"
        "in favor of '~p'.~n"
        "'~p' will be removed ~s.~n~n">>,
      [Old, Old, New, Old, When]).

-spec delayed_halt(integer()) -> no_return().
delayed_halt(Code) ->
    %% Work around buffer flushing issue in erlang:halt if OTP older
    %% than R15B01.
    %% TODO: remove workaround once we require R15B01 or newer
    %% R15B01 introduced erlang:halt/2
    case erlang:is_builtin(erlang, halt, 2) of
        true ->
            halt(Code);
        false ->
            case os:type() of
                {win32, nt} ->
                    timer:sleep(100),
                    halt(Code);
                _ ->
                    halt(Code),
                    %% workaround to delay exit until all output is written
                    receive after infinity -> ok end
            end
    end.

%% @doc Return list of erl_opts
-spec erl_opts(rebar_config:config()) -> list().
erl_opts(Config) ->
    RawErlOpts = filter_defines(rebar_config:get(Config, erl_opts, []), []),
    Defines = [{d, list_to_atom(D)} ||
                  D <- rebar_config:get_xconf(Config, defines, [])],
    Opts = Defines ++ RawErlOpts,
    case proplists:is_defined(no_debug_info, Opts) of
        true ->
            [O || O <- Opts, O =/= no_debug_info];
        false ->
            [debug_info|Opts]
    end.

-spec src_dirs([string()]) -> [file:filename(), ...].
src_dirs([]) ->
    ["src"];
src_dirs(SrcDirs) ->
    SrcDirs.

ebin_dir() ->
    filename:join(get_cwd(), "ebin").

processing_base_dir(Config) ->
    Cwd = rebar_utils:get_cwd(),
    processing_base_dir(Config, Cwd).

processing_base_dir(Config, Dir) ->
    Dir =:= rebar_config:get_xconf(Config, base_dir).

%% ====================================================================
%% Internal functions
%% ====================================================================

get_deprecated_3(Get, Config, OldOpt, NewOpt, Default, When) ->
    case Get(Config, NewOpt, Default) of
        Default ->
            case Get(Config, OldOpt, Default) of
                Default ->
                    Default;
                Old ->
                    deprecated(OldOpt, NewOpt, When),
                    Old
            end;
        New ->
            New
    end.

get_experimental_3(Get, Config, Opt, Default) ->
    Val = Get(Config, Opt, Default),
    case Val of
        Default ->
            Default;
        Val ->
            ?CONSOLE("NOTICE: Using experimental option '~p'~n", [Opt]),
            Val
    end.

%% We do the shell variable substitution ourselves on Windows and hope that the
%% command doesn't use any other shell magic.
patch_on_windows(Cmd, Env) ->
    case os:type() of
        {win32,nt} ->
            Cmd1 = "cmd /q /c "
                ++ lists:foldl(fun({Key, Value}, Acc) ->
                                       expand_env_variable(Acc, Key, Value)
                               end, Cmd, Env),
            %% Remove left-over vars
            re:replace(Cmd1, "\\\$\\w+|\\\${\\w+}", "",
                       [global, {return, list}]);
        _ ->
            Cmd
    end.

expand_sh_flag(return_on_error) ->
    {error_handler,
     fun(_Command, Err) ->
             {error, Err}
     end};
expand_sh_flag({abort_on_error, Message}) ->
    {error_handler,
     log_msg_and_abort(Message)};
expand_sh_flag(abort_on_error) ->
    {error_handler,
     fun log_and_abort/2};
expand_sh_flag(use_stdout) ->
    {output_handler,
     fun(Line, Acc) ->
             ?CONSOLE("~s", [Line]),
             [Line | Acc]
     end};
expand_sh_flag({use_stdout, false}) ->
    {output_handler,
     fun(Line, Acc) ->
             [Line | Acc]
     end};
expand_sh_flag({cd, _CdArg} = Cd) ->
    {port_settings, Cd};
expand_sh_flag({env, _EnvArg} = Env) ->
    {port_settings, Env}.

-type err_handler() :: fun((string(), {integer(), string()}) -> no_return()).
-spec log_msg_and_abort(string()) -> err_handler().
log_msg_and_abort(Message) ->
    fun(_Command, {_Rc, _Output}) ->
            ?ABORT(Message, [])
    end.

-spec log_and_abort(string(), {integer(), string()}) -> no_return().
log_and_abort(Command, {Rc, Output}) ->
    ?ABORT("~s failed with error: ~w and output:~n~s~n",
           [Command, Rc, Output]).

sh_loop(Port, Fun, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            sh_loop(Port, Fun, Fun(Line ++ "\n", Acc));
        {Port, {data, {noeol, Line}}} ->
            sh_loop(Port, Fun, Fun(Line, Acc));
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(lists:reverse(Acc))};
        {Port, {exit_status, Rc}} ->
            {error, {Rc, lists:flatten(lists:reverse(Acc))}}
    end.

beam_to_mod(Dir, Filename) ->
    [Dir | Rest] = filename:split(Filename),
    list_to_atom(filename:basename(string:join(Rest, "."), ".beam")).

erl_to_mod(Filename) ->
    list_to_atom(filename:rootname(filename:basename(Filename))).

beams(Dir) ->
    filelib:fold_files(Dir, ".*\.beam\$", true,
                       fun(F, Acc) -> [F | Acc] end, []).

emulate_escript_foldl(Fun, Acc, File) ->
    case escript:extract(File, [compile_source]) of
        {ok, [_Shebang, _Comment, _EmuArgs, Body]} ->
            case Body of
                {source, BeamCode} ->
                    GetInfo = fun() -> file:read_file_info(File) end,
                    GetBin = fun() -> BeamCode end,
                    {ok, Fun(".", GetInfo, GetBin, Acc)};
                {beam, BeamCode} ->
                    GetInfo = fun() -> file:read_file_info(File) end,
                    GetBin = fun() -> BeamCode end,
                    {ok, Fun(".", GetInfo, GetBin, Acc)};
                {archive, ArchiveBin} ->
                    zip:foldl(Fun, Acc, {File, ArchiveBin})
            end;
        {error, _} = Error ->
            Error
    end.

vcs_vsn_1(Vcs, Dir) ->
    case vcs_vsn_cmd(Vcs) of
        {plain, VsnString} ->
            VsnString;
        {cmd, CmdString} ->
            vcs_vsn_invoke(CmdString, Dir);
        unknown ->
            ?ABORT("vcs_vsn: Unknown vsn format: ~p\n", [Vcs]);
        Cmd ->
            %% If there is a valid VCS directory in the application directory,
            %% use that version info
            Extension = lists:concat([".", Vcs]),
            case filelib:is_dir(filename:join(Dir, Extension)) of
                true ->
                    ?DEBUG("vcs_vsn: Primary vcs used for ~s\n", [Dir]),
                    vcs_vsn_invoke(Cmd, Dir);
                false ->
                    %% No VCS directory found for the app. Depending on source
                    %% tree structure, there may be one higher up, but that can
                    %% yield unexpected results when used with deps. So, we
                    %% fallback to searching for a priv/vsn.Vcs file.
                    VsnFile = filename:join([Dir, "priv", "vsn" ++ Extension]),
                    case file:read_file(VsnFile) of
                        {ok, VsnBin} ->
                            ?DEBUG("vcs_vsn: Read ~s from priv/vsn.~p\n",
                                   [VsnBin, Vcs]),
                            string:strip(binary_to_list(VsnBin), right, $\n);
                        {error, enoent} ->
                            ?DEBUG("vcs_vsn: Fallback to vcs for ~s\n", [Dir]),
                            vcs_vsn_invoke(Cmd, Dir)
                    end
            end
    end.

vcs_vsn_cmd(git)    -> "git describe --always --tags";
vcs_vsn_cmd(hg)     -> "hg identify -i";
vcs_vsn_cmd(bzr)    -> "bzr revno";
vcs_vsn_cmd(svn)    -> "svnversion";
vcs_vsn_cmd(fossil) -> "fossil info";
vcs_vsn_cmd({cmd, _Cmd}=Custom) -> Custom;
vcs_vsn_cmd(Version) when is_list(Version) -> {plain, Version};
vcs_vsn_cmd(_) -> unknown.

vcs_vsn_invoke(Cmd, Dir) ->
    {ok, VsnString} = rebar_utils:sh(Cmd, [{cd, Dir}, {use_stdout, false}]),
    string:strip(VsnString, right, $\n).

%%
%% Filter a list of erl_opts platform_define options such that only
%% those which match the provided architecture regex are returned.
%%
filter_defines([], Acc) ->
    lists:reverse(Acc);
filter_defines([{platform_define, ArchRegex, Key} | Rest], Acc) ->
    case rebar_utils:is_arch(ArchRegex) of
        true ->
            filter_defines(Rest, [{d, Key} | Acc]);
        false ->
            filter_defines(Rest, Acc)
    end;
filter_defines([{platform_define, ArchRegex, Key, Value} | Rest], Acc) ->
    case rebar_utils:is_arch(ArchRegex) of
        true ->
            filter_defines(Rest, [{d, Key, Value} | Acc]);
        false ->
            filter_defines(Rest, Acc)
    end;
filter_defines([Opt | Rest], Acc) ->
    filter_defines(Rest, [Opt | Acc]).
