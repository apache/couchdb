%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
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
-module(rebar_otp_app).

-export([compile/2,
         clean/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, File) ->
    %% If we get an .app.src file, it needs to be pre-processed and
    %% written out as a ebin/*.app file. That resulting file will then
    %% be validated as usual.
    {Config1, AppFile} = case rebar_app_utils:is_app_src(File) of
                             true ->
                                 preprocess(Config, File);
                             false ->
                                 {Config, File}
                         end,

    %% Load the app file and validate it.
    case rebar_app_utils:load_app_file(Config1, AppFile) of
        {ok, Config2, AppName, AppData} ->
            validate_name(AppName, AppFile),

            %% In general, the list of modules is an important thing to validate
            %% for compliance with OTP guidelines and upgrade procedures.
            %% However, some people prefer not to validate this list.
            case rebar_config:get_local(Config1, validate_app_modules, true) of
                true ->
                    Modules = proplists:get_value(modules, AppData),
                    {validate_modules(AppName, Modules), Config2};
                false ->
                    {ok, Config2}
            end;
        {error, Reason} ->
            ?ABORT("Failed to load app file ~s: ~p\n", [AppFile, Reason])
    end.

clean(_Config, File) ->
    %% If the app file is a .app.src, delete the generated .app file
    case rebar_app_utils:is_app_src(File) of
        true ->
            case file:delete(rebar_app_utils:app_src_to_app(File)) of
                ok ->
                    ok;
                {error, enoent} ->
                    %% The file not existing is OK, we can ignore the error.
                    ok;
                Other ->
                    Other
            end;
        false ->
            ok
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, compile) ->
    info_help("Validate .app file");
info(help, clean) ->
    info_help("Delete .app file if generated from .app.src").

info_help(Description) ->
    ?CONSOLE(
       "~s.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  ~p~n",
       [
        Description,
        {validate_app_modules, true}
       ]).

preprocess(Config, AppSrcFile) ->
    case rebar_app_utils:load_app_file(Config, AppSrcFile) of
        {ok, Config1, AppName, AppData} ->
            %% Look for a configuration file with vars we want to
            %% substitute. Note that we include the list of modules available in
            %% ebin/ and update the app data accordingly.
            AppVars = load_app_vars(Config1) ++ [{modules, ebin_modules()}],
            A1 = apply_app_vars(AppVars, AppData),


            %% AppSrcFile may contain instructions for generating a vsn number
            {Config2, Vsn} = rebar_app_utils:app_vsn(Config1, AppSrcFile),
            A2 = lists:keystore(vsn, 1, A1, {vsn, Vsn}),

            %% systools:make_relup/4 fails with {missing_param, registered}
            %% without a 'registered' value.
            A3 = ensure_registered(A2),

            %% Build the final spec as a string
            Spec = io_lib:format("~p.\n", [{application, AppName, A3}]),

            %% Setup file .app filename and write new contents
            AppFile = rebar_app_utils:app_src_to_app(AppSrcFile),
            ok = rebar_file_utils:write_file_if_contents_differ(AppFile, Spec),

            %% Make certain that the ebin/ directory is available
            %% on the code path
            true = code:add_path(filename:absname(filename:dirname(AppFile))),

            {Config2, AppFile};

        {error, Reason} ->
            ?ABORT("Failed to read ~s for preprocessing: ~p\n",
                   [AppSrcFile, Reason])
    end.

load_app_vars(Config) ->
    case rebar_config:get_local(Config, app_vars_file, undefined) of
        undefined ->
            ?INFO("No app_vars_file defined.\n", []),
            [];
        Filename ->
            ?INFO("Loading app vars from ~p\n", [Filename]),
            {ok, Vars} = file:consult(Filename),
            Vars
    end.

apply_app_vars([], AppData) ->
    AppData;
apply_app_vars([{Key, Value} | Rest], AppData) ->
    AppData2 = lists:keystore(Key, 1, AppData, {Key, Value}),
    apply_app_vars(Rest, AppData2).

validate_name(AppName, File) ->
    %% Convert the .app file name to an atom -- check it against the
    %% identifier within the file
    ExpApp = list_to_atom(filename:basename(File, ".app")),
    case ExpApp == AppName of
        true ->
            ok;
        false ->
            ?ERROR("Invalid ~s: name of application (~p) "
                   "must match filename.\n", [File, AppName]),
            ?FAIL
    end.

validate_modules(AppName, undefined) ->
    ?ERROR("Missing modules declaration in ~p.app~n", [AppName]),
    ?FAIL;

validate_modules(AppName, Mods) ->
    %% Construct two sets -- one for the actual .beam files in ebin/
    %% and one for the modules
    %% listed in the .app file
    EbinSet = ordsets:from_list(ebin_modules()),
    ModSet = ordsets:from_list(Mods),

    %% Identify .beam files listed in the .app, but not present in ebin/
    case ordsets:subtract(ModSet, EbinSet) of
        [] ->
            ok;
        MissingBeams ->
            Msg1 = lists:flatten([io_lib:format("\t* ~p\n", [M]) ||
                                     M <- MissingBeams]),
            ?ERROR("One or more modules listed in ~p.app are not "
                   "present in ebin/*.beam:\n~s", [AppName, Msg1]),
            ?FAIL
    end,

    %% Identify .beam files NOT list in the .app, but present in ebin/
    case ordsets:subtract(EbinSet, ModSet) of
        [] ->
            ok;
        MissingMods ->
            Msg2 = lists:flatten([io_lib:format("\t* ~p\n", [M]) ||
                                     M <- MissingMods]),
            ?ERROR("One or more .beam files exist that are not "
                   "listed in ~p.app:\n~s", [AppName, Msg2]),
            ?FAIL
    end.

ebin_modules() ->
    lists:sort([rebar_utils:beam_to_mod("ebin", N) ||
                   N <- rebar_utils:beams("ebin")]).

ensure_registered(AppData) ->
    case lists:keyfind(registered, 1, AppData) of
        false ->
            [{registered, []} | AppData];
        {registered, _} ->
            %% We could further check whether the value is a list of atoms.
            AppData
    end.
