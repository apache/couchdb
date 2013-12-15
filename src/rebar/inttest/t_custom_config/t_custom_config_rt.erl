-module(t_custom_config_rt).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [{copy, "../../rebar", "rebar"},
     {copy, "custom.config", "custom.config"},
     {create, "ebin/custom_config.app", app(custom_config, [custom_config])}].

run(Dir) ->
    retest_log:log(debug, "Running in Dir: ~s~n", [Dir]),
    Ref = retest:sh("./rebar -C custom.config check-deps -vvv",
                    [{async, true}]),
    {ok, Captured} =
        retest:sh_expect(Ref,
                         "DEBUG: Consult config file .*/custom.config.*",
                         [{capture, all, list}]),
    {ok, Missing} =
        retest:sh_expect(Ref,
                         "DEBUG: Missing deps  : \\[\\{dep,bad_name,"
                         "boo,\"\\.\",undefined,false\\}\\]",
                         [{capture, all, list}]),
    retest_log:log(debug, "[CAPTURED]: ~s~n", [Captured]),
    retest_log:log(debug, "[Missing]: ~s~n", [Missing]),
    ok.

%%
%% Generate the contents of a simple .app file
%%
app(Name, Modules) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {modules, Modules},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).
