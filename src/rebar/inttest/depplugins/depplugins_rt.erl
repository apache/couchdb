%%% @doc Plugin handling test
%%%
%%% This test checks if plugins are loaded correctly.
%%%
%%% It has three applications:
%%% <ol>
%%%   <li>fish. top-level module, has one dependency: `dependsonplugin'.</li>
%%%   <li>dependsonplugin. This depends on some pre-compile actions by the
%%%       plugin. In the test the plugin creates a file `pre.compile' in the
%%%       top-level folder of this application.</li>
%%%   <li>testplugin. This is a plugin application which creates the file.</li>
%%% </ol>

-module(depplugins_rt).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [
     {copy, "../../rebar", "rebar"},
     {copy, "rebar.config", "rebar.config"},
     {create, "ebin/fish.app", app(fish, [])},

     {create, "deps/dependsonplugin/ebin/dependsonplugin.app",
        app(dependsonplugin, [])},
     {copy, "rebar_dependsonplugin.config",
        "deps/dependsonplugin/rebar.config"},
     {copy, "testplugin_mod.erl",
        "deps/testplugin/plugins/testplugin_mod.erl"},
     {create, "deps/testplugin/ebin/testplugin.app",
        app(testplugin, [])}
    ].

run(_Dir) ->
    ?assertMatch({ok, _}, retest_sh:run("./rebar compile", [])),
    ?assertEqual(true, filelib:is_regular("deps/dependsonplugin/pre.compile")),
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
