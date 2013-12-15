-module(tplugins_rt).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(COMPILE_ERROR,
        "ERROR: Plugin bad_plugin contains compilation errors:").

files() ->
    [
     {copy, "../../rebar", "rebar"},
     {copy, "rebar.config", "rebar.config"},
     {copy, "bad.config", "bad.config"},
     {copy, "fish.erl", "src/fish.erl"},
     {copy, "test_plugin.erl", "plugins/test_plugin.erl"},
     {copy, "bad_plugin.erl", "bad_plugins/bad_plugin.erl"},
     {create, "fwibble.test", <<"fwibble">>},
     {create, "ebin/fish.app", app(fish, [fish])}
    ].

run(_Dir) ->
    ?assertMatch({ok, _}, retest_sh:run("./rebar fwibble -v", [])),
    ?assertEqual(false, filelib:is_regular("fwibble.test")),
    Ref = retest:sh("./rebar -C bad.config -v clean", [{async, true}]),
    {ok, _} = retest:sh_expect(Ref, "ERROR: Plugin .*bad_plugin.erl "
                               "contains compilation errors:.*",
                               [{newline, any}]),
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
