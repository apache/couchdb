-module(ct2_rt).

-compile(export_all).


files() ->
    [{create, "ebin/foo.app", app(foo)},
     {copy, "../../rebar", "rebar"},
     {copy, "foo.test.spec", "foo.test.spec"},
     {copy, "foo_SUITE.erl", "test/foo_SUITE.erl"}].

run(_Dir) ->
    {ok, _} = retest:sh("./rebar compile ct -vvv"),
    ok.

%%
%% Generate the contents of a simple .app file
%%
app(Name) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {modules, []},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).
