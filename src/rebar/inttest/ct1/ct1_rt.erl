-module(ct1_rt).

-compile(export_all).


files() ->
    [{create, "ebin/a1.app", app(a1)},
     {copy, "../../rebar", "rebar"},
     {copy, "rebar.config", "rebar.config"},
     {copy, "test_SUITE.erl", "itest/test_SUITE.erl"}].

run(_Dir) ->
    {ok, _} = retest:sh("./rebar compile ct"),
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
