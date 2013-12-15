-module(bug_5_rt).

-compile(export_all).


files() ->
    [{create, "ebin/a1.app", app(a1)},
     {create, "deps/d1/src/d1.app.src", app(d1)},
     {create, "rebar.config",
      <<"{deps, [{d1, \"1\", {hg, \"http://example.com\", \"tip\"}}]}.\n">>},
     {copy, "../rebar", "rebar"}].

run(_Dir) ->
    {ok, _} = retest:sh("./rebar compile"),
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
