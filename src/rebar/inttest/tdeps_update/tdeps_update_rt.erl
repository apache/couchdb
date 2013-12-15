-module(tdeps_update_rt).

-compile(export_all).

%% Exercises update deps, with recursive dependency updates.
%% Initially:
%%     A(v0.5) -> B(v0.2.3) -> C(v1.0)
%% But after updating A to 0.6:
%%     A(v0.6) -> B(v0.2.4) -> C(v1.1)
%%                          -> D(v0.7)
%% And after updating A to 0.7:
%%     A(v0.7) -> B(v0.2.5) -> C(v1.2) -> E(v2.0)
%%                          -> D(v0.7)
%% And after updating A to 0.8:
%%     A(v0.8) -> B(v0.2.6) -> C(v1.3) -> E(v2.1)
%%                          -> D(v0.7)
%%             -> F(v0.1)   -> E(v2.1)
files() ->
    [
     %% A1 application
     {create, "apps/a1/ebin/a1.app", app(a1, [a1], "0.5")},
     {copy, "a.rebar.config", "apps/a1/rebar.config"},
     {template, "a.erl", "apps/a1/src/a1.erl", dict:from_list([{module, a1}])},

     {copy, "root.rebar.config", "rebar.config"},
     {copy, "../../rebar", "rebar"},

     %% B application
     {create, "repo/b/ebin/b.app", app(b, [], "0.2.3")},
     {create, "b2.app", app(b, [], "0.2.4")},
     {create, "b3.app", app(b, [], "0.2.5")},
     {create, "b4.app", app(b, [], "0.2.6")},
     {copy, "b.rebar.config", "repo/b/rebar.config"},
     {copy, "b.hrl", "repo/b/include/b.hrl"},

     %% C application
     {create, "repo/c/ebin/c.app", app(c, [], "1.0")},
     {create, "c2.app", app(c, [], "1.1")},
     {create, "c3.app", app(c, [], "1.2")},
     {create, "c4.app", app(c, [], "1.3")},
     {copy, "c.hrl", "repo/c/include/c.hrl"},

     %% D application
     {create, "repo/d/ebin/d.app", app(d, [], "0.7")},
     {copy, "d.hrl", "repo/d/include/d.hrl"},

     %% E application
     {create, "repo/e/ebin/e.app", app(e, [], "2.0")},
     {create, "e2.app", app(e, [], "2.1")},

     %% F application
     {create, "repo/f/ebin/f.app", app(f, [], "0.1")},

     %% update files
     {copy, "a2.rebar.config", "a2.rebar.config"},
     {copy, "a3.rebar.config", "a3.rebar.config"},
     {copy, "a4.rebar.config", "a4.rebar.config"},
     {copy, "b2.rebar.config", "b2.rebar.config"},
     {copy, "b3.rebar.config", "b3.rebar.config"},
     {copy, "b4.rebar.config", "b4.rebar.config"},
     {copy, "c2.hrl", "c2.hrl"},
     {copy, "c.rebar.config", "c.rebar.config"},
     {copy, "c2.rebar.config", "c2.rebar.config"},
     {copy, "c3.rebar.config", "c3.rebar.config"}
    ].

apply_cmds([], _Params) ->
    ok;
apply_cmds([Cmd | Rest], Params) ->
    io:format("Running: ~s (~p)\n", [Cmd, Params]),
    {ok, _} = retest_sh:run(Cmd, Params),
    apply_cmds(Rest, Params).

run(_Dir) ->
    %% Initialize the b/c/d apps as git repos so that dependencies pull
    %% properly
    GitCmds = ["git init",
               "git add -A",
               "git config user.email 'tdeps@example.com'",
               "git config user.name 'tdeps'",
               "git commit -a -m 'Initial Commit'"],
    BCmds = ["git tag 0.2.3",
             "cp ../../b2.rebar.config rebar.config",
             "cp ../../b2.app ebin/b.app",
             "git commit -a -m 'update to 0.2.4'",
             "git tag 0.2.4",
             "cp ../../b3.rebar.config rebar.config",
             "cp ../../b3.app ebin/b.app",
             "git commit -a -m 'update to 0.2.5'",
             "git tag 0.2.5",
             "cp ../../b4.rebar.config rebar.config",
             "cp ../../b4.app ebin/b.app",
             "git commit -a -m 'update to 0.2.6'",
             "git tag 0.2.6"],
             %"git checkout 0.2.3"],
    CCmds = ["git tag 1.0",
             "cp ../../c2.hrl include/c.hrl",
             "cp ../../c2.app ebin/c.app",
             "cp ../../c.rebar.config rebar.config",
             "git add rebar.config",
             "git commit -a -m 'update to 1.1'",
             "git tag 1.1",
             "cp ../../c3.app ebin/c.app",
             "cp ../../c2.rebar.config rebar.config",
             "git commit -a -m 'update to 1.2'",
             "git tag 1.2",
             "cp ../../c4.app ebin/c.app",
             "cp ../../c3.rebar.config rebar.config",
             "git commit -a -m 'update to 1.3'",
             "git tag 1.3"],
             %"git checkout 1.0"],
    DCmds = ["git tag 0.7"],
    ECmds = ["git tag 2.0",
             "cp ../../e2.app ebin/e.app",
             "git commit -a -m 'update to 2.1'",
             "git tag 2.1"],
    FCmds = ["git tag 0.1"],

    ok = apply_cmds(GitCmds++BCmds, [{dir, "repo/b"}]),
    ok = apply_cmds(GitCmds++CCmds, [{dir, "repo/c"}]),
    ok = apply_cmds(GitCmds++DCmds, [{dir, "repo/d"}]),
    ok = apply_cmds(GitCmds++ECmds, [{dir, "repo/e"}]),
    ok = apply_cmds(GitCmds++FCmds, [{dir, "repo/f"}]),

    {ok, _} = retest_sh:run("./rebar -v get-deps compile", []),
    os:cmd("cp a2.rebar.config apps/a1/rebar.config"),
    {ok, _} = retest_sh:run("./rebar -v update-deps", []),
    {ok, _} = retest_sh:run("./rebar -v compile", []),
    os:cmd("cp a3.rebar.config apps/a1/rebar.config"),
    {ok, _} = retest_sh:run("./rebar -v update-deps", []),
    {ok, _} = retest_sh:run("./rebar -v compile", []),
    os:cmd("cp a4.rebar.config apps/a1/rebar.config"),
    {ok, _} = retest_sh:run("./rebar -v update-deps", []),
    {ok, _} = retest_sh:run("./rebar -v compile", []),
    ok.

%%
%% Generate the contents of a simple .app file
%%
app(Name, Modules, Version) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, Version},
            {modules, Modules},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).
