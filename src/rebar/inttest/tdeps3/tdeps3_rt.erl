-module(tdeps3_rt).

-compile(export_all).

%% Exercise transitive dependencies where there are multiple files
%% depending on the same set of deps as well as lib_dir directives
%% A -> B -> C -> D -> E
%%      |--> G(via lib_dir)
%% |--> F -> D -> E

files() ->
    [
     %% A1 application
     {create, "ebin/a.app", app(a, [a])},
     {template, "a.erl", "src/a.erl", dict:from_list([{module, a}, {dep, b}])},

     {copy, "a.rebar.config", "rebar.config"},
     {copy, "../../rebar", "rebar"},

     %% B application
     {create, "repo/b/ebin/b.app", app(b, [b])},
     {template, "a.erl", "repo/b/src/b.erl", dict:from_list([{module, b}, {dep, b}])},
     {copy, "b.rebar.config", "repo/b/rebar.config"},
     {copy, "b.hrl", "repo/b/include/b.hrl"},

     %% C application
     {create, "repo/c/ebin/c.app", app(c, [c])},
     {template, "a.erl", "repo/c/src/c.erl", dict:from_list([{module, c}, {dep, d}])},
     {copy, "c.rebar.config", "repo/c/rebar.config"},
     {copy, "c.hrl", "repo/c/include/c.hrl"},

     %% D application
     {create, "repo/d/ebin/d.app", app(d, [d])},
     {template, "a.erl", "repo/d/src/d.erl", dict:from_list([{module, d}, {dep, e}])},
     {copy, "d.rebar.config", "repo/d/rebar.config"},
     {copy, "d.hrl", "repo/d/include/d.hrl"},

     %% E application
     {create, "repo/e/ebin/e.app", app(e, [])},
     {copy, "e.hrl", "repo/e/include/e.hrl"},


     %% F application
     {create, "repo/f/ebin/f.app", app(f, [f])},
     {template, "a.erl", "repo/f/src/f.erl", dict:from_list([{module, f}, {dep, d}])},
     {copy, "c.rebar.config", "repo/f/rebar.config"},
     {copy, "f.hrl", "repo/f/include/f.hrl"},

     %% G application, which is part of the B repo, in a lib_dir
     {create, "repo/b/apps/g/ebin/g.app", app(g, [])},
     {copy, "e.hrl", "repo/b/apps/g/include/g.hrl"}

    ].

apply_cmds([], _Params) ->
    ok;
apply_cmds([Cmd | Rest], Params) ->
    io:format("Running: ~s (~p)\n", [Cmd, Params]),
    {ok, _} = retest_sh:run(Cmd, Params),
    apply_cmds(Rest, Params).

run(_Dir) ->
    %% Initialize the b/c apps as git repos so that dependencies pull
    %% properly
    GitCmds = ["git init",
               "git add -A",
               "git config user.email 'tdeps@example.com'",
               "git config user.name 'tdeps'",
               "git commit -a -m 'Initial Commit'"],
    ok = apply_cmds(GitCmds, [{dir, "repo/b"}]),
    ok = apply_cmds(GitCmds, [{dir, "repo/c"}]),
    ok = apply_cmds(GitCmds, [{dir, "repo/d"}]),
    ok = apply_cmds(GitCmds, [{dir, "repo/e"}]),
    ok = apply_cmds(GitCmds, [{dir, "repo/f"}]),

    {ok, _} = retest_sh:run("./rebar -v get-deps compile", []),
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
