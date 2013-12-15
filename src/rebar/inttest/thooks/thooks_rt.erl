-module(thooks_rt).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

files() ->
    [
     %% dummy lfe files
     {copy, "../../rebar", "rebar"},
     {copy, "rebar.config", "rebar.config"},
     {copy, "fish.erl", "src/fish.erl"},
     {create, "ebin/fish.app", app(fish, [fish])}
    ].

run(_Dir) ->
    ?assertMatch({ok, _}, retest_sh:run("./rebar -v clean compile", [])),
    ensure_command_ran_only_once("preclean"),
    ensure_command_ran_only_once("precompile"),
    ensure_command_ran_only_once("postclean"),
    ensure_command_ran_only_once("postcompile"),
    ok.

ensure_command_ran_only_once(Command) ->
    File = Command ++ ".out",
    ?assert(filelib:is_regular(File)),
    %% ensure that this command only ran once (not for each module)
    {ok, Content} = file:read_file(File),
    ?assertEqual(Command ++ "\n", binary_to_list(Content)).

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
