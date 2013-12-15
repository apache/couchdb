-module(rgen1_rt).

-compile(export_all).

%% Exercise release generation w/ templating

files() ->
    [
     {copy, "reltool.config"},
     {copy, "test.config"},
     {copy, "vars.config"},
     {copy, "../../rebar"}
    ].

run(_Dir) ->
    {ok, _} = retest_sh:run("./rebar -v generate", []),
    true = filelib:is_dir("mytarget"),
    ok.
