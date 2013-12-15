-module(testplugin_mod).
-compile(export_all).

pre_compile(Config, _) ->
    ok = file:write_file("pre.compile", <<"Yadda!">>),
    rebar_log:log(info, "Wrote ~p/pre.compile~n", [rebar_utils:get_cwd()]).
