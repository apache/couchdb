-module(mango_op_build_info).

-export([
    run/2
]).


-include("mango.hrl").


run(_Props, _Ctx) ->
    Version = get_version(),
    {_, SysInfo} = os:type(),
    VerArray = <<"[2,8,10]">>,
    Reply = {[
        {<<"version">>, Version},
        {<<"gitVersion">>, <<"N/A">>},
        {<<"sysInfo">>, atom_to_binary(SysInfo, latin1)},
        {<<"loaderFlags">>, <<"loaderFlags">>},
        {<<"compilerFlags">>, <<"compilerFlags">>},
        {<<"allocator">>, <<"allocator">>},
        {<<"versionArray">>, VerArray},
        {<<"javascriptingEngine">>, <<"V8">>},
        {<<"bits">>, 1},
        {<<"debug">>, false},
        {<<"maxBsonObjectSize">>, 16777216},
        {<<"ok">>, 1}
    ]},
    {ok, Reply, _Ctx}.

get_version() ->
    Releases = release_handler:which_releases(),
    Version = case [V || {"dbcore", V, _, current} <- Releases] of
    [] ->
        case [V || {"dbcore", V, _, permanent} <- Releases] of
        [] ->
            "dev";
        [Permanent] ->
            Permanent
        end;
    [Current] ->
        Current
    end,
    list_to_binary(Version).

