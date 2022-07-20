-module(ic_prv_setup_eunit).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, setup_eunit).
-define(NAMESPACE, ic).
-define(CWD, filename:absname(rebar_dir:get_cwd())).
-define(TEMPLATE, "setup_eunit").
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {namespace, ?NAMESPACE},
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 ic setup_eunit"},
        {opts, [{force, $f, "force", undefined, "overwrite existing files"}]},
        {short_desc, "Setup eunit from template"},
        {desc, "Setup eunit from template"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    build_eunit_config(State),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================
build_eunit_config(State) ->
    Force = is_forced(State),
    DataDir = ?CWD ++ "/tmp/data",
    TmpDataDir = ?CWD ++ "/tmp/tmp_data",
    cleanup_dirs([DataDir, TmpDataDir]),
    AllOpts = [
        {template, ?TEMPLATE},
        {prefix, ?CWD},
        {data_dir, DataDir},
        {view_index_dir, DataDir},
        {geo_index_dir, DataDir},
        {state_dir, TmpDataDir}
    ],
    StateNew = lists:foldl(
        fun({Key, Value}, StateAcc) ->
            rebar_state:set(StateAcc, Key, Value)
        end,
        State,
        AllOpts
    ),
    ok = rebar_templater:new(?TEMPLATE, [], Force, StateNew).

is_forced(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(force, Args) of
        undefined -> false;
        _ -> true
    end.

cleanup_dirs(Dirs) ->
    [del_dir(Dir) || Dir <- Dirs, filelib:is_dir(Dir)].

del_dir(Dir) ->
    All = filelib:wildcard(Dir ++ "/**"),
    {Dirs, Files} = lists:partition(fun filelib:is_dir/1, All),
    lists:foreach(fun file:delete/1, Files),
    SortedDirs = lists:sort(fun(A, B) -> length(A) > length(B) end, Dirs),
    lists:foreach(fun file:del_dir/1, SortedDirs),
    file:del_dir(Dir).
