%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2010 Mochi Media, Inc.

%% @doc Create temporary files and directories. Requires crypto to be started.

-module(mochitemp).
-export([gettempdir/0]).
-export([mkdtemp/0, mkdtemp/3]).
-export([rmtempdir/1]).
%% -export([mkstemp/4]).
-define(SAFE_CHARS, {$a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m,
                     $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
                     $A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M,
                     $N, $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
                     $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $_}).
-define(TMP_MAX, 10000).

-include_lib("kernel/include/file.hrl").

%% TODO: An ugly wrapper over the mktemp tool with open_port and sadness?
%%       We can't implement this race-free in Erlang without the ability
%%       to issue O_CREAT|O_EXCL. I suppose we could hack something with
%%       mkdtemp, del_dir, open.
%% mkstemp(Suffix, Prefix, Dir, Options) ->
%%    ok.

rmtempdir(Dir) ->
    case file:del_dir(Dir) of
        {error, eexist} ->
            ok = rmtempdirfiles(Dir),
            ok = file:del_dir(Dir);
        ok ->
            ok
    end.

rmtempdirfiles(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    ok = rmtempdirfiles(Dir, Files).

rmtempdirfiles(_Dir, []) ->
    ok;
rmtempdirfiles(Dir, [Basename | Rest]) ->
    Path = filename:join([Dir, Basename]),
    case filelib:is_dir(Path) of
        true ->
            ok = rmtempdir(Path);
        false ->
            ok = file:delete(Path)
    end,
    rmtempdirfiles(Dir, Rest).

mkdtemp() ->
    mkdtemp("", "tmp", gettempdir()).

mkdtemp(Suffix, Prefix, Dir) ->
    mkdtemp_n(rngpath_fun(Suffix, Prefix, Dir), ?TMP_MAX).



mkdtemp_n(RngPath, 1) ->
    make_dir(RngPath());
mkdtemp_n(RngPath, N) ->
    try make_dir(RngPath())
    catch throw:{error, eexist} ->
            mkdtemp_n(RngPath, N - 1)
    end.

make_dir(Path) ->
    case file:make_dir(Path) of
        ok ->
            ok;
        E={error, eexist} ->
            throw(E)
    end,
    %% Small window for a race condition here because dir is created 777
    ok = file:write_file_info(Path, #file_info{mode=8#0700}),
    Path.

rngpath_fun(Prefix, Suffix, Dir) ->
    fun () ->
            filename:join([Dir, Prefix ++ rngchars(6) ++ Suffix])
    end.

rngchars(0) ->
    "";
rngchars(N) ->
    [rngchar() | rngchars(N - 1)].

rngchar() ->
    rngchar(crypto:rand_uniform(0, tuple_size(?SAFE_CHARS))).

rngchar(C) ->
    element(1 + C, ?SAFE_CHARS).

%% @spec gettempdir() -> string()
%% @doc Get a usable temporary directory using the first of these that is a directory:
%%      $TMPDIR, $TMP, $TEMP, "/tmp", "/var/tmp", "/usr/tmp", ".".
gettempdir() ->
    gettempdir(gettempdir_checks(), fun normalize_dir/1).

gettempdir_checks() ->
    [{fun os:getenv/1, ["TMPDIR", "TMP", "TEMP"]},
     {fun gettempdir_identity/1, ["/tmp", "/var/tmp", "/usr/tmp"]},
     {fun gettempdir_cwd/1, [cwd]}].

gettempdir_identity(L) ->
    L.

gettempdir_cwd(cwd) ->
    {ok, L} = file:get_cwd(),
    L.

gettempdir([{_F, []} | RestF], Normalize) ->
    gettempdir(RestF, Normalize);
gettempdir([{F, [L | RestL]} | RestF], Normalize) ->
    case Normalize(F(L)) of
        false ->
            gettempdir([{F, RestL} | RestF], Normalize);
        Dir ->
            Dir
    end.

normalize_dir(False) when False =:= false orelse False =:= "" ->
    %% Erlang doesn't have an unsetenv, wtf.
    false;
normalize_dir(L) ->
    Dir = filename:absname(L),
    case filelib:is_dir(Dir) of
        false ->
            false;
        true ->
            Dir
    end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
pushenv(L) ->
    [{K, os:getenv(K)} || K <- L].
popenv(L) ->
    F = fun ({K, false}) ->
                %% Erlang doesn't have an unsetenv, wtf.
                os:putenv(K, "");
            ({K, V}) ->
                os:putenv(K, V)
        end,
    lists:foreach(F, L).

gettempdir_fallback_test() ->
    ?assertEqual(
       "/",
       gettempdir([{fun gettempdir_identity/1, ["/--not-here--/"]},
                   {fun gettempdir_identity/1, ["/"]}],
                  fun normalize_dir/1)),
    ?assertEqual(
       "/",
       %% simulate a true os:getenv unset env
       gettempdir([{fun gettempdir_identity/1, [false]},
                   {fun gettempdir_identity/1, ["/"]}],
                  fun normalize_dir/1)),
    ok.

gettempdir_identity_test() ->
    ?assertEqual(
       "/",
       gettempdir([{fun gettempdir_identity/1, ["/"]}], fun normalize_dir/1)),
    ok.

gettempdir_cwd_test() ->
    {ok, Cwd} = file:get_cwd(),
    ?assertEqual(
       normalize_dir(Cwd),
       gettempdir([{fun gettempdir_cwd/1, [cwd]}], fun normalize_dir/1)),
    ok.

rngchars_test() ->
    crypto:start(),
    ?assertEqual(
       "",
       rngchars(0)),
    ?assertEqual(
       10,
       length(rngchars(10))),
    ok.

rngchar_test() ->
    ?assertEqual(
       $a,
       rngchar(0)),
    ?assertEqual(
       $A,
       rngchar(26)),
    ?assertEqual(
       $_,
       rngchar(62)),
    ok.

mkdtemp_n_failonce_test() ->
    crypto:start(),
    D = mkdtemp(),
    Path = filename:join([D, "testdir"]),
    %% Toggle the existence of a dir so that it fails
    %% the first time and succeeds the second.
    F = fun () ->
                case filelib:is_dir(Path) of
                    true ->
                        file:del_dir(Path);
                    false ->
                        file:make_dir(Path)
                end,
                Path
        end,
    try
        %% Fails the first time
        ?assertThrow(
           {error, eexist},
           mkdtemp_n(F, 1)),
        %% Reset state
        file:del_dir(Path),
        %% Succeeds the second time
        ?assertEqual(
           Path,
           mkdtemp_n(F, 2))
    after rmtempdir(D)
    end,
    ok.

mkdtemp_n_fail_test() ->
    {ok, Cwd} = file:get_cwd(),
    ?assertThrow(
       {error, eexist},
       mkdtemp_n(fun () -> Cwd end, 1)),
    ?assertThrow(
       {error, eexist},
       mkdtemp_n(fun () -> Cwd end, 2)),
    ok.

make_dir_fail_test() ->
    {ok, Cwd} = file:get_cwd(),
    ?assertThrow(
      {error, eexist},
      make_dir(Cwd)),
    ok.

mkdtemp_test() ->
    crypto:start(),
    D = mkdtemp(),
    ?assertEqual(
       true,
       filelib:is_dir(D)),
    ?assertEqual(
       ok,
       file:del_dir(D)),
    ok.

rmtempdir_test() ->
    crypto:start(),
    D1 = mkdtemp(),
    ?assertEqual(
       true,
       filelib:is_dir(D1)),
    ?assertEqual(
       ok,
       rmtempdir(D1)),
    D2 = mkdtemp(),
    ?assertEqual(
       true,
       filelib:is_dir(D2)),
    ok = file:write_file(filename:join([D2, "foo"]), <<"bytes">>),
    D3 = mkdtemp("suffix", "prefix", D2),
    ?assertEqual(
       true,
       filelib:is_dir(D3)),
    ok = file:write_file(filename:join([D3, "foo"]), <<"bytes">>),
    ?assertEqual(
       ok,
       rmtempdir(D2)),
    ?assertEqual(
       {error, enoent},
       file:consult(D3)),
    ?assertEqual(
       {error, enoent},
       file:consult(D2)),
    ok.

gettempdir_env_test() ->
    Env = pushenv(["TMPDIR", "TEMP", "TMP"]),
    FalseEnv = [{"TMPDIR", false}, {"TEMP", false}, {"TMP", false}],
    try
        popenv(FalseEnv),
        popenv([{"TMPDIR", "/"}]),
        ?assertEqual(
           "/",
           os:getenv("TMPDIR")),
        ?assertEqual(
           "/",
           gettempdir()),
        {ok, Cwd} = file:get_cwd(),
        popenv(FalseEnv),
        popenv([{"TMP", Cwd}]),
        ?assertEqual(
           normalize_dir(Cwd),
           gettempdir())
    after popenv(Env)
    end,
    ok.

-endif.
