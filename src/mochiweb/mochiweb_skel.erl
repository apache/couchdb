-module(mochiweb_skel).
-export([skelcopy/2]).

-include_lib("kernel/include/file.hrl").

%% External API

skelcopy(DestDir, Name) ->
    ok = ensuredir(DestDir),
    LDst = case length(filename:dirname(DestDir)) of
               1 -> %% handle case when dirname returns "/"
                   0;
               N ->
                   N + 1
           end,
    skelcopy(src(), DestDir, Name, LDst),
    DestLink = filename:join([DestDir, Name, "deps", "mochiweb-src"]),
    ok = filelib:ensure_dir(DestLink),
    ok = file:make_symlink(
           filename:join(filename:dirname(code:which(?MODULE)), ".."),
           DestLink).

%% Internal API

src() ->
    Dir = filename:dirname(code:which(?MODULE)),
    filename:join(Dir, "../priv/skel").

skel() ->
    "skel".

skelcopy(Src, DestDir, Name, LDst) ->
    Dest = re:replace(filename:basename(Src), skel(), Name,
                      [global, {return, list}]),
    case file:read_file_info(Src) of
        {ok, #file_info{type=directory, mode=Mode}} ->
            Dir = DestDir ++ "/" ++ Dest,
            EDst = lists:nthtail(LDst, Dir),
            ok = ensuredir(Dir),
            ok = file:write_file_info(Dir, #file_info{mode=Mode}),
            case filename:basename(Src) of
                "ebin" ->
                    ok;
                _ ->
                    {ok, Files} = file:list_dir(Src),
                    io:format("~s/~n", [EDst]),
                    lists:foreach(fun ("." ++ _) -> ok;
                                      (F) ->
                                          skelcopy(filename:join(Src, F),
                                                   Dir,
                                                   Name,
                                                   LDst)
                                  end,
                                  Files),
                        ok
            end;
        {ok, #file_info{type=regular, mode=Mode}} ->
            OutFile = filename:join(DestDir, Dest),
            {ok, B} = file:read_file(Src),
            S = re:replace(binary_to_list(B), skel(), Name,
                           [{return, list}, global]),
            ok = file:write_file(OutFile, list_to_binary(S)),
            ok = file:write_file_info(OutFile, #file_info{mode=Mode}),
            io:format("    ~s~n", [filename:basename(Src)]),
            ok;
        {ok, _} ->
            io:format("ignored source file: ~p~n", [Src]),
            ok
    end.

ensuredir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        E ->
            E
    end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
