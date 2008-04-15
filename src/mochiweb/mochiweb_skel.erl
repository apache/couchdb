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
    ok = file:make_symlink(
        filename:join(filename:dirname(code:which(?MODULE)), ".."),
        filename:join([DestDir, Name, "deps", "mochiweb-src"])).
    

%% Internal API

src() ->
    Dir = filename:dirname(code:which(?MODULE)),
    filename:join(Dir, "../priv/skel").

skel() ->
    "skel".

skelcopy(Src, DestDir, Name, LDst) ->
    {ok, Dest, _} = regexp:gsub(filename:basename(Src), skel(), Name),
    case file:read_file_info(Src) of
        {ok, #file_info{type=directory, mode=Mode}} ->
            Dir = DestDir ++ "/" ++ Dest,
            EDst = lists:nthtail(LDst, Dir),
            ok = ensuredir(Dir),
            ok = file:write_file_info(Dir, #file_info{mode=Mode}),
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
            ok;
        {ok, #file_info{type=regular, mode=Mode}} ->
            OutFile = filename:join(DestDir, Dest),
            {ok, B} = file:read_file(Src),
            {ok, S, _} = regexp:gsub(binary_to_list(B), skel(), Name),
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
