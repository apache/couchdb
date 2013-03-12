%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2010 Mochi Media, Inc.

%% @doc Write newline delimited log files, ensuring that if a truncated
%%      entry is found on log open then it is fixed before writing. Uses
%%      delayed writes and raw files for performance.
-module(mochilogfile2).
-author('bob@mochimedia.com').

-export([open/1, write/2, close/1, name/1]).

%% @spec open(Name) -> Handle
%% @doc Open the log file Name, creating or appending as necessary. All data
%%      at the end of the file will be truncated until a newline is found, to
%%      ensure that all records are complete.
open(Name) ->
    {ok, FD} = file:open(Name, [raw, read, write, delayed_write, binary]),
    fix_log(FD),
    {?MODULE, Name, FD}.

%% @spec name(Handle) -> string()
%% @doc Return the path of the log file.
name({?MODULE, Name, _FD}) ->
    Name.

%% @spec write(Handle, IoData) -> ok
%% @doc Write IoData to the log file referenced by Handle.
write({?MODULE, _Name, FD}, IoData) ->
    ok = file:write(FD, [IoData, $\n]),
    ok.

%% @spec close(Handle) -> ok
%% @doc Close the log file referenced by Handle.
close({?MODULE, _Name, FD}) ->
    ok = file:sync(FD),
    ok = file:close(FD),
    ok.

fix_log(FD) ->
    {ok, Location} = file:position(FD, eof),
    Seek = find_last_newline(FD, Location),
    {ok, Seek} = file:position(FD, Seek),
    ok = file:truncate(FD),
    ok.

%% Seek backwards to the last valid log entry
find_last_newline(_FD, N) when N =< 1 ->
    0;
find_last_newline(FD, Location) ->
    case file:pread(FD, Location - 1, 1) of
	{ok, <<$\n>>} ->
            Location;
	{ok, _} ->
	    find_last_newline(FD, Location - 1)
    end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
name_test() ->
    D = mochitemp:mkdtemp(),
    FileName = filename:join(D, "open_close_test.log"),
    H = open(FileName),
    ?assertEqual(
       FileName,
       name(H)),
    close(H),
    file:delete(FileName),
    file:del_dir(D),
    ok.

open_close_test() ->
    D = mochitemp:mkdtemp(),
    FileName = filename:join(D, "open_close_test.log"),
    OpenClose = fun () ->
                        H = open(FileName),
                        ?assertEqual(
                           true,
                           filelib:is_file(FileName)),
                        ok = close(H),
                        ?assertEqual(
                           {ok, <<>>},
                           file:read_file(FileName)),
                        ok
                end,
    OpenClose(),
    OpenClose(),
    file:delete(FileName),
    file:del_dir(D),
    ok.

write_test() ->
    D = mochitemp:mkdtemp(),
    FileName = filename:join(D, "write_test.log"),
    F = fun () ->
                H = open(FileName),
                write(H, "test line"),
                close(H),
                ok
        end,
    F(),
    ?assertEqual(
       {ok, <<"test line\n">>},
       file:read_file(FileName)),
    F(),
    ?assertEqual(
       {ok, <<"test line\ntest line\n">>},
       file:read_file(FileName)),
    file:delete(FileName),
    file:del_dir(D),
    ok.

fix_log_test() ->
    D = mochitemp:mkdtemp(),
    FileName = filename:join(D, "write_test.log"),
    file:write_file(FileName, <<"first line good\nsecond line bad">>),
    F = fun () ->
                H = open(FileName),
                write(H, "test line"),
                close(H),
                ok
        end,
    F(),
    ?assertEqual(
       {ok, <<"first line good\ntest line\n">>},
       file:read_file(FileName)),
    file:write_file(FileName, <<"first line bad">>),
    F(),
    ?assertEqual(
       {ok, <<"test line\n">>},
       file:read_file(FileName)),
    F(),
    ?assertEqual(
       {ok, <<"test line\ntest line\n">>},
       file:read_file(FileName)),
    ok.

-endif.
