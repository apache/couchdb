% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_cfile_tests).

-include_lib("couch/include/couch_eunit.hrl").

-define(CONCURRENT_READER_JITTER_MSEC, 5).

couch_cfile_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        case os:type() of
            {win32, _} ->
                [
                    ?TDEF_FE(t_unsupported)
                ];
            {_, _} ->
                [
                    ?TDEF_FE(t_basics),
                    ?TDEF_FE(t_pread_and_position),
                    ?TDEF_FE(t_pread_from_other_procesess),
                    ?TDEF_FE(t_write),
                    ?TDEF_FE(t_datasync),
                    ?TDEF_FE(t_position_and_truncate),
                    ?TDEF_FE(t_advise),
                    ?TDEF_FE(t_invalid_fd),
                    ?TDEF_FE(t_fd),
                    ?TDEF_FE(t_cannot_dup_cfile_handle),
                    ?TDEF_FE(t_gc_is_closing_file_handles),
                    ?TDEF_FE(t_monitor_is_closing_file_handles),
                    ?TDEF_FE(t_janitor_proc_is_up),
                    ?TDEF_FE(t_concurrent_reads_512b),
                    ?TDEF_FE(t_concurrent_reads_4kb),
                    ?TDEF_FE(t_concurrent_reads_1mb)
                ]
        end
    }.

setup() ->
    ?tempfile().

teardown(Path) ->
    catch file:delete(Path).

open_raw(Path) ->
    % Use the options couch_file is using
    {ok, Fd} = file:open(Path, [binary, append, raw, read]),
    ok = file:write(Fd, <<"abcd">>),
    Fd.

t_basics(Path) ->
    Fd = open_raw(Path),
    Res = couch_cfile:dup(Fd),
    ?assertMatch({ok, _}, Res),
    {ok, CFd} = Res,
    ?assertEqual({ok, <<"ab">>}, couch_cfile:pread(CFd, 0, 2)),
    file:close(Fd),
    % Check dup-ing a closed raw file descriptor
    ?assertEqual({error, einval}, couch_cfile:dup(Fd)),
    ?assertEqual({ok, [<<"ab">>]}, couch_cfile:pread(CFd, [{0, 2}])),
    ?assertMatch({ok, Int} when is_integer(Int), couch_cfile:fd(CFd)),
    ?assertEqual(ok, couch_cfile:close(CFd)),
    ?assertEqual({error, einval}, couch_cfile:pread(CFd, [{0, 2}])).

t_pread_and_position(Path) ->
    % Note: we'll be reading using 'file' module functions even for cfile
    % handles. We're specifically acting like an OTP file layer to ensure we
    % don't have to duplicate our couch_file code

    Fd = open_raw(Path),
    {ok, CFd} = couch_cfile:dup(Fd),

    % Check positions
    {ok, Eof} = file:position(Fd, eof),
    {ok, CFdEof} = file:position(CFd, eof),
    ?assertEqual(Eof, CFdEof),

    % Basic preads
    ?assertEqual({ok, []}, file:pread(CFd, [])),
    ?assertEqual({ok, [eof]}, file:pread(CFd, [{0, 0}])),
    ?assertEqual(eof, file:pread(CFd, 0, 0)),
    ?assertEqual({ok, [<<"a">>]}, file:pread(CFd, [{0, 1}])),
    ?assertEqual({ok, <<"a">>}, file:pread(CFd, 0, 1)),
    ?assertEqual({ok, <<"d">>}, file:pread(CFd, 3, 1)),
    ?assertEqual({ok, <<"d">>}, file:pread(CFd, 3, 2)),
    ?assertEqual({ok, [<<"a">>, eof]}, file:pread(CFd, [{0, 1}, {4, 1}])),
    ?assertEqual({error, einval}, couch_cfile:pread(junk, [{0, 1}])),
    ?assertEqual({error, badarg}, file:pread(CFd, junk)),

    % Most of all we care that behavior matches file:pread/1 for
    % any combination of valid/invalid/eof ranges
    [
        ?assertEqual(file:pread(Fd, P, L), file:pread(CFd, P, L))
     || P <- lists:seq(-1, Eof + 1) ++ [1 bsl 42],
        L <- lists:seq(-1, Eof + 1) ++ [1 bsl 42]
    ],

    % Positions and preads are updated after a write
    ok = file:write(Fd, <<"ef">>),

    % Check new positions
    {ok, Eof1} = file:position(Fd, eof),
    {ok, CFdEof1} = file:position(CFd, eof),
    ?assertEqual(Eof1, CFdEof1),

    [
        ?assertEqual(file:pread(Fd, P, L), file:pread(CFd, P, L))
     || P <- lists:seq(-1, Eof1 + 1), L <- lists:seq(-1, Eof1 + 1)
    ],

    % File truncation also is reflected in position and preads
    {ok, 4} = file:position(Fd, 4),
    ok = file:truncate(Fd),

    ?assertEqual({ok, 4}, file:position(Fd, eof)),
    ?assertEqual({ok, 4}, file:position(CFd, eof)),

    {ok, 3} = file:position(Fd, 3),
    {ok, 3} = file:position(CFd, 3),

    ok = file:truncate(CFd),

    ?assertEqual({ok, 3}, file:position(Fd, eof)),
    ?assertEqual({ok, 3}, file:position(CFd, eof)),

    [
        ?assertEqual(file:pread(Fd, [{P, L}]), file:pread(CFd, [{P, L}]))
     || P <- lists:seq(-1, 4), L <- lists:seq(-1, 4)
    ],

    % Test closing behavior
    ok = file:close(Fd),
    ?assertEqual({error, einval}, file:pread(Fd, 0, 1)),
    ?assertEqual({error, einval}, file:position(Fd, eof)),

    % Can still read from our dup-ed handle
    ?assertEqual({ok, <<"a">>}, file:pread(CFd, 0, 1)),
    ?assertEqual({ok, 3}, file:position(CFd, eof)),

    ok = file:close(CFd),
    ?assertEqual({error, einval}, file:pread(CFd, 0, 1)),
    ?assertEqual({error, einval}, file:position(CFd, eof)).

t_pread_from_other_procesess(Path) ->
    % Note: we'll be reading using 'file' module functions even for cfile
    % handles. We're specifically acting like an OTP file layer to ensure we
    % don't have to duplicate our couch_file code

    Fd = open_raw(Path),
    {ok, CFd} = couch_cfile:dup(Fd),

    Proc = spawn_proc(),

    {ok, Eof} = file:position(Fd, eof),

    ?assertEqual({ok, Eof}, file:position(CFd, eof)),
    ?assertEqual({ok, Eof}, proc_run(Proc, file, position, [CFd, eof])),

    % Closing original raw fd should still keep ours open and we should still
    % be able to read from it from this or other processes
    ok = file:close(Fd),

    ?assertEqual({ok, [<<"a">>]}, file:pread(CFd, [{0, 1}])),
    ?assertEqual({ok, [<<"a">>]}, proc_run(Proc, file, pread, [CFd, [{0, 1}]])),

    % Fd works from other process, just for completeness
    {ok, FdInt} = couch_cfile:fd(CFd),
    ?assertEqual({ok, FdInt}, proc_run(Proc, couch_cfile, fd, [CFd])),

    ok = file:close(CFd),
    ?assertEqual({error, einval}, file:pread(CFd, [{0, 1}])),
    ?assertEqual({error, einval}, proc_run(Proc, file, pread, [CFd, [{0, 1}]])),

    kill_proc(Proc).

t_datasync(Path) ->
    Fd = open_raw(Path),
    {ok, CFd} = couch_cfile:dup(Fd),
    ok = file:close(Fd),

    {ok, Pos} = file:position(CFd, eof),
    ?assertEqual(ok, file:datasync(CFd)),
    ?assertEqual(ok, file:write(CFd, <<"x">>)),
    ?assertEqual(ok, file:datasync(CFd)),
    {ok, Pos1} = file:position(CFd, eof),
    ?assertEqual(Pos + 1, Pos1),
    ?assertEqual({ok, <<"x">>}, file:pread(CFd, Pos, 1)),

    % Try something larger
    TwoMBs = <<<<"y">> || _ <- lists:seq(1, 1 bsl 21)>>,
    ?assertEqual(ok, file:write(CFd, TwoMBs)),
    ?assertEqual(ok, file:datasync(CFd)),
    {ok, Pos2} = file:position(CFd, eof),
    ?assertEqual(Pos1 + (1 bsl 21), Pos2),

    % 10 in a row
    lists:foreach(
        fun(_) ->
            ?assertEqual(ok, file:datasync(CFd))
        end,
        lists:seq(1, 10)
    ),

    % Others can't datasync
    Proc = spawn_proc(),
    Expect = {exc, error, not_on_controlling_process},
    ?assertEqual(Expect, proc_run(Proc, file, datasync, [CFd])),
    kill_proc(Proc),

    % Can't datasync after closing
    ok = file:close(CFd),
    ?assertEqual({error, einval}, file:datasync(CFd)).

t_write(Path) ->
    Fd = open_raw(Path),
    {ok, CFd} = couch_cfile:dup(Fd),
    ok = file:close(Fd),

    {ok, Pos} = file:position(CFd, eof),

    ?assertEqual(ok, file:write(CFd, <<"x">>)),
    {ok, Pos1} = file:position(CFd, eof),
    ?assertEqual(Pos + 1, Pos1),
    ?assertEqual({ok, <<"x">>}, file:pread(CFd, Pos, 1)),
    TwoMBs = <<<<"y">> || _ <- lists:seq(1, 1 bsl 21)>>,
    ?assertEqual(ok, file:write(CFd, TwoMBs)),
    {ok, Pos2} = file:position(CFd, eof),
    ?assertEqual(Pos1 + (1 bsl 21), Pos2),

    {ok, ReadTwoMBs} = file:pread(CFd, Pos1, 1 bsl 21),
    ?assertEqual(byte_size(TwoMBs), byte_size(ReadTwoMBs)),
    ?assertEqual(TwoMBs, ReadTwoMBs),

    % Others can't write
    Proc = spawn_proc(),
    Expect = {exc, error, not_on_controlling_process},
    ?assertEqual(Expect, proc_run(Proc, file, write, [CFd, <<"y">>])),
    kill_proc(Proc),

    % Can't write after closing
    ?assertEqual(ok, file:close(CFd)),
    ?assertEqual({error, einval}, file:write(CFd, <<"z">>)).

t_position_and_truncate(Path) ->
    Fd = open_raw(Path),
    {ok, CFd} = couch_cfile:dup(Fd),
    ok = file:close(Fd),

    {ok, Pos} = file:position(CFd, eof),
    ?assert(Pos > 0),
    ?assertEqual({ok, 0}, file:position(CFd, 0)),
    ?assertEqual(ok, file:truncate(CFd)),
    {ok, Pos1} = file:position(CFd, eof),
    ?assertEqual(eof, file:pread(CFd, 0, 1)),
    ?assertEqual(Pos1, 0),

    ok = file:write(CFd, <<"abc">>),
    ?assertEqual({ok, 1}, file:position(CFd, 1)),
    ?assertEqual(ok, file:truncate(CFd)),
    ?assertEqual({ok, <<"a">>}, file:pread(CFd, 0, 10)),

    Proc = spawn_proc(),

    % Others can't do absolute position changes or truncate
    Expect = {exc, error, not_on_controlling_process},
    ?assertEqual(Expect, proc_run(Proc, file, position, [CFd, 1])),
    ?assertEqual(Expect, proc_run(Proc, file, truncate, [CFd])),

    % Others can call position(Fd, eof) to get the file size
    ?assertEqual({ok, 1}, proc_run(Proc, file, position, [CFd, eof])),

    kill_proc(Proc),

    % After closing, can't truncate or position
    ok = file:close(CFd),
    ?assertEqual({error, einval}, file:position(CFd, 42)),
    ?assertEqual({error, einval}, file:truncate(CFd)).

t_advise(Path) ->
    % This is one optional so not implemented as a nif
    % we just check that it behaves reasonably
    Fd = open_raw(Path),
    {ok, CFd} = couch_cfile:dup(Fd),
    ok = file:close(Fd),

    ?assertEqual(ok, file:advise(CFd, 42, 42, dont_need)),

    % Others can't call it
    Proc = spawn_proc(),
    Expect = {exc, error, not_on_controlling_process},
    Args = [CFd, 42, 42, dont_need],
    ?assertEqual(Expect, proc_run(Proc, file, advise, Args)),
    kill_proc(Proc).

t_invalid_fd(_Path) ->
    ?assertEqual({error, einval}, couch_cfile:dup(junk)),
    ?assertEqual({error, einval}, couch_cfile:pread(junk, 1, 1)),
    ?assertEqual({error, einval}, couch_cfile:close(junk)),
    ?assertEqual({error, einval}, couch_cfile:fd(junk)),
    ?assertEqual({error, einval}, couch_cfile:position(junk, eof)).

t_fd(Path) ->
    Fd = open_raw(Path),
    {ok, CFd} = couch_cfile:dup(Fd),

    {ok, FdInt} = couch_cfile:fd(Fd),
    {ok, CFdInt} = couch_cfile:fd(CFd),
    ?assert(is_integer(FdInt) andalso FdInt > -1),
    ?assert(is_integer(CFdInt) andalso CFdInt > -1),

    ?assertEqual({error, einval}, couch_cfile:fd(potato)),

    % We can't say a whole lot more just that both are
    % not equal since they are both open and one is dup-ed
    % from the other
    ?assertNotEqual(FdInt, CFdInt),

    ok = file:close(Fd),
    ok = file:close(CFd),

    % Here we check our sanity-checker: after handles are closed we cannot get
    % any access to them. In the sanity checker we access the int fds after
    % dup-ing in order to assert that we still have access to the same file
    % handles we started with.
    ?assertEqual({error, einval}, couch_cfile:fd(Fd)),
    ?assertEqual({error, einval}, couch_cfile:fd(CFd)).

t_janitor_proc_is_up(Path) ->
    Fd = open_raw(Path),
    {ok, CFd} = couch_cfile:dup(Fd),
    couch_cfile:close(CFd),
    ok = file:close(Fd),
    ?assertEqual(true, is_process_alive(whereis(couch_cfile))).

t_unsupported(Fd) ->
    ?assertEqual({error, einval}, couch_cfile:dup(Fd)),
    ?assertEqual({error, einval}, couch_cfile:pread(Fd, 1, 1)),
    ?assertEqual({error, einval}, couch_cfile:close(Fd)),
    ?assertEqual({error, einval}, couch_cfile:fd(Fd)),
    ?assertEqual({error, einval}, couch_cfile:position(Fd, eof)).

t_cannot_dup_cfile_handle(Path) ->
    Fd = open_raw(Path),
    {ok, CFd0} = couch_cfile:dup(Fd),
    ok = file:close(Fd),
    ?assertEqual({error, einval}, couch_cfile:dup(CFd0)).

t_gc_is_closing_file_handles(Path) ->
    Fd = open_raw(Path),
    {ok, FdInt} = couch_cfile:fd(Fd),
    % Since we'll be checking the janitor, send it some junk message
    % it should cope with them by dropping them (like the OTP one)
    whereis(couch_cfile) ! {some_junk, message},
    Cnt = 750,
    {_, Ref} = spawn_monitor(fun() ->
        Fd1 = open_raw(Path),
        lists:foreach(fun(_) -> couch_cfile:dup(Fd1) end, lists:seq(1, Cnt))
    end),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    end,
    % According the dup() docs:
    %
    % "The new file descriptor number is guaranteed to be the lowest-numbered
    % file descriptor that was unused in the calling process."
    %
    % Unless during the test something else opened another Cnt descriptors, if
    % we open another one we should get something lower than FdInt + Cnt
    {ok, Fd2} = couch_cfile:dup(Fd),
    {ok, FdInt1} = couch_cfile:fd(Fd2),
    ?assert(FdInt1 =< FdInt + Cnt),
    ok = file:close(Fd),
    ok = file:close(Fd2).

t_monitor_is_closing_file_handles(Path) ->
    Proc = spawn_proc(),
    {ok, Fd} = proc_run(Proc, file, open, [Path, [binary, append, raw, read]]),
    ?assertError(not_on_controlling_process, couch_cfile:dup(Fd)),
    {ok, CFd} = proc_run(Proc, couch_cfile, dup, [Fd]),
    ?assertEqual(eof, file:pread(CFd, 0, 1)),
    kill_proc(Proc),
    % Make sure to wait until it's closed
    test_util:wait_value(fun() -> file:pread(CFd, 0, 1) end, {error, einval}),
    ?assertEqual({error, einval}, file:pread(CFd, 0, 1)).

t_concurrent_reads_512b(Path) ->
    Fd = cfile(Path),
    Eof = write(Fd, 0, 512),
    ReadersPidRefs = spawn_readers(20, Fd, Eof),
    timer:sleep(1000),
    [Pid ! stop_reading || {Pid, _} <- ReadersPidRefs],
    Count = gather_read_results(ReadersPidRefs, 0),
    ?assert(is_integer(Count) andalso Count > 1000).

t_concurrent_reads_4kb(Path) ->
    Fd = cfile(Path),
    Eof = write(Fd, 0, 4096),
    ReadersPidRefs = spawn_readers(10, Fd, Eof),
    timer:sleep(1000),
    [Pid ! stop_reading || {Pid, _} <- ReadersPidRefs],
    Count = gather_read_results(ReadersPidRefs, 0),
    ?assert(is_integer(Count) andalso Count > 100).

t_concurrent_reads_1mb(Path) ->
    Fd = cfile(Path),
    Eof = write(Fd, 0, 1048576),
    ReadersPidRefs = spawn_readers(2, Fd, Eof),
    timer:sleep(1000),
    [Pid ! stop_reading || {Pid, _} <- ReadersPidRefs],
    Count = gather_read_results(ReadersPidRefs, 0),
    ?assert(is_integer(Count) andalso Count > 10).

spawn_proc() ->
    {Pid, Ref} = spawn_monitor(fun proc_loop/0),
    {Pid, Ref}.

proc_run({Pid, _Ref}, M, F, A) ->
    Pid ! {do, self(), {M, F, A}},
    receive
        {did, Res} ->
            Res
    end.

kill_proc({Pid, Ref}) ->
    exit(Pid, kill),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    end.

proc_loop() ->
    receive
        {do, From, {M, F, A}} ->
            Res =
                try
                    apply(M, F, A)
                catch
                    T:E -> {exc, T, E}
                end,
            From ! {did, Res},
            proc_loop()
    end.

% Concurrent reader helpers

cfile(Path) ->
    {ok, RawFd} = file:open(Path, [binary, append, raw, read]),
    {ok, Fd} = couch_cfile:dup(RawFd),
    ok = file:close(RawFd),
    Fd.

spawn_readers(N, Fd, Eof) ->
    spawn_readers(N, Fd, Eof, []).

spawn_readers(0, _Fd, _Eof, Acc) ->
    Acc;
spawn_readers(N, Fd, Eof, Acc) ->
    {Pid, Ref} = spawn_monitor(fun() -> reader(Fd, Eof, 0) end),
    spawn_readers(N - 1, Fd, Eof, [{Pid, Ref} | Acc]).

reader(Fd, Eof, Count) ->
    Wait = rand:uniform(?CONCURRENT_READER_JITTER_MSEC) - 1,
    case Wait of
        W when W =< 2 ->
            % If wait is too low, just use erlang:yield()
            erlang:yield(),
            pread_and_verify(Fd, Eof),
            reader(Fd, Eof, Count + 1);
        _ ->
            receive
                stop_reading ->
                    exit({shutdown, {read_results, Count}})
            after Wait ->
                pread_and_verify(Fd, Eof),
                reader(Fd, Eof, Count + 1)
            end
    end.

gather_read_results([], Acc) ->
    Acc;
gather_read_results([{Pid, Ref} | Rest], Acc) ->
    Acc1 =
        receive
            {'DOWN', Ref, _, _, {shutdown, {read_results, Count}}} ->
                Acc + Count;
            {'DOWN', Ref, _, _, Other} ->
                error({preader_crashed, Pid, Other})
        end,
    gather_read_results(Rest, Acc1).

% Use a simple scheme: byte at position X should be have value X rem 256
%

write(Fd, Pos, Len) ->
    Bin = <<<<(I rem 256)>> || I <- lists:seq(Pos, Pos + Len - 1)>>,
    %sanity check
    ?assertEqual(Len, byte_size(Bin)),
    ok = file:write(Fd, Bin),
    ?assertEqual({ok, Pos + Len}, file:position(Fd, eof)),
    Pos + Len.

pread_and_verify(Fd, Eof) ->
    Pos = rand:uniform(Eof + 1) - 1,
    Len = rand:uniform(Eof + 1) - 1,
    case {Pos, Len} of
        {P, _} when P >= Eof ->
            ?assertEqual(eof, file:pread(Fd, Pos, Len));
        {_, 0} ->
            ?assertEqual(eof, file:pread(Fd, Pos, Len));
        {P, L} when P + L =< Eof ->
            {ok, Bin} = file:pread(Fd, Pos, Len),
            ?assert(is_binary(Bin)),
            ?assertEqual(Len, byte_size(Bin)),
            verify_binary(Pos, Bin);
        {P, L} when P + L > Eof ->
            {ok, Bin} = file:pread(Fd, Pos, Len),
            ?assertEqual(Eof - Pos, byte_size(Bin)),
            verify_binary(Pos, Bin)
    end.

verify_binary(_, <<>>) ->
    ok;
verify_binary(Pos, <<Byte:8, Rest/binary>>) ->
    ?assertEqual(Pos rem 256, Byte),
    verify_binary(Pos + 1, Rest).
