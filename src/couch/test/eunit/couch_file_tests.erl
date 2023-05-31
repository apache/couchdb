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

-module(couch_file_tests).

-include_lib("couch/include/couch_eunit.hrl").

-define(BLOCK_SIZE, 4096).
-define(setup(F), {setup, fun setup/0, fun teardown/1, F}).
-define(foreach(Fs), {foreach, fun setup/0, fun teardown/1, Fs}).

setup() ->
    {ok, Fd} = couch_file:open(?tempfile(), [create, overwrite]),
    Fd.

teardown(Fd) ->
    case is_process_alive(Fd) of
        true -> ok = couch_file:close(Fd);
        false -> ok
    end.

open_close_test_() ->
    {
        "Test for proper file open and close",
        {
            setup,
            fun() -> test_util:start(?MODULE, [ioq]) end,
            fun test_util:stop/1,
            [
                should_return_enoent_if_missed(),
                should_ignore_invalid_flags_with_open(),
                ?setup(fun should_return_pid_on_file_open/1),
                should_close_file_properly(),
                ?setup(fun should_create_empty_new_files/1)
            ]
        }
    }.

should_return_enoent_if_missed() ->
    ?_assertEqual({error, enoent}, couch_file:open("not a real file")).

should_ignore_invalid_flags_with_open() ->
    ?_assertMatch(
        {ok, _},
        couch_file:open(?tempfile(), [create, invalid_option])
    ).

should_return_pid_on_file_open(Fd) ->
    ?_assert(is_pid(Fd)).

should_close_file_properly() ->
    {ok, Fd} = couch_file:open(?tempfile(), [create, overwrite]),
    ok = couch_file:close(Fd),
    ?_assert(true).

should_create_empty_new_files(Fd) ->
    ?_assertMatch({ok, 0}, couch_file:bytes(Fd)).

read_write_test_() ->
    {
        "Common file read/write tests",
        {
            setup,
            fun() -> test_util:start_couch() end,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(should_increase_file_size_on_write),
                    ?TDEF_FE(should_return_current_file_size_on_write),
                    ?TDEF_FE(should_write_and_read_term),
                    ?TDEF_FE(should_write_and_read_binary),
                    ?TDEF_FE(should_write_and_read_large_binary),
                    ?TDEF_FE(should_return_term_as_binary_for_reading_binary),
                    ?TDEF_FE(should_read_term_written_as_binary),
                    ?TDEF_FE(should_read_iolist),
                    ?TDEF_FE(should_fsync),
                    ?TDEF_FE(should_update_fsync_stats),
                    ?TDEF_FE(should_not_read_beyond_eof),
                    ?TDEF_FE(should_truncate)
                ]
            }
        }
    }.

should_increase_file_size_on_write(Fd) ->
    {ok, 0, _} = couch_file:append_term(Fd, foo),
    {ok, Size} = couch_file:bytes(Fd),
    ?assert(Size > 0).

should_return_current_file_size_on_write(Fd) ->
    {ok, 0, _} = couch_file:append_term(Fd, foo),
    {ok, Size} = couch_file:bytes(Fd),
    ?assertMatch({ok, Size, _}, couch_file:append_term(Fd, bar)).

should_write_and_read_term(Fd) ->
    {ok, Pos, _} = couch_file:append_term(Fd, foo),
    ?assertMatch({ok, foo}, couch_file:pread_term(Fd, Pos)).

should_write_and_read_binary(Fd) ->
    {ok, Pos, _} = couch_file:append_binary(Fd, <<"fancy!">>),
    ?assertMatch({ok, <<"fancy!">>}, couch_file:pread_binary(Fd, Pos)).

should_return_term_as_binary_for_reading_binary(Fd) ->
    {ok, Pos, _} = couch_file:append_term(Fd, foo),
    Foo = couch_compress:compress(foo, snappy),
    ?assertMatch({ok, Foo}, couch_file:pread_binary(Fd, Pos)).

should_read_term_written_as_binary(Fd) ->
    {ok, Pos, _} = couch_file:append_binary(Fd, <<131, 100, 0, 3, 102, 111, 111>>),
    ?assertMatch({ok, foo}, couch_file:pread_term(Fd, Pos)).

should_write_and_read_large_binary(Fd) ->
    BigBin = list_to_binary(lists:duplicate(100000, 0)),
    {ok, Pos, _} = couch_file:append_binary(Fd, BigBin),
    ?assertMatch({ok, BigBin}, couch_file:pread_binary(Fd, Pos)).

should_read_iolist(Fd) ->
    %% append_binary == append_iolist?
    %% Possible bug in pread_iolist or iolist() -> append_binary
    {ok, Pos, _} = couch_file:append_binary(Fd, ["foo", $m, <<"bam">>]),
    {ok, IoList} = couch_file:pread_iolist(Fd, Pos),
    ?assertMatch(<<"foombam">>, iolist_to_binary(IoList)).

should_fsync(Fd) ->
    ?assertMatch(ok, couch_file:sync(Fd)).

should_update_fsync_stats(Fd) ->
    Count0 = couch_stats:sample([fsync, count]),
    Seq = lists:seq(1, 10),
    lists:foreach(fun(_) -> ok = couch_file:sync(Fd) end, Seq),
    Hist = couch_stats:sample([fsync, time]),
    Count1 = couch_stats:sample([fsync, count]),
    ?assert(Count1 > Count0),
    HistMax = proplists:get_value(max, Hist),
    HistPct = proplists:get_value(percentile, Hist),
    ?assert(HistMax > 0),
    ?assertMatch([{50, P50} | _] when P50 > 0, HistPct).

should_not_read_beyond_eof(_) ->
    {ok, Fd} = couch_file:open(?tempfile(), [create, overwrite]),
    BigBin = list_to_binary(lists:duplicate(100000, 0)),
    DoubleBin = round(byte_size(BigBin) * 2),
    {ok, Pos, _Size} = couch_file:append_binary(Fd, BigBin),
    {_, Filepath} = couch_file:process_info(Fd),
    %% corrupt db file
    {ok, Io} = file:open(Filepath, [read, write, binary]),
    ok = file:pwrite(Io, Pos, <<0:1/integer, DoubleBin:31/integer>>),
    file:close(Io),
    unlink(Fd),
    ExpectExit = {bad_return_value, {read_beyond_eof, Filepath}},
    ExpectError = {badmatch, {'EXIT', ExpectExit}},
    ?assertError(ExpectError, couch_file:pread_binary(Fd, Pos)),
    catch file:close(Fd).

should_truncate(Fd) ->
    {ok, 0, _} = couch_file:append_term(Fd, foo),
    {ok, Size} = couch_file:bytes(Fd),
    BigBin = list_to_binary(lists:duplicate(100000, 0)),
    {ok, _, _} = couch_file:append_binary(Fd, BigBin),
    ok = couch_file:truncate(Fd, Size),
    ?assertMatch({ok, foo}, couch_file:pread_term(Fd, 0)).

pread_limit_test_() ->
    {
        "Read limit tests",
        {
            setup,
            fun() ->
                Ctx = test_util:start_couch([ioq]),
                config:set("couchdb", "max_pread_size", "50000", _Persist = false),
                Ctx
            end,
            fun(Ctx) ->
                config:delete("couchdb", "max_pread_size", _Persist = false),
                test_util:stop_couch(Ctx)
            end,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(should_increase_file_size_on_write),
                    ?TDEF_FE(should_return_current_file_size_on_write),
                    ?TDEF_FE(should_write_and_read_term),
                    ?TDEF_FE(should_write_and_read_binary),
                    ?TDEF_FE(should_not_read_more_than_pread_limit)
                ]
            }
        }
    }.

should_not_read_more_than_pread_limit(_) ->
    {ok, Fd} = couch_file:open(?tempfile(), [create, overwrite]),
    {_, Filepath} = couch_file:process_info(Fd),
    BigBin = list_to_binary(lists:duplicate(100000, 0)),
    {ok, Pos, _Size} = couch_file:append_binary(Fd, BigBin),
    unlink(Fd),
    ExpectExit = {bad_return_value, {exceed_pread_limit, Filepath, 50000}},
    ExpectError = {badmatch, {'EXIT', ExpectExit}},
    ?assertError(ExpectError, couch_file:pread_binary(Fd, Pos)),
    catch file:close(Fd).

header_test_() ->
    {
        "File header read/write tests",
        {
            setup,
            fun() -> test_util:start(?MODULE, [ioq]) end,
            fun test_util:stop/1,
            [
                ?foreach([
                    fun should_write_and_read_atom_header/1,
                    fun should_write_and_read_tuple_header/1,
                    fun should_write_and_read_second_header/1,
                    fun should_truncate_second_header/1,
                    fun should_produce_same_file_size_on_rewrite/1,
                    fun should_save_headers_larger_than_block_size/1
                ]),
                should_recover_header_marker_corruption(),
                should_recover_header_size_corruption(),
                should_recover_header_md5sig_corruption(),
                should_recover_header_data_corruption()
            ]
        }
    }.

should_write_and_read_atom_header(Fd) ->
    ok = couch_file:write_header(Fd, hello),
    ?_assertMatch({ok, hello}, couch_file:read_header(Fd)).

should_write_and_read_tuple_header(Fd) ->
    ok = couch_file:write_header(Fd, {<<"some_data">>, 32}),
    ?_assertMatch({ok, {<<"some_data">>, 32}}, couch_file:read_header(Fd)).

should_write_and_read_second_header(Fd) ->
    ok = couch_file:write_header(Fd, {<<"some_data">>, 32}),
    ok = couch_file:write_header(Fd, [foo, <<"more">>]),
    ?_assertMatch({ok, [foo, <<"more">>]}, couch_file:read_header(Fd)).

should_truncate_second_header(Fd) ->
    ok = couch_file:write_header(Fd, {<<"some_data">>, 32}),
    {ok, Size} = couch_file:bytes(Fd),
    ok = couch_file:write_header(Fd, [foo, <<"more">>]),
    ok = couch_file:truncate(Fd, Size),
    ?_assertMatch({ok, {<<"some_data">>, 32}}, couch_file:read_header(Fd)).

should_produce_same_file_size_on_rewrite(Fd) ->
    ok = couch_file:write_header(Fd, {<<"some_data">>, 32}),
    {ok, Size1} = couch_file:bytes(Fd),
    ok = couch_file:write_header(Fd, [foo, <<"more">>]),
    {ok, Size2} = couch_file:bytes(Fd),
    ok = couch_file:truncate(Fd, Size1),
    ok = couch_file:write_header(Fd, [foo, <<"more">>]),
    ?_assertMatch({ok, Size2}, couch_file:bytes(Fd)).

should_save_headers_larger_than_block_size(Fd) ->
    Header = erlang:make_tuple(5000, <<"CouchDB">>),
    couch_file:write_header(Fd, Header),
    {"COUCHDB-1319", ?_assertMatch({ok, Header}, couch_file:read_header(Fd))}.

should_recover_header_marker_corruption() ->
    ?_assertMatch(
        ok,
        check_header_recovery(
            fun(CouchFd, RawFd, Expect, HeaderPos) ->
                ?assertNotMatch(Expect, couch_file:read_header(CouchFd)),
                file:pwrite(RawFd, HeaderPos, <<0>>),
                ?assertMatch(Expect, couch_file:read_header(CouchFd))
            end
        )
    ).

should_recover_header_size_corruption() ->
    ?_assertMatch(
        ok,
        check_header_recovery(
            fun(CouchFd, RawFd, Expect, HeaderPos) ->
                ?assertNotMatch(Expect, couch_file:read_header(CouchFd)),
                % +1 for 0x1 byte marker
                file:pwrite(RawFd, HeaderPos + 1, <<10/integer>>),
                ?assertMatch(Expect, couch_file:read_header(CouchFd))
            end
        )
    ).

should_recover_header_md5sig_corruption() ->
    ?_assertMatch(
        ok,
        check_header_recovery(
            fun(CouchFd, RawFd, Expect, HeaderPos) ->
                ?assertNotMatch(Expect, couch_file:read_header(CouchFd)),
                % +5 = +1 for 0x1 byte and +4 for term size.
                file:pwrite(RawFd, HeaderPos + 5, <<"F01034F88D320B22">>),
                ?assertMatch(Expect, couch_file:read_header(CouchFd))
            end
        )
    ).

should_recover_header_data_corruption() ->
    ?_assertMatch(
        ok,
        check_header_recovery(
            fun(CouchFd, RawFd, Expect, HeaderPos) ->
                ?assertNotMatch(Expect, couch_file:read_header(CouchFd)),
                % +21 = +1 for 0x1 byte, +4 for term size and +16 for MD5 sig
                file:pwrite(RawFd, HeaderPos + 21, <<"some data goes here!">>),
                ?assertMatch(Expect, couch_file:read_header(CouchFd))
            end
        )
    ).

check_header_recovery(CheckFun) ->
    Path = ?tempfile(),
    {ok, Fd} = couch_file:open(Path, [create, overwrite]),
    {ok, RawFd} = file:open(Path, [read, write, raw, binary]),

    {ok, _} = write_random_data(Fd),
    ExpectHeader = {some_atom, <<"a binary">>, 756},
    ok = couch_file:write_header(Fd, ExpectHeader),

    {ok, HeaderPos} = write_random_data(Fd),
    ok = couch_file:write_header(Fd, {2342, <<"corruption! greed!">>}),

    CheckFun(Fd, RawFd, {ok, ExpectHeader}, HeaderPos),

    ok = file:close(RawFd),
    ok = couch_file:close(Fd),
    ok.

write_random_data(Fd) ->
    write_random_data(Fd, 100 + couch_rand:uniform(1000)).

write_random_data(Fd, 0) ->
    {ok, Bytes} = couch_file:bytes(Fd),
    {ok, (1 + Bytes div ?BLOCK_SIZE) * ?BLOCK_SIZE};
write_random_data(Fd, N) ->
    Choices = [foo, bar, <<"bizzingle">>, "bank", ["rough", stuff]],
    Term = lists:nth(couch_rand:uniform(4) + 1, Choices),
    {ok, _, _} = couch_file:append_term(Fd, Term),
    write_random_data(Fd, N - 1).

delete_test_() ->
    {
        "File delete tests",
        {
            setup,
            fun() ->
                meck:new(config, [passthrough])
            end,
            fun(_) ->
                meck:unload()
            end,
            {
                foreach,
                fun() ->
                    meck:reset([config]),
                    File = ?tempfile() ++ ".couch",
                    RootDir = filename:dirname(File),
                    ok = couch_file:init_delete_dir(RootDir),
                    ok = file:write_file(File, <<>>),
                    {RootDir, File}
                end,
                fun({_, File}) ->
                    file:delete(File)
                end,
                [
                    fun(Cfg) ->
                        {"enable_database_recovery = false, context = delete",
                            make_enable_recovery_test_case(Cfg, false, delete)}
                    end,
                    fun(Cfg) ->
                        {"enable_database_recovery = true, context = delete",
                            make_enable_recovery_test_case(Cfg, true, delete)}
                    end,
                    fun(Cfg) ->
                        {"enable_database_recovery = false, context = compaction",
                            make_enable_recovery_test_case(Cfg, false, compaction)}
                    end,
                    fun(Cfg) ->
                        {"enable_database_recovery = true, context = compaction",
                            make_enable_recovery_test_case(Cfg, true, compaction)}
                    end,
                    fun(Cfg) ->
                        {"delete_after_rename = true",
                            make_delete_after_rename_test_case(Cfg, true)}
                    end,
                    fun(Cfg) ->
                        {"delete_after_rename = false",
                            make_delete_after_rename_test_case(Cfg, false)}
                    end
                ]
            }
        }
    }.

make_enable_recovery_test_case({RootDir, File}, EnableRecovery, Context) ->
    meck:expect(config, get_boolean, fun
        ("couchdb", "enable_database_recovery", _) -> EnableRecovery;
        ("couchdb", "delete_after_rename", _) -> false
    end),
    FileExistsBefore = filelib:is_regular(File),
    DeleteResult = couch_file:delete(RootDir, File, [{context, Context}]),
    FileExistsAfter = filelib:is_regular(File),
    RenamedFiles = filelib:wildcard(filename:rootname(File) ++ "*.deleted.*"),
    DeletedFiles = filelib:wildcard(RootDir ++ "/.delete/*"),
    {ExpectRenamedCount, ExpectDeletedCount} =
        if
            EnableRecovery andalso Context =:= delete -> {1, 0};
            true -> {0, 1}
        end,
    [
        ?_assertEqual(ok, DeleteResult),
        ?_assert(FileExistsBefore),
        ?_assertNot(FileExistsAfter),
        ?_assertEqual(ExpectRenamedCount, length(RenamedFiles)),
        ?_assertEqual(ExpectDeletedCount, length(DeletedFiles))
    ].

make_delete_after_rename_test_case({RootDir, File}, DeleteAfterRename) ->
    meck:expect(config, get_boolean, fun
        ("couchdb", "enable_database_recovery", _) -> false;
        ("couchdb", "delete_after_rename", _) -> DeleteAfterRename
    end),
    FileExistsBefore = filelib:is_regular(File),
    couch_file:delete(RootDir, File),
    FileExistsAfter = filelib:is_regular(File),
    RenamedFiles = filelib:wildcard(filename:join([RootDir, ".delete", "*"])),
    ExpectRenamedCount =
        if
            DeleteAfterRename -> 0;
            true -> 1
        end,
    [
        ?_assert(FileExistsBefore),
        ?_assertNot(FileExistsAfter),
        ?_assertEqual(ExpectRenamedCount, length(RenamedFiles))
    ].

nuke_dir_test_() ->
    {
        "Nuke directory tests",
        {
            setup,
            fun() ->
                meck:new(config, [passthrough])
            end,
            fun(_) ->
                meck:unload()
            end,
            {
                foreach,
                fun() ->
                    meck:reset([config]),
                    File0 = ?tempfile() ++ ".couch",
                    RootDir = filename:dirname(File0),
                    BaseName = filename:basename(File0),
                    Seed = couch_rand:uniform(8999999999) + 999999999,
                    DDocDir = io_lib:format("db.~b_design", [Seed]),
                    ViewDir = filename:join([RootDir, DDocDir]),
                    file:make_dir(ViewDir),
                    File = filename:join([ViewDir, BaseName]),
                    file:rename(File0, File),
                    ok = couch_file:init_delete_dir(RootDir),
                    ok = file:write_file(File, <<>>),
                    {RootDir, ViewDir}
                end,
                fun({RootDir, ViewDir}) ->
                    remove_dir(ViewDir),
                    Ext = filename:extension(ViewDir),
                    case filelib:wildcard(RootDir ++ "/*.deleted" ++ Ext) of
                        [DelDir] -> remove_dir(DelDir);
                        _ -> ok
                    end
                end,
                [
                    fun(Cfg) ->
                        {"enable_database_recovery = false", make_rename_dir_test_case(Cfg, false)}
                    end,
                    fun(Cfg) ->
                        {"enable_database_recovery = true", make_rename_dir_test_case(Cfg, true)}
                    end,
                    fun(Cfg) ->
                        {"delete_after_rename = true", make_delete_dir_test_case(Cfg, true)}
                    end,
                    fun(Cfg) ->
                        {"delete_after_rename = false", make_delete_dir_test_case(Cfg, false)}
                    end
                ]
            }
        }
    }.

make_rename_dir_test_case({RootDir, ViewDir}, EnableRecovery) ->
    meck:expect(config, get_boolean, fun
        ("couchdb", "enable_database_recovery", _) -> EnableRecovery;
        ("couchdb", "delete_after_rename", _) -> true;
        (_, _, Default) -> Default
    end),
    DirExistsBefore = filelib:is_dir(ViewDir),
    couch_file:nuke_dir(RootDir, ViewDir),
    DirExistsAfter = filelib:is_dir(ViewDir),
    Ext = filename:extension(ViewDir),
    RenamedDirs = filelib:wildcard(RootDir ++ "/*.deleted" ++ Ext),
    ExpectRenamedCount =
        if
            EnableRecovery -> 1;
            true -> 0
        end,
    [
        ?_assert(DirExistsBefore),
        ?_assertNot(DirExistsAfter),
        ?_assertEqual(ExpectRenamedCount, length(RenamedDirs))
    ].

make_delete_dir_test_case({RootDir, ViewDir}, DeleteAfterRename) ->
    meck:expect(config, get_boolean, fun
        ("couchdb", "enable_database_recovery", _) -> false;
        ("couchdb", "delete_after_rename", _) -> DeleteAfterRename;
        (_, _, Default) -> Default
    end),
    DirExistsBefore = filelib:is_dir(ViewDir),
    couch_file:nuke_dir(RootDir, ViewDir),
    DirExistsAfter = filelib:is_dir(ViewDir),
    Ext = filename:extension(ViewDir),
    RenamedDirs = filelib:wildcard(RootDir ++ "/*.deleted" ++ Ext),
    RenamedFiles = filelib:wildcard(RootDir ++ "/.delete/*"),
    ExpectRenamedCount =
        if
            DeleteAfterRename -> 0;
            true -> 1
        end,
    [
        ?_assert(DirExistsBefore),
        ?_assertNot(DirExistsAfter),
        ?_assertEqual(0, length(RenamedDirs)),
        ?_assertEqual(ExpectRenamedCount, length(RenamedFiles))
    ].

remove_dir(Dir) ->
    [file:delete(File) || File <- filelib:wildcard(filename:join([Dir, "*"]))],
    file:del_dir(Dir).

fsync_error_test_() ->
    {
        "Test fsync raises errors",
        {
            setup,
            fun() ->
                test_util:start(?MODULE, [ioq])
            end,
            fun(Ctx) ->
                test_util:stop(Ctx)
            end,
            [
                fun fsync_raises_errors/0
            ]
        }
    }.

fsync_raises_errors() ->
    Fd = spawn(fun() -> fake_fsync_fd() end),
    ?assertError({fsync_error, eio}, couch_file:sync(Fd)).

fake_fsync_fd() ->
    % Mocking gen_server did not go very
    % well so faking the couch_file pid
    % will have to do.
    receive
        {'$gen_call', From, sync} ->
            gen:reply(From, {error, eio})
    end.

checksum_test_() ->
    {
        foreach,
        fun setup_checksum/0,
        fun teardown_checksum/1,
        [
            ?TDEF_FE(t_write_read_xxhash_checksums),
            ?TDEF_FE(t_downgrade_xxhash_checksums),
            ?TDEF_FE(t_read_legacy_checksums_after_upgrade),
            ?TDEF_FE(t_can_detect_block_corruption_with_xxhash),
            ?TDEF_FE(t_can_detect_block_corruption_with_legacy_checksum)
        ]
    }.

setup_checksum() ->
    Path = ?tempfile(),
    Ctx = test_util:start_couch(),
    config:set("couchdb", "write_xxhash_checksums", "false", _Persist = false),
    {Ctx, Path}.

teardown_checksum({Ctx, Path}) ->
    file:delete(Path),
    meck:unload(),
    test_util:stop_couch(Ctx),
    couch_file:reset_checksum_persistent_term_config().

t_write_read_xxhash_checksums({_Ctx, Path}) ->
    enable_xxhash(),

    {ok, Fd} = couch_file:open(Path, [create]),
    Header = header,
    ok = couch_file:write_header(Fd, Header),
    Bin = <<"bin">>,
    Chunk = couch_file:assemble_file_chunk_and_checksum(Bin),
    {ok, Pos, _} = couch_file:append_raw_chunk(Fd, Chunk),
    couch_file:close(Fd),

    {ok, Fd1} = couch_file:open(Path, []),
    {ok, Header1} = couch_file:read_header(Fd1),
    ?assertEqual(Header, Header1),
    {ok, Bin1} = couch_file:pread_binary(Fd1, Pos),
    ?assertEqual(Bin, Bin1),
    ?assertEqual(0, legacy_stats()),
    couch_file:close(Fd1).

t_downgrade_xxhash_checksums({_Ctx, Path}) ->
    % We're in the future and writting xxhash checkums by default
    enable_xxhash(),
    {ok, Fd} = couch_file:open(Path, [create]),
    Header = header,
    ok = couch_file:write_header(Fd, Header),
    Bin = <<"bin">>,
    Chunk = couch_file:assemble_file_chunk_and_checksum(Bin),
    {ok, Pos, _} = couch_file:append_raw_chunk(Fd, Chunk),
    couch_file:close(Fd),

    % The future was broken, we travel back, but still know how to
    % interpret future checksums without crashing
    disable_xxhash(),
    {ok, Fd1} = couch_file:open(Path, []),
    {ok, Header1} = couch_file:read_header(Fd1),
    ?assertEqual(Header, Header1),
    {ok, Bin1} = couch_file:pread_binary(Fd1, Pos),
    ?assertEqual(Bin, Bin1),

    % We'll write some legacy checksums to the file and then ensure
    % we can read both legacy and the new ones
    OtherBin = <<"otherbin">>,
    OtherChunk = couch_file:assemble_file_chunk_and_checksum(OtherBin),
    {ok, OtherPos, _} = couch_file:append_raw_chunk(Fd1, OtherChunk),
    couch_file:close(Fd1),

    {ok, Fd2} = couch_file:open(Path, []),
    {ok, Header2} = couch_file:read_header(Fd2),
    ?assertEqual(Header, Header2),
    {ok, Bin2} = couch_file:pread_binary(Fd2, Pos),
    {ok, OtherBin1} = couch_file:pread_binary(Fd2, OtherPos),
    ?assertEqual(Bin, Bin2),
    ?assertEqual(OtherBin, OtherBin1),
    couch_file:close(Fd2).

t_read_legacy_checksums_after_upgrade({_Ctx, Path}) ->
    % We're in the past and writting legacy checkums by default
    disable_xxhash(),
    {ok, Fd} = couch_file:open(Path, [create]),
    Header = header,
    ok = couch_file:write_header(Fd, Header),
    Bin = <<"bin">>,
    Chunk = couch_file:assemble_file_chunk_and_checksum(Bin),
    {ok, Pos, _} = couch_file:append_raw_chunk(Fd, Chunk),
    couch_file:close(Fd),

    % We upgrade and xxhash checksums are now the default, but we can
    % still read legacy checksums.
    enable_xxhash(),
    {ok, Fd1} = couch_file:open(Path, []),
    {ok, Header1} = couch_file:read_header(Fd1),
    ?assertEqual(Header, Header1),
    {ok, Bin1} = couch_file:pread_binary(Fd1, Pos),
    ?assertEqual(Bin, Bin1),
    % one header, one chunk
    ?assertEqual(2, legacy_stats()),

    % We'll write some new checksums to the file and then ensure
    % we can read both legacy and the new ones
    OtherBin = <<"otherbin">>,
    OtherChunk = couch_file:assemble_file_chunk_and_checksum(OtherBin),
    {ok, OtherPos, _} = couch_file:append_raw_chunk(Fd1, OtherChunk),
    couch_file:close(Fd1),

    couch_stats:decrement_counter([couchdb, legacy_checksums], legacy_stats()),
    {ok, Fd2} = couch_file:open(Path, []),
    {ok, Header2} = couch_file:read_header(Fd2),
    ?assertEqual(Header, Header2),
    {ok, Bin2} = couch_file:pread_binary(Fd2, Pos),
    {ok, OtherBin1} = couch_file:pread_binary(Fd2, OtherPos),
    ?assertEqual(Bin, Bin2),
    ?assertEqual(OtherBin, OtherBin1),
    % one header, legacy chunk, not counting new chunk
    ?assertEqual(2, legacy_stats()),
    couch_file:close(Fd2).

t_can_detect_block_corruption_with_xxhash({_Ctx, Path}) ->
    enable_xxhash(),

    {ok, Fd} = couch_file:open(Path, [create]),
    Bin = crypto:strong_rand_bytes(100000),
    Chunk = couch_file:assemble_file_chunk_and_checksum(Bin),
    {ok, Pos, _} = couch_file:append_raw_chunk(Fd, Chunk),
    ok = couch_file:write_header(Fd, header),
    couch_file:close(Fd),

    {ok, SneakyFd} = file:open(Path, [binary, read, write, raw]),
    ok = file:pwrite(SneakyFd, Pos + 100, <<"oops!">>),
    file:close(SneakyFd),

    {ok, Fd1} = couch_file:open(Path, []),
    {ok, Header} = couch_file:read_header(Fd1),
    ?assertEqual(header, Header),
    ?assertExit({file_corruption, <<"file corruption">>}, couch_file:pread_binary(Fd1, Pos)),
    catch couch_file:close(Fd1).

t_can_detect_block_corruption_with_legacy_checksum({_Ctx, Path}) ->
    disable_xxhash(),

    {ok, Fd} = couch_file:open(Path, [create]),
    Bin = crypto:strong_rand_bytes(100000),
    Chunk = couch_file:assemble_file_chunk_and_checksum(Bin),
    {ok, Pos, _} = couch_file:append_raw_chunk(Fd, Chunk),
    ok = couch_file:write_header(Fd, header),
    couch_file:close(Fd),

    {ok, SneakyFd} = file:open(Path, [write, binary, read, raw]),
    ok = file:pwrite(SneakyFd, Pos + 100, <<"oops!">>),
    file:close(SneakyFd),

    {ok, Fd1} = couch_file:open(Path, []),
    {ok, Header} = couch_file:read_header(Fd1),
    ?assertEqual(header, Header),
    ?assertExit({file_corruption, <<"file corruption">>}, couch_file:pread_binary(Fd1, Pos)),
    catch couch_file:close(Fd1).

enable_xxhash() ->
    couch_file:reset_checksum_persistent_term_config(),
    reset_legacy_checksum_stats(),
    config:set("couchdb", "write_xxhash_checksums", "true", _Persist = false).

disable_xxhash() ->
    couch_file:reset_checksum_persistent_term_config(),
    reset_legacy_checksum_stats(),
    config:set("couchdb", "write_xxhash_checksums", "false", _Persist = false).

legacy_stats() ->
    couch_stats:sample([couchdb, legacy_checksums]).

reset_legacy_checksum_stats() ->
    Counter = couch_stats:sample([couchdb, legacy_checksums]),
    couch_stats:decrement_counter([couchdb, legacy_checksums], Counter).
