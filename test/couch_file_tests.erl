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
    ok = couch_file:close(Fd).

open_close_test_() ->
    {
        "Test for proper file open and close",
        {
            setup,
            fun() -> test_util:start(?MODULE, [ioq]) end, fun test_util:stop/1,
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
    ?_assertMatch({ok, _},
                  couch_file:open(?tempfile(), [create, invalid_option])).

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
            fun() -> test_util:start(?MODULE, [ioq]) end, fun test_util:stop/1,
            ?foreach([
                fun should_increase_file_size_on_write/1,
                fun should_return_current_file_size_on_write/1,
                fun should_write_and_read_term/1,
                fun should_write_and_read_binary/1,
                fun should_write_and_read_large_binary/1,
                fun should_return_term_as_binary_for_reading_binary/1,
                fun should_read_term_written_as_binary/1,
                fun should_read_iolist/1,
                fun should_fsync/1,
                fun should_not_read_beyond_eof/1,
                fun should_truncate/1
            ])
        }
    }.


should_increase_file_size_on_write(Fd) ->
    {ok, 0, _} = couch_file:append_term(Fd, foo),
    {ok, Size} = couch_file:bytes(Fd),
    ?_assert(Size > 0).

should_return_current_file_size_on_write(Fd) ->
    {ok, 0, _} = couch_file:append_term(Fd, foo),
    {ok, Size} = couch_file:bytes(Fd),
    ?_assertMatch({ok, Size, _}, couch_file:append_term(Fd, bar)).

should_write_and_read_term(Fd) ->
    {ok, Pos, _} = couch_file:append_term(Fd, foo),
    ?_assertMatch({ok, foo}, couch_file:pread_term(Fd, Pos)).

should_write_and_read_binary(Fd) ->
    {ok, Pos, _} = couch_file:append_binary(Fd, <<"fancy!">>),
    ?_assertMatch({ok, <<"fancy!">>}, couch_file:pread_binary(Fd, Pos)).

should_return_term_as_binary_for_reading_binary(Fd) ->
    {ok, Pos, _} = couch_file:append_term(Fd, foo),
    Foo = couch_compress:compress(foo, snappy),
    ?_assertMatch({ok, Foo}, couch_file:pread_binary(Fd, Pos)).

should_read_term_written_as_binary(Fd) ->
    {ok, Pos, _} = couch_file:append_binary(Fd, <<131,100,0,3,102,111,111>>),
    ?_assertMatch({ok, foo}, couch_file:pread_term(Fd, Pos)).

should_write_and_read_large_binary(Fd) ->
    BigBin = list_to_binary(lists:duplicate(100000, 0)),
    {ok, Pos, _} = couch_file:append_binary(Fd, BigBin),
    ?_assertMatch({ok, BigBin}, couch_file:pread_binary(Fd, Pos)).

should_read_iolist(Fd) ->
    %% append_binary == append_iolist?
    %% Possible bug in pread_iolist or iolist() -> append_binary
    {ok, Pos, _} = couch_file:append_binary(Fd, ["foo", $m, <<"bam">>]),
    {ok, IoList} = couch_file:pread_iolist(Fd, Pos),
    ?_assertMatch(<<"foombam">>, iolist_to_binary(IoList)).

should_fsync(Fd) ->
    {"How does on test fsync?", ?_assertMatch(ok, couch_file:sync(Fd))}.

should_not_read_beyond_eof(_) ->
    {"No idea how to test reading beyond EOF", ?_assert(true)}.

should_truncate(Fd) ->
    {ok, 0, _} = couch_file:append_term(Fd, foo),
    {ok, Size} = couch_file:bytes(Fd),
    BigBin = list_to_binary(lists:duplicate(100000, 0)),
    {ok, _, _} = couch_file:append_binary(Fd, BigBin),
    ok = couch_file:truncate(Fd, Size),
    ?_assertMatch({ok, foo}, couch_file:pread_term(Fd, 0)).


header_test_() ->
    {
        "File header read/write tests",
        {
            setup,
            fun() -> test_util:start(?MODULE, [ioq]) end, fun test_util:stop/1,
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
            end)
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
            end)
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
            end)
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
            end)
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
    write_random_data(Fd, 100 + random:uniform(1000)).

write_random_data(Fd, 0) ->
    {ok, Bytes} = couch_file:bytes(Fd),
    {ok, (1 + Bytes div ?BLOCK_SIZE) * ?BLOCK_SIZE};
write_random_data(Fd, N) ->
    Choices = [foo, bar, <<"bizzingle">>, "bank", ["rough", stuff]],
    Term = lists:nth(random:uniform(4) + 1, Choices),
    {ok, _, _} = couch_file:append_term(Fd, Term),
    write_random_data(Fd, N - 1).


delete_test_() ->
    {
        "File delete tests",
        {
            foreach,
            fun() ->
                meck:new(config, [passthrough]),
                File = ?tempfile() ++ ".couch",
                RootDir = filename:dirname(File),
                ok = couch_file:init_delete_dir(RootDir),
                ok = file:write_file(File, <<>>),
                {RootDir, File}
            end,
            fun({_, File}) ->
                meck:unload(config),
                file:delete(File)
            end,
            [
                fun(Cfg) ->
                    {"enable_database_recovery = false",
                    make_enable_recovery_test_case(Cfg, false)}
                end,
                fun(Cfg) ->
                    {"enable_database_recovery = true",
                    make_enable_recovery_test_case(Cfg, true)}
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
    }.


make_enable_recovery_test_case({RootDir, File}, EnableRecovery) ->
    meck:expect(config, get_boolean, fun
        ("couchdb", "enable_database_recovery", _) -> EnableRecovery;
        ("couchdb", "delete_after_rename", _) -> true
    end),
    FileExistsBefore = filelib:is_regular(File),
    couch_file:delete(RootDir, File, false),
    FileExistsAfter = filelib:is_regular(File),
    RenamedFiles = filelib:wildcard(filename:rootname(File) ++ "*.deleted.*"),
    ExpectRenamedCount = if EnableRecovery -> 1; true -> 0 end,
    [
        ?_assert(FileExistsBefore),
        ?_assertNot(FileExistsAfter),
        ?_assertEqual(ExpectRenamedCount, length(RenamedFiles))
    ].

make_delete_after_rename_test_case({RootDir, File}, DeleteAfterRename) ->
    meck:expect(config, get_boolean, fun
        ("couchdb", "enable_database_recovery", _) -> false;
        ("couchdb", "delete_after_rename", _) -> DeleteAfterRename
    end),
    FileExistsBefore = filelib:is_regular(File),
    couch_file:delete(RootDir, File, false),
    FileExistsAfter = filelib:is_regular(File),
    RenamedFiles = filelib:wildcard(filename:join([RootDir, ".delete", "*"])),
    ExpectRenamedCount = if DeleteAfterRename -> 0; true -> 1 end,
    [
        ?_assert(FileExistsBefore),
        ?_assertNot(FileExistsAfter),
        ?_assertEqual(ExpectRenamedCount, length(RenamedFiles))
    ].


nuke_dir_test_() ->
    {
        "Nuke directory tests",
        {
            foreach,
            fun() ->
                meck:new(config, [passthrough]),
                File0 = ?tempfile() ++ ".couch",
                RootDir = filename:dirname(File0),
                BaseName = filename:basename(File0),
                Seed = crypto:rand_uniform(1000000000, 9999999999),
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
                meck:unload(config),
                remove_dir(ViewDir),
                Ext = filename:extension(ViewDir),
                case filelib:wildcard(RootDir ++ "/*.deleted" ++ Ext) of
                    [DelDir] -> remove_dir(DelDir);
                    _ -> ok
                end
            end,
            [
                fun(Cfg) ->
                    {"enable_database_recovery = false",
                    make_rename_dir_test_case(Cfg, false)}
                end,
                fun(Cfg) ->
                    {"enable_database_recovery = true",
                    make_rename_dir_test_case(Cfg, true)}
                end,
                fun(Cfg) ->
                    {"delete_after_rename = true",
                    make_delete_dir_test_case(Cfg, true)}
                end,
                fun(Cfg) ->
                    {"delete_after_rename = false",
                    make_delete_dir_test_case(Cfg, false)}
                end
            ]
        }
    }.


make_rename_dir_test_case({RootDir, ViewDir}, EnableRecovery) ->
    meck:expect(config, get_boolean, fun
        ("couchdb", "enable_database_recovery", _) -> EnableRecovery;
        ("couchdb", "delete_after_rename", _) -> true
    end),
    DirExistsBefore = filelib:is_dir(ViewDir),
    couch_file:nuke_dir(RootDir, ViewDir),
    DirExistsAfter = filelib:is_dir(ViewDir),
    Ext = filename:extension(ViewDir),
    RenamedDirs = filelib:wildcard(RootDir ++ "/*.deleted" ++ Ext),
    ExpectRenamedCount = if EnableRecovery -> 1; true -> 0 end,
    [
        ?_assert(DirExistsBefore),
        ?_assertNot(DirExistsAfter),
        ?_assertEqual(ExpectRenamedCount, length(RenamedDirs))
    ].

make_delete_dir_test_case({RootDir, ViewDir}, DeleteAfterRename) ->
    meck:expect(config, get_boolean, fun
        ("couchdb", "enable_database_recovery", _) -> false;
        ("couchdb", "delete_after_rename", _) -> DeleteAfterRename
    end),
    DirExistsBefore = filelib:is_dir(ViewDir),
    couch_file:nuke_dir(RootDir, ViewDir),
    DirExistsAfter = filelib:is_dir(ViewDir),
    Ext = filename:extension(ViewDir),
    RenamedDirs = filelib:wildcard(RootDir ++ "/*.deleted" ++ Ext),
    RenamedFiles = filelib:wildcard(RootDir ++ "/.delete/*"),
    ExpectRenamedCount = if DeleteAfterRename -> 0; true -> 1 end,
    [
        ?_assert(DirExistsBefore),
        ?_assertNot(DirExistsAfter),
        ?_assertEqual(0, length(RenamedDirs)),
        ?_assertEqual(ExpectRenamedCount, length(RenamedFiles))
    ].

remove_dir(Dir) ->
    [file:delete(File) || File <- filelib:wildcard(filename:join([Dir, "*"]))],
    file:del_dir(Dir).
