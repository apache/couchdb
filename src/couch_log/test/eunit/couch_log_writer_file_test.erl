% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_log_writer_file_test).

-include_lib("kernel/include/file.hrl").
-include_lib("couch_log/include/couch_log.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(WRITER, couch_log_writer_file).

couch_log_writer_file_test_() ->
    {setup, fun couch_log_test_util:start/0, fun couch_log_test_util:stop/1, [
        fun check_init_terminate/0,
        fun() ->
            couch_log_test_util:with_meck(
                [{filelib, [unstick]}],
                fun check_ensure_dir_fail/0
            )
        end,
        fun() ->
            couch_log_test_util:with_meck(
                [{file, [unstick, passthrough]}],
                fun check_open_fail/0
            )
        end,
        fun() ->
            couch_log_test_util:with_meck(
                [{file, [unstick, passthrough]}],
                fun check_read_file_info_fail/0
            )
        end,
        fun check_file_write/0,
        fun check_buffered_file_write/0,
        fun check_reopen/0
    ]}.

check_init_terminate() ->
    {ok, St} = ?WRITER:init(),
    ok = ?WRITER:terminate(stop, St).

check_ensure_dir_fail() ->
    meck:expect(filelib, ensure_dir, 1, {error, eperm}),
    ?assertEqual({error, eperm}, ?WRITER:init()),
    ?assert(meck:called(filelib, ensure_dir, 1)),
    ?assert(meck:validate(filelib)).

check_open_fail() ->
    meck:expect(file, open, 2, {error, enotfound}),
    ?assertEqual({error, enotfound}, ?WRITER:init()),
    ?assert(meck:called(file, open, 2)),
    ?assert(meck:validate(file)).

check_read_file_info_fail() ->
    RFI = fun
        ("./couch.log") -> {error, enoent};
        (Path) -> meck:passthrough([Path])
    end,
    meck:expect(file, read_file_info, RFI),
    ?assertEqual({error, enoent}, ?WRITER:init()),
    ?assert(meck:called(file, read_file_info, 1)),
    ?assert(meck:validate(file)).

check_file_write() ->
    % Make sure we have an empty log for this test
    IsFile = filelib:is_file("./couch.log"),
    if
        not IsFile -> ok;
        true -> file:delete("./couch.log")
    end,

    Entry = #log_entry{
        level = info,
        pid = list_to_pid("<0.1.0>"),
        msg = "stuff",
        msg_id = "msg_id",
        time_stamp = "time_stamp"
    },
    {ok, St} = ?WRITER:init(),
    {ok, NewSt} = ?WRITER:write(Entry, St),
    ok = ?WRITER:terminate(stop, NewSt),

    {ok, Data} = file:read_file("./couch.log"),
    Expect = <<"[info] time_stamp nonode@nohost <0.1.0> msg_id stuff\n">>,
    ?assertEqual(Expect, Data).

check_buffered_file_write() ->
    % Make sure we have an empty log for this test
    IsFile = filelib:is_file("./couch.log"),
    if
        not IsFile -> ok;
        true -> file:delete("./couch.log")
    end,

    config:set("log", "write_buffer", "1024"),
    config:set("log", "write_delay", "10"),

    try
        Entry = #log_entry{
            level = info,
            pid = list_to_pid("<0.1.0>"),
            msg = "stuff",
            msg_id = "msg_id",
            time_stamp = "time_stamp"
        },
        {ok, St} = ?WRITER:init(),
        {ok, NewSt} = ?WRITER:write(Entry, St),
        ok = ?WRITER:terminate(stop, NewSt)
    after
        config:delete("log", "write_buffer"),
        config:delete("log", "write_delay")
    end,

    {ok, Data} = file:read_file("./couch.log"),
    Expect = <<"[info] time_stamp nonode@nohost <0.1.0> msg_id stuff\n">>,
    ?assertEqual(Expect, Data).

check_reopen() ->
    {ok, St1} = clear_clock(?WRITER:init()),
    {ok, St2} = clear_clock(couch_log_writer_file:maybe_reopen(St1)),
    ?assertEqual(St1, St2),

    case os:type() of
        {win32, _} ->
            % Windows file handling doesn't work the same
            % as Unix where you can move or delete an open
            % file so these tests make no sense there.
            yay_we_pass;
        _ ->
            % Delete file
            file:delete("./couch.log"),
            {ok, St3} = clear_clock(couch_log_writer_file:maybe_reopen(St2)),
            ?assert(element(3, St3) /= element(3, St2)),

            % Recreate file
            file:delete("./couch.log"),
            file:write_file("./couch.log", ""),
            {ok, St4} = clear_clock(couch_log_writer_file:maybe_reopen(St3)),
            ?assert(element(3, St4) /= element(3, St2))
    end.

clear_clock({ok, St}) ->
    {ok, clear_clock(St)};
clear_clock(St) ->
    {st, Path, Fd, INode, _} = St,
    {st, Path, Fd, INode, {0, 0, 0}}.
