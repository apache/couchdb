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

-module(couch_cfile_prop_tests).

-ifdef(WITH_PROPER).

-include_lib("couch/include/couch_eunit_proper.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("kernel/include/file.hrl").

property_test_() ->
    ?EUNIT_QUICKCHECK(60, 25000).

-define(SIZE_LIMIT, 5000).
-define(CFILE_FD, cfile_fd).
-define(RAW_FD, raw_fd).
-define(RAW_PATH, raw_path).
-define(CFILE_PATH, cfile_path).

% Check that any combination of file operations on both the raw handle and the
% cfile handle yield the same results
%
prop_file_ops_results_match_raw_file() ->
    case os:type() of
        {win32, _} ->
            % Dummy test that passes on Windows. We want to return a valid
            % property to avoid upsetting PropEr.
            ?FORALL(N, number(), is_number(N));
        {_, _} ->
            % Setup is a bit awkward but this is the PropEr pattern (pun
            % intended). The general idea is ?SETUP takes a function, which
            % returns another teardown function. SETUP?s can be nested so,
            % instead of the property as the second argument, use another
            % ?SETUP and so on.
            ?SETUP(
                fun() ->
                    Path = ?tempfile(),
                    put(?RAW_FD, open_raw(Path)),
                    put(?RAW_PATH, Path),
                    fun() ->
                        ok = file:close(get(?RAW_FD)),
                        ok = file:delete(Path),
                        erase(?RAW_FD),
                        erase(?RAW_PATH),
                        ok
                    end
                end,
                ?SETUP(
                    fun() ->
                        Path = ?tempfile(),
                        put(?CFILE_FD, open_cfile(Path)),
                        put(?CFILE_PATH, Path),
                        fun() ->
                            ok = file:close(get(?CFILE_FD)),
                            ok = file:delete(Path),
                            erase(?CFILE_FD),
                            erase(?CFILE_PATH),
                            ok
                        end
                    end,
                    ?FORALL(
                        {Cmd, Args},
                        g_file_ops(),
                        begin
                            % Apply the same operations to the raw file handle
                            % and to the cfile one, and assert that they return
                            % the same results.
                            RawResult = apply_op(get(?RAW_FD), Cmd, Args),
                            CFileResult = apply_op(get(?CFILE_FD), Cmd, Args),
                            RawResult == CFileResult
                        end
                    )
                )
            )
    end.

open_raw(Path) ->
    {ok, Fd} = file:open(Path, [append, read, binary, raw]),
    Fd.

open_cfile(Path) ->
    Fd0 = open_raw(Path),
    {ok, Fd} = couch_cfile:dup(Fd0),
    ok = file:close(Fd0),
    Fd.

apply_op(#file_descriptor{module = prim_file} = Fd, reopen, []) ->
    ok = file:close(Fd),
    Fd1 = open_raw(get(?RAW_PATH)),
    put(?RAW_FD, Fd1),
    {ok, Len} = file:position(Fd1, eof),
    file:pread(Fd1, 0, Len);
apply_op(#file_descriptor{module = couch_cfile} = Fd, reopen, []) ->
    ok = file:close(Fd),
    Fd1 = open_cfile(get(?CFILE_PATH)),
    put(?CFILE_FD, Fd1),
    {ok, Len} = file:position(Fd1, eof),
    file:pread(Fd1, 0, Len);
apply_op(Fd, truncate_pos, [Pos]) ->
    % Position + truncate immediately after like in couch_file, otherwise
    % position will be reset to the end of the file on next write, and we
    % might not test this combination as often
    PosRes = file:position(Fd, Pos),
    TruncateRes = file:truncate(Fd),
    {PosRes, TruncateRes};
apply_op(Fd, Cmd, Args) ->
    apply(file, Cmd, [Fd] ++ Args).

g_file_ops() ->
    frequency([
        {15, g_write()},
        {15, g_truncate_pos()},
        {10, g_pread()},
        {5, g_datasync()},
        {3, g_truncate()},
        {3, g_position()},
        {2, g_reopen()}
    ]).

g_reopen() ->
    {reopen, []}.

g_pread() ->
    {pread, [range(-1, ?SIZE_LIMIT), range(-1, ?SIZE_LIMIT)]}.

g_position() ->
    {position, [
        frequency([
            {5, range(0, ?SIZE_LIMIT)},
            {1, eof}
        ])
    ]}.

g_write() ->
    {write, [binary()]}.

g_datasync() ->
    {datasync, []}.

g_truncate() ->
    {truncate, []}.

g_truncate_pos() ->
    {truncate_pos, [range(0, ?SIZE_LIMIT)]}.

-endif.
