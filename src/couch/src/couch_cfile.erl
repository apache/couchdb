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

% This module can dup(licate) raw file handles to create file handles which
% allow other proceses to issue pread calls. This lets clients completely
% bypass the couch_file gen_server message queue to do reads.
%
% At the POSIX API level pread() functions are thread-safe so calls can be
% issued in parallel by multiple threads. See these links to find out more
% about dup() and pread():
%
%  - https://www.man7.org/linux/man-pages/man2/dup.2.html
%  - https://www.man7.org/linux/man-pages/man2/pread.2.html

-module(couch_cfile).

-export([
    dup/1,
    pread/2,
    pread/3,
    close/1,
    position/2,
    datasync/1,
    write/2,
    truncate/1,
    fd/1,
    advise/4
]).

% Internal exports
%
-export([
    janitor/0
]).

-on_load(init/0).

-nifs([
    dup_nif/1,
    close_nif/1,
    close_fd_nif/1,
    pread_nif/3,
    info_nif/1,
    eof_nif/1,
    seek_nif/3,
    write_nif/2,
    datasync_nif/1,
    truncate_nif/1
]).

-include_lib("kernel/include/file.hrl").

% Duplicate an open file handle The dup-ed handle will reference the same "file
% description" as the prim_file raw handle. After duplicating, the original
% prim_file handle can be closed.
%
% Handles returned from dup/1 follow the standard Erlang/OTP #file_descriptor{}
% "protocol", so they can be be transparently used by regular `file` module for
% pread, write, truncate and position calls.
%
dup(#file_descriptor{module = prim_file} = Fd) ->
    case fd(Fd) of
        {ok, FdInt} ->
            case dup_nif(FdInt) of
                {ok, Ref} -> make_handle(Fd, Ref);
                {error, _} = Error -> Error
            end;
        {error, _} = Error ->
            Error
    end;
dup(_) ->
    {error, einval}.

close(#file_descriptor{module = ?MODULE} = Fd) ->
    close_nif(owner_handle(Fd));
close(_) ->
    {error, einval}.

pread(#file_descriptor{module = ?MODULE} = Fd, Pos, Len) ->
    pread_nif(handle(Fd), Pos, Len);
pread(_, _, _) ->
    {error, einval}.

pread(#file_descriptor{module = ?MODULE} = Fd, LocNums) ->
    pread_list(handle(Fd), LocNums, []);
pread(_, _) ->
    {error, einval}.

% Only position(Fd, eof|Pos) are supported. The variant eof one can be
% used by other processes. Only the owner can change position via the lseek API
% call. Readers (non-owners) can still call file:position(Fd, eof) to get the
% size of the file but they'll get it via the fstat call.
%
position(#file_descriptor{module = ?MODULE, data = Data} = Fd, eof) ->
    #{owner := Owner} = Data,
    case self() =:= Owner of
        true -> seek_nif(owner_handle(Fd), eof, 0);
        false -> eof_nif(handle(Fd))
    end;
position(#file_descriptor{module = ?MODULE} = Fd, Pos) when is_integer(Pos), Pos >= 0 ->
    seek_nif(owner_handle(Fd), bof, Pos);
position(_, _) ->
    {error, einval}.

datasync(#file_descriptor{module = ?MODULE} = Fd) ->
    datasync_nif(owner_handle(Fd));
datasync(_) ->
    {error, einval}.

write(#file_descriptor{module = ?MODULE} = Fd, IOData) ->
    write_1(owner_handle(Fd), erlang:iolist_to_iovec(IOData)).

truncate(#file_descriptor{module = ?MODULE} = Fd) ->
    truncate_nif(owner_handle(Fd)).

% Can use this for debugging to inspect the raw (integer) file descriptors
%
fd(#file_descriptor{module = prim_file} = RawFd) ->
    case prim_file:get_handle(RawFd) of
        <<FdInt:32/native-signed-integer>> -> {ok, FdInt};
        _ -> {error, einval}
    end;
fd(#file_descriptor{module = ?MODULE, data = Data}) ->
    #{handle := Ref} = Data,
    case info_nif(Ref) of
        {ok, {FdInt, _}} -> {ok, FdInt};
        {error, _} = Error -> Error
    end;
fd(_) ->
    {error, einval}.

% Since this is optional to implement we skip it for now
%
advise(#file_descriptor{module = ?MODULE} = Fd, Offset, Length, Advice) when
    is_integer(Offset) andalso Offset >= 0,
    is_integer(Length) andalso Length >= 0,
    is_atom(Advice)
->
    % Check the owner at least. If/when we implement this, only the owner will
    % get to call it.
    _ = owner_handle(Fd),
    ok;
advise(_, _, _, _) ->
    {error, einval}.

% Internal helpers

make_handle(#file_descriptor{module = prim_file} = Orig, Ref) ->
    Data = #{handle => Ref, owner => self()},
    Dup = #file_descriptor{module = ?MODULE, data = Data},
    case sanity_check(Orig, Dup) of
        true ->
            {ok, Dup};
        false ->
            close_nif(Ref),
            couch_log:error("~p : sanity check failed fd:~p dup:~p", [?MODULE, Orig, Dup]),
            {error, einval}
    end.

sanity_check(#file_descriptor{} = Orig, #file_descriptor{} = Dup) ->
    % Compare original and dup-ed saved origin fds. This should run after the
    % dup call. Not sure how this could fail (somehow the raw fd crashed and
    % re-opened by someone else right before dup-ing?) but it's better to be
    % safe than sorry here. Another important bit is re-fetching both
    % descriptors implicitly is asserting they haven't closed in the meantime.
    case fd(Orig) of
        {ok, Fd1} when is_integer(Fd1), Fd1 > -1 ->
            case info_nif(owner_handle(Dup)) of
                {ok, {_, Fd2}} when is_integer(Fd2), Fd2 > -1 -> Fd1 =:= Fd2;
                {ok, {_, _}} -> false;
                {error, _} -> false
            end;
        _ ->
            false
    end.

handle(#file_descriptor{module = ?MODULE, data = #{} = Data}) ->
    #{handle := Ref} = Data,
    Ref.

owner_handle(#file_descriptor{module = ?MODULE, data = #{} = Data}) ->
    #{handle := Ref, owner := Owner} = Data,
    case self() =:= Owner of
        true -> Ref;
        false -> error(not_on_controlling_process)
    end.

% These are are copied from the OTP pread/write logic.
%
pread_list(_Fd, [], ResultList) ->
    {ok, lists:reverse(ResultList)};
pread_list(Fd, [{Pos, Len} | Rest], ResultList) ->
    case pread_nif(Fd, Pos, Len) of
        {ok, Data} -> pread_list(Fd, Rest, [Data | ResultList]);
        eof -> pread_list(Fd, Rest, [eof | ResultList]);
        {error, _} = Error -> Error
    end.

write_1(Ref, IOVec) ->
    case write_nif(Ref, IOVec) of
        {continue, Remainder} ->
            write_1(Ref, Remainder);
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

init() ->
    case os:type() of
        {unix, _} ->
            PrivDir =
                case code:priv_dir(?MODULE) of
                    {error, _} ->
                        EbinDir = filename:dirname(code:which(?MODULE)),
                        AppPath = filename:dirname(EbinDir),
                        filename:join(AppPath, "priv");
                    Path ->
                        Path
                end,
            erlang:load_nif(filename:join(PrivDir, "couch_cfile"), spawn_janitor());
        {_, _} ->
            ok
    end.

% Spawn a janitor process to run all the delayed close calls on the dirty IO
% schedulers. This is what OTP does, so we stick to the same pattern in order
% to avoid re-inventing the wheel
%
spawn_janitor() ->
    case whereis(?MODULE) of
        ExistingPid when is_pid(ExistingPid) ->
            ExistingPid;
        _ ->
            Pid = spawn(?MODULE, janitor, []),
            register(?MODULE, Pid),
            Pid
    end.

janitor() ->
    % We want to crash the node if this process dies. This approximates the
    % behavior of erts_internal:spawn_system_process/3.
    link(whereis(init)),
    loop().

loop() ->
    receive
        {close, Fd} when is_integer(Fd) -> close_fd_nif(Fd);
        _ -> ok
    end,
    loop().

dup_nif(_) ->
    {error, einval}.

close_nif(_) ->
    {error, einval}.

close_fd_nif(_) ->
    {error, einval}.

pread_nif(_, _, _) ->
    {error, einval}.

eof_nif(_) ->
    {error, einval}.

info_nif(_) ->
    {error, einval}.

seek_nif(_, _, _) ->
    {error, einval}.

write_nif(_, _) ->
    {error, einval}.

datasync_nif(_) ->
    {error, einval}.

truncate_nif(_) ->
    {error, einval}.
