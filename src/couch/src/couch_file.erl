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

-module(couch_file).
-behaviour(gen_server).

-include_lib("couch/include/couch_db.hrl").

-define(INITIAL_WAIT, 60000).
-define(MONITOR_CHECK, 10000).
% 4 KiB
-define(SIZE_BLOCK, 16#1000).
-define(IS_OLD_STATE(S), is_pid(S#file.db_monitor)).
-define(PREFIX_SIZE, 5).
-define(DEFAULT_READ_COUNT, 1024).
-define(WRITE_XXHASH_CHECKSUMS_KEY, {?MODULE, write_xxhash_checksums}).
-define(WRITE_XXHASH_CHECKSUMS_DEFAULT, false).

-type block_id() :: non_neg_integer().
-type location() :: non_neg_integer().
-type header_size() :: non_neg_integer().

-record(file, {
    fd,
    is_sys,
    eof = 0,
    db_monitor,
    pread_limit = 0
}).

% public API
-export([open/1, open/2, close/1, bytes/1, sync/1, truncate/2, set_db_pid/2]).
-export([pread_term/2, pread_iolist/2, pread_binary/2]).
-export([append_binary/2]).
-export([append_raw_chunk/2, assemble_file_chunk_and_checksum/1]).
-export([append_term/2, append_term/3]).
-export([pread_terms/2]).
-export([append_terms/2, append_terms/3]).
-export([write_header/2, read_header/1]).
-export([delete/2, delete/3, nuke_dir/2, init_delete_dir/1]).
-export([last_read/1]).

% gen_server callbacks
-export([init/1, terminate/2, format_status/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% helper functions
-export([process_info/1]).

% test helper functions
-export([reset_checksum_persistent_term_config/0]).

%%----------------------------------------------------------------------
%% Args:   Valid Options are [create] and [create,overwrite].
%%  Files are opened in read/write mode.
%% Returns: On success, {ok, Fd}
%%  or {error, Reason} if the file could not be opened.
%%----------------------------------------------------------------------

open(Filepath) ->
    open(Filepath, []).

open(Filepath, Options) ->
    case
        gen_server:start_link(
            couch_file,
            {Filepath, Options, self(), Ref = make_ref()},
            []
        )
    of
        {ok, Fd} ->
            {ok, Fd};
        ignore ->
            % get the error
            receive
                {Ref, Pid, {error, Reason} = Error} ->
                    case process_info(self(), trap_exit) of
                        {trap_exit, true} ->
                            receive
                                {'EXIT', Pid, _} -> ok
                            end;
                        {trap_exit, false} ->
                            ok
                    end,
                    case {lists:member(nologifmissing, Options), Reason} of
                        {true, enoent} ->
                            ok;
                        _ ->
                            couch_log:error(
                                "Could not open file ~s: ~s",
                                [Filepath, file:format_error(Reason)]
                            )
                    end,
                    Error
            end;
        Error ->
            % We can't say much here, because it could be any kind of error.
            % Just let it bubble and an encapsulating subcomponent can perhaps
            % be more informative. It will likely appear in the SASL log, anyway.
            Error
    end.

set_db_pid(Fd, Pid) ->
    gen_server:call(Fd, {set_db_pid, Pid}).

%%----------------------------------------------------------------------
%% Purpose: To append an Erlang term to the end of the file.
%% Args:    Erlang term to serialize and append to the file.
%% Returns: {ok, Pos, NumBytesWritten} where Pos is the file offset to
%%  the beginning the serialized  term. Use pread_term to read the term
%%  back.
%%  or {error, Reason}.
%%----------------------------------------------------------------------

append_term(Fd, Term) ->
    append_term(Fd, Term, []).

append_term(Fd, Term, Options) ->
    Comp = couch_util:get_value(compression, Options, ?DEFAULT_COMPRESSION),
    append_binary(Fd, couch_compress:compress(Term, Comp)).

%%----------------------------------------------------------------------
%% Purpose: To append an Erlang binary to the end of the file.
%% Args:    Erlang term to serialize and append to the file.
%% Returns: {ok, Pos, NumBytesWritten} where Pos is the file offset to the
%%  beginning the serialized term. Use pread_term to read the term back.
%%  or {error, Reason}.
%%----------------------------------------------------------------------

append_binary(Fd, Bin) ->
    ioq:call(Fd, {append_bin, assemble_file_chunk(Bin)}, erlang:get(io_priority)).

append_raw_chunk(Fd, Chunk) ->
    ioq:call(Fd, {append_bin, Chunk}, erlang:get(io_priority)).

assemble_file_chunk(Bin) ->
    [<<0:1/integer, (iolist_size(Bin)):31/integer>>, Bin].

assemble_file_chunk_and_checksum(Bin) ->
    Checksum = generate_checksum(Bin),
    [<<1:1/integer, (iolist_size(Bin)):31/integer>>, Checksum, Bin].

%%----------------------------------------------------------------------
%% Purpose: Reads a term from a file that was written with append_term
%% Args:    Pos, the offset into the file where the term is serialized.
%% Returns: {ok, Term}
%%  or {error, Reason}.
%%----------------------------------------------------------------------

pread_term(Fd, Pos) ->
    {ok, Bin} = pread_binary(Fd, Pos),
    {ok, couch_compress:decompress(Bin)}.

%%----------------------------------------------------------------------
%% Purpose: Reads a binrary from a file that was written with append_binary
%% Args:    Pos, the offset into the file where the term is serialized.
%% Returns: {ok, Term}
%%  or {error, Reason}.
%%----------------------------------------------------------------------

pread_binary(Fd, Pos) ->
    {ok, L} = pread_iolist(Fd, Pos),
    {ok, iolist_to_binary(L)}.

pread_iolist(Fd, Pos) ->
    case ioq:call(Fd, {pread_iolist, Pos}, erlang:get(io_priority)) of
        {ok, IoList, Checksum} ->
            {ok, verify_checksum(Fd, Pos, IoList, Checksum, false)};
        Error ->
            Error
    end.

pread_terms(Fd, PosList) ->
    {ok, Bins} = pread_binaries(Fd, PosList),
    Terms = lists:map(
        fun(Bin) ->
            couch_compress:decompress(Bin)
        end,
        Bins
    ),
    {ok, Terms}.

pread_binaries(Fd, PosList) ->
    {ok, Data} = pread_iolists(Fd, PosList),
    {ok, lists:map(fun erlang:iolist_to_binary/1, Data)}.

pread_iolists(Fd, PosList) ->
    case ioq:call(Fd, {pread_iolists, PosList}, erlang:get(io_priority)) of
        {ok, DataAndChecksums} ->
            Data = lists:zipwith(
                fun(Pos, {IoList, Checksum}) ->
                    verify_checksum(Fd, Pos, IoList, Checksum, false)
                end,
                PosList,
                DataAndChecksums
            ),
            {ok, Data};
        Error ->
            Error
    end.

append_terms(Fd, Terms) ->
    append_terms(Fd, Terms, []).

append_terms(Fd, Terms, Options) ->
    Comp = couch_util:get_value(compression, Options, ?DEFAULT_COMPRESSION),
    Bins = lists:map(
        fun(Term) ->
            couch_compress:compress(Term, Comp)
        end,
        Terms
    ),
    append_binaries(Fd, Bins).

append_binaries(Fd, Bins) ->
    WriteBins = lists:map(fun assemble_file_chunk/1, Bins),
    ioq:call(Fd, {append_bins, WriteBins}, erlang:get(io_priority)).

%%----------------------------------------------------------------------
%% Purpose: The length of a file, in bytes.
%% Returns: {ok, Bytes}
%%  or {error, Reason}.
%%----------------------------------------------------------------------

% length in bytes
bytes(Fd) ->
    gen_server:call(Fd, bytes, infinity).

%%----------------------------------------------------------------------
%% Purpose: Truncate a file to the number of bytes.
%% Returns: ok
%%  or {error, Reason}.
%%----------------------------------------------------------------------

truncate(Fd, Pos) ->
    gen_server:call(Fd, {truncate, Pos}, infinity).

%%----------------------------------------------------------------------
%% Purpose: Ensure all bytes written to the file are flushed to disk.
%% Returns: ok
%%  or {error, Reason}.
%%----------------------------------------------------------------------

sync(Filepath) when is_list(Filepath) ->
    case file:open(Filepath, [append, raw]) of
        {ok, Fd} ->
            try
                case fsync(Fd) of
                    ok ->
                        ok;
                    {error, Reason} ->
                        erlang:error({fsync_error, Reason})
                end
            after
                ok = file:close(Fd)
            end;
        {error, Error} ->
            erlang:error(Error)
    end;
sync(Fd) ->
    case gen_server:call(Fd, sync, infinity) of
        ok ->
            ok;
        {error, Reason} ->
            erlang:error({fsync_error, Reason})
    end.

%%----------------------------------------------------------------------
%% Purpose: Close the file.
%% Returns: ok
%%----------------------------------------------------------------------
close(Fd) ->
    gen_server:call(Fd, close, infinity).

delete(RootDir, Filepath) ->
    delete(RootDir, Filepath, []).

delete(RootDir, FullFilePath, Options) ->
    EnableRecovery = config:get_boolean(
        "couchdb",
        "enable_database_recovery",
        false
    ),
    Async = not lists:member(sync, Options),
    Context = couch_util:get_value(context, Options, compaction),
    case Context =:= delete andalso EnableRecovery of
        true ->
            rename_file(FullFilePath);
        false ->
            DeleteAfterRename = config:get_boolean(
                "couchdb",
                "delete_after_rename",
                true
            ),
            delete_file(RootDir, FullFilePath, Async, DeleteAfterRename)
    end.

delete_file(RootDir, Filepath, Async, DeleteAfterRename) ->
    DelFile = filename:join([RootDir, ".delete", ?b2l(couch_uuids:random())]),
    case file:rename(Filepath, DelFile) of
        ok when DeleteAfterRename ->
            if
                (Async) ->
                    spawn(file, delete, [DelFile]),
                    ok;
                true ->
                    file:delete(DelFile)
            end;
        Else ->
            Else
    end.

rename_file(Original) ->
    DeletedFileName = deleted_filename(Original),
    Now = calendar:local_time(),
    case file:rename(Original, DeletedFileName) of
        ok -> file:change_time(DeletedFileName, Now);
        Else -> Else
    end.

deleted_filename(Original) ->
    {{Y, Mon, D}, {H, Min, S}} = calendar:universal_time(),
    Suffix = lists:flatten(
        io_lib:format(
            ".~w~2.10.0B~2.10.0B." ++
                "~2.10.0B~2.10.0B~2.10.0B.deleted" ++
                filename:extension(Original),
            [Y, Mon, D, H, Min, S]
        )
    ),
    filename:rootname(Original) ++ Suffix.

nuke_dir(RootDelDir, Dir) ->
    EnableRecovery = config:get_boolean(
        "couchdb",
        "enable_database_recovery",
        false
    ),
    case EnableRecovery of
        true ->
            rename_file(Dir);
        false ->
            delete_dir(RootDelDir, Dir)
    end.

delete_dir(RootDelDir, Dir) ->
    DeleteAfterRename = config:get_boolean(
        "couchdb",
        "delete_after_rename",
        true
    ),
    FoldFun = fun(File) ->
        Path = Dir ++ "/" ++ File,
        case filelib:is_dir(Path) of
            true ->
                ok = nuke_dir(RootDelDir, Path),
                file:del_dir(Path);
            false ->
                delete_file(RootDelDir, Path, false, DeleteAfterRename)
        end
    end,
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(FoldFun, Files),
            ok = file:del_dir(Dir);
        {error, enoent} ->
            ok
    end.

init_delete_dir(RootDir) ->
    Dir = filename:join(RootDir, ".delete"),
    % note: ensure_dir requires an actual filename companent, which is the
    % reason for "foo".
    filelib:ensure_dir(filename:join(Dir, "foo")),
    spawn(fun() ->
        filelib:fold_files(
            Dir,
            ".*",
            true,
            fun(Filename, _) ->
                ok = file:delete(Filename)
            end,
            ok
        )
    end),
    ok.

read_header(Fd) ->
    case ioq:call(Fd, find_header, erlang:get(io_priority)) of
        {ok, Bin} ->
            {ok, binary_to_term(Bin)};
        Else ->
            Else
    end.

write_header(Fd, Data) ->
    Bin = ?term_to_bin(Data),
    Checksum = generate_checksum(Bin),
    % now we assemble the final header binary and write to disk
    FinalBin = <<Checksum/binary, Bin/binary>>,
    ioq:call(Fd, {write_header, FinalBin}, erlang:get(io_priority)).

init_status_error(ReturnPid, Ref, Error) ->
    ReturnPid ! {Ref, self(), Error},
    ignore.

last_read(Fd) when is_pid(Fd) ->
    Now = os:timestamp(),
    couch_util:process_dict_get(Fd, read_timestamp, Now).

% server functions

init({Filepath, Options, ReturnPid, Ref}) ->
    OpenOptions = file_open_options(Options),
    Limit = get_pread_limit(),
    IsSys = lists:member(sys_db, Options),
    update_read_timestamp(),
    case lists:member(create, Options) of
        true ->
            filelib:ensure_dir(Filepath),
            case file:open(Filepath, OpenOptions) of
                {ok, Fd} ->
                    %% Save Fd in process dictionary for debugging purposes
                    put(couch_file_fd, {Fd, Filepath}),
                    {ok, Length} = file:position(Fd, eof),
                    case Length > 0 of
                        true ->
                            % this means the file already exists and has data.
                            % FYI: We don't differentiate between empty files and non-existant
                            % files here.
                            case lists:member(overwrite, Options) of
                                true ->
                                    {ok, 0} = file:position(Fd, 0),
                                    ok = file:truncate(Fd),
                                    ok = fsync(Fd),
                                    maybe_track_open_os_files(Options),
                                    erlang:send_after(?INITIAL_WAIT, self(), maybe_close),
                                    {ok, #file{fd = Fd, is_sys = IsSys, pread_limit = Limit}};
                                false ->
                                    ok = file:close(Fd),
                                    init_status_error(ReturnPid, Ref, {error, eexist})
                            end;
                        false ->
                            maybe_track_open_os_files(Options),
                            erlang:send_after(?INITIAL_WAIT, self(), maybe_close),
                            {ok, #file{fd = Fd, is_sys = IsSys, pread_limit = Limit}}
                    end;
                Error ->
                    init_status_error(ReturnPid, Ref, Error)
            end;
        false ->
            % open in read mode first, so we don't create the file if it doesn't exist.
            case file:open(Filepath, [read, raw]) of
                {ok, Fd_Read} ->
                    case file:open(Filepath, OpenOptions) of
                        {ok, Fd} ->
                            %% Save Fd in process dictionary for debugging purposes
                            put(couch_file_fd, {Fd, Filepath}),
                            ok = file:close(Fd_Read),
                            maybe_track_open_os_files(Options),
                            {ok, Eof} = file:position(Fd, eof),
                            erlang:send_after(?INITIAL_WAIT, self(), maybe_close),
                            {ok, #file{fd = Fd, eof = Eof, is_sys = IsSys, pread_limit = Limit}};
                        Error ->
                            init_status_error(ReturnPid, Ref, Error)
                    end;
                Error ->
                    init_status_error(ReturnPid, Ref, Error)
            end
    end.

file_open_options(Options) ->
    [read, raw, binary] ++
        case lists:member(read_only, Options) of
            true ->
                [];
            false ->
                [append]
        end.

maybe_track_open_os_files(Options) ->
    case not lists:member(sys_db, Options) of
        true ->
            couch_stats_process_tracker:track([couchdb, open_os_files]);
        false ->
            ok
    end.

terminate(_Reason, #file{fd = nil}) ->
    ok;
terminate(_Reason, #file{fd = Fd}) ->
    ok = file:close(Fd).

handle_call(Msg, From, File) when ?IS_OLD_STATE(File) ->
    handle_call(Msg, From, upgrade_state(File));
handle_call(close, _From, #file{fd = Fd} = File) ->
    {stop, normal, file:close(Fd), File#file{fd = nil}};
handle_call({pread_iolist, Pos}, _From, File) ->
    update_read_timestamp(),
    {LenIolist, NextPos} = read_raw_iolist_int(File, Pos, 4),
    case iolist_to_binary(LenIolist) of
        % an checksum-prefixed term
        <<1:1/integer, Len:31/integer>> ->
            {ChecksumAndIoList, _} = read_raw_iolist_int(File, NextPos, Len + 16),
            {Checksum, IoList} = extract_checksum(ChecksumAndIoList),
            {reply, {ok, IoList, Checksum}, File};
        <<0:1/integer, Len:31/integer>> ->
            {Iolist, _} = read_raw_iolist_int(File, NextPos, Len),
            {reply, {ok, Iolist, <<>>}, File}
    end;
handle_call({pread_iolists, PosL}, _From, File) ->
    update_read_timestamp(),
    LocNums1 = [{Pos, 4} || Pos <- PosL],
    DataSizes = read_multi_raw_iolists_int(File, LocNums1),
    LocNums2 = lists:map(
        fun({LenIoList, NextPos}) ->
            case iolist_to_binary(LenIoList) of
                % a checksum-prefixed term
                <<1:1/integer, Len:31/integer>> ->
                    {NextPos, Len + 16};
                <<0:1/integer, Len:31/integer>> ->
                    {NextPos, Len}
            end
        end,
        DataSizes
    ),
    Resps = read_multi_raw_iolists_int(File, LocNums2),
    Extracted = lists:zipwith(
        fun({LenIoList, _}, {IoList, _}) ->
            case iolist_to_binary(LenIoList) of
                <<1:1/integer, _:31/integer>> ->
                    {Checksum, IoList} = extract_checksum(IoList),
                    {IoList, Checksum};
                <<0:1/integer, _:31/integer>> ->
                    {IoList, <<>>}
            end
        end,
        DataSizes,
        Resps
    ),
    {reply, {ok, Extracted}, File};
handle_call(bytes, _From, #file{fd = Fd} = File) ->
    {reply, file:position(Fd, eof), File};
handle_call({set_db_pid, Pid}, _From, #file{db_monitor = OldRef} = File) ->
    case is_reference(OldRef) of
        true -> demonitor(OldRef, [flush]);
        false -> ok
    end,
    Ref = monitor(process, Pid),
    {reply, ok, File#file{db_monitor = Ref}};
handle_call(sync, _From, #file{fd = Fd} = File) ->
    case fsync(Fd) of
        ok ->
            {reply, ok, File};
        {error, _} = Error ->
            % We're intentionally dropping all knowledge
            % of this Fd so that we don't accidentally
            % recover in some whacky edge case that I
            % can't fathom.
            {stop, Error, Error, #file{fd = nil}}
    end;
handle_call({truncate, Pos}, _From, #file{fd = Fd} = File) ->
    {ok, Pos} = file:position(Fd, Pos),
    case file:truncate(Fd) of
        ok ->
            {reply, ok, File#file{eof = Pos}};
        Error ->
            {reply, Error, File}
    end;
handle_call({append_bin, Bin}, _From, #file{fd = Fd, eof = Pos} = File) ->
    Blocks = make_blocks(Pos rem ?SIZE_BLOCK, Bin),
    Size = iolist_size(Blocks),
    case file:write(Fd, Blocks) of
        ok ->
            {reply, {ok, Pos, Size}, File#file{eof = Pos + Size}};
        Error ->
            {reply, Error, reset_eof(File)}
    end;
handle_call({append_bins, Bins}, _From, #file{fd = Fd, eof = Pos} = File) ->
    {BlockResps, FinalPos} = lists:mapfoldl(
        fun(Bin, PosAcc) ->
            Blocks = make_blocks(PosAcc rem ?SIZE_BLOCK, Bin),
            Size = iolist_size(Blocks),
            {{Blocks, {PosAcc, Size}}, PosAcc + Size}
        end,
        Pos,
        Bins
    ),
    {AllBlocks, Resps} = lists:unzip(BlockResps),
    case file:write(Fd, AllBlocks) of
        ok ->
            {reply, {ok, Resps}, File#file{eof = FinalPos}};
        Error ->
            {reply, Error, reset_eof(File)}
    end;
handle_call({write_header, Bin}, _From, #file{fd = Fd, eof = Pos} = File) ->
    BinSize = byte_size(Bin),
    case Pos rem ?SIZE_BLOCK of
        0 ->
            Padding = <<>>;
        BlockOffset ->
            Padding = <<0:(8 * (?SIZE_BLOCK - BlockOffset))>>
    end,
    FinalBin = [Padding, <<1, BinSize:32/integer>> | make_blocks(5, [Bin])],
    case file:write(Fd, FinalBin) of
        ok ->
            {reply, ok, File#file{eof = Pos + iolist_size(FinalBin)}};
        Error ->
            {reply, Error, reset_eof(File)}
    end;
handle_call(find_header, _From, #file{fd = Fd, eof = Pos} = File) ->
    {reply, find_header(Fd, Pos div ?SIZE_BLOCK), File}.

handle_cast(close, Fd) ->
    {stop, normal, Fd}.

handle_info(Msg, File) when ?IS_OLD_STATE(File) ->
    handle_info(Msg, upgrade_state(File));
handle_info(maybe_close, File) ->
    case is_idle(File) of
        true ->
            {stop, normal, File};
        false ->
            erlang:send_after(?MONITOR_CHECK, self(), maybe_close),
            {noreply, File}
    end;
handle_info({'DOWN', Ref, process, _Pid, _Info}, #file{db_monitor = Ref} = File) ->
    case is_idle(File) of
        true -> {stop, normal, File};
        false -> {noreply, File}
    end.

format_status(_Opt, [PDict, #file{} = File]) ->
    {_Fd, FilePath} = couch_util:get_value(couch_file_fd, PDict),
    [{data, [{"State", File}, {"InitialFilePath", FilePath}]}].

fsync(Fd) ->
    T0 = erlang:monotonic_time(),
    Res = file:sync(Fd),
    T1 = erlang:monotonic_time(),
    % Since histograms can consume floating point values we can measure in
    % nanoseconds, then turn it into floating point milliseconds
    DtNSec = erlang:convert_time_unit(T1 - T0, native, nanosecond),
    DtMSec = DtNSec / 1000000,
    couch_stats:update_histogram([fsync, time], DtMSec),
    couch_stats:increment_counter([fsync, count]),
    Res.

find_header(Fd, Block) ->
    case (catch load_header(Fd, Block)) of
        {ok, Bin} ->
            {ok, Bin};
        _Error ->
            ReadCount = config:get_integer(
                "couchdb", "find_header_read_count", ?DEFAULT_READ_COUNT
            ),
            find_header(Fd, Block - 1, ReadCount)
    end.

load_header(Fd, Block) ->
    {ok, <<1, HeaderLen:32/integer, RestBlock/binary>>} =
        file:pread(Fd, Block * ?SIZE_BLOCK, ?SIZE_BLOCK),
    load_header(Fd, Block * ?SIZE_BLOCK, HeaderLen, RestBlock).

load_header(Fd, Pos, HeaderLen) ->
    load_header(Fd, Pos, HeaderLen, <<>>).

load_header(Fd, Pos, HeaderLen, RestBlock) ->
    TotalBytes = calculate_total_read_len(?PREFIX_SIZE, HeaderLen),
    RawBin =
        case TotalBytes =< byte_size(RestBlock) of
            true ->
                <<RawBin0:TotalBytes/binary, _/binary>> = RestBlock,
                RawBin0;
            false ->
                ReadStart = Pos + ?PREFIX_SIZE + byte_size(RestBlock),
                ReadLen = TotalBytes - byte_size(RestBlock),
                {ok, Missing} = file:pread(Fd, ReadStart, ReadLen),
                <<RestBlock/binary, Missing/binary>>
        end,
    <<Checksum:16/binary, HeaderBin/binary>> =
        iolist_to_binary(remove_block_prefixes(?PREFIX_SIZE, RawBin)),
    {ok, verify_checksum(Fd, Pos, HeaderBin, Checksum, true)}.

%% Read multiple block locations using a single file:pread/2.
-spec find_header(file:fd(), block_id(), non_neg_integer()) ->
    {ok, binary()} | no_valid_header.
find_header(_Fd, Block, _ReadCount) when Block < 0 ->
    no_valid_header;
find_header(Fd, Block, ReadCount) ->
    FirstBlock = max(0, Block - ReadCount + 1),
    BlockLocations = [?SIZE_BLOCK * B || B <- lists:seq(FirstBlock, Block)],
    {ok, DataL} = file:pread(Fd, [{L, ?PREFIX_SIZE} || L <- BlockLocations]),
    %% Since BlockLocations are ordered from oldest to newest, we rely
    %% on lists:foldl/3 to reverse the order, making HeaderLocations
    %% correctly ordered from newest to oldest.
    HeaderLocations = lists:foldl(
        fun
            ({Loc, <<1, HeaderSize:32/integer>>}, Acc) ->
                [{Loc, HeaderSize} | Acc];
            (_, Acc) ->
                Acc
        end,
        [],
        lists:zip(BlockLocations, DataL)
    ),
    case find_newest_header(Fd, HeaderLocations) of
        {ok, _Location, HeaderBin} ->
            {ok, HeaderBin};
        _ ->
            ok = file:advise(
                Fd, hd(BlockLocations), ReadCount * ?SIZE_BLOCK, dont_need
            ),
            NextBlock = hd(BlockLocations) div ?SIZE_BLOCK - 1,
            find_header(Fd, NextBlock, ReadCount)
    end.

-spec find_newest_header(file:fd(), [{location(), header_size()}]) ->
    {ok, location(), binary()} | not_found.
find_newest_header(_Fd, []) ->
    not_found;
find_newest_header(Fd, [{Location, Size} | LocationSizes]) ->
    case (catch load_header(Fd, Location, Size)) of
        {ok, HeaderBin} ->
            {ok, Location, HeaderBin};
        _Error ->
            find_newest_header(Fd, LocationSizes)
    end.

-spec read_raw_iolist_int(#file{}, Pos :: non_neg_integer(), Len :: non_neg_integer()) ->
    {Data :: iolist(), CurPos :: non_neg_integer()}.
% 0110 UPGRADE CODE
read_raw_iolist_int(Fd, {Pos, _Size}, Len) ->
    read_raw_iolist_int(Fd, Pos, Len);
read_raw_iolist_int(#file{fd = Fd} = File, Pos, Len) ->
    {Pos, TotalBytes} = get_pread_locnum(File, Pos, Len),
    case catch file:pread(Fd, Pos, TotalBytes) of
        {ok, <<RawBin:TotalBytes/binary>>} ->
            {remove_block_prefixes(Pos rem ?SIZE_BLOCK, RawBin), Pos + TotalBytes};
        Else ->
            % This clause matches when the file we are working with got truncated
            % outside of CouchDB after we opened it. To find affected files, we
            % need to log the file path.
            %
            % Technically, this should also go into read_multi_raw_iolists_int/2,
            % but that doesn’t seem to be in use anywhere.
            {_Fd, Filepath} = get(couch_file_fd),
            throw({file_truncate_error, Else, Filepath})
    end.

% TODO: check if this is really unused
read_multi_raw_iolists_int(#file{fd = Fd} = File, PosLens) ->
    LocNums = lists:map(
        fun({Pos, Len}) ->
            get_pread_locnum(File, Pos, Len)
        end,
        PosLens
    ),
    {ok, Bins} = file:pread(Fd, LocNums),
    lists:zipwith(
        fun({Pos, TotalBytes}, Bin) ->
            <<RawBin:TotalBytes/binary>> = Bin,
            {remove_block_prefixes(Pos rem ?SIZE_BLOCK, RawBin), Pos + TotalBytes}
        end,
        LocNums,
        Bins
    ).

get_pread_locnum(File, Pos, Len) ->
    BlockOffset = Pos rem ?SIZE_BLOCK,
    TotalBytes = calculate_total_read_len(BlockOffset, Len),
    case Pos + TotalBytes of
        Size when Size > File#file.eof ->
            couch_stats:increment_counter([pread, exceed_eof]),
            {_Fd, Filepath} = get(couch_file_fd),
            throw({read_beyond_eof, Filepath});
        Size when Size > File#file.pread_limit ->
            couch_stats:increment_counter([pread, exceed_limit]),
            {_Fd, Filepath} = get(couch_file_fd),
            throw({exceed_pread_limit, Filepath, File#file.pread_limit});
        _ ->
            {Pos, TotalBytes}
    end.

-spec extract_checksum(iolist()) -> {binary(), iolist()}.
extract_checksum(FullIoList) ->
    {ChecksumList, IoList} = split_iolist(FullIoList, 16, []),
    {iolist_to_binary(ChecksumList), IoList}.

calculate_total_read_len(0, FinalLen) ->
    calculate_total_read_len(1, FinalLen) + 1;
calculate_total_read_len(BlockOffset, FinalLen) ->
    case ?SIZE_BLOCK - BlockOffset of
        BlockLeft when BlockLeft >= FinalLen ->
            FinalLen;
        BlockLeft ->
            FinalLen + ((FinalLen - BlockLeft) div (?SIZE_BLOCK - 1)) +
                if
                    ((FinalLen - BlockLeft) rem (?SIZE_BLOCK - 1)) =:= 0 -> 0;
                    true -> 1
                end
    end.

remove_block_prefixes(_BlockOffset, <<>>) ->
    [];
remove_block_prefixes(0, <<_BlockPrefix, Rest/binary>>) ->
    remove_block_prefixes(1, Rest);
remove_block_prefixes(BlockOffset, Bin) ->
    BlockBytesAvailable = ?SIZE_BLOCK - BlockOffset,
    case size(Bin) of
        Size when Size > BlockBytesAvailable ->
            <<DataBlock:BlockBytesAvailable/binary, Rest/binary>> = Bin,
            [DataBlock | remove_block_prefixes(0, Rest)];
        _Size ->
            [Bin]
    end.

make_blocks(_BlockOffset, []) ->
    [];
make_blocks(0, IoList) ->
    [<<0>> | make_blocks(1, IoList)];
make_blocks(BlockOffset, IoList) ->
    case split_iolist(IoList, (?SIZE_BLOCK - BlockOffset), []) of
        {Begin, End} ->
            [Begin | make_blocks(0, End)];
        _SplitRemaining ->
            IoList
    end.

%% @doc Returns a tuple where the first element contains the leading SplitAt
%% bytes of the original iolist, and the 2nd element is the tail. If SplitAt
%% is larger than byte_size(IoList), return the difference.
-spec split_iolist(IoList :: iolist(), SplitAt :: non_neg_integer(), Acc :: list()) ->
    {iolist(), iolist()} | non_neg_integer().
split_iolist(List, 0, BeginAcc) ->
    {lists:reverse(BeginAcc), List};
split_iolist([], SplitAt, _BeginAcc) ->
    SplitAt;
split_iolist([<<Bin/binary>> | Rest], SplitAt, BeginAcc) when SplitAt > byte_size(Bin) ->
    split_iolist(Rest, SplitAt - byte_size(Bin), [Bin | BeginAcc]);
split_iolist([<<Bin/binary>> | Rest], SplitAt, BeginAcc) ->
    <<Begin:SplitAt/binary, End/binary>> = Bin,
    split_iolist([End | Rest], 0, [Begin | BeginAcc]);
split_iolist([Sublist | Rest], SplitAt, BeginAcc) when is_list(Sublist) ->
    case split_iolist(Sublist, SplitAt, BeginAcc) of
        {Begin, End} ->
            {Begin, [End | Rest]};
        SplitRemaining ->
            split_iolist(Rest, SplitAt - (SplitAt - SplitRemaining), [Sublist | BeginAcc])
    end;
split_iolist([Byte | Rest], SplitAt, BeginAcc) when is_integer(Byte) ->
    split_iolist(Rest, SplitAt - 1, [Byte | BeginAcc]).

monitored_by_pids() ->
    {monitored_by, PidsAndRefs} = process_info(self(), monitored_by),
    lists:filter(fun is_pid/1, PidsAndRefs).

verify_checksum(_Fd, _Pos, IoList, <<>>, _IsHeader) ->
    IoList;
verify_checksum(Fd, Pos, IoList, Checksum, IsHeader) ->
    % If writing xxhash checksums is enabled, check those first, then check
    % legacy ones. If any legacy ones are found, bump the legacy metric. If
    % generating xxhash checksums is disabled, assume most checksums would be
    % legacy, so check that first, and then, in a likely case of release
    % downgrade, check xxhash ones.
    case generate_xxhash_checksums() of
        true ->
            case exxhash:xxhash128(iolist_to_binary(IoList)) of
                Checksum ->
                    IoList;
                <<_/binary>> ->
                    case couch_hash:md5_hash(IoList) of
                        Checksum ->
                            legacy_checksums_stats_update(),
                            IoList;
                        _ ->
                            report_checksum_error(Fd, Pos, IsHeader)
                    end
            end;
        false ->
            case couch_hash:md5_hash(IoList) of
                Checksum ->
                    IoList;
                _ ->
                    case exxhash:xxhash128(iolist_to_binary(IoList)) of
                        Checksum ->
                            IoList;
                        <<_/binary>> ->
                            report_checksum_error(Fd, Pos, IsHeader)
                    end
            end
    end.

report_checksum_error(_Fd, _Pos, _IsHeader = true) ->
    % When loading header we'd expect to find junk data which might not have
    % been committed at the end of the file, so we don't emit an emergency log for it.
    exit({file_corruption, <<"file corruption">>});
report_checksum_error(Fd, Pos, _IsHeader = false) ->
    couch_log:emergency("File corruption in ~p at position ~B", [Fd, Pos]),
    exit({file_corruption, <<"file corruption">>}).

% System dbs aren't monitored by couch_stats_process_tracker
is_idle(#file{is_sys = true}) ->
    case monitored_by_pids() of
        [] -> true;
        _ -> false
    end;
is_idle(#file{is_sys = false}) ->
    Tracker = whereis(couch_stats_process_tracker),
    case monitored_by_pids() of
        [] -> true;
        [Tracker] -> true;
        [_] -> exit(tracker_monitoring_failed);
        _ -> false
    end.

-spec process_info(CouchFilePid :: pid()) ->
    {Fd :: pid() | tuple(), FilePath :: string()} | undefined.

process_info(Pid) ->
    couch_util:process_dict_get(Pid, couch_file_fd).

update_read_timestamp() ->
    put(read_timestamp, os:timestamp()).

upgrade_state(#file{db_monitor = DbPid} = File) when is_pid(DbPid) ->
    unlink(DbPid),
    Ref = monitor(process, DbPid),
    File#file{db_monitor = Ref};
upgrade_state(State) ->
    State.

get_pread_limit() ->
    case config:get_integer("couchdb", "max_pread_size", 0) of
        N when N > 0 -> N;
        _ -> infinity
    end.

%% in event of a partially successful write.
reset_eof(#file{} = File) ->
    {ok, Eof} = file:position(File#file.fd, eof),
    File#file{eof = Eof}.

-spec generate_checksum(binary()) -> <<_:128>>.
generate_checksum(Bin) when is_binary(Bin) ->
    case generate_xxhash_checksums() of
        true -> <<_:128>> = exxhash:xxhash128(Bin);
        false -> <<_:128>> = couch_hash:md5_hash(Bin)
    end.

legacy_checksums_stats_update() ->
    % Bump stats only if we're writing new checksums.
    case generate_xxhash_checksums() of
        true -> couch_stats:increment_counter([couchdb, legacy_checksums]);
        false -> ok
    end.

reset_checksum_persistent_term_config() ->
    persistent_term:erase(?WRITE_XXHASH_CHECKSUMS_KEY).

generate_xxhash_checksums() ->
    % Caching the config value here as we'd need to call this per file chunk
    % and also from various processes (not just couch_file pids). Node must be
    % restarted for the new value to take effect.
    case persistent_term:get(?WRITE_XXHASH_CHECKSUMS_KEY, not_cached) of
        not_cached ->
            Default = ?WRITE_XXHASH_CHECKSUMS_DEFAULT,
            Val = config:get_boolean("couchdb", "write_xxhash_checksums", Default),
            persistent_term:put(?WRITE_XXHASH_CHECKSUMS_KEY, Val),
            Val;
        Val when is_boolean(Val) ->
            Val
    end.

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

deleted_filename_test_() ->
    DbNames = ["dbname", "db.name", "user/dbname"],
    Fixtures = make_filename_fixtures(DbNames),
    lists:map(
        fun(Fixture) ->
            should_create_proper_deleted_filename(Fixture)
        end,
        Fixtures
    ).

should_create_proper_deleted_filename(Before) ->
    {Before,
        ?_test(begin
            BeforeExtension = filename:extension(Before),
            BeforeBasename = filename:basename(Before, BeforeExtension),
            Re = "^" ++ BeforeBasename ++ "\.[0-9]{8}\.[0-9]{6}\.deleted\..*$",
            After = deleted_filename(Before),
            ?assertEqual(
                match,
                re:run(filename:basename(After), Re, [{capture, none}])
            ),
            ?assertEqual(BeforeExtension, filename:extension(After))
        end)}.

make_filename_fixtures(DbNames) ->
    Formats = [
        "~s.couch",
        ".~s_design/mrview/3133e28517e89a3e11435dd5ac4ad85a.view",
        "shards/00000000-1fffffff/~s.1458336317.couch",
        ".shards/00000000-1fffffff/~s.1458336317_design",
        ".shards/00000000-1fffffff/~s.1458336317_design"
        "/mrview/3133e28517e89a3e11435dd5ac4ad85a.view"
    ],
    lists:flatmap(
        fun(DbName) ->
            lists:map(
                fun(Format) ->
                    filename:join("/srv/data", io_lib:format(Format, [DbName]))
                end,
                Formats
            )
        end,
        DbNames
    ).

-endif.
