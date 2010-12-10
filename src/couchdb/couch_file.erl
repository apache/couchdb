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

-include("couch_db.hrl").

-define(SIZE_BLOCK, 4096).

-record(file, {
    fd,
    tail_append_begin = 0, % 09 UPGRADE CODE
    eof = 0
    }).

-export([open/1, open/2, close/1, bytes/1, sync/1, append_binary/2,old_pread/3]).
-export([append_term/2, pread_term/2, pread_iolist/2, write_header/2]).
-export([pread_binary/2, read_header/1, truncate/2, upgrade_old_header/2]).
-export([append_term_md5/2,append_binary_md5/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, code_change/3, handle_info/2]).
-export([delete/2,delete/3,init_delete_dir/1]).

%%----------------------------------------------------------------------
%% Args:   Valid Options are [create] and [create,overwrite].
%%  Files are opened in read/write mode.
%% Returns: On success, {ok, Fd}
%%  or {error, Reason} if the file could not be opened.
%%----------------------------------------------------------------------

open(Filepath) ->
    open(Filepath, []).

open(Filepath, Options) ->
    case gen_server:start_link(couch_file,
            {Filepath, Options, self(), Ref = make_ref()}, []) of
    {ok, Fd} ->
        {ok, Fd};
    ignore ->
        % get the error
        receive
        {Ref, Pid, Error} ->
            case process_info(self(), trap_exit) of
            {trap_exit, true} -> receive {'EXIT', Pid, _} -> ok end;
            {trap_exit, false} -> ok
            end,
            Error
        end;
    Error ->
        Error
    end.


%%----------------------------------------------------------------------
%% Purpose: To append an Erlang term to the end of the file.
%% Args:    Erlang term to serialize and append to the file.
%% Returns: {ok, Pos} where Pos is the file offset to the beginning the
%%  serialized  term. Use pread_term to read the term back.
%%  or {error, Reason}.
%%----------------------------------------------------------------------

append_term(Fd, Term) ->
    append_binary(Fd, term_to_binary(Term)).
    
append_term_md5(Fd, Term) ->
    append_binary_md5(Fd, term_to_binary(Term)).


%%----------------------------------------------------------------------
%% Purpose: To append an Erlang binary to the end of the file.
%% Args:    Erlang term to serialize and append to the file.
%% Returns: {ok, Pos} where Pos is the file offset to the beginning the
%%  serialized  term. Use pread_term to read the term back.
%%  or {error, Reason}.
%%----------------------------------------------------------------------

append_binary(Fd, Bin) ->
    Size = iolist_size(Bin),
    gen_server:call(Fd, {append_bin,
            [<<0:1/integer,Size:31/integer>>, Bin]}, infinity).
    
append_binary_md5(Fd, Bin) ->
    Size = iolist_size(Bin),
    gen_server:call(Fd, {append_bin,
            [<<1:1/integer,Size:31/integer>>, couch_util:md5(Bin), Bin]}, infinity).


%%----------------------------------------------------------------------
%% Purpose: Reads a term from a file that was written with append_term
%% Args:    Pos, the offset into the file where the term is serialized.
%% Returns: {ok, Term}
%%  or {error, Reason}.
%%----------------------------------------------------------------------


pread_term(Fd, Pos) ->
    {ok, Bin} = pread_binary(Fd, Pos),
    {ok, binary_to_term(Bin)}.


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
    case gen_server:call(Fd, {pread_iolist, Pos}, infinity) of
    {ok, IoList, <<>>} ->
        {ok, IoList};
    {ok, IoList, Md5} ->
        case couch_util:md5(IoList) of
        Md5 ->
            {ok, IoList};
        _ ->
            exit({file_corruption, <<"file corruption">>})
        end;
    Error ->
        Error
    end.

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
    {ok, Fd} = file:open(Filepath, [append, raw]),
    try file:sync(Fd) after file:close(Fd) end;
sync(Fd) ->
    gen_server:call(Fd, sync, infinity).

%%----------------------------------------------------------------------
%% Purpose: Close the file.
%% Returns: ok
%%----------------------------------------------------------------------
close(Fd) ->
    MRef = erlang:monitor(process, Fd),
    try
        catch unlink(Fd),
        catch exit(Fd, shutdown),
        receive
        {'DOWN', MRef, _, _, _} ->
            ok
        end
    after
        erlang:demonitor(MRef, [flush])
    end.


delete(RootDir, Filepath) ->
    delete(RootDir, Filepath, true).


delete(RootDir, Filepath, Async) ->
    DelFile = filename:join([RootDir,".delete", ?b2l(couch_uuids:random())]),
    case file:rename(Filepath, DelFile) of
    ok ->
        if (Async) ->
            spawn(file, delete, [DelFile]),
            ok;
        true ->
            file:delete(DelFile)
        end;
    Error ->
        Error
    end.


init_delete_dir(RootDir) ->
    Dir = filename:join(RootDir,".delete"),
    % note: ensure_dir requires an actual filename companent, which is the
    % reason for "foo".
    filelib:ensure_dir(filename:join(Dir,"foo")),
    filelib:fold_files(Dir, ".*", true,
        fun(Filename, _) ->
            ok = file:delete(Filename)
        end, ok).


% 09 UPGRADE CODE
old_pread(Fd, Pos, Len) ->
    {ok, <<RawBin:Len/binary>>, false} = gen_server:call(Fd, {pread, Pos, Len}, infinity),
    {ok, RawBin}.

% 09 UPGRADE CODE
upgrade_old_header(Fd, Sig) ->
    gen_server:call(Fd, {upgrade_old_header, Sig}, infinity).


read_header(Fd) ->
    case gen_server:call(Fd, find_header, infinity) of
    {ok, Bin} ->
        {ok, binary_to_term(Bin)};
    Else ->
        Else
    end.

write_header(Fd, Data) ->
    Bin = term_to_binary(Data),
    Md5 = couch_util:md5(Bin),
    % now we assemble the final header binary and write to disk
    FinalBin = <<Md5/binary, Bin/binary>>,
    gen_server:call(Fd, {write_header, FinalBin}, infinity).




init_status_error(ReturnPid, Ref, Error) ->
    ReturnPid ! {Ref, self(), Error},
    ignore.

% server functions

init({Filepath, Options, ReturnPid, Ref}) ->
    process_flag(trap_exit, true),
    case lists:member(create, Options) of
    true ->
        filelib:ensure_dir(Filepath),
        case file:open(Filepath, [read, append, raw, binary]) of
        {ok, Fd} ->
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
                    ok = file:sync(Fd),
                    maybe_track_open_os_files(Options),
                    {ok, #file{fd=Fd}};
                false ->
                    ok = file:close(Fd),
                    init_status_error(ReturnPid, Ref, file_exists)
                end;
            false ->
                maybe_track_open_os_files(Options),
                {ok, #file{fd=Fd}}
            end;
        Error ->
            init_status_error(ReturnPid, Ref, Error)
        end;
    false ->
        % open in read mode first, so we don't create the file if it doesn't exist.
        case file:open(Filepath, [read, raw]) of
        {ok, Fd_Read} ->
            {ok, Fd} = file:open(Filepath, [read, append, raw, binary]),
            ok = file:close(Fd_Read),
            maybe_track_open_os_files(Options),
            {ok, Length} = file:position(Fd, eof),
            {ok, #file{fd=Fd, eof=Length}};
        Error ->
            init_status_error(ReturnPid, Ref, Error)
        end
    end.

maybe_track_open_os_files(FileOptions) ->
    case lists:member(sys_db, FileOptions) of
    true ->
        ok;
    false ->
        couch_stats_collector:track_process_count({couchdb, open_os_files})
    end.

terminate(_Reason, _Fd) ->
    ok.


handle_call({pread_iolist, Pos}, _From, File) ->
    {LenIolist, NextPos} = read_raw_iolist_int(File, Pos, 4),
    case iolist_to_binary(LenIolist) of
    <<1:1/integer,Len:31/integer>> -> % an MD5-prefixed term
        {Md5AndIoList, _} = read_raw_iolist_int(File, NextPos, Len+16),
        {Md5, IoList} = extract_md5(Md5AndIoList),
        {reply, {ok, IoList, Md5}, File};
    <<0:1/integer,Len:31/integer>> ->
        {Iolist, _} = read_raw_iolist_int(File, NextPos, Len),
        {reply, {ok, Iolist, <<>>}, File}
    end;
handle_call({pread, Pos, Bytes}, _From, #file{fd=Fd,tail_append_begin=TailAppendBegin}=File) ->
    {ok, Bin} = file:pread(Fd, Pos, Bytes),
    {reply, {ok, Bin, Pos >= TailAppendBegin}, File};
handle_call(bytes, _From, #file{eof=Length}=File) ->
    {reply, {ok, Length}, File};
handle_call(sync, _From, #file{fd=Fd}=File) ->
    {reply, file:sync(Fd), File};
handle_call({truncate, Pos}, _From, #file{fd=Fd}=File) ->
    {ok, Pos} = file:position(Fd, Pos),
    case file:truncate(Fd) of
    ok ->
        {reply, ok, File#file{eof=Pos}};
    Error ->
        {reply, Error, File}
    end;
handle_call({append_bin, Bin}, _From, #file{fd=Fd, eof=Pos}=File) ->
    Blocks = make_blocks(Pos rem ?SIZE_BLOCK, Bin),
    case file:write(Fd, Blocks) of
    ok ->
        {reply, {ok, Pos}, File#file{eof=Pos+iolist_size(Blocks)}};
    Error ->
        {reply, Error, File}
    end;
handle_call({write_header, Bin}, _From, #file{fd=Fd, eof=Pos}=File) ->
    BinSize = size(Bin),
    case Pos rem ?SIZE_BLOCK of
    0 ->
        Padding = <<>>;
    BlockOffset ->
        Padding = <<0:(8*(?SIZE_BLOCK-BlockOffset))>>
    end,
    FinalBin = [Padding, <<1, BinSize:32/integer>> | make_blocks(5, [Bin])],
    case file:write(Fd, FinalBin) of
    ok ->
        {reply, ok, File#file{eof=Pos+iolist_size(FinalBin)}};
    Error ->
        {reply, Error, File}
    end;


handle_call({upgrade_old_header, Prefix}, _From, #file{fd=Fd}=File) ->
    case (catch read_old_header(Fd, Prefix)) of
    {ok, Header} ->
        TailAppendBegin = File#file.eof,
        Bin = term_to_binary(Header),
        Md5 = couch_util:md5(Bin),
        % now we assemble the final header binary and write to disk
        FinalBin = <<Md5/binary, Bin/binary>>,
        {reply, ok, _} = handle_call({write_header, FinalBin}, ok, File),
        ok = write_old_header(Fd, <<"upgraded">>, TailAppendBegin),
        {reply, ok, File#file{tail_append_begin=TailAppendBegin}};
    _Error ->
        case (catch read_old_header(Fd, <<"upgraded">>)) of
        {ok, TailAppendBegin} ->
            {reply, ok, File#file{tail_append_begin = TailAppendBegin}};
        _Error2 ->
            {reply, ok, File}
        end
    end;


handle_call(find_header, _From, #file{fd=Fd, eof=Pos}=File) ->
    {reply, find_header(Fd, Pos div ?SIZE_BLOCK), File}.

% 09 UPGRADE CODE
-define(HEADER_SIZE, 2048). % size of each segment of the doubly written header

% 09 UPGRADE CODE
read_old_header(Fd, Prefix) ->
    {ok, Bin} = file:pread(Fd, 0, 2*(?HEADER_SIZE)),
    <<Bin1:(?HEADER_SIZE)/binary, Bin2:(?HEADER_SIZE)/binary>> = Bin,
    Result =
    % read the first header
    case extract_header(Prefix, Bin1) of
    {ok, Header1} ->
        case extract_header(Prefix, Bin2) of
        {ok, Header2} ->
            case Header1 == Header2 of
            true ->
                % Everything is completely normal!
                {ok, Header1};
            false ->
                % To get here we must have two different header versions with signatures intact.
                % It's weird but possible (a commit failure right at the 2k boundary). Log it and take the first.
                ?LOG_INFO("Header version differences.~nPrimary Header: ~p~nSecondary Header: ~p", [Header1, Header2]),
                {ok, Header1}
            end;
        Error ->
            % error reading second header. It's ok, but log it.
            ?LOG_INFO("Secondary header corruption (error: ~p). Using primary header.", [Error]),
            {ok, Header1}
        end;
    Error ->
        % error reading primary header
        case extract_header(Prefix, Bin2) of
        {ok, Header2} ->
            % log corrupt primary header. It's ok since the secondary is still good.
            ?LOG_INFO("Primary header corruption (error: ~p). Using secondary header.", [Error]),
            {ok, Header2};
        _ ->
            % error reading secondary header too
            % return the error, no need to log anything as the caller will be responsible for dealing with the error.
            Error
        end
    end,
    case Result of
    {ok, {pointer_to_header_data, Ptr}} ->
        pread_term(Fd, Ptr);
    _ ->
        Result
    end.

% 09 UPGRADE CODE
extract_header(Prefix, Bin) ->
    SizeOfPrefix = size(Prefix),
    SizeOfTermBin = ?HEADER_SIZE -
                    SizeOfPrefix -
                    16,     % md5 sig

    <<HeaderPrefix:SizeOfPrefix/binary, TermBin:SizeOfTermBin/binary, Sig:16/binary>> = Bin,

    % check the header prefix
    case HeaderPrefix of
    Prefix ->
        % check the integrity signature
        case couch_util:md5(TermBin) == Sig of
        true ->
            Header = binary_to_term(TermBin),
            {ok, Header};
        false ->
            header_corrupt
        end;
    _ ->
        unknown_header_type
    end.


% 09 UPGRADE CODE
write_old_header(Fd, Prefix, Data) ->
    TermBin = term_to_binary(Data),
    % the size of all the bytes written to the header, including the md5 signature (16 bytes)
    FilledSize = byte_size(Prefix) + byte_size(TermBin) + 16,
    {TermBin2, FilledSize2} =
    case FilledSize > ?HEADER_SIZE of
    true ->
        % too big!
        {ok, Pos} = append_binary(Fd, TermBin),
        PtrBin = term_to_binary({pointer_to_header_data, Pos}),
        {PtrBin, byte_size(Prefix) + byte_size(PtrBin) + 16};
    false ->
        {TermBin, FilledSize}
    end,
    ok = file:sync(Fd),
    % pad out the header with zeros, then take the md5 hash
    PadZeros = <<0:(8*(?HEADER_SIZE - FilledSize2))>>,
    Sig = couch_util:md5([TermBin2, PadZeros]),
    % now we assemble the final header binary and write to disk
    WriteBin = <<Prefix/binary, TermBin2/binary, PadZeros/binary, Sig/binary>>,
    ?HEADER_SIZE = size(WriteBin), % sanity check
    DblWriteBin = [WriteBin, WriteBin],
    ok = file:pwrite(Fd, 0, DblWriteBin),
    ok = file:sync(Fd).


handle_cast(close, Fd) ->
    {stop,normal,Fd}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({'EXIT', _, normal}, Fd) ->
    {noreply, Fd};
handle_info({'EXIT', _, Reason}, Fd) ->
    {stop, Reason, Fd}.


find_header(_Fd, -1) ->
    no_valid_header;
find_header(Fd, Block) ->
    case (catch load_header(Fd, Block)) of
    {ok, Bin} ->
        {ok, Bin};
    _Error ->
        find_header(Fd, Block -1)
    end.

load_header(Fd, Block) ->
    {ok, <<1>>} = file:pread(Fd, Block*?SIZE_BLOCK, 1),
    {ok, <<HeaderLen:32/integer>>} = file:pread(Fd, (Block*?SIZE_BLOCK) + 1, 4),
    TotalBytes = calculate_total_read_len(1, HeaderLen),
    {ok, <<RawBin:TotalBytes/binary>>} =
            file:pread(Fd, (Block*?SIZE_BLOCK) + 5, TotalBytes),
    <<Md5Sig:16/binary, HeaderBin/binary>> =
        iolist_to_binary(remove_block_prefixes(1, RawBin)),
    Md5Sig = couch_util:md5(HeaderBin),
    {ok, HeaderBin}.

-spec read_raw_iolist_int(#file{}, Pos::non_neg_integer(), Len::non_neg_integer()) ->
    {Data::iolist(), CurPos::non_neg_integer()}.
read_raw_iolist_int(#file{fd=Fd, tail_append_begin=TAB}, Pos, Len) ->
    BlockOffset = Pos rem ?SIZE_BLOCK,
    TotalBytes = calculate_total_read_len(BlockOffset, Len),
    {ok, <<RawBin:TotalBytes/binary>>} = file:pread(Fd, Pos, TotalBytes),
    if Pos >= TAB ->
        {remove_block_prefixes(BlockOffset, RawBin), Pos + TotalBytes};
    true ->
        % 09 UPGRADE CODE
        <<ReturnBin:Len/binary, _/binary>> = RawBin,
        {[ReturnBin], Pos + Len}
    end.

-spec extract_md5(iolist()) -> {binary(), iolist()}.
extract_md5(FullIoList) ->
    {Md5List, IoList} = split_iolist(FullIoList, 16, []),
    {iolist_to_binary(Md5List), IoList}.

calculate_total_read_len(0, FinalLen) ->
    calculate_total_read_len(1, FinalLen) + 1;
calculate_total_read_len(BlockOffset, FinalLen) ->
    case ?SIZE_BLOCK - BlockOffset of
    BlockLeft when BlockLeft >= FinalLen ->
        FinalLen;
    BlockLeft ->
        FinalLen + ((FinalLen - BlockLeft) div (?SIZE_BLOCK -1)) +
            if ((FinalLen - BlockLeft) rem (?SIZE_BLOCK -1)) =:= 0 -> 0;
                true -> 1 end
    end.

remove_block_prefixes(_BlockOffset, <<>>) ->
    [];
remove_block_prefixes(0, <<_BlockPrefix,Rest/binary>>) ->
    remove_block_prefixes(1, Rest);
remove_block_prefixes(BlockOffset, Bin) ->
    BlockBytesAvailable = ?SIZE_BLOCK - BlockOffset,
    case size(Bin) of
    Size when Size > BlockBytesAvailable ->
        <<DataBlock:BlockBytesAvailable/binary,Rest/binary>> = Bin,
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
-spec split_iolist(IoList::iolist(), SplitAt::non_neg_integer(), Acc::list()) ->
    {iolist(), iolist()} | non_neg_integer().
split_iolist(List, 0, BeginAcc) ->
    {lists:reverse(BeginAcc), List};
split_iolist([], SplitAt, _BeginAcc) ->
    SplitAt;
split_iolist([<<Bin/binary>> | Rest], SplitAt, BeginAcc) when SplitAt > byte_size(Bin) ->
    split_iolist(Rest, SplitAt - byte_size(Bin), [Bin | BeginAcc]);
split_iolist([<<Bin/binary>> | Rest], SplitAt, BeginAcc) ->
    <<Begin:SplitAt/binary,End/binary>> = Bin,
    split_iolist([End | Rest], 0, [Begin | BeginAcc]);
split_iolist([Sublist| Rest], SplitAt, BeginAcc) when is_list(Sublist) ->
    case split_iolist(Sublist, SplitAt, BeginAcc) of
    {Begin, End} ->
        {Begin, [End | Rest]};
    SplitRemaining ->
        split_iolist(Rest, SplitAt - (SplitAt - SplitRemaining), [Sublist | BeginAcc])
    end;
split_iolist([Byte | Rest], SplitAt, BeginAcc) when is_integer(Byte) ->
    split_iolist(Rest, SplitAt - 1, [Byte | BeginAcc]).
