% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_file).
-behaviour(gen_server).

-include("couch_db.hrl").

-define(HEADER_SIZE, 2048). % size of each segment of the doubly written header

-export([open/1, open/2, close/1, pread/3, pwrite/3, expand/2, bytes/1, sync/1]).
-export([append_term/2, pread_term/2,write_header/3, read_header/2, truncate/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, code_change/3, handle_info/2]).

%%----------------------------------------------------------------------
%% Args:   Valid Options are [create] and [create,overwrite].
%%  Files are opened in read/write mode.
%% Returns: On success, {ok, Fd}
%%  or {error, Reason} if the file could not be opened.
%%----------------------------------------------------------------------

open(Filepath) ->
    open(Filepath, []).
    
open(Filepath, Options) ->
    case gen_server:start_link(couch_file, {Filepath, Options, self()}, []) of
    {ok, FdPid} ->
        % we got back an ok, but that doesn't really mean it was successful.
        % Instead the true status has been sent back to us as a message.
        % We do this because if the gen_server doesn't initialize properly,
        % it generates a crash report that will get logged. This avoids
        % that mess, because we don't want crash reports generated
        % every time a file cannot be found.
        receive
        {FdPid, ok} ->
            {ok, FdPid};
        {FdPid, Error} ->
            Error
        end;
    Error ->
        Error
    end.


%%----------------------------------------------------------------------
%% Args:    Pos is the offset from the beginning of the file, Bytes is
%%  is the number of bytes to read.
%% Returns: {ok, Binary} where Binary is a binary data from disk
%%  or {error, Reason}.
%%----------------------------------------------------------------------

pread(Fd, Pos, Bytes) when Bytes > 0 ->
    gen_server:call(Fd, {pread, Pos, Bytes}).


%%----------------------------------------------------------------------
%% Args:    Pos is the offset from the beginning of the file, Bin is
%%  is the binary to write
%% Returns: ok
%%  or {error, Reason}.
%%----------------------------------------------------------------------

pwrite(Fd, Pos, Bin) ->
    gen_server:call(Fd, {pwrite, Pos, Bin}).

%%----------------------------------------------------------------------
%% Purpose: To append a segment of zeros to the end of the file.
%% Args:    Bytes is the number of bytes to append to the file.
%% Returns: {ok, Pos} where Pos is the file offset to the beginning of
%%  the new segments.
%%  or {error, Reason}.
%%----------------------------------------------------------------------

expand(Fd, Bytes) when Bytes > 0 ->
    gen_server:call(Fd, {expand, Bytes}).


%%----------------------------------------------------------------------
%% Purpose: To append an Erlang term to the end of the file.
%% Args:    Erlang term to serialize and append to the file.
%% Returns: {ok, Pos} where Pos is the file offset to the beginning the
%%  serialized  term. Use pread_term to read the term back.
%%  or {error, Reason}.
%%----------------------------------------------------------------------

append_term(Fd, Term) ->
    append_binary(Fd, term_to_binary(Term, [compressed])).


%%----------------------------------------------------------------------
%% Purpose: To append an Erlang binary to the end of the file.
%% Args:    Erlang term to serialize and append to the file.
%% Returns: {ok, Pos} where Pos is the file offset to the beginning the
%%  serialized  term. Use pread_term to read the term back.
%%  or {error, Reason}.
%%----------------------------------------------------------------------
    
append_binary(Fd, Bin) ->
    gen_server:call(Fd, {append_bin, Bin}, infinity).


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
    gen_server:call(Fd, {pread_bin, Pos}, infinity).


%%----------------------------------------------------------------------
%% Purpose: The length of a file, in bytes.
%% Returns: {ok, Bytes}
%%  or {error, Reason}.
%%----------------------------------------------------------------------

% length in bytes
bytes(Fd) ->
    gen_server:call(Fd, bytes).

%%----------------------------------------------------------------------
%% Purpose: Truncate a file to the number of bytes.
%% Returns: ok
%%  or {error, Reason}.
%%----------------------------------------------------------------------

truncate(Fd, Pos) ->
    gen_server:call(Fd, {truncate, Pos}).

%%----------------------------------------------------------------------
%% Purpose: Ensure all bytes written to the file are flushed to disk.
%% Returns: ok
%%  or {error, Reason}.
%%----------------------------------------------------------------------

sync(Fd) ->
    gen_server:call(Fd, sync, infinity).

%%----------------------------------------------------------------------
%% Purpose: Close the file. Is performed asynchronously.
%% Returns: ok
%%----------------------------------------------------------------------
close(Fd) ->
    gen_server:cast(Fd, close).


write_header(Fd, Prefix, Data) ->
    TermBin = term_to_binary(Data),
    % the size of all the bytes written to the header, including the md5 signature (16 bytes)
    FilledSize = size(Prefix) + size(TermBin) + 16,
    {TermBin2, FilledSize2} =
    case FilledSize > ?HEADER_SIZE of
    true ->
        % too big!
        {ok, Pos} = append_binary(Fd, TermBin),
        PtrBin = term_to_binary({pointer_to_header_data, Pos}),
        {PtrBin, size(Prefix) + size(PtrBin) + 16};
    false ->
        {TermBin, FilledSize}
    end,
    ok = sync(Fd),
    % pad out the header with zeros, then take the md5 hash
    PadZeros = <<0:(8*(?HEADER_SIZE - FilledSize2))>>,
    Sig = erlang:md5([TermBin2, PadZeros]),
    % now we assemble the final header binary and write to disk
    WriteBin = <<Prefix/binary, TermBin2/binary, PadZeros/binary, Sig/binary>>,
    ?HEADER_SIZE = size(WriteBin), % sanity check
    DblWriteBin = [WriteBin, WriteBin],
    ok = pwrite(Fd, 0, DblWriteBin),
    ok = sync(Fd).


read_header(Fd, Prefix) ->
    {ok, Bin} = couch_file:pread(Fd, 0, 2*(?HEADER_SIZE)),
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
        {error, Error} ->
            % error reading second header. It's ok, but log it.
            ?LOG_INFO("Secondary header corruption (error: ~p). Using primary header.", [Error]),
            {ok, Header1}
        end;
    {error, Error} ->
        % error reading primary header
        case extract_header(Prefix, Bin2) of
        {ok, Header2} ->
            % log corrupt primary header. It's ok since the secondary is still good.
            ?LOG_INFO("Primary header corruption (error: ~p). Using secondary header.", [Error]),
            {ok, Header2};
        _ ->
            % error reading secondary header too
            % return the error, no need to log anything as the caller will be responsible for dealing with the error.
            {error, Error}
        end
    end,
    case Result of
    {ok, {pointer_to_header_data, Ptr}} ->
        pread_term(Fd, Ptr);
    _ ->
        Result
    end.
    
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
        case erlang:md5(TermBin) == Sig of
        true ->
            Header = binary_to_term(TermBin),
            {ok, Header};
        false ->
            {error, header_corrupt}
        end;
    _ ->
        {error, unknown_header_type}
    end.



init_status_ok(ReturnPid, Fd) ->
    ReturnPid ! {self(), ok}, % signal back ok
    {ok, Fd}.

init_status_error(ReturnPid, Error) ->
    ReturnPid ! {self(), Error}, % signal back error status
    self() ! self_close, % tell ourself to close async
    {ok, nil}.

% server functions

init({Filepath, Options, ReturnPid}) ->
    case lists:member(create, Options) of
    true ->
        filelib:ensure_dir(Filepath),
        case file:open(Filepath, [read, write, raw, binary]) of
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
                    init_status_ok(ReturnPid, Fd);
                false ->
                    ok = file:close(Fd),
                    init_status_error(ReturnPid, {error, file_exists})
                end;
            false ->
                init_status_ok(ReturnPid, Fd)
            end;
        Error ->
            init_status_error(ReturnPid, Error)
        end;
    false ->
        % open in read mode first, so we don't create the file if it doesn't exist.
        case file:open(Filepath, [read, raw]) of
        {ok, Fd_Read} ->
            {ok, Fd} = file:open(Filepath, [read, write, raw, binary]),
            ok = file:close(Fd_Read),
            init_status_ok(ReturnPid, Fd);
        Error ->
            init_status_error(ReturnPid, Error)
        end
    end.


terminate(_Reason, nil) ->
    ok;
terminate(_Reason, Fd) ->
    file:close(Fd),
    ok.


handle_call({pread, Pos, Bytes}, _From, Fd) ->
    {reply, file:pread(Fd, Pos, Bytes), Fd};
handle_call({pwrite, Pos, Bin}, _From, Fd) ->
    {reply, file:pwrite(Fd, Pos, Bin), Fd};
handle_call({expand, Num}, _From, Fd) ->
    {ok, Pos} = file:position(Fd, eof),
    {reply, {file:pwrite(Fd, Pos + Num - 1, <<0>>), Pos}, Fd};
handle_call(bytes, _From, Fd) ->
    {reply, file:position(Fd, eof), Fd};
handle_call(sync, _From, Fd) ->
    {reply, file:sync(Fd), Fd};
handle_call({truncate, Pos}, _From, Fd) ->
    {ok, Pos} = file:position(Fd, Pos),
    {reply, file:truncate(Fd), Fd};
handle_call({append_bin, Bin}, _From, Fd) ->
    Len = size(Bin),
    Bin2 = <<Len:32, Bin/binary>>,
    {ok, Pos} = file:position(Fd, eof),
    {reply, {file:pwrite(Fd, Pos, Bin2), Pos}, Fd};
handle_call({pread_bin, Pos}, _From, Fd) ->
    {ok, <<TermLen:32>>}
        = file:pread(Fd, Pos, 4),
    {ok, Bin} = file:pread(Fd, Pos + 4, TermLen),
    {reply, {ok, Bin}, Fd}.


handle_cast(close, Fd) ->
    {stop,normal,Fd}. % causes terminate to be called

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(self_close, State) ->
    {stop,normal,State};
handle_info(_Info, State) ->
    {noreply, State}.
