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

-module(couch_stream).
-behaviour(gen_server).


-define(FILE_POINTER_BYTES, 8).
-define(FILE_POINTER_BITS, 8*(?FILE_POINTER_BYTES)).

-define(STREAM_OFFSET_BYTES, 4).
-define(STREAM_OFFSET_BITS, 8*(?STREAM_OFFSET_BYTES)).

-define(HUGE_CHUNK, 1000000000). % Huge chuck size when reading all in one go

-define(DEFAULT_STREAM_CHUNK, 16#00100000). % 1 meg chunks when streaming data

-export([test/0]).
-export([open/1, close/1, write/2, foldl/4, old_foldl/5,old_copy_to_new_stream/4]).
-export([copy_to_new_stream/3,old_read_term/2]).
-export([init/1, terminate/2, handle_call/3]).
-export([handle_cast/2,code_change/3,handle_info/2]).

-include("couch_db.hrl").

-record(stream,
    {fd = 0,
    written_pointers=[],
    buffer_list = [],
    buffer_len = 0,
    max_buffer = 4096,
    written_len = 0
    }).


%%% Interface functions %%%

open(Fd) ->
    gen_server:start_link(couch_stream, Fd, []).

close(Pid) ->
    gen_server:call(Pid, close, infinity).

copy_to_new_stream(Fd, PosList, DestFd) ->
    {ok, Dest} = open(DestFd),
    foldl(Fd, PosList,
        fun(Bin, _) ->
            ok = write(Dest, Bin)
        end, ok),
    close(Dest).


% 09 UPGRADE CODE
old_copy_to_new_stream(Fd, Pos, Len, DestFd) ->
    {ok, Dest} = open(DestFd),
    old_foldl(Fd, Pos, Len,
        fun(Bin, _) ->
            ok = write(Dest, Bin)
        end, ok),
    close(Dest).

% 09 UPGRADE CODE    
old_foldl(_Fd, null, 0, _Fun, Acc) ->
    Acc;
old_foldl(Fd, OldPointer, Len, Fun, Acc) when is_tuple(OldPointer)->
    old_stream_data(Fd, OldPointer, Len, ?DEFAULT_STREAM_CHUNK, Fun, Acc).

foldl(_Fd, [], _Fun, Acc) ->
    Acc;
foldl(Fd, [Pos|Rest], Fun, Acc) ->
    {ok, Bin} = couch_file:pread_iolist(Fd, Pos),
    foldl(Fd, Rest, Fun, Fun(Bin, Acc)).

write(_Pid, <<>>) ->
    ok;
write(Pid, Bin) ->
    gen_server:call(Pid, {write, Bin}, infinity).


init(Fd) ->
    {ok, #stream{fd = Fd}}.

terminate(_Reason, _Stream) ->
    ok.

handle_call({write, Bin}, _From, Stream) ->
    BinSize = iolist_size(Bin),
    #stream{
        fd = Fd,
        written_len = WrittenLen,
        written_pointers = Written,
        buffer_len = BufferLen,
        buffer_list = Buffer,
        max_buffer = Max} = Stream,
    if BinSize + BufferLen > Max ->
        {ok, Pos} = couch_file:append_binary(Fd, lists:reverse(Buffer, [Bin])),
        {reply, ok, Stream#stream{
                        written_len=WrittenLen + BufferLen + BinSize,
                        written_pointers=[Pos|Written],
                        buffer_list=[],
                        buffer_len=0}};
    true ->
        {reply, ok, Stream#stream{
                        buffer_list=[Bin|Buffer],
                        buffer_len=BufferLen + BinSize}}
    end;
handle_call(close, _From, Stream) ->
    #stream{
        fd = Fd,
        written_len = WrittenLen,
        written_pointers = Written,
        buffer_len = BufferLen,
        buffer_list = Buffer} = Stream,
    
    case Buffer of
    [] ->
        Result = {lists:reverse(Written), WrittenLen};
    _ ->
        {ok, Pos} = couch_file:append_binary(Fd, lists:reverse(Buffer)),
        Result = {lists:reverse(Written, [Pos]), WrittenLen + BufferLen}
    end,
    {stop, normal, Result, Stream}.

handle_cast(_Msg, State) ->
    {noreply,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.
    


% 09 UPGRADE CODE
old_read_term(Fd, Sp) ->
    {ok, <<TermLen:(?STREAM_OFFSET_BITS)>>, Sp2}
        = old_read(Fd, Sp, ?STREAM_OFFSET_BYTES),
    {ok, Bin, _Sp3} = old_read(Fd, Sp2, TermLen),
    {ok, binary_to_term(Bin)}.

old_read(Fd, Sp, Num) ->
    {ok, RevBin, Sp2} = old_stream_data(Fd, Sp, Num, ?HUGE_CHUNK, fun(Bin, Acc) -> [Bin | Acc] end, []),
    Bin = list_to_binary(lists:reverse(RevBin)),
    {ok, Bin, Sp2}.

% 09 UPGRADE CODE
old_stream_data(_Fd, Sp, 0, _MaxChunk, _Fun, Acc) ->
    {ok, Acc, Sp};
old_stream_data(Fd, {Pos, 0}, Num, MaxChunk, Fun, Acc) ->
    {ok, <<NextPos:(?FILE_POINTER_BITS), NextOffset:(?STREAM_OFFSET_BITS)>>}
        = couch_file:old_pread(Fd, Pos, ?FILE_POINTER_BYTES + ?STREAM_OFFSET_BYTES),
    Sp = {NextPos, NextOffset},
    % Check NextPos is past current Pos (this is always true in a stream)
    % Guards against potential infinite loops caused by corruption.
    case NextPos > Pos of
        true -> ok;
        false -> throw({error, stream_corruption})
    end,
    old_stream_data(Fd, Sp, Num, MaxChunk, Fun, Acc);
old_stream_data(Fd, {Pos, Offset}, Num, MaxChunk, Fun, Acc) ->
    ReadAmount = lists:min([MaxChunk, Num, Offset]),
    {ok, Bin} = couch_file:old_pread(Fd, Pos, ReadAmount),
    Sp = {Pos + ReadAmount, Offset - ReadAmount},
    old_stream_data(Fd, Sp, Num - ReadAmount, MaxChunk, Fun, Fun(Bin, Acc)).



%%% Tests %%%

read_all(Fd, PosList) ->
    iolist_to_binary(foldl(Fd, PosList,
        fun(Bin, Acc) ->
            [Bin, Acc]
        end, [])).


test() ->
    {ok, Fd} = couch_file:open("foo", [create,overwrite]),
    ok = couch_file:write_header(Fd, {howdy, howdy}),
    Bin = <<"damienkatz">>,
    {ok, Pos} = couch_file:append_binary(Fd, Bin),
    {ok, Bin} = couch_file:pread_binary(Fd, Pos),
    {ok, {howdy, howdy}} = couch_file:read_header(Fd),
    ok = couch_file:write_header(Fd, {foo, foo}),
    {ok, {foo, foo}} = couch_file:read_header(Fd),
    
    {ok, Stream} = open(Fd),
    ok = write(Stream, <<"food">>),
    ok = write(Stream, <<"foob">>),
    {PosList, 8} = close(Stream),
    <<"foodfoob">> = read_all(Fd, PosList),
    {ok, Stream2} = open(Fd),
    OneBits = <<1:(8*10)>>,
    ZeroBits = <<0:(8*10)>>,
    ok = write(Stream2, OneBits),
    ok = write(Stream2, ZeroBits),
    {PosList2, 20} = close(Stream2),
    AllBits = iolist_to_binary([OneBits,ZeroBits]),
    AllBits = read_all(Fd, PosList2),
    couch_file:close(Fd),
    PosList2.

