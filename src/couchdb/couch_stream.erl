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

-export([test/1]).
-export([open/1, open/2, close/1, read/3, read_term/2, write/2, write_term/2, get_state/1, foldl/5]).
-export([copy/4, copy_to_new_stream/4]).
-export([ensure_buffer/2, set_min_buffer/2]).
-export([init/1, terminate/2, handle_call/3]).
-export([handle_cast/2,code_change/3,handle_info/2]).

-include("couch_db.hrl").

-define(FILE_POINTER_BYTES, 8).
-define(FILE_POINTER_BITS, 8*(?FILE_POINTER_BYTES)).

-define(STREAM_OFFSET_BYTES, 4).
-define(STREAM_OFFSET_BITS, 8*(?STREAM_OFFSET_BYTES)).

-define(HUGE_CHUNK, 1000000000). % Huge chuck size when reading all in one go

-define(DEFAULT_STREAM_CHUNK, 16#00100000). % 1 meg chunks when streaming data


-record(write_stream,
    {fd = 0,
    current_pos = 0,
    bytes_remaining = 0,
    next_alloc = 0,
    min_alloc = 16#00010000
    }).

-record(stream,
    {
    pid,
    fd
    }).


%%% Interface functions %%%

open(Fd) ->
    open(nil, Fd).

open(nil, Fd) ->
    open({0,0}, Fd);
open(State, Fd) ->
    {ok, Pid} = gen_server:start_link(couch_stream, {State, Fd}, []),
    {ok, #stream{pid = Pid, fd = Fd}}.

close(#stream{pid = Pid, fd = _Fd}) ->
    gen_server:call(Pid, close).

get_state(#stream{pid = Pid, fd = _Fd}) ->
    gen_server:call(Pid, get_state).

ensure_buffer(#stream{pid = Pid, fd = _Fd}, Bytes) ->
    gen_server:call(Pid, {ensure_buffer, Bytes}).

set_min_buffer(#stream{pid = Pid, fd = _Fd}, Bytes) ->
    gen_server:call(Pid, {set_min_buffer, Bytes}).

read(#stream{pid = _Pid, fd = Fd}, Sp, Num) ->
    read(Fd, Sp, Num);
read(Fd, Sp, Num) ->
    {ok, RevBin, Sp2} = stream_data(Fd, Sp, Num, ?HUGE_CHUNK, fun(Bin, Acc) -> {ok, [Bin | Acc]} end, []),
    Bin = list_to_binary(lists:reverse(RevBin)),
    {ok, Bin, Sp2}.

copy_to_new_stream(Src, Sp, Len, DestFd) ->
    {ok, Dest} = open(DestFd),
    ok = set_min_buffer(Dest, 0),
    {ok, NewSp} = copy(Src, Sp, Len, Dest),
    close(Dest),
    {ok, NewSp}.

copy(#stream{pid = _Pid, fd = Fd}, Sp, Len, DestStream) ->
    copy(Fd, Sp, Len, DestStream);
copy(Fd, Sp, Len, DestStream) ->
    {ok, NewSp, _Sp2} = stream_data(Fd, Sp, Len, ?HUGE_CHUNK,
        fun(Bin, AccPointer) ->
            {ok, NewPointer} = write(DestStream, Bin),
            {ok, if AccPointer == null -> NewPointer; true -> AccPointer end}
        end,
        null),
    {ok, NewSp}.

foldl(#stream{pid = _Pid, fd = Fd}, Sp, Num, Fun, Acc) ->
    foldl(Fd, Sp, Num, Fun, Acc);
foldl(Fd, Sp, Num, Fun, Acc) ->
    {ok, _Acc, _Sp} = stream_data(Fd, Sp, Num, ?DEFAULT_STREAM_CHUNK, Fun, Acc).

read_term(#stream{pid = _Pid, fd = Fd}, Sp) ->
    read_term(Fd, Sp);
read_term(Fd, Sp) ->
    {ok, <<TermLen:(?STREAM_OFFSET_BITS)>>, Sp2}
        = read(Fd, Sp, ?STREAM_OFFSET_BYTES),
    {ok, Bin, _Sp3} = read(Fd, Sp2, TermLen),
    {ok, binary_to_term(Bin)}.

write_term(Stream, Term) ->
    Bin = term_to_binary(Term),
    Size = size(Bin),
    Bin2 = <<Size:(?STREAM_OFFSET_BITS), Bin/binary>>,
    write(Stream, Bin2).

write(#stream{}, <<>>) ->
    {ok, {0,0}};
write(#stream{pid = Pid}, Bin) when is_binary(Bin) ->
    gen_server:call(Pid, {write, Bin}).


init({{Pos, BytesRemaining}, Fd}) ->
    {ok, #write_stream
        {fd = Fd,
        current_pos = Pos,
        bytes_remaining = BytesRemaining
        }}.

terminate(_Reason, _Stream) ->
    ok.

handle_call(get_state, _From, Stream) ->
    #write_stream{current_pos = Pos, bytes_remaining = BytesRemaining} = Stream,
    {reply, {Pos, BytesRemaining}, Stream};
handle_call({set_min_buffer, MinBuffer}, _From, Stream) ->
    {reply, ok, Stream#write_stream{min_alloc = MinBuffer}};
handle_call({ensure_buffer, BufferSizeRequested}, _From, Stream) ->
    #write_stream{bytes_remaining = BytesRemainingInCurrentBuffer} = Stream,
    case BytesRemainingInCurrentBuffer < BufferSizeRequested of
        true ->  NextAlloc = BufferSizeRequested - BytesRemainingInCurrentBuffer;
        false -> NextAlloc = 0 % enough room in current segment
    end,
    {reply, ok, Stream#write_stream{next_alloc = NextAlloc}};
handle_call({write, Bin}, _From, Stream) ->
    % ensure init is called first so we can get a pointer to the begining of the binary
    {ok, Sp, Stream2} = write_data(Stream, Bin),
    {reply, {ok, Sp}, Stream2};
handle_call(close, _From, Stream) ->
    #write_stream{current_pos=Pos, bytes_remaining = BytesRemaining} = Stream,
    {stop, normal, {ok, {Pos, BytesRemaining}}, Stream}.

handle_cast(_Msg, State) ->
    {noreply,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%%% Internal function %%%

stream_data(_Fd, Sp, 0, _MaxChunk, _Fun, Acc) ->
    {ok, Acc, Sp};
stream_data(Fd, {Pos, 0}, Num, MaxChunk, Fun, Acc) ->
    {ok, <<NextPos:(?FILE_POINTER_BITS), NextOffset:(?STREAM_OFFSET_BITS)>>}
        = couch_file:pread(Fd, Pos, ?FILE_POINTER_BYTES + ?STREAM_OFFSET_BYTES),
    Sp = {NextPos, NextOffset},
    % Check NextPos is past current Pos (this is always true in a stream)
    % Guards against potential infinite loops caused by corruption.
    case NextPos > Pos of
        true -> ok;
        false -> throw({error, stream_corruption})
    end,
    stream_data(Fd, Sp, Num, MaxChunk, Fun, Acc);
stream_data(Fd, {Pos, Offset}, Num, MaxChunk, Fun, Acc) ->
    ReadAmount = lists:min([MaxChunk, Num, Offset]),
    {ok, Bin} = couch_file:pread(Fd, Pos, ReadAmount),
    Sp = {Pos + ReadAmount, Offset - ReadAmount},
    case Fun(Bin, Acc) of
    {ok, Acc2} ->
        stream_data(Fd, Sp, Num - ReadAmount, MaxChunk, Fun, Acc2);
    {stop, Acc2} ->
        {ok, Acc2, Sp}
    end.

write_data(Stream, <<>>) ->
    {ok, {0,0}, Stream};
write_data(#write_stream{bytes_remaining=0} = Stream, Bin) ->
    #write_stream {
        fd = Fd,
        current_pos = CurrentPos,
        next_alloc = NextAlloc,
        min_alloc = MinAlloc
        }= Stream,

    NewSize = lists:max([MinAlloc, NextAlloc, size(Bin)]),
    % no space in the current segment, must alloc a new segment
    {ok, NewPos} = couch_file:expand(Fd, NewSize + ?FILE_POINTER_BYTES + ?STREAM_OFFSET_BYTES),

    case CurrentPos of
    0 ->
        ok;
    _ ->
        ok = couch_file:pwrite(Fd, CurrentPos, <<NewPos:(?FILE_POINTER_BITS), NewSize:(?STREAM_OFFSET_BITS)>>)
    end,
    Stream2 = Stream#write_stream{
        current_pos=NewPos,
        bytes_remaining=NewSize,
        next_alloc=0},
    write_data(Stream2, Bin);
write_data(#write_stream{fd=Fd, current_pos=Pos, bytes_remaining=BytesRemaining} = Stream, Bin) ->
    BytesToWrite = lists:min([size(Bin), BytesRemaining]),
    {WriteBin, Rest} = split_binary(Bin, BytesToWrite),
    ok = couch_file:pwrite(Fd, Pos, WriteBin),
    Stream2 = Stream#write_stream{
        bytes_remaining=BytesRemaining - BytesToWrite,
        current_pos=Pos + BytesToWrite
        },
    {ok, _, Stream3} = write_data(Stream2, Rest),
    {ok, {Pos, BytesRemaining}, Stream3}.



%%% Tests %%%


test(Term) ->
    {ok, Fd} = couch_file:open("foo", [write]),
    {ok, Stream} = open({0,0}, Fd),
    {ok, Pos} = write_term(Stream, Term),
    {ok, Pos2} = write_term(Stream, {Term, Term}),
    close(Stream),
    couch_file:close(Fd),
    {ok, Fd2} = couch_file:open("foo", [read, write]),
    {ok, Stream2} = open({0,0}, Fd2),
    {ok, Term1} = read_term(Fd2, Pos),
    io:format("Term1: ~w ~n",[Term1]),
    {ok, Term2} = read_term(Fd2, Pos2),
    io:format("Term2: ~w ~n",[Term2]),
    {ok, PointerList} = deep_write_test(Stream2, Term, 1000, []),
    deep_read_test(Fd2, PointerList),
    close(Stream2),
    couch_file:close(Fd2).

deep_read_test(_Fd, []) ->
    ok;
deep_read_test(Fd, [Pointer | RestPointerList]) ->
    {ok, _Term} = read_term(Fd, Pointer),
    deep_read_test(Fd, RestPointerList).

deep_write_test(_Stream, _Term, 0, PointerList) ->
    {ok, PointerList};
deep_write_test(Stream, Term, N, PointerList) ->
    WriteList = lists:duplicate(random:uniform(N), Term),
    {ok, Pointer} = write_term(Stream, WriteList),
    deep_write_test(Stream, Term, N-1, [Pointer | PointerList]).
