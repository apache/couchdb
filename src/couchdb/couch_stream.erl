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

-module(couch_stream).
-behaviour(gen_server).

% public API
-export([open/1, open/3, close/1]).
-export([foldl/4, foldl/5, foldl_decode/6, range_foldl/6]).
-export([copy_to_new_stream/3, write/2]).

% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_cast/2, handle_call/3, handle_info/2]).

-include("couch_db.hrl").

-record(stream,
    {fd = 0,
    written_pointers=[],
    buffer_list = [],
    buffer_len = 0,
    max_buffer = 4096,
    written_len = 0,
    md5,
    % md5 of the content without any transformation applied (e.g. compression)
    % needed for the attachment upload integrity check (ticket 558)
    identity_md5,
    identity_len = 0,
    encoding_fun,
    end_encoding_fun
    }).


%%% Interface functions %%%

open(Fd) ->
    open(Fd, identity, []).

open(Fd, Encoding, Options) ->
    gen_server:start_link(couch_stream, {Fd, Encoding, Options}, []).

close(Pid) ->
    gen_server:call(Pid, close, infinity).

copy_to_new_stream(Fd, PosList, DestFd) ->
    {ok, Dest} = open(DestFd),
    foldl(Fd, PosList,
        fun(Bin, _) ->
            ok = write(Dest, Bin)
        end, ok),
    close(Dest).

foldl(_Fd, [], _Fun, Acc) ->
    Acc;
foldl(Fd, [Pos|Rest], Fun, Acc) ->
    {ok, Bin} = couch_file:pread_iolist(Fd, Pos),
    foldl(Fd, Rest, Fun, Fun(Bin, Acc)).

foldl(Fd, PosList, <<>>, Fun, Acc) ->
    foldl(Fd, PosList, Fun, Acc);
foldl(Fd, PosList, Md5, Fun, Acc) ->
    foldl(Fd, PosList, Md5, couch_util:md5_init(), Fun, Acc).

foldl_decode(Fd, PosList, Md5, Enc, Fun, Acc) ->
    {DecDataFun, DecEndFun} = case Enc of
    gzip ->
        ungzip_init();
    identity ->
        identity_enc_dec_funs()
    end,
    Result = foldl_decode(
        DecDataFun, Fd, PosList, Md5, couch_util:md5_init(), Fun, Acc
    ),
    DecEndFun(),
    Result.

foldl(_Fd, [], Md5, Md5Acc, _Fun, Acc) ->
    Md5 = couch_util:md5_final(Md5Acc),
    Acc;
foldl(Fd, [{Pos, _Size}], Md5, Md5Acc, Fun, Acc) -> % 0110 UPGRADE CODE
    foldl(Fd, [Pos], Md5, Md5Acc, Fun, Acc);
foldl(Fd, [Pos], Md5, Md5Acc, Fun, Acc) ->
    {ok, Bin} = couch_file:pread_iolist(Fd, Pos),
    Md5 = couch_util:md5_final(couch_util:md5_update(Md5Acc, Bin)),
    Fun(Bin, Acc);
foldl(Fd, [{Pos, _Size}|Rest], Md5, Md5Acc, Fun, Acc) ->
    foldl(Fd, [Pos|Rest], Md5, Md5Acc, Fun, Acc);
foldl(Fd, [Pos|Rest], Md5, Md5Acc, Fun, Acc) ->
    {ok, Bin} = couch_file:pread_iolist(Fd, Pos),
    foldl(Fd, Rest, Md5, couch_util:md5_update(Md5Acc, Bin), Fun, Fun(Bin, Acc)).

range_foldl(Fd, PosList, From, To, Fun, Acc) ->
    range_foldl(Fd, PosList, From, To, 0, Fun, Acc).

range_foldl(_Fd, _PosList, _From, To, Off, _Fun, Acc) when Off >= To ->
    Acc;
range_foldl(Fd, [Pos|Rest], From, To, Off, Fun, Acc) when is_integer(Pos) -> % old-style attachment
    {ok, Bin} = couch_file:pread_iolist(Fd, Pos),
    range_foldl(Fd, [{Pos, iolist_size(Bin)}] ++ Rest, From, To, Off, Fun, Acc);
range_foldl(Fd, [{_Pos, Size}|Rest], From, To, Off, Fun, Acc) when From > Off + Size ->
    range_foldl(Fd, Rest, From, To, Off + Size, Fun, Acc);
range_foldl(Fd, [{Pos, Size}|Rest], From, To, Off, Fun, Acc) ->
    {ok, Bin} = couch_file:pread_iolist(Fd, Pos),
    Bin1 = if
        From =< Off andalso To >= Off + Size -> Bin; %% the whole block is covered
        true ->
            PrefixLen = clip(From - Off, 0, Size),
            PostfixLen = clip(Off + Size - To, 0, Size),
            MatchLen = Size - PrefixLen - PostfixLen,
            <<_Prefix:PrefixLen/binary,Match:MatchLen/binary,_Postfix:PostfixLen/binary>> = iolist_to_binary(Bin),
            Match
    end,
    range_foldl(Fd, Rest, From, To, Off + Size, Fun, Fun(Bin1, Acc)).

clip(Value, Lo, Hi) ->
    if
        Value < Lo -> Lo;
        Value > Hi -> Hi;
        true -> Value
    end.

foldl_decode(_DecFun, _Fd, [], Md5, Md5Acc, _Fun, Acc) ->
    Md5 = couch_util:md5_final(Md5Acc),
    Acc;
foldl_decode(DecFun, Fd, [{Pos, _Size}], Md5, Md5Acc, Fun, Acc) ->
    foldl_decode(DecFun, Fd, [Pos], Md5, Md5Acc, Fun, Acc);
foldl_decode(DecFun, Fd, [Pos], Md5, Md5Acc, Fun, Acc) ->
    {ok, EncBin} = couch_file:pread_iolist(Fd, Pos),
    Md5 = couch_util:md5_final(couch_util:md5_update(Md5Acc, EncBin)),
    Bin = DecFun(EncBin),
    Fun(Bin, Acc);
foldl_decode(DecFun, Fd, [{Pos, _Size}|Rest], Md5, Md5Acc, Fun, Acc) ->
    foldl_decode(DecFun, Fd, [Pos|Rest], Md5, Md5Acc, Fun, Acc);
foldl_decode(DecFun, Fd, [Pos|Rest], Md5, Md5Acc, Fun, Acc) ->
    {ok, EncBin} = couch_file:pread_iolist(Fd, Pos),
    Bin = DecFun(EncBin),
    Md5Acc2 = couch_util:md5_update(Md5Acc, EncBin),
    foldl_decode(DecFun, Fd, Rest, Md5, Md5Acc2, Fun, Fun(Bin, Acc)).

gzip_init(Options) ->
    case couch_util:get_value(compression_level, Options, 0) of
    Lvl when Lvl >= 1 andalso Lvl =< 9 ->
        Z = zlib:open(),
        % 15 = ?MAX_WBITS (defined in the zlib module)
        % the 16 + ?MAX_WBITS formula was obtained by inspecting zlib:gzip/1
        ok = zlib:deflateInit(Z, Lvl, deflated, 16 + 15, 8, default),
        {
            fun(Data) ->
                zlib:deflate(Z, Data)
            end,
            fun() ->
                Last = zlib:deflate(Z, [], finish),
                ok = zlib:deflateEnd(Z),
                ok = zlib:close(Z),
                Last
            end
        };
    _ ->
        identity_enc_dec_funs()
    end.

ungzip_init() ->
    Z = zlib:open(),
    zlib:inflateInit(Z, 16 + 15),
    {
        fun(Data) ->
            zlib:inflate(Z, Data)
        end,
        fun() ->
            ok = zlib:inflateEnd(Z),
            ok = zlib:close(Z)
        end
    }.

identity_enc_dec_funs() ->
    {
        fun(Data) -> Data end,
        fun() -> [] end
    }.

write(_Pid, <<>>) ->
    ok;
write(Pid, Bin) ->
    gen_server:call(Pid, {write, Bin}, infinity).


init({Fd, Encoding, Options}) ->
    {EncodingFun, EndEncodingFun} = case Encoding of
    identity ->
        identity_enc_dec_funs();
    gzip ->
        gzip_init(Options)
    end,
    {ok, #stream{
            fd=Fd,
            md5=couch_util:md5_init(),
            identity_md5=couch_util:md5_init(),
            encoding_fun=EncodingFun,
            end_encoding_fun=EndEncodingFun
        }
    }.

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
        max_buffer = Max,
        md5 = Md5,
        identity_md5 = IdenMd5,
        identity_len = IdenLen,
        encoding_fun = EncodingFun} = Stream,
    if BinSize + BufferLen > Max ->
        WriteBin = lists:reverse(Buffer, [Bin]),
        IdenMd5_2 = couch_util:md5_update(IdenMd5, WriteBin),
        case EncodingFun(WriteBin) of
        [] ->
            % case where the encoder did some internal buffering
            % (zlib does it for example)
            WrittenLen2 = WrittenLen,
            Md5_2 = Md5,
            Written2 = Written;
        WriteBin2 ->
            {ok, Pos, _} = couch_file:append_binary(Fd, WriteBin2),
            WrittenLen2 = WrittenLen + iolist_size(WriteBin2),
            Md5_2 = couch_util:md5_update(Md5, WriteBin2),
            Written2 = [{Pos, iolist_size(WriteBin2)}|Written]
        end,

        {reply, ok, Stream#stream{
                        written_len=WrittenLen2,
                        written_pointers=Written2,
                        buffer_list=[],
                        buffer_len=0,
                        md5=Md5_2,
                        identity_md5=IdenMd5_2,
                        identity_len=IdenLen + BinSize}};
    true ->
        {reply, ok, Stream#stream{
                        buffer_list=[Bin|Buffer],
                        buffer_len=BufferLen + BinSize,
                        identity_len=IdenLen + BinSize}}
    end;
handle_call(close, _From, Stream) ->
    #stream{
        fd = Fd,
        written_len = WrittenLen,
        written_pointers = Written,
        buffer_list = Buffer,
        md5 = Md5,
        identity_md5 = IdenMd5,
        identity_len = IdenLen,
        encoding_fun = EncodingFun,
        end_encoding_fun = EndEncodingFun} = Stream,

    WriteBin = lists:reverse(Buffer),
    IdenMd5Final = couch_util:md5_final(couch_util:md5_update(IdenMd5, WriteBin)),
    WriteBin2 = EncodingFun(WriteBin) ++ EndEncodingFun(),
    Md5Final = couch_util:md5_final(couch_util:md5_update(Md5, WriteBin2)),
    Result = case WriteBin2 of
    [] ->
        {lists:reverse(Written), WrittenLen, IdenLen, Md5Final, IdenMd5Final};
    _ ->
        {ok, Pos, _} = couch_file:append_binary(Fd, WriteBin2),
        StreamInfo = lists:reverse(Written, [{Pos, iolist_size(WriteBin2)}]),
        StreamLen = WrittenLen + iolist_size(WriteBin2),
        {StreamInfo, StreamLen, IdenLen, Md5Final, IdenMd5Final}
    end,
    {stop, normal, Result, Stream}.

handle_cast(_Msg, State) ->
    {noreply,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.
