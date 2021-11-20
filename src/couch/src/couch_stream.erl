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
-vsn(1).

-export([
    open/1,
    open/2,
    close/1,

    copy/2,
    write/2,
    to_disk_term/1,

    foldl/3,
    foldl/4,
    foldl_decode/5,
    range_foldl/5
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-include_lib("couch/include/couch_db.hrl").

-define(DEFAULT_BUFFER_SIZE, 4096).

-record(stream, {
    engine,
    opener_monitor,
    written_pointers = [],
    buffer_list = [],
    buffer_len = 0,
    max_buffer,
    written_len = 0,
    md5,
    % md5 of the content without any transformation applied (e.g. compression)
    % needed for the attachment upload integrity check (ticket 558)
    identity_md5,
    identity_len = 0,
    encoding_fun,
    end_encoding_fun
}).

open({_StreamEngine, _StreamEngineState} = Engine) ->
    open(Engine, []).

open({_StreamEngine, _StreamEngineState} = Engine, Options) ->
    gen_server:start_link(?MODULE, {Engine, self(), erlang:get(io_priority), Options}, []).

close(Pid) ->
    gen_server:call(Pid, close, infinity).

copy(Src, Dst) ->
    foldl(
        Src,
        fun(Bin, _) ->
            ok = write(Dst, Bin)
        end,
        ok
    ).

write(_Pid, <<>>) ->
    ok;
write(Pid, Bin) ->
    gen_server:call(Pid, {write, Bin}, infinity).

to_disk_term({Engine, EngineState}) ->
    Engine:to_disk_term(EngineState).

foldl({Engine, EngineState}, Fun, Acc) ->
    Engine:foldl(EngineState, Fun, Acc).

foldl(Engine, <<>>, Fun, Acc) ->
    foldl(Engine, Fun, Acc);
foldl(Engine, Md5, UserFun, UserAcc) ->
    InitAcc = {couch_hash:md5_hash_init(), UserFun, UserAcc},
    {Md5Acc, _, OutAcc} = foldl(Engine, fun foldl_md5/2, InitAcc),
    Md5 = couch_hash:md5_hash_final(Md5Acc),
    OutAcc.

foldl_decode(Engine, Md5, Enc, UserFun, UserAcc1) ->
    {DecDataFun, DecEndFun} =
        case Enc of
            gzip -> ungzip_init();
            identity -> identity_enc_dec_funs()
        end,
    InitAcc = {DecDataFun, UserFun, UserAcc1},
    {_, _, UserAcc2} = foldl(Engine, Md5, fun foldl_decode/2, InitAcc),
    DecEndFun(),
    UserAcc2.

range_foldl(Engine, From, To, UserFun, UserAcc) when To >= From ->
    NewEngine = do_seek(Engine, From),
    InitAcc = {To - From, UserFun, UserAcc},
    try
        {_, _, UserAcc2} = foldl(NewEngine, fun foldl_length/2, InitAcc),
        UserAcc2
    catch
        throw:{finished, UserAcc3} ->
            UserAcc3
    end.

foldl_md5(Bin, {Md5Acc, UserFun, UserAcc}) ->
    NewMd5Acc = couch_hash:md5_hash_update(Md5Acc, Bin),
    {NewMd5Acc, UserFun, UserFun(Bin, UserAcc)}.

foldl_decode(EncBin, {DecFun, UserFun, UserAcc}) ->
    case DecFun(EncBin) of
        <<>> -> {DecFun, UserFun, UserAcc};
        Dec -> {DecFun, UserFun, UserFun(Dec, UserAcc)}
    end.

foldl_length(Bin, {Length, UserFun, UserAcc}) ->
    BinSize = size(Bin),
    case BinSize =< Length of
        true ->
            {Length - BinSize, UserFun, UserFun(Bin, UserAcc)};
        false ->
            <<Trunc:Length/binary, _/binary>> = Bin,
            throw({finished, UserFun(Trunc, UserAcc)})
    end.

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

init({Engine, OpenerPid, OpenerPriority, Options}) ->
    erlang:put(io_priority, OpenerPriority),
    {EncodingFun, EndEncodingFun} =
        case couch_util:get_value(encoding, Options, identity) of
            identity -> identity_enc_dec_funs();
            gzip -> gzip_init(Options)
        end,
    {ok, #stream{
        engine = Engine,
        opener_monitor = erlang:monitor(process, OpenerPid),
        md5 = couch_hash:md5_hash_init(),
        identity_md5 = couch_hash:md5_hash_init(),
        encoding_fun = EncodingFun,
        end_encoding_fun = EndEncodingFun,
        max_buffer = couch_util:get_value(
            buffer_size, Options, ?DEFAULT_BUFFER_SIZE
        )
    }}.

terminate(_Reason, _Stream) ->
    ok.

handle_call({write, Bin}, _From, Stream) ->
    BinSize = iolist_size(Bin),
    #stream{
        engine = Engine,
        written_len = WrittenLen,
        buffer_len = BufferLen,
        buffer_list = Buffer,
        max_buffer = Max,
        md5 = Md5,
        identity_md5 = IdenMd5,
        identity_len = IdenLen,
        encoding_fun = EncodingFun
    } = Stream,
    if
        BinSize + BufferLen > Max ->
            WriteBin = lists:reverse(Buffer, [Bin]),
            IdenMd5_2 = couch_hash:md5_hash_update(IdenMd5, WriteBin),
            case EncodingFun(WriteBin) of
                [] ->
                    % case where the encoder did some internal buffering
                    % (zlib does it for example)
                    NewEngine = Engine,
                    WrittenLen2 = WrittenLen,
                    Md5_2 = Md5;
                WriteBin2 ->
                    NewEngine = do_write(Engine, WriteBin2),
                    WrittenLen2 = WrittenLen + iolist_size(WriteBin2),
                    Md5_2 = couch_hash:md5_hash_update(Md5, WriteBin2)
            end,

            {reply, ok,
                Stream#stream{
                    engine = NewEngine,
                    written_len = WrittenLen2,
                    buffer_list = [],
                    buffer_len = 0,
                    md5 = Md5_2,
                    identity_md5 = IdenMd5_2,
                    identity_len = IdenLen + BinSize
                },
                hibernate};
        true ->
            {reply, ok, Stream#stream{
                buffer_list = [Bin | Buffer],
                buffer_len = BufferLen + BinSize,
                identity_len = IdenLen + BinSize
            }}
    end;
handle_call(close, _From, Stream) ->
    #stream{
        engine = Engine,
        opener_monitor = MonRef,
        written_len = WrittenLen,
        buffer_list = Buffer,
        md5 = Md5,
        identity_md5 = IdenMd5,
        identity_len = IdenLen,
        encoding_fun = EncodingFun,
        end_encoding_fun = EndEncodingFun
    } = Stream,

    WriteBin = lists:reverse(Buffer),
    IdenMd5Final = couch_hash:md5_hash_final(couch_hash:md5_hash_update(IdenMd5, WriteBin)),
    WriteBin2 = EncodingFun(WriteBin) ++ EndEncodingFun(),
    Md5Final = couch_hash:md5_hash_final(couch_hash:md5_hash_update(Md5, WriteBin2)),
    Result =
        case WriteBin2 of
            [] ->
                {do_finalize(Engine), WrittenLen, IdenLen, Md5Final, IdenMd5Final};
            _ ->
                NewEngine = do_write(Engine, WriteBin2),
                StreamLen = WrittenLen + iolist_size(WriteBin2),
                {do_finalize(NewEngine), StreamLen, IdenLen, Md5Final, IdenMd5Final}
        end,
    erlang:demonitor(MonRef),
    {stop, normal, Result, Stream}.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({'DOWN', Ref, _, _, _}, #stream{opener_monitor = Ref} = State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

do_seek({Engine, EngineState}, Offset) ->
    {ok, NewState} = Engine:seek(EngineState, Offset),
    {Engine, NewState}.

do_write({Engine, EngineState}, Data) ->
    {ok, NewState} = Engine:write(EngineState, Data),
    {Engine, NewState}.

do_finalize({Engine, EngineState}) ->
    {ok, NewState} = Engine:finalize(EngineState),
    {Engine, NewState}.
