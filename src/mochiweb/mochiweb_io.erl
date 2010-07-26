%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Utilities for dealing with IO devices (open files).

-module(mochiweb_io).
-author('bob@mochimedia.com').

-export([iodevice_stream/3, iodevice_stream/2]).
-export([iodevice_foldl/4, iodevice_foldl/3]).
-export([iodevice_size/1]).
-define(READ_SIZE, 8192).

iodevice_foldl(F, Acc, IoDevice) ->
    iodevice_foldl(F, Acc, IoDevice, ?READ_SIZE).

iodevice_foldl(F, Acc, IoDevice, BufferSize) ->
    case file:read(IoDevice, BufferSize) of
        eof ->
            Acc;
        {ok, Data} ->
            iodevice_foldl(F, F(Data, Acc), IoDevice, BufferSize)
    end.

iodevice_stream(Callback, IoDevice) ->
    iodevice_stream(Callback, IoDevice, ?READ_SIZE).

iodevice_stream(Callback, IoDevice, BufferSize) ->
    F = fun (Data, ok) -> Callback(Data) end,
    ok = iodevice_foldl(F, ok, IoDevice, BufferSize).

iodevice_size(IoDevice) ->
    {ok, Size} = file:position(IoDevice, eof),
    {ok, 0} = file:position(IoDevice, bof),
    Size.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).



-endif.
