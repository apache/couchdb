%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2006 Mochi Media, Inc.

%% @doc Utilities for working with hexadecimal strings.

-module(mochihex).
-author('bob@mochimedia.com').

-export([test/0, to_hex/1, to_bin/1, to_int/1, dehex/1, hexdigit/1]).

%% @type iolist() = [char() | binary() | iolist()]
%% @type iodata() = iolist() | binary()

%% @spec to_hex(integer | iolist()) -> string()
%% @doc Convert an iolist to a hexadecimal string.
to_hex(0) ->
    "0";
to_hex(I) when is_integer(I), I > 0 ->
    to_hex_int(I, []);
to_hex(B) ->
    to_hex(iolist_to_binary(B), []).

%% @spec to_bin(string()) -> binary()
%% @doc Convert a hexadecimal string to a binary.
to_bin(L) ->
    to_bin(L, []).

%% @spec to_int(string()) -> integer()
%% @doc Convert a hexadecimal string to an integer.
to_int(L) ->
    erlang:list_to_integer(L, 16).

%% @spec dehex(char()) -> integer()
%% @doc Convert a hex digit to its integer value.
dehex(C) when C >= $0, C =< $9 ->
    C - $0;
dehex(C) when C >= $a, C =< $f ->
    C - $a + 10;
dehex(C) when C >= $A, C =< $F ->
    C - $A + 10.

%% @spec hexdigit(integer()) -> char()
%% @doc Convert an integer less than 16 to a hex digit.
hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.

%% @spec test() -> ok
%% @doc Test this module.
test() ->
    "ff000ff1" = to_hex([255, 0, 15, 241]),
    <<255, 0, 15, 241>> = to_bin("ff000ff1"),
    16#ff000ff1 = to_int("ff000ff1"),
    "ff000ff1" = to_hex(16#ff000ff1),
    ok.


%% Internal API

to_hex(<<>>, Acc) ->
    lists:reverse(Acc);
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdigit(C2), hexdigit(C1) | Acc]).

to_hex_int(0, Acc) ->
    Acc;
to_hex_int(I, Acc) ->
    to_hex_int(I bsr 4, [hexdigit(I band 15) | Acc]).

to_bin([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
to_bin([C1, C2 | Rest], Acc) ->
    to_bin(Rest, [(dehex(C1) bsl 4) bor dehex(C2) | Acc]).

