-module(exxhash_tests).

-include_lib("eunit/include/eunit.hrl").

empty_test() ->
    % Empty test vector taken from:
    % https://github.com/Cyan4973/xxHash/blob/dev/cli/xsum_sanity_check.c
    %   H64 = 0x99AA06D3014798D8
    %   L64 = 0x6001C324468D497F
    Empty = "99AA06D3014798D86001C324468D497F",
    ?assertEqual(from_hex(Empty), hash128(<<>>)).

zeros_1_test() ->
    Zeros_1 = "A6CD5E9392000F6AC44BDFF4074EECDB",
    ?assertEqual(from_hex(Zeros_1), hash128(<<0>>)).

zeros_1k_test() ->
    Zeros_1k = "0717191E67688313DE5F15AB6DAF7941",
    ?assertEqual(from_hex(Zeros_1k), hash128(<<0:1024/unit:8>>)).

zeros_10MB_test() ->
    Zeros_10MB = "C8A31A1122549FD37F74F91CF4508E33",
    Len = 1024 * 1024 * 10,
    ?assertEqual(from_hex(Zeros_10MB), hash128(<<0:Len/unit:8>>)).

% These are mainly to check the NIF can handle multiple calls
% without segfaulting and accept random data basides the specific
% test values above
%
small_random_test() ->
    lists:foreach(
        fun(I) ->
            hash128(crypto:strong_rand_bytes(I))
        end,
        lists:seq(1, 10000)
    ).

large_random_test() ->
    % Larger than 1MB to be dispatched to the dirty CPU schedulers
    lists:foreach(
        fun(I) ->
            hash128(crypto:strong_rand_bytes(1024 * 1024 * I))
        end,
        lists:seq(2, 10)
    ).

hash128(Bin) ->
    Res = exxhash:xxhash128(Bin),
    ?assert(is_binary(Res)),
    ?assertEqual(16, byte_size(Res)),
    Res.

from_hex(List) when is_list(List) ->
    binary:decode_hex(list_to_binary(List)).
