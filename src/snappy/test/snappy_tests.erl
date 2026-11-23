%% Copyright 2011,  Filipe David Manana  <fdmanana@apache.org>
%% Web:  http://github.com/fdmanana/snappy-erlang-nif
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%  http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(snappy_tests).
-include_lib("eunit/include/eunit.hrl").

% Frames written by snappy 1.1.3, the version CouchDB bundled before 1.3.1, for
% the inputs from inputs/0, base64 encoded. Database files written by older
% releases are full of such frames, so they must keep decoding. If inputs/0
% changes these have to be regenerated with snappy 1.1.3.
-define(SNAPPY_1_1_3_FRAMES, [
    {empty, ~"AA=="},
    {byte, ~"AQBh"},
    {zeros, ~"6AcAAP4BAP4BAP4BAP4BAP4BAP4BAP4BAP4BAP4BAP4BAP4BAP4BAP4BAP4BAP4BAJoBAA=="},
    {offset_2, ~"6AcEYWL+AgD+AgD+AgD+AgD+AgD+AgD+AgD+AgD+AgD+AgD+AgD+AgD+AgD+AgD+AgCWAgA="},
    {offset_3, ~"hAcIYWJj/gMA/gMA/gMA/gMA/gMA/gMA/gMA/gMA/gMA/gMA/gMA/gMA/gMA7gMABQM="},
    {offset_7, <<
        "mggYYWJjZGVmZ/4HAP4HAP4HAP4HAP4HAP4HAP4HAP4HAP4HAP4HAP4HAP4HAP4HAP4HAP4HAP4H",
        "AEoHAA=="
    >>},
    {text, <<
        "hAdod29yZHMgdGhhdCBnbyB1bnNwb2tlbiwgZGVlMhgAEGRvbmUg/i0A/i0A/i0A/i0A/i0A/i0A",
        "/i0A/i0A/i0A/i0A/i0A/i0A/i0AWi0A"
    >>},
    {random, <<
        "gAT0/wHxRQMGUXYkpX6vu/jtmVmF8RF30uxj2ZX7SsYo4NeC31hKFakPL5WdBwNZStRHrpPqSVnr",
        "ZKHwm+WA2VCWTzhDuz56z52EdLVnKyqm8nv2RLJQMMF2YUBh+yu/2BlK8uBnnhkjSyle5DKjkgww",
        "/aasxHswCu5OcSWA+eyL4JBxhj0+AgVGuVT+k2LbeBq86FHPuLszlc7587NYbCUW6WamprbKVIQx",
        "4SAuQkjM8o/UrW/MYdj5nzRJbYCMHBBnbPDWdroG64puQbMmfRP5YwRLn2qoFNH9XB6AZVMdEZPH",
        "iS46pD3qBxLFNObP8r8E5cmkMAv7wC89Sh5H1Mr5H9KeViqPPuTlyUmB4iWeMlO+sUZ6S4ejBvgd",
        "ff9UG79DM3f5R8K2YB0zpjKHIAE/U6MIjpc4Ly5qH+Zg3e3oA5VdIgCdMJgKpAj2KhLg5+esvRcH",
        "ZJJO6cPP9tdM009dmG1rTdmEWyDAIFtjWrshNPXf57RSblmlNDA+9EF/dPklq5FZ3qBtWkBflQsm",
        "R4sxOLUZfeu1aoaJK6vzygbwqKChWhpvAM7aQcT5AQDa3WqlogylRmbY1nIjURl2pUmb0O1146mO",
        "yYGkx1e0G+Tyo2CdaDYQRxV9cYCPb56z5jBaKIkZl4DTLMAPN4OQcW21RClKxlqfFpAaOwwRrcWl",
        "KALffg=="
    >>},
    {json, <<
        "iREweyJfaWQiOiJkb2MtMAEBcDEiLCJ2YWx1ZSI6NywidGFncyI6WyJyZWQiLCJiARcIXX0KQjUA",
        "ADIZNQQxNKI2AAAzGTYEMjGiNgAANB02ADiiNgAANRk2BDM1ojYAADYZNgQ0MqI2AAA3HTYAOaI2",
        "AAA4GTYENTaiNgAAORk2BDYznjYABDEwGTYENzCiNgAuGwKiHAIAMV0cADiiHAIAMV0cADmiHAIA",
        "MV0cADmiHAIAMV0cBDEwoh0CADFdHQQxMaIeAgAxXR4EMTGiHwIAMV0fBDEyoiACADFdIAQxM6Ih",
        "AgAyXSEEMTSiIgIAMl0iBDE0oiMCADJdIwQxNaIkAgAyXSQEMTaiJQIAMl0lBDE2oiYCADIuJgIA",
        "N6ImAgAyLiYCADiiJgIAMi4mAgA4oiYCADIuJgIAOaImAgAyXSYEMjCiJgIAM10mBDIxoiYCADNd",
        "JgQyMaImAgAzXSYEMjKiJgIAM10mBDIzoiYCADNdJgQyM6ImAgAzXSYEMjSiJgIAM10mBDI1oiYC",
        "ADNdJgQyNaImAgAzXSYEMjaiJgIAMy4mAgA3oiYCADQuJgIAOGImAg=="
    >>},
    {rev_chain, <<
        "wA5waANtAAAAEPpa2ahVflqEzyPlLT0633dqbAAAAAENHTzmdcwhwfeCF89dalsZKa/eMh0APFlr",
        "4tRPpfVXcCsUT/3hAU42HQA4z/VC+ufgxCZ+RXQKEsmgMh0APKqgdFT6k+0tN7TF2p8vh/0yHQA8",
        "StyCJOtzaqEOFoOQrIzCUTIdADzj4TATt02HKVPioSJcK1zQMh0APK4iGbU3FjNsO7Jvil/2k8Ay",
        "HQA821cuWkrPHFmOKjwGwNYAVzIdADztnH1aUqisoePaez5ZkosqMh0APIlFf/NqTWkXE99ZC6Zz",
        "i4cyHQA8hZIZ8PiOnGU5N2rXEgK3UzIdADwHjXiWqkZipG0QwU8ZT6CJMh0APLv645Oz3w3zKQtc",
        "1UMXHkIyHQA8Gzz/efpu+1AWoADOK5tyGzIdADyjcM1wPQ0EWr6eQFJee63aMh0APCPffLGtd0LF",
        "XZ3/skxV/MIyHQA42P0tXXbjqf3R3+6C3HXfNpYBPNT0NPblfUh3evVsJ5acMJAyOgA8aR3MEWj5",
        "/9FcBDMXbnFLsTIdADwXu987wQkwKMeJXIs62amlMh0APB0T9MPYwLf7/G/E6jnpnUEyHQA86w7o",
        "7CqpLP4yueH/HGlEezIdADyiEbRwPKfWLsCTv0V7SQcpMh0APKTeQDgbJwcr6BA0g4Tm+FgyHQA8",
        "D+iWXQXDdwalv77IhintaDIdADyvNnB2NSRD7fvprNeVF1W8Mh0APKh60lnICTriAPDc3+w3uxYy",
        "HQA8jUlw4nP7UI6RRI6rBaCkhDIdADwqC7ICDYgi3W9DKh9/yMdJMh0APDbA4+vNj55D/NmwNqiv",
        "rioyHQA8tU1IpPdfWnnvB3IZioc+lTIdADxpuMo2Immqt8XgcpJQhmvRMh0APJq5Sy7KUO8P+wuG",
        "F5vnBagyHQA8crQBOvN6I6AYCLv6I/VsNDIdADxcSqjAZVIjOUkg6SnSftpNMh0APGHlmUv5NPnS",
        "6sGNpcsr8HMyHQBAh/JQ+I2Vv3zoHsFitHPbdWouMQQ8njWFVqfwqDu82jFQ2Lk8JTIdADxXDTMK",
        "M0W7O5AhykaOlJ7wMh0APFyRZTbwX3W7H8o0IIw5OiEyHQA8fgU1hozUXf90iEv7oPoVlDIdADz8",
        "lueS8AFkQS3NcdNWukv7Mh0AONrniyocBJMGOADnw3IDkTYiAUQgoqUDJSHFNSat4llTMFX6amyd",
        "/DwkUTWmnHyxoSDPSnHQvlYiMh0APK9CZUVzvV/yyD0zeWvGgaEyHQA86Jj41LbP73b3tLk6wMBZ",
        "nTIdADyO35y9DnKorSGHhJmhPUkzMh0APOIgH+AalbgVHLEPQEkkUKwyHQA8wCIKqdAhnTHqxXiA",
        "xVSFajIdADyjw8WOXX4/Zm+NmK8aU8IuMh0APL2Qx8Na0PxKmlGyUqZc1QYyHQA8hJcGz+YBb4Z+",
        "4LLjkF99UDIdADwj/2kgwsd8MCC1xvbfgYHhMh0APPxOECs9m3PLw5wSxN0XHc0yHQA81kykaWgh",
        "n3xVEwzN0x2VlzIdADy4UibnGCg6jaawGAmb0P73Mh0AOLr16Gil2KtK/EhIiqO5dTaIBDzyZxe2",
        "jGvUIFEyNRBp74L2MjoAPPuN7B7XMu0KXLflx0o3X4gyHQA8VpoK7PffiyIADHa/00S5SjIdADxD",
        "C9mldthKZ2WZY5xtV075Mh0AVMltVQK5/TnySRMCW1EnzMFqbAAAAAE="
    >>}
]).

compression_test_() ->
    {timeout, 60, [fun compression/0]}.

decompression_test_() ->
    {timeout, 60, [fun decompression/0]}.

snappy_1_1_3_frames_test() ->
    Inputs = inputs(),
    ?assertEqual([N || {N, _} <- Inputs], [N || {N, _} <- ?SNAPPY_1_1_3_FRAMES]),
    lists:foreach(
        fun({{Name, Input}, {Name, Frame}}) ->
            ?assertEqual({Name, {ok, Input}}, {Name, snappy:decompress(base64:decode(Frame))})
        end,
        lists:zip(Inputs, ?SNAPPY_1_1_3_FRAMES)
    ).

round_trip_test_() ->
    {timeout, 60, fun() ->
        lists:foreach(fun round_trip/1, inputs() ++ big_inputs())
    end}.

compression() ->
    DataIoList = lists:duplicate(11, ~"words that go unspoken, deeds that go undone"),
    Data = iolist_to_binary(DataIoList),
    Result = snappy:compress(Data),
    ?assertMatch({ok, _}, Result),
    {ok, Compressed} = Result,

    ?assertEqual(true, byte_size(Compressed) < byte_size(Data)),

    ?assertEqual(true, snappy:is_valid(Compressed)),
    ?assertEqual(false, snappy:is_valid(Data)),
    ?assertEqual(false, snappy:is_valid(~"foobar123")),
    ?assertEqual({ok, byte_size(Data)}, snappy:uncompressed_length(Compressed)),

    Result2 = snappy:compress(DataIoList),
    ?assertMatch({ok, _}, Result2),
    {ok, Compressed2} = Result2,

    ?assertEqual(byte_size(Compressed2), byte_size(Compressed)),
    ?assertEqual(true, snappy:is_valid(Compressed2)),
    ?assertEqual({ok, byte_size(Data)}, snappy:uncompressed_length(Compressed2)),
    ok.

decompression() ->
    DataIoList = lists:duplicate(11, ~"words that go unspoken, deeds that go undone"),
    Data = iolist_to_binary(DataIoList),
    Result = snappy:compress(Data),
    ?assertMatch({ok, _}, Result),
    {ok, Compressed} = Result,
    ?assertEqual({ok, Data}, snappy:decompress(Compressed)),

    Result2 = snappy:compress(DataIoList),
    ?assertMatch({ok, _}, Result2),
    {ok, Compressed2} = Result2,
    ?assertEqual({ok, Data}, snappy:decompress(Compressed2)),

    BigData = <<
        "mVPZzfDzKNeZrh1QdkMEgh2U0Bv2i3+bLJaCqgNibXuMuwfjrqTuxPGupxjI",
        "xEbuYR+u/KZvSDhoxnkpPbgJo7oiQv2ibDrrGZx7RDs3Nn7Ww51B7+zUL4tr",
        "G+16TlJilJT47Z4cQn8EpWex2bMRFAoJ6AMJAodLGbiD78yUyIorRKVcCa+k",
        "udzjsqYAoXzW/z8JCB6rbGGSbnLyqztR//ch5sRwSvYARlV+IamzBkDXFZxj",
        "5TAwAl2ZcbCeMX0qgXX4EonVZxc="
    >>,
    Result3 = snappy:compress(BigData),
    ?assertMatch({ok, _}, Result3),
    {ok, Compressed3} = Result3,
    ?assertEqual({ok, BigData}, snappy:decompress(Compressed3)),
    ok.

round_trip({Name, Input}) ->
    {ok, Frame} = snappy:compress(Input),
    ?assertEqual({Name, true}, {Name, snappy:is_valid(Frame)}),
    ?assertEqual({Name, {ok, byte_size(Input)}}, {Name, snappy:uncompressed_length(Frame)}),
    ?assertEqual({Name, {ok, Input}}, {Name, snappy:decompress(Frame)}).

% Built without term_to_binary/2 so the bytes cannot change with the OTP
% release. Short repeats (offsets 1 to 7) go through the decompressor's
% overlapping pattern copies.
inputs() ->
    [
        {empty, <<>>},
        {byte, ~"a"},
        {zeros, <<0:8000>>},
        {offset_2, binary:copy(~"ab", 500)},
        {offset_3, binary:copy(~"abc", 300)},
        {offset_7, binary:copy(~"abcdefg", 150)},
        {text, binary:copy(~"words that go unspoken, deeds that go undone ", 20)},
        {random, random_bytes(512)},
        {json, json_docs(40)},
        {rev_chain, rev_chain(64)}
    ].

% Shaped like the btree nodes couch_bt_engine writes
big_inputs() ->
    [
        {rev_tree_1000, term_to_binary(rev_tree(1, 1000), [{minor_version, 1}])},
        {kv_node_10_revs, term_to_binary(kv_node(40, 10), [{minor_version, 1}])},
        {random_64k, random_bytes(65536)}
    ].

% External term format of a linear rev tree: every 16 byte rev id is wrapped in
% the same tuple, binary and list headers: {RevId, [], [Child]}
rev_chain(Depth) ->
    <<<<104, 3, 109, 16:32, (rev(0, I))/binary, 106, 108, 1:32>> || I <- lists:seq(1, Depth)>>.

json_docs(Count) ->
    Fmt = "{\"_id\":\"doc-~6..0B\",\"value\":~B,\"tags\":[\"red\",\"blue\"]}\n",
    iolist_to_binary([io_lib:format(Fmt, [I, I * 7]) || I <- lists:seq(1, Count)]).

rev_tree(Seed, Depth) ->
    [LeafRev | Revs] = [rev(Seed, I) || I <- lists:seq(1, Depth)],
    Leaf = {LeafRev, {0, 4123456789, 1234, {512, 768}, []}, []},
    [{1, lists:foldl(fun(Rev, Child) -> {Rev, [], [Child]} end, Leaf, Revs)}].

kv_node(Docs, Depth) ->
    KVs = [
        {iolist_to_binary(io_lib:format("doc-~6..0B", [I])), {I, 0, {512, 768}, rev_tree(I, Depth)}}
     || I <- lists:seq(1, Docs)
    ],
    {kv_node, KVs}.

rev(Seed, I) ->
    erlang:md5(<<Seed:32, I:32>>).

random_bytes(Size) ->
    Bytes = <<<<(erlang:md5(<<I:32>>))/binary>> || I <- lists:seq(1, (Size + 15) div 16)>>,
    binary:part(Bytes, 0, Size).
