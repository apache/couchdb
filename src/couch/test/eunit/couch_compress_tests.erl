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

-module(couch_compress_tests).

-include_lib("couch/include/couch_eunit.hrl").

-define(TERM, {[{a, 1}, {b, 2}, {c, 3}, {d, 4}, {e, 5}]}).

-define(NONE,
    <<131, 104, 1, 108, 0, 0, 0, 5, 104, 2, 100, 0, 1, 97, 97, 1, 104, 2, 100, 0, 1, 98, 97, 2, 104,
        2, 100, 0, 1, 99, 97, 3, 104, 2, 100, 0, 1, 100, 97, 4, 104, 2, 100, 0, 1, 101, 97, 5, 106>>
).
-define(DEFLATE,
    <<131, 80, 0, 0, 0, 48, 120, 218, 203, 96, 204, 97, 96, 96, 96, 205, 96, 74, 97, 96, 76, 76,
        100, 4, 211, 73, 137, 76, 96, 58, 57, 145, 25, 76, 167, 36, 178, 128, 233, 212, 68, 214, 44,
        0, 212, 169, 9, 51>>
).
-define(SNAPPY,
    <<1, 49, 64, 131, 104, 1, 108, 0, 0, 0, 5, 104, 2, 100, 0, 1, 97, 97, 1, 104, 1, 8, 8, 98, 97,
        2, 5, 8, 8, 99, 97, 3, 5, 8, 44, 100, 97, 4, 104, 2, 100, 0, 1, 101, 97, 5, 106>>
).
-define(SNAPPY_BIGENDIAN,
    <<1, 49, 60, 131, 104, 1, 108, 0, 0, 0, 5, 104, 2, 100, 0, 1, 97, 97, 1, 5, 8, 8, 98, 97, 2, 5,
        8, 8, 99, 97, 3, 5, 8, 44, 100, 97, 4, 104, 2, 100, 0, 1, 101, 97, 5, 106>>
).
-define(CORRUPT, <<2, 12, 85, 06>>).

-define(DEFLATE_COMPRESSION, {deflate, 9}).
-define(DEFLATE_COMPRESSION_ZERO, {deflate, 0}).

couch_compress_test_() ->
    {
        "couch compress tests",
        [
            fun t_compress/0,
            fun t_decompress/0,
            fun t_recompress/0,
            fun t_is_compressed/0,
            fun t_uncompressed_size/0
        ]
    }.

t_compress() ->
    ?assertEqual(?NONE, couch_compress:compress(?TERM, none)),
    ?assertNotEqual(?NONE, couch_compress:compress(?TERM, ?DEFLATE_COMPRESSION)),
    ?assertNotEqual(?NONE, couch_compress:compress(?TERM, snappy)),

    % assert that compressed output is smaller than uncompressed output
    ?assert(bit_size(couch_compress:compress(?TERM, ?DEFLATE_COMPRESSION)) < bit_size(?NONE)),
    ?assert(bit_size(couch_compress:compress(?TERM, snappy)) < bit_size(?NONE)).

t_decompress() ->
    ?assertEqual(?TERM, couch_compress:decompress(?NONE)),
    ?assertEqual(?TERM, couch_compress:decompress(?DEFLATE)),
    ?assertEqual(?TERM, couch_compress:decompress(?SNAPPY)),
    ?assertEqual(?TERM, couch_compress:decompress(?SNAPPY_BIGENDIAN)),
    ?assertError(invalid_compression, couch_compress:decompress(?CORRUPT)).

t_recompress() ->
    Result1 = ?NONE,

    % none -> deflate
    Result2 = couch_compress:compress(Result1, ?DEFLATE_COMPRESSION),
    ?assert(couch_compress:is_compressed(Result2, ?DEFLATE_COMPRESSION)),

    % deflate -> snappy
    Result3 = couch_compress:compress(Result2, snappy),
    ?assert(couch_compress:is_compressed(Result3, snappy)),

    % snappy -> none
    Result4 = couch_compress:compress(Result3, none),
    ?assert(couch_compress:is_compressed(Result4, none)),

    % none -> snappy
    Result5 = couch_compress:compress(Result4, snappy),
    ?assert(couch_compress:is_compressed(Result5, snappy)),

    % snappy -> deflate
    Result6 = couch_compress:compress(Result5, ?DEFLATE_COMPRESSION),
    ?assert(couch_compress:is_compressed(Result6, ?DEFLATE_COMPRESSION)),

    % deflate -> none
    Result7 = couch_compress:compress(Result6, none),
    ?assert(couch_compress:is_compressed(Result7, none)).

t_is_compressed() ->
    ?assert(couch_compress:is_compressed(?NONE, none)),
    ?assert(couch_compress:is_compressed(?DEFLATE, ?DEFLATE_COMPRESSION)),
    ?assert(couch_compress:is_compressed(?SNAPPY, snappy)),
    ?assert(couch_compress:is_compressed(?SNAPPY_BIGENDIAN, snappy)),
    ?assertNot(couch_compress:is_compressed(?NONE, ?DEFLATE_COMPRESSION_ZERO)),
    ?assertNot(couch_compress:is_compressed(?NONE, ?DEFLATE_COMPRESSION)),
    ?assertNot(couch_compress:is_compressed(?NONE, snappy)),
    ?assertNot(couch_compress:is_compressed(?DEFLATE, none)),
    ?assertNot(couch_compress:is_compressed(?DEFLATE, snappy)),
    ?assertNot(couch_compress:is_compressed(?SNAPPY, none)),
    ?assertNot(couch_compress:is_compressed(?SNAPPY, ?DEFLATE_COMPRESSION)),
    ?assertNot(couch_compress:is_compressed(?SNAPPY_BIGENDIAN, none)),
    ?assertNot(couch_compress:is_compressed(?SNAPPY_BIGENDIAN, ?DEFLATE_COMPRESSION)),

    ?assertError(invalid_compression, couch_compress:is_compressed(?CORRUPT, none)),
    ?assertError(invalid_compression, couch_compress:is_compressed(?CORRUPT, ?DEFLATE_COMPRESSION)),
    ?assertError(invalid_compression, couch_compress:is_compressed(?CORRUPT, snappy)).

t_uncompressed_size() ->
    ?assertEqual(49, couch_compress:uncompressed_size(?NONE)),
    ?assertEqual(49, couch_compress:uncompressed_size(?DEFLATE)),
    ?assertEqual(49, couch_compress:uncompressed_size(?SNAPPY)),
    ?assertEqual(49, couch_compress:uncompressed_size(?SNAPPY_BIGENDIAN)),

    ?assertEqual(
        5,
        couch_compress:uncompressed_size(
            couch_compress:compress(x, ?DEFLATE_COMPRESSION)
        )
    ),

    ?assertError(invalid_compression, couch_compress:uncompressed_size(?CORRUPT)).
