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

-module(couch_base32).

-export([encode/1, decode/1]).

-define(SET, <<"ABCDEFGHIJKLMNOPQRSTUVWXYZ234567">>).

-spec encode(binary()) -> binary().
encode(Plain) when is_binary(Plain) ->
    IoList = encode(Plain, 0, byte_size(Plain) * 8, []),
    iolist_to_binary(lists:reverse(IoList)).

encode(_Plain, _ByteOffset, 0, Acc) ->
    Acc;
encode(Plain, ByteOffset, BitsRemaining, Acc) when BitsRemaining == 8 ->
    <<A:5, B:3>> = binary:part(Plain, ByteOffset, 1),
    [<<(binary:at(?SET, A)), (binary:at(?SET, B bsl 2)), "======">> | Acc];
encode(Plain, ByteOffset, BitsRemaining, Acc) when BitsRemaining == 16 ->
    <<A:5, B:5, C:5, D:1>> = binary:part(Plain, ByteOffset, 2),
    [
        <<
            (binary:at(?SET, A)),
            (binary:at(?SET, B)),
            (binary:at(?SET, C)),
            (binary:at(?SET, D bsl 4)),
            "===="
        >>
        | Acc
    ];
encode(Plain, ByteOffset, BitsRemaining, Acc) when BitsRemaining == 24 ->
    <<A:5, B:5, C:5, D:5, E:4>> = binary:part(Plain, ByteOffset, 3),
    [
        <<
            (binary:at(?SET, A)),
            (binary:at(?SET, B)),
            (binary:at(?SET, C)),
            (binary:at(?SET, D)),
            (binary:at(?SET, E bsl 1)),
            "==="
        >>
        | Acc
    ];
encode(Plain, ByteOffset, BitsRemaining, Acc) when BitsRemaining == 32 ->
    <<A:5, B:5, C:5, D:5, E:5, F:5, G:2>> = binary:part(Plain, ByteOffset, 4),
    [
        <<
            (binary:at(?SET, A)),
            (binary:at(?SET, B)),
            (binary:at(?SET, C)),
            (binary:at(?SET, D)),
            (binary:at(?SET, E)),
            (binary:at(?SET, F)),
            (binary:at(?SET, G bsl 3)),
            "="
        >>
        | Acc
    ];
encode(Plain, ByteOffset, BitsRemaining, Acc) when BitsRemaining >= 40 ->
    <<A:5, B:5, C:5, D:5, E:5, F:5, G:5, H:5>> =
        binary:part(Plain, ByteOffset, 5),
    Output = <<
        (binary:at(?SET, A)),
        (binary:at(?SET, B)),
        (binary:at(?SET, C)),
        (binary:at(?SET, D)),
        (binary:at(?SET, E)),
        (binary:at(?SET, F)),
        (binary:at(?SET, G)),
        (binary:at(?SET, H))
    >>,
    encode(Plain, ByteOffset + 5, BitsRemaining - 40, [Output | Acc]).

-spec decode(binary()) -> binary().
decode(Encoded) when is_binary(Encoded) ->
    IoList = decode(Encoded, 0, []),
    iolist_to_binary(lists:reverse(IoList)).

decode(Encoded, ByteOffset, Acc) when ByteOffset == byte_size(Encoded) ->
    Acc;
decode(Encoded, ByteOffset, Acc) ->
    case binary:part(Encoded, ByteOffset, 8) of
        <<A:1/binary, B:1/binary, "======">> ->
            [<<(find_in_set(A)):5, (find_in_set(B) bsr 2):3>> | Acc];
        <<A:1/binary, B:1/binary, C:1/binary, D:1/binary, "====">> ->
            [
                <<
                    (find_in_set(A)):5,
                    (find_in_set(B)):5,
                    (find_in_set(C)):5,
                    (find_in_set(D) bsr 4):1
                >>
                | Acc
            ];
        <<A:1/binary, B:1/binary, C:1/binary, D:1/binary, E:1/binary, "===">> ->
            [
                <<
                    (find_in_set(A)):5,
                    (find_in_set(B)):5,
                    (find_in_set(C)):5,
                    (find_in_set(D)):5,
                    (find_in_set(E) bsr 1):4
                >>
                | Acc
            ];
        <<A:1/binary, B:1/binary, C:1/binary, D:1/binary, E:1/binary, F:1/binary, G:1/binary, "=">> ->
            [
                <<
                    (find_in_set(A)):5,
                    (find_in_set(B)):5,
                    (find_in_set(C)):5,
                    (find_in_set(D)):5,
                    (find_in_set(E)):5,
                    (find_in_set(F)):5,
                    (find_in_set(G) bsr 3):2
                >>
                | Acc
            ];
        <<A:1/binary, B:1/binary, C:1/binary, D:1/binary, E:1/binary, F:1/binary, G:1/binary,
            H:1/binary>> ->
            decode(
                Encoded,
                ByteOffset + 8,
                [
                    <<
                        (find_in_set(A)):5,
                        (find_in_set(B)):5,
                        (find_in_set(C)):5,
                        (find_in_set(D)):5,
                        (find_in_set(E)):5,
                        (find_in_set(F)):5,
                        (find_in_set(G)):5,
                        (find_in_set(H)):5
                    >>
                    | Acc
                ]
            )
    end.

find_in_set(Char) ->
    case binary:match(?SET, Char) of
        nomatch ->
            erlang:error(not_base32);
        {Offset, _} ->
            Offset
    end.
