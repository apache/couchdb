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

-module(aegis_siv).

-export([block_encrypt/3, block_decrypt/3]).

-include("aegis_compat.hrl").

-spec block_encrypt(binary(), list(), binary()) -> {binary(), binary()}.
block_encrypt(Key, AAD, PlainText) when
    bit_size(Key) == 256; bit_size(Key) == 512
->
    {K1, K2} = split(Key),
    <<V:128>> = aegis_s2v:s2v(K1, AAD, PlainText),
    Q = V band 16#ffffffffffffffff7fffffff7fffffff,
    CipherText = aes_ctr(K2, <<Q:128>>, PlainText),
    {CipherText, <<V:128>>}.

block_decrypt(Key, AAD, {CipherText, <<V:128>>}) when
    bit_size(Key) == 256; bit_size(Key) == 512
->
    {K1, K2} = split(Key),
    Q = V band 16#ffffffffffffffff7fffffff7fffffff,
    PlainText = aes_ctr(K2, <<Q:128>>, CipherText),
    <<T:128>> = aegis_s2v:s2v(K1, AAD, PlainText),
    case V == T of
        true ->
            PlainText;
        false ->
            fail
    end.

split(Key) ->
    Half = byte_size(Key) div 2,
    <<K1:Half/binary, K2:Half/binary>> = Key,
    {K1, K2}.

aes_ctr(Key, IV, Data) ->
    ?stream_encrypt(?ctr(Key), Key, IV, Data).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encrypt_test_() ->
    [
        ?_assertEqual(
            {<<16#40c02b9690c4dc04daef7f6afe5c:112>>, <<16#85632d07c6e8f37f950acd320a2ecc93:128>>},
            block_encrypt(
                <<16#fffefdfcfbfaf9f8f7f6f5f4f3f2f1f0f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff:256>>,
                [<<16#101112131415161718191a1b1c1d1e1f2021222324252627:192>>],
                <<16#112233445566778899aabbccddee:112>>
            )
        ),

        ?_assertEqual(
            {
                <<
                    16#cb900f2fddbe404326601965c889bf17dba77ceb094fa663b7a3f748ba8af829ea64ad544a272e9c485b62a3fd5c0d:376
                >>,
                <<16#7bdb6e3b432667eb06f4d14bff2fbd0f:128>>
            },
            block_encrypt(
                <<16#7f7e7d7c7b7a79787776757473727170404142434445464748494a4b4c4d4e4f:256>>,
                [
                    <<
                        16#00112233445566778899aabbccddeeffdeaddadadeaddadaffeeddccbbaa99887766554433221100:320
                    >>,
                    <<16#102030405060708090a0:80>>,
                    <<16#09f911029d74e35bd84156c5635688c0:128>>
                ],
                <<
                    16#7468697320697320736f6d6520706c61696e7465787420746f20656e6372797074207573696e67205349562d414553:376
                >>
            )
        )
    ].

decrypt_test_() ->
    [
        ?_assertEqual(
            <<16#112233445566778899aabbccddee:112>>,
            block_decrypt(
                <<16#fffefdfcfbfaf9f8f7f6f5f4f3f2f1f0f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff:256>>,
                [<<16#101112131415161718191a1b1c1d1e1f2021222324252627:192>>],
                {<<16#40c02b9690c4dc04daef7f6afe5c:112>>, <<
                    16#85632d07c6e8f37f950acd320a2ecc93:128
                >>}
            )
        ),

        ?_assertEqual(
            fail,
            block_decrypt(
                <<16#fffefdfcfbfaf9f8f7f6f5f4f3f2f1f0f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff:256>>,
                [<<16#101112131415161718191a1b1c1d1e1f2021222324252627:192>>],
                {<<16#40c02b9690c4dc04daef7f6afe5c:112>>, <<
                    16#85632d07c6e8f37f950acd320a2ecc94:128
                >>}
            )
        ),

        ?_assertEqual(
            <<
                16#7468697320697320736f6d6520706c61696e7465787420746f20656e6372797074207573696e67205349562d414553:376
            >>,

            block_decrypt(
                <<16#7f7e7d7c7b7a79787776757473727170404142434445464748494a4b4c4d4e4f:256>>,
                [
                    <<
                        16#00112233445566778899aabbccddeeffdeaddadadeaddadaffeeddccbbaa99887766554433221100:320
                    >>,
                    <<16#102030405060708090a0:80>>,
                    <<16#09f911029d74e35bd84156c5635688c0:128>>
                ],
                {
                    <<
                        16#cb900f2fddbe404326601965c889bf17dba77ceb094fa663b7a3f748ba8af829ea64ad544a272e9c485b62a3fd5c0d:376
                    >>,
                    <<16#7bdb6e3b432667eb06f4d14bff2fbd0f:128>>
                }
            )
        )
    ].

-endif.
