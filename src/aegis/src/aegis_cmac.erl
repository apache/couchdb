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

-module(aegis_cmac).

-export([cmac/2]).

cmac(Key, Message) ->
    cmac(Key, <<0:128>>, Message).

cmac(Key, X, <<Last:16/binary>>) ->
    {K1, _K2} = generate_subkeys(Key),
    crypto:crypto_one_time(cmac_cipher(Key), Key, crypto:exor(X, crypto:exor(Last, K1)), true);
cmac(Key, X, <<Block:16/binary, Rest/binary>>) ->
    cmac(Key, crypto:crypto_one_time(cmac_cipher(Key), Key, crypto:exor(X, Block), true), Rest);
cmac(Key, X, Last) ->
    {_K1, K2} = generate_subkeys(Key),
    crypto:crypto_one_time(
        cmac_cipher(Key),
        Key,
        crypto:exor(X, crypto:exor(aegis_util:pad(Last), K2)),
        true
    ).

generate_subkeys(Key) ->
    L = crypto:crypto_one_time(cmac_cipher(Key), Key, <<0:128>>, true),
    K1 = aegis_util:double(L),
    K2 = aegis_util:double(K1),
    {K1, K2}.

cmac_cipher(Key) when bit_size(Key) == 128 ->
    aes_128_ecb;
cmac_cipher(Key) when bit_size(Key) == 256 ->
    aes_256_ecb.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

cmac_test_() ->
    [
        ?_assertEqual(
            <<16#bb1d6929e95937287fa37d129b756746:128>>,
            cmac(
                <<16#2b7e151628aed2a6abf7158809cf4f3c:128>>,
                <<>>
            )
        ),

        ?_assertEqual(
            <<16#070a16b46b4d4144f79bdd9dd04a287c:128>>,
            cmac(
                <<16#2b7e151628aed2a6abf7158809cf4f3c:128>>,
                <<16#6bc1bee22e409f96e93d7e117393172a:128>>
            )
        ),

        ?_assertEqual(
            <<16#028962f61b7bf89efc6b551f4667d983:128>>,
            cmac(
                <<16#603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4:256>>,
                <<>>
            )
        ),

        ?_assertEqual(
            <<16#28a7023f452e8f82bd4bf28d8c37c35c:128>>,
            cmac(
                <<16#603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4:256>>,
                <<16#6bc1bee22e409f96e93d7e117393172a:128>>
            )
        )
    ].

-endif.
