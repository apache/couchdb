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

%% Assume old crypto api

-define(sha256_hmac(Key, PlainText), crypto:hmac(sha256, Key, PlainText)).

-define(aes_gcm_encrypt(Key, IV, AAD, Data),
    crypto:block_encrypt(aes_gcm, Key, IV, {AAD, Data, 16})).

-define(aes_gcm_decrypt(Key, IV, AAD, CipherText, CipherTag),
    crypto:block_decrypt(aes_gcm, Key, IV, {AAD, CipherText, CipherTag})).

-define(aes_ecb_encrypt(Key, Data),
	crypto:block_encrypt(aes_ecb, Key, Data)).

-define(aes_ecb_decrypt(Key, Data),
	crypto:block_decrypt(aes_ecb, Key, Data)).

%% Replace macros if new crypto api is available
-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 22).

-undef(sha256_hmac).
-define(sha256_hmac(Key, PlainText), crypto:mac(hmac, sha256, Key, PlainText)).

-undef(aes_gcm_encrypt).
-define(aes_gcm_encrypt(Key, IV, AAD, Data),
    crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, Data, AAD, 16, true)).

-undef(aes_gcm_decrypt).
-define(aes_gcm_decrypt(Key, IV, AAD, CipherText, CipherTag),
    crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, CipherText,
    AAD, CipherTag, false)).

-define(key_alg(Key), case bit_size(Key) of
	128 -> aes_128_ecb; 192 -> aes_192_ecb; 256 -> aes_256_ecb end).

-undef(aes_ecb_encrypt).
-define(aes_ecb_encrypt(Key, Data),
        crypto:crypto_one_time(?key_alg(Key), Key, Data, true)).

-undef(aes_ecb_decrypt).
-define(aes_ecb_decrypt(Key, Data),
        crypto:crypto_one_time(?key_alg(Key), Key, Data, false)).

-endif.
-endif.