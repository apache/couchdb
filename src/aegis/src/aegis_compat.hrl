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

-define(block_encrypt(Cipher, Key, Data), crypto:block_encrypt(Cipher, Key, Data)).

-define(stream_encrypt(Cipher, Key, IVec, Data), element(2, crypto:stream_encrypt(crypto:stream_init(Cipher, Key, IVec), Data))).

-define(ecb(Key), aes_ecb).

-define(ctr(Key), aes_ctr).

%% Replace macros if new crypto api is available
-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 22).

-undef(block_encrypt).
-define(block_encrypt(Cipher, Key, Data), crypto:crypto_one_time(Cipher, Key, Data, true)).

-undef(stream_encrypt).
-define(stream_encrypt(Cipher, Key, IVec, Data), crypto:crypto_one_time(Cipher, Key, IVec, Data, true)).

-undef(ecb).
-define(ecb(Key), case bit_size(Key) of
	128 -> aes_128_ecb; 192 -> aes_192_ecb; 256 -> aes_256_ecb end).

-undef(ctr).
-define(ctr(Key), case bit_size(Key) of
	128 -> aes_128_ctr; 192 -> aes_192_ctr; 256 -> aes_256_ctr end).

-endif.
-endif.