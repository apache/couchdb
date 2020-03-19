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

-module(fabric2_encryption_provider).

-export([
    get_kek/1,
    unwrap_kek/1
]).


get_kek(_DbName) ->
    case get_mek_iv() of
        {ok, MEK, IV} ->
            KEK = crypto:strong_rand_bytes(32),
            Enc = crypto:stream_init(aes_ctr, MEK, IV),
            {_, WrappedKEK} = crypto:stream_encrypt(Enc, KEK),
            {decided, {ok, KEK, WrappedKEK}};
        {error, Error} ->
            {decided, {error, Error}}
    end.


unwrap_kek(WrappedKEK) ->
    case get_mek_iv() of
        {ok, MEK, IV} ->
            Enc = crypto:stream_init(aes_ctr, MEK, IV),
            {_, KEK} = crypto:stream_decrypt(Enc, WrappedKEK),
            {decided, {ok, KEK, WrappedKEK}};
        {error, Error} ->
            {decided, {error, Error}}
    end.



get_mek_iv() ->
    KeyFileReturn = file:read_file(config:get("encryption", "key_file")),
    IVFileReturn = file:read_file(config:get("encryption", "iv_file")),
    case {KeyFileReturn, IVFileReturn} of
        {{ok, MEK}, {ok, IV}}
                when bit_size(MEK) == 512, bit_size(IV) == 256 ->
            {ok, <<<<I:4>> || <<I>> <= MEK>>, <<<<I:4>> || <<I>> <= IV>>};
        {{ok, _}, _} ->
            {error, invalid_key_length};
        {{error, Error}, _} ->
            {error, {invalid_key_file, Error}};
        {_, {ok, _}} ->
            {error, invalid_iv_length};
        {_, {error, Error}} ->
            {error, {invalid_iv_file, Error}}
    end.
