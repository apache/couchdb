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

-module(aegis_key_manager_config).
-behaviour(aegis_key_manager).

-export([
    wrap_key/1,
    unwrap_key/1
]).

wrap_key(DataEncryptionKey) when is_binary(DataEncryptionKey) ->
    {ok, WrappingKeyId, WrappingKey} = current_wrapping_key(),
    WrappedKey = aegis:wrap_key(WrappingKey, [WrappingKeyId], DataEncryptionKey),
    {ok, <<(byte_size(WrappingKeyId)):8, WrappingKeyId/binary, WrappedKey/binary>>}.

unwrap_key(<<WrappingKeyIdLen:8, WrappingKeyId:WrappingKeyIdLen/binary, WrappedKey/binary>>) ->
    case wrapping_key(WrappingKeyId) of
        {ok, WrappingKeyId, WrappingKey} ->
            case aegis:unwrap_key(WrappingKey, [WrappingKeyId], WrappedKey) of
                fail ->
                    {error, unwrap_failed};
                Key when is_binary(Key) ->
                    {ok, Key}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
unwrap_key(_) ->
    {error, invalid_key}.

current_wrapping_key() ->
    wrapping_key(config:get("encryption", "wrapping_key_id")).

wrapping_key(KeyId) when is_binary(KeyId) ->
    wrapping_key(binary_to_list(KeyId));
wrapping_key(KeyId) when is_list(KeyId), length(KeyId) == 16 ->
    case get_config_binary("encryption_keys", KeyId, undefined) of
        Hex when is_binary(Hex), byte_size(Hex) == 64 ->
            {ok, list_to_binary(KeyId), couch_util:from_hex(Hex)};
        undefined ->
            {error, no_key};
        _ ->
            {error, invalid_key}
    end.

get_config_binary(Section, Key, Default) ->
    case config:get(Section, Key) of
        undefined ->
            Default;
        Value ->
            list_to_binary(Value)
    end.
