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

-module(aegis).
-include("aegis.hrl").
-include_lib("fabric/include/fabric2.hrl").


-define(WRAPPED_KEY, {?DB_AEGIS, 1}).

-define(CACHE, aegis_key_cache).


-export([
    create/2,
    open/2,

    decrypt/2,
    decrypt/3,
    decrypt/4,
    encrypt/3,
    encrypt/4,
    wrap_fold_fun/2
]).

create(#{} = Db, _Options) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    % Fetch unwrapped key
    WrappedKey = gen_server:call(?CACHE, {get_wrapped_key, Db}),

    % And store it
    FDBKey = erlfdb_tuple:pack(?WRAPPED_KEY, DbPrefix),
    ok = erlfdb:set(Tx, FDBKey, WrappedKey),

    Db#{
        aegis => WrappedKey
    }.


open(#{} = Db, _Options) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    % Fetch wrapped key
    FDBKey = erlfdb_tuple:pack(?WRAPPED_KEY, DbPrefix),
    WrappedKey = erlfdb:wait(erlfdb:get(Tx, FDBKey)),

    Db1 = Db#{aegis => WrappedKey},

    case gen_server:call(?CACHE, {unwrap_key, Db1}) of
        WrappedKey ->
            Db1;
        NewWrappedKey ->
            FDBKey = erlfdb_tuple:pack(?WRAPPED_KEY, DbPrefix),
            ok = erlfdb:set(Tx, FDBKey, NewWrappedKey),
            Db1#{aegis => NewWrappedKey}
    end.


encrypt(#{} = _Db, _Key, <<>>) ->
    <<>>;

encrypt(#{} = Db, Key, Value) when is_binary(Key), is_binary(Value) ->
    gen_server:call(?CACHE, {encrypt, Db, Key, Value}).

encrypt(DbKey, UUID, Key, Value) ->
    EncryptionKey = crypto:strong_rand_bytes(32),
    <<WrappedKey:320>> = aegis_keywrap:key_wrap(DbKey, EncryptionKey),

    {CipherText, <<CipherTag:128>>} =
        ?aes_gcm_encrypt(
           EncryptionKey,
           <<0:96>>,
           <<UUID/binary, 0:8, Key/binary>>,
           Value),
    <<1:8, WrappedKey:320, CipherTag:128, CipherText/binary>>.


decrypt(#{} = Db, Rows) when is_list(Rows) ->
    lists:map(fun({Key, Value}) ->
        {Key, decrypt(Db, Key, Value)}
    end, Rows).

decrypt(#{} = _Db, _Key, <<>>) ->
    <<>>;

decrypt(#{} = Db, Key, Value) when is_binary(Key), is_binary(Value) ->
    gen_server:call(?CACHE, {decrypt, Db, Key, Value}).

decrypt(DbKey, UUID, Key, Value) ->
    case Value of
        <<1:8, WrappedKey:320, CipherTag:128, CipherText/binary>> ->
            case aegis_keywrap:key_unwrap(DbKey, <<WrappedKey:320>>) of
                fail ->
                    erlang:error(decryption_failed);
                DecryptionKey ->
                    Decrypted =
                    ?aes_gcm_decrypt(
                        DecryptionKey,
                        <<0:96>>,
                        <<UUID/binary, 0:8, Key/binary>>,
                        CipherText,
                        <<CipherTag:128>>),
                    if Decrypted /= error -> Decrypted; true ->
                        erlang:error(decryption_failed)
                    end
            end;
        _ ->
            erlang:error(not_ciphertext)
    end.


wrap_fold_fun(Db, Fun) when is_function(Fun, 2) ->
    fun({Key, Value}, Acc) ->
        Fun({Key, decrypt(Db, Key, Value)}, Acc)
    end.
