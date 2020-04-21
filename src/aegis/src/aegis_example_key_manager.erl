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

-module(aegis_example_key_manager).


-behaviour(aegis_key_manager).


-export([
    init/0,
    generate_key/3,
    unwrap_key/3
]).



init() ->
    case config:get("aegis_example_key_manager", "key_provider") of
        undefined ->
            erlang:error(misconfigured_key_provider);
        KeyProvider ->
            PassPhrase = os:cmd(KeyProvider),
            {ok, RootKey} = couch_passwords:pbkdf2(
                iolist_to_binary(PassPhrase), <<0:256>>, 10000, 32),
            <<(binary_to_integer(RootKey, 16)):256>>
    end.


generate_key(RootKey, #{} = _Db, _Options) ->
    DbKey = crypto:strong_rand_bytes(32),
    WrappedKey = aegis_keywrap:key_wrap(RootKey, DbKey),

    %% just an example of how to represent the arbitrary options
    AegisConfig = {<<"wrapped_key">>, WrappedKey},
    {ok, DbKey, AegisConfig}.


unwrap_key(RootKey, #{} = _Db, AegisConfig) ->
    {<<"wrapped_key">>, WrappedKey} = AegisConfig,
    case aegis_keywrap:key_unwrap(RootKey, WrappedKey) of
        fail ->
            erlang:error(unwrap_failed);
        DbKey ->
            {ok, DbKey, AegisConfig}
    end.
