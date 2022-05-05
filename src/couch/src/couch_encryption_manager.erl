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

-module(couch_encryption_manager).

-export([new_dek/1, unwrap_dek/2, encryption_options/1]).

-callback new_dek(DbName :: binary()) ->
    {ok, KeyID :: binary(), DEK :: binary(), WEK :: binary()}
    | dont_encrypt
    | {error, Reason :: term()}.

-callback unwrap_dek(KeyID :: binary(), WEK :: binary()) ->
    {ok, DEK :: binary()}
    | {ok, NewKeyID :: binary(), DEK :: binary(), NewWEK :: binary()}
    | {error, Reason :: term()}.

new_dek(DbName) ->
    case manager() of
        undefined ->
            dont_encrypt;
        Module ->
            Module:new_dek(DbName)
    end.

unwrap_dek(KeyID, WEK) ->
    case manager() of
        undefined ->
            {error, encryption_not_supported};
        Manager ->
            Manager:unwrap_dek(KeyID, WEK)
    end.

manager() ->
    case config:get("encryption", "manager") of
        undefined ->
            undefined;
        Module ->
            list_to_atom(Module)
    end.

%% Extract just the encryption related options from an options list.
encryption_options(Options) ->
    case lists:keyfind(db_name, 1, Options) of
        false -> [];
        {db_name, DbName} -> [{db_name, DbName}]
    end.
