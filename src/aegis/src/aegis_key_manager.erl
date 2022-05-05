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

-module(aegis_key_manager).

-export([
    wrap_key/1,
    unwrap_key/1
]).

-type dek() :: binary().
-type wek() :: binary().

-callback wrap_key(DataEncryptionKey :: dek()) ->
    {ok, WrappedKey :: wek()}
    | {error, Reason :: term()}.

-callback unwrap_key(WrappedKey :: wek()) ->
    {ok, DataEncryptionKey :: dek()}
    | {error, Reason :: term()}.

wrap_key(DataEncryptionKey) ->
    Module = key_manager_module(),
    Module:wrap_key(DataEncryptionKey).

unwrap_key(WrappedKey) ->
    Module = key_manager_module(),
    Module:unwrap_key(WrappedKey).

key_manager_module() ->
    Module = config:get("aegis", "key_manager_module", "aegis_key_manager_config"),
    list_to_atom(Module).
