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


-type key() :: binary().
-type wrapped_key() :: binary().

-callback generate_key(Db :: #{}) ->
    {ok, key(), wrapped_key()}.

-callback unwrap_key(Db :: #{}, WrappedKey :: wrapped_key()) ->
    {ok, key(), wrapped_key()}.


-export([
    generate_key/1,
    unwrap_key/2
]).


generate_key(#{} = Db) ->
    ?AEGIS_KEY_MANAGER:generate_key(Db).


unwrap_key(#{} = Db, WrappedKey) ->
    ?AEGIS_KEY_MANAGER:unwrap_key(Db, WrappedKey).
