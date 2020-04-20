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
-type aegis_config() :: term().

-callback generate_key(Db :: #{}, DbOptions :: list()) ->
    {ok, key(), aegis_config()} | {ok, false}.

-callback unwrap_key(Db :: #{}, AegisConfig :: aegis_config()) ->
    {ok, key(), aegis_config()}.


-export([
    generate_key/2,
    unwrap_key/2
]).


generate_key(#{} = Db, Options) ->
    ?AEGIS_KEY_MANAGER:generate_key(Db, Options).


unwrap_key(#{} = Db, AegisConfig) ->
    ?AEGIS_KEY_MANAGER:unwrap_key(Db, AegisConfig).
