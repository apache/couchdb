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

-module(aegis_file_key_manager).


-behaviour(aegis_key_manager).


-export([
    generate_key/2,
    unwrap_key/2
]).


-define(ROOT_KEY, <<1:256>>).


generate_key(#{} = _Db, _Options) ->
    DbKey = crypto:strong_rand_bytes(32),
    WrappedKey = aegis_keywrap:key_wrap(?ROOT_KEY, DbKey),
    %% just an example of how to represent the arbitrary options
    AegisConfig = {<<"wrapped_key">>, WrappedKey},
    {ok, DbKey, AegisConfig}.


unwrap_key(#{} = _Db, {<<"wrapped_key">>, WrappedKey} = AegisConfig) ->
    case aegis_keywrap:key_unwrap(?ROOT_KEY, WrappedKey) of
        fail ->
            error(unwrap_failed);
        DbKey ->
            {ok, DbKey, AegisConfig}
    end.
