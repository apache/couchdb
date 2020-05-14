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
-include_lib("fabric/include/fabric2.hrl").


-define(WRAPPED_KEY, {?DB_AEGIS, 1}).


-export([
    init_db/2,
    open_db/1,
    get_db_info/1,

    decrypt/2,
    decrypt/3,
    encrypt/3,
    wrap_fold_fun/2
]).

init_db(#{} = Db, Options) ->
    Db#{
        is_encrypted => aegis_server:init_db(Db, Options)
    }.


open_db(#{} = Db) ->
    Db#{
        is_encrypted => aegis_server:open_db(Db)
    }.


get_db_info(#{} = Db) ->
    ?AEGIS_KEY_MANAGER:get_db_info(Db).


encrypt(#{} = _Db, _Key, <<>>) ->
    <<>>;

encrypt(#{is_encrypted := false}, _Key, Value) when is_binary(Value) ->
    Value;

encrypt(#{is_encrypted := true} = Db, Key, Value)
        when is_binary(Key), is_binary(Value) ->
    aegis_server:encrypt(Db, Key, Value).


decrypt(#{} = Db, Rows) when is_list(Rows) ->
    lists:map(fun({Key, Value}) ->
        {Key, decrypt(Db, Key, Value)}
    end, Rows).

decrypt(#{} = _Db, _Key, <<>>) ->
    <<>>;

decrypt(#{is_encrypted := false}, _Key, Value) when is_binary(Value) ->
    Value;

decrypt(#{is_encrypted := true} = Db, Key, Value)
        when is_binary(Key), is_binary(Value) ->
    aegis_server:decrypt(Db, Key, Value).


wrap_fold_fun(Db, Fun) when is_function(Fun, 2) ->
    fun({Key, Value}, Acc) ->
        Fun({Key, decrypt(Db, Key, Value)}, Acc)
    end.
