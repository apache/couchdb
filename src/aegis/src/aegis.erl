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
    create/2,
    open/2,

    decrypt/2,
    decrypt/3,
    encrypt/3,
    wrap_fold_fun/2
]).

create(#{} = Db, Options) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    {ok, AegisConfig} = aegis_server:generate_key(Db, Options),

    FDBKey = erlfdb_tuple:pack(?WRAPPED_KEY, DbPrefix),
    Packed = erlfdb_tuple:pack({AegisConfig}),
    ok = erlfdb:set(Tx, FDBKey, Packed),

    Db#{
        aegis => AegisConfig
    }.


open(#{} = Db, _Options) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    FDBKey = erlfdb_tuple:pack(?WRAPPED_KEY, DbPrefix),
    Packed = erlfdb:wait(erlfdb:get(Tx, FDBKey)),
    {AegisConfig} = erlfdb_tuple:unpack(Packed),

    Db#{
        aegis => AegisConfig
    }.


encrypt(#{} = _Db, _Key, <<>>) ->
    <<>>;

encrypt(#{aegis := false}, _Key, Value) when is_binary(Value) ->
    Value;

encrypt(#{} = Db, Key, Value) when is_binary(Key), is_binary(Value) ->
    aegis_server:encrypt(Db, Key, Value).


decrypt(#{} = Db, Rows) when is_list(Rows) ->
    lists:map(fun({Key, Value}) ->
        {Key, decrypt(Db, Key, Value)}
    end, Rows).

decrypt(#{} = _Db, _Key, <<>>) ->
    <<>>;

decrypt(#{aegis := false}, _Key, Value) when is_binary(Value) ->
    Value;

decrypt(#{} = Db, Key, Value) when is_binary(Key), is_binary(Value) ->
    aegis_server:decrypt(Db, Key, Value).


wrap_fold_fun(Db, Fun) when is_function(Fun, 2) ->
    fun({Key, Value}, Acc) ->
        Fun({Key, decrypt(Db, Key, Value)}, Acc)
    end.
