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

-module(aegis_server).

-behaviour(gen_server).

-vsn(1).


-include("aegis.hrl").


%% aegis_server API
-export([
    start_link/0,
    init_db/2,
    open_db/2,
    encrypt/3,
    decrypt/3
]).

%% gen_server callbacks
-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).



-define(KEY_CHECK, aegis_key_check).
-define(INIT_TIMEOUT, 60000).
-define(TIMEOUT, 10000).


-record(entry, {uuid, encryption_key}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec init_db(Db :: #{}, Options :: list()) -> boolean().
init_db(#{uuid := UUID} = Db, Options) ->
    process_flag(sensitive, true),

    case ?AEGIS_KEY_MANAGER:init_db(Db, Options) of
        {ok, DbKey} ->
            gen_server:call(?MODULE, {insert_key, UUID, DbKey}),
            true;
        false ->
            false
    end.


-spec open_db(Db :: #{}, Options :: list()) -> boolean().
open_db(#{} = Db, Options) ->
    process_flag(sensitive, true),

    case do_open_db(Db, Options) of
        {ok, _DbKey} ->
            true;
        false ->
            false
    end.


-spec encrypt(Db :: #{}, Key :: binary(), Value :: binary()) -> binary().
encrypt(#{} = Db, Key, Value) when is_binary(Key), is_binary(Value) ->
    #{
        uuid := UUID
    } = Db,

    case ets:member(?KEY_CHECK, UUID) of
        true ->
            case gen_server:call(?MODULE, {encrypt, Db, Key, Value}) of
                CipherText when is_binary(CipherText) ->
                    CipherText;
                {error, {_Tag, {_C_FileName,_LineNumber}, _Desc} = Reason} ->
                    couch_log:error("aegis encryption failure: ~p ", [Reason]),
                    erlang:error(decryption_failed);
                {error, Reason} ->
                    erlang:error(Reason)
            end;
        false ->
            process_flag(sensitive, true),

            {ok, DbKey} = do_open_db(Db),
            do_encrypt(DbKey, Db, Key, Value)
    end.


-spec decrypt(Db :: #{}, Key :: binary(), Value :: binary()) -> binary().
decrypt(#{} = Db, Key, Value) when is_binary(Key), is_binary(Value) ->
    #{
        uuid := UUID
    } = Db,

    case ets:member(?KEY_CHECK, UUID) of
        true ->
            case gen_server:call(?MODULE, {decrypt, Db, Key, Value}) of
                PlainText when is_binary(PlainText) ->
                    PlainText;
                {error, {_Tag, {_C_FileName,_LineNumber}, _Desc} = Reason} ->
                    couch_log:error("aegis decryption failure: ~p ", [Reason]),
                    erlang:error(decryption_failed);
                {error, Reason} ->
                    erlang:error(Reason)
            end;
        false ->
            process_flag(sensitive, true),

            {ok, DbKey} = do_open_db(Db),
            do_decrypt(DbKey, Db, Key, Value)
    end.


%% gen_server functions

init([]) ->
    process_flag(sensitive, true),
    Cache = ets:new(?MODULE, [set, private, {keypos, #entry.uuid}]),
    ets:new(?KEY_CHECK, [named_table, protected, {read_concurrency, true}]),

    St = #{
        cache => Cache
    },
    {ok, St, ?INIT_TIMEOUT}.


terminate(_Reason, _St) ->
    ok.


handle_call({insert_key, UUID, DbKey}, _From, #{cache := Cache} = St) ->
    ok = insert(Cache, UUID, DbKey),
    {reply, ok, St, ?TIMEOUT};

handle_call({encrypt, #{uuid := UUID} = Db, Key, Value}, From, St) ->
    #{
        cache := Cache
    } = St,

    {ok, DbKey} = lookup(Cache, UUID),

    erlang:spawn(fun() ->
        process_flag(sensitive, true),
        try
            do_encrypt(DbKey, Db, Key, Value)
        of
            Resp ->
                gen_server:reply(From, Resp)
        catch
            _:Error ->
                gen_server:reply(From, {error, Error})
        end
    end),

    {noreply, St, ?TIMEOUT};

handle_call({decrypt, #{uuid := UUID} = Db, Key, Value}, From, St) ->
    #{
        cache := Cache
    } = St,

    {ok, DbKey} = lookup(Cache, UUID),

    erlang:spawn(fun() ->
        process_flag(sensitive, true),
        try
            do_decrypt(DbKey, Db, Key, Value)
        of
            Resp ->
                gen_server:reply(From, Resp)
        catch
            _:Error ->
                gen_server:reply(From, {error, Error})
        end
    end),

    {noreply, St, ?TIMEOUT};

handle_call(_Msg, _From, St) ->
    {noreply, St}.


handle_cast(_Msg, St) ->
    {noreply, St}.


handle_info(_Msg, St) ->
    {noreply, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


%% private functions

do_open_db(#{} = Db) ->
    #{
        uuid := UUID,
        user_ctx := UserCtx,
        db_options := Options0
    } = Db,

    %% put back elements removed in fabric2_fdb:open/2
    Options = [{uuid, UUID}, {user_ctx, UserCtx} | Options0],
    do_open_db(Db, Options).


do_open_db(#{uuid := UUID} = Db, Options) ->
    case ?AEGIS_KEY_MANAGER:open_db(Db, Options) of
        {ok, DbKey} ->
            gen_server:call(?MODULE, {insert_key, UUID, DbKey}),
            {ok, DbKey};
        false ->
            false
    end.


do_encrypt(DbKey, #{uuid := UUID}, Key, Value) ->
    EncryptionKey = crypto:strong_rand_bytes(32),
    <<WrappedKey:320>> = aegis_keywrap:key_wrap(DbKey, EncryptionKey),

    {CipherText, <<CipherTag:128>>} =
        ?aes_gcm_encrypt(
           EncryptionKey,
           <<0:96>>,
           <<UUID/binary, 0:8, Key/binary>>,
           Value),
    <<1:8, WrappedKey:320, CipherTag:128, CipherText/binary>>.


do_decrypt(DbKey, #{uuid := UUID}, Key, Value) ->
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


%% cache functions

insert(Cache, UUID, DbKey) ->
    Entry = #entry{uuid = UUID, encryption_key = DbKey},
    true = ets:insert(Cache, Entry),
    true = ets:insert(?KEY_CHECK, {UUID, true}),
    ok.


lookup(Cache, UUID) ->
    case ets:lookup(Cache, UUID) of
        [#entry{uuid = UUID, encryption_key = DbKey}] ->
            {ok, DbKey};
        [] ->
            {error, not_found}
    end.
