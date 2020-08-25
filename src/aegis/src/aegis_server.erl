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
    open_db/1,
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
-define(CACHE_LIMIT, 100000).
-define(CACHE_MAX_AGE_SEC, 1800).
-define(CACHE_EXPIRATION_CHECK_SEC, 10).
-define(LAST_ACCESSED_INACTIVITY_SEC, 10).


-record(entry, {uuid, encryption_key, counter, last_accessed, expires_at}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec init_db(Db :: #{}, Options :: list()) -> boolean().
init_db(#{uuid := UUID} = Db, Options) ->
    sensitive(fun() ->
        case ?AEGIS_KEY_MANAGER:init_db(Db, Options) of
            {ok, DbKey} ->
                gen_server:call(?MODULE, {insert_key, UUID, DbKey}),
                true;
            false ->
                false
        end
    end).


-spec open_db(Db :: #{}) -> boolean().
open_db(#{} = Db) ->
    sensitive(fun() ->
        case do_open_db(Db) of
            {ok, _DbKey} ->
                true;
            false ->
                false
        end
    end).


-spec encrypt(Db :: #{}, Key :: binary(), Value :: binary()) -> binary().
encrypt(#{} = Db, Key, Value) when is_binary(Key), is_binary(Value) ->
    #{
        uuid := UUID
    } = Db,

    case is_key_fresh(UUID) of
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
            sensitive(fun() ->
                {ok, DbKey} = do_open_db(Db),
                do_encrypt(DbKey, Db, Key, Value)
            end)
    end.


-spec decrypt(Db :: #{}, Key :: binary(), Value :: binary()) -> binary().
decrypt(#{} = Db, Key, Value) when is_binary(Key), is_binary(Value) ->
    #{
        uuid := UUID
    } = Db,

    case is_key_fresh(UUID) of
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
            sensitive(fun() ->
                {ok, DbKey} = do_open_db(Db),
                do_decrypt(DbKey, Db, Key, Value)
            end)
    end.


%% gen_server functions

init([]) ->
    process_flag(sensitive, true),
    Cache = ets:new(?MODULE, [set, private, {keypos, #entry.uuid}]),
    ByAccess = ets:new(?MODULE,
        [ordered_set, private, {keypos, #entry.counter}]),
    ets:new(?KEY_CHECK, [named_table, protected, {read_concurrency, true}]),

    erlang:send_after(0, self(), maybe_remove_expired),

    St = #{
        cache => Cache,
        by_access => ByAccess,
        counter => 0
    },
    {ok, St, ?INIT_TIMEOUT}.


terminate(_Reason, _St) ->
    ok.


handle_call({insert_key, UUID, DbKey}, _From, #{cache := Cache} = St) ->
    case ets:lookup(Cache, UUID) of
        [#entry{uuid = UUID} = Entry] ->
            delete(St, Entry);
        [] ->
            ok
    end,
    NewSt = insert(St, UUID, DbKey),
    {reply, ok, NewSt, ?TIMEOUT};

handle_call({encrypt, #{uuid := UUID} = Db, Key, Value}, From, St) ->

    {ok, DbKey} = lookup(St, UUID),

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

    {ok, DbKey} = lookup(St, UUID),

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


handle_cast({accessed, UUID}, St) ->
    NewSt = bump_last_accessed(St, UUID),
    {noreply, NewSt};


handle_cast(_Msg, St) ->
    {noreply, St}.


handle_info(maybe_remove_expired, St) ->
    remove_expired_entries(St),
    CheckInterval = erlang:convert_time_unit(
        expiration_check_interval(), second, millisecond),
    erlang:send_after(CheckInterval, self(), maybe_remove_expired),
    {noreply, St};

handle_info(_Msg, St) ->
    {noreply, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


%% private functions

do_open_db(#{uuid := UUID} = Db) ->
    case ?AEGIS_KEY_MANAGER:open_db(Db) of
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


is_key_fresh(UUID) ->
    Now = fabric2_util:now(sec),

    case ets:lookup(?KEY_CHECK, UUID) of
        [{UUID, ExpiresAt}] when ExpiresAt >= Now -> true;
        _ -> false
    end.


%% cache functions

insert(St, UUID, DbKey) ->
    #{
        cache := Cache,
        by_access := ByAccess,
        counter := Counter
    } = St,

    Now = fabric2_util:now(sec),
    ExpiresAt = Now + max_age(),

    Entry = #entry{
        uuid = UUID,
        encryption_key = DbKey,
        counter = Counter,
        last_accessed = Now,
        expires_at = ExpiresAt
    },

    true = ets:insert(Cache, Entry),
    true = ets:insert_new(ByAccess, Entry),
    true = ets:insert(?KEY_CHECK, {UUID, ExpiresAt}),

    CacheLimit = cache_limit(),
    CacheSize = ets:info(Cache, size),

    case CacheSize > CacheLimit of
        true ->
            LRUKey = ets:first(ByAccess),
            [LRUEntry] = ets:lookup(ByAccess, LRUKey),
            delete(St, LRUEntry);
        false ->
            ok
    end,

    St#{counter := Counter + 1}.


lookup(#{cache := Cache}, UUID) ->
    case ets:lookup(Cache, UUID) of
        [#entry{uuid = UUID, encryption_key = DbKey} = Entry] ->
            maybe_bump_last_accessed(Entry),
            {ok, DbKey};
        [] ->
            {error, not_found}
    end.


delete(St, #entry{uuid = UUID} = Entry) ->
    #{
        cache := Cache,
        by_access := ByAccess
    } = St,

    true = ets:delete(?KEY_CHECK, UUID),
    true = ets:delete_object(Cache, Entry),
    true = ets:delete_object(ByAccess, Entry).


maybe_bump_last_accessed(#entry{last_accessed = LastAccessed} = Entry) ->
    case fabric2_util:now(sec) > LastAccessed + ?LAST_ACCESSED_INACTIVITY_SEC of
        true ->
            gen_server:cast(?MODULE, {accessed, Entry#entry.uuid});
        false ->
            ok
    end.


bump_last_accessed(St, UUID) ->
    #{
        cache := Cache,
        by_access := ByAccess,
        counter := Counter
    } = St,


    [#entry{counter = OldCounter} = Entry0] = ets:lookup(Cache, UUID),

    Entry = Entry0#entry{
        last_accessed = fabric2_util:now(sec),
        counter = Counter
    },

    true = ets:insert(Cache, Entry),
    true = ets:insert_new(ByAccess, Entry),

    ets:delete(ByAccess, OldCounter),

    St#{counter := Counter + 1}.


remove_expired_entries(St) ->
    #{
        cache := Cache,
        by_access := ByAccess
    } = St,

    MatchConditions = [{'=<', '$1', fabric2_util:now(sec)}],

    KeyCheckMatchHead = {'_', '$1'},
    KeyCheckExpired = [{KeyCheckMatchHead, MatchConditions, [true]}],
    Count = ets:select_delete(?KEY_CHECK, KeyCheckExpired),

    CacheMatchHead = #entry{expires_at = '$1', _ = '_'},
    CacheExpired = [{CacheMatchHead, MatchConditions, [true]}],
    Count = ets:select_delete(Cache, CacheExpired),
    Count = ets:select_delete(ByAccess, CacheExpired).



max_age() ->
    config:get_integer("aegis", "cache_max_age_sec", ?CACHE_MAX_AGE_SEC).


expiration_check_interval() ->
    config:get_integer(
        "aegis", "cache_expiration_check_sec", ?CACHE_EXPIRATION_CHECK_SEC).


cache_limit() ->
    config:get_integer("aegis", "cache_limit", ?CACHE_LIMIT).


sensitive(Fun) when is_function(Fun, 0) ->
    OldValue = process_flag(sensitive, true),
    try
        Fun()
    after
        process_flag(sensitive, OldValue)
    end.
