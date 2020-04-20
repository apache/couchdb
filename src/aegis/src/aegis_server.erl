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
    generate_key/2,
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

%% workers callbacks
-export([
    do_generate_key/2,
    do_unwrap_key/1,
    do_encrypt/5,
    do_decrypt/5
]).


-define(INIT_TIMEOUT, 60000).
-define(TIMEOUT, 10000).


-record(entry, {id, key}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec generate_key(Db :: #{}, Options :: list()) ->
        {ok, term()} | {error, term()}.
generate_key(#{} = Db, Options) ->
    gen_server:call(?MODULE, {generate_key, Db, Options}).


-spec encrypt(Db :: #{}, Key :: binary(), Value :: binary()) -> binary().
encrypt(#{} = Db, Key, Value)  when is_binary(Key), is_binary(Value) ->
    gen_server:call(?MODULE, {encrypt, Db, Key, Value}).


-spec decrypt(Db :: #{}, Key :: binary(), Value :: binary()) -> binary().
decrypt(#{} = Db, Key, Value) when is_binary(Key), is_binary(Value) ->
    gen_server:call(?MODULE, {decrypt, Db, Key, Value}).


%% gen_server functions

init([]) ->
    process_flag(sensitive, true),
    Cache = ets:new(?MODULE, [set, private, {keypos, #entry.id}]),

    St = #{
        cache => Cache,
        openers => dict:new(),
        waiters => dict:new(),
        unwrappers => dict:new()
    },
    {ok, St, ?INIT_TIMEOUT}.


terminate(_Reason, St) ->
    #{
        openers := Openers,
        waiters := Waiters
    } = St,

    dict:fold(fun(_AegisConfig, WaitList, _) ->
        lists:foreach(fun(#{from := From}) ->
            gen_server:reply(From, {error, decryption_aborted})
        end, WaitList)
    end, ok, Waiters),

    dict:fold(fun(Ref, From, _) ->
        erlang:demonitor(Ref),
        gen_server:reply(From, {error, decryption_aborted})
    end, ok, Openers),
    ok.


handle_call({generate_key, Db, Options}, From, #{openers := Openers} = St) ->
    {_, Ref} = erlang:spawn_monitor(?MODULE, do_generate_key, [Db, Options]),
    Openers1 = dict:store(Ref, From, Openers),
    {noreply, St#{openers := Openers1}, ?TIMEOUT};

handle_call({encrypt, Db, Key, Value}, From, St) ->
    NewSt = maybe_spawn_worker(St, From, do_encrypt, Db, Key, Value),
    {noreply, NewSt, ?TIMEOUT};

handle_call({decrypt, Db, Key, Value}, From, St) ->
    NewSt = maybe_spawn_worker(St, From, do_decrypt, Db, Key, Value),
    {noreply, NewSt, ?TIMEOUT};

handle_call(_Msg, _From, St) ->
    {noreply, St}.


handle_cast(_Msg, St) ->
    {noreply, St}.


handle_info({'DOWN', Ref, _, _Pid, {ok, DbKey, AegisConfig}}, St) ->
    #{
        cache := Cache,
        openers := Openers,
        waiters := Waiters,
        unwrappers := Unwrappers
    } = St,

    ok = insert(Cache, AegisConfig, DbKey),

    case dict:take(Ref, Openers) of
        {From, Openers1} ->
            gen_server:reply(From, {ok, AegisConfig}),
            {noreply, St#{openers := Openers1}, ?TIMEOUT};
        error ->
            Unwrappers1 = dict:erase(AegisConfig, Unwrappers),
            {WaitList, Waiters1} = dict:take(AegisConfig, Waiters),
            lists:foreach(fun(Waiter) ->
                #{
                    from := From,
                    action := Action,
                    args := Args
                } = Waiter,
                erlang:spawn(?MODULE, Action, [From, DbKey | Args])
            end, WaitList),
            NewSt = St#{waiters := Waiters1, unwrappers := Unwrappers1},
            {noreply, NewSt, ?TIMEOUT}
    end;

handle_info({'DOWN', Ref, process, _Pid, {error, Error}}, St) ->
    #{
        openers := Openers,
        waiters := Waiters,
        unwrappers := Unwrappers
    } = St,

    case dict:take(Ref, Openers) of
        {From, Openers1} ->
            gen_server:reply(From, {error, Error}),
            {noreply, St#{openers := Openers1}, ?TIMEOUT};
        error ->
            {ok, AegisConfig} = dict:fold(fun
                (K, V, _) when V == Ref -> {ok, K};
                (_, _, Acc) -> Acc
            end, not_found, Unwrappers),
            Unwrappers1 = dict:erase(AegisConfig, Unwrappers),
            {WaitList, Waiters1} = dict:take(AegisConfig, Waiters),
            lists:foreach(fun(#{from := From}) ->
                gen_server:reply(From, {error, Error})
            end, WaitList),
            NewSt = St#{waiters := Waiters1, unwrappers := Unwrappers1},
            {noreply, NewSt, ?TIMEOUT}
    end;

handle_info(_Msg, St) ->
    {noreply, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


%% workers functions

do_generate_key(#{} = Db, Options) ->
    process_flag(sensitive, true),
    try
        aegis_key_manager:generate_key(Db, Options)
    of
        Resp ->
            exit(Resp)
    catch
        _:Error ->
            exit({error, Error})
    end.


do_unwrap_key(#{aegis := AegisConfig} = Db) ->
    process_flag(sensitive, true),
    try
        aegis_key_manager:unwrap_key(Db, AegisConfig)
    of
        Resp ->
            exit(Resp)
    catch
        _:Error ->
            exit({error, Error})
    end.


do_encrypt(From, DbKey, #{uuid := UUID}, Key, Value) ->
    process_flag(sensitive, true),
    try
        EncryptionKey = crypto:strong_rand_bytes(32),
        <<WrappedKey:320>> = aegis_keywrap:key_wrap(DbKey, EncryptionKey),

        {CipherText, <<CipherTag:128>>} =
            ?aes_gcm_encrypt(
               EncryptionKey,
               <<0:96>>,
               <<UUID/binary, 0:8, Key/binary>>,
               Value),
        <<1:8, WrappedKey:320, CipherTag:128, CipherText/binary>>
    of
        Resp ->
            gen_server:reply(From, Resp)
    catch
        _:Error ->
            gen_server:reply(From, {error, Error})
    end.


do_decrypt(From, DbKey, #{uuid := UUID}, Key, Value) ->
    process_flag(sensitive, true),
    try
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
        end
    of
        Resp ->
            gen_server:reply(From, Resp)
    catch
        _:Error ->
            gen_server:reply(From, {error, Error})
    end.


%% private functions

maybe_spawn_worker(St, From, Action, Db, Key, Value) ->
    #{
        cache := Cache,
        waiters := Waiters
    } = St,

    #{aegis := AegisConfig} = Db,

    case lookup(Cache, AegisConfig) of
        {ok, DbKey} ->
            erlang:spawn(?MODULE, Action, [From, DbKey, Db, Key, Value]),
            St;
        {error, not_found} ->
            NewSt = maybe_spawn_unwrapper(St, Db),
            Waiter = #{
                from => From,
                action => Action,
                args => [Db, Key, Value]
            },
            Waiters1 = dict:append(AegisConfig, Waiter, Waiters),
            NewSt#{waiters := Waiters1}
     end.


maybe_spawn_unwrapper(St, #{aegis := AegisConfig} = Db) ->
    #{
        unwrappers := Unwrappers
    } = St,

    case dict:is_key(AegisConfig, Unwrappers) of
        true ->
            St;
        false ->
            {_Pid, Ref} = erlang:spawn_monitor(?MODULE, do_unwrap_key, [Db]),
            Unwrappers1 = dict:store(AegisConfig, Ref, Unwrappers),
            St#{unwrappers := Unwrappers1}
    end.


%% cache functions

insert(Cache, AegisConfig, DbKey) ->
    Entry = #entry{id = AegisConfig, key = DbKey},
    true = ets:insert(Cache, Entry),
    ok.


lookup(Cache, AegisConfig) ->
    case ets:lookup(Cache, AegisConfig) of
        [#entry{id = AegisConfig, key = DbKey}] ->
            {ok, DbKey};
        [] ->
            {error, not_found}
    end.
