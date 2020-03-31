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

-module(fabric2_encryption).
-behaviour(gen_server).
-vsn(1).


-export([
    start_link/0,
    get_wrapped_kek/1,
    encrypt/5,
    decrypt/5
]).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-export([
    req_wrapped_kek/1,
    do_encrypt/6,
    do_decrypt/6
]).


-define(INIT_TIMEOUT, 60000).
-define(TIMEOUT, 10000).
-define(LABEL, "couchdb-aes256-gcm-encryption-key").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_wrapped_kek(DbName) when is_binary(DbName) ->
    case config:get_boolean("encryption", "enabled", false) of
        true -> gen_server:call(?MODULE, {get_wrapped_kek, DbName});
        false -> {ok, false}
    end.


encrypt(WrappedKEK, DbName, DocId, DocRev, Value)
    when is_binary(WrappedKEK),
         is_binary(DbName),
         is_binary(DocId),
         is_binary(DocRev),
         is_binary(Value) ->
    Request = {encrypt, WrappedKEK, DbName, DocId, DocRev, Value},
    gen_server:call(?MODULE, Request).


decrypt(WrappedKEK, DbName, DocId, DocRev, Value)
    when is_binary(WrappedKEK),
         is_binary(DbName),
         is_binary(DocId),
         is_binary(DocRev),
         is_binary(Value) ->
    Request = {decrypt, WrappedKEK, DbName, DocId, DocRev, Value},
    gen_server:call(?MODULE, Request).



init(_) ->
    process_flag(sensitive, true),

    FdbDirs = fabric2_server:fdb_directory(),
    Cache = ets:new(?MODULE, [set, private]),
    St = #{
        iid => iolist_to_binary(FdbDirs),
        cache => Cache,
        waiters => dict:new()
    },

    proc_lib:init_ack({ok, self()}),
    gen_server:enter_loop(?MODULE, [], St, ?INIT_TIMEOUT).


terminate(_, #{waiters := Waiters} = _St) ->
    ok = dict:fold(fun(Ref, From, ok) ->
        erlang:demonitor(Ref),
        gen_server:reply(From, {error, encryption_server_terminated}),
        ok
    end, ok, Waiters).


handle_call({get_wrapped_kek, DbName}, From, St) ->
    #{
        waiters := Waiters
    } = St,

    {_Pid, Ref} = erlang:spawn_monitor(?MODULE, req_wrapped_kek, [DbName]),

    NewSt = St#{
        waiters := dict:store(Ref, From, Waiters)
    },
    {noreply, NewSt, ?TIMEOUT};

handle_call({encrypt, WrappedKEK, DbName, DocId, DocRev, Value}, From, St) ->
    #{
        iid := InstanceId,
        cache := Cache,
        waiters := Waiters
    } = St,

    {ok, KEK} = unwrap_kek(Cache, WrappedKEK),
    {_Pid, Ref} = erlang:spawn_monitor(?MODULE,
        do_encrypt, [KEK, InstanceId, DbName, DocId, DocRev, Value]),

    NewSt = St#{
        waiters := dict:store(Ref, From, Waiters)
    },
    {noreply, NewSt, ?TIMEOUT};

handle_call({decrypt, WrappedKEK, DbName, DocId, DocRev, Value}, From, St) ->
    #{
        iid := InstanceId,
        cache := Cache,
        waiters := Waiters
    } = St,

    {ok, KEK} = unwrap_kek(Cache, WrappedKEK),
    {_Pid, Ref} = erlang:spawn_monitor(?MODULE,
        do_decrypt, [KEK, InstanceId, DbName, DocId, DocRev, Value]),

    NewSt = St#{
        waiters := dict:store(Ref, From, Waiters)
    },
    {noreply, NewSt, ?TIMEOUT}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info({'DOWN', Ref, process, _Pid, Resp}, St) ->
    #{
        cache := Cache,
        waiters := Waiters
    } = St,

    case dict:take(Ref, Waiters) of
        {From, Waiters1} ->
            case Resp of
                {kek, {ok, KEK, WrappedKEK}} ->
                    true = ets:insert(Cache, {WrappedKEK, KEK}),
                    gen_server:reply(From, {ok, WrappedKEK});
                _ ->
                    gen_server:reply(From, Resp)
            end,

            NewSt = St#{
                waiters := Waiters1
            },
            {noreply, NewSt, ?TIMEOUT};
        error ->
            {noreply, St, ?TIMEOUT}
    end;

handle_info(timeout, St) ->
    {stop, normal, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.



req_wrapped_kek(DbName) ->
    process_flag(sensitive, true),
    try
        fabric2_encryption_plugin:get_wrapped_kek(DbName)
    of
        Resp ->
            exit({kek, Resp})
    catch
        _:Error ->
            exit({error, Error})
    end.


do_encrypt(KEK, InstanceId, DbName, DocId, DocRev, Value) ->
    process_flag(sensitive, true),
    try
        {ok, AAD} = get_aad(InstanceId, DbName),
        {ok, DEK} = get_dek(KEK, DocId, DocRev),
        {CipherText, CipherTag} = crypto:block_encrypt(
            aes_gcm, DEK, <<0:96>>, {AAD, Value, 16}),
        <<CipherTag/binary, CipherText/binary>>
    of
        Resp ->
            exit({ok, Resp})
    catch
        _:Error ->
            exit({error, Error})
    end.


do_decrypt(KEK, InstanceId, DbName, DocId, DocRev, Value) ->
    process_flag(sensitive, true),
    try
        <<CipherTag:16/binary, CipherText/binary>> = Value,
        {ok, AAD} = get_aad(InstanceId, DbName),
        {ok, DEK} = get_dek(KEK, DocId, DocRev),
        crypto:block_decrypt(
            aes_gcm, DEK, <<0:96>>, {AAD, CipherText, CipherTag})
    of
        Resp ->
            exit({ok, Resp})
    catch
        _:Error ->
            exit({error, Error})
    end.


get_aad(InstanceId, DbName) when is_binary(InstanceId), is_binary(DbName) ->
    {ok, <<InstanceId/binary, 0:8, DbName/binary>>}.


get_dek(KEK, DocId, DocRev) when bit_size(KEK) == 256 ->
    Context = <<DocId/binary, 0:8, DocRev/binary>>,
    PlainText = <<1:16, ?LABEL, 0:8, Context/binary, 256:16>>,
    <<_:256>> = DEK = crypto:hmac(sha256, KEK, PlainText),
    {ok, DEK}.


unwrap_kek(Cache, WrappedKEK) ->
    case ets:lookup(Cache, WrappedKEK) of
        [{WrappedKEK, KEK}] ->
            {ok, KEK};
        [] ->
            {ok, KEK, WrappedKEK} = fabric2_encryption_plugin:unwrap_kek(
                WrappedKEK),
            true = ets:insert(Cache, {WrappedKEK, KEK}),
            {ok, KEK}
    end.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_dek_test() ->
    KEK = crypto:strong_rand_bytes(32),
    {ok, DEK} = get_dek(KEK, <<"0001">>, <<"1-abcdefgh">>),
    ?assertNotEqual(KEK, DEK),
    ?assertEqual(32, byte_size(DEK)).

encrypt_decrypt_test() ->
    KEK = crypto:strong_rand_bytes(32),
    IId = <<"dev">>,
    DbName = <<"db">>,
    DocId = <<"0001">>,
    DocRev = <<"1-abcdefgh">>,
    Value = term_to_binary({{{[{<<"text">>, <<"test">>}]}, [], false}}),

    {ok, EncResult} = try
        do_encrypt(KEK, IId, DbName, DocId, DocRev, Value)
    catch
        exit:ER -> ER
    end,
    ?assertNotEqual(Value, EncResult),

    {ok, DecResult} = try
        do_decrypt(KEK, IId, DbName, DocId, DocRev, EncResult)
    catch
        exit:DR -> DR
    end,
    ?assertEqual(Value, DecResult).


-endif.
