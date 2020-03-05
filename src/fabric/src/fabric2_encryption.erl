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
    encode/5,
    decode/5
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
    do_encode/6,
    do_decode/6
]).


-define(INIT_TIMEOUT, 60000).
-define(LABEL, "couchdb-aes256-gcm-encryption-key").

%% Master encryption key. Obviously never known to this module in real life
-define(MEK, <<246,83,186,200,242,183,138,51,2,193,181,37,156,130,190,209,181,69,206,157,69,154,112,158,141,158,196,132,81,253,187,67>>).
-define(IV, <<0:128>>).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_wrapped_kek(DbName) when is_binary(DbName) ->
    gen_server:call(?MODULE, {get_wrapped_kek, DbName}).


encode(WrappedKEK, DbName, DocId, DocRev, DocBody)
    when is_binary(WrappedKEK),
         is_binary(DbName),
         is_binary(DocId),
         is_binary(DocRev),
         is_binary(DocBody) ->
    gen_server:call(?MODULE, {encode, WrappedKEK, DbName, DocId, DocRev, DocBody}).


decode(WrappedKEK, DbName, DocId, DocRev, DocBody)
    when is_binary(WrappedKEK),
         is_binary(DbName),
         is_binary(DocId),
         is_binary(DocRev),
         is_binary(DocBody) ->
    gen_server:call(?MODULE, {decode, WrappedKEK, DbName, DocId, DocRev, DocBody}).



init(_) ->
    process_flag(sensitive, true),
    process_flag(trap_exit, true),

    case init_st() of
        {ok, St} ->
            proc_lib:init_ack({ok, self()}),
            gen_server:enter_loop(?MODULE, [], St, ?INIT_TIMEOUT);
        Error ->
            proc_lib:init_ack(Error)
    end.


terminate(_, _St) ->
    ok.


handle_call({get_wrapped_kek, _DbName}, _From, #{cache := Cache} = St) ->
    {ok, KEK, WrappedKEK} = get_kek(),
    true = ets:insert(Cache, {WrappedKEK, KEK}),
    {reply, {ok, WrappedKEK}, St};

handle_call({encode, WrappedKEK, DbName, DocId, DocRev, DocBody}, From, St) ->
    #{
        iid := InstanceId,
        cache := Cache,
        waiters := Waiters
    } = St,

    {ok, KEK} = unwrap_kek(Cache, WrappedKEK),
    {Pid, _Ref} = erlang:spawn_monitor(?MODULE,
        do_encode, [KEK, InstanceId, DbName, DocId, DocRev, DocBody]),

    NewSt = St#{
        waiters := dict:store(Pid, From, Waiters)
    },
    {noreply, NewSt};

handle_call({decode, WrappedKEK, DbName, DocId, DocRev, Encoded}, From, St) ->
    #{
        iid := InstanceId,
        cache := Cache,
        waiters := Waiters
    } = St,

    {ok, KEK} = unwrap_kek(Cache, WrappedKEK),
    {Pid, _Ref} = erlang:spawn_monitor(?MODULE,
        do_decode, [KEK, InstanceId, DbName, DocId, DocRev, Encoded]),

    NewSt = St#{
        waiters := dict:store(Pid, From, Waiters)
    },
    {noreply, NewSt}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info({'DOWN', _, _, Pid, Resp}, #{waiters := Waiters} = St) ->
    case dict:take(Pid, Waiters) of
        {From, Waiters1} ->
            gen_server:reply(From, Resp),
            NewSt = St#{
                waiters := Waiters1
            },
            {noreply, NewSt};
        error ->
            {noreply, St}
    end;

handle_info(timeout, St) ->
    {stop, normal, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.



init_st() ->
    FdbDirs = fabric2_server:fdb_directory(),
    Cache = ets:new(?MODULE, [set, private, compressed]),
    {ok, #{
        iid => iolist_to_binary(FdbDirs),
        cache => Cache,
        waiters => dict:new()
    }}.


do_encode(KEK, InstanceId, DbName, DocId, DocRev, DocBody) ->
    try
        {ok, AAD} = get_aad(InstanceId, DbName),
        {ok, DEK} = get_dek(KEK, DocId, DocRev),
        {CipherText, CipherTag} = crypto:crypto_one_time_aead(
            aes_256_gcm, DEK, <<0:96>>, DocBody, AAD, 16, true),
        <<CipherTag/binary, CipherText/binary>>
    of
        Resp ->
            exit({ok, Resp})
    catch
        _:Error ->
            exit({error, Error})
    end.


do_decode(KEK, InstanceId, DbName, DocId, DocRev, Encoded) ->
    try
        <<CipherTag:16/binary, CipherText/binary>> = Encoded,
        {ok, AAD} = get_aad(InstanceId, DbName),
        {ok, DEK} = get_dek(KEK, DocId, DocRev),
        crypto:crypto_one_time_aead(
            aes_256_gcm, DEK, <<0:96>>, CipherText, AAD, CipherTag, false)
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
    <<_:256>> = DEK = crypto:mac(hmac, sha256, KEK, PlainText),
    {ok, DEK}.


unwrap_kek(Cache, WrappedKEK) ->
    case ets:lookup(Cache, WrappedKEK) of
        [{WrappedKEK, KEK}] ->
            {ok, KEK};
        [] ->
            {ok, KEK, WrappedKEK} = unwrap_kek(WrappedKEK),
            true = ets:insert(Cache, {WrappedKEK, KEK}),
            {ok, KEK}
    end.


%% this mocks a call to an expernal system to aquire KEK
get_kek() ->
    KEK = crypto:strong_rand_bytes(32),
    WrappedKEK = crypto:crypto_one_time(aes_256_ctr, ?MEK, ?IV, KEK, true),
    {ok, KEK, WrappedKEK}.


%% this mocks a call to an expernal system to unwrap KEK
unwrap_kek(WrappedKEK) ->
    KEK = crypto:crypto_one_time(aes_256_ctr, ?MEK, ?IV, WrappedKEK, true),
    {ok, KEK, WrappedKEK}.
