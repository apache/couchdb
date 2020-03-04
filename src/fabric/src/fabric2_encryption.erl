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
    encode/4,
    decode/4
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
    do_encode/5,
    do_decode/5
]).


-define(INIT_TIMEOUT, 60000).
-define(LABEL, "couchdb-aes256-gcm-encryption-key").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


encode(DbName, DocId, DocRev, DocBody)
    when is_binary(DbName),
         is_binary(DocId),
         is_binary(DocRev),
         is_binary(DocBody) ->
    gen_server:call(?MODULE, {encode, DbName, DocId, DocRev, DocBody}).


decode(DbName, DocId, DocRev, DocBody)
    when is_binary(DbName),
         is_binary(DocId),
         is_binary(DocRev),
         is_binary(DocBody) ->
    gen_server:call(?MODULE, {decode, DbName, DocId, DocRev, DocBody}).



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


handle_call({encode, DbName, DocId, DocRev, DocBody}, From, St) ->
    #{
        iid := InstanceId,
        waiters := Waiters
    } = St,

    {Pid, _Ref} = erlang:spawn_monitor(?MODULE,
        do_encode, [InstanceId, DbName, DocId, DocRev, DocBody]),

    NewSt = St#{
        waiters := dict:store(Pid, From, Waiters)
    },
    {noreply, NewSt};

handle_call({decode, DbName, DocId, DocRev, Encoded}, From, St) ->
    #{
        iid := InstanceId,
        waiters := Waiters
    } = St,

    {Pid, _Ref} = erlang:spawn_monitor(?MODULE,
        do_decode, [InstanceId, DbName, DocId, DocRev, Encoded]),

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
    {ok, #{
        iid => iolist_to_binary(FdbDirs),
        waiters => dict:new()
    }}.


do_encode(InstanceId, DbName, DocId, DocRev, DocBody) ->
    try
        {ok, AAD} = get_aad(InstanceId, DbName),
        {ok, DEK} = get_dek(DbName, DocId, DocRev),
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


do_decode(InstanceId, DbName, DocId, DocRev, Encoded) ->
    try
        <<CipherTag:16/binary, CipherText/binary>> = Encoded,
        {ok, AAD} = get_aad(InstanceId, DbName),
        {ok, DEK} = get_dek(DbName, DocId, DocRev),
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


get_dek(DbName, DocId, DocRev) ->
    {ok, KEK} = get_kek(DbName),
    Context = <<DocId/binary, 0:8, DocRev/binary>>,
    PlainText = <<1:16, ?LABEL, 0:8, Context/binary, 256:16>>,
    <<_:256>> = DEK = crypto:mac(hmac, sha256, KEK, PlainText),
    {ok, DEK}.


get_kek(DbName) ->
    KEK = crypto:hash(sha256, DbName),
    {ok, KEK}.
