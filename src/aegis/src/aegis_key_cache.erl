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

-module(aegis_key_cache).

-behaviour(gen_server).

-vsn(1).


-export([
    start_link/0
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
    get_wrapped_key/1,
    unwrap_key/1,
    do_encrypt/4,
    do_decrypt/4
]).


-define(ROOT_KEY, <<1:256>>).

-define(INIT_TIMEOUT, 60000).

-define(TIMEOUT, 10000).


-record(entry, {id, key}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% gen_server functions

init([]) ->
    process_flag(sensitive, true),
    Cache = ets:new(?MODULE, [set, private, {keypos, #entry.id}]),

    St = #{
        cache => Cache,
        clients => dict:new(),
        waiters => dict:new(),
        unwrappers => dict:new()
    },
    {ok, St, ?INIT_TIMEOUT}.


terminate(_Reason, St) ->
    #{
        clients := Clients,
        waiters := Waiters
    } = St,

    dict:fold(fun(_WrappedKey, WaitList, _) ->
        lists:foreach(fun(#{from := From}) ->
            gen_server:reply(From, {error, decryption_failed})
        end, WaitList)
    end, ok, Waiters),

    dict:fold(fun(Ref, From, _) ->
        erlang:demonitor(Ref),
        gen_server:reply(From, {error, decryption_failed})
    end, ok, Clients),
    ok.


handle_call({get_wrapped_key, Db}, From, #{clients := Clients} = St) ->
    {_Pid, Ref} = erlang:spawn_monitor(?MODULE, get_wrapped_key, [Db]),
    Clients1 = dict:store(Ref, From, Clients),
    {noreply, St#{clients := Clients1}, ?TIMEOUT};

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


handle_info({'DOWN', Ref, _, _Pid, {key, {ok, DbKey, WrappedKey}}}, St) ->
    #{
        cache := Cache,
        clients := Clients,
        waiters := Waiters,
        unwrappers := Unwrappers
    } = St,

    NewSt1 = case dict:take(WrappedKey, Unwrappers) of
        {Ref, Unwrappers1} ->
            ok = insert(Cache, WrappedKey, DbKey),
            St#{unwrappers := Unwrappers1};
        error ->
            %% FIXME! it might be new wrapped key != old wrapped key
            %% fold here to search for it based on ref
            St
    end,

    NewSt2 = case dict:take(WrappedKey, Waiters) of
        {WaitList, Waiters1} ->
            Clients1 = lists:foldl(fun(Waiter, Acc) ->
                #{
                    from := From,
                    action := Action,
                    args := Args
                } = Waiter,

                {_Pid1, Ref1} = erlang:spawn_monitor(
                    ?MODULE, Action, [DbKey | Args]),

                dict:store(Ref1, From, Acc)
            end, Clients, WaitList),

            NewSt1#{clients := Clients1, waiters := Waiters1};
        error ->
            NewSt1
    end,

    NewSt3 = maybe_reply(NewSt2, Ref, WrappedKey),
    {noreply, NewSt3, ?TIMEOUT};

handle_info({'DOWN', Ref, process, _Pid, Resp}, St) ->
    NewSt = maybe_reply(St, Ref, Resp),
    {noreply, NewSt, ?TIMEOUT};

handle_info(_Msg, St) ->
    {noreply, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


%% workers functions

maybe_spawn_worker(St, From, Action, #{aegis := WrappedKey} = Db, Key, Value) ->
    #{
        cache := Cache,
        clients := Clients,
        waiters := Waiters,
        unwrappers := Unwrappers
    } = St,

    case lookup(Cache, WrappedKey) of
        {ok, DbKey} ->
            {_Pid, Ref} = erlang:spawn_monitor(
                ?MODULE, Action, [DbKey, Db, Key, Value]),
            Clients1 = dict:store(Ref, From, Clients),
            St#{clients := Clients1};
        {error, not_found} ->
            NewSt = case dict:is_key(WrappedKey, Unwrappers) of
                true ->
                    St;
                false ->
                    {_Pid, Ref} = erlang:spawn_monitor(
                        ?MODULE, unwrap_key, [Db]),
                    Unwrappers1 = dict:store(WrappedKey, Ref, Unwrappers),
                    St#{unwrappers := Unwrappers1}
            end,
            Waiter = #{
                from => From,
                action => Action,
                args => [Db, Key, Value]
            },
            Waiters1 = dict:append(WrappedKey, Waiter, Waiters),
            NewSt#{waiters := Waiters1}
     end.


maybe_reply(#{clients := Clients} = St, Ref, Resp) ->
    case dict:take(Ref, Clients) of
        {From, Clients1} ->
            gen_server:reply(From, Resp),
            St#{clients := Clients1};
        error ->
            St
    end.


get_wrapped_key(#{} = _Db) ->
    process_flag(sensitive, true),
    try
        DbKey = crypto:strong_rand_bytes(32),
        WrappedKey = aegis_keywrap:key_wrap(?ROOT_KEY, DbKey),
        {ok, DbKey, WrappedKey}
    of
        Resp ->
            exit({key, Resp})
    catch
        _:Error ->
            exit({error, Error})
    end.


unwrap_key(#{aegis := WrappedKey} = _Db) ->
    process_flag(sensitive, true),
    try
        %% this could be atom fail, throw error is so !!
        DbKey = aegis_keywrap:key_unwrap(?ROOT_KEY, WrappedKey),
        {ok, DbKey, WrappedKey}
    of
        Resp ->
            exit({key, Resp})
    catch
        _:Error ->
            %% FIXME! add tag key and WrappedKey so we can respond to Waiters
            exit({error, Error})
    end.


do_encrypt(DbKey, #{uuid := UUID}, Key, Value) ->
    process_flag(sensitive, true),
    try
        aegis:encrypt(DbKey, UUID, Key, Value)
    of
        Resp ->
            exit(Resp)
    catch
        _:Error ->
            exit({error, Error})
    end.


do_decrypt(DbKey, #{uuid := UUID}, Key, Value) ->
    process_flag(sensitive, true),
    try
        aegis:decrypt(DbKey, UUID, Key, Value)
    of
        Resp ->
            exit(Resp)
    catch
        _:Error ->
            exit({error, Error})
    end.


%% cache functions

insert(Cache, WrappedKey, DbKey) ->
    Entry = #entry{id = WrappedKey, key = DbKey},
    true = ets:insert(Cache, Entry),
    ok.


lookup(Cache, WrappedKey) ->
    case ets:lookup(Cache, WrappedKey) of
        [#entry{id = WrappedKey, key = DbKey}] ->
            {ok, DbKey};
        [] ->
            {error, not_found}
    end.
