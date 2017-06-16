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

-module(ddoc_cache_entry).
-behaviour(gen_server).
-vsn(1).


-export([
    dbname/1,
    ddocid/1,
    recover/1,
    insert/2,

    start_link/2,
    shutdown/1,
    open/2,
    accessed/1,
    refresh/1
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
    do_open/1
]).


-include("ddoc_cache.hrl").


-ifndef(TEST).
-define(ENTRY_SHUTDOWN_TIMEOUT, 5000).
-else.
-define(ENTRY_SHUTDOWN_TIMEOUT, 500).
-endif.


-record(st, {
    key,
    val,
    opener,
    waiters,
    ts,
    accessed
}).


dbname({Mod, Arg}) ->
    Mod:dbname(Arg).


ddocid({Mod, Arg}) ->
    Mod:ddocid(Arg).


recover({Mod, Arg}) ->
    Mod:recover(Arg).


insert({Mod, Arg}, Value) ->
    Mod:insert(Arg, Value).


start_link(Key, Default) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [{Key, Default}]),
    {ok, Pid}.


shutdown(Pid) ->
    Ref = erlang:monitor(process, Pid),
    ok = gen_server:cast(Pid, shutdown),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            ok;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:exit(Reason)
    after ?ENTRY_SHUTDOWN_TIMEOUT ->
        erlang:demonitor(Ref, [flush]),
        erlang:exit({timeout, {entry_shutdown, Pid}})
    end.


open(Pid, Key) ->
    try
        Resp = gen_server:call(Pid, open),
        case Resp of
            {open_ok, Val} ->
                Val;
            {open_error, {T, R, S}} ->
                erlang:raise(T, R, S)
        end
    catch exit:_ ->
        % Its possible that this process was evicted just
        % before we tried talking to it. Just fallback
        % to a standard recovery
        recover(Key)
    end.


accessed(Pid) ->
    gen_server:cast(Pid, accessed).


refresh(Pid) ->
    gen_server:cast(Pid, force_refresh).


init({Key, undefined}) ->
    true = ets:update_element(?CACHE, Key, {#entry.pid, self()}),
    St = #st{
        key = Key,
        opener = spawn_opener(Key),
        waiters = [],
        accessed = 1
    },
    ?EVENT(started, Key),
    gen_server:enter_loop(?MODULE, [], St);

init({Key, Wrapped}) ->
    Default = ddoc_cache_value:unwrap(Wrapped),
    Updates = [
        {#entry.val, Default},
        {#entry.pid, self()}
    ],
    NewTs = os:timestamp(),
    true = ets:update_element(?CACHE, Key, Updates),
    true = ets:insert(?LRU, {{NewTs, Key, self()}}),
    St = #st{
        key = Key,
        val = {open_ok, {ok, Default}},
        opener = start_timer(),
        waiters = [],
        ts = NewTs,
        accessed = 1
    },
    ?EVENT(default_started, Key),
    gen_server:enter_loop(?MODULE, [], St, hibernate).


terminate(_Reason, St) ->
    #st{
        key = Key,
        opener = Pid,
        ts = Ts
    } = St,
    % We may have already deleted our cache entry
    % during shutdown
    Pattern = #entry{key = Key, pid = self(), _ = '_'},
    CacheMSpec = [{Pattern, [], [true]}],
    true = ets:select_delete(?CACHE, CacheMSpec) < 2,
    % We may have already deleted our LRU entry
    % during shutdown
    if Ts == undefined -> ok; true ->
        LruMSpec = [{{{Ts, Key, self()}}, [], [true]}],
        true = ets:select_delete(?LRU, LruMSpec) < 2
    end,
    % Blow away any current opener if it exists
    if not is_pid(Pid) -> ok; true ->
        catch exit(Pid, kill)
    end,
    ok.


handle_call(open, From, #st{opener = Pid} = St) when is_pid(Pid) ->
    NewSt = St#st{
        waiters = [From | St#st.waiters]
    },
    {noreply, NewSt};

handle_call(open, _From, St) ->
    {reply, St#st.val, St};

handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(accessed, St) ->
    ?EVENT(accessed, St#st.key),
    drain_accessed(),
    NewSt = St#st{
        accessed = St#st.accessed + 1
    },
    {noreply, update_lru(NewSt)};

handle_cast(force_refresh, St) ->
    % If we had frequent design document updates
    % they could end up racing accessed events and
    % end up prematurely evicting this entry from
    % cache. To prevent this we just make sure that
    % accessed is set to at least 1 before we
    % execute a refresh.
    NewSt = if St#st.accessed > 0 -> St; true ->
        St#st{accessed = 1}
    end,
    % We remove the cache entry value so that any
    % new client comes to us for the refreshed
    % value.
    true = ets:update_element(?CACHE, St#st.key, {#entry.val, undefined}),
    handle_cast(refresh, NewSt);

handle_cast(refresh, #st{accessed = 0} = St) ->
    {stop, normal, St};

handle_cast(refresh, #st{opener = Ref} = St) when is_reference(Ref) ->
    #st{
        key = Key
    } = St,
    erlang:cancel_timer(Ref),
    NewSt = St#st{
        opener = spawn_opener(Key),
        accessed = 0
    },
    {noreply, NewSt};

handle_cast(refresh, #st{opener = Pid} = St) when is_pid(Pid) ->
    catch exit(Pid, kill),
    receive
        {'DOWN', _, _, Pid, _} -> ok
    end,
    NewSt = St#st{
        opener = spawn_opener(St#st.key),
        accessed = 0
    },
    {noreply, NewSt};

handle_cast(shutdown, St) ->
    remove_from_cache(St),
    {stop, normal, St};

handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info({'DOWN', _, _, Pid, Resp}, #st{key = Key, opener = Pid} = St) ->
    case Resp of
        {open_ok, Key, {ok, Val}} ->
            update_cache(St, Val),
            NewSt1 = St#st{
                val = {open_ok, {ok, Val}},
                opener = start_timer(),
                waiters = []
            },
            NewSt2 = update_lru(NewSt1),
            respond(St#st.waiters, {open_ok, {ok, Val}}),
            {noreply, NewSt2};
        {Status, Key, Other} ->
            NewSt = St#st{
                val = {Status, Other},
                opener = undefined,
                waiters = undefined
            },
            remove_from_cache(NewSt),
            respond(St#st.waiters, {Status, Other}),
            {stop, normal, NewSt}
    end;

handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_, St, _) ->
    {ok, St}.


spawn_opener(Key) ->
    {Pid, _} = erlang:spawn_monitor(?MODULE, do_open, [Key]),
    Pid.


start_timer() ->
    TimeOut = config:get_integer(
            "ddoc_cache", "refresh_timeout", ?REFRESH_TIMEOUT),
    erlang:send_after(TimeOut, self(), {'$gen_cast', refresh}).


do_open(Key) ->
    try recover(Key) of
        Resp ->
            erlang:exit({open_ok, Key, Resp})
    catch T:R ->
        S = erlang:get_stacktrace(),
        erlang:exit({open_error, Key, {T, R, S}})
    end.


update_lru(#st{key = Key, ts = Ts} = St) ->
    remove_from_lru(Ts, Key),
    NewTs = os:timestamp(),
    true = ets:insert(?LRU, {{NewTs, Key, self()}}),
    St#st{ts = NewTs}.


update_cache(#st{val = undefined} = St, Val) ->
    true = ets:update_element(?CACHE, St#st.key, {#entry.val, Val}),
    ?EVENT(inserted, St#st.key);

update_cache(#st{val = V1} = _St, V2) when {open_ok, {ok, V2}} == V1 ->
    ?EVENT(update_noop, _St#st.key);

update_cache(St, Val) ->
    true = ets:update_element(?CACHE, St#st.key, {#entry.val, Val}),
    ?EVENT(updated, {St#st.key, Val}).


remove_from_cache(St) ->
    #st{
        key = Key,
        ts = Ts
    } = St,
    Pattern = #entry{key = Key, pid = self(), _ = '_'},
    CacheMSpec = [{Pattern, [], [true]}],
    1 = ets:select_delete(?CACHE, CacheMSpec),
    remove_from_lru(Ts, Key),
    ?EVENT(removed, St#st.key),
    ok.


remove_from_lru(Ts, Key) ->
    if Ts == undefined -> ok; true ->
        LruMSpec = [{{{Ts, Key, self()}}, [], [true]}],
        1 = ets:select_delete(?LRU, LruMSpec)
    end.


drain_accessed() ->
    receive
        {'$gen_cast', accessed} ->
            drain_accessed()
    after 0 ->
        ok
    end.


respond(Waiters, Resp) ->
    [gen_server:reply(W, Resp) || W <- Waiters].
