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

-module(ddoc_cache_lru_test).


-export([
    recover/1
]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ddoc_cache_test.hrl").


recover(<<"pause", _/binary>>) ->
    receive go -> ok end,
    {ok, paused};

recover(<<"big", _/binary>>) ->
    {ok, [random:uniform() || _ <- lists:seq(1, 8192)]};

recover(DbName) ->
    {ok, DbName}.


start_couch() ->
    Ctx = ddoc_cache_tutil:start_couch(),
    meck:new(ddoc_cache_ev, [passthrough]),
    Ctx.


stop_couch(Ctx) ->
    meck:unload(),
    ddoc_cache_tutil:stop_couch(Ctx).


check_not_started_test() ->
    % Starting couch, but not ddoc_cache
    Ctx = test_util:start_couch(),
    try
        Key = {ddoc_cache_entry_custom, {<<"dbname">>, ?MODULE}},
        ?assertEqual({ok, <<"dbname">>}, ddoc_cache_lru:open(Key))
    after
        test_util:stop_couch(Ctx)
    end.


check_lru_test_() ->
    {
        setup,
        fun start_couch/0,
        fun stop_couch/1,
        {with, [
            fun check_multi_start/1,
            fun check_multi_open/1,
            fun check_capped_size/1,
            fun check_cache_refill/1,
            fun check_evict_and_exit/1
        ]}
    }.


check_multi_start(_) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    Key = {ddoc_cache_entry_custom, {<<"pause">>, ?MODULE}},
    % These will all get sent through ddoc_cache_lru
    Clients = lists:map(fun(_) ->
        spawn_monitor(fun() ->
            ddoc_cache_lru:open(Key)
        end)
    end, lists:seq(1, 10)),
    meck:wait(ddoc_cache_ev, event, [started, Key], 1000),
    lists:foreach(fun({Pid, _Ref}) ->
        ?assert(is_process_alive(Pid))
    end, Clients),
    [#entry{pid = Pid}] = ets:tab2list(?CACHE),
    Opener = element(4, sys:get_state(Pid)),
    OpenerRef = erlang:monitor(process, Opener),
    ?assert(is_process_alive(Opener)),
    Opener ! go,
    receive {'DOWN', OpenerRef, _, _, _} -> ok end,
    lists:foreach(fun({_, Ref}) ->
        receive
            {'DOWN', Ref, _, _, normal} -> ok
        end
    end, Clients).


check_multi_open(_) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    Key = {ddoc_cache_entry_custom, {<<"pause">>, ?MODULE}},
    % We wait after the first client so that
    % the rest of the clients go directly to
    % ddoc_cache_entry bypassing ddoc_cache_lru
    Client1 = spawn_monitor(fun() ->
        ddoc_cache_lru:open(Key)
    end),
    meck:wait(ddoc_cache_ev, event, [started, Key], 1000),
    Clients = [Client1] ++ lists:map(fun(_) ->
        spawn_monitor(fun() ->
            ddoc_cache_lru:open(Key)
        end)
    end, lists:seq(1, 9)),
    lists:foreach(fun({Pid, _Ref}) ->
        ?assert(is_process_alive(Pid))
    end, Clients),
    [#entry{pid = Pid}] = ets:tab2list(?CACHE),
    Opener = element(4, sys:get_state(Pid)),
    OpenerRef = erlang:monitor(process, Opener),
    ?assert(is_process_alive(Opener)),
    Opener ! go,
    receive {'DOWN', OpenerRef, _, _, _} -> ok end,
    lists:foreach(fun({_, Ref}) ->
        receive {'DOWN', Ref, _, _, normal} -> ok end
    end, Clients).


check_capped_size(_) ->
    % The extra factor of two in the size checks is
    % a fudge factor. We don't reject entries from
    % the cache if they would put us over the limit
    % as we don't have the size information a
    % priori.
    config:set("ddoc_cache", "max_size", "1048576", false),
    MaxSize = 1048576,
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    lists:foreach(fun(I) ->
        DbName = list_to_binary("big_" ++ integer_to_list(I)),
        ddoc_cache:open_custom(DbName, ?MODULE),
        meck:wait(I, ddoc_cache_ev, event, [started, '_'], 1000),
        ?assert(cache_size() < MaxSize * 2)
    end, lists:seq(1, 25)),
    lists:foreach(fun(I) ->
        DbName = list_to_binary("big_" ++ integer_to_list(I)),
        ddoc_cache:open_custom(DbName, ?MODULE),
        meck:wait(I, ddoc_cache_ev, event, [started, '_'], 1000),
        ?assert(cache_size() < MaxSize * 2)
    end, lists:seq(26, 100)).


check_cache_refill({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),

    InitDDoc = fun(I) ->
        NumBin = list_to_binary(integer_to_list(I)),
        DDocId = <<"_design/", NumBin/binary>>,
        Doc = #doc{id = DDocId, body = {[]}},
        {ok, _} = fabric:update_doc(DbName, Doc, [?ADMIN_CTX]),
        {ok, _} = ddoc_cache:open_doc(DbName, DDocId),
        {ddoc_cache_entry_ddocid, {DbName, DDocId}}
    end,

    lists:foreach(fun(I) ->
        Key = InitDDoc(I),
        meck:wait(ddoc_cache_ev, event, [started, Key], 1000)
    end, lists:seq(1, 5)),

    ShardName = mem3:name(hd(mem3:shards(DbName))),
    {ok, _} = ddoc_cache_lru:handle_db_event(ShardName, deleted, foo),
    meck:wait(ddoc_cache_ev, event, [evicted, DbName], 1000),
    meck:wait(10, ddoc_cache_ev, event, [removed, '_'], 1000),
    ?assertEqual(0, ets:info(?CACHE, size)),

    lists:foreach(fun(I) ->
        Key = InitDDoc(I),
        meck:wait(ddoc_cache_ev, event, [started, Key], 1000)
    end, lists:seq(6, 10)).


check_evict_and_exit(_) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),

    Key = {ddoc_cache_entry_custom, {<<"dbname">>, ?MODULE}},
    ?assertEqual({ok, <<"dbname">>}, ddoc_cache_lru:open(Key)),
    [#entry{key = Key, pid = Pid}] = ets:tab2list(?CACHE),

    erlang:monitor(process, whereis(ddoc_cache_lru)),

    % Pause the LRU so we can queue multiple messages
    erlang:suspend_process(whereis(ddoc_cache_lru)),

    gen_server:cast(ddoc_cache_lru, {do_evict, <<"dbname">>}),
    whereis(ddoc_cache_lru) ! {'EXIT', Pid, normal},

    % Resume the LRU and ensure that it doesn't die
    erlang:resume_process(whereis(ddoc_cache_lru)),

    meck:wait(ddoc_cache_ev, event, [evicted, <<"dbname">>], 1000),

    % Make sure it can handle another message
    OtherKey = {ddoc_cache_entry_custom, {<<"otherdb">>, ?MODULE}},
    ?assertEqual({ok, <<"otherdb">>}, ddoc_cache_lru:open(OtherKey)),

    % And verify our monitor doesn't fire
    timer:sleep(500),
    ?assertEqual({messages, []}, process_info(self(), messages)).


cache_size() ->
    ets:info(?CACHE, memory) * erlang:system_info(wordsize).
