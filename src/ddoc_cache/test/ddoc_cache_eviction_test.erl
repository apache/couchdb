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

-module(ddoc_cache_eviction_test).


-export([
    recover/1
]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("mem3/include/mem3.hrl").
-include("ddoc_cache_test.hrl").


recover(DbName) ->
    {ok, {DbName, totes_custom}}.


start_couch() ->
    Ctx = ddoc_cache_tutil:start_couch(),
    meck:new(ddoc_cache_ev, [passthrough]),
    Ctx.


stop_couch(Ctx) ->
    meck:unload(),
    ddoc_cache_tutil:stop_couch(Ctx).


check_eviction_test_() ->
    {
        setup,
        fun start_couch/0,
        fun stop_couch/1,
        {with, [
            fun evict_all/1,
            fun dont_evict_all_unrelated/1,
            fun check_upgrade_clause/1
        ]}
    }.


evict_all({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    Rev = ddoc_cache_tutil:get_rev(DbName, ?FOOBAR),
    #shard{name = ShardName} = hd(mem3:shards(DbName)),
    {ok, _} = ddoc_cache:open_doc(DbName, ?FOOBAR),
    {ok, _} = ddoc_cache:open_doc(DbName, ?FOOBAR, Rev),
    {ok, _} = ddoc_cache:open_validation_funs(DbName),
    {ok, _} = ddoc_cache:open_custom(DbName, ?MODULE),
    ?assertEqual(4, ets:info(?CACHE, size)),
    {ok, _} = ddoc_cache_lru:handle_db_event(ShardName, deleted, foo),
    meck:wait(ddoc_cache_ev, event, [evicted, DbName], 1000),
    meck:wait(4, ddoc_cache_ev, event, [removed, '_'], 1000),
    ?assertEqual(0, ets:info(?CACHE, size)).


dont_evict_all_unrelated({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    Rev = ddoc_cache_tutil:get_rev(DbName, ?FOOBAR),
    {ok, _} = ddoc_cache:open_doc(DbName, ?FOOBAR),
    {ok, _} = ddoc_cache:open_doc(DbName, ?FOOBAR, Rev),
    {ok, _} = ddoc_cache:open_validation_funs(DbName),
    {ok, _} = ddoc_cache:open_custom(DbName, ?MODULE),
    ?assertEqual(4, ets:info(?CACHE, size)),
    ShardName = <<"shards/00000000-ffffffff/test.1384769918">>,
    {ok, _} = ddoc_cache_lru:handle_db_event(ShardName, deleted, foo),
    meck:wait(ddoc_cache_ev, event, [evict_noop, <<"test">>], 1000),
    ?assertEqual(4, ets:info(?CACHE, size)).


check_upgrade_clause({DbName, _}) ->
    ddoc_cache_tutil:clear(),
    meck:reset(ddoc_cache_ev),
    {ok, _} = ddoc_cache:open_doc(DbName, ?FOOBAR),
    meck:wait(ddoc_cache_ev, event, [started, '_'], 1000),
    meck:wait(ddoc_cache_ev, event, [default_started, '_'], 1000),
    ?assertEqual(2, ets:info(?CACHE, size)),
    gen_server:cast(ddoc_cache_opener, {do_evict, DbName}),
    meck:wait(ddoc_cache_ev, event, [evicted, DbName], 1000),
    meck:wait(2, ddoc_cache_ev, event, [removed, '_'], 1000),
    ?assertEqual(0, ets:info(?CACHE, size)).
