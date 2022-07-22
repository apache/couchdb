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

-module(ddoc_cache_open_test).

-export([
    dbname/1,
    ddocid/1,
    recover/1,
    insert/2
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ddoc_cache_test.hrl").

%% behaviour callbacks
dbname(DbName) ->
    DbName.

ddocid(_) ->
    no_ddocid.

recover({deleted, _DbName}) ->
    erlang:error(database_does_not_exist);
recover(DbName) ->
    ddoc_cache_entry_validation_funs:recover(DbName).

insert(_, _) ->
    ok.

start_couch() ->
    Ctx = ddoc_cache_tutil:start_couch(),
    meck:new(ddoc_cache_entry_validation_funs, [passthrough]),
    meck:expect(
        ddoc_cache_entry_validation_funs,
        recover,
        ['_'],
        meck:passthrough()
    ),
    Ctx.

stop_couch(Ctx) ->
    meck:unload(),
    ddoc_cache_tutil:stop_couch(Ctx).

check_open_error_test_() ->
    {
        setup,
        fun start_couch/0,
        fun stop_couch/1,
        ddoc_cache_tutil:with([
            {"should_return_database_does_not_exist", fun should_return_database_does_not_exist/1},
            {"should_not_call_recover_when_database_does_not_exist",
                fun should_not_call_recover_when_database_does_not_exist/1},
            {"should_call_recover_when_needed", fun should_call_recover_when_needed/1},
            {"should_call_recover_when_needed", fun should_not_crash_lru_process/1}
        ])
    }.

should_return_database_does_not_exist({DbName, _}) ->
    ?assertError(
        database_does_not_exist,
        ddoc_cache_lru:open({?MODULE, {deleted, DbName}})
    ).

should_not_call_recover_when_database_does_not_exist({DbName, _}) ->
    meck:reset(ddoc_cache_entry_validation_funs),
    ?assertError(
        database_does_not_exist,
        ddoc_cache_lru:open({?MODULE, {deleted, DbName}})
    ),
    ?assertError(
        timeout,
        meck:wait(1, ddoc_cache_entry_validation_funs, recover, '_', 100)
    ).

should_call_recover_when_needed({DbName, _}) ->
    meck:reset(ddoc_cache_entry_validation_funs),
    ddoc_cache_lru:open({?MODULE, DbName}),
    ?assertEqual(
        ok,
        meck:wait(1, ddoc_cache_entry_validation_funs, recover, '_', 500)
    ).

should_not_crash_lru_process({DbName, _}) ->
    LRUPid = whereis(ddoc_cache_lru),
    ?assert(is_process_alive(LRUPid)),
    ?assertError(
        database_does_not_exist,
        ddoc_cache_lru:open({?MODULE, {deleted, DbName}})
    ),
    ?assert(is_process_alive(LRUPid)).
