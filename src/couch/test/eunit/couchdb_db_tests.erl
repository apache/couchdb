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

-module(couchdb_db_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").

setup() ->
    DbName = ?b2l(?tempdb()),
    fabric:create_db(DbName),
    DbName.

teardown(DbName) ->
    (catch fabric:delete_db(DbName)),
    ok.

clustered_db_test_() ->
    {
        "Checking clustered db API",
        {
            setup,
            fun() -> test_util:start_couch([ddoc_cache, mem3]) end,
            fun test_util:stop/1,
            [
                {
                    "DB deletion",
                    {
                        foreach,
                        fun setup/0,
                        fun teardown/1,
                        [
                            fun should_close_deleted_db/1,
                            fun should_kill_caller_from_load_validation_funs_for_deleted_db/1
                        ]
                    }
                }
            ]
        }
    }.

should_close_deleted_db(DbName) ->
    ?_test(begin
        [#shard{name = ShardName} | _] = mem3:shards(DbName),
        {ok, Db} = couch_db:open(ShardName, []),

        MonitorRef = couch_db:monitor(Db),
        fabric:delete_db(DbName),
        receive
            {'DOWN', MonitorRef, _Type, _Pid, _Info} ->
                ok
        after 2000 ->
            throw(timeout_error)
        end,
        test_util:wait(fun() ->
            case ets:lookup(couch_server:couch_dbs(DbName), DbName) of
                [] -> ok;
                _ -> wait
            end
        end),
        ?assertEqual([], ets:lookup(couch_server:couch_dbs(DbName), DbName))
    end).

should_kill_caller_from_load_validation_funs_for_deleted_db(DbName) ->
    ?_test(begin
        [#shard{name = ShardName} | _] = mem3:shards(DbName),
        {ok, Db} = couch_db:open(ShardName, []),

        MonitorRef = couch_db:monitor(Db),
        fabric:delete_db(DbName),
        receive
            {'DOWN', MonitorRef, _Type, _Pid, _Info} ->
                ok
        after 2000 ->
            throw(timeout_error)
        end,
        ?assertError(database_does_not_exist, couch_db:load_validation_funs(Db))
    end).
