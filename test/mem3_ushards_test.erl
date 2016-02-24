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

-module(mem3_ushards_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").

-define(TIMEOUT, 10000).

setup() ->
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [?ADMIN_CTX]),
    DbName.

teardown(DbName) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]).

ushard_test_() ->
    {
        "Maintenance node test",
        {
            setup,
            fun() -> test_util:start_couch([mem3]) end,
            fun test_util:stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun maintenance_mode_true/1,
                    fun maintenance_mode_nolb/1
                ]
            }
        }
    }.

maintenance_mode_true(DbName) ->
    config:set("couchdb", "maintenance_mode", "true"),
    UShards = mem3:ushards(DbName),
    Nodes = [Node || #shard{node=Node} <- UShards, Node =:= node()],
    ?_assertEqual([], Nodes).

maintenance_mode_nolb(DbName) ->
    config:set("couchdb", "maintenance_mode", "nolb"),
    UShards = mem3:ushards(DbName),
    Nodes = [Node || #shard{node=Node} <- UShards, Node =:= node()],
    ?_assertEqual([], Nodes).
