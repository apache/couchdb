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

-module(couch_db_props_upgrade_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

setup() ->
    DbName = <<"test">>,
    DbFileName = "test.couch",
    OldDbFilePath = filename:join([?FIXTURESDIR, DbFileName]),

    DbDir = config:get("couchdb", "database_dir"),
    NewDbFilePath = filename:join([DbDir, DbFileName]),

    file:delete(NewDbFilePath),
    {ok, _} = file:copy(OldDbFilePath, NewDbFilePath),

    DbName.

teardown(DbName) when is_binary(DbName) ->
    couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.

old_db_info_test_() ->
    {
        "Old database versions work",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun can_get_props/1,
                    fun can_get_db_info/1,
                    fun can_compact_db/1
                ]
            }
        }
    }.

can_get_props(DbName) ->
    ?_test(begin
        {ok, Db} = couch_db:open_int(DbName, []),
        Props = couch_db_engine:get_props(Db),
        ?assert(is_list(Props))
    end).

can_get_db_info(DbName) ->
    ?_test(begin
        {ok, Db} = couch_db:open_int(DbName, []),
        {ok, Info} = couch_db:get_db_info(Db),
        Props = couch_util:get_value(props, Info),
        ?assertEqual({[]}, Props)
    end).

can_compact_db(DbName) ->
    ?_test(begin
        couch_util:with_db(DbName, fun(Db) ->
            couch_db:start_compact(Db),
            couch_db:wait_for_compaction(Db)
        end)
    end).
