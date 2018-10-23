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

-module(couch_bt_engine_partition_downgrade_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


setup() ->
    Ctx = test_util:start_couch(),
    DbDir = config:get("couchdb", "database_dir"),
    DbFileNames = [
        "db_non_partitioned.couch"
    ],
    NewPaths = lists:map(fun(DbFileName) ->
        OldDbFilePath = filename:join([?FIXTURESDIR, DbFileName]),
        NewDbFilePath = filename:join([DbDir, DbFileName]),
        ok = filelib:ensure_dir(NewDbFilePath),
        file:delete(NewDbFilePath),
        {ok, _} = file:copy(OldDbFilePath, NewDbFilePath),
        NewDbFilePath
    end, DbFileNames),
    {Ctx, NewPaths}.


teardown({Ctx, Paths}) ->
    test_util:stop_couch(Ctx),
    lists:foreach(fun(Path) ->
        file:delete(Path)
    end, Paths).


downgrade_test_() ->
    {
        "Couch Bt Engine partition downgrade tests",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            [
                t_downgrade_non_partitioned_db()
            ]
        }
    }.


t_downgrade_non_partitioned_db() ->
    ?_test(begin
        % There are 13 documents in the fixture with 1 conflicted
        DbName = <<"db_non_partitioned">>,
        ?assertEqual(8, get_disk_version_from_header(DbName)),

        {ok, _} = save_doc(DbName, {[{<<"_id">>, <<"doc4">>}, {<<"v">>, 1}]}),
        {ok, _} = save_doc(DbName, {[{<<"_id">>, <<"doc5">>}, {<<"v">>, 2}]}),

        couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual(7, couch_db_engine:get_disk_version(Db))
        end)
    end).


get_disk_version_from_header(DbFileName) ->
    DbDir = config:get("couchdb", "database_dir"),
    DbFilePath = filename:join([DbDir, ?l2b(?b2l(DbFileName) ++ ".couch")]),
    {ok, Fd} = couch_file:open(DbFilePath, []),
    {ok, Header} = couch_file:read_header(Fd),
    DiskVerison = couch_bt_engine_header:disk_version(Header),
    couch_file:close(Fd),
    DiskVerison.


save_doc(DbName, Json) ->
    Doc = couch_doc:from_json_obj(Json),
    couch_util:with_db(DbName, fun(Db) ->
        couch_db:update_doc(Db, Doc, [])
    end).
