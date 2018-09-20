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

-module(couch_bt_engine_downgrade_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


setup() ->
    Ctx = test_util:start_couch(),
    DbDir = config:get("couchdb", "database_dir"),
    DbFileNames = [
        "db_with_v6_and_0_purge_req.couch",
        "db_with_v7_and_0_purge_req.couch",
        "db_with_v7_and_1_purge_req.couch",
        "db_with_v7_and_2_purge_req.couch"
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
        "Couch Bt Engine downgrade tests",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            [
                t_no_downgrade_without_purge_req(),
                t_downgrade_without_purge_req(),
                t_downgrade_with_1_purge_req(),
                t_downgrade_with_2_purge_req()
            ]
        }
    }.


t_no_downgrade_without_purge_req() ->
    ?_test(begin
        % There are three documents in the fixture
        % db with zero purge entries
        DbName = <<"db_with_v6_and_0_purge_req">>,
        ?assertEqual(6, get_disk_version_from_header(DbName)),
        {ok, PurgeSeq} = couch_util:with_db(DbName, fun(Db) ->
            couch_db:get_purge_seq(Db)
        end),
        ?assertEqual(0, PurgeSeq),

        {ok, _} = save_doc(DbName, {[{<<"_id">>, <<"doc4">>}, {<<"v">>, 1}]}),
        {ok, _} = save_doc(DbName, {[{<<"_id">>, <<"doc5">>}, {<<"v">>, 2}]}),

        couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual({ok, 5}, couch_db:get_doc_count(Db)),
            ?assertEqual({ok, 0}, couch_db:get_purge_seq(Db)),
            ?assertEqual(6, couch_db_engine:get_disk_version(Db))
        end)

    end).

t_downgrade_without_purge_req() ->
    ?_test(begin
        % There are three documents in the fixture
        % db with zero purge entries
        DbName = <<"db_with_v7_and_0_purge_req">>,
        ?assertEqual(7, get_disk_version_from_header(DbName)),
        {ok, PurgeSeq} = couch_util:with_db(DbName, fun(Db) ->
            couch_db:get_purge_seq(Db)
        end),
        ?assertEqual(0, PurgeSeq),

        {ok, _} = save_doc(DbName, {[{<<"_id">>, <<"doc4">>}, {<<"v">>, 1}]}),
        {ok, _} = save_doc(DbName, {[{<<"_id">>, <<"doc5">>}, {<<"v">>, 2}]}),

        couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual({ok, 5}, couch_db:get_doc_count(Db)),
            ?assertEqual({ok, 0}, couch_db:get_purge_seq(Db)),
            ?assertEqual(6, couch_db_engine:get_disk_version(Db))
        end)
    end).


t_downgrade_with_1_purge_req() ->
    ?_test(begin
        % There are two documents in the fixture database
        % with a single purge entry
        DbName = <<"db_with_v7_and_1_purge_req">>,
        ?assertEqual(7, get_disk_version_from_header(DbName)),
        {ok, PurgeReq} = couch_util:with_db(DbName, fun(Db) ->
            couch_db:get_purge_seq(Db)
        end),
        ?assertEqual(3, PurgeReq),

        {ok, _} = save_doc(DbName, {[{<<"_id">>, <<"doc4">>}, {<<"v">>, 1}]}),
        {ok, _} = save_doc(DbName, {[{<<"_id">>, <<"doc5">>}, {<<"v">>, 2}]}),

        couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual({ok, 4}, couch_db:get_doc_count(Db)),
            ?assertEqual({ok, 3}, couch_db:get_purge_seq(Db)),
            ?assertEqual(6, couch_db_engine:get_disk_version(Db))
        end)
    end).


t_downgrade_with_2_purge_req() ->
    ?_test(begin
        % There is one document in the fixture database
        % with two docs that have been purged
        DbName = <<"db_with_v7_and_2_purge_req">>,
        ?assertEqual(7, get_disk_version_from_header(DbName)),
        {ok, PurgeReq} = couch_util:with_db(DbName, fun(Db) ->
            couch_db:get_purge_seq(Db)
        end),
        ?assertEqual(4, PurgeReq),

        {ok, _} = save_doc(DbName, {[{<<"_id">>, <<"doc4">>}, {<<"v">>, 1}]}),
        {ok, _} = save_doc(DbName, {[{<<"_id">>, <<"doc5">>}, {<<"v">>, 2}]}),

        couch_util:with_db(DbName, fun(Db) ->
            ?assertEqual({ok, 3}, couch_db:get_doc_count(Db)),
            ?assertEqual({ok, 4}, couch_db:get_purge_seq(Db)),
            ?assertEqual(6, couch_db_engine:get_disk_version(Db))
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
