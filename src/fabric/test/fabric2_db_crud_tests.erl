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

-module(fabric2_db_crud_tests).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fabric2_test.hrl").


crud_test_() ->
    {
        "Test database CRUD operations",
        {
            setup,
            fun() -> test_util:start_couch([fabric]) end,
            fun test_util:stop_couch/1,
            with([
                ?TDEF(create_db),
                ?TDEF(open_db),
                ?TDEF(delete_db),
                ?TDEF(list_dbs),
                ?TDEF(list_dbs_info)
            ])
        }
    }.


create_db(_) ->
    DbName = ?tempdb(),
    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(true, ets:member(fabric2_server, DbName)),
    ?assertEqual({error, file_exists}, fabric2_db:create(DbName, [])).


open_db(_) ->
    DbName = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:open(DbName, [])),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(true, ets:member(fabric2_server, DbName)),

    % Opening the cached version
    ?assertMatch({ok, _}, fabric2_db:open(DbName, [])),

    % Remove from cache and re-open
    true = ets:delete(fabric2_server, DbName),
    ?assertMatch({ok, _}, fabric2_db:open(DbName, [])).


delete_db(_) ->
    DbName = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:delete(DbName, [])),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(true, ets:member(fabric2_server, DbName)),

    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    ?assertEqual(false, ets:member(fabric2_server, DbName)),

    ?assertError(database_does_not_exist, fabric2_db:open(DbName, [])).


list_dbs(_) ->
    DbName = ?tempdb(),
    AllDbs1 = fabric2_db:list_dbs(),

    ?assert(is_list(AllDbs1)),
    ?assert(not lists:member(DbName, AllDbs1)),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    AllDbs2 = fabric2_db:list_dbs(),
    ?assert(lists:member(DbName, AllDbs2)),

    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    AllDbs3 = fabric2_db:list_dbs(),
    ?assert(not lists:member(DbName, AllDbs3)).


list_dbs_info(_) ->
    DbName = ?tempdb(),
    {ok, AllDbInfos1} = fabric2_db:list_dbs_info(),

    ?assert(is_list(AllDbInfos1)),
    ?assert(not is_db_info_member(DbName, AllDbInfos1)),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    {ok, AllDbInfos2} = fabric2_db:list_dbs_info(),
    ?assert(is_db_info_member(DbName, AllDbInfos2)),

    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    {ok, AllDbInfos3} = fabric2_db:list_dbs_info(),
    ?assert(not is_db_info_member(DbName, AllDbInfos3)).


is_db_info_member(_, []) ->
    false;

is_db_info_member(DbName, [DbInfo | RestInfos]) ->
    case lists:keyfind(db_name, 1, DbInfo) of
        {db_name, DbName} ->
            true;
        _E ->
            is_db_info_member(DbName, RestInfos)
    end.
