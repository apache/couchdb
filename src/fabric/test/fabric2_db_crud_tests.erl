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


-define(TDEF(A), {atom_to_list(A), fun A/0}).


crud_test_() ->
    {
        "Test database CRUD operations",
        {
            setup,
            fun() -> test_util:start_couch([fabric]) end,
            fun test_util:stop_couch/1,
            [
                ?TDEF(create_db),
                ?TDEF(open_db),
                ?TDEF(delete_db)
            ]
        }
    }.


create_db() ->
    DbName = ?tempdb(),
    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(true, ets:member(fabric2_server, DbName)),
    ?assertEqual({error, file_exists}, fabric2_db:create(DbName, [])).


open_db() ->
    DbName = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:open(DbName, [])),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(true, ets:member(fabric2_server, DbName)),

    % Opening the cached version
    ?assertMatch({ok, _}, fabric2_db:open(DbName, [])),

    % Remove from cache and re-open
    true = ets:delete(fabric2_server, DbName),
    ?assertMatch({ok, _}, fabric2_db:open(DbName, [])).


delete_db() ->
    DbName = ?tempdb(),
    ?assertError(database_does_not_exist, fabric2_db:delete(DbName, [])),

    ?assertMatch({ok, _}, fabric2_db:create(DbName, [])),
    ?assertEqual(true, ets:member(fabric2_server, DbName)),

    ?assertEqual(ok, fabric2_db:delete(DbName, [])),
    ?assertEqual(false, ets:member(fabric2_server, DbName)),

    ?assertError(database_does_not_exist, fabric2_db:open(DbName, [])).
