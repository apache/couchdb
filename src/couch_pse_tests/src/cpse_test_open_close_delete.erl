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

-module(cpse_test_open_close_delete).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").


cet_open_non_existent() ->
    % Try twice to check that a failed open doesn't create
    % the database for some reason.
    DbName = cpse_util:dbname(),
    ?assertEqual({not_found, no_db_file}, cpse_util:open_db(DbName)),
    ?assertEqual({not_found, no_db_file}, cpse_util:open_db(DbName)).


cet_open_create() ->
    DbName = cpse_util:dbname(),

    ?assertEqual(false, couch_server:exists(DbName)),
    ?assertEqual({not_found, no_db_file}, cpse_util:open_db(DbName)),
    ?assertMatch({ok, _}, cpse_util:create_db(DbName)),
    ?assertEqual(true, couch_server:exists(DbName)).


cet_open_when_exists() ->
    DbName = cpse_util:dbname(),

    ?assertEqual(false, couch_server:exists(DbName)),
    ?assertEqual({not_found, no_db_file}, cpse_util:open_db(DbName)),
    ?assertMatch({ok, _}, cpse_util:create_db(DbName)),
    ?assertEqual(file_exists, cpse_util:create_db(DbName)).


cet_terminate() ->
    DbName = cpse_util:dbname(),

    ?assertEqual(false, couch_server:exists(DbName)),
    ?assertEqual({not_found, no_db_file}, cpse_util:open_db(DbName)),
    ?assertEqual(ok, cycle_db(DbName, create_db)),
    ?assertEqual(true, couch_server:exists(DbName)).


cet_rapid_recycle() ->
    DbName = cpse_util:dbname(),

    ?assertEqual(ok, cycle_db(DbName, create_db)),
    lists:foreach(fun(_) ->
        ?assertEqual(ok, cycle_db(DbName, open_db))
    end, lists:seq(1, 100)).


cet_delete() ->
    DbName = cpse_util:dbname(),

    ?assertEqual(false, couch_server:exists(DbName)),
    ?assertMatch(ok, cycle_db(DbName, create_db)),
    ?assertEqual(true, couch_server:exists(DbName)),
    ?assertEqual(ok, couch_server:delete(DbName, [])),
    ?assertEqual(false, couch_server:exists(DbName)).


cycle_db(DbName, Type) ->
    {ok, Db} = cpse_util:Type(DbName),
    cpse_util:shutdown_db(Db).
