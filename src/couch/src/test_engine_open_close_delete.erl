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

-module(test_engine_open_close_delete).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").


cet_open_non_existent() ->
    Engine = test_engine_util:get_engine(),
    DbPath = test_engine_util:dbpath(),

    ?assertEqual(false, Engine:exists(DbPath)),
    ?assertThrow({not_found, no_db_file}, Engine:init(DbPath, [])),
    ?assertEqual(false, Engine:exists(DbPath)).


cet_open_create() ->
    process_flag(trap_exit, true),
    Engine = test_engine_util:get_engine(),
    DbPath = test_engine_util:dbpath(),

    ?assertEqual(false, Engine:exists(DbPath)),
    ?assertMatch({ok, _}, Engine:init(DbPath, [create])),
    ?assertEqual(true, Engine:exists(DbPath)).


cet_open_when_exists() ->
    Engine = test_engine_util:get_engine(),
    DbPath = test_engine_util:dbpath(),

    ?assertEqual(false, Engine:exists(DbPath)),
    ?assertMatch({ok, _}, Engine:init(DbPath, [create])),
    ?assertThrow({error, eexist}, Engine:init(DbPath, [create])).


cet_terminate() ->
    Engine = test_engine_util:get_engine(),
    DbPath = test_engine_util:dbpath(),

    ?assertEqual(false, Engine:exists(DbPath)),
    {ok, St} = Engine:init(DbPath, [create]),
    Engine:terminate(normal, St),
    ?assertEqual(true, Engine:exists(DbPath)).


cet_rapid_recycle() ->
    Engine = test_engine_util:get_engine(),
    DbPath = test_engine_util:dbpath(),

    {ok, St0} = Engine:init(DbPath, [create]),
    Engine:terminate(normal, St0),

    lists:foreach(fun(_) ->
        {ok, St1} = Engine:init(DbPath, []),
        Engine:terminate(normal, St1)
    end, lists:seq(1, 100)).


cet_delete() ->
    Engine = test_engine_util:get_engine(),
    RootDir = test_engine_util:rootdir(),
    DbPath = test_engine_util:dbpath(),

    ?assertEqual(false, Engine:exists(DbPath)),
    {ok, St} = Engine:init(DbPath, [create]),
    Engine:terminate(normal, St),
    ?assertEqual(true, Engine:exists(DbPath)),
    ?assertEqual(ok, Engine:delete(RootDir, DbPath, [async])),
    ?assertEqual(false, Engine:exists(DbPath)).
