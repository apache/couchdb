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

-module(fabric2_active_tasks_tests).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include("fabric2_test.hrl").


-define(JOB_TYPE, <<"fabric2_active_tasks_tests_type">>).
-define(JOB_ID, <<"job_id">>).


active_tasks_test_() ->
    {
        "Test cleanup of stale indices",
        {
            setup,
            fun setup_all/0,
            fun cleanup_all/1,
            {
                foreach,
                fun setup/0,
                fun cleanup/1,
                [
                    ?TDEF_FE(no_active_tasks_defined),
                    ?TDEF_FE(empty_map_info),
                    ?TDEF_FE(can_read_active_tasks),
                    ?TDEF_FE(only_running_tasks_appear)
                ]
            }
        }
    }.


setup_all() ->
    Ctx = test_util:start_couch([fabric, couch_jobs]),
    couch_jobs:set_type_timeout(?JOB_TYPE, 5000),
    meck:new(couch_jobs, [passthrough]),
    meck:expect(couch_jobs, get_types, 1, [?JOB_TYPE]),
    Ctx.


cleanup_all(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).


setup() ->
    ok = couch_jobs:add(undefined, ?JOB_TYPE, ?JOB_ID, #{}),
    ok.


cleanup(_) ->
    meck:reset(couch_jobs),
    couch_jobs:remove(undefined, ?JOB_TYPE, ?JOB_ID).


no_active_tasks_defined(_) ->
    {ok, Job1, #{}} = couch_jobs:accept(?JOB_TYPE),
    ?assertEqual([], fabric2_active_tasks:get_active_tasks()),
    ok = couch_jobs:finish(undefined, Job1).


empty_map_info(_) ->
    {ok, Job1, Data} = couch_jobs:accept(?JOB_TYPE),

    Data1 = fabric2_active_tasks:update_active_task_info(Data, #{}),
    {ok, Job2} = couch_jobs:update(undefined, Job1, Data1),
    ?assertEqual([], fabric2_active_tasks:get_active_tasks()),
    ok = couch_jobs:finish(undefined, Job2).


can_read_active_tasks(_) ->
    {ok, Job1, Data} = couch_jobs:accept(?JOB_TYPE),

    Info = #{<<"x">> => 1},
    Data1 = fabric2_active_tasks:update_active_task_info(Data, Info),
    {ok, Job2} = couch_jobs:update(undefined, Job1, Data1),
    ?assertEqual([#{<<"x">> => 1}], fabric2_active_tasks:get_active_tasks()),

    Info1 = fabric2_active_tasks:get_active_task_info(Data1),
    Info2 = Info1#{<<"y">> => 2},
    Data2 = fabric2_active_tasks:update_active_task_info(Data1, Info2),
    {ok, Job3} = couch_jobs:update(undefined, Job2, Data2),
    ?assertEqual([#{<<"x">> => 1, <<"y">> => 2}],
        fabric2_active_tasks:get_active_tasks()),
    ok = couch_jobs:finish(undefined, Job3).


only_running_tasks_appear(_) ->
    {ok, Job1, Data} = couch_jobs:accept(?JOB_TYPE),

    Info = #{<<"x">> => 1},
    Data1 = fabric2_active_tasks:update_active_task_info(Data, Info),
    {ok, Job2} = couch_jobs:update(undefined, Job1, Data1),

    ?assertEqual([#{<<"x">> => 1}], fabric2_active_tasks:get_active_tasks()),
    {ok, _} = couch_jobs:resubmit(undefined, Job2),

    ok = couch_jobs:finish(undefined, Job2),

    ?assertEqual([], fabric2_active_tasks:get_active_tasks()),
    {ok, Job3, #{}} = couch_jobs:accept(?JOB_TYPE),
    ?assertEqual([#{<<"x">> => 1}], fabric2_active_tasks:get_active_tasks()),

    ok = couch_jobs:finish(undefined, Job3),
    ?assertEqual([], fabric2_active_tasks:get_active_tasks()).
