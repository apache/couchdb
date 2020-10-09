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


-module(fabric2_active_tasks).


-export([
    get_active_tasks/0,
    get_active_task_info/1,

    update_active_task_info/2
]).


-define(ACTIVE_TASK_INFO, <<"active_task_info">>).


get_active_tasks() ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(undefined), fun(JTx) ->
        Types = couch_jobs:get_types(JTx),
        lists:foldl(fun(Type, TaskAcc) ->
            JobIds = couch_jobs:get_active_jobs_ids(JTx, Type),
            Tasks = lists:filtermap(fun(JobId) ->
                {ok, Data} = couch_jobs:get_job_data(JTx, Type, JobId),
                case maps:get(?ACTIVE_TASK_INFO, Data, not_found) of
                    not_found -> false;
                    #{} = Map when map_size(Map) == 0 -> false;
                    #{} = Info -> {true, Info}
                end
            end, JobIds),
            TaskAcc ++ Tasks
        end, [], Types)
    end).


get_active_task_info(JobData) ->
    #{?ACTIVE_TASK_INFO:= ActiveTaskInfo} = JobData,
    ActiveTaskInfo.


update_active_task_info(JobData, ActiveTaskInfo) ->
    JobData#{?ACTIVE_TASK_INFO => ActiveTaskInfo}.
