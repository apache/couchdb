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

-module(couch_replicator_jobs).


-export([
    % couch_jobs type timeouts
    set_timeout/0,
    get_timeout/0,

    % Job creation and querying
    new_job/7,
    add_job/3,
    remove_job/2,
    get_job_data/2,
    fold_jobs/3,
    pending_count/2,

    % Job subscription
    wait_running/1,
    wait_result/1,

    % Job execution
    accept_job/1,
    update_job_data/3,
    finish_job/3,
    reschedule_job/4,

    % (..., ?REPLICATION_IDS) -> JobId handling
    try_update_rep_id/3,
    update_rep_id/3,
    clear_old_rep_id/3,
    get_job_id/2,

    % Debug functions
    remove_jobs/2,
    get_job_ids/1
]).


-include("couch_replicator.hrl").
-include_lib("fabric/include/fabric2.hrl").


-define(REP_JOBS, <<"rep_jobs">>).
-define(REP_JOBS_TIMEOUT_SEC, 61).


% Data model
% ----------
%
% State kept in couch_jobs under the ?REP_JOBS type
%
% Job IDs are defined as:
%   * Replicator DB instance UUID + doc ID for persistent replications
%   * Hash(username|source|target|options) for transient replications
%
% To map replication IDs to couch_job jobs, there is a separate index that
% looks like:
%   (?REPLICATION_IDS, RepId) -> JobId
%

set_timeout() ->
    couch_jobs:set_type_timeout(?REP_JOBS, ?REP_JOBS_TIMEOUT_SEC).


get_timeout() ->
    ?REP_JOBS_TIMEOUT_SEC.


new_job(#{} = Rep, DbName, DbUUID, DocId, State, StateInfo, DocState) ->
    NowSec = erlang:system_time(second),
    AddedEvent = #{?HIST_TYPE => ?HIST_ADDED, ?HIST_TIMESTAMP => NowSec},
    #{
        ?REP => Rep,
        ?REP_ID => null,
        ?BASE_ID => null,
        ?DB_NAME => DbName,
        ?DB_UUID => DbUUID,
        ?DOC_ID => DocId,
        ?ERROR_COUNT => 0,
        ?REP_STATS => #{},
        ?STATE => State,
        ?STATE_INFO => StateInfo,
        ?DOC_STATE => DocState,
        ?LAST_UPDATED => NowSec,
        ?LAST_START => 0,
        ?LAST_ERROR => null,
        ?REP_NODE => null,
        ?REP_PID => null,
        ?JOB_HISTORY => [AddedEvent],
        ?CHECKPOINT_HISTORY => []
    }.


add_job(Tx, JobId, JobData) ->
    couch_stats:increment_counter([couch_replicator, jobs, adds]),
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        case couch_jobs:get_job_data(JTx, ?REP_JOBS, JobId) of
            {ok, #{} = OldData} ->
                ok = remove_job(JTx, JobId, OldData);
            {error, not_found} ->
                ok
        end,
        ok = couch_jobs:add(JTx, ?REP_JOBS, JobId, JobData)
    end).


remove_job(Tx, JobId) ->
    couch_stats:increment_counter([couch_replicator, jobs, removes]),
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        case couch_jobs:get_job_data(JTx, ?REP_JOBS, JobId) of
            {ok, #{} = JobData} ->
                ok = remove_job(JTx, JobId, JobData);
            {error, not_found} ->
                ok
        end
    end).


get_job_data(Tx, JobId) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        couch_jobs:get_job_data(JTx, ?REP_JOBS, JobId)
    end).


% UserFun = fun(JTx, JobId, JobState, JobData, UserAcc)
%
fold_jobs(Tx, UserFun, Acc) when is_function(UserFun, 5) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        couch_jobs:fold_jobs(JTx, ?REP_JOBS, UserFun, Acc)
    end).


pending_count(_Tx, Limit) when is_integer(Limit), Limit =< 0 ->
    0;

pending_count(Tx, Limit) when is_integer(Limit), Limit > 0 ->
    Opts = #{
        max_sched_time => erlang:system_time(second),
        limit => Limit
    },
    couch_jobs:pending_count(Tx, ?REP_JOBS, Opts).


wait_running(JobId) ->
    case couch_jobs:subscribe(?REP_JOBS, JobId) of
        {ok, finished, JobData} ->
            {ok, JobData};
        {ok, SubId, running, #{?STATE := ?ST_PENDING}} ->
            wait_running(JobId, SubId);
        {ok, SubId, running, JobData} ->
            ok = couch_jobs:unsubscribe(SubId),
            {ok, JobData};
        {ok, SubId, pending, _} ->
            wait_running(JobId, SubId);
        {error, Error} ->
            {error, Error}
    end.


wait_running(JobId, SubId) ->
    case couch_jobs:wait(SubId, running, infinity) of
        {?REP_JOBS, _, running, #{?STATE := ?ST_PENDING}} ->
            wait_running(JobId, SubId);
        {?REP_JOBS, _, running, JobData} ->
            ok = couch_jobs:unsubscribe(SubId),
            {ok, JobData};
        {?REP_JOBS, _, finished, JobData} ->
            ok = couch_jobs:unsubscribe(SubId),
            {ok, JobData}
    end.


wait_result(JobId) ->
    case couch_jobs:subscribe(?REP_JOBS, JobId) of
        {ok, finished, JobData} ->
            {ok, JobData};
        {ok, SubId, _, _} ->
            {?REP_JOBS, _, finished, JobData} = couch_jobs:wait(SubId,
                finished, infinity),
            {ok, JobData};
        {error, Error} ->
            {error, Error}
    end.


accept_job(MaxSchedTime) when is_integer(MaxSchedTime) ->
    Opts = #{max_sched_time => MaxSchedTime},
    couch_jobs:accept(?REP_JOBS, Opts).


update_job_data(Tx, #{} = Job, #{} = JobData) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        couch_jobs:update(JTx, Job, JobData)
    end).


finish_job(Tx, #{} = Job, #{} = JobData) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        couch_jobs:finish(JTx, Job, JobData)
    end).


reschedule_job(Tx, #{} = Job, #{} = JobData, Time) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        {ok, Job1} = couch_jobs:resubmit(JTx, Job, Time),
        ok = couch_jobs:finish(JTx, Job1, JobData)
    end).


try_update_rep_id(Tx, JobId, RepId) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        #{tx := ErlFdbTx, layer_prefix := LayerPrefix} = JTx,
        Key = erlfdb_tuple:pack({?REPLICATION_IDS, RepId}, LayerPrefix),
        case get_job_id(JTx, RepId) of
            {error, not_found} ->
                ok = erlfdb:set(ErlFdbTx, Key, JobId);
            {ok, JobId} ->
                ok;
            {ok, OtherJobId} when is_binary(OtherJobId) ->
                {error, {replication_job_conflict, OtherJobId}}
        end
    end).


update_rep_id(Tx, JobId, RepId) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        #{tx := ErlFdbTx, layer_prefix := LayerPrefix} = JTx,
        Key = erlfdb_tuple:pack({?REPLICATION_IDS, RepId}, LayerPrefix),
        ok = erlfdb:set(ErlFdbTx, Key, JobId)
    end).


clear_old_rep_id(_, _, null) ->
    ok;

clear_old_rep_id(Tx, JobId, RepId) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        #{tx := ErlFdbTx, layer_prefix := LayerPrefix} = JTx,
        Key = erlfdb_tuple:pack({?REPLICATION_IDS, RepId}, LayerPrefix),
        case get_job_id(JTx, RepId) of
            {error, not_found} ->
                ok;
            {ok, JobId} ->
                ok = erlfdb:clear(ErlFdbTx, Key);
            {ok, OtherJobId} when is_binary(OtherJobId) ->
                ok
        end
    end).


get_job_id(Tx, RepId) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        #{tx := ErlFdbTx, layer_prefix := LayerPrefix} = JTx,
        Key = erlfdb_tuple:pack({?REPLICATION_IDS, RepId}, LayerPrefix),
        case erlfdb:wait(erlfdb:get(ErlFdbTx, Key)) of
            not_found ->
                {error, not_found};
            <<_/binary>> = JobId ->
                {ok, JobId}
        end
    end).


% Debug functions

remove_jobs(Tx, JobIds) when is_list(JobIds) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        lists:foreach(fun(JobId) -> remove_job(JTx, JobId) end, JobIds)
    end),
    [].


get_job_ids(Tx) ->
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(Tx), fun(JTx) ->
        #{tx := ErlFdbTx, layer_prefix := LayerPrefix} = JTx,
        Prefix = erlfdb_tuple:pack({?REPLICATION_IDS}, LayerPrefix),
        KVs = erlfdb:wait(erlfdb:get_range_startswith(ErlFdbTx, Prefix)),
        lists:map(fun({K, JobId}) ->
            {RepId} = erlfdb_tuple:unpack(K, Prefix),
            {RepId, JobId}
        end, KVs)
    end).


% Private functions

remove_job(#{jtx := true} = JTx, JobId, OldJobData) ->
    #{tx := Tx, layer_prefix := LayerPrefix} = JTx,
    case OldJobData of
        #{?REP_ID := null} ->
            couch_jobs:remove(JTx, ?REP_JOBS, JobId);
        #{?REP_ID := RepId} when is_binary(RepId) ->
            Key = erlfdb_tuple:pack({?REPLICATION_IDS, RepId}, LayerPrefix),
            case erlfdb:wait(erlfdb:get(Tx, Key)) of
                not_found -> ok;
                JobId -> erlfdb:clear(Tx, Key);
                <<_/binary>> -> ok
            end,
            couch_jobs:remove(JTx, ?REP_JOBS, JobId)
    end.
