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

-module(fabric2_db_expiration).


-behaviour(gen_server).


-export([
    start_link/0,
    cleanup/1,
    process_expirations/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("fabric/include/fabric2.hrl").

-define(JOB_TYPE, <<"db_expiration">>).
-define(JOB_ID, <<"db_expiration_job">>).
-define(DEFAULT_JOB_Version, 1).
-define(DEFAULT_RETENTION_SEC, 172800). % 48 hours
-define(DEFAULT_SCHEDULE_SEC, 3600). % 1 hour
-define(ERROR_RESCHEDULE_SEC, 5).
-define(CHECK_ENABLED_SEC, 2).
-define(JOB_TIMEOUT_SEC, 30).


-record(st, {
    job
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    process_flag(trap_exit, true),
    {ok, #st{job = undefined}, 0}.


terminate(_M, _St) ->
    ok.


handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info(timeout, #st{job = undefined} = St) ->
    ok = wait_for_couch_jobs_app(),
    ok = couch_jobs:set_type_timeout(?JOB_TYPE, ?JOB_TIMEOUT_SEC),
    ok = maybe_add_job(),
    Pid = spawn_link(?MODULE, cleanup, [is_enabled()]),
    {noreply, St#st{job = Pid}};

handle_info({'EXIT', Pid, Exit}, #st{job = Pid} = St) ->
    case Exit of
        normal -> ok;
        Error -> couch_log:error("~p : job error ~p", [?MODULE, Error])
    end,
    NewPid = spawn_link(?MODULE, cleanup, [is_enabled()]),
    {noreply, St#st{job = NewPid}};

handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


wait_for_couch_jobs_app() ->
    % Because of a circular dependency between couch_jobs and fabric apps, wait
    % for couch_jobs to initialize before continuing. If we refactor the
    % commits FDB utilities out we can remove this bit of code.
    case lists:keysearch(couch_jobs, 1, application:which_applications()) of
        {value, {couch_jobs, _, _}} ->
            ok;
        false ->
            timer:sleep(100),
            wait_for_couch_jobs_app()
    end.


maybe_add_job() ->
    case couch_jobs:get_job_data(undefined, ?JOB_TYPE, job_id()) of
        {error, not_found} ->
            Now = erlang:system_time(second),
            ok = couch_jobs:add(undefined, ?JOB_TYPE, job_id(), #{}, Now);
        {ok, _JobData} ->
            ok
    end.


cleanup(false) ->
    timer:sleep(?CHECK_ENABLED_SEC * 1000),
    exit(normal);

cleanup(true) ->
    Now = erlang:system_time(second),
    ScheduleSec = schedule_sec(),
    Opts = #{max_sched_time => Now + min(ScheduleSec div 3, 15)},
    case couch_jobs:accept(?JOB_TYPE, Opts) of
        {ok, Job, Data} ->
            try
                {ok, Job1, Data1} = ?MODULE:process_expirations(Job, Data),
                ok = resubmit_job(Job1, Data1, schedule_sec())
            catch
                _Tag:Error ->
                    Stack = erlang:get_stacktrace(),
                    couch_log:error("~p : processing error ~p ~p ~p",
                        [?MODULE, Job, Error, Stack]),
                    ok = resubmit_job(Job, Data, ?ERROR_RESCHEDULE_SEC),
                    exit({job_error, Error, Stack})
            end;
        {error, not_found} ->
            timer:sleep(?CHECK_ENABLED_SEC * 1000),
            ?MODULE:cleanup(is_enabled())
    end.


resubmit_job(Job, Data, After) ->
    Now = erlang:system_time(second),
    SchedTime = Now + After,
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(), fun(JTx) ->
         {ok, Job1} = couch_jobs:resubmit(JTx, Job, SchedTime),
         ok = couch_jobs:finish(JTx, Job1, Data)
    end),
    ok.


process_expirations(#{} = Job, #{} = Data) ->
    Start = now_sec(),
    Callback = fun(Value, LastUpdateAt) ->
        case Value of
            {meta, _} -> ok;
            {row, DbInfo} -> process_row(DbInfo);
            complete -> ok
        end,
        {ok, maybe_report_progress(Job, LastUpdateAt)}
    end,
    {ok, _Infos} = fabric2_db:list_deleted_dbs_info(
        Callback,
        Start,
        [{restart_tx, true}]
    ),
    {ok, Job, Data}.


process_row(DbInfo) ->
    DbName = proplists:get_value(db_name, DbInfo),
    TimeStamp = proplists:get_value(timestamp, DbInfo),
    Now = now_sec(),
    Retention = retention_sec(),
    Since = Now - Retention,
    case Since >= timestamp_to_sec(TimeStamp)  of
        true ->
            couch_log:notice("Permanently deleting ~p database with"
                "  timestamp ~p", [DbName, TimeStamp]),
            ok = fabric2_db:delete(DbName, [{deleted_at, TimeStamp}]);
        false ->
            ok
    end.


maybe_report_progress(Job, LastUpdateAt) ->
    % Update periodically the job so it doesn't expire
    Now = now_sec(),
    Progress = #{
        <<"processed_at">> => Now

    },
    case (Now - LastUpdateAt) > (?JOB_TIMEOUT_SEC div 2) of
        true ->
            couch_jobs:update(undefined, Job, Progress),
            Now;
        false ->
            LastUpdateAt
    end.


job_id() ->
    JobVersion = job_version(),
    <<?JOB_ID/binary, "-", JobVersion:16/integer>>.


now_sec() ->
    Now = os:timestamp(),
    Nowish = calendar:now_to_universal_time(Now),
    calendar:datetime_to_gregorian_seconds(Nowish).


timestamp_to_sec(TimeStamp) ->
    <<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary,
        "T",
        Hour:2/binary, ":", Minutes:2/binary, ":", Second:2/binary,
        "Z">> = TimeStamp,

    calendar:datetime_to_gregorian_seconds(
        {{?bin2int(Year), ?bin2int(Month), ?bin2int(Day)},
            {?bin2int(Hour), ?bin2int(Minutes), ?bin2int(Second)}}
    ).


is_enabled() ->
    config:get_boolean("couchdb", "db_expiration_enabled", false).


job_version() ->
    config:get_integer("couchdb", "db_expiration_job_version",
        ?DEFAULT_JOB_Version).


retention_sec() ->
    config:get_integer("couchdb", "db_expiration_retention_sec",
        ?DEFAULT_RETENTION_SEC).


schedule_sec() ->
    config:get_integer("couchdb", "db_expiration_schedule_sec",
        ?DEFAULT_SCHEDULE_SEC).
