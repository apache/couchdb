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

% This module implements the "Fair Share" algorithm by Judy Kay and Piers
% Lauder [1] and applies it to the scheduling of replication jobs.
%
% The main idea is _replicator dbs can have a configurable number of "shares"
% assigned to them. Shares is an abstract quantity from 1 to 1000. The default
% is 100. Jobs from _replicator databases with more shares get proportionally a
% higher chance to run than those from databases with a lower number of shares.
%
% Every scheduler cycle running jobs are "charged" based on how much time they
% spent running during that cycle. At the end of the cycle the accumulated
% charges for each job, the number of shares configured, and the total number
% of jobs in the pending queue from the same _replicator db, are used to
% calculate new priority values for all the jobs. To match the algorithm from
% the paper, jobs with lower priority values are the ones at the front of the
% run queue and have a higher chance of running.
%
% Here is how charges, shares, and number of sibling jobs affect the
% priority value:
%
%   1) Jobs from dbs with higher configured shares get assigned lower
%   priority values and so stay closer to the front of the queue.
%
%   2) Jobs from dbs with many other jobs (many siblings) get assigned a
%   higher priority value, so they get pushed further down the queue
%   and have a lower chance of running.
%
%   3) Jobs which run longer accumulate more charges and get assigned a
%   higher priority value and get to wait longer to run.
%
% In order to prevent job starvation, all job priorities are periodicaly
% decayed (decreased). This effectively moves all the jobs towards the front of
% the run queue. So, in effect, there are two competing processes: one
% uniformly moves all jobs to the front, and the other throws them back in
% proportion to those factors mentioned above. The speed of this uniform
% priority decay is controlled by the priority_coeff parameter.
%
% In order to prevent jobs from low shares dbs from "cheating" by getting
% deleted and immediately re-added, charges are accumulated using a
% historically decayed usage value. The speed of the usage decay is controlled
% by the `usage_coeff = 0.5` parameter.
%
% [1] : https://proteusmaster.urcf.drexel.edu/urcfwiki/images/KayLauderFairShare.pdf

-module(couch_replicator_share).

-export([
    init/0,
    clear/0,
    update_shares/2,
    reset_shares/1,
    job_added/1,
    job_removed/1,
    update/3,
    priority/1,
    charge/3
]).

-include_lib("couch/include/couch_db.hrl").
-include("couch_replicator.hrl").

% Usage coefficient decays historic usage every scheduling cycle. For example,
% the usage value for a job running 1 minute is 60000000 (i.e microseconds /
% minute), then if the job stops running it will take about 26 cycles (minutes)
% for it to decay to 0 and the system to "forget" about it completely:
%
%  trunc(60000000 * math:pow(0.5, 26)) = 0
%
-define(DEFAULT_USAGE_COEFF, 0.5).

% Priority coefficient decays all the job priorities such that they slowly
% drift towards the front of the run queue. This coefficient defines a maximum
% time window over which this algorithm would operate. For example, if this
% value is too small (0.1), after a few cycles quite a few jobs would end up at
% priority 0, and would render this algorithm useless. The default value of
% 0.98 is picked such that if a job ran for one scheduler cycle, then didn't
% get to run for 7 hours, it would still have priority > 0. 7 hours was picked
% as it was close enought to 8 hours which is the default maximum error backoff
% interval.
%
% Example calculation:
%   shares = 100
%   usage after 1 minute cycle run = 60000000
%   initial priority =  60000000 / (100 * 100) = 6000
%   trunc(6000 * math:pow(0.98, 431)) = 0
%   431 / 60 ~= 7 hrs
%
-define(DEFAULT_PRIORITY_COEFF, 0.98).

-define(MIN_SHARES, 1).
-define(MAX_SHARES, 1000).
-define(DEFAULT_SHARES, 100).

-define(SHARES, couch_replicator_shares).
-define(PRIORITIES, couch_replicator_priorities).
-define(USAGE, couch_replicator_usage).
-define(CHARGES, couch_replicator_stopped_usage).
-define(NUM_JOBS, couch_replicator_num_jobs).

init() ->
    EtsOpts = [named_table, public],
    % {Key, Shares}
    ?SHARES = ets:new(?SHARES, EtsOpts),
    % {JobId, Priority}
    ?PRIORITIES = ets:new(?PRIORITIES, EtsOpts),
    % {Key, Usage}
    ?USAGE = ets:new(?USAGE, EtsOpts),
    % {Key, Charges}
    ?CHARGES = ets:new(?CHARGES, EtsOpts),
    % {Key, NumJobs}
    ?NUM_JOBS = ets:new(?NUM_JOBS, EtsOpts),
    lists:foreach(fun({K, V}) -> update_shares(K, V) end, get_config_shares()).

clear() ->
    Tables = [?SHARES, ?PRIORITIES, ?USAGE, ?CHARGES, ?NUM_JOBS],
    lists:foreach(fun(T) -> catch ets:delete(T) end, Tables).

% This should be called when user updates the replicator.shares config section
%
update_shares(Key, Shares) when is_integer(Shares) ->
    ets:insert(?SHARES, {Key, bounded(Shares, ?MIN_SHARES, ?MAX_SHARES)}).

% Called when the config value is deleted and shares are reset to the default
% value.
reset_shares(Key) ->
    ets:delete(?SHARES, Key).

job_added(#job{} = Job) ->
    Key = key(Job),
    % If the entry is not present {Key, 0} is used as the default
    ets:update_counter(?NUM_JOBS, Key, 1, {Key, 0}),
    % Update job's priority as if it ran during one scheduler cycle. This is so
    % new jobs don't get to be at priority 0 (highest).
    update_priority(Job).

job_removed(#job{} = Job) ->
    Key = key(Job),
    ets:delete(?PRIORITIES, Job#job.id),
    case ets:update_counter(?NUM_JOBS, Key, -1, {Key, 0}) of
        N when is_integer(N), N =< 0 ->
            ets:delete(?NUM_JOBS, Key);
        N when is_integer(N), N > 0 ->
            ok
    end,
    ok.

% This is the main algorithm update function. It should be called during each
% rescheduling cycle with a list of running jobs, the interval from the
% scheduler (in milliseconds), and the current timestamp.
%
% This function does all three main steps as described in [1].
%
% 1. Update usage from all the charges in the last scheduling cycle
%
% 2. Uniformly decay all job priorities
%
% 3. Update priorities for all the running jobs based on usage and number of
%    sibling jobs.
%
update(RunningJobs, Interval, {_, _, _} = Now) ->
    lists:foreach(fun(Job) -> charge(Job, Interval, Now) end, RunningJobs),
    update_usage(),
    decay_priorities(),
    lists:foreach(fun(Job) -> update_priority(Job) end, RunningJobs).

priority(JobId) ->
    % Not found means it was removed because it's value was 0
    case ets:lookup(?PRIORITIES, JobId) of
        [{_, Priority}] -> Priority;
        [] -> 0
    end.

charge(#job{pid = undefined}, _, _) ->
    0;
charge(#job{} = Job, Interval, {_, _, _} = Now) when is_integer(Interval) ->
    Key = key(Job),
    Charges = job_charges(Job, Interval, Now),
    % If the entry is not present {Key, 0} is used as the default
    ets:update_counter(?CHARGES, Key, Charges, {Key, 0}).

usage(Key) ->
    case ets:lookup(?USAGE, Key) of
        [{_, Usage}] -> Usage;
        [] -> 0
    end.

num_jobs(Key) ->
    case ets:lookup(?NUM_JOBS, Key) of
        [{_, NumJobs}] -> NumJobs;
        [] -> 0
    end.

shares(Key) ->
    case ets:lookup(?SHARES, Key) of
        [{_, Shares}] -> Shares;
        [] -> ?DEFAULT_SHARES
    end.

% In [1] this described in the "Decay of Process Priorities" section
%
decay_priorities() ->
    decay(?PRIORITIES, priority_coeff()),
    % If priority becomes 0, it's removed. When looking it up, if it
    % is missing we assume it is 0
    clear_zero(?PRIORITIES).

% This is the main part of the alrgorithm. In [1] it is described in the
% "Priority Adjustment" section.
%
update_priority(#job{} = Job) ->
    Id = Job#job.id,
    Key = key(Job),
    Shares = shares(Key),
    Priority = (usage(Key) * num_jobs(Key)) / (Shares * Shares),
    % If the entry is not present {Id, 0} is used as the default
    ets:update_counter(?PRIORITIES, Id, trunc(Priority), {Id, 0}).

% This is the "User-Level Scheduling" part from [1]
%
update_usage() ->
    decay(?USAGE, usage_coeff()),
    clear_zero(?USAGE),
    ets:foldl(
        fun({Key, Charges}, _) ->
            % If the entry is not present {Key, 0} is used as the default
            ets:update_counter(?USAGE, Key, Charges, {Key, 0})
        end,
        0,
        ?CHARGES
    ),
    % Start each interval with a fresh charges table
    ets:delete_all_objects(?CHARGES).

% Private helper functions

decay(Ets, Coeff) when is_atom(Ets) ->
    % Use trunc to ensure the result stays an integer in order for
    % ets:update_counter to work properly. It throws a badarg otherwise.
    Head = {'$1', '$2'},
    Result = {{'$1', {trunc, {'*', '$2', {const, Coeff}}}}},
    ets:select_replace(Ets, [{Head, [], [Result]}]).

clear_zero(Ets) when is_atom(Ets) ->
    ets:select_delete(Ets, [{{'_', '$1'}, [{'=<', '$1', 0}], [true]}]).

key(#job{} = Job) ->
    Rep = Job#job.rep,
    case is_binary(Rep#rep.db_name) of
        true -> mem3:dbname(Rep#rep.db_name);
        false -> (Rep#rep.user_ctx)#user_ctx.name
    end.

% Jobs are charged based on the amount of time the job was running during the
% last scheduling interval. The time units used are microseconds in order to
% have a large enough usage values so that when priority is calculated the
% rounded value won't be rounded off to 0 easily. The formula for the priority
% calculation is:
%
%    Priority = (Usage * NumJobs) / Shares^2
%
% Then in the worst case of a single job in the db, running only for one
% second,for one job, with 1000 (max) shares, the priority would be:
%
%    1000000 * 1 / (1000^2) = 1
%
job_charges(#job{} = Job, IntervalMSec, {_, _, _} = Now) ->
    TimeRunning = timer:now_diff(Now, last_started(Job)),
    IntervalUSec = IntervalMSec * 1000,
    bounded(TimeRunning, 0, IntervalUSec).

last_started(#job{} = Job) ->
    case lists:keyfind(started, 1, Job#job.history) of
        % In case user set too low of a max history
        false -> {0, 0, 0};
        {started, When} -> When
    end.

bounded(Val, Min, Max) ->
    max(Min, min(Max, Val)).

% Config helper functions

get_config_shares() ->
    lists:map(
        fun({K, V}) ->
            {list_to_binary(K), int_val(V, ?DEFAULT_SHARES)}
        end,
        config:get("replicator.shares")
    ).

priority_coeff() ->
    % This is the K2 coefficient from [1]
    Default = ?DEFAULT_PRIORITY_COEFF,
    Val = float_val(config:get("replicator", "priority_coeff"), Default),
    bounded(Val, 0.0, 1.0).

usage_coeff() ->
    % This is the K1 coefficient from [1]
    Default = ?DEFAULT_USAGE_COEFF,
    Val = float_val(config:get("replicator", "usage_coeff"), Default),
    bounded(Val, 0.0, 1.0).

int_val(Str, Default) when is_list(Str) ->
    try list_to_integer(Str) of
        Val -> Val
    catch
        error:badarg ->
            Default
    end.

float_val(undefined, Default) ->
    Default;
float_val(Str, Default) when is_list(Str) ->
    try list_to_float(Str) of
        Val -> Val
    catch
        error:badarg ->
            Default
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_replicator/test/eunit/couch_replicator_test.hrl").

-define(DB1, <<"db1">>).
-define(DB2, <<"db2">>).
-define(DB3, <<"db3">>).
-define(J1, <<"j1">>).
-define(J2, <<"j2">>).
-define(J3, <<"j3">>).

fair_share_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(init_works),
                ?TDEF_FE(shares_are_updated_and_reset),
                ?TDEF_FE(jobs_are_added_and_removed),
                ?TDEF_FE(can_fetch_job_priority),
                ?TDEF_FE(jobs_are_charged),
                ?TDEF_FE(usage_is_updated),
                ?TDEF_FE(priority_coefficient_works),
                ?TDEF_FE(priority_decays_when_jobs_stop_running),
                ?TDEF_FE(priority_increases_when_jobs_run),
                ?TDEF_FE(two_dbs_equal_shares_equal_number_of_jobs),
                ?TDEF_FE(two_dbs_unequal_shares_equal_number_of_jobs),
                ?TDEF_FE(two_dbs_equal_shares_unequal_number_of_jobs),
                ?TDEF_FE(two_dbs_unequal_shares_unequal_number_of_jobs),
                ?TDEF_FE(three_dbs_equal_shares_equal_number_of_jobs),
                ?TDEF_FE(three_dbs_unequal_shares_equal_number_of_jobs),
                ?TDEF_FE(three_dbs_equal_shares_unequal_number_of_jobs),
                ?TDEF_FE(three_dbs_unequal_shares_unequal_number_of_jobs)
            ]
        }
    }.

setup_all() ->
    test_util:start_couch().

teardown_all(Ctx) ->
    config_delete("priority_coeff"),
    config_delete("usage_coeff"),
    config_shares_delete(),
    test_util:stop_couch(Ctx).

setup() ->
    init(),
    ok.

teardown(_) ->
    clear(),
    config_delete("priority_coeff"),
    config_delete("usage_coeff"),
    config_shares_delete().

init_works(_) ->
    Tables = [?SHARES, ?PRIORITIES, ?USAGE, ?CHARGES, ?NUM_JOBS],
    [?assert(is_list(ets:info(T))) || T <- Tables],
    ?assertEqual(#{}, tab2map(?SHARES)),

    clear(),
    [?assertEqual(undefined, ets:info(T)) || T <- Tables],

    config_share_set("db1", "200"),
    init(),
    ?assertEqual(200, shares(?DB1)),
    ?assertEqual(#{?DB1 => 200}, tab2map(?SHARES)).

shares_are_updated_and_reset(_) ->
    ?assertEqual(#{}, tab2map(?SHARES)),

    update_shares(?DB1, 42),
    ?assertEqual(42, shares(?DB1)),

    reset_shares(?DB1),
    ?assertEqual(100, shares(?DB1)),
    ?assertEqual(#{}, tab2map(?SHARES)),

    % min shares
    update_shares(?DB1, 0),
    ?assertEqual(1, shares(?DB1)),

    % max shares
    update_shares(?DB1, 1001),
    ?assertEqual(1000, shares(?DB1)).

jobs_are_added_and_removed(_) ->
    job_added(job(?J1, ?DB1)),
    ?assertEqual(1, num_jobs(?DB1)),
    ?assertEqual(#{?J1 => 0}, tab2map(?PRIORITIES)),

    job_added(job(?J2, ?DB1)),
    ?assertEqual(2, num_jobs(?DB1)),
    ?assertEqual(#{?J1 => 0, ?J2 => 0}, tab2map(?PRIORITIES)),

    job_added(job(?J3, ?DB2)),
    ?assertEqual(1, num_jobs(?DB2)),
    ?assertEqual(#{?J1 => 0, ?J2 => 0, ?J3 => 0}, tab2map(?PRIORITIES)),

    job_removed(job(?J1, ?DB1)),
    ?assertEqual(1, num_jobs(?DB1)),
    ?assertEqual(#{?J2 => 0, ?J3 => 0}, tab2map(?PRIORITIES)),

    job_removed(job(?J3, ?DB2)),
    ?assertEqual(0, num_jobs(?DB2)),
    ?assertEqual(0, priority(?J3)),

    job_removed(job(?J2, ?DB1)),
    ?assertEqual(0, num_jobs(?DB2)),
    ?assertEqual(#{}, tab2map(?NUM_JOBS)),
    ?assertEqual(0, priority(?J2)),
    ?assertEqual(#{}, tab2map(?PRIORITIES)).

can_fetch_job_priority(_) ->
    job_added(job(?J1, ?DB1)),
    ?assertEqual(0, priority(?J1)),

    ets:insert(?PRIORITIES, {?J1, 42}),
    ?assertEqual(42, priority(?J1)),

    ets:delete(?PRIORITIES, ?J1),
    ?assertEqual(0, priority(?J1)).

jobs_are_charged(_) ->
    Job1 = running_job(?J1, ?DB1),
    job_added(Job1),
    ?assertEqual(#{}, tab2map(?CHARGES)),

    charge(Job1, 1000, {0, 1, 0}),
    ?assertEqual(#{?DB1 => 1000000}, tab2map(?CHARGES)),

    % Stopped jobs are not charged
    charge(stop(Job1), 1000, {0, 1, 0}),
    ?assertEqual(#{?DB1 => 1000000}, tab2map(?CHARGES)),

    % Only charge up to one interval's worth even if job ran longer
    charge(Job1, 1000, {0, 5, 0}),
    ?assertEqual(#{?DB1 => 2000000}, tab2map(?CHARGES)),

    % Charges are accumulated from jobs in same db
    Job2 = running_job(?J2, ?DB1),
    job_added(Job2),
    charge(Job2, 1000, {0, 0, 1}),
    ?assertEqual(#{?DB1 => 2000001}, tab2map(?CHARGES)),

    % Charges are not cleared if jobs are removed
    job_removed(Job1),
    job_removed(Job2),
    ?assertEqual(#{?DB1 => 2000001}, tab2map(?CHARGES)).

usage_is_updated(_) ->
    Job = running_job(?J1, ?DB1),
    job_added(Job),

    charge(Job, 60000, {0, 60, 0}),
    update_usage(),
    ?assertEqual(60000000, usage(?DB1)),

    % Charges table is cleared after usage is updated
    ?assertEqual(#{}, tab2map(?CHARGES)),

    % Check that usage decay works
    config_set("usage_coeff", "0.2"),
    update_usage(),
    ?assertEqual(12000000, usage(?DB1)),

    config_set("usage_coeff", "0.5"),
    update_usage(),
    ?assertEqual(6000000, usage(?DB1)),

    % Check that function both decays and updates from charges
    charge(Job, 60000, {0, 60, 0}),
    update_usage(),
    ?assertEqual(63000000, usage(?DB1)),

    % Usage eventually decays to 0 and is removed from the table
    [update_usage() || _ <- lists:seq(1, 100)],
    ?assertEqual(0, usage(?DB1)),
    ?assertEqual(#{}, tab2map(?USAGE)).

priority_coefficient_works(_) ->
    job_added(job(?J1, ?DB1)),
    ets:insert(?PRIORITIES, {?J1, 1000}),

    config_set("priority_coeff", "0.8"),
    decay_priorities(),
    ?assertEqual(800, priority(?J1)),

    config_set("priority_coeff", "0.5"),
    decay_priorities(),
    ?assertEqual(400, priority(?J1)),

    % If non-float junk value is set then the default is used
    config_set("priority_coeff", "junk"),
    decay_priorities(),
    ?assertEqual(392, priority(?J1)),

    % Clipped to 1.0 max
    config_set("priority_coeff", "1.1"),
    decay_priorities(),
    ?assertEqual(392, priority(?J1)),

    % Clipped to 0.0 min and removed when =< 0
    config_set("priority_coeff", "-0.1"),
    decay_priorities(),
    ?assertEqual(0, priority(?J1)),
    ?assertEqual(#{}, tab2map(?PRIORITIES)).

priority_decays_when_jobs_stop_running(_) ->
    Job = running_job(?J1, ?DB1),
    job_added(Job),

    % Ran for one cycle then stop
    {[], Pending} = reschedule(1, {[Job], []}),

    % Priority is non-0 initially
    ?assert(priority(?J1) > 0),

    % Priority decays to 0 after some cycles
    [reschedule(0, {[], Pending}) || _ <- lists:seq(1, 500)],
    ?assertEqual(0, priority(?J1)).

priority_increases_when_jobs_run(_) ->
    Job = running_job(?J1, ?DB1),
    job_added(Job),

    Running = [Job],
    reschedule(0, {Running, []}),
    P1 = priority(?J1),
    ?assert(P1 > 0),

    % Priority increases
    reschedule(0, {Running, []}),
    P2 = priority(?J1),
    ?assert(P2 > P1),

    % Additive priority increase is balanced out by priority decay
    [reschedule(0, {Running, []}) || _ <- lists:seq(1, 500)],
    Pn = priority(?J1),
    ?assert(Pn > P2),

    reschedule(0, {Running, []}),
    Pm = priority(?J1),
    ?assertEqual(Pn, Pm).

two_dbs_equal_shares_equal_number_of_jobs(_) ->
    update_shares(?DB1, 100),
    update_shares(?DB2, 100),
    Jobs = jobs(#{?DB1 => {25, 75}, ?DB2 => {25, 75}}),
    #{?DB1 := Db1, ?DB2 := Db2} = run_scheduler(1000, 10, Jobs),
    ?assert(49 =< Db1 andalso Db1 =< 51),
    ?assert(49 =< Db2 andalso Db2 =< 51).

two_dbs_unequal_shares_equal_number_of_jobs(_) ->
    update_shares(?DB1, 100),
    update_shares(?DB1, 900),
    Jobs = jobs(#{?DB1 => {25, 75}, ?DB2 => {25, 75}}),
    #{?DB1 := Db1, ?DB2 := Db2} = run_scheduler(1000, 10, Jobs),
    ?assert(89 =< Db1 andalso Db1 =< 91),
    ?assert(9 =< Db2 andalso Db2 =< 11).

two_dbs_equal_shares_unequal_number_of_jobs(_) ->
    update_shares(?DB1, 100),
    update_shares(?DB2, 100),
    Jobs = jobs(#{?DB1 => {25, 25}, ?DB2 => {25, 125}}),
    #{?DB1 := Db1, ?DB2 := Db2} = run_scheduler(1000, 10, Jobs),
    ?assert(49 =< Db1 andalso Db1 =< 51),
    ?assert(49 =< Db2 andalso Db2 =< 51).

two_dbs_unequal_shares_unequal_number_of_jobs(_) ->
    update_shares(?DB1, 1),
    update_shares(?DB2, 100),
    Jobs = jobs(#{?DB1 => {25, 25}, ?DB2 => {25, 125}}),
    #{?DB1 := Db1, ?DB2 := Db2} = run_scheduler(1000, 10, Jobs),
    ?assert(0 =< Db1 andalso Db1 =< 2),
    ?assert(98 =< Db2 andalso Db2 =< 100).

three_dbs_equal_shares_equal_number_of_jobs(_) ->
    update_shares(?DB1, 100),
    update_shares(?DB2, 100),
    update_shares(?DB3, 100),
    Jobs = jobs(#{?DB1 => {25, 75}, ?DB2 => {25, 75}, ?DB3 => {25, 75}}),
    #{?DB1 := Db1, ?DB2 := Db2, ?DB3 := Db3} = run_scheduler(1000, 10, Jobs),
    ?assert(32 =< Db1 andalso Db1 =< 34),
    ?assert(32 =< Db2 andalso Db2 =< 34),
    ?assert(32 =< Db3 andalso Db3 =< 34).

three_dbs_unequal_shares_equal_number_of_jobs(_) ->
    update_shares(?DB1, 100),
    update_shares(?DB2, 700),
    update_shares(?DB3, 200),
    Jobs = jobs(#{?DB1 => {25, 75}, ?DB2 => {25, 75}, ?DB3 => {25, 75}}),
    #{?DB1 := Db1, ?DB2 := Db2, ?DB3 := Db3} = run_scheduler(1000, 10, Jobs),
    ?assert(9 =< Db1 andalso Db1 =< 11),
    ?assert(69 =< Db2 andalso Db2 =< 71),
    ?assert(19 =< Db3 andalso Db3 =< 21).

three_dbs_equal_shares_unequal_number_of_jobs(_) ->
    update_shares(?DB1, 100),
    update_shares(?DB2, 100),
    update_shares(?DB3, 100),
    Jobs = jobs(#{?DB1 => {25, 25}, ?DB2 => {25, 100}, ?DB3 => {25, 75}}),
    #{?DB1 := Db1, ?DB2 := Db2, ?DB3 := Db3} = run_scheduler(1000, 10, Jobs),
    ?assert(32 =< Db1 andalso Db1 =< 34),
    ?assert(32 =< Db2 andalso Db2 =< 34),
    ?assert(32 =< Db3 andalso Db3 =< 34).

three_dbs_unequal_shares_unequal_number_of_jobs(_) ->
    update_shares(?DB1, 1000),
    update_shares(?DB2, 100),
    update_shares(?DB3, 1),
    Jobs = jobs(#{?DB1 => {25, 100}, ?DB2 => {25, 125}, ?DB3 => {25, 875}}),
    #{?DB1 := Db1, ?DB2 := Db2, ?DB3 := Db3} = run_scheduler(1000, 10, Jobs),
    ?assert(87 =< Db1 andalso Db1 =< 89),
    ?assert(9 =< Db2 andalso Db2 =< 11),
    ?assert(2 =< Db3 andalso Db3 =< 4).

config_set(K, V) ->
    config:set("replicator", K, V, _Persist = false).

config_delete(K) ->
    config:delete("replicator", K, _Persist = false).

config_share_set(K, V) ->
    config:set("replicator.shares", K, V, _Persist = false).

config_shares_delete() ->
    [
        config:delete("replicator.shares", K, _Persist = false)
     || {K, _} <- config:get("replicator.shares")
    ].

tab2map(T) when is_atom(T) ->
    maps:from_list(ets:tab2list(T)).

job(rand, Db) ->
    job(rand:uniform(1 bsl 59), Db);
job(Id, Db) ->
    Job = #job{
        id = Id,
        rep = #rep{
            db_name = Db,
            user_ctx = #user_ctx{}
        }
    },
    stop(Job).

running_job(Id, Db) ->
    run(job(Id, Db)).

run(#job{} = Job) ->
    Job#job{
        pid = list_to_pid("<0.9999.999>"),
        history = [{started, {0, 0, 0}}, {added, {0, 0, 0}}]
    }.

stop(#job{} = Job) ->
    Job#job{
        pid = undefined,
        history = [{added, {0, 0, 0}}]
    }.

% Simple scheduler simulator. Start and stop N jobs and do the
% accounting steps. Return a new list of running and pending jobs. If
% N is 0 then jobs which were running stay running and jobs were
% pending stay pending.
%
reschedule(N, {Running, Pending}) ->
    update(Running, 60000, {0, 60, 0}),

    RunPr = [{priority(Job#job.id), Job} || Job <- Running],
    StopPr = [{priority(Job#job.id), Job} || Job <- Pending],

    {_, Running1} = lists:unzip(lists:reverse(lists:sort(RunPr))),
    {_, Pending1} = lists:unzip(lists:sort(StopPr)),

    ToStop = lists:sublist(Running1, N),
    ToStart = lists:sublist(Pending1, N),

    Running2 = [run(Job) || Job <- ToStart] ++ Running1 -- ToStop,
    Pending2 = [stop(Job) || Job <- ToStop] ++ Pending1 -- ToStart,

    {Running2, Pending2}.

% Run a few scheduling cycles and calculate usage percentage for each db
%
run_scheduler(Cycles, Churn, Jobs0) ->
    Acc0 = {#{}, Jobs0},

    {Sum, _} = lists:foldl(
        fun(_CycleCnt, {UsageAcc, {Running, _} = Jobs}) ->
            UsageAcc1 = lists:foldl(
                fun(#job{} = Job, Acc) ->
                    Db = Job#job.rep#rep.db_name,
                    maps:update_with(Db, fun(V) -> V + 1 end, 0, Acc)
                end,
                UsageAcc,
                Running
            ),
            {UsageAcc1, reschedule(Churn, Jobs)}
        end,
        Acc0,
        lists:seq(1, Cycles)
    ),

    Total = maps:fold(fun(_, V, Acc) -> Acc + V end, 0, Sum),
    maps:map(fun(_Db, V) -> round(V / Total * 100) end, Sum).

% Dbs = #{Db => {RunningCount, PendingCount}
%
jobs(#{} = Dbs) ->
    maps:fold(
        fun(Db, {RCnt, PCnt}, {Running, Pending}) ->
            RJobs = [running_job(rand, Db) || _ <- lists:seq(1, RCnt)],
            PJobs = [job(rand, Db) || _ <- lists:seq(1, PCnt)],
            [job_added(Job) || Job <- RJobs ++ PJobs],
            {Running ++ RJobs, Pending ++ PJobs}
        end,
        {[], []},
        Dbs
    ).

-endif.
