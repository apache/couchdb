% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(mango_execution_stats).

-export([
    to_json/1,
    to_map/1,
    incr_keys_examined/1,
    incr_keys_examined/2,
    incr_docs_examined/1,
    incr_docs_examined/2,
    incr_quorum_docs_examined/1,
    incr_results_returned/1,
    log_start/2,
    log_end/1,
    log_stats/1,
    maybe_add_stats/4,
    shard_init/0,
    shard_incr_keys_examined/0,
    shard_incr_docs_examined/0,
    shard_get_stats/0
]).

-include("mango.hrl").
-include("mango_cursor.hrl").

-define(SHARD_STATS_KEY, mango_shard_execution_stats).

to_json(Stats) ->
    {[
        {total_keys_examined, Stats#execution_stats.totalKeysExamined},
        {total_docs_examined, Stats#execution_stats.totalDocsExamined},
        {total_quorum_docs_examined, Stats#execution_stats.totalQuorumDocsExamined},
        {results_returned, Stats#execution_stats.resultsReturned},
        {execution_time_ms, Stats#execution_stats.executionTimeMs},
        {dbname, Stats#execution_stats.dbname}
    ]}.

to_map(Stats) ->
    #{
        total_keys_examined => Stats#execution_stats.totalKeysExamined,
        total_docs_examined => Stats#execution_stats.totalDocsExamined,
        total_quorum_docs_examined => Stats#execution_stats.totalQuorumDocsExamined,
        results_returned => Stats#execution_stats.resultsReturned,
        dbname => Stats#execution_stats.dbname,
        execution_time_ms => Stats#execution_stats.executionTimeMs
    }.

incr_keys_examined(Stats) ->
    incr_keys_examined(Stats, 1).

incr_keys_examined(Stats, N) ->
    Stats#execution_stats{
        totalKeysExamined = Stats#execution_stats.totalKeysExamined + N
    }.

incr_docs_examined(Stats) ->
    incr_docs_examined(Stats, 1).

incr_docs_examined(Stats, N) ->
    Stats#execution_stats{
        totalDocsExamined = Stats#execution_stats.totalDocsExamined + N
    }.

incr_quorum_docs_examined(Stats) ->
    Stats#execution_stats{
        totalQuorumDocsExamined = Stats#execution_stats.totalQuorumDocsExamined + 1
    }.

incr_results_returned(Stats) ->
    couch_stats:increment_counter([mango, results_returned]),
    Stats#execution_stats{
        resultsReturned = Stats#execution_stats.resultsReturned + 1
    }.

log_start(Stats, DbName) ->
    Stats#execution_stats{
        executionStartTime = os:timestamp(),
        dbname = DbName
    }.

log_end(Stats) ->
    End = os:timestamp(),
    Diff = timer:now_diff(End, Stats#execution_stats.executionStartTime) / 1000,
    Stats#execution_stats{
        executionTimeMs = Diff
    }.

maybe_add_stats(Opts, UserFun, Stats0, UserAcc) ->
    Stats1 = log_end(Stats0),
    couch_stats:update_histogram([mango, query_time], Stats1#execution_stats.executionTimeMs),
    %% TODO: add docs vs quorum docs
    chttpd_stats:incr_rows(Stats1#execution_stats.totalKeysExamined),
    chttpd_stats:incr_reads(Stats1#execution_stats.totalDocsExamined),

    FinalAcc =
        case couch_util:get_value(execution_stats, Opts) of
            true ->
                JSONValue = to_json(Stats1),
                Arg = {add_key, execution_stats, JSONValue},
                {_Go, FinalUserAcc} = UserFun(Arg, UserAcc),
                FinalUserAcc;
            _ ->
                UserAcc
        end,
    {FinalAcc, Stats1}.

log_stats(Stats) ->
    MStats0 = to_map(Stats),
    Nonce = list_to_binary(couch_log_util:get_msg_id()),
    MStats1 = MStats0#{nonce => Nonce},
    couch_log:report("mango-stats", MStats1).

-spec shard_init() -> any().
shard_init() ->
    InitialState = #{docs_examined => 0, keys_examined => 0},
    put(?SHARD_STATS_KEY, InitialState).

-spec shard_incr_keys_examined() -> any().
shard_incr_keys_examined() ->
    incr(keys_examined).

-spec shard_incr_docs_examined() -> any().
shard_incr_docs_examined() ->
    incr(docs_examined).

-spec incr(atom()) -> any().
incr(Key) ->
    case get(?SHARD_STATS_KEY) of
        #{} = Stats0 ->
            Stats = maps:update_with(Key, fun(X) -> X + 1 end, Stats0),
            put(?SHARD_STATS_KEY, Stats);
        _ ->
            ok
    end.

-spec shard_get_stats() -> shard_stats_v2().
shard_get_stats() ->
    get(?SHARD_STATS_KEY).
