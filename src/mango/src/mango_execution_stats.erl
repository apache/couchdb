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
    incr_keys_examined/1,
    incr_docs_examined/1,
    incr_docs_examined/2,
    incr_results_returned/1,
    log_start/1,
    log_end/1,
    maybe_add_stats/4
]).


-include("mango_cursor.hrl").


to_json(Stats) ->
    {[
        {total_keys_examined, Stats#execution_stats.totalKeysExamined},
        {total_docs_examined, Stats#execution_stats.totalDocsExamined},
        {results_returned, Stats#execution_stats.resultsReturned},
        {execution_time_ms, Stats#execution_stats.executionTimeMs}
    ]}.


incr_keys_examined(Stats) ->
    Stats#execution_stats {
        totalKeysExamined = Stats#execution_stats.totalKeysExamined + 1
    }.


incr_docs_examined(Stats) ->
    incr_docs_examined(Stats, 1).


incr_docs_examined(Stats, N) ->
    Stats#execution_stats {
        totalDocsExamined = Stats#execution_stats.totalDocsExamined + N
    }.


incr_results_returned(Stats) ->
    couch_stats:increment_counter([mango, results_returned]),
    Stats#execution_stats {
        resultsReturned = Stats#execution_stats.resultsReturned + 1
    }.


log_start(Stats) ->
    Stats#execution_stats {
        executionStartTime = os:timestamp()
    }.


log_end(Stats) ->
    End = os:timestamp(),
    Diff = timer:now_diff(End, Stats#execution_stats.executionStartTime) / 1000,
    Stats#execution_stats {
        executionTimeMs = Diff
    }.


maybe_add_stats(Opts, UserFun, Stats0, UserAcc) ->
    Stats1 = log_end(Stats0),
    couch_stats:update_histogram([mango, query_time], Stats1#execution_stats.executionTimeMs),

    case couch_util:get_value(execution_stats, Opts) of
        true ->
            JSONValue = to_json(Stats1),
            Arg = {add_key, execution_stats, JSONValue},
            {_Go, FinalUserAcc} = UserFun(Arg, UserAcc),
            FinalUserAcc;
        _ ->
            UserAcc
    end.
