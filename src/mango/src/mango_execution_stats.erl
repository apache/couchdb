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
    incr_quorum_docs_examined/1,
    incr_results_returned/1,
    log_execution_start/1,
    log_execution_end/1
]).


-include("mango_execution_stats.hrl").


to_json(Stats) ->
    {[
        {total_keys_examined, Stats#execution_stats.totalKeysExamined},
        {total_docs_examined, Stats#execution_stats.totalDocsExamined},
        {total_quorum_docs_examined, Stats#execution_stats.totalQuorumDocsExamined},
        {results_returned, Stats#execution_stats.resultsReturned},
        {execution_time_ms, Stats#execution_stats.executionTimeMs}
    ]}.


incr_keys_examined(Stats) ->
    Stats#execution_stats {
        totalKeysExamined = Stats#execution_stats.totalKeysExamined + 1
    }.

incr_docs_examined(Stats) ->
    Stats#execution_stats {
        totalDocsExamined = Stats#execution_stats.totalDocsExamined + 1
    }.

incr_quorum_docs_examined(Stats) ->
    Stats#execution_stats {
        totalQuorumDocsExamined = Stats#execution_stats.totalQuorumDocsExamined + 1
    }.

incr_results_returned(Stats) ->
    Stats#execution_stats {
        resultsReturned = Stats#execution_stats.resultsReturned + 1
    }.

log_execution_start(Stats) ->
    Stats#execution_stats {
        executionStartTime = now()
    }.

log_execution_end(Stats) ->
    End = now(),
    Diff = timer:now_diff(End, Stats#execution_stats.executionStartTime) / 1000 % ms,
    Stats#execution_stats {
        executionTimeMs = Diff
    }.
