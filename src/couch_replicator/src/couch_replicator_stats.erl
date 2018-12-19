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

-module(couch_replicator_stats).

-record(rep_stats, {
    missing_checked = 0,
    missing_found = 0,
    docs_read = 0,
    docs_written = 0,
    doc_write_failures = 0
}).

-export([
    new/0,
    new/1,
    get/2,
    increment/2,
    sum_stats/2
]).

-export([
    missing_checked/1,
    missing_found/1,
    docs_read/1,
    docs_written/1,
    doc_write_failures/1
]).

new() ->
    orddict:new().

new(Initializers) when is_list(Initializers) ->
    orddict:from_list(Initializers).

missing_checked(Stats) ->
    get(missing_checked, upgrade(Stats)).

missing_found(Stats) ->
    get(missing_found, upgrade(Stats)).

docs_read(Stats) ->
    get(docs_read, upgrade(Stats)).

docs_written(Stats) ->
    get(docs_written, upgrade(Stats)).

doc_write_failures(Stats) ->
    get(doc_write_failures, upgrade(Stats)).

get(Field, Stats) ->
    case orddict:find(Field, upgrade(Stats)) of
        {ok, Value} ->
            Value;
        error ->
            0
    end.

increment(Field, Stats) ->
    orddict:update_counter(Field, 1, upgrade(Stats)).

sum_stats(S1, S2) ->
    orddict:merge(fun(_, V1, V2) -> V1+V2 end, upgrade(S1), upgrade(S2)).

upgrade(#rep_stats{} = Stats) ->
    orddict:from_list([
        {missing_checked, Stats#rep_stats.missing_checked},
        {missing_found, Stats#rep_stats.missing_found},
        {docs_read, Stats#rep_stats.docs_read},
        {docs_written, Stats#rep_stats.docs_written},
        {doc_write_failures, Stats#rep_stats.doc_write_failures}
    ]);
upgrade(Stats) ->
    Stats.
