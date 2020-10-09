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

-export([
    new/0,
    new/1,
    get/2,
    increment/2,
    sum_stats/2,
    max_stats/2
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

new(#{} = Map) ->
    new(maps:to_list(Map));
new(Initializers0) when is_list(Initializers0) ->
    Initializers1 = lists:filtermap(fun fmap/1, Initializers0),
    orddict:from_list(Initializers1).

missing_checked(Stats) ->
    get(missing_checked, Stats).

missing_found(Stats) ->
    get(missing_found, Stats).

docs_read(Stats) ->
    get(docs_read, Stats).

docs_written(Stats) ->
    get(docs_written, Stats).

doc_write_failures(Stats) ->
    get(doc_write_failures, Stats).

get(Field, Stats) ->
    case orddict:find(Field, Stats) of
        {ok, Value} ->
            Value;
        error ->
            0
    end.

increment(Field, Stats) ->
    orddict:update_counter(Field, 1, Stats).

sum_stats(S1, S2) ->
    orddict:merge(fun(_, V1, V2) -> V1+V2 end, S1, S2).

max_stats(S1, S2) ->
    orddict:merge(fun(_, V1, V2) -> max(V1, V2) end, S1, S2).


% Handle initializing from a status object, which uses same values but
% different field names, as well as from ejson props from the checkpoint
% history
%
fmap({missing_found, _})             -> true;
fmap({missing_revisions_found, V})   -> {true, {missing_found, V}};
fmap({<<"missing_found">>, V})       -> {true, {missing_found, V}};

fmap({missing_checked, _})           -> true;
fmap({revisions_checked, V})         -> {true, {missing_checked, V}};
fmap({<<"missing_checked">>, V})     -> {true, {missing_checked, V}};

fmap({docs_read, _})                 -> true;
fmap({<<"docs_read">>, V})           -> {true, {docs_read, V}};

fmap({docs_written, _})              -> true;
fmap({<<"docs_written">>, V})        -> {true, {docs_written, V}};

fmap({doc_write_failures, _})        -> true;
fmap({<<"doc_write_failures">>, V})  -> {true, {doc_write_failures, V}};

fmap({_, _})                         -> false.
