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


% Maintain cluster membership and stability notifications for replications.
% On changes to cluster membership, broadcast events to `replication` gen_event.
% Listeners will get `{cluster, stable}` or `{cluster, unstable}` events.
%
% Cluster stability is defined as "there have been no nodes added or removed in
% last `QuietPeriod` seconds". QuietPeriod value is configurable. To ensure a
% speedier startup, during initialization there is a shorter StartupQuietPeriod
% in effect (also configurable).
%
% This module is also in charge of calculating ownership of replications based
% on where their _repicator db documents shards live.

-module(couch_replicator_rate_limiter_tables).

-export([
   create/1,
   tids/0,
   term_to_table/1
]).

-define(SHARDS_N, 16).


-spec create(non_neg_integer()) -> ok.
create(KeyPos) ->
    Opts = [named_table, public, {keypos, KeyPos}, {read_concurrency, true}],
    [ets:new(list_to_atom(TableName), Opts) || TableName <- table_names()],
    ok.


-spec tids() -> [atom()].
tids() ->
    [list_to_existing_atom(TableName) || TableName <- table_names()].


-spec term_to_table(any()) -> atom().
term_to_table(Term) ->
    PHash = erlang:phash2(Term),
    list_to_existing_atom(table_name(PHash rem ?SHARDS_N)).


-spec table_names() -> [string()].
table_names() ->
    [table_name(N) || N <- lists:seq(0, ?SHARDS_N - 1)].


-spec table_name(non_neg_integer()) -> string().
table_name(Id) when is_integer(Id), Id >= 0 andalso Id < ?SHARDS_N ->
    atom_to_list(?MODULE) ++ "_" ++ integer_to_list(Id).
