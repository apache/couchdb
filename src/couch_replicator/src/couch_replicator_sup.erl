%
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

-module(couch_replicator_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    Children = [
        {couch_replication_event,
            {gen_event, start_link, [{local, couch_replication}]},
            permanent,
            brutal_kill,
            worker,
            dynamic},
       {couch_replicator_clustering,
            {couch_replicator_clustering, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_replicator_clustering]},
       {couch_replicator_connection,
            {couch_replicator_connection, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_replicator_connection]},
       {couch_replicator_rate_limiter,
            {couch_replicator_rate_limiter, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_replicator_rate_limiter]},
        {couch_replicator_scheduler_sup,
            {couch_replicator_scheduler_sup, start_link, []},
            permanent,
            infinity,
            supervisor,
            [couch_replicator_scheduler_sup]},
        {couch_replicator_scheduler,
            {couch_replicator_scheduler, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_replicator_scheduler]},
        {couch_replicator_doc_processor,
            {couch_replicator_doc_processor, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_replicator_doc_processor]},
        {couch_replicator,
            % This is a simple function call which does not create a process
            % but returns `ignore`. It is used to make sure each node
            % has a local `_replicator` database.
            {couch_replicator, ensure_rep_db_exists, []},
            transient,
            brutal_kill,
            worker,
            [couch_replicator]},
        {couch_replicator_db_changes,
            {couch_replicator_db_changes, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_multidb_changes]}
    ],
    {ok, {{rest_for_one,10,1}, Children}}.
