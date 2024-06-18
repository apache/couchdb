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
        #{
            id => couch_replication_event,
            start => {gen_event, start_link, [{local, couch_replication}]},
            modules => dynamic
        },
        #{
            id => couch_replicator_pg,
            start => {couch_replicator_pg, start_link, []},
            shutdown => 1000,
            modules => [pg]
        },
        worker(couch_replicator_connection),
        worker(couch_replicator_rate_limiter),
        worker(couch_replicator_scheduler),
        worker(couch_replicator_doc_processor)
    ],
    SupFlags = #{strategy => rest_for_one, intensity => 10, period => 1},
    {ok, {SupFlags, Children}}.

worker(Mod) ->
    #{id => Mod, start => {Mod, start_link, []}}.
