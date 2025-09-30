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

-module(mem3_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    % Some startup order constraints based on call dependencies:
    %
    % * mem3_events gen_event should be started before all the others
    %
    % * mem3_nodes gen_server is needed so everyone can call mem3:nodes()
    %
    % * mem3_sync_nodes needs to run before mem3_sync and
    %   mem3_sync_event_listener, so they can both can call
    %   mem3_sync_nodes:add/1
    %
    % * mem3_distribution force connects nodes from mem3:nodes(), so start it
    %   before mem3_sync since mem3_sync:initial_sync/0 expects the connected
    %   nodes to be there when calling mem3_sync_nodes:add(nodes())
    %
    % * mem3_sync_event_listener has to start after mem3_sync, so it can call
    %   mem3_sync:push/2
    %
    % * mem3_seeds and mem3_reshard_sup can wait till the end, as they will
    %   spawn background work that can go on for a while: seeding system dbs
    %   from other nodes running resharding jobs
    %
    Children = [
        child(mem3_events),
        child(mem3_nodes),
        child(mem3_shards),
        child(mem3_sync_nodes),
        child(mem3_distribution),
        child(mem3_sync),
        child(mem3_sync_event_listener),
        child(mem3_seeds),
        child(mem3_db_doc_updater),
        child(mem3_reshard_sup)
    ],
    {ok, {{rest_for_one, 10, 1}, couch_epi:register_service(mem3_epi, Children)}}.

child(mem3_events) ->
    MFA = {gen_event, start_link, [{local, mem3_events}]},
    {mem3_events, MFA, permanent, 1000, worker, dynamic};
child(mem3_reshard_sup = Child) ->
    MFA = {Child, start_link, []},
    {Child, MFA, permanent, infinity, supervisor, [Child]};
child(Child) ->
    {Child, {Child, start_link, []}, permanent, 1000, worker, [Child]}.
