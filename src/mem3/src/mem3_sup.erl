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
    Children = [
        child(mem3_events),
        child(mem3_nodes),
        child(mem3_seeds),
        child(mem3_sync_nodes), % Order important?
        child(mem3_sync),
        child(mem3_shards),
        child(mem3_sync_event_listener),
        child(mem3_shard_split_sup)
    ],
    {ok, {{one_for_one,10,1}, couch_epi:register_service(mem3_epi, Children)}}.

child(mem3_events) ->
    MFA = {gen_event, start_link, [{local, mem3_events}]},
    {mem3_events, MFA, permanent, 1000, worker, dynamic};
child(mem3_shard_split_sup = Child) ->
    MFA = {Child, start_link, []},
    {Child, MFA, permanent, infinity, supervisor, [Child]};
child(Child) ->
    {Child, {Child, start_link, []}, permanent, 1000, worker, [Child]}.
