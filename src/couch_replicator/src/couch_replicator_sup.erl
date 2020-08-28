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


-export([
    start_link/0
]).

-export([
    init/1
]).


start_link() ->
    Backend = fabric2_node_types:is_type(replication),
    Frontend = fabric2_node_types:is_type(api_frontend),
    Arg = {Backend, Frontend},
    supervisor:start_link({local, ?MODULE}, ?MODULE, Arg).


init({Backend, Frontend}) ->
    Children = case {Backend, Frontend} of
        {true, true} -> backend() ++ frontend();
        {true, false} -> backend();
        {false, true} -> frontend();
        {false, false} -> []
    end,
    Flags =  #{
        strategy => rest_for_one,
        intensity => 1,
        period => 5
    },
    {ok, {Flags, Children}}.


backend() ->
    Timeout = 5000,
    [
        #{
            id => couch_replicator_connection,
            start => {couch_replicator_connection, start_link, []}
        },
        #{
            id => couch_replicator_rate_limiter,
            start => {couch_replicator_rate_limiter, start_link, []}
        },
        #{
            id => couch_replicator_job_server,
            start => {couch_replicator_job_server, start_link, [Timeout]},
            shutdown => Timeout
        }
    ].


frontend() ->
    [
        #{
            id => couch_replicator,
            start => {couch_replicator, ensure_rep_db_exists, []},
            restart => transient
        }
    ] ++ couch_epi:register_service(couch_replicator_epi, []).
