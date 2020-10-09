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

-module(couch_jobs_sup).


-behaviour(supervisor).


-export([
    start_link/0
]).

-export([
    init/1
]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Flags = #{
        strategy => rest_for_one,
        intensity => 3,
        period => 10
    },
    Children = [
        #{
            id => couch_jobs_fdb,
            restart => transient,
            start => {couch_jobs_fdb, init_cache, []}
        },
        #{
            id => couch_jobs_activity_monitor_sup,
            restart => permanent,
            shutdown => brutal_kill,
            type => supervisor,
            start => {couch_jobs_activity_monitor_sup, start_link, []}
        },
        #{
            id => couch_jobs_notifier_sup,
            restart => permanent,
            shutdown => brutal_kill,
            type => supervisor,
            start => {couch_jobs_notifier_sup, start_link, []}
        },
        #{
            id => couch_jobs_server,
            restart => permanent,
            shutdown => brutal_kill,
            start => {couch_jobs_server, start_link, []}
        }
    ],
    {ok, {Flags, Children}}.
