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

-module(mem3_reshard_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    Children = [
        {mem3_reshard_dbdoc, {mem3_reshard_dbdoc, start_link, []}, permanent, infinity, worker, [
            mem3_reshard_dbdoc
        ]},
        {mem3_reshard_job_sup, {mem3_reshard_job_sup, start_link, []}, permanent, infinity,
            supervisor, [mem3_reshard_job_sup]},
        {mem3_reshard, {mem3_reshard, start_link, []}, permanent, brutal_kill, worker, [
            mem3_reshard
        ]}
    ],
    {ok, {{one_for_all, 5, 5}, Children}}.
