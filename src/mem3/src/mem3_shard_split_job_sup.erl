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

-module(mem3_shard_split_job_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    start_child/1,
    terminate_child/1,
    count_children/0,
    init/1
]).

-include("mem3_shard_split.hrl").


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_child(Job) ->
    supervisor:start_child(?MODULE, [Job]).


terminate_child(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).


count_children() ->
    Props = supervisor:count_children(?MODULE),
    proplists:get_value(active, Props).


init(_Args) ->
    Children = [
        {undefined,
            {mem3_shard_split_job, start_link, []},
            temporary,
            60000,
            worker,
            [mem3_shard_split_job]}
    ],
    {ok, {{simple_one_for_one, 10, 3}, Children}}.
