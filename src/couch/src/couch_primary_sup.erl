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

-module(couch_primary_sup).
-behaviour(supervisor).
-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local, couch_primary_services}, ?MODULE, []).

init([]) ->
    Children =
        [
            {couch_task_status, {couch_task_status, start_link, []}, permanent, brutal_kill, worker,
                [couch_task_status]},
            {couch_password_hasher, {couch_password_hasher, start_link, []}, permanent, brutal_kill,
                worker, [couch_password_hasher]},
            {couch_passwords_cache_lru,
                {ets_lru, start_link, [
                    couch_passwords_cache_lru,
                    [
                        {max_objects,
                            config:get_integer("couch_passwords_cache", "max_objects", 10_000)},
                        {max_idle, config:get_integer("couch_passwords_cache", "max_idle", 600_000)}
                    ]
                ]},
                permanent, 5000, worker, [ets_lru]}
        ] ++ couch_servers(),
    {ok, {{one_for_one, 10, 3600}, Children}}.

couch_servers() ->
    N = couch_server:num_servers(),
    [couch_server(I) || I <- lists:seq(1, N)].

couch_server(N) ->
    Name = couch_server:couch_server(N),
    {Name, {couch_server, sup_start_link, [N]}, permanent, brutal_kill, worker, [couch_server]}.
