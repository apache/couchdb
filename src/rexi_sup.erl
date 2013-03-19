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

-module(rexi_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).

init([]) ->
    {ok, {{one_for_one, 3, 10}, [
        {
            rexi_gov_manager,
            {rexi_gov_manager, start_link, []},
            permanent,
            100,
            worker,
            [rexi_gov_manager]
        },
        {
            rexi_server,
            {rexi_server, start_link, [rexi_server]},
            permanent,
            100,
            worker,
            [rexi_server]
        },
        {
            rexi_server_sup,
            {rexi_server_sup, start_link, []},
            permanent,
            100,
            supervisor,
            [rexi_server_sup]
        },
        {
            rexi_server_mon,
            {rexi_server_mon, start_link, []},
            permanent,
            100,
            worker,
            [rexi_server_mon]
        }
    ]}}.
