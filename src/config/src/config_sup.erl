% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(config_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(IniFiles) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, IniFiles).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(IniFiles) ->
    Children = [
        {config, {config, start_link, [IniFiles]}, permanent, 5000, worker, [config]},
        {config_event, {gen_event, start_link, [{local, config_event}]}, permanent, 5000, worker,
            dynamic}
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.
