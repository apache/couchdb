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

-module(ctrace_sup).
-behaviour(supervisor).
-vsn(1).

-export([
    start_link/0,
    init/1
]).

start_link() ->
    ctrace_config:update(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Flags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    Children = [
        #{
            id => config_listener_mon,
            type => worker,
            restart => permanent,
            shutdown => 5000,
            start => {config_listener_mon, start_link, [ctrace_config, nil]}
        }
    ],
    {ok, {Flags, Children}}.