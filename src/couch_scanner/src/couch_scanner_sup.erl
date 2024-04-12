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

-module(couch_scanner_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{
            id => couch_scanner_rate_limiter,
            start => {couch_scanner_rate_limiter, start_link, []},
            shutdown => 5000
        },
        #{
            id => couch_scanner_server,
            start => {couch_scanner_server, start_link, []},
            shutdown => 5000
        }
    ],
    SupFlags = #{strategy => rest_for_one, intensity => 25, period => 1},
    {ok, {SupFlags, Children}}.
