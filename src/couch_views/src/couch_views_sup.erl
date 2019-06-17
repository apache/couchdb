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


-module(couch_views_sup).


-behaviour(supervisor).


-export([
    start_link/1
]).


-export([
    init/1
]).


start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).


init([]) ->
    Flags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    Children = [
        #{
            id => couch_views_server,
            start => {couch_views_server, start_link, []}
        }
    ],
    {ok, {Flags, Children}}.
