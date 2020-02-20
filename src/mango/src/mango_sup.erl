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

-module(mango_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/1]).


start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).

init([]) ->
    Flags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 10
    },

    Args = [{worker, mango_jobs_indexer}],
    Children = [
        #{
            id => mango_indexer_server,
            start => {couch_views_server, start_link, [Args]}
        }
    ] ++ couch_epi:register_service(mango_epi, []),
    {ok, {Flags, Children}}.
