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
    start_link/0
]).


-export([
    init/1
]).


start_link() ->
    Arg = case fabric2_node_types:is_type(view_indexing) of
        true -> normal;
        false -> builds_disabled
    end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, Arg).


init(normal) ->
    Args = [{worker, couch_views_indexer}],
    Children = [
        #{
            id => couch_views_server,
            start => {couch_views_server, start_link, [Args]}
        }
    ],
    {ok, {flags(), Children}};

init(builds_disabled) ->
    couch_log:notice("~p : view_indexing disabled", [?MODULE]),
    couch_views_jobs:set_timeout(),
    {ok, {flags(), []}}.


flags() ->
    #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    }.
