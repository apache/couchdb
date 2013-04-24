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

% This is named couch_event_sup2 to avoid
% naming collisions with the couch_event_sup
% module contained in the couch app. When
% that supervisor is removed we'll be free
% to rename this one.

-module(couch_event_sup2).
-behavior(supervisor).


-export([
    start_link/0,
    init/1
]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, nil).


init(_) ->
    Children = [
        {couch_event_registry,
            {couch_event_registry, start_link, []},
            permanent,
            5000,
            worker,
            [couch_event_registry]
        },
        {couch_event_dist,
            {couch_event_dist, start_link, []},
            permanent,
            5000,
            worker,
            [couch_event_dist]
        },
        {couch_event_os_sup,
            {couch_event_os_sup, start_link, []},
            permanent,
            5000,
            supervisor,
            [couch_event_os_sup]
        }
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.

