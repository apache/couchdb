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

-module(couch_srt_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

%% Set the child workers to have restart strategy set to `transient`
%% so that if a CSRT failure arrises that triggers the sup rate limiter
%% thresholds, that shutdown signal will bubble up here and be ignored,
%% as the use of transient specifies that `normal` and `shutdown` signals
%% are ignored.
%% Switch this to `permanent` once CSRT is out of experimental stage.
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {
            {one_for_one, 5, 10}, [
                ?CHILD(couch_srt_server, worker),
                ?CHILD(couch_srt_logger, worker)
            ]
        }}.
