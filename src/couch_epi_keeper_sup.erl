% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_epi_keeper_sup).

-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([start_child/2, terminate_child/1]).

%% ------------------------------------------------------------------
%% supervisor Function Exports
%% ------------------------------------------------------------------

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Codegen, Module) ->
    supervisor:start_child(?MODULE, [Codegen, Module]).

terminate_child(undefined) -> ok;
terminate_child(Child) when is_atom(Child) ->
    terminate_child(whereis(Child));
terminate_child(ChildPid) ->
    supervisor:terminate_child(?MODULE, ChildPid).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children = [
        ?CHILD(couch_epi_module_keeper, worker)
    ],
    {ok, { {simple_one_for_one, 5, 10}, Children} }.
