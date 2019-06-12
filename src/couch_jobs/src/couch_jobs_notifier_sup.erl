%
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

-module(couch_jobs_notifier_sup).


-behaviour(supervisor).


-export([
    start_link/0,

    start_notifier/1,
    stop_notifier/1,
    get_child_pids/0
]).

-export([
    init/1
]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


start_notifier(Type) ->
    supervisor:start_child(?MODULE, [Type]).


stop_notifier(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).


get_child_pids() ->
    lists:map(fun({_Id, Pid, _Type, _Mod}) ->
        Pid
    end, supervisor:which_children(?MODULE)).


init(_) ->
    Flags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 3
    },
    Children = [
        #{
            id => couch_jobs_notifier,
            restart => temporary,
            start => {couch_jobs_notifier, start_link, []}
        }
    ],
    {ok, {Flags, Children}}.
