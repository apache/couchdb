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

-module(ioq_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-export([handle_config_change/5, handle_config_terminate/3]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
        {
            config_listener_mon,
            {config_listener_mon, start_link, [?MODULE, nil]},
            permanent,
            5000,
            worker,
            [config_listener_mon]
        },
        ?CHILD(ioq, worker)
    ]} }.

handle_config_change("ioq", _Key, _Val, _Persist, St) ->
    gen_server:cast(ioq_server, update_config),
    {ok, St};
handle_config_change(_Sec, _Key, _Val, _Persist, St) ->
    {ok, St}.

handle_config_terminate(_Server, _Reason, _State) ->
    gen_server:cast(ioq_server, update_config),
    ok.
