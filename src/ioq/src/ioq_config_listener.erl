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

-module(ioq_config_listener).

-vsn(2).
-behaviour(config_listener).

-export([
    subscribe/0
]).

-export([
    handle_config_change/5,
    handle_config_terminate/3
]).

subscribe() ->
    config:listen_for_changes(?MODULE, nil).

handle_config_change("ioq", _Key, _Val, _Persist, St) ->
    ok = notify_ioq_pids(),
    {ok, St};
handle_config_change("ioq2", _Key, _Val, _Persist, St) ->
    ok = notify_ioq_pids(),
    {ok, St};
handle_config_change("ioq2."++_Type, _Key, _Val, _Persist, St) ->
    ok = notify_ioq_pids(),
    {ok, St};
handle_config_change(_Sec, _Key, _Val, _Persist, St) ->
    {ok, St}.

handle_config_terminate(_, stop, _) -> ok;
handle_config_terminate(_, _, _) ->
    % We may have missed a change in the last five seconds
    gen_server:cast(ioq_server, update_config),
    spawn(fun() ->
        timer:sleep(5000),
        config:listen_for_changes(?MODULE, nil)
    end).

notify_ioq_pids() ->
    ok = lists:foreach(fun(Pid) ->
        gen_server:cast(Pid, update_config)
    end, ioq_sup:get_ioq2_servers()).
