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

-module(mem3_cluster_test).

-behavior(mem3_cluster).

-include_lib("eunit/include/eunit.hrl").

-export([
    cluster_unstable/1,
    cluster_stable/1
]).

% Mem3 cluster callbacks

cluster_unstable(Server) ->
    Server ! cluster_unstable,
    Server.

cluster_stable(Server) ->
    Server ! cluster_stable,
    Server.

mem3_cluster_test_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            t_cluster_stable_during_startup_period(),
            t_cluster_unstable_delivered_on_nodeup(),
            t_cluster_unstable_delivered_on_nodedown(),
            t_wait_period_is_reset_after_last_change()
        ]
    }.

t_cluster_stable_during_startup_period() ->
    ?_test(begin
        {ok, Pid} = mem3_cluster:start_link(?MODULE, self(), 1, 2),
        register(?MODULE, Pid),
        receive
            cluster_stable ->
                ?assert(true)
        after 1500 ->
            throw(timeout)
        end,
        unlink(Pid),
        exit(Pid, kill)
    end).

t_cluster_unstable_delivered_on_nodeup() ->
    ?_test(begin
        {ok, Pid} = mem3_cluster:start_link(?MODULE, self(), 1, 2),
        register(?MODULE, Pid),
        Pid ! {nodeup, node()},
        receive
            cluster_unstable ->
                ?assert(true)
        after 1000 ->
            throw(timeout)
        end,
        unlink(Pid),
        exit(Pid, kill)
    end).

t_cluster_unstable_delivered_on_nodedown() ->
    ?_test(begin
        {ok, Pid} = mem3_cluster:start_link(?MODULE, self(), 1, 2),
        register(?MODULE, Pid),
        Pid ! {nodedown, node()},
        receive
            cluster_unstable ->
                ?assert(true)
        after 1000 ->
            throw(timeout)
        end,
        unlink(Pid),
        exit(Pid, kill)
    end).

t_wait_period_is_reset_after_last_change() ->
    ?_test(begin
        {ok, Pid} = mem3_cluster:start_link(?MODULE, self(), 1, 1),
        register(?MODULE, Pid),
        timer:sleep(800),
        % after 800 sec send a nodeup
        Pid ! {nodeup, node()},
        receive
            cluster_stable ->
                ?assert(false)
        after 400 ->
            % stability check should have been reset
            ?assert(true)
        end,
        timer:sleep(1000),
        receive
            cluster_stable ->
                ?assert(true)
        after 0 ->
            % cluster_stable arrives after enough quiet time
            ?assert(false)
        end,
        unlink(Pid),
        exit(Pid, kill)
    end).

% Test helper functions

setup() ->
    ok.

teardown(_) ->
    case whereis(?MODULE) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            unlink(Pid),
            exit(Pid, kill)
    end.
