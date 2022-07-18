%% -------------------------------------------------------------------
%%
%% weatherreport - automated diagnostic tools for CouchDB
%%
%% Copyright (c) 2014 Cloudant
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc <p>The <code>weatherreport_runner</code> module provides
%% utility functions for running checks either on a single node or
%% multiple nodes.

-module(weatherreport_runner).

-export([run/1, run/2, format/1]).

%% @doc Run the supplied list of checks on the local node
-spec run([Module :: atom()]) -> [tuple()].
run(Checks) ->
    weatherreport_node:can_connect(),
    run(Checks, [weatherreport_node:nodename()]).

%% @doc Run the supplied list of checks on the supplied list of cluster nodes
-spec run([Module :: atom()], [node()] | all) -> [tuple()].
run(Checks, all) ->
    weatherreport_node:can_connect(),
    case weatherreport_node:local_command(mem3, nodes, []) of
        ClusterNodes when is_list(ClusterNodes) ->
            run(Checks, ClusterNodes);
        Error ->
            [{node(), critical, weatherreport_runner, {checks_failed, Error}}]
    end;
run(Checks, Nodes) ->
    CheckOpts = get_check_options(),
    lists:flatten(
        lists:foldl(
            fun(Mod, Acc) ->
                {Resps, BadNodes} = weatherreport_node:multicall(
                    Nodes,
                    erlang,
                    apply,
                    [fun() -> {node(), weatherreport_check:check(Mod, CheckOpts)} end, []],
                    weatherreport_config:timeout()
                ),
                TransformFailedCheck = fun(Node) ->
                    {node(), crit, weatherreport_runner, {check_failed, Mod, Node}}
                end,
                FailedChecks = [TransformFailedCheck(Node) || Node <- BadNodes],
                TransformResponse = fun
                    ({badrpc, Error}) ->
                        [{node(), crit, weatherreport_runner, {badrpc, Mod, Error}}];
                    ({Node, Messages}) ->
                        [{Node, Lvl, Module, Msg} || {Lvl, Module, Msg} <- Messages]
                end,
                Responses = [TransformResponse(Resp) || Resp <- Resps],
                [Responses ++ FailedChecks | Acc]
            end,
            [],
            Checks
        )
    ).

%% @doc Part of the weatherreport_check behaviour. This means that any messages
%% returned by this module can be handled via the existing message reporting
%% code.
format({checks_failed, Error}) ->
    {"Could not run checks - received error: ~w", [Error]};
format({check_failed, Check, Node}) ->
    {"Could not run check ~w on cluster node ~w", [Check, Node]};
format({badrpc, Check, Error}) ->
    {"Bad rpc call executing check ~w: ~w", [Check, Error]}.

%% Private functions
get_check_options() ->
    Expert =
        case application:get_env(weatherreport, expert) of
            {ok, true} ->
                true;
            _ ->
                false
        end,
    [{expert, Expert}].
