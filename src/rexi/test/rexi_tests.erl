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

-module(rexi_tests).

-export([
    rpc_test_fun/1
]).

-include_lib("couch/include/couch_eunit.hrl").

rexi_buffer_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_cast),
            ?TDEF_FE(t_sync_cast),
            ?TDEF_FE(t_kill),
            ?TDEF_FE(t_cast_error),
            ?TDEF_FE(t_metrics),
            ?TDEF_FE(t_ping)
        ]
    }.

setup() ->
    test_util:start_couch([rexi]).

teardown(Ctx) ->
    test_util:stop_couch(Ctx).

rpc_test_fun({sleep, MSec}) ->
    rexi:reply({sleeping, self()}),
    timer:sleep(MSec);
rpc_test_fun({error, Error}) ->
    error(Error);
rpc_test_fun(ping) ->
    rexi:ping();
rpc_test_fun(Arg) ->
    rexi:reply({Arg, get()}).

t_cast(_) ->
    ?assertMatch({RexiServer, node42} when is_atom(RexiServer), rexi_utils:server_pid(node42)),
    put(nonce, yup),
    Ref = rexi:cast(node(), {?MODULE, rpc_test_fun, [potato]}),
    {Res, Dict} =
        receive
            {Ref, {R, D}} -> {R, maps:from_list(D)}
        end,
    ?assertEqual(potato, Res),
    ?assertMatch(
        #{
            nonce := yup,
            '$initial_call' := {?MODULE, rpc_test_fun, 1},
            rexi_from := {_Pid, _Ref}
        },
        Dict
    ).

t_sync_cast(_) ->
    ?assertMatch({RexiServer, node42} when is_atom(RexiServer), rexi_utils:server_pid(node42)),
    put(nonce, yup),
    Ref = rexi:cast(node(), self(), {?MODULE, rpc_test_fun, [potato]}, [sync]),
    {Res, Dict} =
        receive
            {Ref, {R, D}} -> {R, maps:from_list(D)}
        end,
    ?assertEqual(potato, Res),
    ?assertMatch(
        #{
            nonce := yup,
            '$initial_call' := {?MODULE, rpc_test_fun, 1},
            rexi_from := {_Pid, _Ref}
        },
        Dict
    ).

t_cast_error(_) ->
    ?assertMatch({RexiServer, node42} when is_atom(RexiServer), rexi_utils:server_pid(node42)),
    Ref = rexi:cast(node(), self(), {?MODULE, rpc_test_fun, [{error, tomato}]}, []),
    Res =
        receive
            {Ref, RexiExit} -> RexiExit
        end,
    ?assertMatch({rexi_EXIT, {tomato, [{?MODULE, rpc_test_fun, 1, _} | _]}}, Res).

t_kill(_) ->
    Ref = rexi:cast(node(), {?MODULE, rpc_test_fun, [{sleep, 10000}]}),
    WorkerPid =
        receive
            {Ref, {sleeping, Pid}} -> Pid
        end,
    ?assert(is_process_alive(WorkerPid)),
    Mon = monitor(process, WorkerPid),
    rexi:kill_all([{node(), Ref}]),
    KillReason =
        receive
            {'DOWN', Mon, _, _, Res} -> Res
        end,
    ?assertEqual(killed, KillReason).

t_metrics(_) ->
    ?assertEqual(0, rexi:aggregate_buffer_queue_len()),
    ?assertEqual(0, rexi:aggregate_server_queue_len()).

t_ping(_) ->
    rexi:cast(node(), {?MODULE, rpc_test_fun, [ping]}),
    Res =
        receive
            {rexi, Ping} -> Ping
        end,
    ?assertEqual('$rexi_ping', Res).
