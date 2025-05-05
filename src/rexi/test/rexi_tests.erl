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
            ?TDEF_FE(t_server_pid),
            ?TDEF_FE(t_cast),
            ?TDEF_FE(t_cast_explicit_caller),
            ?TDEF_FE(t_cast_ref),
            ?TDEF_FE(t_sync_cast),
            ?TDEF_FE(t_stream2),
            ?TDEF_FE(t_stream2_acks),
            ?TDEF_FE(t_stream2_cancel),
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
rpc_test_fun(stream2_init) ->
    rexi:stream2(stream1),
    rexi:stream_last(stream2);
rpc_test_fun(stream2_acks) ->
    rexi:stream2(a),
    rexi:stream2(b),
    rexi:stream2(c),
    rexi:stream2(d),
    rexi:stream2(e),
    rexi:stream2(f),
    rexi:stream2(g),
    rexi:stream_last(h);
rpc_test_fun(Arg) ->
    rexi:reply({Arg, get()}).

t_server_pid(_) ->
    ?assertMatch({RexiServer, node42} when is_atom(RexiServer), rexi_utils:server_pid(node42)).

t_cast(_) ->
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

t_cast_explicit_caller(_) ->
    put(nonce, yep),
    {CallerPid, CallerRef} = spawn_monitor(fun() ->
        receive
            Msg -> exit(Msg)
        end
    end),
    Ref = rexi:cast(node(), CallerPid, {?MODULE, rpc_test_fun, [potato]}),
    Result =
        receive
            {'DOWN', CallerRef, _, _, Exit} -> Exit
        end,
    ?assertMatch({Ref, {potato, [_ | _]}}, Result).

t_cast_ref(_) ->
    put(nonce, yesh),
    Ref = make_ref(),
    Ref2 = rexi:cast_ref(Ref, node(), {?MODULE, rpc_test_fun, [potato]}),
    ?assertEqual(Ref, Ref2),
    {Res, Dict} = recv(Ref),
    ?assertEqual(potato, Res),
    ?assertMatch(
        #{
            nonce := yesh,
            '$initial_call' := {?MODULE, rpc_test_fun, 1},
            rexi_from := {_Pid, Ref}
        },
        maps:from_list(Dict)
    ).

t_sync_cast(_) ->
    put(nonce, yup),
    Ref = rexi:cast(node(), self(), {?MODULE, rpc_test_fun, [potato]}, [sync]),
    {Res, Dict} = recv(Ref),
    ?assertEqual(potato, Res),
    ?assertMatch(
        #{
            nonce := yup,
            '$initial_call' := {?MODULE, rpc_test_fun, 1},
            rexi_from := {_Pid, _Ref}
        },
        maps:from_list(Dict)
    ).

t_stream2(_) ->
    % We act as the coordinator
    Ref = rexi:cast(node(), {?MODULE, rpc_test_fun, [stream2_init]}),
    rexi:stream_start(stream_init(Ref)),
    ?assertEqual(stream1, recv(Ref)),
    ?assertEqual(stream2, recv(Ref)),
    % No more messages
    ?assertEqual(timeout, recv(Ref)).

t_stream2_acks(_) ->
    Ref = rexi:cast(node(), {?MODULE, rpc_test_fun, [stream2_acks]}),
    {WPid, _Tag} = From = stream_init(Ref),
    Mon = monitor(process, WPid),
    rexi:stream_start(From),
    ?assertEqual(a, recv(Ref)),
    ?assertEqual(b, recv(Ref)),
    ?assertEqual(c, recv(Ref)),
    ?assertEqual(d, recv(Ref)),
    ?assertEqual(e, recv(Ref)),
    ?assertEqual(timeout, recv(Ref)),
    rexi:stream_ack(WPid),
    ?assertEqual(f, recv(Ref)),
    ?assertEqual(timeout, recv(Ref)),
    rexi:stream_ack(WPid),
    ?assertEqual(g, recv(Ref)),
    ?assertEqual(h, recv(Ref)),
    % Done streaming. Ensure worker is dead.
    ?assertEqual(timeout, recv(Ref)),
    Res =
        receive
            {'DOWN', Mon, _, _, Exit} -> Exit
        end,
    ?assertEqual(normal, Res).

t_stream2_cancel(_) ->
    Ref = rexi:cast(node(), {?MODULE, rpc_test_fun, [stream2_init]}),
    {WPid, _Tag} = From = stream_init(Ref),
    Mon = monitor(process, WPid),
    rexi:stream_cancel(From),
    Res =
        receive
            {'DOWN', Mon, _, _, Exit} -> Exit
        end,
    ?assertEqual(normal, Res).

t_cast_error(_) ->
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

stream_init(Ref) ->
    receive
        {Ref, From, rexi_STREAM_INIT} ->
            From
    end.

recv(Ref) when is_reference(Ref) ->
    receive
        {Ref, _, Msg} -> Msg;
        {Ref, Msg} -> Msg
    after 500 -> timeout
    end.
