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

-module(rexi_buffer_tests).

-include_lib("couch/include/couch_eunit.hrl").

rexi_buffer_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_send),
            ?TDEF_FE(t_get_buffered_count),
            ?TDEF_FE(t_buffer_erase),
            ?TDEF_FE(t_terminate_clears_persistent_term)
        ]
    }.

setup() ->
    Module = atom_to_binary(?MODULE),
    RandSuffix = binary:encode_hex(rand:bytes(4)),
    ServerId = binary_to_atom(<<Module/binary, "_", RandSuffix/binary>>),
    {ok, Pid} = rexi_buffer:start_link(ServerId),
    unlink(Pid),
    {ServerId, Pid}.

teardown({_ServerId, Pid}) ->
    case is_process_alive(Pid) of
        true -> test_util:stop_sync(Pid);
        false -> ok
    end.

t_send({ServerId, Pid}) ->
    ?assert(is_process_alive(Pid)),
    ?assertEqual(Pid, whereis(ServerId)),
    {DestPid, DestRef} = spawn_monitor(fun() ->
        receive
            Msg -> exit({got, Msg})
        end
    end),
    gen_server:cast(ServerId, {deliver, DestPid, potato}),
    ReceivedVal =
        receive
            {'DOWN', DestRef, process, DestPid, Res} -> Res
        end,
    ?assertEqual({got, potato}, ReceivedVal).

t_get_buffered_count({ServerId, _}) ->
    NonExistentDest = {foo, 'nonexistent@127.0.0.1'},
    ?assertEqual(0, rexi_buffer:get_buffered_count('nonexistent_server_id')),
    ?assertEqual(0, rexi_buffer:get_buffered_count(ServerId)),
    % Set a fake sender to make the buffer block
    sys:replace_state(ServerId, fun(OldSt) -> setelement(4, OldSt, {foo, bar}) end),
    gen_server:cast(ServerId, {deliver, NonExistentDest, potato}),
    test_util:wait(fun() ->
        case rexi_buffer:get_buffered_count(ServerId) of
            0 -> wait;
            N when is_integer(N), N > 0 -> ok
        end
    end),
    ?assertEqual(1, rexi_buffer:get_buffered_count(ServerId)),
    gen_server:cast(ServerId, {deliver, NonExistentDest, tomato}),
    gen_server:cast(ServerId, {deliver, NonExistentDest, cabbage}),
    test_util:wait(fun() ->
        case rexi_buffer:get_buffered_count(ServerId) of
            N when is_integer(N), N =< 2 -> wait;
            N when is_integer(N), N > 2 -> ok
        end
    end),
    ?assertEqual(3, rexi_buffer:get_buffered_count(ServerId)),
    % Unblock sender
    sys:replace_state(ServerId, fun(OldSt) -> setelement(4, OldSt, nil) end),
    gen_server:cast(ServerId, {deliver, NonExistentDest, cucumber}),
    test_util:wait(fun() ->
        case rexi_buffer:get_buffered_count(ServerId) of
            N when is_integer(N), N > 0 -> wait;
            0 -> ok
        end
    end),
    ?assertEqual(ok, rexi_buffer:erase_buffer(ServerId)),
    ?assertEqual(0, rexi_buffer:get_buffered_count(ServerId)).

t_buffer_erase({ServerId, _}) ->
    NonExistentDest = {foo, 'nonexistent@127.0.0.1'},
    ?assertEqual(0, rexi_buffer:get_buffered_count('nonexistent_server_id')),
    ?assertEqual(0, rexi_buffer:get_buffered_count(ServerId)),
    % Set a fake sender to make the buffer block
    sys:replace_state(ServerId, fun(OldSt) -> setelement(4, OldSt, {foo, bar}) end),
    gen_server:cast(ServerId, {deliver, NonExistentDest, potato}),
    test_util:wait(fun() ->
        case rexi_buffer:get_buffered_count(ServerId) of
            0 -> wait;
            N when is_integer(N), N > 0 -> ok
        end
    end),
    ?assertEqual(1, rexi_buffer:get_buffered_count(ServerId)),
    ?assertEqual(ok, rexi_buffer:erase_buffer(ServerId)),
    ?assertEqual(0, rexi_buffer:get_buffered_count(ServerId)).

t_terminate_clears_persistent_term({ServerId, Pid}) ->
    ?assertNotEqual(undefined, persistent_term:get({rexi_buffer, counter, ServerId}, undefined)),
    ?assertEqual(ok, gen_server:stop(Pid, shutdown, infinity)),
    ?assertEqual(undefined, persistent_term:get({rexi_buffer, counter, ServerId}, undefined)).
