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

-module(mem3_distribution_test).

-include_lib("couch/include/couch_eunit.hrl").

-define(MOD, mem3_distribution).

setup() ->
    Ctx = test_util:start_couch([mem3]),
    meck:new(mem3, [passthrough]),
    meck:new(mem3_distribution, [passthrough]),
    meck:new(couch_log, [passthrough]),
    Ctx.

teardown(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).

mem3_distribution_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(periodic_scheduler_works),
            ?TDEF_FE(connect_to_unconnected_nodes)
        ]
    }.

periodic_scheduler_works(_) ->
    St = sys:get_state(?MOD),
    {st, TRef} = St,
    TVal = erlang:read_timer(TRef),
    ?assert(is_integer(TVal)),
    ?assert(TVal > 0),
    ?assert(TVal =< 70000),
    {noreply, St1} = ?MOD:handle_info(connect, St),
    {st, TRef1} = St1,
    ?assertNotEqual(TRef, TRef1),
    TVal1 = erlang:read_timer(TRef1),
    ?assert(is_integer(TVal1)).

connect_to_unconnected_nodes(_) ->
    Nodes = ['foo', 'bar'],
    meck:expect(mem3, nodes, 0, Nodes),
    meck:reset(?MOD),
    % Simulate connect timer expiry
    ?MOD ! connect,
    meck:wait(?MOD, connect_node, [foo], 5000),
    meck:wait(?MOD, connect_node, [bar], 5000),
    % connect_node returns false => no reconnection log
    timer:sleep(100),
    ?assertEqual(0, meck:num_calls(couch_log, warning, 2)),
    % Make connect return true
    meck:reset(?MOD),
    meck:expect(?MOD, connect_node, 1, true),
    % Simulate connect timer expiry
    ?MOD ! connect,
    meck:wait(?MOD, connect_node, [foo], 5000),
    meck:wait(?MOD, connect_node, [bar], 5000),
    % connect_node returns true => emit reconnection log
    meck:wait(2, couch_log, warning, 2, 5000).
