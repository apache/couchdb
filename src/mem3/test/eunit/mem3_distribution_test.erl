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
    meck:new(mem3_util, [passthrough]),
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
            ?TDEF_FE(connect_to_unconnected_nodes),
            ?TDEF_FE(log_non_standard_nodedown),
            ?TDEF_FE(append_nodeup_event),
            ?TDEF_FE(append_nodedown_event),
            ?TDEF_FE(events_are_trimmed),
            ?TDEF_FE(ping_nodes_test),
            ?TDEF_FE(dead_nodes_test)
        ]
    }.

periodic_scheduler_works(_) ->
    St = sys:get_state(?MOD),
    {st, TRef, #{}} = St,
    TVal = erlang:read_timer(TRef),
    ?assert(is_integer(TVal)),
    ?assert(TVal > 0),
    ?assert(TVal =< 70000),
    {noreply, St1} = ?MOD:handle_info(connect, St),
    {st, TRef1, #{}} = St1,
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

log_non_standard_nodedown(_) ->
    meck:reset(couch_log),
    ?MOD ! {nodedown, 'baz@foo.net', [{nodedown_reason, potato}]},
    meck:wait(1, couch_log, warning, 2, 5000).

append_nodeup_event(_) ->
    ?assertEqual(#{}, mem3_distribution:events()),
    ?MOD ! {nodeup, 'foo@bar.org', []},
    test_util:wait(fun() ->
        case map_size(mem3_distribution:events()) == 0 of
            true -> wait;
            false -> ok
        end
    end),
    Events = mem3_distribution:events(),
    ?assert(is_map_key('foo@bar.org', Events)),
    #{'foo@bar.org' := NodeEvents} = Events,
    ?assertMatch([[<<"2", _/binary>>, nodeup]], NodeEvents).

append_nodedown_event(_) ->
    ?assertEqual(#{}, mem3_distribution:events()),
    ?MOD ! {nodedown, 'foo@bar.org', [{nodedown_reason, badness}]},
    test_util:wait(fun() ->
        case map_size(mem3_distribution:events()) == 0 of
            true -> wait;
            false -> ok
        end
    end),
    Events = mem3_distribution:events(),
    ?assert(is_map_key('foo@bar.org', Events)),
    #{'foo@bar.org' := NodeEvents} = Events,
    ?assertMatch([[<<_/binary>>, nodedown, badness]], NodeEvents).

events_are_trimmed(_) ->
    ?assertEqual(#{}, mem3_distribution:events()),
    [?MOD ! {nodedown, 'x', [{nodedown_reason, I}]} || I <- lists:seq(1, 10)],
    test_util:wait(fun() ->
        case mem3_distribution:events() of
            #{x := [[_, nodedown, <<"4">>] | _]} ->
                ok;
            #{} ->
                wait
        end
    end),
    Events = mem3_distribution:events(),
    #{x := NodeEvents} = Events,
    ?assertEqual(7, length(NodeEvents)),
    ?assertMatch([<<_/binary>>, nodedown, <<"4">>], hd(NodeEvents)),
    ?assertMatch([<<_/binary>>, nodedown, <<"10">>], lists:last(NodeEvents)).

ping_nodes_test(_) ->
    meck:expect(mem3_util, live_nodes, 0, [n1, n2]),
    ?assertEqual(
        [
            {n1, {nodedown, n1}},
            {n2, {nodedown, n2}}
        ],
        couch_debug:ping_live_cluster_nodes()
    ),
    ?assertEqual({nodedown, n3}, couch_debug:ping(n3, 100)).

dead_nodes_test(_) ->
    meck:expect(mem3, nodes, 0, [n1, n2, n3]),
    meck:expect(mem3_util, live_nodes, 0, [n1, n2]),
    Node = node(),
    ?assertEqual([{Node, [n3]}], couch_debug:dead_nodes()).
