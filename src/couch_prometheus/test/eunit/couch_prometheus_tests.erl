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

-module(couch_prometheus_tests).

-include_lib("couch/include/couch_eunit.hrl").

unit_test_() ->
    {
        "Unit tests for system stats metrics",
        {
            setup,
            fun setup_unit/0,
            fun teardown_unit/1,
            with([
                ?TDEF(t_clouseau_connected),
                ?TDEF(t_clouseau_disconnected),
                ?TDEF(t_database_count_success),
                ?TDEF(t_database_count_error),
                ?TDEF(t_ioq_stats_basic),
                ?TDEF(t_ioq_stats_with_channels),
                ?TDEF(t_ioq_stats_couchdb_ioq_format),
                ?TDEF(t_ioq_stats_couchdb_ioq_format_empty),
                ?TDEF(t_smoosh_stats_success),
                ?TDEF(t_smoosh_stats_error)
            ])
        }
    }.

setup_unit() ->
    Mods = [clouseau_rpc, fabric, ioq, smoosh, config],
    lists:foreach(fun(M) -> meck:new(M, [passthrough]) end, Mods),
    Mods.

teardown_unit(Mods) ->
    lists:foreach(fun meck:unload/1, Mods).

t_clouseau_connected(_) ->
    meck:expect(clouseau_rpc, connected, [], meck:val(true)),
    Result = couch_prometheus:get_clouseau_status(),
    ?assertMatch([_, _, <<"couchdb_clouseau_connected 1">>], Result).

t_clouseau_disconnected(_) ->
    meck:expect(clouseau_rpc, connected, [], meck:val(false)),
    Result = couch_prometheus:get_clouseau_status(),
    ?assertMatch([_, _, <<"couchdb_clouseau_connected 0">>], Result).

t_database_count_success(_) ->
    meck:expect(config, get, 3, meck:val("_dbs")),
    meck:expect(fabric, get_db_info, 1, meck:val({ok, [{doc_count, 42}]})),
    Result = couch_prometheus:get_database_count(),
    ?assertMatch([_, _, <<"couchdb_database_count 42">>], Result).

t_database_count_error(_) ->
    meck:expect(config, get, 3, meck:val("_dbs")),
    meck:expect(fabric, get_db_info, 1, meck:val({error, not_found})),
    Result = couch_prometheus:get_database_count(),
    ?assertEqual([], Result).

t_ioq_stats_basic(_) ->
    meck:expect(
        ioq,
        get_disk_queues,
        [],
        meck:val([
            {compaction, 1},
            {low, 2},
            {replication, 3}
        ])
    ),
    Result = couch_prometheus:get_ioq_stats(),
    ?assert(is_list(Result)),
    ?assertEqual(6, length(Result)),
    % Verify total requests = 1 + 2 + 3 = 6
    [_, _, TotalLine] = lists:last(Result),
    ?assertMatch(<<"couchdb_ioq_total_requests 6">>, TotalLine).

t_ioq_stats_with_channels(_) ->
    meck:expect(
        ioq,
        get_disk_queues,
        [],
        meck:val([
            {compaction, 1},
            {low, 2},
            {replication, 3},
            {channels, {[{<<"user1">>, [1, 2, 3]}]}}
        ])
    ),
    Result = couch_prometheus:get_ioq_stats(),
    ?assert(is_list(Result)),
    ?assertEqual(6, length(Result)),
    % Verify channel requests = 1 + 2 + 3 = 6
    [_, _, ChannelLine] = lists:nth(5, Result),
    ?assertMatch(<<"couchdb_ioq_channel_requests 6">>, ChannelLine),
    % Verify total requests = 1 + 2 + 3 + 6 = 12
    [_, _, TotalLine] = lists:last(Result),
    ?assertMatch(<<"couchdb_ioq_total_requests 12">>, TotalLine).

t_ioq_stats_couchdb_ioq_format(_) ->
    meck:expect(
        ioq,
        get_disk_queues,
        [],
        meck:val([
            {interactive, 10},
            {background, 5}
        ])
    ),
    Result = couch_prometheus:get_ioq_stats(),
    ?assert(is_list(Result)),
    ?assertEqual(3, length(Result)),
    % Verify interactive requests
    [_, _, InteractiveLine] = lists:nth(1, Result),
    ?assertMatch(<<"couchdb_ioq_interactive_requests 10">>, InteractiveLine),
    % Verify background requests
    [_, _, BackgroundLine] = lists:nth(2, Result),
    ?assertMatch(<<"couchdb_ioq_background_requests 5">>, BackgroundLine),
    % Verify total requests = 10 + 5 = 15
    [_, _, TotalLine] = lists:nth(3, Result),
    ?assertMatch(<<"couchdb_ioq_total_requests 15">>, TotalLine).

t_ioq_stats_couchdb_ioq_format_empty(_) ->
    meck:expect(
        ioq,
        get_disk_queues,
        [],
        meck:val([
            {interactive, 0},
            {background, 0}
        ])
    ),
    Result = couch_prometheus:get_ioq_stats(),
    ?assert(is_list(Result)),
    ?assertEqual(3, length(Result)),
    % Verify all metrics are 0
    [_, _, InteractiveLine] = lists:nth(1, Result),
    ?assertMatch(<<"couchdb_ioq_interactive_requests 0">>, InteractiveLine),
    [_, _, BackgroundLine] = lists:nth(2, Result),
    ?assertMatch(<<"couchdb_ioq_background_requests 0">>, BackgroundLine),
    [_, _, TotalLine] = lists:nth(3, Result),
    ?assertMatch(<<"couchdb_ioq_total_requests 0">>, TotalLine).

t_smoosh_stats_success(_) ->
    meck:expect(
        smoosh,
        status,
        [],
        meck:val(
            {ok, #{
                channels => #{
                    <<"ratio_dbs">> => #{
                        active => 2,
                        starting => 1,
                        waiting => #{size => 5}
                    }
                }
            }}
        )
    ),
    Result = couch_prometheus:get_smoosh_stats(),
    ?assert(is_list(Result)),
    ?assertEqual(4, length(Result)),
    [_, _, ChannelLine] = lists:nth(1, Result),
    ?assertMatch(<<"couchdb_smoosh_channel_count 1">>, ChannelLine),
    [_, _, ActiveLine] = lists:nth(2, Result),
    ?assertMatch(<<"couchdb_smoosh_active_jobs 2">>, ActiveLine),
    [_, _, StartingLine] = lists:nth(3, Result),
    ?assertMatch(<<"couchdb_smoosh_starting_jobs 1">>, StartingLine),
    [_, _, WaitingLine] = lists:nth(4, Result),
    ?assertMatch(<<"couchdb_smoosh_waiting_jobs 5">>, WaitingLine).

t_smoosh_stats_error(_) ->
    meck:expect(smoosh, status, [], meck:val({error, down})),
    Result = couch_prometheus:get_smoosh_stats(),
    ?assertEqual([], Result).
