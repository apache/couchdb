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

-module(fabric_db_purged_infos).

-export([go/1]).

-include_lib("mem3/include/mem3.hrl").

-record(pacc, {
    counters,
    replies,
    ring_opts
}).

go(DbName) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(Shards, get_purged_infos, []),
    RexiMon = fabric_util:create_monitors(Shards),
    Fun = fun handle_message/3,
    Acc0 = #pacc{
        counters = fabric_dict:init(Workers, nil),
        replies = sets:new([{version, 2}]),
        ring_opts = [{any, Shards}]
    },
    try
        case fabric_util:recv(Workers, #shard.ref, Fun, Acc0) of
            {ok, Res} ->
                {ok, sets:to_list(Res)};
            {timeout, {WorkersDict, _}} ->
                DefunctWorkers = fabric_util:remove_done_workers(WorkersDict, nil),
                fabric_util:log_timeout(DefunctWorkers, "get_purged_infos"),
                {error, timeout};
            {error, Error} ->
                throw(Error)
        end
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _Shard, #pacc{} = Acc) ->
    #pacc{counters = Counters, ring_opts = RingOpts} = Acc,
    case fabric_util:remove_down_workers(Counters, NodeRef, RingOpts) of
        {ok, NewCounters} ->
            {ok, Acc#pacc{counters = NewCounters}};
        error ->
            {error, {nodedown, <<"progress not possible">>}}
    end;
handle_message({rexi_EXIT, Reason}, Shard, #pacc{} = Acc) ->
    #pacc{counters = Counters, ring_opts = RingOpts} = Acc,
    NewCounters = fabric_dict:erase(Shard, Counters),
    case fabric_ring:is_progress_possible(NewCounters, RingOpts) of
        true ->
            {ok, Acc#pacc{counters = NewCounters}};
        false ->
            {error, Reason}
    end;
handle_message({ok, Info}, #shard{} = Shard, #pacc{} = Acc) ->
    #pacc{counters = Counters, replies = Replies} = Acc,
    InfoSet = sets:from_list(Info, [{version, 2}]),
    Replies1 = sets:union(InfoSet, Replies),
    Counters1 = fabric_dict:erase(Shard, Counters),
    case fabric_dict:size(Counters1) =:= 0 of
        true ->
            {stop, Replies1};
        false ->
            {ok, Acc#pacc{counters = Counters1, replies = Replies1}}
    end;
handle_message(_, _, #pacc{} = Acc) ->
    {ok, Acc}.

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

make_shards() ->
    S1 = #shard{node = n1, range = [1, 2], ref = make_ref()},
    S2 = #shard{node = n2, range = [1, 2], ref = make_ref()},
    S3 = #shard{node = n3, range = [1, 2], ref = make_ref()},
    S4 = #shard{node = n1, range = [3, 4], ref = make_ref()},
    S5 = #shard{node = n2, range = [3, 4], ref = make_ref()},
    [S1, S2, S3, S4, S5].

init_acc(Shards) ->
    #pacc{
        counters = fabric_dict:init(Shards, nil),
        replies = sets:new([{version, 2}]),
        ring_opts = [{any, Shards}]
    }.

first_result_ok_test() ->
    Shards = [S1 | _] = make_shards(),
    Acc0 = init_acc(Shards),
    DocA = <<"a">>,
    RevsA = [<<"1">>, <<"2">>],

    {ok, Acc1} = handle_message({ok, [{DocA, RevsA}]}, S1, Acc0),
    ?assertEqual([{DocA, RevsA}], sets:to_list(Acc1#pacc.replies)).

result_duplicate_test() ->
    Shards = [S1, S2 | _] = make_shards(),
    Acc0 = init_acc(Shards),
    DocA = <<"a">>,
    RevsA1 = [<<"1">>, <<"2">>],

    {ok, Acc1} = handle_message({ok, [{DocA, RevsA1}]}, S1, Acc0),
    ?assertEqual([{DocA, RevsA1}], sets:to_list(Acc1#pacc.replies)),

    {ok, Acc2} = handle_message({ok, [{DocA, RevsA1}]}, S2, Acc1),
    ?assertEqual([{DocA, RevsA1}], sets:to_list(Acc2#pacc.replies)).

result_union_existing_test() ->
    Shards = [S1, S2 | _] = make_shards(),
    Acc0 = init_acc(Shards),
    DocA = <<"a">>,
    RevsA1 = [<<"1">>, <<"2">>],
    RevsA2 = [<<"3">>],

    {ok, Acc1} = handle_message({ok, [{DocA, RevsA1}]}, S1, Acc0),
    ?assertEqual([{DocA, RevsA1}], sets:to_list(Acc1#pacc.replies)),

    {ok, Acc2} = handle_message({ok, [{DocA, RevsA2}]}, S2, Acc1),
    Res2 = lists:sort(sets:to_list(Acc2#pacc.replies)),
    ?assertEqual([{DocA, RevsA1}, {DocA, RevsA2}], Res2).

res_finalize_test() ->
    Shards = [S1, S2, S3, S4, S5] = make_shards(),
    Acc0 = init_acc(Shards),
    DocA = <<"a">>,
    RevsA1 = [<<"1">>, <<"2">>],
    RevsA2 = [<<"3">>],
    DocB = <<"b">>,
    RevsB = [<<"5">>, <<"6">>],

    {ok, Acc1} = handle_message({ok, [{DocA, RevsA1}]}, S1, Acc0),
    ?assertEqual([{DocA, RevsA1}], sets:to_list(Acc1#pacc.replies)),

    {ok, Acc2} = handle_message({ok, [{DocA, RevsA1}]}, S2, Acc1),
    ?assertEqual([{DocA, RevsA1}], sets:to_list(Acc2#pacc.replies)),

    {ok, Acc3} = handle_message({ok, [{DocA, RevsA2}]}, S3, Acc2),
    Res3 = lists:sort(sets:to_list(Acc3#pacc.replies)),
    ?assertEqual([{DocA, RevsA1}, {DocA, RevsA2}], Res3),

    {ok, Acc4} = handle_message({ok, [{DocB, RevsB}]}, S4, Acc3),
    Res4 = lists:sort(sets:to_list(Acc4#pacc.replies)),
    ?assertEqual([{DocA, RevsA1}, {DocA, RevsA2}, {DocB, RevsB}], Res4),

    {stop, Acc5} = handle_message({ok, [{DocB, RevsB}]}, S5, Acc4),
    Res5 = lists:sort(sets:to_list(Acc5)),
    ?assertEqual([{DocA, RevsA1}, {DocA, RevsA2}, {DocB, RevsB}], Res5).

-endif.
