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

-module(fabric_doc_purge).

-export([
    go/3
]).

-include_lib("mem3/include/mem3.hrl").

-record(acc, {
    worker_uuids,
    resps,
    uuid_counts,
    w
}).

go(_, [], _) ->
    {ok, []};
go(DbName, IdsRevs, Options) ->
    % Generate our purge requests of {UUID, DocId, Revs}. Return:
    %  * Reqs : [{UUID, DocId, Revs}]
    %  * UUIDs : [UUID] in the same order as Reqs
    %  * Responses : #{UUID => []} initial response accumulator
    %
    {UUIDs, Reqs, Responses} = create_requests_and_responses(IdsRevs),

    % Fire off rexi workers for each shard. Return:
    %  * Workers : [#shard{ref = Ref}]
    %  * WorkerUUIDs : #{Worker => [UUID]}
    %  * UUIDCounts : #{UUID => Counter}
    %
    {Workers, WorkerUUIDs, UUIDCounts} = maps:fold(
        fun(Shard, ShardReqs, {WorkersAcc, WorkersUUIDsAcc, CountsAcc}) ->
            #shard{name = ShardDbName, node = Node} = Shard,
            Args = [ShardDbName, ShardReqs, Options],
            Ref = rexi:cast(Node, {fabric_rpc, purge_docs, Args}),
            Worker = Shard#shard{ref = Ref},
            ShardUUIDs = [UUID || {UUID, _Id, _Revs} <- ShardReqs],
            Fun = fun(UUID, Acc) -> update_counter(UUID, Acc) end,
            CountsAcc1 = lists:foldl(Fun, CountsAcc, ShardUUIDs),
            WorkersUUIDAcc1 = WorkersUUIDsAcc#{Worker => ShardUUIDs},
            {[Worker | WorkersAcc], WorkersUUIDAcc1, CountsAcc1}
        end,
        {[], #{}, #{}},
        group_reqs_by_shard(DbName, Reqs)
    ),

    RexiMon = fabric_util:create_monitors(Workers),
    Timeout = fabric_util:request_timeout(),
    Acc0 = #acc{
        worker_uuids = WorkerUUIDs,
        resps = Responses,
        uuid_counts = UUIDCounts,
        w = fabric_util:w_from_opts(DbName, Options)
    },
    Callback = fun handle_message/3,
    Acc2 =
        try rexi_utils:recv(Workers, #shard.ref, Callback, Acc0, infinity, Timeout) of
            {ok, Acc1} -> Acc1;
            {timeout, Acc1} -> handle_timeout(Acc1);
            Else -> Else
        after
            rexi_monitor:stop(RexiMon)
        end,

    FinalResps = format_resps(UUIDs, Acc2),
    {resp_health(FinalResps), FinalResps}.

handle_message({rexi_DOWN, _, {_, Node}, _}, _Worker, Acc) ->
    #acc{
        worker_uuids = WorkerUUIDs,
        resps = Resps
    } = Acc,
    Pred = fun(#shard{node = N}, _) -> N == Node end,
    Failed = maps:filter(Pred, WorkerUUIDs),
    Rest = maps:without(maps:keys(Failed), WorkerUUIDs),
    NewResps = append_errors(internal_server_error, Failed, Resps),
    maybe_stop(Acc#acc{worker_uuids = Rest, resps = NewResps});
handle_message({rexi_EXIT, _}, Worker, Acc) ->
    #acc{
        worker_uuids = WorkerUUIDs,
        resps = Resps
    } = Acc,
    {FailedUUIDs, WorkerUUIDs1} = maps:take(Worker, WorkerUUIDs),
    NewResps = append_errors(internal_server_error, #{Worker => FailedUUIDs}, Resps),
    maybe_stop(Acc#acc{worker_uuids = WorkerUUIDs1, resps = NewResps});
handle_message({ok, Replies}, Worker, Acc) ->
    #acc{
        worker_uuids = WorkerUUIDs,
        resps = Resps
    } = Acc,
    {UUIDs, WorkerUUIDs1} = maps:take(Worker, WorkerUUIDs),
    NewResps = append_resps(UUIDs, Replies, Resps),
    maybe_stop(Acc#acc{worker_uuids = WorkerUUIDs1, resps = NewResps});
handle_message({bad_request, Msg}, _, _) ->
    throw({bad_request, Msg}).

handle_timeout(#acc{worker_uuids = DefunctWorkerUUIDs, resps = Resps} = Acc) ->
    DefunctWorkers = maps:keys(DefunctWorkerUUIDs),
    fabric_util:log_timeout(DefunctWorkers, "purge_docs"),
    NewResps = append_errors(timeout, DefunctWorkerUUIDs, Resps),
    Acc#acc{worker_uuids = #{}, resps = NewResps}.

create_requests_and_responses(IdsRevs) ->
    Fun = fun({Id, Revs}, {UUIDsAcc, RespAcc}) ->
        UUID = couch_uuids:v7_bin(),
        {{UUID, Id, lists:usort(Revs)}, {[UUID | UUIDsAcc], RespAcc#{UUID => []}}}
    end,
    {IdRevs1, {UUIDs, Resps}} = lists:mapfoldl(Fun, {[], #{}}, IdsRevs),
    {lists:reverse(UUIDs), IdRevs1, Resps}.

group_reqs_by_shard(DbName, Reqs) ->
    ReqFoldFun =
        fun({_UUID, Id, _Revs} = Req, #{} = Map0) ->
            AppendFun = fun(Shard, Map1) -> map_append(Shard, Req, Map1) end,
            lists:foldl(AppendFun, Map0, mem3:shards(DbName, Id))
        end,
    lists:foldl(ReqFoldFun, #{}, Reqs).

% Failed WorkerUUIDs = #{#shard{} => [UUIDs, ...]}
% Resps = #{UUID => [{ok, ...} | {error, ...}]
append_errors(Type, #{} = WorkerUUIDs, #{} = Resps) ->
    maps:fold(
        fun(_Worker, UUIDs, RespAcc) ->
            Errors = [{error, Type} || _UUID <- UUIDs],
            append_resps(UUIDs, Errors, RespAcc)
        end,
        Resps,
        WorkerUUIDs
    ).

append_resps([], [], #{} = Resps) ->
    Resps;
append_resps([UUID | RestUUIDs], [Reply | RestReplies], #{} = Resps) ->
    NewResps = map_append(UUID, Reply, Resps),
    append_resps(RestUUIDs, RestReplies, NewResps).

maybe_stop(#acc{worker_uuids = #{} = Map} = Acc) when map_size(Map) == 0 ->
    {stop, Acc};
maybe_stop(#acc{resps = #{} = Resps, uuid_counts = #{} = Counts, w = W} = Acc) ->
    try
        Fun =
            fun(UUID, UUIDResps) ->
                #{UUID := UUIDCount} = Counts,
                case has_quorum(UUIDResps, UUIDCount, W) of
                    true -> ok;
                    false -> throw(keep_going)
                end
            end,
        maps:foreach(Fun, Resps),
        {stop, Acc}
    catch
        throw:keep_going ->
            {ok, Acc}
    end.

format_resps(UUIDs, #acc{resps = Resps, w = W}) ->
    Fun = fun(_UUID, Replies) ->
        OkReplies = [Reply || {ok, Reply} <- Replies],
        case OkReplies of
            [] ->
                [Error | _] = lists:usort(Replies),
                Error;
            [_ | _] ->
                AllRevs = lists:usort(lists:flatten(OkReplies)),
                IsOk = length(OkReplies) >= W andalso length(lists:usort(OkReplies)) == 1,
                Health =
                    if
                        IsOk -> ok;
                        true -> accepted
                    end,
                {Health, AllRevs}
        end
    end,
    FinalReplies = maps:map(Fun, Resps),
    % Reorder results in the same order as the the initial IdRevs
    % this also implicitly asserts that the all UUIDs should have
    % a matching reply
    [map_get(UUID, FinalReplies) || UUID <- UUIDs];
format_resps(_UUIDs, Else) ->
    Else.

resp_health(Resps) ->
    Healths = lists:usort([H || {H, _} <- Resps]),
    HasError = lists:member(error, Healths),
    HasAccepted = lists:member(accepted, Healths),
    AllOk = Healths == [ok],
    if
        HasError -> error;
        HasAccepted -> accepted;
        AllOk -> ok;
        true -> error
    end.

has_quorum(Resps, Count, W) ->
    OkResps = [R || {ok, _} = R <- Resps],
    OkCounts = lists:foldl(fun(R, Acc) -> update_counter(R, Acc) end, #{}, OkResps),
    MaxOk = lists:max([0 | maps:values(OkCounts)]),
    if
        MaxOk >= W -> true;
        length(Resps) >= Count -> true;
        true -> false
    end.

map_append(Key, Val, #{} = Map) ->
    maps:update_with(Key, fun(V) -> [Val | V] end, [Val], Map).

update_counter(Key, #{} = Map) ->
    maps:update_with(Key, fun(V) -> V + 1 end, 1, Map).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

response_health_test() ->
    ?assertEqual(error, resp_health([])),
    ?assertEqual(error, resp_health([{potato, x}])),
    ?assertEqual(ok, resp_health([{ok, x}, {ok, y}])),
    ?assertEqual(accepted, resp_health([{accepted, x}])),
    ?assertEqual(accepted, resp_health([{ok, x}, {accepted, y}])),
    ?assertEqual(error, resp_health([{error, x}])),
    ?assertEqual(error, resp_health([{ok, x}, {error, y}])),
    ?assertEqual(error, resp_health([{error, x}, {accepted, y}, {ok, z}])).

has_quorum_test() ->
    ?assertEqual(true, has_quorum([], 0, 0)),
    ?assertEqual(true, has_quorum([], 1, 0)),
    ?assertEqual(true, has_quorum([], 0, 1)),
    ?assertEqual(false, has_quorum([], 1, 1)),
    ?assertEqual(true, has_quorum([{ok, x}], 1, 1)),
    ?assertEqual(true, has_quorum([{accepted, x}], 1, 1)),
    ?assertEqual(false, has_quorum([{accepted, x}], 2, 1)),
    ?assertEqual(false, has_quorum([{accepted, x}, {ok, y}], 3, 2)),
    ?assertEqual(true, has_quorum([{accepted, x}, {ok, y}], 2, 2)).

purge_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        with([
            ?TDEF(t_create_reqs),

            ?TDEF(t_w2_ok),
            ?TDEF(t_w3_ok),

            ?TDEF(t_w2_mixed_accepted),
            ?TDEF(t_w3_mixed_accepted),

            ?TDEF(t_w2_exit1_ok),
            ?TDEF(t_w2_exit2_accepted),
            ?TDEF(t_w2_exit3_error),

            ?TDEF(t_w4_accepted),

            ?TDEF(t_mixed_ok_accepted),
            ?TDEF(t_mixed_errors),
            ?TDEF(t_rexi_down_error),
            ?TDEF(t_timeout)
        ])
    }.

setup() ->
    test_util:start_couch().

teardown(Ctx) ->
    test_util:stop_couch(Ctx).

t_create_reqs(_) ->
    ?assertEqual({[], [], #{}}, create_requests_and_responses([])),
    IdRevs = [
        {<<"3">>, []},
        {<<"1">>, [<<"2-b">>, <<"1-a">>]},
        {<<"2">>, [<<"3-c">>, <<"1-d">>, <<"3-c">>]}
    ],
    Res = create_requests_and_responses(IdRevs),
    ?assertMatch({[<<_/binary>> | _], [{<<_/binary>>, _, _} | _], #{}}, Res),
    {UUIDs, IdRevs1, Resps} = Res,
    ?assertEqual(3, length(UUIDs)),
    ?assertEqual(3, length(IdRevs1)),
    ?assertEqual(3, map_size(Resps)),
    ?assertEqual(lists:sort(UUIDs), lists:sort(maps:keys(Resps))),
    {IdRevsUUIDs, DocIds, Revs} = lists:unzip3(IdRevs1),
    ?assertEqual(UUIDs, IdRevsUUIDs),
    ?assertEqual([<<"3">>, <<"1">>, <<"2">>], DocIds),
    ?assertEqual(
        [
            [],
            [<<"1-a">>, <<"2-b">>],
            [<<"1-d">>, <<"3-c">>]
        ],
        Revs
    ).

t_w2_ok(_) ->
    Acc0 = create_init_acc(2),
    Msg = {ok, [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}]},

    {ok, Acc1} = handle_message(Msg, worker(1, Acc0), Acc0),
    ?assertEqual(2, map_size(Acc1#acc.worker_uuids)),
    check_quorum(Acc1, false),

    {stop, Acc2} = handle_message(Msg, worker(2, Acc0), Acc1),
    ?assertEqual(1, map_size(Acc2#acc.worker_uuids)),
    check_quorum(Acc2, true),

    Expect = [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}],
    Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc2),
    ?assertEqual(Expect, Resps),
    ?assertEqual(ok, resp_health(Resps)).

t_w3_ok(_) ->
    Acc0 = create_init_acc(3),
    Msg = {ok, [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}]},

    {ok, Acc1} = handle_message(Msg, worker(1, Acc0), Acc0),
    check_quorum(Acc1, false),

    {ok, Acc2} = handle_message(Msg, worker(2, Acc0), Acc1),
    ?assertEqual(1, map_size(Acc2#acc.worker_uuids)),
    check_quorum(Acc2, false),

    {stop, Acc3} = handle_message(Msg, worker(3, Acc0), Acc2),
    ?assertEqual(0, map_size(Acc3#acc.worker_uuids)),
    check_quorum(Acc3, true),

    Expect = [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}],
    Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc3),
    ?assertEqual(Expect, Resps),
    ?assertEqual(ok, resp_health(Resps)).

t_w2_mixed_accepted(_) ->
    Acc0 = create_init_acc(2),
    Msg1 = {ok, [{ok, [{1, <<"foo1">>}]}, {ok, [{2, <<"bar1">>}]}]},
    Msg2 = {ok, [{ok, [{1, <<"foo2">>}]}, {ok, [{2, <<"bar2">>}]}]},

    {ok, Acc1} = handle_message(Msg1, worker(1, Acc0), Acc0),
    ?assertEqual(2, map_size(Acc1#acc.worker_uuids)),
    check_quorum(Acc1, false),

    {ok, Acc2} = handle_message(Msg2, worker(2, Acc0), Acc1),
    ?assertEqual(1, map_size(Acc2#acc.worker_uuids)),
    check_quorum(Acc2, false),

    {stop, Acc3} = handle_message(Msg1, worker(3, Acc0), Acc2),
    ?assertEqual(0, map_size(Acc3#acc.worker_uuids)),
    check_quorum(Acc3, true),

    Expect = [
        {accepted, [{1, <<"foo1">>}, {1, <<"foo2">>}]},
        {accepted, [{2, <<"bar1">>}, {2, <<"bar2">>}]}
    ],
    Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc2),
    ?assertEqual(Expect, Resps),
    ?assertEqual(accepted, resp_health(Resps)).

t_w3_mixed_accepted(_) ->
    Acc0 = create_init_acc(3),
    Msg1 = {ok, [{ok, [{1, <<"foo1">>}]}, {ok, [{2, <<"bar1">>}]}]},
    Msg2 = {ok, [{ok, [{1, <<"foo2">>}]}, {ok, [{2, <<"bar2">>}]}]},

    {ok, Acc1} = handle_message(Msg1, worker(1, Acc0), Acc0),
    ?assertEqual(2, map_size(Acc1#acc.worker_uuids)),
    check_quorum(Acc1, false),

    {ok, Acc2} = handle_message(Msg2, worker(2, Acc0), Acc1),
    ?assertEqual(1, map_size(Acc2#acc.worker_uuids)),
    check_quorum(Acc2, false),

    {stop, Acc3} = handle_message(Msg2, worker(3, Acc0), Acc2),
    ?assertEqual(0, map_size(Acc3#acc.worker_uuids)),
    check_quorum(Acc3, true),

    Expect = [
        {accepted, [{1, <<"foo1">>}, {1, <<"foo2">>}]},
        {accepted, [{2, <<"bar1">>}, {2, <<"bar2">>}]}
    ],
    Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc2),
    ?assertEqual(Expect, Resps),
    ?assertEqual(accepted, resp_health(Resps)).

t_w2_exit1_ok(_) ->
    Acc0 = create_init_acc(2),
    Msg = {ok, [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}]},
    ExitMsg = {rexi_EXIT, blargh},

    {ok, Acc1} = handle_message(Msg, worker(1, Acc0), Acc0),
    ?assertEqual(2, map_size(Acc1#acc.worker_uuids)),
    check_quorum(Acc1, false),

    {ok, Acc2} = handle_message(ExitMsg, worker(2, Acc0), Acc1),
    ?assertEqual(1, map_size(Acc2#acc.worker_uuids)),
    check_quorum(Acc2, false),

    {stop, Acc3} = handle_message(Msg, worker(3, Acc0), Acc2),
    ?assertEqual(0, map_size(Acc3#acc.worker_uuids)),
    check_quorum(Acc3, true),

    Expect = [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}],
    Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc3),
    ?assertEqual(Expect, Resps),
    ?assertEqual(ok, resp_health(Resps)).

t_w2_exit2_accepted(_) ->
    Acc0 = create_init_acc(2),
    Msg = {ok, [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}]},
    ExitMsg = {rexi_EXIT, blargh},

    {ok, Acc1} = handle_message(Msg, worker(1, Acc0), Acc0),
    ?assertEqual(2, map_size(Acc1#acc.worker_uuids)),
    check_quorum(Acc1, false),

    {ok, Acc2} = handle_message(ExitMsg, worker(2, Acc0), Acc1),
    ?assertEqual(1, map_size(Acc2#acc.worker_uuids)),
    check_quorum(Acc2, false),

    {stop, Acc3} = handle_message(ExitMsg, worker(3, Acc0), Acc2),
    ?assertEqual(0, map_size(Acc3#acc.worker_uuids)),
    check_quorum(Acc3, true),

    Expect = [{accepted, [{1, <<"foo">>}]}, {accepted, [{2, <<"bar">>}]}],
    Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc3),
    ?assertEqual(Expect, Resps),
    ?assertEqual(accepted, resp_health(Resps)).

t_w2_exit3_error(_) ->
    Acc0 = create_init_acc(2),
    ExitMsg = {rexi_EXIT, blargh},

    {ok, Acc1} = handle_message(ExitMsg, worker(1, Acc0), Acc0),
    ?assertEqual(2, map_size(Acc1#acc.worker_uuids)),
    check_quorum(Acc1, false),

    {ok, Acc2} = handle_message(ExitMsg, worker(2, Acc0), Acc1),
    ?assertEqual(1, map_size(Acc2#acc.worker_uuids)),
    check_quorum(Acc2, false),

    {stop, Acc3} = handle_message(ExitMsg, worker(3, Acc0), Acc2),
    ?assertEqual(0, map_size(Acc3#acc.worker_uuids)),
    check_quorum(Acc3, true),

    Expect = [
        {error, internal_server_error},
        {error, internal_server_error}
    ],
    Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc3),
    ?assertEqual(Expect, Resps),
    ?assertEqual(error, resp_health(Resps)).

t_w4_accepted(_) ->
    % Make sure we return when all workers have responded
    % rather than wait around for a timeout if a user asks
    % for a qourum with more than the available number of
    % shards.
    Acc0 = create_init_acc(4),
    Msg = {ok, [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}]},

    {ok, Acc1} = handle_message(Msg, worker(1, Acc0), Acc0),
    ?assertEqual(2, map_size(Acc1#acc.worker_uuids)),
    check_quorum(Acc1, false),

    {ok, Acc2} = handle_message(Msg, worker(2, Acc0), Acc1),
    ?assertEqual(1, map_size(Acc2#acc.worker_uuids)),
    check_quorum(Acc2, false),

    {stop, Acc3} = handle_message(Msg, worker(3, Acc0), Acc2),
    ?assertEqual(0, map_size(Acc3#acc.worker_uuids)),
    check_quorum(Acc3, true),

    Expect = [{accepted, [{1, <<"foo">>}]}, {accepted, [{2, <<"bar">>}]}],
    Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc3),
    ?assertEqual(Expect, Resps),
    ?assertEqual(accepted, resp_health(Resps)).

t_mixed_ok_accepted(_) ->
    WorkerUUIDs = #{
        #shard{node = a, range = [1, 2]} => [<<"uuid1">>],
        #shard{node = b, range = [1, 2]} => [<<"uuid1">>],
        #shard{node = c, range = [1, 2]} => [<<"uuid1">>],

        #shard{node = a, range = [3, 4]} => [<<"uuid2">>],
        #shard{node = b, range = [3, 4]} => [<<"uuid2">>],
        #shard{node = c, range = [3, 4]} => [<<"uuid2">>]
    },

    Acc0 = #acc{
        worker_uuids = WorkerUUIDs,
        resps = maps:from_list([{<<"uuid1">>, []}, {<<"uuid2">>, []}]),
        uuid_counts = maps:from_list([{<<"uuid1">>, 3}, {<<"uuid2">>, 3}]),
        w = 2
    },

    Msg1 = {ok, [{ok, [{1, <<"foo">>}]}]},
    Msg2 = {ok, [{ok, [{2, <<"bar">>}]}]},
    ExitMsg = {rexi_EXIT, blargh},

    {ok, Acc1} = handle_message(Msg1, worker(a, [1, 2], Acc0), Acc0),
    {ok, Acc2} = handle_message(Msg1, worker(b, [1, 2], Acc0), Acc1),
    {ok, Acc3} = handle_message(ExitMsg, worker(a, [3, 4], Acc0), Acc2),
    {ok, Acc4} = handle_message(ExitMsg, worker(b, [3, 4], Acc0), Acc3),
    {stop, Acc5} = handle_message(Msg2, worker(c, [3, 4], Acc0), Acc4),

    Expect = [{ok, [{1, <<"foo">>}]}, {accepted, [{2, <<"bar">>}]}],
    Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc5),
    ?assertEqual(Expect, Resps),
    ?assertEqual(accepted, resp_health(Resps)).

t_mixed_errors(_) ->
    WorkerUUIDs = #{
        #shard{node = a, range = [1, 2]} => [<<"uuid1">>],
        #shard{node = b, range = [1, 2]} => [<<"uuid1">>],
        #shard{node = c, range = [1, 2]} => [<<"uuid1">>],

        #shard{node = a, range = [3, 4]} => [<<"uuid2">>],
        #shard{node = b, range = [3, 4]} => [<<"uuid2">>],
        #shard{node = c, range = [3, 4]} => [<<"uuid2">>]
    },

    Acc0 = #acc{
        worker_uuids = WorkerUUIDs,
        resps = maps:from_list([{<<"uuid1">>, []}, {<<"uuid2">>, []}]),
        uuid_counts = maps:from_list([{<<"uuid1">>, 3}, {<<"uuid2">>, 3}]),
        w = 2
    },

    Msg = {ok, [{ok, [{1, <<"foo">>}]}]},
    ExitMsg = {rexi_EXIT, blargh},

    {ok, Acc1} = handle_message(Msg, worker(a, [1, 2], Acc0), Acc0),
    {ok, Acc2} = handle_message(Msg, worker(b, [1, 2], Acc0), Acc1),
    {ok, Acc3} = handle_message(ExitMsg, worker(a, [3, 4], Acc0), Acc2),
    {ok, Acc4} = handle_message(ExitMsg, worker(b, [3, 4], Acc0), Acc3),
    {stop, Acc5} = handle_message(ExitMsg, worker(c, [3, 4], Acc0), Acc4),

    Expect = [{ok, [{1, <<"foo">>}]}, {error, internal_server_error}],
    Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc5),
    ?assertEqual(Expect, Resps),
    ?assertEqual(error, resp_health(Resps)).

t_rexi_down_error(_) ->
    WorkerUUIDs = #{
        #shard{node = a, range = [1, 2]} => [<<"uuid1">>],
        #shard{node = b, range = [1, 2]} => [<<"uuid1">>],
        #shard{node = c, range = [1, 2]} => [<<"uuid1">>],

        #shard{node = a, range = [3, 4]} => [<<"uuid2">>],
        #shard{node = b, range = [3, 4]} => [<<"uuid2">>],
        #shard{node = c, range = [3, 4]} => [<<"uuid2">>]
    },

    Acc0 = #acc{
        worker_uuids = WorkerUUIDs,
        resps = maps:from_list([{<<"uuid1">>, []}, {<<"uuid2">>, []}]),
        uuid_counts = maps:from_list([{<<"uuid1">>, 3}, {<<"uuid2">>, 3}]),
        w = 2
    },

    Msg = {ok, [{ok, [{1, <<"foo">>}]}]},
    {ok, Acc1} = handle_message(Msg, worker(a, [1, 2], Acc0), Acc0),

    DownMsgB = {rexi_DOWN, nodedown, {nil, b}, nil},
    {ok, Acc2} = handle_message(DownMsgB, worker(b, [1, 2], Acc0), Acc1),

    DownMsgC = {rexi_DOWN, nodedown, {nil, c}, nil},
    {ok, Acc3} = handle_message(DownMsgC, worker(c, [3, 4], Acc0), Acc2),

    Expect = [
        {accepted, [{1, <<"foo">>}]},
        {error, internal_server_error}
    ],
    Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc3),
    ?assertEqual(Expect, Resps),
    ?assertEqual(error, resp_health(Resps)).

t_timeout(_) ->
    WorkerUUIDs = #{
        #shard{node = a, range = [1, 2]} => [<<"uuid1">>],
        #shard{node = b, range = [1, 2]} => [<<"uuid1">>],
        #shard{node = c, range = [1, 2]} => [<<"uuid1">>],

        #shard{node = a, range = [3, 4]} => [<<"uuid2">>],
        #shard{node = b, range = [3, 4]} => [<<"uuid2">>],
        #shard{node = c, range = [3, 4]} => [<<"uuid2">>]
    },

    Acc0 = #acc{
        worker_uuids = WorkerUUIDs,
        resps = maps:from_list([{<<"uuid1">>, []}, {<<"uuid2">>, []}]),
        uuid_counts = maps:from_list([{<<"uuid1">>, 3}, {<<"uuid2">>, 3}]),
        w = 2
    },

    Msg = {ok, [{ok, [{1, <<"foo">>}]}]},
    {ok, Acc1} = handle_message(Msg, worker(a, [1, 2], Acc0), Acc0),
    {ok, Acc2} = handle_message(Msg, worker(b, [1, 2], Acc0), Acc1),
    {ok, Acc3} = handle_message(Msg, worker(c, [1, 2], Acc0), Acc2),
    Acc4 = handle_timeout(Acc3),
    Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc4),
    ?assertEqual([{ok, [{1, <<"foo">>}]}, {error, timeout}], Resps).

create_init_acc(W) ->
    UUID1 = <<"uuid1">>,
    UUID2 = <<"uuid2">>,

    Nodes = [node1, node2, node3],
    Shards = mem3_util:create_partition_map(<<"foo">>, 3, 1, Nodes),

    % Create our worker_uuids. We're relying on the fact that
    % we're using a fake Q=1 db so we don't have to worry
    % about any hashing here.
    UUIDs = [UUID1, UUID2],
    Workers = [{S#shard{ref = make_ref()}, UUIDs} || S <- Shards],
    WorkerUUIDs = maps:from_list(Workers),

    #acc{
        worker_uuids = WorkerUUIDs,
        resps = #{UUID1 => [], UUID2 => []},
        uuid_counts = #{UUID1 => 3, UUID2 => 3},
        w = W
    }.

worker(Node, Range, #acc{worker_uuids = WorkerUUIDs}) ->
    Workers = maps:keys(WorkerUUIDs),
    Pred = fun(#shard{node = N, range = R}) ->
        Node =:= N andalso Range =:= R
    end,
    case lists:filter(Pred, Workers) of
        [W] -> W;
        _ -> error(not_found)
    end.

worker(N, #acc{worker_uuids = WorkerUUIDs}) ->
    Workers = maps:keys(WorkerUUIDs),
    lists:nth(N, lists:sort(Workers)).

check_quorum(Acc, Expect) ->
    maps:map(
        fun(_Shard, Resps) ->
            ?assertEqual(Expect, has_quorum(Resps, 3, Acc#acc.w))
        end,
        Acc#acc.resps
    ).

purge_end_to_end_test_() ->
    {
        setup,
        fun() ->
            Ctx = test_util:start_couch([fabric]),
            DbName = ?tempdb(),
            ok = fabric:create_db(DbName, [{q, 2}, {n, 1}]),
            {Ctx, DbName}
        end,
        fun({Ctx, DbName}) ->
            fabric:delete_db(DbName),
            test_util:stop_couch(Ctx),
            meck:unload()
        end,
        with([
            ?TDEF(t_purge),
            ?TDEF(t_purge_missing_doc_id),
            ?TDEF(t_purge_missing_rev)
        ])
    }.

t_purge({_Ctx, DbName}) ->
    Rev1 = update_doc(DbName, <<"1">>),
    Rev2 = update_doc(DbName, <<"2">>),
    Rev3 = update_doc(DbName, <<"3">>),
    Res = fabric:purge_docs(
        DbName,
        [
            {<<"3">>, [Rev3]},
            {<<"1">>, [Rev1]},
            {<<"2">>, [Rev2]}
        ],
        []
    ),
    ?assertMatch({ok, [_, _, _]}, Res),
    {ok, [Res3, Res1, Res2]} = Res,
    ?assertMatch({ok, [Rev1]}, Res1),
    ?assertMatch({ok, [Rev2]}, Res2),
    ?assertMatch({ok, [Rev3]}, Res3).

t_purge_missing_doc_id({_Ctx, DbName}) ->
    ?assertMatch({ok, []}, fabric:purge_docs(DbName, [], [])),
    Rev1 = update_doc(DbName, <<"1">>),
    Rev2 = update_doc(DbName, <<"2">>),
    Res = fabric:purge_docs(
        DbName,
        [
            {<<"3">>, [Rev1]},
            {<<"1">>, [Rev1]},
            {<<"2">>, [Rev2]}
        ],
        []
    ),
    ?assertMatch({ok, [_, _, _]}, Res),
    {ok, [Res3, Res1, Res2]} = Res,
    ?assertMatch({ok, [Rev1]}, Res1),
    ?assertMatch({ok, [Rev2]}, Res2),
    ?assertMatch({ok, []}, Res3).

t_purge_missing_rev({_Ctx, DbName}) ->
    Rev1 = update_doc(DbName, <<"1">>),
    Rev2 = update_doc(DbName, <<"2">>),
    update_doc(DbName, <<"3">>),
    Res = fabric:purge_docs(
        DbName,
        [
            {<<"1">>, [Rev2, Rev1]},
            {<<"2">>, [Rev1]},
            {<<"3">>, []}
        ],
        []
    ),
    ?assertMatch({ok, [_, _, _]}, Res),
    {ok, [Res1, Res2, Res3]} = Res,
    ?assertMatch({ok, [Rev1]}, Res1),
    ?assertMatch({ok, []}, Res2),
    ?assertMatch({ok, []}, Res3).

update_doc(DbName, Id) ->
    fabric_util:isolate(fun() ->
        Data = binary:encode_hex(crypto:strong_rand_bytes(10)),
        Doc = #doc{id = Id, body = {[{<<"foo">>, Data}]}},
        case fabric:update_doc(DbName, Doc, []) of
            {ok, Res} -> Res
        end
    end).

-endif.
