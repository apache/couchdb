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

-include_lib("fabric/include/fabric.hrl").
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
    % Generate our purge requests of {UUID, DocId, Revs}
    {UUIDs, Reqs} = create_reqs(IdsRevs, [], []),

    % Fire off rexi workers for each shard.
    {Workers, WorkerUUIDs} = dict:fold(
        fun(Shard, ShardReqs, {Ws, WUUIDs}) ->
            #shard{name = ShardDbName, node = Node} = Shard,
            Args = [ShardDbName, ShardReqs, Options],
            Ref = rexi:cast(Node, {fabric_rpc, purge_docs, Args}),
            Worker = Shard#shard{ref = Ref},
            ShardUUIDs = [UUID || {UUID, _Id, _Revs} <- ShardReqs],
            {[Worker | Ws], [{Worker, ShardUUIDs} | WUUIDs]}
        end,
        {[], []},
        group_reqs_by_shard(DbName, Reqs)
    ),

    UUIDCounts = lists:foldl(
        fun({_Worker, WUUIDs}, CountAcc) ->
            lists:foldl(
                fun(UUID, InnerCountAcc) ->
                    dict:update_counter(UUID, 1, InnerCountAcc)
                end,
                CountAcc,
                WUUIDs
            )
        end,
        dict:new(),
        WorkerUUIDs
    ),

    RexiMon = fabric_util:create_monitors(Workers),
    Timeout = fabric_util:request_timeout(),
    Acc0 = #acc{
        worker_uuids = WorkerUUIDs,
        resps = dict:from_list([{UUID, []} || UUID <- UUIDs]),
        uuid_counts = UUIDCounts,
        w = w(DbName, Options)
    },
    Acc2 =
        try
            rexi_utils:recv(
                Workers,
                #shard.ref,
                fun handle_message/3,
                Acc0,
                infinity,
                Timeout
            )
        of
            {ok, Acc1} ->
                Acc1;
            {timeout, Acc1} ->
                #acc{
                    worker_uuids = WorkerUUIDs,
                    resps = Resps
                } = Acc1,
                DefunctWorkers = [Worker || {Worker, _} <- WorkerUUIDs],
                fabric_util:log_timeout(DefunctWorkers, "purge_docs"),
                NewResps = append_errors(timeout, WorkerUUIDs, Resps),
                Acc1#acc{worker_uuids = [], resps = NewResps};
            Else ->
                Else
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
    Pred = fun({#shard{node = N}, _}) -> N == Node end,
    {Failed, Rest} = lists:partition(Pred, WorkerUUIDs),
    NewResps = append_errors(internal_server_error, Failed, Resps),
    maybe_stop(Acc#acc{worker_uuids = Rest, resps = NewResps});
handle_message({rexi_EXIT, _}, Worker, Acc) ->
    #acc{
        worker_uuids = WorkerUUIDs,
        resps = Resps
    } = Acc,
    {value, WorkerPair, Rest} = lists:keytake(Worker, 1, WorkerUUIDs),
    NewResps = append_errors(internal_server_error, [WorkerPair], Resps),
    maybe_stop(Acc#acc{worker_uuids = Rest, resps = NewResps});
handle_message({ok, Replies}, Worker, Acc) ->
    #acc{
        worker_uuids = WorkerUUIDs,
        resps = Resps
    } = Acc,
    {value, {_W, UUIDs}, Rest} = lists:keytake(Worker, 1, WorkerUUIDs),
    NewResps = append_resps(UUIDs, Replies, Resps),
    maybe_stop(Acc#acc{worker_uuids = Rest, resps = NewResps});
handle_message({bad_request, Msg}, _, _) ->
    throw({bad_request, Msg}).

create_reqs([], UUIDs, Reqs) ->
    {lists:reverse(UUIDs), lists:reverse(Reqs)};
create_reqs([{Id, Revs} | RestIdsRevs], UUIDs, Reqs) ->
    UUID = couch_uuids:new(),
    NewUUIDs = [UUID | UUIDs],
    NewReqs = [{UUID, Id, Revs} | Reqs],
    create_reqs(RestIdsRevs, NewUUIDs, NewReqs).

group_reqs_by_shard(DbName, Reqs) ->
    lists:foldl(
        fun({_UUID, Id, _Revs} = Req, D0) ->
            lists:foldl(
                fun(Shard, D1) ->
                    dict:append(Shard, Req, D1)
                end,
                D0,
                mem3:shards(DbName, Id)
            )
        end,
        dict:new(),
        Reqs
    ).

w(DbName, Options) ->
    try
        list_to_integer(couch_util:get_value(w, Options))
    catch
        _:_ ->
            mem3:quorum(DbName)
    end.

append_errors(Type, WorkerUUIDs, Resps) ->
    lists:foldl(
        fun({_Worker, UUIDs}, RespAcc) ->
            Errors = [{error, Type} || _UUID <- UUIDs],
            append_resps(UUIDs, Errors, RespAcc)
        end,
        Resps,
        WorkerUUIDs
    ).

append_resps([], [], Resps) ->
    Resps;
append_resps([UUID | RestUUIDs], [Reply | RestReplies], Resps) ->
    NewResps = dict:append(UUID, Reply, Resps),
    append_resps(RestUUIDs, RestReplies, NewResps).

maybe_stop(#acc{worker_uuids = []} = Acc) ->
    {stop, Acc};
maybe_stop(#acc{resps = Resps, uuid_counts = Counts, w = W} = Acc) ->
    try
        dict:fold(
            fun(UUID, UUIDResps, _) ->
                UUIDCount = dict:fetch(UUID, Counts),
                case has_quorum(UUIDResps, UUIDCount, W) of
                    true -> ok;
                    false -> throw(keep_going)
                end
            end,
            nil,
            Resps
        ),
        {stop, Acc}
    catch
        throw:keep_going ->
            {ok, Acc}
    end.

format_resps(UUIDs, #acc{} = Acc) ->
    #acc{
        resps = Resps,
        w = W
    } = Acc,
    FoldFun = fun(UUID, Replies, ReplyAcc) ->
        OkReplies = [Reply || {ok, Reply} <- Replies],
        case OkReplies of
            [] ->
                [Error | _] = lists:usort(Replies),
                [{UUID, Error} | ReplyAcc];
            _ ->
                AllRevs = lists:usort(lists:flatten(OkReplies)),
                IsOk =
                    length(OkReplies) >= W andalso
                        length(lists:usort(OkReplies)) == 1,
                Health =
                    if
                        IsOk -> ok;
                        true -> accepted
                    end,
                [{UUID, {Health, AllRevs}} | ReplyAcc]
        end
    end,
    FinalReplies = dict:fold(FoldFun, [], Resps),
    couch_util:reorder_results(UUIDs, FinalReplies);
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
    OkCounts = lists:foldl(
        fun(R, Acc) ->
            orddict:update_counter(R, 1, Acc)
        end,
        orddict:new(),
        OkResps
    ),
    MaxOk = lists:max([0 | element(2, lists:unzip(OkCounts))]),
    if
        MaxOk >= W -> true;
        length(Resps) >= Count -> true;
        true -> false
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

purge_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        [
            t_w2_ok(),
            t_w3_ok(),

            t_w2_mixed_accepted(),
            t_w3_mixed_accepted(),

            t_w2_exit1_ok(),
            t_w2_exit2_accepted(),
            t_w2_exit3_error(),

            t_w4_accepted(),

            t_mixed_ok_accepted(),
            t_mixed_errors()
        ]
    }.

setup() ->
    meck:new(couch_log),
    meck:expect(couch_log, warning, fun(_, _) -> ok end),
    meck:expect(couch_log, notice, fun(_, _) -> ok end).

teardown(_) ->
    meck:unload().

t_w2_ok() ->
    ?_test(begin
        Acc0 = create_init_acc(2),
        Msg = {ok, [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}]},

        {ok, Acc1} = handle_message(Msg, worker(1, Acc0), Acc0),
        ?assertEqual(2, length(Acc1#acc.worker_uuids)),
        check_quorum(Acc1, false),

        {stop, Acc2} = handle_message(Msg, worker(2, Acc0), Acc1),
        ?assertEqual(1, length(Acc2#acc.worker_uuids)),
        check_quorum(Acc2, true),

        Expect = [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}],
        Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc2),
        ?assertEqual(Expect, Resps),
        ?assertEqual(ok, resp_health(Resps))
    end).

t_w3_ok() ->
    ?_test(begin
        Acc0 = create_init_acc(3),
        Msg = {ok, [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}]},

        {ok, Acc1} = handle_message(Msg, worker(1, Acc0), Acc0),
        check_quorum(Acc1, false),

        {ok, Acc2} = handle_message(Msg, worker(2, Acc0), Acc1),
        ?assertEqual(1, length(Acc2#acc.worker_uuids)),
        check_quorum(Acc2, false),

        {stop, Acc3} = handle_message(Msg, worker(3, Acc0), Acc2),
        ?assertEqual(0, length(Acc3#acc.worker_uuids)),
        check_quorum(Acc3, true),

        Expect = [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}],
        Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc3),
        ?assertEqual(Expect, Resps),
        ?assertEqual(ok, resp_health(Resps))
    end).

t_w2_mixed_accepted() ->
    ?_test(begin
        Acc0 = create_init_acc(2),
        Msg1 = {ok, [{ok, [{1, <<"foo1">>}]}, {ok, [{2, <<"bar1">>}]}]},
        Msg2 = {ok, [{ok, [{1, <<"foo2">>}]}, {ok, [{2, <<"bar2">>}]}]},

        {ok, Acc1} = handle_message(Msg1, worker(1, Acc0), Acc0),
        ?assertEqual(2, length(Acc1#acc.worker_uuids)),
        check_quorum(Acc1, false),

        {ok, Acc2} = handle_message(Msg2, worker(2, Acc0), Acc1),
        ?assertEqual(1, length(Acc2#acc.worker_uuids)),
        check_quorum(Acc2, false),

        {stop, Acc3} = handle_message(Msg1, worker(3, Acc0), Acc2),
        ?assertEqual(0, length(Acc3#acc.worker_uuids)),
        check_quorum(Acc3, true),

        Expect = [
            {accepted, [{1, <<"foo1">>}, {1, <<"foo2">>}]},
            {accepted, [{2, <<"bar1">>}, {2, <<"bar2">>}]}
        ],
        Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc2),
        ?assertEqual(Expect, Resps),
        ?assertEqual(accepted, resp_health(Resps))
    end).

t_w3_mixed_accepted() ->
    ?_test(begin
        Acc0 = create_init_acc(3),
        Msg1 = {ok, [{ok, [{1, <<"foo1">>}]}, {ok, [{2, <<"bar1">>}]}]},
        Msg2 = {ok, [{ok, [{1, <<"foo2">>}]}, {ok, [{2, <<"bar2">>}]}]},

        {ok, Acc1} = handle_message(Msg1, worker(1, Acc0), Acc0),
        ?assertEqual(2, length(Acc1#acc.worker_uuids)),
        check_quorum(Acc1, false),

        {ok, Acc2} = handle_message(Msg2, worker(2, Acc0), Acc1),
        ?assertEqual(1, length(Acc2#acc.worker_uuids)),
        check_quorum(Acc2, false),

        {stop, Acc3} = handle_message(Msg2, worker(3, Acc0), Acc2),
        ?assertEqual(0, length(Acc3#acc.worker_uuids)),
        check_quorum(Acc3, true),

        Expect = [
            {accepted, [{1, <<"foo1">>}, {1, <<"foo2">>}]},
            {accepted, [{2, <<"bar1">>}, {2, <<"bar2">>}]}
        ],
        Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc2),
        ?assertEqual(Expect, Resps),
        ?assertEqual(accepted, resp_health(Resps))
    end).

t_w2_exit1_ok() ->
    ?_test(begin
        Acc0 = create_init_acc(2),
        Msg = {ok, [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}]},
        ExitMsg = {rexi_EXIT, blargh},

        {ok, Acc1} = handle_message(Msg, worker(1, Acc0), Acc0),
        ?assertEqual(2, length(Acc1#acc.worker_uuids)),
        check_quorum(Acc1, false),

        {ok, Acc2} = handle_message(ExitMsg, worker(2, Acc0), Acc1),
        ?assertEqual(1, length(Acc2#acc.worker_uuids)),
        check_quorum(Acc2, false),

        {stop, Acc3} = handle_message(Msg, worker(3, Acc0), Acc2),
        ?assertEqual(0, length(Acc3#acc.worker_uuids)),
        check_quorum(Acc3, true),

        Expect = [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}],
        Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc3),
        ?assertEqual(Expect, Resps),
        ?assertEqual(ok, resp_health(Resps))
    end).

t_w2_exit2_accepted() ->
    ?_test(begin
        Acc0 = create_init_acc(2),
        Msg = {ok, [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}]},
        ExitMsg = {rexi_EXIT, blargh},

        {ok, Acc1} = handle_message(Msg, worker(1, Acc0), Acc0),
        ?assertEqual(2, length(Acc1#acc.worker_uuids)),
        check_quorum(Acc1, false),

        {ok, Acc2} = handle_message(ExitMsg, worker(2, Acc0), Acc1),
        ?assertEqual(1, length(Acc2#acc.worker_uuids)),
        check_quorum(Acc2, false),

        {stop, Acc3} = handle_message(ExitMsg, worker(3, Acc0), Acc2),
        ?assertEqual(0, length(Acc3#acc.worker_uuids)),
        check_quorum(Acc3, true),

        Expect = [{accepted, [{1, <<"foo">>}]}, {accepted, [{2, <<"bar">>}]}],
        Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc3),
        ?assertEqual(Expect, Resps),
        ?assertEqual(accepted, resp_health(Resps))
    end).

t_w2_exit3_error() ->
    ?_test(begin
        Acc0 = create_init_acc(2),
        ExitMsg = {rexi_EXIT, blargh},

        {ok, Acc1} = handle_message(ExitMsg, worker(1, Acc0), Acc0),
        ?assertEqual(2, length(Acc1#acc.worker_uuids)),
        check_quorum(Acc1, false),

        {ok, Acc2} = handle_message(ExitMsg, worker(2, Acc0), Acc1),
        ?assertEqual(1, length(Acc2#acc.worker_uuids)),
        check_quorum(Acc2, false),

        {stop, Acc3} = handle_message(ExitMsg, worker(3, Acc0), Acc2),
        ?assertEqual(0, length(Acc3#acc.worker_uuids)),
        check_quorum(Acc3, true),

        Expect = [
            {error, internal_server_error},
            {error, internal_server_error}
        ],
        Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc3),
        ?assertEqual(Expect, Resps),
        ?assertEqual(error, resp_health(Resps))
    end).

t_w4_accepted() ->
    % Make sure we return when all workers have responded
    % rather than wait around for a timeout if a user asks
    % for a qourum with more than the available number of
    % shards.
    ?_test(begin
        Acc0 = create_init_acc(4),
        Msg = {ok, [{ok, [{1, <<"foo">>}]}, {ok, [{2, <<"bar">>}]}]},

        {ok, Acc1} = handle_message(Msg, worker(1, Acc0), Acc0),
        ?assertEqual(2, length(Acc1#acc.worker_uuids)),
        check_quorum(Acc1, false),

        {ok, Acc2} = handle_message(Msg, worker(2, Acc0), Acc1),
        ?assertEqual(1, length(Acc2#acc.worker_uuids)),
        check_quorum(Acc2, false),

        {stop, Acc3} = handle_message(Msg, worker(3, Acc0), Acc2),
        ?assertEqual(0, length(Acc3#acc.worker_uuids)),
        check_quorum(Acc3, true),

        Expect = [{accepted, [{1, <<"foo">>}]}, {accepted, [{2, <<"bar">>}]}],
        Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc3),
        ?assertEqual(Expect, Resps),
        ?assertEqual(accepted, resp_health(Resps))
    end).

t_mixed_ok_accepted() ->
    ?_test(begin
        WorkerUUIDs = [
            {#shard{node = a, range = [1, 2]}, [<<"uuid1">>]},
            {#shard{node = b, range = [1, 2]}, [<<"uuid1">>]},
            {#shard{node = c, range = [1, 2]}, [<<"uuid1">>]},

            {#shard{node = a, range = [3, 4]}, [<<"uuid2">>]},
            {#shard{node = b, range = [3, 4]}, [<<"uuid2">>]},
            {#shard{node = c, range = [3, 4]}, [<<"uuid2">>]}
        ],

        Acc0 = #acc{
            worker_uuids = WorkerUUIDs,
            resps = dict:from_list([{<<"uuid1">>, []}, {<<"uuid2">>, []}]),
            uuid_counts = dict:from_list([{<<"uuid1">>, 3}, {<<"uuid2">>, 3}]),
            w = 2
        },

        Msg1 = {ok, [{ok, [{1, <<"foo">>}]}]},
        Msg2 = {ok, [{ok, [{2, <<"bar">>}]}]},
        ExitMsg = {rexi_EXIT, blargh},

        {ok, Acc1} = handle_message(Msg1, worker(1, Acc0), Acc0),
        {ok, Acc2} = handle_message(Msg1, worker(2, Acc0), Acc1),
        {ok, Acc3} = handle_message(ExitMsg, worker(4, Acc0), Acc2),
        {ok, Acc4} = handle_message(ExitMsg, worker(5, Acc0), Acc3),
        {stop, Acc5} = handle_message(Msg2, worker(6, Acc0), Acc4),

        Expect = [{ok, [{1, <<"foo">>}]}, {accepted, [{2, <<"bar">>}]}],
        Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc5),
        ?assertEqual(Expect, Resps),
        ?assertEqual(accepted, resp_health(Resps))
    end).

t_mixed_errors() ->
    ?_test(begin
        WorkerUUIDs = [
            {#shard{node = a, range = [1, 2]}, [<<"uuid1">>]},
            {#shard{node = b, range = [1, 2]}, [<<"uuid1">>]},
            {#shard{node = c, range = [1, 2]}, [<<"uuid1">>]},

            {#shard{node = a, range = [3, 4]}, [<<"uuid2">>]},
            {#shard{node = b, range = [3, 4]}, [<<"uuid2">>]},
            {#shard{node = c, range = [3, 4]}, [<<"uuid2">>]}
        ],

        Acc0 = #acc{
            worker_uuids = WorkerUUIDs,
            resps = dict:from_list([{<<"uuid1">>, []}, {<<"uuid2">>, []}]),
            uuid_counts = dict:from_list([{<<"uuid1">>, 3}, {<<"uuid2">>, 3}]),
            w = 2
        },

        Msg = {ok, [{ok, [{1, <<"foo">>}]}]},
        ExitMsg = {rexi_EXIT, blargh},

        {ok, Acc1} = handle_message(Msg, worker(1, Acc0), Acc0),
        {ok, Acc2} = handle_message(Msg, worker(2, Acc0), Acc1),
        {ok, Acc3} = handle_message(ExitMsg, worker(4, Acc0), Acc2),
        {ok, Acc4} = handle_message(ExitMsg, worker(5, Acc0), Acc3),
        {stop, Acc5} = handle_message(ExitMsg, worker(6, Acc0), Acc4),

        Expect = [{ok, [{1, <<"foo">>}]}, {error, internal_server_error}],
        Resps = format_resps([<<"uuid1">>, <<"uuid2">>], Acc5),
        ?assertEqual(Expect, Resps),
        ?assertEqual(error, resp_health(Resps))
    end).

create_init_acc(W) ->
    UUID1 = <<"uuid1">>,
    UUID2 = <<"uuid2">>,

    Nodes = [node1, node2, node3],
    Shards = mem3_util:create_partition_map(<<"foo">>, 3, 1, Nodes),

    % Create our worker_uuids. We're relying on the fact that
    % we're using a fake Q=1 db so we don't have to worry
    % about any hashing here.
    WorkerUUIDs = lists:map(
        fun(Shard) ->
            {Shard#shard{ref = erlang:make_ref()}, [UUID1, UUID2]}
        end,
        Shards
    ),

    #acc{
        worker_uuids = WorkerUUIDs,
        resps = dict:from_list([{UUID1, []}, {UUID2, []}]),
        uuid_counts = dict:from_list([{UUID1, 3}, {UUID2, 3}]),
        w = W
    }.

worker(N, #acc{worker_uuids = WorkerUUIDs}) ->
    {Worker, _} = lists:nth(N, WorkerUUIDs),
    Worker.

check_quorum(Acc, Expect) ->
    dict:fold(
        fun(_Shard, Resps, _) ->
            ?assertEqual(Expect, has_quorum(Resps, 3, Acc#acc.w))
        end,
        nil,
        Acc#acc.resps
    ).

-endif.
