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

-module(fabric_view_map).

-export([go/8]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

go(DbName, Options, GroupId, View, Args, Callback, Acc, VInfo) when
    is_binary(GroupId)
->
    {ok, DDoc} = fabric:open_doc(DbName, <<"_design/", GroupId/binary>>, []),
    go(DbName, Options, DDoc, View, Args, Callback, Acc, VInfo);
go(Db, Options, DDoc, View, Args0, Callback, Acc, VInfo) ->
    DbName = fabric:dbname(Db),
    Args =
        case Args0 of
            #mrargs{keys = Keys, direction = rev} when is_list(Keys) ->
                Args0#mrargs{keys = lists:reverse(Keys)};
            #mrargs{} ->
                Args0
        end,
    {Shards, RingOpts} = fabric_view:get_shards(Db, Args),
    {CoordArgs, WorkerArgs} = fabric_view:fix_skip_and_limit(Args),
    DocIdAndRev = fabric_util:doc_id_and_rev(DDoc),
    fabric_view:maybe_update_others(DbName, DocIdAndRev, Shards, View, Args),
    Repls = fabric_ring:get_shard_replacements(DbName, Shards),
    RPCArgs = [DocIdAndRev, View, WorkerArgs, Options],
    StartFun = fun(Shard) ->
        hd(fabric_streams:submit_jobs([Shard], fabric_rpc, map_view, RPCArgs))
    end,
    Workers0 = fabric_streams:submit_jobs(Shards, fabric_rpc, map_view, RPCArgs),
    RexiMon = fabric_util:create_monitors(Workers0),
    try
        case
            fabric_streams:start(
                Workers0,
                #shard.ref,
                StartFun,
                Repls,
                RingOpts
            )
        of
            {ok, ddoc_updated} ->
                Callback({error, ddoc_updated}, Acc);
            {ok, insufficient_storage} ->
                Callback(
                    {error, {insufficient_storage, <<"not enough room to update index">>}}, Acc
                );
            {ok, Workers} ->
                try
                    go(DbName, Workers, VInfo, CoordArgs, Callback, Acc)
                after
                    fabric_streams:cleanup(Workers)
                end;
            {timeout, DefunctWorkers} ->
                fabric_util:log_timeout(DefunctWorkers, "map_view"),
                Callback({error, timeout}, Acc);
            {error, Error} ->
                Callback({error, Error}, Acc)
        end
    after
        rexi_monitor:stop(RexiMon)
    end.

go(DbName, Workers, {map, View, _}, Args, Callback, Acc0) ->
    #mrargs{limit = Limit, skip = Skip, keys = Keys, update_seq = UpdateSeq} = Args,
    Collation = couch_util:get_value(<<"collation">>, View#mrview.options),
    State = #collector{
        db_name = DbName,
        query_args = Args,
        callback = Callback,
        counters = fabric_dict:init(Workers, 0),
        skip = Skip,
        limit = Limit,
        keys = fabric_view:keydict(Keys),
        sorted = Args#mrargs.sorted,
        collation = Collation,
        user_acc = Acc0,
        update_seq =
            case UpdateSeq of
                true -> [];
                false -> nil
            end
    },
    case
        rexi_utils:recv(
            Workers,
            #shard.ref,
            fun handle_message/3,
            State,
            fabric_util:view_timeout(Args),
            fabric_util:timeout("view_permsg", "3600000")
        )
    of
        {ok, NewState} ->
            {ok, NewState#collector.user_acc};
        {timeout, NewState} ->
            Callback({error, timeout}, NewState#collector.user_acc);
        {error, Resp} ->
            {ok, Resp}
    end.

handle_stop(State) ->
    #collector{callback = Callback} = State,
    {_, Acc} = Callback(complete, State#collector.user_acc),
    {stop, State#collector{user_acc = Acc}}.

handle_non_sorted(Row, {_, From}, State) ->
    #collector{callback = Callback, user_acc = AccIn, limit = Limit} = State,
    {Go, Acc} = Callback(fabric_view_row:transform(Row), AccIn),
    rexi:stream_ack(From),
    {Go, State#collector{user_acc = Acc, limit = Limit - 1}}.

handle_sorted(Row0, {Worker, _} = Source, State) ->
    #collector{
        query_args = #mrargs{direction = Dir},
        counters = Counters0,
        rows = Rows0,
        keys = KeyDict0,
        collation = Collation
    } = State,
    Row = fabric_view_row:set_worker(Row0, Source),
    {Rows, KeyDict} = merge_row(
        Dir,
        Collation,
        KeyDict0,
        Row,
        Rows0
    ),
    Counters1 = fabric_dict:update_counter(Worker, 1, Counters0),
    State1 = State#collector{rows = Rows, counters = Counters1, keys = KeyDict},
    fabric_view:maybe_send_row(State1).

handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _, State) ->
    fabric_view:check_down_shards(State, NodeRef);
handle_message({rexi_EXIT, Reason}, Worker, State) ->
    fabric_view:handle_worker_exit(State, Worker, Reason);
handle_message({meta, Meta0}, {Worker, From}, State) ->
    Tot = couch_util:get_value(total, Meta0, 0),
    Off = couch_util:get_value(offset, Meta0, 0),
    Seq = couch_util:get_value(update_seq, Meta0, 0),
    #collector{
        callback = Callback,
        counters = Counters0,
        total_rows = Total0,
        offset = Offset0,
        user_acc = AccIn,
        update_seq = UpdateSeq0
    } = State,
    % Assert that we don't have other messages from this
    % worker when the total_and_offset message arrives.
    0 = fabric_dict:lookup_element(Worker, Counters0),
    rexi:stream_ack(From),
    Counters1 = fabric_dict:update_counter(Worker, 1, Counters0),
    Total = Total0 + Tot,
    Offset = Offset0 + Off,
    UpdateSeq =
        case UpdateSeq0 of
            nil -> nil;
            _ -> [{Worker, Seq} | UpdateSeq0]
        end,
    case fabric_dict:any(0, Counters1) of
        true ->
            {ok, State#collector{
                counters = Counters1,
                total_rows = Total,
                update_seq = UpdateSeq,
                offset = Offset
            }};
        false ->
            FinalOffset = min(Total, Offset + State#collector.skip),
            Meta =
                [{total, Total}, {offset, FinalOffset}] ++
                    case UpdateSeq of
                        nil ->
                            [];
                        _ ->
                            [{update_seq, fabric_view_changes:pack_seqs(UpdateSeq)}]
                    end,
            {Go, Acc} = Callback({meta, Meta}, AccIn),
            {Go, State#collector{
                counters = fabric_dict:decrement_all(Counters1),
                total_rows = Total,
                offset = FinalOffset,
                user_acc = Acc
            }}
    end;
handle_message(#view_row{}, {_, _}, #collector{sorted = false, limit = 0} = State) ->
    handle_stop(State);
handle_message(#view_row{} = Row, {_, _} = Source, #collector{sorted = false} = State) ->
    handle_non_sorted(Row, Source, State);
handle_message(#view_row{} = Row, {_, _} = Source, State) ->
    handle_sorted(Row, Source, State);
handle_message({view_row, #{}}, {_, _}, #collector{sorted = false, limit = 0} = State) ->
    handle_stop(State);
handle_message({view_row, #{}} = Row, {_, _} = Source, #collector{sorted = false} = State) ->
    handle_non_sorted(Row, Source, State);
handle_message({view_row, #{}} = Row, {_, _} = Source, State) ->
    handle_sorted(Row, Source, State);
handle_message(complete, Worker, State) ->
    Counters = fabric_dict:update_counter(Worker, 1, State#collector.counters),
    fabric_view:maybe_send_row(State#collector{counters = Counters});
handle_message({execution_stats, _} = Msg, {_, From}, State) ->
    #collector{callback = Callback, user_acc = AccIn} = State,
    {Go, Acc} = Callback(Msg, AccIn),
    rexi:stream_ack(From),
    {Go, State#collector{user_acc = Acc}};
handle_message(ddoc_updated, _Worker, State) ->
    {stop, State};
handle_message(insufficient_storage, _Worker, State) ->
    {stop, State}.

merge_row(Dir, Collation, undefined, Row, Rows0) ->
    Rows1 = lists:merge(
        fun(RowA, RowB) ->
            KeyA = fabric_view_row:get_key(RowA),
            KeyB = fabric_view_row:get_key(RowB),
            IdA = fabric_view_row:get_id(RowA),
            IdB = fabric_view_row:get_id(RowB),
            compare(Dir, Collation, {KeyA, IdA}, {KeyB, IdB})
        end,
        [Row],
        Rows0
    ),
    {Rows1, undefined};
merge_row(Dir, Collation, KeyDict0, Row, Rows0) ->
    CmpFun =
        case Collation of
            <<"raw">> ->
                fun
                    (A, A) ->
                        0;
                    (A, B) ->
                        case A < B of
                            true -> -1;
                            false -> 1
                        end
                end;
            _ ->
                fun couch_ejson_compare:less/2
        end,
    Key = fabric_view_row:get_key(Row),
    case maybe_update_keydict(Key, KeyDict0, CmpFun) of
        undefined ->
            {Rows0, KeyDict0};
        KeyDict1 ->
            Rows1 = lists:merge(
                fun(RowA, RowB) ->
                    A = fabric_view_row:get_key(RowA),
                    B = fabric_view_row:get_key(RowB),
                    IdA = fabric_view_row:get_id(RowA),
                    IdB = fabric_view_row:get_id(RowB),
                    case {Dir, CmpFun(A, B)} of
                        {fwd, 0} ->
                            IdA < IdB;
                        {rev, 0} ->
                            IdB < IdA;
                        {_, _} ->
                            % We already have a reversed key dict, and sent the
                            % workers the same reversed keys list. So here we
                            % just enforce sorting according to the order in
                            % the key dict
                            dict:fetch(A, KeyDict1) < dict:fetch(B, KeyDict1)
                    end
                end,
                [Row],
                Rows0
            ),
            {Rows1, KeyDict1}
    end.

compare(fwd, <<"raw">>, A, B) -> A < B;
compare(rev, <<"raw">>, A, B) -> B < A;
compare(fwd, _, A, B) -> couch_ejson_compare:less_json_ids(A, B);
compare(rev, _, A, B) -> couch_ejson_compare:less_json_ids(B, A).

% KeyDict captures the user-supplied ordering of keys POSTed by the user by
% mapping to integers (see fabric_view:keydict/1). It's possible that these keys
% do not compare equal (i.e., =:=, used by dict) to those returned by the view
% but are in fact equal under ICU. In this case (assuming the view uses ICU
% collation) we must update KeyDict with a mapping from the ICU-equal key to its
% appropriate value.
maybe_update_keydict(Key, KeyDict, CmpFun) ->
    case dict:find(Key, KeyDict) of
        {ok, _} ->
            KeyDict;
        error ->
            case key_index(Key, dict:to_list(KeyDict), CmpFun) of
                undefined ->
                    undefined;
                Value ->
                    dict:store(Key, Value, KeyDict)
            end
    end.

key_index(_, [], _) ->
    undefined;
key_index(KeyA, [{KeyB, Value} | KVs], CmpFun) ->
    case CmpFun(KeyA, KeyB) of
        0 -> Value;
        _ -> key_index(KeyA, KVs, CmpFun)
    end.

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

handle_message_test_() ->
    {
        foreach,
        fun() ->
            meck:new(foo, [non_strict]),
            meck:new(fabric_view)
        end,
        fun(_) -> meck:unload() end,
        [
            ?TDEF_FE(t_handle_message_rexi_down),
            ?TDEF_FE(t_handle_message_rexi_exit),
            ?TDEF_FE(t_handle_message_meta_zero),
            ?TDEF_FE(t_handle_message_meta),
            ?TDEF_FE(t_handle_message_limit),
            ?TDEF_FE(t_handle_message_non_sorted),
            ?TDEF_FE(t_handle_message_sorted),
            ?TDEF_FE(t_handle_message_complete),
            ?TDEF_FE(t_handle_message_execution_stats),
            ?TDEF_FE(t_handle_message_ddoc_updated),
            ?TDEF_FE(t_handle_message_insufficient_storage)
        ]
    }.

t_handle_message_rexi_down(_) ->
    Message = {rexi_DOWN, undefined, {undefined, node}, undefined},
    meck:expect(fabric_view, check_down_shards, [state, node], meck:val(fabric_view_result)),
    ?assertEqual(fabric_view_result, handle_message(Message, source, state)).

t_handle_message_rexi_exit(_) ->
    Message = {rexi_EXIT, reason},
    meck:expect(
        fabric_view, handle_worker_exit, [state, source, reason], meck:val(fabric_view_result)
    ),
    ?assertEqual(fabric_view_result, handle_message(Message, source, state)).

t_handle_message_meta_zero(_) ->
    Meta = [{total, 3}, {offset, 2}, {update_seq, 1}],
    Worker = {worker1, from},
    Counters1 = [{worker1, 0}, {worker2, 0}],
    Counters2 = [{worker1, 1}, {worker2, 0}],
    State1 = #collector{counters = Counters1, total_rows = 0, update_seq = nil, offset = 0},
    State2 = #collector{counters = Counters2, total_rows = 3, update_seq = nil, offset = 2},
    meck:expect(rexi, stream_ack, [from], meck:val(ok)),
    ?assertEqual({ok, State2}, handle_message({meta, Meta}, Worker, State1)).

t_handle_message_meta(_) ->
    Meta1 = [{total, 10}, {offset, 2}, {update_seq, seq}],
    Meta2 = [{total, 10}, {offset, 5}, {update_seq, packed_seq}],
    Meta3 = [{total, 10}, {offset, 5}],
    Worker = {worker1, from},
    Counters1 = [{worker1, 0}, {worker2, 3}, {worker3, 5}],
    Counters2 = [{worker1, 0}, {worker2, 2}, {worker3, 4}],
    State1 = #collector{
        counters = Counters1,
        total_rows = 0,
        update_seq = [],
        offset = 0,
        skip = 3,
        callback = fun foo:bar/2,
        user_acc = accumulator1
    },
    State2 = #collector{
        counters = Counters1,
        total_rows = 0,
        update_seq = nil,
        offset = 0,
        skip = 3,
        callback = fun foo:bar/2,
        user_acc = accumulator2
    },
    State3 = #collector{
        counters = Counters2,
        total_rows = 10,
        update_seq = [],
        offset = 5,
        skip = 3,
        callback = fun foo:bar/2,
        user_acc = updated_accumulator1
    },
    State4 = #collector{
        counters = Counters2,
        total_rows = 10,
        update_seq = nil,
        offset = 5,
        skip = 3,
        callback = fun foo:bar/2,
        user_acc = updated_accumulator2
    },
    meck:expect(
        foo,
        bar,
        [
            {[{meta, Meta2}, accumulator1], meck:val({go1, updated_accumulator1})},
            {[{meta, Meta3}, accumulator2], meck:val({go2, updated_accumulator2})}
        ]
    ),
    meck:expect(fabric_view_changes, pack_seqs, [[{worker1, seq}]], meck:val(packed_seq)),
    meck:expect(rexi, stream_ack, [from], meck:val(ok)),
    ?assertEqual({go1, State3}, handle_message({meta, Meta1}, Worker, State1)),
    ?assertEqual({go2, State4}, handle_message({meta, Meta1}, Worker, State2)).

t_handle_message_limit(_) ->
    State1 = #collector{
        sorted = false, limit = 0, callback = fun foo:bar/2, user_acc = accumulator
    },
    State2 = #collector{
        sorted = false, limit = 0, callback = fun foo:bar/2, user_acc = updated_accumulator
    },
    Worker = {worker, from},
    Row1 = #view_row{},
    Row2 = {view_row, #{}},
    meck:expect(foo, bar, [complete, accumulator], meck:val({go, updated_accumulator})),
    meck:expect(rexi, stream_ack, [from], undefined),
    ?assertEqual({stop, State2}, handle_message(Row1, Worker, State1)),
    ?assertEqual({stop, State2}, handle_message(Row2, Worker, State1)),
    ?assertNot(meck:called(rexi, stream_ack, '_')).

t_handle_message_non_sorted(_) ->
    State1 = #collector{
        sorted = false, limit = 10, callback = fun foo:bar/2, user_acc = accumulator
    },
    State2 = #collector{
        sorted = false, limit = 9, callback = fun foo:bar/2, user_acc = updated_accumulator
    },
    Worker = {worker, from},
    Row1 = #view_row{id = id, key = key, doc = doc},
    Row2 = {view_row, #{id => id, key => key, doc => doc}},
    Props = {row, [{id, id}, {key, key}, {value, undefined}, {doc, doc}]},
    meck:expect(foo, bar, [Props, accumulator], meck:val({go, updated_accumulator})),
    meck:expect(rexi, stream_ack, [from], meck:val(ok)),
    ?assertEqual({go, State2}, handle_message(Row1, Worker, State1)),
    ?assertEqual({go, State2}, handle_message(Row2, Worker, State1)).

t_handle_message_sorted(_) ->
    QueryArgs = #mrargs{direction = fwd},
    Counters1 = [{worker, 1}],
    Counters2 = [{worker, 2}],
    Worker = {worker, from},
    Row1 = #view_row{id = id2, key = key2, doc = doc2},
    Row2 = {view_row, #{id => id2, key => key2, doc => doc2}},
    Props = {row, [{id, id2}, {key, key2}, {value, undefined}, {doc, doc2}]},
    Rows11 = #view_row{id = id1, key = key1, doc = doc1},
    Rows12 = #view_row{id = id2, key = key2, doc = doc2, worker = Worker},
    Rows13 = #view_row{id = id3, key = key3, doc = doc3},
    Rows21 = {view_row, #{id => id1, key => key1}},
    Rows22 = {view_row, #{id => id2, key => key2, doc => doc2, worker => Worker}},
    Rows23 = {view_row, #{id => id3, key => key3}},
    Rows1 = [Rows11, Rows13],
    Rows2 = [Rows21, Rows23],
    Rows3 = [Rows11, Rows12, Rows13],
    Rows4 = [Rows21, Rows22, Rows23],
    State1 = #collector{
        sorted = true,
        limit = 10,
        callback = fun foo:bar/2,
        user_acc = accumulator,
        query_args = QueryArgs,
        counters = Counters1,
        rows = Rows1,
        collation = <<"raw">>
    },
    State2 = #collector{
        sorted = true,
        limit = 10,
        callback = fun foo:bar/2,
        user_acc = accumulator,
        query_args = QueryArgs,
        counters = Counters1,
        rows = Rows2,
        collation = <<"raw">>
    },
    State3 = #collector{
        sorted = true,
        limit = 10,
        callback = fun foo:bar/2,
        user_acc = accumulator,
        query_args = QueryArgs,
        counters = Counters2,
        rows = Rows3,
        collation = <<"raw">>
    },
    State4 = #collector{
        sorted = true,
        limit = 10,
        callback = fun foo:bar/2,
        user_acc = accumulator,
        query_args = QueryArgs,
        counters = Counters2,
        rows = Rows4,
        collation = <<"raw">>
    },
    meck:expect(foo, bar, [Props, accumulator], meck:val({go, updated_accumulator})),
    meck:expect(
        fabric_view,
        maybe_send_row,
        [
            {[State3], meck:val(next_row1)},
            {[State4], meck:val(next_row2)}
        ]
    ),
    ?assertEqual(next_row1, handle_message(Row1, Worker, State1)),
    ?assertEqual(next_row2, handle_message(Row2, Worker, State2)).

t_handle_message_complete(_) ->
    Worker = worker,
    Counters1 = [{Worker, 6}],
    Counters2 = [{Worker, 7}],
    State1 = #collector{counters = Counters1},
    State2 = #collector{counters = Counters2},
    meck:expect(fabric_view, maybe_send_row, [State2], meck:val(maybe_row)),
    ?assertEqual(maybe_row, handle_message(complete, Worker, State1)).

t_handle_message_execution_stats(_) ->
    Message = {execution_stats, stats},
    Source = {worker, from},
    meck:expect(foo, bar, [Message, accumulator], meck:val({go, updated_accumulator})),
    meck:expect(rexi, stream_ack, [from], meck:val(ok)),
    State1 = #collector{callback = fun foo:bar/2, user_acc = accumulator},
    State2 = #collector{callback = fun foo:bar/2, user_acc = updated_accumulator},
    ?assertEqual({go, State2}, handle_message(Message, Source, State1)).

t_handle_message_ddoc_updated(_) ->
    ?assertEqual({stop, state}, handle_message(ddoc_updated, source, state)).

t_handle_message_insufficient_storage(_) ->
    ?assertEqual({stop, state}, handle_message(insufficient_storage, source, state)).

merge_row_test_() ->
    {
        foreach,
        fun() -> ok end,
        fun(_) -> ok end,
        [
            ?TDEF_FE(t_merge_row_no_keys),
            ?TDEF_FE(t_merge_row_raw),
            ?TDEF_FE(t_merge_row)
        ]
    }.

t_merge_row_no_keys(_) ->
    Row1 = #view_row{id = id2, key = <<"key2">>},
    Rows11 = #view_row{id = id1, key = <<"key1">>},
    Rows13 = #view_row{id = id3, key = <<"key3">>},
    Rows1 = [Rows11, Rows13],
    Rows3 = [Rows11, Row1, Rows13],
    Row2 = {view_row, #{id => id2, key => <<"key2">>}},
    Rows21 = {view_row, #{id => id1, key => <<"key1">>}},
    Rows23 = {view_row, #{id => id3, key => <<"key3">>}},
    Rows2 = [Rows23, Rows21],
    Rows4 = [Rows23, Row2, Rows21],
    ?assertEqual({Rows3, undefined}, merge_row(fwd, <<"raw">>, undefined, Row1, Rows1)),
    ?assertEqual({Rows3, undefined}, merge_row(fwd, <<"collation">>, undefined, Row1, Rows1)),
    ?assertEqual({Rows4, undefined}, merge_row(rev, <<"raw">>, undefined, Row2, Rows2)),
    ?assertEqual({Rows4, undefined}, merge_row(rev, <<"collation">>, undefined, Row2, Rows2)).

t_merge_row_raw(_) ->
    Keys1 = dict:from_list([{key1, id1}, {key2, id2}, {key3, id3}]),
    Keys2 = dict:from_list([{key1, id1}, {key2, id2}, {key3, id3}]),
    Row1 = #view_row{id = id22, key = key2},
    Rows11 = #view_row{id = id1, key = key1},
    Rows13 = #view_row{id = id3, key = key3},
    Rows1 = [Rows11, Rows13],
    Rows3 = [Rows11, Row1, Rows13],
    Row2 = {view_row, #{id => id22, key => key2}},
    Rows21 = {view_row, #{id => id1, key => key1}},
    Rows23 = {view_row, #{id => id3, key => key3}},
    Rows2 = [Rows23, Rows21],
    Rows4 = [Row2, Rows23, Rows21],
    ?assertEqual({Rows3, Keys2}, merge_row(fwd, <<"raw">>, Keys1, Row1, Rows1)),
    ?assertEqual({Rows4, Keys2}, merge_row(rev, <<"raw">>, Keys1, Row2, Rows2)).

t_merge_row(_) ->
    Row1 = #view_row{id = id2, key = <<"key2">>},
    Rows11 = #view_row{id = id1, key = <<"key1">>},
    Rows13 = #view_row{id = id2, key = <<"key2">>},
    Rows1 = [Rows11, Rows13],
    Rows3 = [Rows11, Row1, Rows13],
    Row2 = {view_row, #{id => id2, key => <<"key2">>}},
    Rows21 = {view_row, #{id => id1, key => <<"key1">>}},
    Rows23 = {view_row, #{id => id2, key => <<"key2">>}},
    Rows2 = [Rows23, Rows21],
    Rows4 = [Rows23, Rows21, Row2],
    Keys1 = dict:from_list([{<<"key1">>, id1}, {<<"key2">>, id2}]),
    Keys2 = dict:from_list([]),
    ?assertEqual({Rows3, Keys1}, merge_row(fwd, <<"collation">>, Keys1, Row1, Rows1)),
    ?assertEqual({Rows4, Keys1}, merge_row(rev, <<"collation">>, Keys1, Row2, Rows2)),
    ?assertEqual({Rows1, Keys2}, merge_row(fwd, <<"collation">>, Keys2, Row1, Rows1)),
    ?assertEqual({Rows4, Keys1}, merge_row(fwd, <<"raw">>, Keys1, Row2, Rows2)).

-endif.
