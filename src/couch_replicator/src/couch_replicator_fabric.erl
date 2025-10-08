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

-module(couch_replicator_fabric).

-export([
    docs/5
]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

docs(DbName, Options, QueryArgs, Callback, Acc) ->
    Shards = mem3:shards(DbName),
    Workers0 = fabric_streams:submit_jobs(
        Shards, couch_replicator_fabric_rpc, docs, [Options, QueryArgs]
    ),
    RexiMon = fabric_util:create_monitors(Workers0),
    try
        case fabric_streams:start(Workers0, #shard.ref) of
            {ok, Workers} ->
                try
                    docs_int(DbName, Workers, QueryArgs, Callback, Acc)
                after
                    fabric_streams:cleanup(Workers)
                end;
            {timeout, DefunctWorkers} ->
                fabric_util:log_timeout(DefunctWorkers, "replicator docs"),
                Callback({error, timeout}, Acc);
            {error, Error} ->
                Callback({error, Error}, Acc)
        end
    after
        rexi_monitor:stop(RexiMon)
    end.

docs_int(DbName, Workers, QueryArgs, Callback, Acc0) ->
    #mrargs{limit = Limit, skip = Skip} = QueryArgs,
    State = #collector{
        db_name = DbName,
        query_args = QueryArgs,
        callback = Callback,
        counters = fabric_dict:init(Workers, 0),
        skip = Skip,
        limit = Limit,
        user_acc = Acc0,
        update_seq = nil
    },
    case
        rexi_utils:recv(
            Workers,
            #shard.ref,
            fun handle_message/3,
            State,
            infinity,
            5000
        )
    of
        {ok, NewState} ->
            {ok, NewState#collector.user_acc};
        {timeout, NewState} ->
            Callback({error, timeout}, NewState#collector.user_acc);
        {error, Resp} ->
            {ok, Resp}
    end.

handle_row(Row0, {Worker, From} = Source, State) ->
    #collector{query_args = Args, counters = Counters0, rows = Rows0} = State,
    Id = fabric_view_row:get_id(Row0),
    Doc = fabric_view_row:get_doc(Row0),
    case maybe_fetch_and_filter_doc(Id, Doc, State) of
        {[_ | _]} = NewDoc ->
            Row1 = fabric_view_row:set_doc(Row0, NewDoc),
            Row = fabric_view_row:set_worker(Row1, Source),
            Dir = Args#mrargs.direction,
            Rows = merge_row(Dir, Row, Rows0),
            Counters1 = fabric_dict:update_counter(Worker, 1, Counters0),
            State1 = State#collector{rows = Rows, counters = Counters1},
            fabric_view:maybe_send_row(State1);
        skip ->
            rexi:stream_ack(From),
            {ok, State}
    end.

handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _, State) ->
    fabric_view:check_down_shards(State, NodeRef);
handle_message({rexi_EXIT, Reason}, Worker, State) ->
    fabric_view:handle_worker_exit(State, Worker, Reason);
handle_message({meta, Meta0}, {Worker, From}, State) ->
    Tot = couch_util:get_value(total, Meta0, 0),
    Off = couch_util:get_value(offset, Meta0, 0),
    #collector{
        callback = Callback,
        counters = Counters0,
        total_rows = Total0,
        offset = Offset0,
        user_acc = AccIn
    } = State,
    % Assert that we don't have other messages from this
    % worker when the total_and_offset message arrives.
    0 = fabric_dict:lookup_element(Worker, Counters0),
    rexi:stream_ack(From),
    Counters1 = fabric_dict:update_counter(Worker, 1, Counters0),
    Total = Total0 + Tot,
    Offset = Offset0 + Off,
    case fabric_dict:any(0, Counters1) of
        true ->
            {ok, State#collector{
                counters = Counters1,
                total_rows = Total,
                offset = Offset
            }};
        false ->
            FinalOffset = min(Total, Offset + State#collector.skip),
            Meta = [{total, Total}, {offset, FinalOffset}],
            {Go, Acc} = Callback({meta, Meta}, AccIn),
            {Go, State#collector{
                counters = fabric_dict:decrement_all(Counters1),
                total_rows = Total,
                offset = FinalOffset,
                user_acc = Acc
            }}
    end;
handle_message(#view_row{} = Row, {_, _} = Source, State) ->
    handle_row(Row, Source, State);
handle_message({view_row, #{}} = Row, {_, _} = Source, State) ->
    handle_row(Row, Source, State);
handle_message(complete, Worker, State) ->
    Counters = fabric_dict:update_counter(Worker, 1, State#collector.counters),
    fabric_view:maybe_send_row(State#collector{counters = Counters}).

merge_row(Dir, Row, Rows) ->
    lists:merge(
        fun(RowA, RowB) ->
            IdA = fabric_view_row:get_id(RowA),
            IdB = fabric_view_row:get_id(RowB),
            case Dir of
                fwd -> IdA < IdB;
                rev -> IdA > IdB
            end
        end,
        [Row],
        Rows
    ).

maybe_fetch_and_filter_doc(Id, undecided, State) ->
    #collector{db_name = DbName, query_args = #mrargs{extra = Options}} = State,
    FilterStates = couch_util:get_value(filter_states, Options),
    case couch_replicator:active_doc(DbName, Id) of
        {ok, {Props} = DocInfo} ->
            DocState = couch_util:get_value(state, Props),
            couch_replicator_utils:filter_state(DocState, FilterStates, DocInfo);
        {ok, nil} ->
            % could have just completed
            skip;
        {error, not_found} ->
            % could have been deleted
            skip
    end;
maybe_fetch_and_filter_doc(_Id, Doc, _State) ->
    Doc.

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
            ?TDEF_FE(t_handle_message_row_skip),
            ?TDEF_FE(t_handle_message_row),
            ?TDEF_FE(t_handle_message_complete)
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
    Meta1 = [{total, 10}, {offset, 2}],
    Meta2 = [{total, 10}, {offset, 5}],
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
    meck:expect(rexi, stream_ack, [from], meck:val(ok)),
    ?assertEqual({go1, State3}, handle_message({meta, Meta1}, Worker, State1)),
    ?assertEqual({go2, State4}, handle_message({meta, Meta1}, Worker, State2)).

t_handle_message_row_skip(_) ->
    State = #collector{db_name = db, query_args = #mrargs{}},
    Worker = {worker, from},
    Row1 = #view_row{id = id, doc = undecided},
    Row2 = {view_row, #{id => id, doc => undecided}},
    meck:expect(couch_replicator, active_doc, [db, id], meck:val({error, not_found})),
    meck:expect(rexi, stream_ack, [from], meck:val(ok)),
    ?assertEqual({ok, State}, handle_message(Row1, Worker, State)),
    ?assertEqual({ok, State}, handle_message(Row2, Worker, State)).

t_handle_message_row(_) ->
    QueryArgs = #mrargs{direction = fwd, extra = [{filter_states, []}]},
    Counters1 = [{worker, 4}],
    Counters2 = [{worker, 5}],
    Worker = {worker, from},
    Doc = {[{<<"_id">>, doc_id}, {<<"_rev">>, doc_rev}]},
    Row1 = #view_row{id = id, doc = Doc},
    Row11 = #view_row{id = id, doc = Doc, worker = Worker},
    Row2 = {view_row, #{id => id, doc => undecided}},
    Row21 = {view_row, #{id => id, doc => Doc, worker => Worker}},
    Rows1 = [],
    Rows2 = [],
    Rows3 = [Row11],
    Rows4 = [Row21],
    State1 = #collector{db_name = db, query_args = QueryArgs, counters = Counters1, rows = Rows1},
    State2 = #collector{db_name = db, query_args = QueryArgs, counters = Counters1, rows = Rows2},
    State3 = #collector{db_name = db, query_args = QueryArgs, counters = Counters2, rows = Rows3},
    State4 = #collector{db_name = db, query_args = QueryArgs, counters = Counters2, rows = Rows4},
    meck:expect(couch_replicator, active_doc, [db, id], meck:val({ok, Doc})),
    meck:expect(fabric_view, maybe_send_row, [
        {[State3], meck:val(next_row1)}, {[State4], meck:val(next_row2)}
    ]),
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

merge_row_test_() ->
    {
        foreach,
        fun() -> ok end,
        fun(_) -> ok end,
        [
            ?TDEF_FE(t_merge_row_record_fwd),
            ?TDEF_FE(t_merge_row_record_rev),
            ?TDEF_FE(t_merge_row_map_fwd),
            ?TDEF_FE(t_merge_row_map_rev),
            ?TDEF_FE(t_merge_row_mixed_fwd),
            ?TDEF_FE(t_merge_row_mixed_rev)
        ]
    }.

t_merge_row_record_fwd(_) ->
    RowX1 = #view_row{id = 4},
    Row1 = #view_row{id = 1},
    Row2 = #view_row{id = 3},
    Row3 = #view_row{id = 5},
    Row4 = #view_row{id = 7},
    Rows = [Row1, Row2, Row3, Row4],
    Expected1 = [Row1, Row2, RowX1, Row3, Row4],
    ?assertEqual(Expected1, merge_row(fwd, RowX1, Rows)),
    RowX2 = #view_row{id = 0},
    Expected2 = [RowX2, Row1, Row2, Row3, Row4],
    ?assertEqual(Expected2, merge_row(fwd, RowX2, Rows)),
    RowX3 = #view_row{id = 8},
    Expected3 = [Row1, Row2, Row3, Row4, RowX3],
    ?assertEqual(Expected3, merge_row(fwd, RowX3, Rows)),
    RowX4 = #view_row{id = 5},
    Expected4 = [Row1, Row2, RowX4, Row3, Row4],
    ?assertEqual(Expected4, merge_row(fwd, RowX4, Rows)).

t_merge_row_record_rev(_) ->
    RowX1 = #view_row{id = 5},
    Row1 = #view_row{id = 2},
    Row2 = #view_row{id = 4},
    Row3 = #view_row{id = 6},
    Row4 = #view_row{id = 8},
    Rows = [Row4, Row3, Row2, Row1],
    Expected1 = [Row4, Row3, RowX1, Row2, Row1],
    ?assertEqual(Expected1, merge_row(rev, RowX1, Rows)),
    RowX2 = #view_row{id = 1},
    Expected2 = [Row4, Row3, Row2, Row1, RowX2],
    ?assertEqual(Expected2, merge_row(rev, RowX2, Rows)),
    RowX3 = #view_row{id = 9},
    Expected3 = [RowX3, Row4, Row3, Row2, Row1],
    ?assertEqual(Expected3, merge_row(rev, RowX3, Rows)),
    RowX4 = #view_row{id = 6},
    Expected4 = [Row4, Row3, RowX4, Row2, Row1],
    ?assertEqual(Expected4, merge_row(rev, RowX4, Rows)).

t_merge_row_map_fwd(_) ->
    RowX1 = {view_row, #{id => 4}},
    Row1 = {view_row, #{id => 1}},
    Row2 = {view_row, #{id => 3}},
    Row3 = {view_row, #{id => 5}},
    Row4 = {view_row, #{id => 7}},
    Rows = [Row1, Row2, Row3, Row4],
    Expected1 = [Row1, Row2, RowX1, Row3, Row4],
    ?assertEqual(Expected1, merge_row(fwd, RowX1, Rows)),
    RowX2 = {view_row, #{id => 0}},
    Expected2 = [RowX2, Row1, Row2, Row3, Row4],
    ?assertEqual(Expected2, merge_row(fwd, RowX2, Rows)),
    RowX3 = {view_row, #{id => 8}},
    Expected3 = [Row1, Row2, Row3, Row4, RowX3],
    ?assertEqual(Expected3, merge_row(fwd, RowX3, Rows)),
    RowX4 = {view_row, #{id => 5}},
    Expected4 = [Row1, Row2, RowX4, Row3, Row4],
    ?assertEqual(Expected4, merge_row(fwd, RowX4, Rows)).

t_merge_row_map_rev(_) ->
    RowX1 = {view_row, #{id => 5}},
    Row1 = {view_row, #{id => 2}},
    Row2 = {view_row, #{id => 4}},
    Row3 = {view_row, #{id => 6}},
    Row4 = {view_row, #{id => 8}},
    Rows = [Row4, Row3, Row2, Row1],
    Expected1 = [Row4, Row3, RowX1, Row2, Row1],
    ?assertEqual(Expected1, merge_row(rev, RowX1, Rows)),
    RowX2 = {view_row, #{id => 1}},
    Expected2 = [Row4, Row3, Row2, Row1, RowX2],
    ?assertEqual(Expected2, merge_row(rev, RowX2, Rows)),
    RowX3 = {view_row, #{id => 9}},
    Expected3 = [RowX3, Row4, Row3, Row2, Row1],
    ?assertEqual(Expected3, merge_row(rev, RowX3, Rows)),
    RowX4 = {view_row, #{id => 6}},
    Expected4 = [Row4, Row3, RowX4, Row2, Row1],
    ?assertEqual(Expected4, merge_row(rev, RowX4, Rows)).

t_merge_row_mixed_fwd(_) ->
    RowX1 = #view_row{id = 4},
    Row1 = {view_row, #{id => 1}},
    Row2 = {view_row, #{id => 3}},
    Row3 = #view_row{id = 5},
    Row4 = {view_row, #{id => 7}},
    Rows = [Row1, Row2, Row3, Row4],
    Expected1 = [Row1, Row2, RowX1, Row3, Row4],
    ?assertEqual(Expected1, merge_row(fwd, RowX1, Rows)),
    RowX2 = {view_row, #{id => 0}},
    Expected2 = [RowX2, Row1, Row2, Row3, Row4],
    ?assertEqual(Expected2, merge_row(fwd, RowX2, Rows)),
    RowX3 = {view_row, #{id => 8}},
    Expected3 = [Row1, Row2, Row3, Row4, RowX3],
    ?assertEqual(Expected3, merge_row(fwd, RowX3, Rows)),
    RowX4 = {view_row, #{id => 5}},
    Expected4 = [Row1, Row2, Row3, RowX4, Row4],
    ?assertEqual(Expected4, merge_row(fwd, RowX4, Rows)).

t_merge_row_mixed_rev(_) ->
    RowX1 = {view_row, #{id => 5}},
    Row1 = #view_row{id = 2},
    Row2 = #view_row{id = 4},
    Row3 = {view_row, #{id => 6}},
    Row4 = #view_row{id = 8},
    Rows = [Row4, Row3, Row2, Row1],
    Expected1 = [Row4, Row3, RowX1, Row2, Row1],
    ?assertEqual(Expected1, merge_row(rev, RowX1, Rows)),
    RowX2 = #view_row{id = 1},
    Expected2 = [Row4, Row3, Row2, Row1, RowX2],
    ?assertEqual(Expected2, merge_row(rev, RowX2, Rows)),
    RowX3 = #view_row{id = 9},
    Expected3 = [RowX3, Row4, Row3, Row2, Row1],
    ?assertEqual(Expected3, merge_row(rev, RowX3, Rows)),
    RowX4 = #view_row{id = 6},
    Expected4 = [Row4, Row3, RowX4, Row2, Row1],
    ?assertEqual(Expected4, merge_row(rev, RowX4, Rows)).

-endif.
