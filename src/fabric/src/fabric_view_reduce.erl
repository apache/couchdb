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

-module(fabric_view_reduce).

-export([go/7]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("couch/include/couch_db.hrl").

go(DbName, GroupId, View, Args, Callback, Acc0, VInfo) when is_binary(GroupId) ->
    {ok, DDoc} = fabric:open_doc(DbName, <<"_design/", GroupId/binary>>, []),
    go(DbName, DDoc, View, Args, Callback, Acc0, VInfo);
go(Db, DDoc, VName, Args, Callback, Acc, VInfo) ->
    DbName = fabric:dbname(Db),
    {Shards, RingOpts} = fabric_view:get_shards(Db, Args),
    {CoordArgs, WorkerArgs} = fabric_view:fix_skip_and_limit(Args),
    DocIdAndRev = fabric_util:doc_id_and_rev(DDoc),
    RPCArgs = [DocIdAndRev, VName, WorkerArgs],
    fabric_view:maybe_update_others(DbName, DocIdAndRev, Shards, VName, Args),
    Repls = fabric_ring:get_shard_replacements(DbName, Shards),
    StartFun = fun(Shard) ->
        hd(fabric_streams:submit_jobs([Shard], fabric_rpc, reduce_view, RPCArgs))
    end,
    Workers0 = fabric_streams:submit_jobs(Shards, fabric_rpc, reduce_view, RPCArgs),
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
                Callback({error, insufficient_storage}, Acc);
            {ok, Workers} ->
                try
                    go2(DbName, DDoc#doc.id, VName, Workers, VInfo, CoordArgs, Callback, Acc)
                after
                    fabric_streams:cleanup(Workers)
                end;
            {timeout, DefunctWorkers} ->
                fabric_util:log_timeout(DefunctWorkers, "reduce_view"),
                Callback({error, timeout}, Acc);
            {error, Error} ->
                Callback({error, Error}, Acc)
        end
    after
        rexi_monitor:stop(RexiMon)
    end.

go2(DbName, DDocId, VName, Workers, {red, {_, Lang, View}, _} = VInfo, Args, Callback, Acc0) ->
    #mrargs{limit = Limit, skip = Skip, keys = Keys, update_seq = UpdateSeq} = Args,
    RedSrc = couch_mrview_util:extract_view_reduce(VInfo),
    OsProc =
        case os_proc_needed(RedSrc) of
            true -> couch_query_servers:get_os_process(Lang);
            _ -> nil
        end,
    State = #collector{
        db_name = DbName,
        ddoc_name = DDocId,
        view_name = VName,
        query_args = Args,
        callback = Callback,
        counters = fabric_dict:init(Workers, 0),
        keys = Keys,
        skip = Skip,
        limit = Limit,
        lang = Lang,
        os_proc = OsProc,
        reducer = RedSrc,
        collation = couch_util:get_value(<<"collation">>, View#mrview.options),
        rows = dict:new(),
        user_acc = Acc0,
        update_seq =
            case UpdateSeq of
                true -> [];
                false -> nil
            end
    },
    try
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
    after
        if
            OsProc == nil -> ok;
            true -> catch couch_query_servers:ret_os_process(OsProc)
        end
    end.

handle_row(Row0, {Worker, _} = Source, State) ->
    #collector{counters = Counters0, rows = Rows0} = State,
    true = fabric_dict:is_key(Worker, Counters0),
    Row = fabric_view_row:set_worker(Row0, Source),
    Key = fabric_view_row:get_key(Row),
    Rows = dict:append(Key, Row, Rows0),
    C1 = fabric_dict:update_counter(Worker, 1, Counters0),
    State1 = State#collector{rows = Rows, counters = C1},
    fabric_view:maybe_send_row(State1).

handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _, State) ->
    fabric_view:check_down_shards(State, NodeRef);
handle_message({rexi_EXIT, Reason}, Worker, State) ->
    fabric_view:handle_worker_exit(State, Worker, Reason);
handle_message({meta, Meta0}, {Worker, From}, State) ->
    Seq = couch_util:get_value(update_seq, Meta0, 0),
    #collector{
        callback = Callback,
        counters = Counters0,
        user_acc = AccIn,
        update_seq = UpdateSeq0
    } = State,
    % Assert that we don't have other messages from this
    % worker when the total_and_offset message arrives.
    0 = fabric_dict:lookup_element(Worker, Counters0),
    rexi:stream_ack(From),
    Counters1 = fabric_dict:update_counter(Worker, 1, Counters0),
    UpdateSeq =
        case UpdateSeq0 of
            nil -> nil;
            _ -> [{Worker, Seq} | UpdateSeq0]
        end,
    case fabric_dict:any(0, Counters1) of
        true ->
            {ok, State#collector{
                counters = Counters1,
                update_seq = UpdateSeq
            }};
        false ->
            Meta =
                case UpdateSeq of
                    nil ->
                        [];
                    _ ->
                        [{update_seq, fabric_view_changes:pack_seqs(UpdateSeq)}]
                end,
            {Go, Acc} = Callback({meta, Meta}, AccIn),
            {Go, State#collector{
                counters = fabric_dict:decrement_all(Counters1),
                user_acc = Acc
            }}
    end;
handle_message(#view_row{} = Row, {_, _} = Source, State) ->
    handle_row(Row, Source, State);
handle_message({view_row, #{}} = Row, {_, _} = Source, State) ->
    handle_row(Row, Source, State);
handle_message(complete, Worker, #collector{counters = Counters0} = State) ->
    true = fabric_dict:is_key(Worker, Counters0),
    C1 = fabric_dict:update_counter(Worker, 1, Counters0),
    fabric_view:maybe_send_row(State#collector{counters = C1});
handle_message(ddoc_updated, _Worker, State) ->
    {stop, State};
handle_message(insufficient_storage, _Worker, State) ->
    {stop, State}.

os_proc_needed(<<"_", _/binary>>) -> false;
os_proc_needed(_) -> true.

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
            ?TDEF_FE(t_handle_message_row),
            ?TDEF_FE(t_handle_message_complete),
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
    State1 = #collector{counters = Counters1, update_seq = nil},
    State2 = #collector{counters = Counters2, update_seq = nil},
    meck:expect(rexi, stream_ack, [from], meck:val(ok)),
    ?assertEqual({ok, State2}, handle_message({meta, Meta}, Worker, State1)).

t_handle_message_meta(_) ->
    Meta1 = [{update_seq, seq}],
    Meta2 = [{update_seq, packed_seq}],
    Meta3 = [],
    Worker = {worker1, from},
    Counters1 = [{worker1, 0}, {worker2, 3}, {worker3, 5}],
    Counters2 = [{worker1, 0}, {worker2, 2}, {worker3, 4}],
    State1 = #collector{
        counters = Counters1,
        update_seq = [],
        callback = fun foo:bar/2,
        user_acc = accumulator1
    },
    State2 = #collector{
        counters = Counters1,
        update_seq = nil,
        callback = fun foo:bar/2,
        user_acc = accumulator2
    },
    State3 = #collector{
        counters = Counters2,
        update_seq = [],
        callback = fun foo:bar/2,
        user_acc = updated_accumulator1
    },
    State4 = #collector{
        counters = Counters2,
        update_seq = nil,
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

t_handle_message_row(_) ->
    Worker = {worker, from},
    Counters1 = [{worker, 3}],
    Counters2 = [{worker, 4}],
    Row1 = #view_row{key = key1},
    Row11 = #view_row{key = key1, worker = Worker},
    Rows1 = dict:from_list([{key1, []}, {key2, []}, {key3, []}]),
    Rows3 = dict:from_list([{key1, [Row11]}, {key2, []}, {key3, []}]),
    Row2 = {view_row, #{key => key1}},
    Row21 = {view_row, #{key => key1, worker => Worker}},
    Rows2 = dict:from_list([{key1, []}, {key2, []}, {key3, []}]),
    Rows4 = dict:from_list([{key1, [Row21]}, {key2, []}, {key3, []}]),
    State1 = #collector{counters = Counters1, rows = Rows1},
    State2 = #collector{counters = Counters1, rows = Rows2},
    State3 = #collector{counters = Counters2, rows = Rows3},
    State4 = #collector{counters = Counters2, rows = Rows4},
    meck:expect(fabric_view, maybe_send_row, [
        {[State3], meck:val(maybe_row1)}, {[State4], meck:val(maybe_row2)}
    ]),
    ?assertEqual(maybe_row1, handle_message(Row1, Worker, State1)),
    ?assertEqual(maybe_row2, handle_message(Row2, Worker, State2)).

t_handle_message_complete(_) ->
    Worker = worker,
    Counters1 = [{Worker, 6}],
    Counters2 = [{Worker, 7}],
    State1 = #collector{counters = Counters1},
    State2 = #collector{counters = Counters2},
    meck:expect(fabric_view, maybe_send_row, [State2], meck:val(maybe_row)),
    ?assertEqual(maybe_row, handle_message(complete, Worker, State1)).

t_handle_message_ddoc_updated(_) ->
    ?assertEqual({stop, state}, handle_message(ddoc_updated, source, state)).

t_handle_message_insufficient_storage(_) ->
    ?assertEqual({stop, state}, handle_message(insufficient_storage, source, state)).

-endif.
