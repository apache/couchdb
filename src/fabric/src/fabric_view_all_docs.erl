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

-module(fabric_view_all_docs).

-export([go/5]).
% exported for spawn
-export([open_doc/5]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

go(Db, Options, #mrargs{keys = undefined} = QueryArgs, Callback, Acc) ->
    {CoordArgs, WorkerArgs} = fabric_view:fix_skip_and_limit(QueryArgs),
    DbName = fabric:dbname(Db),
    {Shards, RingOpts} = shards(Db, QueryArgs),
    Workers0 = fabric_streams:submit_jobs(
        Shards, fabric_rpc, all_docs, [Options, WorkerArgs]
    ),
    RexiMon = fabric_util:create_monitors(Workers0),
    try
        case fabric_streams:start(Workers0, #shard.ref, RingOpts) of
            {ok, Workers} ->
                try
                    go(DbName, Options, Workers, CoordArgs, Callback, Acc)
                after
                    fabric_streams:cleanup(Workers)
                end;
            {timeout, DefunctWorkers} ->
                fabric_util:log_timeout(DefunctWorkers, "all_docs"),
                Callback({error, timeout}, Acc);
            {error, Error} ->
                Callback({error, Error}, Acc)
        end
    after
        rexi_monitor:stop(RexiMon)
    end;
go(DbName, Options, QueryArgs, Callback, Acc0) ->
    #mrargs{
        direction = Dir,
        include_docs = IncludeDocs,
        doc_options = DocOptions0,
        limit = Limit,
        conflicts = Conflicts,
        skip = Skip,
        keys = Keys0,
        extra = Extra,
        update_seq = UpdateSeq
    } = QueryArgs,
    DocOptions1 =
        case Conflicts of
            true -> [conflicts | DocOptions0];
            _ -> DocOptions0
        end,
    SpawnFun = fun(Key) ->
        spawn_monitor(?MODULE, open_doc, [DbName, Options ++ DocOptions1, Key, IncludeDocs, Extra])
    end,
    MaxJobs = all_docs_concurrency(),
    %% namespace can be _set_ to `undefined`, so we want simulate enum here
    Namespace =
        case couch_util:get_value(namespace, Extra) of
            <<"_all_docs">> -> <<"_all_docs">>;
            <<"_design">> -> <<"_design">>;
            <<"_non_design">> -> <<"_non_design">>;
            <<"_local">> -> <<"_local">>;
            _ -> <<"_all_docs">>
        end,
    Keys1 =
        case Dir of
            fwd -> Keys0;
            _ -> lists:reverse(Keys0)
        end,
    Keys2 =
        case Skip < length(Keys1) of
            true -> lists:nthtail(Skip, Keys1);
            false -> []
        end,
    Keys3 =
        case Limit < length(Keys2) of
            true -> lists:sublist(Keys2, Limit);
            false -> Keys2
        end,
    Keys4 = filter_keys_by_namespace(Keys3, Namespace),
    Timeout = fabric_util:all_docs_timeout(),
    {_, Ref} = spawn_monitor(fun() ->
        exit(fabric:get_doc_count(DbName, Namespace))
    end),
    receive
        {'DOWN', Ref, _, _, {ok, TotalRows}} ->
            Meta =
                case UpdateSeq of
                    false ->
                        [{total, TotalRows}, {offset, null}];
                    true ->
                        [{total, TotalRows}, {offset, null}, {update_seq, null}]
                end,
            {ok, Acc1} = Callback({meta, Meta}, Acc0),
            Resp = doc_receive_loop(
                Keys4, queue:new(), SpawnFun, MaxJobs, Callback, Acc1
            ),
            case Resp of
                {ok, Acc2} ->
                    Callback(complete, Acc2);
                timeout ->
                    Callback({error, timeout}, Acc0)
            end;
        {'DOWN', Ref, _, _, Error} ->
            Callback({error, Error}, Acc0)
    after Timeout ->
        Callback({error, timeout}, Acc0)
    end.

go(DbName, _Options, Workers, QueryArgs, Callback, Acc0) ->
    #mrargs{limit = Limit, skip = Skip, update_seq = UpdateSeq} = QueryArgs,
    State = #collector{
        db_name = DbName,
        query_args = QueryArgs,
        callback = Callback,
        counters = fabric_dict:init(Workers, 0),
        skip = Skip,
        limit = Limit,
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
            fabric_util:view_timeout(QueryArgs),
            fabric_util:timeout("all_docs_view_permsg", "5000")
        )
    of
        {ok, NewState} ->
            {ok, NewState#collector.user_acc};
        {timeout, NewState} ->
            Callback({error, timeout}, NewState#collector.user_acc);
        {error, Resp} ->
            {ok, Resp}
    end.

shards(Db, Args) ->
    DbPartitioned = fabric_util:is_partitioned(Db),
    Partition = couch_mrview_util:get_extra(Args, partition),
    NewArgs =
        case {DbPartitioned, Partition} of
            {true, undefined} ->
                % If a user specifies the same partition on both
                % the start and end keys we can optimize the
                % query by limiting to the partition shard.
                Start = couch_partition:extract(Args#mrargs.start_key),
                End = couch_partition:extract(Args#mrargs.end_key),
                case {Start, End} of
                    {{Partition, SK}, {Partition, EK}} ->
                        A1 = Args#mrargs{
                            start_key = SK,
                            end_key = EK
                        },
                        couch_mrview_util:set_extra(A1, partition, Partition);
                    _ ->
                        Args
                end;
            _ ->
                Args
        end,
    fabric_view:get_shards(Db, NewArgs).

handle_row(Row0, {Worker, _} = Source, State) ->
    #collector{query_args = Args, counters = Counters0, rows = Rows0} = State,
    Dir = Args#mrargs.direction,
    Row = fabric_view_row:set_worker(Row0, Source),
    Rows = merge_row(Dir, Row, Rows0),
    Counters1 = fabric_dict:update_counter(Worker, 1, Counters0),
    State1 = State#collector{rows = Rows, counters = Counters1},
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
    Total =
        if
            Tot == null -> null;
            true -> Total0 + Tot
        end,
    Offset =
        if
            Off == null -> null;
            true -> Offset0 + Off
        end,
    UpdateSeq =
        case {UpdateSeq0, Seq} of
            {nil, _} -> nil;
            {_, null} -> null;
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
            FinalOffset =
                case Offset of
                    null -> null;
                    _ -> min(Total, Offset + State#collector.skip)
                end,
            Meta =
                [{total, Total}, {offset, FinalOffset}] ++
                    case UpdateSeq of
                        nil ->
                            [];
                        null ->
                            [{update_seq, null}];
                        _ ->
                            [{update_seq, fabric_view_changes:pack_seqs(UpdateSeq)}]
                    end,
            {Go, Acc} = Callback({meta, Meta}, AccIn),
            {Go, State#collector{
                counters = fabric_dict:decrement_all(Counters1),
                total_rows = Total,
                offset = FinalOffset,
                user_acc = Acc,
                update_seq = UpdateSeq0
            }}
    end;
handle_message(#view_row{} = Row, {_, _} = Source, State) ->
    handle_row(Row, Source, State);
handle_message({view_row, #{}} = Row, {_, _} = Source, State) ->
    handle_row(Row, Source, State);
handle_message(complete, Worker, State) ->
    Counters = fabric_dict:update_counter(Worker, 1, State#collector.counters),
    fabric_view:maybe_send_row(State#collector{counters = Counters});
handle_message({execution_stats, _} = Msg, {_, From}, St) ->
    #collector{callback = Callback, user_acc = AccIn} = St,
    {Go, Acc} = Callback(Msg, AccIn),
    rexi:stream_ack(From),
    {Go, St#collector{user_acc = Acc}}.

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

all_docs_concurrency() ->
    Value = config:get("fabric", "all_docs_concurrency", "10"),
    try
        list_to_integer(Value)
    catch
        _:_ ->
            10
    end.

doc_receive_loop(Keys, Pids, SpawnFun, MaxJobs, Callback, AccIn) ->
    case {Keys, queue:len(Pids)} of
        {[], 0} ->
            {ok, AccIn};
        {[K | RKeys], Len} when Len < MaxJobs ->
            Pids1 = queue:in(SpawnFun(K), Pids),
            doc_receive_loop(RKeys, Pids1, SpawnFun, MaxJobs, Callback, AccIn);
        _ ->
            {{value, {Pid, Ref}}, RestPids} = queue:out(Pids),
            Timeout = fabric_util:all_docs_timeout(),
            Receive = fun(Row) ->
                case Callback(fabric_view_row:transform(Row), AccIn) of
                    {ok, Acc} ->
                        doc_receive_loop(
                            Keys, RestPids, SpawnFun, MaxJobs, Callback, Acc
                        );
                    {stop, Acc} ->
                        cancel_read_pids(RestPids),
                        {ok, Acc}
                end
            end,
            receive
                {'DOWN', Ref, process, Pid, Row} ->
                    case Row of
                        #view_row{} ->
                            Receive(Row);
                        {view_row, #{}} ->
                            Receive(Row);
                        Error ->
                            cancel_read_pids(RestPids),
                            Callback({error, Error}, AccIn)
                    end
            after Timeout ->
                timeout
            end
    end.

open_doc(DbName, Options, Id, IncludeDocs, Extra) ->
    try open_doc_int(DbName, Options, Id, IncludeDocs, Extra) of
        #view_row{} = Row ->
            exit(Row);
        {view_row, #{}} = Row ->
            exit(Row)
    catch
        Type:Reason:Stack ->
            couch_log:error("_all_docs open error: ~s ~s :: ~w ~w", [
                DbName, Id, {Type, Reason}, Stack
            ]),
            exit({Id, Reason})
    end.

open_doc_int(DbName, Options, Id, IncludeDocs, Extra) ->
    Row =
        case fabric:open_doc(DbName, Id, [deleted | Options]) of
            {not_found, missing} ->
                Doc = undefined,
                fabric_view_row:from_props([{key, Id}], Extra);
            {ok, #doc{deleted = true, revs = Revs}} ->
                Doc = null,
                {RevPos, [RevId | _]} = Revs,
                Value = {[{rev, couch_doc:rev_to_str({RevPos, RevId})}, {deleted, true}]},
                fabric_view_row:from_props([{key, Id}, {id, Id}, {value, Value}], Extra);
            {ok, #doc{revs = Revs} = Doc0} ->
                Doc = couch_doc:to_json_obj(Doc0, Options),
                {RevPos, [RevId | _]} = Revs,
                Value = {[{rev, couch_doc:rev_to_str({RevPos, RevId})}]},
                fabric_view_row:from_props([{key, Id}, {id, Id}, {value, Value}], Extra)
        end,
    if
        IncludeDocs -> fabric_view_row:set_doc(Row, Doc);
        true -> Row
    end.

cancel_read_pids(Pids) ->
    case queue:out(Pids) of
        {{value, {Pid, Ref}}, RestPids} ->
            exit(Pid, kill),
            demonitor(Ref, [flush]),
            cancel_read_pids(RestPids);
        {empty, _} ->
            ok
    end.

filter_keys_by_namespace(Keys, Namespace) when Namespace =:= <<"_design">> ->
    lists:filter(
        fun(Key) ->
            case Key of
                <<?DESIGN_DOC_PREFIX, _/binary>> -> true;
                _ -> false
            end
        end,
        Keys
    );
filter_keys_by_namespace(Keys, Namespace) when Namespace =:= <<"_local">> ->
    lists:filter(
        fun(Key) ->
            case Key of
                <<?LOCAL_DOC_PREFIX, _/binary>> -> true;
                _ -> false
            end
        end,
        Keys
    );
filter_keys_by_namespace(Keys, _Namespace) ->
    Keys.

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
            ?TDEF_FE(t_handle_message_execution_stats)
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
    Meta1 = [{total, 3}, {offset, 2}, {update_seq, 1}],
    Meta2 = [{total, null}, {offset, null}, {update_seq, null}],
    Worker = {worker1, from},
    Counters1 = [{worker1, 0}, {worker2, 0}],
    Counters2 = [{worker1, 1}, {worker2, 0}],
    State1 = #collector{counters = Counters1, total_rows = 0, update_seq = nil, offset = 0},
    State2 = #collector{counters = Counters2, total_rows = 3, update_seq = nil, offset = 2},
    State3 = #collector{counters = Counters1, total_rows = null, update_seq = null, offset = null},
    State4 = #collector{counters = Counters2, total_rows = null, update_seq = null, offset = null},
    meck:expect(rexi, stream_ack, [from], meck:val(ok)),
    ?assertEqual({ok, State2}, handle_message({meta, Meta1}, Worker, State1)),
    ?assertEqual({ok, State4}, handle_message({meta, Meta2}, Worker, State3)).

t_handle_message_meta(_) ->
    Meta1 = [{total, 10}, {offset, 2}, {update_seq, seq}],
    Meta2 = [{total, 10}, {offset, 5}, {update_seq, packed_seq}],
    Meta3 = [{total, 10}, {offset, 5}],
    Meta4 = [{total, null}, {offset, null}, {update_seq, null}],
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
        counters = Counters1,
        total_rows = 0,
        update_seq = [],
        offset = 0,
        skip = 3,
        callback = fun foo:bar/2,
        user_acc = accumulator3
    },
    State4 = #collector{
        counters = Counters2,
        total_rows = 10,
        update_seq = [],
        offset = 5,
        skip = 3,
        callback = fun foo:bar/2,
        user_acc = updated_accumulator1
    },
    State5 = #collector{
        counters = Counters2,
        total_rows = 10,
        update_seq = nil,
        offset = 5,
        skip = 3,
        callback = fun foo:bar/2,
        user_acc = updated_accumulator2
    },
    State6 = #collector{
        counters = Counters2,
        total_rows = null,
        update_seq = [],
        offset = null,
        skip = 3,
        callback = fun foo:bar/2,
        user_acc = updated_accumulator3
    },
    meck:expect(fabric_view_changes, pack_seqs, [[{worker1, seq}]], meck:val(packed_seq)),
    meck:expect(rexi, stream_ack, [from], meck:val(ok)),
    meck:expect(foo, bar, [{meta, Meta2}, accumulator1], meck:val({go1, updated_accumulator1})),
    ?assertEqual({go1, State4}, handle_message({meta, Meta1}, Worker, State1)),
    meck:expect(foo, bar, [{meta, Meta3}, accumulator2], meck:val({go2, updated_accumulator2})),
    ?assertEqual({go2, State5}, handle_message({meta, Meta1}, Worker, State2)),
    meck:expect(foo, bar, [{meta, Meta4}, accumulator3], meck:val({go3, updated_accumulator3})),
    ?assertEqual({go3, State6}, handle_message({meta, Meta4}, Worker, State3)).

t_handle_message_row(_) ->
    Worker = {worker, from},
    QueryArgs1 = #mrargs{direction = fwd},
    QueryArgs2 = #mrargs{direction = rev},
    Counters1 = [{worker, 1}],
    Counters2 = [{worker, 2}],
    Row1 = #view_row{id = id2, key = key2, doc = doc2},
    Row2 = {view_row, #{id => id2, key => key2, doc => doc2}},
    Rows11 = #view_row{id = id1, key = key1, doc = doc1},
    Rows12 = #view_row{id = id2, key = key2, doc = doc2, worker = Worker},
    Rows13 = #view_row{id = id3, key = key3, doc = doc3},
    Rows21 = {view_row, #{id => id1, key => key1}},
    Rows22 = {view_row, #{id => id2, key => key2, doc => doc2, worker => Worker}},
    Rows23 = {view_row, #{id => id3, key => key3}},
    Rows1 = [Rows11, Rows13],
    Rows2 = [Rows23, Rows21],
    Rows3 = [Rows11, Rows12, Rows13],
    Rows4 = [Rows23, Rows22, Rows21],
    State1 = #collector{query_args = QueryArgs1, counters = Counters1, rows = Rows1},
    State2 = #collector{query_args = QueryArgs2, counters = Counters1, rows = Rows2},
    State3 = #collector{query_args = QueryArgs1, counters = Counters2, rows = Rows3},
    State4 = #collector{query_args = QueryArgs2, counters = Counters2, rows = Rows4},
    meck:expect(fabric_view, maybe_send_row, [State3], meck:val(send_row1)),
    ?assertEqual(send_row1, handle_message(Row1, Worker, State1)),
    meck:expect(fabric_view, maybe_send_row, [State4], meck:val(send_row2)),
    ?assertEqual(send_row2, handle_message(Row2, Worker, State2)).

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

open_doc_test_() ->
    {
        foreach,
        fun() -> meck:new(fabric) end,
        fun(_) -> meck:unload() end,
        [
            ?TDEF_FE(t_open_doc_not_found),
            ?TDEF_FE(t_open_doc_deleted),
            ?TDEF_FE(t_open_doc),
            ?TDEF_FE(t_open_doc_error)
        ]
    }.

t_open_doc_not_found(_) ->
    Extra1 = [],
    Extra2 = [{view_row_map, true}],
    Options1 = [],
    Options2 = [deleted],
    Row1 = #view_row{key = id, doc = undefined},
    Row2 = {view_row, #{key => id}},
    meck:expect(fabric, open_doc, [db, id, Options2], meck:val({not_found, missing})),
    {_, Ref1} = spawn_monitor(?MODULE, open_doc, [db, Options1, id, true, Extra1]),
    receive
        {'DOWN', Ref1, _, _, Result1} ->
            ?assertEqual(Row1, Result1)
    end,
    {_, Ref2} = spawn_monitor(?MODULE, open_doc, [db, Options1, id, false, Extra2]),
    receive
        {'DOWN', Ref2, _, _, Result2} ->
            ?assertEqual(Row2, Result2)
    end.

t_open_doc_deleted(_) ->
    Extra1 = [],
    Extra2 = [{view_row_map, true}],
    Options1 = [],
    Options2 = [deleted],
    Revs = {1, [<<"foo">>]},
    Doc = #doc{deleted = true, revs = Revs},
    Value = {[{rev, <<"1-foo">>}, {deleted, true}]},
    Row1 = #view_row{key = id, id = id, value = Value, doc = null},
    Row2 = {view_row, #{key => id, id => id, value => Value, doc => null}},
    meck:expect(fabric, open_doc, [db, id, Options2], meck:val({ok, Doc})),
    {_, Ref1} = spawn_monitor(?MODULE, open_doc, [db, Options1, id, true, Extra1]),
    receive
        {'DOWN', Ref1, _, _, Result1} ->
            ?assertEqual(Row1, Result1)
    end,
    {_, Ref2} = spawn_monitor(?MODULE, open_doc, [db, Options1, id, true, Extra2]),
    receive
        {'DOWN', Ref2, _, _, Result2} ->
            ?assertEqual(Row2, Result2)
    end.

t_open_doc(_) ->
    Extra1 = [],
    Extra2 = [{view_row_map, true}],
    Options1 = [],
    Options2 = [deleted],
    Revs = {1, [<<"foo">>]},
    Doc = #doc{revs = Revs, id = <<"bar">>},
    DocJson = {[{<<"_id">>, <<"bar">>}, {<<"_rev">>, <<"1-foo">>}]},
    Value = {[{rev, <<"1-foo">>}]},
    Row1 = #view_row{key = id, id = id, value = Value, doc = DocJson},
    Row2 = {view_row, #{key => id, id => id, value => Value, doc => DocJson}},
    meck:expect(fabric, open_doc, [db, id, Options2], meck:val({ok, Doc})),
    {_, Ref1} = spawn_monitor(?MODULE, open_doc, [db, Options1, id, true, Extra1]),
    receive
        {'DOWN', Ref1, _, _, Result1} ->
            ?assertEqual(Row1, Result1)
    end,
    {_, Ref2} = spawn_monitor(?MODULE, open_doc, [db, Options1, id, true, Extra2]),
    receive
        {'DOWN', Ref2, _, _, Result2} ->
            ?assertEqual(Row2, Result2)
    end.

t_open_doc_error(_) ->
    Extra = [],
    Options1 = [],
    Options2 = [deleted],
    Exception = {id, reason},
    meck:expect(fabric, open_doc, [db, id, Options2], meck:raise(error, reason)),
    meck:expect(
        couch_log,
        error,
        ["_all_docs open error: ~s ~s :: ~w ~w", [db, id, {error, reason}, '_']],
        meck:val(ok)
    ),
    {_, Ref} = spawn_monitor(?MODULE, open_doc, [db, Options1, id, true, Extra]),
    receive
        {'DOWN', Ref, _, _, Result} ->
            ?assertEqual(Exception, Result)
    end.

doc_receive_loop_test_() ->
    {
        foreach,
        fun() ->
            meck:new(foo, [non_strict]),
            meck:new(fabric_util)
        end,
        fun(_) -> meck:unload() end,
        [
            ?TDEF_FE(t_doc_receive_loop_empty),
            ?TDEF_FE(t_doc_receive_loop),
            ?TDEF_FE(t_doc_receive_loop_error),
            ?TDEF_FE(t_doc_receive_loop_timeout)
        ]
    }.

t_doc_receive_loop_empty(_) ->
    Keys = [],
    Pids = queue:new(),
    ?assertEqual(
        {ok, accumulator},
        doc_receive_loop(Keys, Pids, undefined, undefined, undefined, accumulator)
    ).

t_doc_receive_loop(_) ->
    Keys = [key1, key2, key3],
    Pids = queue:from_list([]),
    MaxJobs = 4,
    Props1 = {row, [{id, id1}, {key, key1}, {value, value1}]},
    Props2 = {row, [{id, id2}, {key, key2}, {value, value2}]},
    Row1 = #view_row{id = id1, key = key1, value = value1},
    Row2 = {view_row, #{id => id2, key => key2, value => value2}},
    meck:expect(
        foo,
        spawned,
        [
            {[key1], meck:raise(exit, Row1)},
            {[key2], meck:raise(exit, Row2)}
        ]
    ),
    meck:expect(foo, spawn, fun(K) -> spawn_monitor(foo, spawned, [K]) end),
    meck:expect(
        foo,
        callback,
        [
            {[Props1, accumulator1], meck:val({ok, accumulator2})},
            {[Props2, accumulator2], meck:val({stop, accumulator3})}
        ]
    ),
    meck:expect(fabric_util, all_docs_timeout, [], meck:val(1000)),
    ?assertEqual(
        {ok, accumulator3},
        doc_receive_loop(Keys, Pids, fun foo:spawn/1, MaxJobs, fun foo:callback/2, accumulator1)
    ).

t_doc_receive_loop_error(_) ->
    Keys = [key1, key2, key3],
    Pids = queue:from_list([]),
    MaxJobs = 3,
    meck:expect(foo, spawned, [key1], meck:raise(exit, error)),
    meck:expect(foo, spawn, fun(K) -> spawn_monitor(foo, spawned, [K]) end),
    meck:expect(foo, callback, [{error, error}, accumulator1], meck:val({ok, accumulator2})),
    meck:expect(fabric_util, all_docs_timeout, [], meck:val(1000)),
    ?assertEqual(
        {ok, accumulator2},
        doc_receive_loop(Keys, Pids, fun foo:spawn/1, MaxJobs, fun foo:callback/2, accumulator1)
    ).

t_doc_receive_loop_timeout(_) ->
    Keys = [key1, key2, key3],
    Pids = queue:from_list([]),
    MaxJobs = 3,
    meck:expect(foo, spawned, fun(key1) -> timer:sleep(infinity) end),
    meck:expect(foo, spawn, fun(K) -> spawn_monitor(foo, spawned, [K]) end),
    meck:expect(foo, callback, ['_', '_'], undefined),
    meck:expect(fabric_util, all_docs_timeout, [], meck:val(1)),
    ?assertEqual(
        timeout,
        doc_receive_loop(Keys, Pids, fun foo:spawn/1, MaxJobs, fun foo:callback/2, accumulator1)
    ),
    ?assertNot(meck:called(foo, callback, '_')).

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
