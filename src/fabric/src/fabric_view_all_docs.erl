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
-export([open_doc/4]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

go(Db, Options, #mrargs{keys = undefined} = QueryArgs, Callback, Acc) ->
    {CoordArgs, WorkerArgs} = fabric_view:fix_skip_and_limit(QueryArgs),
    DbName = fabric:dbname(Db),
    {Shards, RingOpts} = shards(Db, QueryArgs),
    Workers0 = fabric_util:submit_jobs(
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
            {timeout, NewState} ->
                DefunctWorkers = fabric_util:remove_done_workers(
                    NewState#stream_acc.workers, waiting
                ),
                fabric_util:log_timeout(
                    DefunctWorkers,
                    "all_docs"
                ),
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
        spawn_monitor(?MODULE, open_doc, [DbName, Options ++ DocOptions1, Key, IncludeDocs])
    end,
    MaxJobs = all_docs_concurrency(),
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
    %% namespace can be _set_ to `undefined`, so we want simulate enum here
    Namespace =
        case couch_util:get_value(namespace, Extra) of
            <<"_all_docs">> -> <<"_all_docs">>;
            <<"_design">> -> <<"_design">>;
            <<"_local">> -> <<"_local">>;
            _ -> <<"_all_docs">>
        end,
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
                Keys3, queue:new(), SpawnFun, MaxJobs, Callback, Acc1
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
                    _ -> erlang:min(Total, Offset + State#collector.skip)
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
handle_message(#view_row{} = Row, {Worker, From}, State) ->
    #collector{query_args = Args, counters = Counters0, rows = Rows0} = State,
    Dir = Args#mrargs.direction,
    Rows = merge_row(Dir, Row#view_row{worker = {Worker, From}}, Rows0),
    Counters1 = fabric_dict:update_counter(Worker, 1, Counters0),
    State1 = State#collector{rows = Rows, counters = Counters1},
    fabric_view:maybe_send_row(State1);
handle_message(complete, Worker, State) ->
    Counters = fabric_dict:update_counter(Worker, 1, State#collector.counters),
    fabric_view:maybe_send_row(State#collector{counters = Counters});
handle_message({execution_stats, _} = Msg, {_, From}, St) ->
    #collector{callback = Callback, user_acc = AccIn} = St,
    {Go, Acc} = Callback(Msg, AccIn),
    rexi:stream_ack(From),
    {Go, St#collector{user_acc = Acc}}.

merge_row(fwd, Row, Rows) ->
    lists:keymerge(#view_row.id, [Row], Rows);
merge_row(rev, Row, Rows) ->
    lists:rkeymerge(#view_row.id, [Row], Rows).

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
            receive
                {'DOWN', Ref, process, Pid, Row} ->
                    case Row of
                        #view_row{} ->
                            case Callback(fabric_view:transform_row(Row), AccIn) of
                                {ok, Acc} ->
                                    doc_receive_loop(
                                        Keys, RestPids, SpawnFun, MaxJobs, Callback, Acc
                                    );
                                {stop, Acc} ->
                                    cancel_read_pids(RestPids),
                                    {ok, Acc}
                            end;
                        Error ->
                            cancel_read_pids(RestPids),
                            Callback({error, Error}, AccIn)
                    end
            after Timeout ->
                timeout
            end
    end.

open_doc(DbName, Options, Id, IncludeDocs) ->
    try open_doc_int(DbName, Options, Id, IncludeDocs) of
        #view_row{} = Row ->
            exit(Row)
    catch ?STACKTRACE(Type, Reason, Stack)
        couch_log:error("_all_docs open error: ~s ~s :: ~w ~w", [
                DbName, Id, {Type, Reason}, Stack]),
        exit({Id, Reason})
    end.

open_doc_int(DbName, Options, Id, IncludeDocs) ->
    Row =
        case fabric:open_doc(DbName, Id, [deleted | Options]) of
            {not_found, missing} ->
                Doc = undefined,
                #view_row{key = Id};
            {ok, #doc{deleted = true, revs = Revs}} ->
                Doc = null,
                {RevPos, [RevId | _]} = Revs,
                Value = {[{rev, couch_doc:rev_to_str({RevPos, RevId})}, {deleted, true}]},
                #view_row{key = Id, id = Id, value = Value};
            {ok, #doc{revs = Revs} = Doc0} ->
                Doc = couch_doc:to_json_obj(Doc0, Options),
                {RevPos, [RevId | _]} = Revs,
                Value = {[{rev, couch_doc:rev_to_str({RevPos, RevId})}]},
                #view_row{key = Id, id = Id, value = Value}
        end,
    if
        IncludeDocs -> Row#view_row{doc = Doc};
        true -> Row
    end.

cancel_read_pids(Pids) ->
    case queue:out(Pids) of
        {{value, {Pid, Ref}}, RestPids} ->
            exit(Pid, kill),
            erlang:demonitor(Ref, [flush]),
            cancel_read_pids(RestPids);
        {empty, _} ->
            ok
    end.
