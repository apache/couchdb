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

-export([go/7]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

go(DbName, GroupId, View, Args, Callback, Acc, VInfo) when is_binary(GroupId) ->
    {ok, DDoc} = fabric:open_doc(DbName, <<"_design/", GroupId/binary>>, []),
    go(DbName, DDoc, View, Args, Callback, Acc, VInfo);

go(DbName, DDoc, View, Args, Callback, Acc, VInfo) ->
    Shards = fabric_view:get_shards(DbName, Args),
    DocIdAndRev = fabric_util:doc_id_and_rev(DDoc),
    fabric_view:maybe_update_others(DbName, DocIdAndRev, Shards, View, Args),
    Repls = fabric_view:get_shard_replacements(DbName, Shards),
    RPCArgs = [DocIdAndRev, View, Args],
    StartFun = fun(Shard) ->
        hd(fabric_util:submit_jobs([Shard], fabric_rpc, map_view, RPCArgs))
    end,
    Workers0 = fabric_util:submit_jobs(Shards, fabric_rpc, map_view, RPCArgs),
    RexiMon = fabric_util:create_monitors(Workers0),
    try
        case fabric_util:stream_start(Workers0, #shard.ref, StartFun, Repls) of
            {ok, Workers} ->
                try
                    go(DbName, Workers, VInfo, Args, Callback, Acc)
                after
                    fabric_util:cleanup(Workers)
                end;
            {timeout, NewState} ->
                DefunctWorkers = fabric_util:remove_done_workers(
                    NewState#stream_acc.workers,
                    waiting
                ),
                fabric_util:log_timeout(
                    DefunctWorkers,
                    "map_view"
                ),
                Callback({error, timeout}, Acc);
            {error, Error} ->
                Callback({error, Error}, Acc)
        end
    after
        rexi_monitor:stop(RexiMon)
    end.

go(DbName, Workers, {map, View, _}, Args, Callback, Acc0) ->
    #mrargs{limit = Limit, skip = Skip, keys = Keys, update_seq=UpdateSeq} = Args,
    Collation = couch_util:get_value(<<"collation">>, View#mrview.options),
    State = #collector{
        db_name=DbName,
        query_args = Args,
        callback = Callback,
        counters = fabric_dict:init(Workers, 0),
        skip = Skip,
        limit = Limit,
        keys = fabric_view:keydict(Keys),
        sorted = Args#mrargs.sorted,
        collation = Collation,
        user_acc = Acc0,
        update_seq = case UpdateSeq of true -> []; false -> nil end
    },
    case rexi_utils:recv(Workers, #shard.ref, fun handle_message/3,
        State, infinity, 1000 * 60 * 60) of
    {ok, NewState} ->
        {ok, NewState#collector.user_acc};
    {timeout, NewState} ->
        Callback({error, timeout}, NewState#collector.user_acc);
    {error, Resp} ->
        {ok, Resp}
    end.

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
    UpdateSeq = case UpdateSeq0 of
        nil -> nil;
        _   -> [{Worker, Seq} | UpdateSeq0]
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
        FinalOffset = erlang:min(Total, Offset+State#collector.skip),
        Meta = [{total, Total}, {offset, FinalOffset}] ++
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

handle_message(#view_row{}, {_, _}, #collector{limit=0} = State) ->
    #collector{callback=Callback} = State,
    {_, Acc} = Callback(complete, State#collector.user_acc),
    {stop, State#collector{user_acc=Acc}};

handle_message(#view_row{} = Row, {_,From}, #collector{sorted=false} = St) ->
    #collector{callback=Callback, user_acc=AccIn, limit=Limit} = St,
    {Go, Acc} = Callback(fabric_view:transform_row(Row), AccIn),
    rexi:stream_ack(From),
    {Go, St#collector{user_acc=Acc, limit=Limit-1}};

handle_message(#view_row{} = Row, {Worker, From}, State) ->
    #collector{
        query_args = #mrargs{direction = Dir},
        counters = Counters0,
        rows = Rows0,
        keys = KeyDict0,
        collation = Collation
    } = State,
    {Rows, KeyDict} = merge_row(
        Dir,
        Collation,
        KeyDict0,
        Row#view_row{worker={Worker, From}},
        Rows0
    ),
    Counters1 = fabric_dict:update_counter(Worker, 1, Counters0),
    State1 = State#collector{rows=Rows, counters=Counters1, keys=KeyDict},
    fabric_view:maybe_send_row(State1);

handle_message(complete, Worker, State) ->
    Counters = fabric_dict:update_counter(Worker, 1, State#collector.counters),
    fabric_view:maybe_send_row(State#collector{counters = Counters}).

merge_row(Dir, Collation, undefined, Row, Rows0) ->
    Rows1 = lists:merge(
        fun(#view_row{key=KeyA, id=IdA}, #view_row{key=KeyB, id=IdB}) ->
            compare(Dir, Collation, {KeyA, IdA}, {KeyB, IdB})
        end,
        [Row],
        Rows0
    ),
    {Rows1, undefined};
merge_row(Dir, Collation, KeyDict0, Row, Rows0) ->
    CmpFun = case Collation of
        <<"raw">> ->
            fun (A, A) -> 0;
                (A, B) -> case A < B of
                    true -> -1;
                    false -> 1
                end
            end;
        _ ->
            fun couch_ejson_compare:less/2
    end,
    case maybe_update_keydict(Row#view_row.key, KeyDict0, CmpFun) of
        undefined ->
            {Rows0, KeyDict0};
        KeyDict1 ->
            Rows1 = lists:merge(
                fun(#view_row{key=A, id=IdA}, #view_row{key=B, id=IdB}) ->
                    case {Dir, CmpFun(A, B)} of
                        {fwd, 0} ->
                            IdA < IdB;
                        {rev, 0} ->
                            IdB < IdA;
                        {fwd, _} ->
                            dict:fetch(A, KeyDict1) < dict:fetch(B, KeyDict1);
                        {rev, _} ->
                            dict:fetch(B, KeyDict1) < dict:fetch(A, KeyDict1)
                    end
                end,
                [Row],
                Rows0
            ),
            {Rows1, KeyDict1}
    end.

compare(_, _, A, A) -> true;
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
key_index(KeyA, [{KeyB, Value}|KVs], CmpFun) ->
    case CmpFun(KeyA, KeyB) of
        0 -> Value;
        _ -> key_index(KeyA, KVs, CmpFun)
    end.
