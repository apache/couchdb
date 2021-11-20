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
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

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
        hd(fabric_util:submit_jobs([Shard], fabric_rpc, reduce_view, RPCArgs))
    end,
    Workers0 = fabric_util:submit_jobs(Shards, fabric_rpc, reduce_view, RPCArgs),
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
            {ok, Workers} ->
                try
                    go2(DbName, Workers, VInfo, CoordArgs, Callback, Acc)
                after
                    fabric_streams:cleanup(Workers)
                end;
            {timeout, NewState} ->
                DefunctWorkers = fabric_util:remove_done_workers(
                    NewState#stream_acc.workers,
                    waiting
                ),
                fabric_util:log_timeout(
                    DefunctWorkers,
                    "reduce_view"
                ),
                Callback({error, timeout}, Acc);
            {error, Error} ->
                Callback({error, Error}, Acc)
        end
    after
        rexi_monitor:stop(RexiMon)
    end.

go2(DbName, Workers, {red, {_, Lang, View}, _} = VInfo, Args, Callback, Acc0) ->
    #mrargs{limit = Limit, skip = Skip, keys = Keys, update_seq = UpdateSeq} = Args,
    RedSrc = couch_mrview_util:extract_view_reduce(VInfo),
    OsProc =
        case os_proc_needed(RedSrc) of
            true -> couch_query_servers:get_os_process(Lang);
            _ -> nil
        end,
    State = #collector{
        db_name = DbName,
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
            1000 * 60 * 60
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
handle_message(#view_row{key = Key} = Row, {Worker, From}, State) ->
    #collector{counters = Counters0, rows = Rows0} = State,
    true = fabric_dict:is_key(Worker, Counters0),
    Rows = dict:append(Key, Row#view_row{worker = {Worker, From}}, Rows0),
    C1 = fabric_dict:update_counter(Worker, 1, Counters0),
    State1 = State#collector{rows = Rows, counters = C1},
    fabric_view:maybe_send_row(State1);
handle_message(complete, Worker, #collector{counters = Counters0} = State) ->
    true = fabric_dict:is_key(Worker, Counters0),
    C1 = fabric_dict:update_counter(Worker, 1, Counters0),
    fabric_view:maybe_send_row(State#collector{counters = C1});
handle_message(ddoc_updated, _Worker, State) ->
    {stop, State}.

os_proc_needed(<<"_", _/binary>>) -> false;
os_proc_needed(_) -> true.
