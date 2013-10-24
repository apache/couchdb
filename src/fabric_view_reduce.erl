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

go(DbName, DDoc, VName, Args, Callback, Acc, {red, {_, Lang, _}, _}=VInfo) ->
    RedSrc = couch_mrview_util:extract_view_reduce(VInfo),
    RPCArgs = [DDoc, VName, Args],
    Shards = fabric_view:get_shards(DbName, Args),
    Repls = fabric_view:get_shard_replacements(DbName, Shards),
    StartFun = fun(Shard) ->
        hd(fabric_util:submit_jobs([Shard], fabric_rpc, reduce_view, RPCArgs))
    end,
    Workers0 = fabric_util:submit_jobs(Shards,fabric_rpc,reduce_view,RPCArgs),
    RexiMon = fabric_util:create_monitors(Workers0),
    try
        case fabric_util:stream_start(Workers0, #shard.ref, StartFun, Repls) of
            {ok, Workers} ->
                try
                    go2(DbName, Workers, Lang, RedSrc, Args, Callback, Acc)
                after
                    fabric_util:cleanup(Workers)
                end;
            {timeout, _} ->
                Callback({error, timeout}, Acc);
            {error, Error} ->
                Callback({error, Error}, Acc)
        end
    after
        rexi_monitor:stop(RexiMon)
    end.

go2(DbName, Workers, Lang, RedSrc, Args, Callback, Acc0) ->
    #mrargs{limit = Limit, skip = Skip, keys = Keys} = Args,
    OsProc = case os_proc_needed(RedSrc) of
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
        rows = dict:new(),
        user_acc = Acc0
    },
    try rexi_utils:recv(Workers, #shard.ref, fun handle_message/3,
        State, infinity, 1000 * 60 * 60) of
    {ok, NewState} ->
        {ok, NewState#collector.user_acc};
    {timeout, NewState} ->
        Callback({error, timeout}, NewState#collector.user_acc);
    {error, Resp} ->
        {ok, Resp}
    after
        if OsProc == nil -> ok; true ->
            catch couch_query_servers:ret_os_process(OsProc)
        end
    end.

handle_message({rexi_DOWN, _, {_, NodeRef}, _}, _, State) ->
    fabric_view:check_down_shards(State, NodeRef);

handle_message({rexi_EXIT, Reason}, Worker, State) ->
    fabric_view:handle_worker_exit(State, Worker, Reason);

%% HACK: this just sends meta once. Instead we should move the counter logic
%% from the #view_row handle_message below into this function and and pass the
%% meta call through maybe_send_row. This will also be more efficient doing it
%% here as it's one less worker round trip reply.
%% Prior to switching to couch_mrview, the fabric_view_reduce implementation
%% did not get a total_and_offset call, whereas now we do. We now use this
%% message as a clean way to indicate to couch_mrview_http:view_cb that the
%% reduce response is starting.
handle_message({meta, Meta}, {_Worker, From}, State) ->
    gen_server:reply(From, ok),
    #collector{
        callback = Callback,
        user_acc = AccIn
    } = State,

    {Go, Acc} = case get(meta_sent) of
        undefined ->
            put(meta_sent, true),
            Callback({meta, Meta}, AccIn);
        _ ->
            {ok, AccIn}
    end,
    {Go, State#collector{user_acc = Acc}};

handle_message(#view_row{key=Key} = Row, {Worker, From}, State) ->
    #collector{counters = Counters0, rows = Rows0} = State,
    true = fabric_dict:is_key(Worker, Counters0),
    Rows = dict:append(Key, Row#view_row{worker={Worker, From}}, Rows0),
    C1 = fabric_dict:update_counter(Worker, 1, Counters0),
    State1 = State#collector{rows=Rows, counters=C1},
    fabric_view:maybe_send_row(State1);

handle_message(complete, Worker, #collector{counters = Counters0} = State) ->
    true = fabric_dict:is_key(Worker, Counters0),
    C1 = fabric_dict:update_counter(Worker, 1, Counters0),
    fabric_view:maybe_send_row(State#collector{counters = C1}).

os_proc_needed(<<"_", _/binary>>) -> false;
os_proc_needed(_) -> true.

