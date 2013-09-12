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

-export([go/6]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

go(DbName, GroupId, View, Args, Callback, Acc0) when is_binary(GroupId) ->
    {ok, DDoc} = fabric:open_doc(DbName, <<"_design/", GroupId/binary>>, []),
    go(DbName, DDoc, View, Args, Callback, Acc0);

go(DbName, DDoc, View, Args, Callback, Acc) ->
    Shards = fabric_view:get_shards(DbName, Args),
    Repls = fabric_view:get_shard_replacements(DbName, Shards),
    RPCArgs = [DDoc, View, Args],
    StartFun = fun(Shard) ->
        hd(fabric_util:submit_jobs([Shard], fabric_rpc, map_view, RPCArgs))
    end,
    Workers0 = fabric_util:submit_jobs(Shards, fabric_rpc, map_view, RPCArgs),
    RexiMon = fabric_util:create_monitors(Workers0),
    try
        case fabric_util:stream_start(Workers0, #shard.ref, StartFun, Repls) of
            {ok, Workers} ->
                try
                    go(DbName, Workers, Args, Callback, Acc)
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

go(DbName, Workers, Args, Callback, Acc0) ->
    #mrargs{limit = Limit, skip = Skip, keys = Keys} = Args,
    State = #collector{
        db_name=DbName,
        query_args = Args,
        callback = Callback,
        counters = fabric_dict:init(Workers, 0),
        skip = Skip,
        limit = Limit,
        keys = fabric_view:keydict(Keys),
        sorted = Args#mrargs.sorted,
        user_acc = Acc0
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
        FinalOffset = erlang:min(Total, Offset+State#collector.skip),
        Meta = [{total, Total}, {offset, FinalOffset}],
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
        query_args = #mrargs{direction=Dir},
        counters = Counters0,
        rows = Rows0,
        keys = KeyDict
    } = State,
    Rows = merge_row(Dir, KeyDict, Row#view_row{worker={Worker, From}}, Rows0),
    Counters1 = fabric_dict:update_counter(Worker, 1, Counters0),
    State1 = State#collector{rows=Rows, counters=Counters1},
    fabric_view:maybe_send_row(State1);

handle_message(complete, Worker, State) ->
    Counters = fabric_dict:update_counter(Worker, 1, State#collector.counters),
    fabric_view:maybe_send_row(State#collector{counters = Counters}).

merge_row(fwd, undefined, Row, Rows) ->
    lists:merge(fun(#view_row{key=KeyA, id=IdA}, #view_row{key=KeyB, id=IdB}) ->
        couch_ejson_compare:less_json_ids({KeyA, IdA}, {KeyB, IdB})
    end, [Row], Rows);
merge_row(rev, undefined, Row, Rows) ->
    lists:merge(fun(#view_row{key=KeyA, id=IdA}, #view_row{key=KeyB, id=IdB}) ->
        couch_ejson_compare:less_json_ids({KeyB, IdB}, {KeyA, IdA})
    end, [Row], Rows);
merge_row(_, KeyDict, Row, Rows) ->
    lists:merge(fun(#view_row{key=A, id=IdA}, #view_row{key=B, id=IdB}) ->
        if A =:= B -> IdA < IdB; true ->
            dict:fetch(A, KeyDict) < dict:fetch(B, KeyDict)
        end
    end, [Row], Rows).
