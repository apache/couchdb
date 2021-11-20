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
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

docs(DbName, Options, QueryArgs, Callback, Acc) ->
    Shards = mem3:shards(DbName),
    Workers0 = fabric_util:submit_jobs(
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
            {timeout, NewState} ->
                DefunctWorkers = fabric_util:remove_done_workers(
                    NewState#stream_acc.workers, waiting
                ),
                fabric_util:log_timeout(
                    DefunctWorkers,
                    "replicator docs"
                ),
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
            FinalOffset = erlang:min(Total, Offset + State#collector.skip),
            Meta = [{total, Total}, {offset, FinalOffset}],
            {Go, Acc} = Callback({meta, Meta}, AccIn),
            {Go, State#collector{
                counters = fabric_dict:decrement_all(Counters1),
                total_rows = Total,
                offset = FinalOffset,
                user_acc = Acc
            }}
    end;
handle_message(#view_row{id = Id, doc = Doc} = Row0, {Worker, From}, State) ->
    #collector{query_args = Args, counters = Counters0, rows = Rows0} = State,
    case maybe_fetch_and_filter_doc(Id, Doc, State) of
        {[_ | _]} = NewDoc ->
            Row = Row0#view_row{doc = NewDoc},
            Dir = Args#mrargs.direction,
            Rows = merge_row(Dir, Row#view_row{worker = {Worker, From}}, Rows0),
            Counters1 = fabric_dict:update_counter(Worker, 1, Counters0),
            State1 = State#collector{rows = Rows, counters = Counters1},
            fabric_view:maybe_send_row(State1);
        skip ->
            rexi:stream_ack(From),
            {ok, State}
    end;
handle_message(complete, Worker, State) ->
    Counters = fabric_dict:update_counter(Worker, 1, State#collector.counters),
    fabric_view:maybe_send_row(State#collector{counters = Counters}).

merge_row(fwd, Row, Rows) ->
    lists:keymerge(#view_row.id, [Row], Rows);
merge_row(rev, Row, Rows) ->
    lists:rkeymerge(#view_row.id, [Row], Rows).

maybe_fetch_and_filter_doc(Id, undecided, State) ->
    #collector{db_name = DbName, query_args = #mrargs{extra = Extra}} = State,
    FilterStates = proplists:get_value(filter_states, Extra),
    case couch_replicator:active_doc(DbName, Id) of
        {ok, {Props} = DocInfo} ->
            DocState = couch_util:get_value(state, Props),
            couch_replicator_utils:filter_state(DocState, FilterStates, DocInfo);
        {error, not_found} ->
            % could have been deleted
            skip
    end;
maybe_fetch_and_filter_doc(_Id, Doc, _State) ->
    Doc.
