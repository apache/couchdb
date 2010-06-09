-module(fabric_view_map).

-export([go/6]).

-include("fabric.hrl").

go(DbName, GroupId, View, Args, Callback, Acc0) ->
    {ok, DDoc} = fabric:open_doc(DbName, <<"_design/", GroupId/binary>>, []),
    Workers = lists:map(fun(#shard{name=Name, node=Node} = Shard) ->
        Ref = rexi:cast(Node, {fabric_rpc, map_view, [Name, DDoc, View, Args]}),
        Shard#shard{ref = Ref}
    end, partitions:all_parts(DbName)),
    BufferSize = couch_config:get("fabric", "map_buffer_size", "2"),
    #view_query_args{limit = Limit, skip = Skip, keys = Keys} = Args,
    State = #collector{
        query_args = Args,
        callback = Callback,
        buffer_size = list_to_integer(BufferSize),
        counters = fabric_dict:init(Workers, 0),
        skip = Skip,
        limit = Limit,
        stop_fun = stop_fun(Args),
        keys = fabric_view:keydict(Keys),
        sorted = Args#view_query_args.sorted,
        user_acc = Acc0
    },
    try fabric_util:receive_loop(Workers, #shard.ref, fun handle_message/3,
        State, infinity, 5000) of
    {ok, NewState} ->
        {ok, NewState#collector.user_acc};
    Error ->
        Error
    after
        fabric_util:cleanup(Workers)
    end.

handle_message({rexi_DOWN, _, _, _}, nil, State) ->
    % TODO see if progress can be made here, possibly by removing all shards
    % from that node and checking is_progress_possible
    {ok, State};

handle_message({rexi_EXIT, Reason}, Worker, State) ->
    ?LOG_ERROR("~p rexi_EXIT ~p", [?MODULE, Reason]),
    #collector{callback=Callback, counters=Counters0, user_acc=Acc} = State,
    Counters = fabric_dict:erase(Worker, Counters0),
    case fabric_view:is_progress_possible(Counters) of
    true ->
        {ok, State#collector{counters = Counters}};
    false ->
        Callback({error, dead_shards}, Acc),
        {error, dead_shards}
    end;

handle_message({total_and_offset, Tot, Off}, {Worker, From}, State) ->
    #collector{
        callback = Callback,
        counters = Counters0,
        total_rows = Total0,
        offset = Offset0,
        user_acc = AccIn
    } = State,
    case fabric_dict:lookup_element(Worker, Counters0) of
    undefined ->
        % this worker lost the race with other partition copies, terminate
        gen_server:reply(From, stop),
        {ok, State};
    0 ->
        gen_server:reply(From, ok),
        Counters1 = fabric_dict:update_counter(Worker, 1, Counters0),
        Counters2 = fabric_view:remove_overlapping_shards(Worker, Counters1),
        Total = Total0 + Tot,
        Offset = Offset0 + Off,
        case fabric_dict:any(0, Counters2) of
        true ->
            {ok, State#collector{
                counters = Counters2,
                total_rows = Total,
                offset = Offset
            }};
        false ->
            FinalOffset = erlang:min(Total, Offset+State#collector.skip),
            {Go, Acc} = Callback({total_and_offset, Total, FinalOffset}, AccIn),
            {Go, State#collector{
                counters = fabric_dict:decrement_all(Counters2),
                total_rows = Total,
                offset = FinalOffset,
                user_acc = Acc
            }}
        end
    end;

handle_message(#view_row{}, {_, _}, #collector{limit=0} = State) ->
    #collector{callback=Callback} = State,
    {_, Acc} = Callback(complete, State#collector.user_acc),
    {stop, State#collector{user_acc=Acc}};

handle_message(#view_row{} = Row, {_,From}, #collector{sorted=false} = St) ->
    #collector{callback=Callback, user_acc=AccIn, limit=Limit} = St,
    {Go, Acc} = Callback(fabric_view:transform_row(Row), AccIn),
    gen_server:reply(From, ok),
    {Go, St#collector{user_acc=Acc, limit=Limit-1}};
    
handle_message(#view_row{} = Row, {Worker, From}, State) ->
    #collector{
        query_args = #view_query_args{direction=Dir},
        counters = Counters0,
        rows = Rows0,
        keys = KeyDict
    } = State,
    Rows = merge_row(Dir, KeyDict, Row#view_row{worker=Worker}, Rows0),
    Counters1 = fabric_dict:update_counter(Worker, 1, Counters0),
    State1 = State#collector{rows=Rows, counters=Counters1},
    State2 = fabric_view:maybe_pause_worker(Worker, From, State1),
    fabric_view:maybe_send_row(State2);

handle_message(complete, Worker, State) ->
    Counters = fabric_dict:update_counter(Worker, 1, State#collector.counters),
    fabric_view:maybe_send_row(State#collector{counters = Counters}).

stop_fun(#view_query_args{} = QueryArgs) ->
    #view_query_args{
        direction = Dir,
        inclusive_end = Inclusive,
        end_key = EndKey,
        end_docid = EndDocId
    } = QueryArgs,
    stop_fun(Dir, Inclusive, EndKey, EndDocId).

stop_fun(fwd, true, EndKey, EndDocId) ->
    fun(#view_row{key=Key, id=Id}) ->
        couch_view:less_json([EndKey, EndDocId], [Key, Id])
    end;
stop_fun(fwd, false, EndKey, EndDocId) ->
    fun(#view_row{key=K}) when K==EndKey -> true; (#view_row{key=Key, id=Id}) ->
        couch_view:less_json([EndKey, EndDocId], [Key, Id])
    end;
stop_fun(rev, true, EndKey, EndDocId) ->
    fun(#view_row{key=Key, id=Id}) ->
        couch_view:less_json([Key, Id], [EndKey, EndDocId])
    end;
stop_fun(rev, false, EndKey, EndDocId) ->
    fun(#view_row{key=K}) when K==EndKey -> true; (#view_row{key=Key, id=Id}) ->
        couch_view:less_json([Key, Id], [EndKey, EndDocId])
    end.

merge_row(fwd, undefined, Row, Rows) ->
    lists:merge(fun(#view_row{key=KeyA, id=IdA}, #view_row{key=KeyB, id=IdB}) ->
        couch_view:less_json([KeyA, IdA], [KeyB, IdB])
    end, [Row], Rows);
merge_row(rev, undefined, Row, Rows) ->
    lists:merge(fun(#view_row{key=KeyA, id=IdA}, #view_row{key=KeyB, id=IdB}) ->
        couch_view:less_json([KeyB, IdB], [KeyA, IdA])
    end, [Row], Rows);
merge_row(_, KeyDict, Row, Rows) ->
    lists:merge(fun(#view_row{key=A, id=IdA}, #view_row{key=B, id=IdB}) ->
        if A =:= B -> IdA < IdB; true ->
            dict:fetch(A, KeyDict) < dict:fetch(B, KeyDict)
        end
    end, [Row], Rows).

