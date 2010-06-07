-module(fabric_view_all_docs).

-export([go/4]).
-export([open_doc/3]). % exported for spawn

-include("fabric.hrl").

go(DbName, #view_query_args{keys=nil} = QueryArgs, Callback, Acc0) ->
    Workers = lists:map(fun(#shard{name=Name, node=Node} = Shard) ->
        Ref = rexi:cast(Node, {fabric_rpc, all_docs, [Name, QueryArgs]}),
        Shard#shard{ref = Ref}
    end, partitions:all_parts(DbName)),
    BufferSize = couch_config:get("fabric", "map_buffer_size", "2"),
    #view_query_args{limit = Limit, skip = Skip} = QueryArgs,
    State = #collector{
        query_args = QueryArgs,
        callback = Callback,
        buffer_size = list_to_integer(BufferSize),
        counters = init_counters(Workers),
        skip = Skip,
        limit = Limit,
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
    end;

go(DbName, QueryArgs, Callback, Acc0) ->
    #view_query_args{
        direction = Dir,
        include_docs = IncludeDocs,
        limit = Limit0,
        skip = Skip0,
        keys = Keys
    } = QueryArgs,
    {_, Ref0} = spawn_monitor(fun() -> exit(fabric:get_doc_count(DbName)) end),
    Monitors0 = [spawn_monitor(?MODULE, open_doc, [DbName, Id, IncludeDocs]) ||
        Id <- Keys],
    Monitors = if Dir=:=fwd -> Monitors0; true -> lists:reverse(Monitors0) end,
    receive {'DOWN', Ref0, _, _, {ok, TotalRows}} ->
        {ok, Acc1} = Callback({total_and_offset, TotalRows, 0}, Acc0),
        {ok, Acc2} = doc_receive_loop(Monitors, Skip0, Limit0, Callback, Acc1),
        Callback(complete, Acc2)
    after 10000 ->
        Callback(timeout, Acc0)
    end.

handle_message({rexi_DOWN, _, _, _}, nil, State) ->
    % TODO see if progress can be made here, possibly by removing all shards
    % from that node and checking is_progress_possible
    {ok, State};

handle_message({rexi_EXIT, _}, Worker, State) ->
    #collector{callback=Callback, counters=Counters0, user_acc=Acc} = State,
    Counters = remove(Worker, Counters0),
    case is_progress_possible(Counters) of
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
    case lookup_element(Worker, Counters0) of
    undefined ->
        % this worker lost the race with other partition copies, terminate
        gen_server:reply(From, stop),
        {ok, State};
    0 ->
        gen_server:reply(From, ok),
        Counters1 = update_counter(Worker, 1, Counters0),
        Counters2 = remove_overlapping_shards(Worker, Counters1),
        Total = Total0 + Tot,
        Offset = Offset0 + Off,
        case waiting_on_shards(Counters2) of
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
                counters = decrement_all_counters(Counters2),
                total_rows = Total,
                offset = FinalOffset,
                user_acc = Acc
            }}
        end
    end;

handle_message(#view_row{} = Row, {Worker, From}, State) ->
    #collector{query_args = Args, counters = Counters0, rows = Rows0} = State,
    Dir = Args#view_query_args.direction,
    Rows = merge_row(Dir, Row#view_row{worker=Worker}, Rows0),
    Counters1 = update_counter(Worker, 1, Counters0),
    State1 = State#collector{rows=Rows, counters=Counters1},
    State2 = maybe_pause_worker(Worker, From, State1),
    maybe_send_row(State2);

handle_message(complete, Worker, State) ->
    Counters = update_counter(Worker, 1, State#collector.counters),
    maybe_send_row(State#collector{counters = Counters}).


maybe_pause_worker(Worker, From, State) ->
    #collector{buffer_size = BufferSize, counters = Counters} = State,
    case lookup_element(Worker, Counters) of
    BufferSize ->
        State#collector{blocked = [{Worker,From} | State#collector.blocked]};
    _Count ->
        gen_server:reply(From, ok),
        State
    end.

maybe_resume_worker(Worker, State) ->
    #collector{buffer_size = Buffer, counters = C, blocked = B} = State,
    case lookup_element(Worker, C) of
    Count when Count < Buffer/2 ->
        case couch_util:get_value(Worker, B) of
        undefined ->
            State;
        From ->
            gen_server:reply(From, ok),
            State#collector{blocked = lists:keydelete(Worker, 1, B)}
        end;
    _Other ->
        State
    end.

maybe_send_row(#collector{limit=0} = State) ->
    #collector{user_acc=AccIn, callback=Callback} = State,
    {_, Acc} = Callback(complete, AccIn),
    {stop, State#collector{user_acc=Acc}};
maybe_send_row(State) ->
    #collector{
        callback = Callback,
        counters = Counters,
        skip = Skip,
        limit = Limit,
        user_acc = AccIn
    } = State,
    case waiting_on_shards(Counters) of
    true ->
        {ok, State};
    false ->
        case get_next_row(State) of
        complete ->
            {_, Acc} = Callback(complete, AccIn),
            {stop, State#collector{user_acc=Acc}};
        {_, NewState} when Skip > 0 ->
            maybe_send_row(NewState#collector{skip=Skip-1, limit=Limit-1});
        {Row, NewState} ->
            case Callback(transform_row(Row), AccIn) of
            {stop, Acc} ->
                {stop, NewState#collector{user_acc=Acc, limit=Limit-1}};
            {ok, Acc} ->
                maybe_send_row(NewState#collector{user_acc=Acc, limit=Limit-1})
            end
        end
    end.

get_next_row(#collector{rows = []}) ->
    complete;
get_next_row(State) ->
    #collector{query_args=Args, rows=[Row|Rest], counters=Counters0} = State,
    Worker = Row#view_row.worker,
    Counters1 = update_counter(Worker, -1, Counters0),
    NewState = maybe_resume_worker(Worker, State#collector{counters=Counters1}),
    case stop(Args, Row) of
    true ->
        complete;
    false ->
        {Row, NewState#collector{rows = Rest}}
    end.

stop(#view_query_args{direction=fwd, end_key=EndKey}, #view_row{id=Id}) ->
    couch_db_updater:less_docid(EndKey, Id);
stop(#view_query_args{direction=rev, end_key=EndKey}, #view_row{id=Id}) ->
    couch_db_updater:less_docid(Id, EndKey).

transform_row(#view_row{key=Key, id=undefined}) ->
    {row, {[{key,Key}, {error,not_found}]}};
transform_row(#view_row{key=Key, id=Id, value=Value, doc=undefined}) ->
    {row, {[{key,Key}, {id,Id}, {value,Value}]}};
transform_row(#view_row{key=Key, id=Id, value=Value, doc={error,Reason}}) ->
    {row, {[{key,Key}, {id,Id}, {value,Value}, {error,Reason}]}};
transform_row(#view_row{key=Key, id=Id, value=Value, doc=Doc}) ->
    {row, {[{key,Key}, {id,Id}, {value,Value}, {doc,Doc}]}}.

merge_row(fwd, Row, Rows) ->
    lists:keymerge(#view_row.id, [Row], Rows);
merge_row(rev, Row, Rows) ->
    lists:rkeymerge(#view_row.id, [Row], Rows).

remove_overlapping_shards(#shard{range=[A,B]} = Shard0, Shards) ->
    filter(fun(#shard{range=[X,Y]} = Shard, _Value) ->
        if Shard =:= Shard0 ->
            % we can't remove ourselves
            true;
        A < B, X >= A, X < B ->
            % lower bound is inside our range
            false;
        A < B, Y > A, Y =< B ->
            % upper bound is inside our range
            false;
        B < A, X >= A orelse B < A, X < B ->
            % target shard wraps the key range, lower bound is inside
            false;
        B < A, Y > A orelse B < A, Y =< B ->
            % target shard wraps the key range, upper bound is inside
            false;
        true ->
            true
        end
    end, Shards).

%% @doc looks for a fully covered keyrange in the list of counters
-spec is_progress_possible([{#shard{}, non_neg_integer()}]) -> boolean().
is_progress_possible(Counters) ->
    Ranges = fold(fun(#shard{range=[X,Y]}, _, A) -> [{X,Y}|A] end, [], Counters),
    [First | Rest] = lists:ukeysort(1, Ranges),
    {Head, Tail} = lists:foldl(fun
    (_, {Head, Tail}) when Head =:= Tail ->
        % this is the success condition, we can fast-forward
        {Head, Tail};
    (_, {foo, bar}) ->
        % we've already declared failure
        {foo, bar};
    ({X,_}, {Head, Tail}) when Head < Tail, X > Tail ->
        % gap in the keyrange, we're dead
        {foo, bar};
    ({X,Y}, {Head, Tail}) when Head < Tail, X < Y ->
        % the normal condition, adding to the tail
        {Head, erlang:max(Tail, Y)};
    ({X,Y}, {Head, Tail}) when Head < Tail, X > Y, Y >= Head ->
        % we've wrapped all the way around, trigger success condition
        {Head, Head};
    ({X,Y}, {Head, Tail}) when Head < Tail, X > Y ->
        % this wraps the keyspace, but there's still a gap.  We're dead
        % TODO technically, another shard could be a superset of this one, and
        % we could still be alive.  Pretty unlikely though, and impossible if
        % we don't allow shards to wrap around the boundary
        {foo, bar}
    end, First, Rest),
    Head =:= Tail.

doc_receive_loop([], _, _, _, Acc) ->
    {ok, Acc};
doc_receive_loop(_, _, 0, _, Acc) ->
    {ok, Acc};
doc_receive_loop([{Pid,Ref}|Rest], Skip, Limit, Callback, Acc) when Skip > 0 ->
    receive {'DOWN', Ref, process, Pid, #view_row{}} ->
        doc_receive_loop(Rest, Skip-1, Limit-1, Callback, Acc)
    after 10000 ->
        timeout
    end;
doc_receive_loop([{Pid,Ref}|Rest], 0, Limit, Callback, AccIn) ->
    receive {'DOWN', Ref, process, Pid, #view_row{} = Row} ->
        case Callback(transform_row(Row), AccIn) of
        {ok, Acc} ->
            doc_receive_loop(Rest, 0, Limit-1, Callback, Acc);
        {stop, Acc} ->
            {ok, Acc}
        end
    after 10000 ->
        timeout
    end.

open_doc(DbName, Id, IncludeDocs) ->
    Row = case fabric:open_doc(DbName, Id, [deleted]) of
    {not_found, missing} ->
        Doc = undefined,
        #view_row{key=Id};
    {ok, #doc{deleted=true, revs=Revs}} ->
        Doc = null,
        {RevPos, [RevId|_]} = Revs,
        Value = {[{rev,couch_doc:rev_to_str({RevPos, RevId})}, {deleted,true}]},
        #view_row{key=Id, id=Id, value=Value};
    {ok, #doc{revs=Revs} = Doc0} ->
        Doc = couch_doc:to_json_obj(Doc0, []),
        {RevPos, [RevId|_]} = Revs,
        Value = {[{rev,couch_doc:rev_to_str({RevPos, RevId})}]},
        #view_row{key=Id, id=Id, value=Value}
    end,
    exit(if IncludeDocs -> Row#view_row{doc=Doc}; true -> Row end).


% Instead of ets, let's use an ordered keylist. We'll need to revisit if we
% have >> 100 shards, so a private interface is a good idea. - APK June 2010

init_counters(Keys) ->
    orddict:from_list([{Key,0} || Key <- Keys]).

decrement_all_counters(Dict) ->
    [{K,V-1} || {K,V} <- Dict].

update_counter(Key, Incr, Dict0) ->
    orddict:update_counter(Key, Incr, Dict0).

lookup_element(Key, Dict) ->
    couch_util:get_value(Key, Dict).

waiting_on_shards(Dict) ->
    lists:keymember(0, 2, Dict).

remove(Shard, Dict) ->
    orddict:erase(Shard, Dict).
    
filter(Fun, Dict) ->
    orddict:filter(Fun, Dict).

fold(Fun, Acc0, Dict) ->
    orddict:fold(Fun, Acc0, Dict).
