-module(fabric_view).

-export([is_progress_possible/1, remove_overlapping_shards/2, maybe_send_row/1,
    maybe_pause_worker/3, maybe_resume_worker/2, transform_row/1, keydict/1,
    extract_view/4]).

-include("fabric.hrl").

%% @doc looks for a fully covered keyrange in the list of counters
-spec is_progress_possible([{#shard{}, non_neg_integer()}]) -> boolean().
is_progress_possible(Counters) ->
    Ranges = fabric_dict:fold(fun(#shard{range=[X,Y]}, _, A) -> [{X,Y}|A] end,
        [], Counters),
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

-spec remove_overlapping_shards(#shard{}, [#shard{}]) -> [#shard{}].
remove_overlapping_shards(#shard{range=[A,B]} = Shard0, Shards) ->
    fabric_dict:filter(fun(#shard{range=[X,Y]} = Shard, _Value) ->
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

maybe_pause_worker(Worker, From, State) ->
    #collector{buffer_size = BufferSize, counters = Counters} = State,
    case fabric_dict:lookup_element(Worker, Counters) of
    BufferSize ->
        State#collector{blocked = [{Worker,From} | State#collector.blocked]};
    _Count ->
        gen_server:reply(From, ok),
        State
    end.

maybe_resume_worker(Worker, State) ->
    #collector{buffer_size = Buffer, counters = C, blocked = B} = State,
    case fabric_dict:lookup_element(Worker, C) of
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
    case fabric_dict:any(0, Counters) of
    true ->
        {ok, State};
    false ->
        try get_next_row(State) of
        {_, NewState} when Skip > 0 ->
            maybe_send_row(NewState#collector{skip=Skip-1, limit=Limit-1});
        {Row, NewState} ->
            case Callback(transform_row(Row), AccIn) of
            {stop, Acc} ->
                {stop, NewState#collector{user_acc=Acc, limit=Limit-1}};
            {ok, Acc} ->
                maybe_send_row(NewState#collector{user_acc=Acc, limit=Limit-1})
            end
        catch complete ->
            {_, Acc} = Callback(complete, AccIn),
            {stop, State#collector{user_acc=Acc}}
        end
    end.

keydict(nil) ->
    undefined;
keydict(Keys) ->
    {Dict,_} = lists:foldl(fun(K, {D,I}) -> {dict:store(K,I,D), I+1} end,
        {dict:new(),0}, Keys),
    Dict.

%% internal %%

get_next_row(#collector{rows = []}) ->
    throw(complete);
get_next_row(#collector{reducer = RedSrc} = St) when RedSrc =/= undefined ->
    #collector{
        query_args = #view_query_args{direction=Dir},
        keys = Keys,
        rows = RowDict,
        os_proc = Proc,
        counters = Counters0
    } = St,
    {Key, RestKeys} = find_next_key(Keys, Dir, RowDict),
    case dict:find(Key, RowDict) of
    {ok, Records} ->
        NewRowDict = dict:erase(Key, RowDict),
        Counters = lists:foldl(fun(#view_row{worker=Worker}, CountersAcc) ->
            fabric_dict:update_counter(Worker, -1, CountersAcc)
        end, Counters0, Records),
        Wrapped = [[V] || #view_row{value=V} <- Records],
        {ok, [Reduced]} = couch_query_servers:rereduce(Proc, [RedSrc], Wrapped),
        NewSt = St#collector{keys=RestKeys, rows=NewRowDict, counters=Counters},
        {#view_row{key=Key, id=reduced, value=Reduced}, NewSt};
    error ->
        get_next_row(St#collector{keys=RestKeys})
    end;
get_next_row(State) ->
    #collector{rows = [Row|Rest], counters = Counters0} = State,
    Worker = Row#view_row.worker,
    Counters1 = fabric_dict:update_counter(Worker, -1, Counters0),
    NewState = maybe_resume_worker(Worker, State#collector{counters=Counters1}),
    {Row, NewState#collector{rows = Rest}}.

find_next_key(nil, Dir, RowDict) ->
    case lists:sort(sort_fun(Dir), dict:fetch_keys(RowDict)) of
    [] ->
        throw(complete);
    [Key|_] ->
        {Key, nil}
    end;
find_next_key([], _, _) ->
    throw(complete);
find_next_key([Key|Rest], _, _) ->
    {Key, Rest}.

transform_row(#view_row{key=Key, id=reduced, value=Value}) ->
    {row, {[{key,Key}, {value,Value}]}};
transform_row(#view_row{key=Key, id=undefined}) ->
    {row, {[{key,Key}, {error,not_found}]}};
transform_row(#view_row{key=Key, id=Id, value=Value, doc=undefined}) ->
    {row, {[{id,Id}, {key,Key}, {value,Value}]}};
transform_row(#view_row{key=Key, id=Id, value=Value, doc={error,Reason}}) ->
    {row, {[{id,Id}, {key,Key}, {value,Value}, {error,Reason}]}};
transform_row(#view_row{key=Key, id=Id, value=Value, doc=Doc}) ->
    {row, {[{id,Id}, {key,Key}, {value,Value}, {doc,Doc}]}}.

sort_fun(fwd) ->
    fun(A,A) -> true; (A,B) -> couch_view:less_json(A,B) end;
sort_fun(rev) ->
    fun(A,A) -> true; (A,B) -> couch_view:less_json(B,A) end.

extract_view(Pid, ViewName, [], _ViewType) ->
    ?LOG_ERROR("missing_named_view ~p", [ViewName]),
    exit(Pid, kill),
    exit(missing_named_view);
extract_view(Pid, ViewName, [View|Rest], ViewType) ->
    case lists:member(ViewName, view_names(View, ViewType)) of
    true ->
        if ViewType == reduce ->
            {index_of(ViewName, view_names(View, reduce)), View};
        true ->
            View
        end;
    false ->
        extract_view(Pid, ViewName, Rest, ViewType)
    end.

view_names(View, Type) when Type == red_map; Type == reduce ->
    [Name || {Name, _} <- View#view.reduce_funs];
view_names(View, map) ->
    View#view.map_names.

index_of(X, List) ->
    index_of(X, List, 1).

index_of(_X, [], _I) ->
    not_found;
index_of(X, [X|_Rest], I) ->
    I;
index_of(X, [_|Rest], I) ->
    index_of(X, Rest, I+1).
