% Copyright 2010 Cloudant
%
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

-module(fabric_view).

-export([is_progress_possible/1, remove_overlapping_shards/2, maybe_send_row/1,
    maybe_pause_worker/3, maybe_resume_worker/2, transform_row/1, keydict/1,
    extract_view/4, get_shards/2, remove_down_shards/2]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-spec remove_down_shards(#collector{}, node()) ->
    {ok, #collector{}} | {error, any()}.
remove_down_shards(Collector, BadNode) ->
    #collector{callback=Callback, counters=Counters, user_acc=Acc} = Collector,
    case fabric_util:remove_down_workers(Counters, BadNode) of
    {ok, NewCounters} ->
        {ok, Collector#collector{counters = NewCounters}};
    error ->
        Reason = {nodedown, <<"progress not possible">>},
        Callback({error, Reason}, Acc)
    end.

%% @doc looks for a fully covered keyrange in the list of counters
-spec is_progress_possible([{#shard{}, term()}]) -> boolean().
is_progress_possible([]) ->
    false;
is_progress_possible(Counters) ->
    Ranges = fabric_dict:fold(fun(#shard{range=[X,Y]}, _, A) -> [{X,Y}|A] end,
        [], Counters),
    [{Start, Tail0} | Rest] = lists:ukeysort(1, Ranges),
    Result = lists:foldl(fun
    (_, fail) ->
        % we've already declared failure
        fail;
    (_, complete) ->
        % this is the success condition, we can fast-forward
        complete;
    ({X,_}, Tail) when X > (Tail+1) ->
        % gap in the keyrange, we're dead
        fail;
    ({_,Y}, Tail) ->
        case erlang:max(Tail, Y) of
        End when (End+1) =:= (2 bsl 31) ->
            complete;
        Else ->
            % the normal condition, adding to the tail
            Else
        end
    end, if (Tail0+1) =:= (2 bsl 31) -> complete; true -> Tail0 end, Rest),
    (Start =:= 0) andalso (Result =:= complete).

-spec remove_overlapping_shards(#shard{}, [{#shard{}, any()}]) ->
    [{#shard{}, any()}].
remove_overlapping_shards(#shard{range=[A,B]} = Shard0, Shards) ->
    fabric_dict:filter(fun(#shard{range=[X,Y], node=Node, ref=Ref} = Shard, _) ->
        if Shard =:= Shard0 ->
            % we can't remove ourselves
            true;
        A < B, X >= A, X < B ->
            % lower bound is inside our range
            rexi:kill(Node, Ref),
            false;
        A < B, Y > A, Y =< B ->
            % upper bound is inside our range
            rexi:kill(Node, Ref),
            false;
        B < A, X >= A orelse B < A, X < B ->
            % target shard wraps the key range, lower bound is inside
            rexi:kill(Node, Ref),
            false;
        B < A, Y > A orelse B < A, Y =< B ->
            % target shard wraps the key range, upper bound is inside
            rexi:kill(Node, Ref),
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
    #collector{counters=Counters, user_acc=AccIn, callback=Callback} = State,
    case fabric_dict:any(0, Counters) of
    true ->
        % we still need to send the total/offset header
        {ok, State};
    false ->
        {_, Acc} = Callback(complete, AccIn),
        {stop, State#collector{user_acc=Acc}}
    end;
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
            maybe_send_row(NewState#collector{skip=Skip-1});
        {Row, NewState} ->
            case Callback(transform_row(possibly_embed_doc(NewState,Row)), AccIn) of
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

%% if include_docs=true is used when keys and
%% the values contain "_id" then use the "_id"s
%% to retrieve documents and embed in result
possibly_embed_doc(_State,
              #view_row{id=reduced}=Row) ->
    Row;
possibly_embed_doc(_State,
                   #view_row{value=undefined}=Row) ->
    Row;
possibly_embed_doc(#collector{db_name=DbName, query_args=Args},
              #view_row{key=_Key, id=_Id, value=Value, doc=_Doc}=Row) ->
    #view_query_args{include_docs=IncludeDocs} = Args,
    case IncludeDocs andalso is_tuple(Value) of
    true ->
        {Props} = Value,
        Rev0 = couch_util:get_value(<<"_rev">>, Props),
        case couch_util:get_value(<<"_id">>,Props) of
        null -> Row#view_row{doc=null};
        undefined -> Row;
        IncId ->
            % use separate process to call fabric:open_doc
            % to not interfere with current call
            {Pid, Ref} = spawn_monitor(fun() ->
                exit(
                case Rev0 of
                undefined ->
                    case fabric:open_doc(DbName, IncId, []) of
                    {ok, NewDoc} ->
                        Row#view_row{doc=couch_doc:to_json_obj(NewDoc,[])};
                    {not_found, _} ->
                        Row#view_row{doc=null}
                    end;
                Rev0 ->
                    Rev = couch_doc:parse_rev(Rev0),
                    case fabric:open_revs(DbName, IncId, [Rev], []) of
                    {ok, [{ok, NewDoc}]} ->
                        Row#view_row{doc=couch_doc:to_json_obj(NewDoc,[])};
                    {ok, [{{not_found, _}, Rev}]} ->
                        Row#view_row{doc=null}
                    end
                end) end),
            receive {'DOWN',Ref,process,Pid, Resp} ->
                        Resp
            end
        end;
        _ -> Row
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
        NewState = lists:foldl(fun(#view_row{worker=Worker}, StateAcc) ->
            maybe_resume_worker(Worker, StateAcc)
        end, NewSt, Records),
        {#view_row{key=Key, id=reduced, value=Reduced}, NewState};
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
    twig:log(error, "missing_named_view ~p", [ViewName]),
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

get_shards(DbName, #view_query_args{stale=Stale})
  when Stale == ok orelse Stale == update_after ->
    mem3:ushards(DbName);
get_shards(DbName, #view_query_args{stale=false}) ->
    mem3:shards(DbName).

% unit test
is_progress_possible_test() ->
    EndPoint = 2 bsl 31,
    T1 = [[0, EndPoint-1]],
    ?assertEqual(is_progress_possible(mk_cnts(T1)),true),
    T2 = [[0,10],[11,20],[21,EndPoint-1]],
    ?assertEqual(is_progress_possible(mk_cnts(T2)),true),
    % gap
    T3 = [[0,10],[12,EndPoint-1]],
    ?assertEqual(is_progress_possible(mk_cnts(T3)),false),
    % outside range
    T4 = [[1,10],[11,20],[21,EndPoint-1]],
    ?assertEqual(is_progress_possible(mk_cnts(T4)),false),
    % outside range
    T5 = [[0,10],[11,20],[21,EndPoint]],
    ?assertEqual(is_progress_possible(mk_cnts(T5)),false).

remove_overlapping_shards_test() ->
    EndPoint = 2 bsl 31,
    T1 = [[0,10],[11,20],[21,EndPoint-1]],
    Shards = mk_cnts(T1,3),
    ?assertEqual(orddict:size(
              remove_overlapping_shards(#shard{name=list_to_atom("node-3"),
                                               node=list_to_atom("node-3"),
                                               range=[11,20]},
                                        Shards)),7).

mk_cnts(Ranges) ->
    Shards = lists:map(fun(Range) ->
                               #shard{range=Range}
                                    end,
                        Ranges),
    orddict:from_list([{Shard,nil} || Shard <- Shards]).

mk_cnts(Ranges, NoNodes) ->
    orddict:from_list([{Shard,nil}
                       || Shard <-
                              lists:flatten(lists:map(
                                 fun(Range) ->
                                         mk_shards(NoNodes,Range,[])
                                 end, Ranges))]
                     ).

mk_shards(0,_Range,Shards) ->
    Shards;
mk_shards(NoNodes,Range,Shards) ->
    NodeName = list_to_atom("node-" ++ integer_to_list(NoNodes)),
    mk_shards(NoNodes-1,Range,
              [#shard{name=NodeName, node=NodeName, range=Range} | Shards]).
