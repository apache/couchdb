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
    transform_row/1, keydict/1, extract_view/4, get_shards/2,
    check_down_shards/2, handle_worker_exit/3,
    get_shard_replacements/2]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

%% @doc Check if a downed node affects any of our workers
-spec check_down_shards(#collector{}, node()) ->
    {ok, #collector{}} | {error, any()}.
check_down_shards(Collector, BadNode) ->
    #collector{callback=Callback, counters=Counters, user_acc=Acc} = Collector,
    Filter = fun(#shard{node = Node}, _) -> Node == BadNode end,
    BadCounters = fabric_dict:filter(Filter, Counters),
    case fabric_dict:size(BadCounters) > 0 of
        true ->
            Reason = {nodedown, <<"progress not possible">>},
            Callback({error, Reason}, Acc),
            {error, Reason};
        false ->
            {ok, Collector}
    end.

%% @doc Handle a worker that dies during a stream
-spec handle_worker_exit(#collector{}, #shard{}, any()) -> {error, any()}.
handle_worker_exit(Collector, _Worker, Reason) ->
    #collector{callback=Callback, user_acc=Acc} = Collector,
    {ok, Resp} = Callback({error, fabric_util:error_info(Reason)}, Acc),
    {error, Resp}.

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

maybe_send_row(#collector{limit=0} = State) ->
    #collector{counters=Counters, user_acc=AccIn, callback=Callback} = State,
    case fabric_dict:any(0, Counters) of
    true ->
        % we still need to send the total/offset header
        {ok, State};
    false ->
        erase(meta_sent),
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
            erase(meta_sent),
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
    #mrargs{include_docs=IncludeDocs} = Args,
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


keydict(undefined) ->
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
        query_args = #mrargs{direction=Dir},
        keys = Keys,
        rows = RowDict,
        lang = Lang,
        counters = Counters0
    } = St,
    {Key, RestKeys} = find_next_key(Keys, Dir, RowDict),
    case dict:find(Key, RowDict) of
    {ok, Records} ->
        NewRowDict = dict:erase(Key, RowDict),
        Counters = lists:foldl(fun(#view_row{worker={Worker,From}}, CntrsAcc) ->
            case From of
                {Pid, _} when is_pid(Pid) ->
                    gen_server:reply(From, ok);
                Pid when is_pid(Pid) ->
                    rexi:stream_ack(From)
            end,
            fabric_dict:update_counter(Worker, -1, CntrsAcc)
        end, Counters0, Records),
        Wrapped = [[V] || #view_row{value=V} <- Records],
        {ok, [Reduced]} = couch_query_servers:rereduce(Lang, [RedSrc], Wrapped),
        NewSt = St#collector{keys=RestKeys, rows=NewRowDict, counters=Counters},
        {#view_row{key=Key, id=reduced, value=Reduced}, NewSt};
    error ->
        get_next_row(St#collector{keys=RestKeys})
    end;
get_next_row(State) ->
    #collector{rows = [Row|Rest], counters = Counters0} = State,
    {Worker, From} = Row#view_row.worker,
    rexi:stream_ack(From),
    Counters1 = fabric_dict:update_counter(Worker, -1, Counters0),
    {Row, State#collector{rows = Rest, counters=Counters1}}.

%% TODO: rectify nil <-> undefined discrepancies
find_next_key(nil, Dir, RowDict) ->
    find_next_key(undefined, Dir, RowDict);
find_next_key(undefined, Dir, RowDict) ->
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
    {row, [{key,Key}, {value,Value}]};
transform_row(#view_row{key=Key, id=undefined}) ->
    {row, [{key,Key}, {id,error}, {value,not_found}]};
transform_row(#view_row{key=Key, id=Id, value=Value, doc=undefined}) ->
    {row, [{id,Id}, {key,Key}, {value,Value}]};
transform_row(#view_row{key=Key, id=_Id, value=_Value, doc={error,Reason}}) ->
    {row, [{id,error}, {key,Key}, {value,Reason}]};
transform_row(#view_row{key=Key, id=Id, value=Value, doc=Doc}) ->
    {row, [{id,Id}, {key,Key}, {value,Value}, {doc,Doc}]}.


sort_fun(fwd) ->
    fun(A,A) -> true; (A,B) -> couch_ejson_compare:less_json(A,B) end;
sort_fun(rev) ->
    fun(A,A) -> true; (A,B) -> couch_ejson_compare:less_json(B,A) end.

extract_view(Pid, ViewName, [], _ViewType) ->
    couch_log:error("missing_named_view ~p", [ViewName]),
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
    [Name || {Name, _} <- View#mrview.reduce_funs];
view_names(View, map) ->
    View#mrview.map_names.

index_of(X, List) ->
    index_of(X, List, 1).

index_of(_X, [], _I) ->
    not_found;
index_of(X, [X|_Rest], I) ->
    I;
index_of(X, [_|Rest], I) ->
    index_of(X, Rest, I+1).

get_shards(DbName, #mrargs{stale=Stale})
  when Stale == ok orelse Stale == update_after ->
    mem3:ushards(DbName);
get_shards(DbName, #mrargs{stale=false}) ->
    mem3:shards(DbName).

get_shard_replacements(DbName, UsedShards) ->
    % We only want to generate a replacements list from shards
    % that aren't already used.
    AllLiveShards = mem3:live_shards(DbName, [node() | nodes()]),
    UnusedShards = AllLiveShards -- UsedShards,

    % If we have more than one copy of a range then we don't
    % want to try and add a replacement to any copy.
    RangeCounts = lists:foldl(fun(#shard{range=R}, Acc) ->
        dict:update_counter(R, 1, Acc)
    end, dict:new(), UsedShards),

    % For each seq shard range with a count of 1, find any
    % possible replacements from the unused shards. The
    % replacement list is keyed by range.
    lists:foldl(fun(#shard{range=Range}, Acc) ->
        case dict:find(Range, RangeCounts) of
            {ok, 1} ->
                Repls = [S || S <- UnusedShards, S#shard.range =:= Range],
                % Only keep non-empty lists of replacements
                if Repls == [] -> Acc; true ->
                    [{Range, Repls} | Acc]
                end;
            _ ->
                Acc
        end
    end, [], UsedShards).

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
    meck:new(rexi),
    meck:expect(rexi, kill, fun(_, _) -> ok end),
    EndPoint = 2 bsl 31,
    T1 = [[0,10],[11,20],[21,EndPoint-1]],
    Shards = mk_cnts(T1,3),
    ?assertEqual(orddict:size(
              remove_overlapping_shards(#shard{name=list_to_atom("node-3"),
                                               node=list_to_atom("node-3"),
                                               range=[11,20]},
                                        Shards)),7),
    meck:unload(rexi).

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
