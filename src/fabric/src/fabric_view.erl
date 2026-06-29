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

-export([
    remove_overlapping_shards/2,
    maybe_send_row/1,
    keydict/1,
    extract_view/4,
    get_shards/2,
    check_down_shards/2,
    handle_worker_exit/3,
    get_shard_replacements/2,
    maybe_update_others/5
]).
-export([fix_skip_and_limit/1]).
-export([reduce_buffer_new/0, reduce_buffer_add/4, buf_key/2]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

%% @doc Check if a downed node affects any of our workers
-spec check_down_shards(#collector{}, node()) ->
    {ok, #collector{}} | {error, any()}.
check_down_shards(Collector, BadNode) ->
    #collector{callback = Callback, counters = Counters, user_acc = Acc} = Collector,
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
    #collector{callback = Callback, user_acc = Acc} = Collector,
    case Callback({error, fabric_util:error_info(Reason)}, Acc) of
        {error, Resp} ->
            {error, Resp};
        {ok, Resp} ->
            {error, Resp}
    end.

-spec remove_overlapping_shards(#shard{}, [{#shard{}, any()}]) ->
    [{#shard{}, any()}].
remove_overlapping_shards(#shard{} = Shard, Counters) ->
    remove_overlapping_shards(Shard, Counters, fun stop_worker/1).

-spec remove_overlapping_shards(#shard{}, [{#shard{}, any()}], fun()) ->
    [{#shard{}, any()}].
remove_overlapping_shards(#shard{} = Shard, Counters, RemoveCb) ->
    Counters1 = filter_exact_copies(Shard, Counters, RemoveCb),
    filter_possible_overlaps(Shard, Counters1, RemoveCb).

filter_possible_overlaps(Shard, Counters, RemoveCb) ->
    Ranges0 = fabric_util:worker_ranges(Counters),
    #shard{range = [BShard, EShard]} = Shard,
    Ranges = Ranges0 ++ [{BShard, EShard}],
    {Bs, Es} = lists:unzip(Ranges),
    {MinB, MaxE} = {lists:min(Bs), lists:max(Es)},
    % Use a custom sort function which prioritizes the given shard
    % range when the start endpoints match.
    SortFun = fun
        ({B, E}, {B, _}) when {B, E} =:= {BShard, EShard} ->
            % If start matches with the shard's start, shard always wins
            true;
        ({B, _}, {B, E}) when {B, E} =:= {BShard, EShard} ->
            % If start matches with the shard's start, shard always wins
            false;
        ({B, E1}, {B, E2}) ->
            % If start matches, pick the longest range first
            E2 >= E1;
        ({B1, _}, {B2, _}) ->
            % Then, by default, sort by start point
            B1 =< B2
    end,
    Ring = mem3_util:get_ring(Ranges, SortFun, MinB, MaxE),
    fabric_dict:filter(
        fun
            (S, _) when S =:= Shard ->
                % Keep the original shard
                true;
            (#shard{range = [B, E]} = S, _) ->
                case lists:member({B, E}, Ring) of
                    true ->
                        % Keep it
                        true;
                    false ->
                        % Duplicate range, delete after calling callback function
                        case is_function(RemoveCb) of
                            true -> RemoveCb(S);
                            false -> ok
                        end,
                        false
                end
        end,
        Counters
    ).

filter_exact_copies(#shard{range = Range0} = Shard0, Shards, Cb) ->
    fabric_dict:filter(
        fun
            (Shard, _) when Shard =:= Shard0 ->
                % Don't remove ourselves
                true;
            (#shard{range = Range} = Shard, _) when Range =:= Range0 ->
                case is_function(Cb) of
                    true -> Cb(Shard);
                    false -> ok
                end,
                false;
            (_, _) ->
                true
        end,
        Shards
    ).

stop_worker(#shard{ref = Ref, node = Node}) ->
    rexi:kill_all([{Node, Ref}]).

maybe_send_row(#collector{limit = 0} = State) ->
    #collector{counters = Counters, user_acc = AccIn, callback = Callback} = State,
    case fabric_dict:any(0, Counters) of
        true ->
            % we still need to send the total/offset header
            {ok, State};
        false ->
            erase(meta_sent),
            {_, Acc} = Callback(complete, AccIn),
            {stop, State#collector{user_acc = Acc}}
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
                    maybe_send_row(NewState#collector{skip = Skip - 1});
                {Row0, NewState} ->
                    Row1 = possibly_embed_doc(NewState, Row0),
                    Row2 = detach_partition(Row1),
                    Row3 = fabric_view_row:transform(Row2),
                    case Callback(Row3, AccIn) of
                        {stop, Acc} ->
                            {stop, NewState#collector{user_acc = Acc, limit = Limit - 1}};
                        {ok, Acc} ->
                            maybe_send_row(NewState#collector{user_acc = Acc, limit = Limit - 1})
                    end
            catch
                complete ->
                    erase(meta_sent),
                    {_, Acc} = Callback(complete, AccIn),
                    {stop, State#collector{user_acc = Acc}}
            end
    end.

%% if include_docs=true is used when keys and
%% the values contain "_id" then use the "_id"s
%% to retrieve documents and embed in result
possibly_embed_doc(
    #collector{db_name = DbName, query_args = Args},
    Row
) ->
    IsReduced = fabric_view_row:get_id(Row) == reduced,
    #mrargs{include_docs = IncludeDocs} = Args,
    Value = fabric_view_row:get_value(Row),
    case (not IsReduced) andalso IncludeDocs andalso is_tuple(Value) of
        true ->
            {Props} = Value,
            case couch_util:get_value(<<"_id">>, Props) of
                null ->
                    fabric_view_row:set_doc(Row, null);
                undefined ->
                    Row;
                IncId ->
                    Rev0 = couch_util:get_value(<<"_rev">>, Props),
                    % use separate process to call fabric:open_doc
                    % to not interfere with current call
                    {Pid, Ref} = spawn_monitor(fun() ->
                        exit(
                            case Rev0 of
                                undefined ->
                                    case fabric:open_doc(DbName, IncId, []) of
                                        {ok, NewDoc} ->
                                            fabric_view_row:set_doc(
                                                Row, couch_doc:to_json_obj(NewDoc, [])
                                            );
                                        {not_found, _} ->
                                            fabric_view_row:set_doc(Row, null);
                                        Else ->
                                            fabric_view_row:set_doc(Row, {error, Else})
                                    end;
                                Rev0 ->
                                    Rev = couch_doc:parse_rev(Rev0),
                                    case fabric:open_revs(DbName, IncId, [Rev], []) of
                                        {ok, [{ok, NewDoc}]} ->
                                            fabric_view_row:set_doc(
                                                Row, couch_doc:to_json_obj(NewDoc, [])
                                            );
                                        {ok, [{{not_found, _}, Rev}]} ->
                                            fabric_view_row:set_doc(Row, null);
                                        Else ->
                                            fabric_view_row:set_doc(Row, {error, Else})
                                    end
                            end
                        )
                    end),
                    receive
                        {'DOWN', Ref, process, Pid, Resp} ->
                            Resp
                    end
            end;
        false ->
            Row
    end.

detach_partition(Row) ->
    case fabric_view_row:get_key(Row) of
        {p, _Partition, Key} -> fabric_view_row:set_key(Row, Key);
        _Key -> Row
    end.

keydict(undefined) ->
    undefined;
keydict(Keys) ->
    {Dict, _} = lists:foldl(
        fun(K, {D, I}) -> {dict:store(K, I, D), I + 1} end,
        {dict:new(), 0},
        Keys
    ),
    Dict.

%% internal %%

get_next_row(#collector{rows = []}) ->
    throw(complete);
get_next_row(#collector{reducer = RedSrc} = State0) when RedSrc =/= undefined ->
    #collector{
        query_args = #mrargs{direction = Dir, extra = Options},
        keys = Keys,
        rows = Buffer0,
        lang = Lang,
        counters = Counters0,
        collation = Collation
    } = State0,
    case reduce_take(Keys, Dir, Collation, Buffer0) of
        {skip, RestKeys} ->
            % explicit key not present in the buffer, move on to the next one
            get_next_row(State0#collector{keys = RestKeys});
        {Key, Rows, RestKeys, Buffer} ->
            Counters = lists:foldl(
                fun(Row, CntrsAcc) ->
                    {Worker, From} = fabric_view_row:get_worker(Row),
                    case From of
                        {Pid, _} when is_pid(Pid) ->
                            gen_server:reply(From, ok);
                        Pid when is_pid(Pid) ->
                            rexi:stream_ack(From)
                    end,
                    fabric_dict:update_counter(Worker, -1, CntrsAcc)
                end,
                Counters0,
                Rows
            ),
            Wrapped = [[fabric_view_row:get_value(R)] || R <- Rows],
            ReduceCtx = ?l2b(
                io_lib:format("~s/~s/_view/~s", [
                    State0#collector.db_name,
                    State0#collector.ddoc_name,
                    State0#collector.view_name
                ])
            ),
            {ok, [Reduced]} = couch_query_servers:rereduce(Lang, [RedSrc], Wrapped, ReduceCtx),
            {ok, Finalized} = couch_query_servers:finalize(RedSrc, Reduced),
            State = State0#collector{keys = RestKeys, rows = Buffer, counters = Counters},
            ViewRow = fabric_view_row:from_props(
                [{key, Key}, {id, reduced}, {value, Finalized}], Options
            ),
            {ViewRow, State}
    end;
get_next_row(State) ->
    #collector{rows = [Head | Rest], counters = Counters0} = State,
    % map rows may be wrapped as {BufKey, Row} by fabric_view_map:merge_row;
    % a plain row is tagged view_row, a wrapped one has a sort-key tuple first.
    Row =
        case element(1, Head) of
            view_row -> Head;
            _ -> element(2, Head)
        end,
    {Worker, From} = fabric_view_row:get_worker(Row),
    rexi:stream_ack(From),
    Counters1 = fabric_dict:update_counter(Worker, -1, Counters0),
    {Row, State#collector{rows = Rest, counters = Counters1}}.

% The reduce buffer is a gb_tree keyed by buf_key. A buf_key can be either a
% raw key (when using the "raw" collation) or an ICU sort term. Under ICU keys
% that collate equal will group under the same buf_key. Then take_smallest of a
% gb_tree will yield the next row to be emitted with an O(log N) complexity.
% (As opposed O(n) if just search through all the entries with a pair-wise ICU
% compare(A,B) function.
reduce_buffer_new() ->
    gb_trees:empty().

reduce_buffer_add(Collation, Key, Row, Buffer) ->
    BufKey = buf_key(Collation, Key),
    case gb_trees:lookup(BufKey, Buffer) of
        none ->
            gb_trees:insert(BufKey, {Key, [Row]}, Buffer);
        {value, {Key0, Rows}} ->
            gb_trees:update(BufKey, {Key0, Rows ++ [Row]}, Buffer)
    end.

% The buffer key for views. For a "raw" collator it's just the key as is. For
% the default ICU collation it's sort key generated by ICU We also handle the
% partition wrapped rows and unwrap them before comparing (see detach_partition/1).
buf_key(Collation, {p, Partition, Key}) ->
    {p, Partition, buf_key(Collation, Key)};
buf_key(<<"raw">>, Key) ->
    Key;
buf_key(_Collation, Key) ->
    couch_ejson_compare:sort_key(Key).

% Get the next key from the reduce buffer. This would be called from
% get_next_row() and and only after we've checked fabric_dict:any(0, Counters),
% so that when a group key is removed, all shards would have stream passed it.
% The rows we return is the complete set and we can then re-reduce them.
%
reduce_take(undefined, Dir, _Collation, Buffer) ->
    % Get the next key in collation order or throw(complete) when no more keys
    case gb_trees:is_empty(Buffer) of
        true ->
            throw(complete);
        false ->
            {_BufKey, {Key, Rows}, Buffer1} =
                case Dir of
                    rev -> gb_trees:take_largest(Buffer);
                    _ -> gb_trees:take_smallest(Buffer)
                end,
            {Key, Rows, undefined, Buffer1}
    end;
reduce_take([], _Dir, _Collation, _Buffer) ->
    %   For an explicit list of user provided keys:
    %     - Get the next matching key or throw(complete) if no more keys remain
    %     - Return {skip, RestKeys} if we have keys but this key is not found
    throw(complete);
reduce_take([Key | RestKeys], _Dir, Collation, Buffer) ->
    % Explicit keys are emitted in the requested order
    BufKey = buf_key(Collation, Key),
    case gb_trees:lookup(BufKey, Buffer) of
        none ->
            {skip, RestKeys};
        {value, {Key0, Rows}} ->
            {Key0, Rows, RestKeys, gb_trees:delete(BufKey, Buffer)}
    end.

extract_view(Pid, ViewName, [], _ViewType) ->
    couch_log:error("missing_named_view ~p", [ViewName]),
    exit(Pid, kill),
    exit(missing_named_view);
extract_view(Pid, ViewName, [View | Rest], ViewType) ->
    case lists:member(ViewName, view_names(View, ViewType)) of
        true ->
            if
                ViewType == reduce ->
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
index_of(X, [X | _Rest], I) ->
    I;
index_of(X, [_ | Rest], I) ->
    index_of(X, Rest, I + 1).

get_shards(Db, #mrargs{} = Args) ->
    DbPartitioned = fabric_util:is_partitioned(Db),
    Partition = couch_mrview_util:get_extra(Args, partition),
    if
        DbPartitioned orelse Partition == undefined -> ok;
        true -> throw({bad_request, <<"partition specified on non-partitioned db">>})
    end,
    DbName = fabric:dbname(Db),
    % Decide which version of mem3:shards/1,2 or
    % mem3:ushards/1,2 to use for the current
    % request.
    case {Args#mrargs.stable, Partition} of
        {true, undefined} ->
            {mem3:ushards(DbName), []};
        {true, Partition} ->
            Shards = mem3:ushards(DbName, couch_partition:shard_key(Partition)),
            {Shards, [{any, Shards}]};
        {false, undefined} ->
            {mem3:shards(DbName), []};
        {false, Partition} ->
            Shards = mem3:shards(DbName, couch_partition:shard_key(Partition)),
            {Shards, [{any, Shards}]}
    end.

maybe_update_others(
    DbName,
    DDoc,
    ShardsInvolved,
    ViewName,
    #mrargs{update = lazy} = Args
) ->
    BlockInteractiveIndexing = couch_disk_monitor:block_interactive_view_indexing(),
    ShardsNeedUpdated =
        case BlockInteractiveIndexing of
            true ->
                [];
            false ->
                mem3:shards(DbName) -- ShardsInvolved
        end,
    lists:foreach(
        fun(#shard{node = Node, name = ShardName}) ->
            rpc:cast(Node, fabric_rpc, update_mrview, [ShardName, DDoc, ViewName, Args])
        end,
        ShardsNeedUpdated
    );
maybe_update_others(_DbName, _DDoc, _ShardsInvolved, _ViewName, _Args) ->
    ok.

get_shard_replacements(DbName, UsedShards0) ->
    % We only want to generate a replacements list from shards
    % that aren't already used.
    AllLiveShards = mem3:live_shards(DbName, [node() | nodes()]),
    UsedShards = [S#shard{ref = undefined} || S <- UsedShards0],
    get_shard_replacements_int(AllLiveShards -- UsedShards, UsedShards).

get_shard_replacements_int(UnusedShards, UsedShards) ->
    % If we have more than one copy of a range then we don't
    % want to try and add a replacement to any copy.
    RangeCounts = lists:foldl(
        fun(#shard{range = R}, Acc) ->
            dict:update_counter(R, 1, Acc)
        end,
        dict:new(),
        UsedShards
    ),

    % For each seq shard range with a count of 1, find any
    % possible replacements from the unused shards. The
    % replacement list is keyed by range.
    lists:foldl(
        fun(#shard{range = [B, E] = Range}, Acc) ->
            case dict:find(Range, RangeCounts) of
                {ok, 1} ->
                    Repls = mem3_util:non_overlapping_shards(UnusedShards, B, E),
                    % Only keep non-empty lists of replacements
                    if
                        Repls == [] -> Acc;
                        true -> [{Range, Repls} | Acc]
                    end;
                _ ->
                    Acc
            end
        end,
        [],
        UsedShards
    ).

-spec fix_skip_and_limit(#mrargs{}) -> {CoordArgs :: #mrargs{}, WorkerArgs :: #mrargs{}}.
fix_skip_and_limit(#mrargs{} = Args) ->
    {CoordArgs, WorkerArgs} =
        case couch_mrview_util:get_extra(Args, partition) of
            undefined ->
                #mrargs{skip = Skip, limit = Limit} = Args,
                {Args, Args#mrargs{skip = 0, limit = Skip + Limit}};
            _Partition ->
                {Args#mrargs{skip = 0}, Args}
        end,
    %% the coordinator needs to finalize each row, so make sure the shards don't
    {CoordArgs, remove_finalizer(WorkerArgs)}.

remove_finalizer(Args) ->
    couch_mrview_util:set_extra(Args, finalizer, null).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

% These used to be our main collation comparison before switching to sort keys.
% Move them to the testing section to test before and after and that our sort
% keys work the same as pair-wise comparisons
%
compare(fwd, <<"raw">>, A, B) -> A < B;
compare(rev, <<"raw">>, A, B) -> B < A;
compare(fwd, _, A, B) -> couch_ejson_compare:less_json(A, B);
compare(rev, _, A, B) -> couch_ejson_compare:less_json(B, A).

remove_overlapping_shards_test() ->
    Cb = undefined,

    Shards = mk_cnts([[0, 10], [11, 20], [21, ?RING_END]], 3),

    % Simple (exact) overlap
    Shard1 = mk_shard("node-3", [11, 20]),
    Shards1 = fabric_dict:store(Shard1, nil, Shards),
    R1 = remove_overlapping_shards(Shard1, Shards1, Cb),
    ?assertEqual(
        [{0, 10}, {11, 20}, {21, ?RING_END}],
        fabric_util:worker_ranges(R1)
    ),
    ?assert(fabric_dict:is_key(Shard1, R1)),

    % Split overlap (shard overlap multiple workers)
    Shard2 = mk_shard("node-3", [0, 20]),
    Shards2 = fabric_dict:store(Shard2, nil, Shards),
    R2 = remove_overlapping_shards(Shard2, Shards2, Cb),
    ?assertEqual(
        [{0, 20}, {21, ?RING_END}],
        fabric_util:worker_ranges(R2)
    ),
    ?assert(fabric_dict:is_key(Shard2, R2)).

get_shard_replacements_test() ->
    Unused = [
        mk_shard(N, [B, E])
     || {N, B, E} <- [
            {"n1", 11, 20},
            {"n1", 21, ?RING_END},
            {"n2", 0, 4},
            {"n2", 5, 10},
            {"n2", 11, 20},
            {"n3", 0, 21, ?RING_END}
        ]
    ],
    Used = [
        mk_shard(N, [B, E])
     || {N, B, E} <- [
            {"n2", 21, ?RING_END},
            {"n3", 0, 10},
            {"n3", 11, 20}
        ]
    ],
    Res = lists:sort(get_shard_replacements_int(Unused, Used)),
    % Notice that [0, 10] range can be replaced by spawning the [0, 4] and [5,
    % 10] workers on n1
    Expect = [
        {[0, 10], [mk_shard("n2", [0, 4]), mk_shard("n2", [5, 10])]},
        {[11, 20], [mk_shard("n1", [11, 20]), mk_shard("n2", [11, 20])]},
        {[21, ?RING_END], [mk_shard("n1", [21, ?RING_END])]}
    ],
    ?assertEqual(Expect, Res).

mk_cnts(Ranges, NoNodes) ->
    orddict:from_list([
        {Shard, nil}
     || Shard <-
            lists:flatten(
                lists:map(
                    fun(Range) ->
                        mk_shards(NoNodes, Range, [])
                    end,
                    Ranges
                )
            )
    ]).

mk_shards(0, _Range, Shards) ->
    Shards;
mk_shards(NoNodes, Range, Shards) ->
    Name = "node-" ++ integer_to_list(NoNodes),
    mk_shards(NoNodes - 1, Range, [mk_shard(Name, Range) | Shards]).

mk_shard(Name, Range) ->
    Node = list_to_atom(Name),
    BName = list_to_binary(Name),
    #shard{name = BName, node = Node, range = Range}.

possibly_embed_doc_test_() ->
    {
        foreach,
        fun() -> meck:new(fabric) end,
        fun(_) -> meck:unload() end,
        [
            ?TDEF_FE(t_possibly_embed_doc_reduced),
            ?TDEF_FE(t_possibly_embed_doc_no_docs),
            ?TDEF_FE(t_possible_embed_doc_no_props),
            ?TDEF_FE(t_possible_embed_doc_id_null),
            ?TDEF_FE(t_possible_embed_doc_id_undefined),
            ?TDEF_FE(t_possible_embed_doc_no_rev),
            ?TDEF_FE(t_possible_embed_doc_no_rev_doc_not_found),
            ?TDEF_FE(t_possible_embed_doc_no_rev_doc_error),
            ?TDEF_FE(t_possible_embed_doc),
            ?TDEF_FE(t_possible_embed_doc_not_found),
            ?TDEF_FE(t_possible_embed_doc_error)
        ]
    }.

t_possibly_embed_doc_reduced(_) ->
    State = #collector{query_args = #mrargs{}},
    Row1 = #view_row{id = reduced},
    Row2 = {view_row, #{id => reduced}},
    ?assertEqual(Row1, possibly_embed_doc(State, Row1)),
    ?assertEqual(Row2, possibly_embed_doc(State, Row2)).

t_possibly_embed_doc_no_docs(_) ->
    QueryArgs = #mrargs{include_docs = false},
    State = #collector{query_args = QueryArgs},
    Row1 = #view_row{id = id},
    Row2 = {view_row, #{id => id}},
    ?assertEqual(Row1, possibly_embed_doc(State, Row1)),
    ?assertEqual(Row2, possibly_embed_doc(State, Row2)).

t_possible_embed_doc_no_props(_) ->
    QueryArgs = #mrargs{include_docs = true},
    State = #collector{query_args = QueryArgs},
    Row1 = #view_row{id = id},
    Row2 = {view_row, #{id => id}},
    ?assertEqual(Row1, possibly_embed_doc(State, Row1)),
    ?assertEqual(Row2, possibly_embed_doc(State, Row2)).

t_possible_embed_doc_id_null(_) ->
    QueryArgs = #mrargs{include_docs = true},
    State = #collector{query_args = QueryArgs},
    Value = {[{<<"_id">>, null}]},
    Row1 = #view_row{id = id, value = Value},
    Row2 = {view_row, #{id => id, value => Value}},
    Row3 = #view_row{id = id, value = Value, doc = null},
    Row4 = {view_row, #{id => id, value => Value, doc => null}},
    ?assertEqual(Row3, possibly_embed_doc(State, Row1)),
    ?assertEqual(Row4, possibly_embed_doc(State, Row2)).

t_possible_embed_doc_id_undefined(_) ->
    QueryArgs = #mrargs{include_docs = true},
    State = #collector{query_args = QueryArgs},
    Value = {[{<<"_id">>, undefined}]},
    Row1 = #view_row{id = id, value = Value},
    Row2 = {view_row, #{id => id, value => Value}},
    ?assertEqual(Row1, possibly_embed_doc(State, Row1)),
    ?assertEqual(Row2, possibly_embed_doc(State, Row2)).

t_possible_embed_doc_no_rev(_) ->
    DbName = <<"db">>,
    Id = <<"id">>,
    DocId = <<"doc_id">>,
    QueryArgs = #mrargs{include_docs = true},
    State = #collector{db_name = DbName, query_args = QueryArgs},
    Value = {[{<<"_id">>, Id}]},
    NewDoc = #doc{id = DocId},
    EmbeddedDoc = {[{<<"_id">>, DocId}]},
    Row1 = #view_row{id = id, value = Value},
    Row2 = {view_row, #{id => id, value => Value}},
    Row3 = #view_row{id = id, value = Value, doc = EmbeddedDoc},
    Row4 = {view_row, #{id => id, value => Value, doc => EmbeddedDoc}},
    meck:expect(fabric, open_doc, [DbName, Id, []], meck:val({ok, NewDoc})),
    ?assertEqual(Row3, possibly_embed_doc(State, Row1)),
    ?assertEqual(Row4, possibly_embed_doc(State, Row2)).

t_possible_embed_doc_no_rev_doc_not_found(_) ->
    DbName = <<"db">>,
    Id = <<"id">>,
    QueryArgs = #mrargs{include_docs = true},
    State = #collector{db_name = DbName, query_args = QueryArgs},
    Value = {[{<<"_id">>, Id}]},
    Row1 = #view_row{id = id, value = Value},
    Row2 = {view_row, #{id => id, value => Value}},
    Row3 = #view_row{id = id, value = Value, doc = null},
    Row4 = {view_row, #{id => id, value => Value, doc => null}},
    meck:expect(fabric, open_doc, [DbName, Id, []], meck:val({not_found, undefined})),
    ?assertEqual(Row3, possibly_embed_doc(State, Row1)),
    ?assertEqual(Row4, possibly_embed_doc(State, Row2)).

t_possible_embed_doc_no_rev_doc_error(_) ->
    DbName = <<"db">>,
    Id = <<"id">>,
    QueryArgs = #mrargs{include_docs = true},
    State = #collector{db_name = DbName, query_args = QueryArgs},
    Value = {[{<<"_id">>, Id}]},
    Row1 = #view_row{id = id, value = Value},
    Row2 = {view_row, #{id => id, value => Value}},
    Row3 = #view_row{id = id, value = Value, doc = {error, fabric_error}},
    Row4 = {view_row, #{id => id, value => Value, doc => {error, fabric_error}}},
    meck:expect(fabric, open_doc, [DbName, Id, []], meck:val(fabric_error)),
    ?assertEqual(Row3, possibly_embed_doc(State, Row1)),
    ?assertEqual(Row4, possibly_embed_doc(State, Row2)).

t_possible_embed_doc(_) ->
    DbName = <<"db">>,
    Id = <<"id">>,
    Rev = <<"1-foo">>,
    ParsedRev = {1, <<"foo">>},
    DocId = <<"doc_id">>,
    QueryArgs = #mrargs{include_docs = true},
    State = #collector{db_name = DbName, query_args = QueryArgs},
    Value = {[{<<"_id">>, Id}, {<<"_rev">>, Rev}]},
    NewDoc = #doc{id = DocId},
    EmbeddedDoc = {[{<<"_id">>, DocId}]},
    Row1 = #view_row{id = id, value = Value},
    Row2 = {view_row, #{id => id, value => Value}},
    Row3 = #view_row{id = id, value = Value, doc = EmbeddedDoc},
    Row4 = {view_row, #{id => id, value => Value, doc => EmbeddedDoc}},
    meck:expect(fabric, open_revs, [DbName, Id, [ParsedRev], []], meck:val({ok, [{ok, NewDoc}]})),
    ?assertEqual(Row3, possibly_embed_doc(State, Row1)),
    ?assertEqual(Row4, possibly_embed_doc(State, Row2)).

t_possible_embed_doc_not_found(_) ->
    DbName = <<"db">>,
    Id = <<"id">>,
    Rev = <<"1-foo">>,
    ParsedRev = {1, <<"foo">>},
    QueryArgs = #mrargs{include_docs = true},
    State = #collector{db_name = DbName, query_args = QueryArgs},
    Value = {[{<<"_id">>, Id}, {<<"_rev">>, Rev}]},
    Row1 = #view_row{id = id, value = Value},
    Row2 = {view_row, #{id => id, value => Value}},
    Row3 = #view_row{id = id, value = Value, doc = null},
    Row4 = {view_row, #{id => id, value => Value, doc => null}},
    meck:expect(
        fabric,
        open_revs,
        [DbName, Id, [ParsedRev], []],
        meck:val({ok, [{{not_found, undefined}, ParsedRev}]})
    ),
    ?assertEqual(Row3, possibly_embed_doc(State, Row1)),
    ?assertEqual(Row4, possibly_embed_doc(State, Row2)).

t_possible_embed_doc_error(_) ->
    DbName = <<"db">>,
    Id = <<"id">>,
    Rev = <<"1-foo">>,
    ParsedRev = {1, <<"foo">>},
    QueryArgs = #mrargs{include_docs = true},
    State = #collector{db_name = DbName, query_args = QueryArgs},
    Value = {[{<<"_id">>, Id}, {<<"_rev">>, Rev}]},
    Row1 = #view_row{id = id, value = Value},
    Row2 = {view_row, #{id => id, value => Value}},
    Row3 = #view_row{id = id, value = Value, doc = {error, fabric_error}},
    Row4 = {view_row, #{id => id, value => Value, doc => {error, fabric_error}}},
    meck:expect(fabric, open_revs, [DbName, Id, [ParsedRev], []], meck:val(fabric_error)),
    ?assertEqual(Row3, possibly_embed_doc(State, Row1)),
    ?assertEqual(Row4, possibly_embed_doc(State, Row2)).

detach_partition_test_() ->
    {
        foreach,
        fun() -> ok end,
        fun(_) -> ok end,
        [
            ?TDEF_FE(t_detach_partition_partition_record),
            ?TDEF_FE(t_detach_partition_no_partition_record),
            ?TDEF_FE(t_detach_partition_partition_map),
            ?TDEF_FE(t_detach_partition_no_partition_map)
        ]
    }.

t_detach_partition_partition_record(_) ->
    ViewRow1 = #view_row{key = {p, partition, key}},
    ViewRow2 = #view_row{key = key},
    ?assertEqual(ViewRow2, detach_partition(ViewRow1)).

t_detach_partition_no_partition_record(_) ->
    ViewRow = #view_row{key = key},
    ?assertEqual(ViewRow, detach_partition(ViewRow)).

t_detach_partition_partition_map(_) ->
    ViewRow1 = {view_row, #{key => {p, partition, key}}},
    ViewRow2 = {view_row, #{key => key}},
    ?assertEqual(ViewRow2, detach_partition(ViewRow1)).

t_detach_partition_no_partition_map(_) ->
    ViewRow = {view_row, #{key => key}},
    ?assertEqual(ViewRow, detach_partition(ViewRow)).

get_next_row_test_() ->
    {
        foreach,
        fun() -> meck:new([rexi, couch_query_servers]) end,
        fun(_) -> meck:unload() end,
        [
            ?TDEF_FE(t_get_next_row_end),
            ?TDEF_FE(t_get_next_row_map),
            ?TDEF_FE(t_get_next_row_reduce)
        ]
    }.

t_get_next_row_end(_) ->
    State = #collector{rows = []},
    ?assertThrow(complete, get_next_row(State)).

t_get_next_row_map(_) ->
    Rest = [row2, row3],
    Counters1 = [{worker, 8}],
    Counters2 = [{worker, 7}],
    Row1 = #view_row{worker = {worker, from}},
    Row2 = {view_row, #{worker => {worker, from}}},
    State1 = #collector{rows = [Row1 | Rest], counters = Counters1},
    State2 = #collector{rows = [Row2 | Rest], counters = Counters1},
    State3 = #collector{rows = Rest, counters = Counters2},
    meck:expect(rexi, stream_ack, [from], meck:val(ok)),
    ?assertEqual({Row1, State3}, get_next_row(State1)),
    ?assertEqual({Row2, State3}, get_next_row(State2)).

t_get_next_row_reduce(_) ->
    QueryArgs1 = #mrargs{direction = fwd, extra = []},
    QueryArgs2 = #mrargs{direction = fwd, extra = [{view_row_map, true}]},
    KeysRest = [key2, key3],
    Key = key1,
    Keys = [key0, Key | KeysRest],
    W1From = list_to_pid("<0.4.1>"),
    W2From = list_to_pid("<0.4.2>"),
    ViewRows1 = [
        #view_row{value = value1, worker = {worker1, W1From}},
        #view_row{value = value2, worker = {worker2, W2From}},
        #view_row{value = value3, worker = {worker2, W2From}}
    ],
    ViewRows2 = [
        {view_row, #{value => value1, worker => {worker1, W1From}}},
        {view_row, #{value => value2, worker => {worker2, W2From}}},
        {view_row, #{value => value3, worker => {worker2, W2From}}}
    ],
    Values = [[value1], [value2], [value3]],
    BuildBuffer = fun(Pairs) ->
        lists:foldl(fun({K, V}, T) -> gb_trees:insert(K, {K, V}, T) end, gb_trees:empty(), Pairs)
    end,
    Buffer1 = BuildBuffer([{key1, ViewRows1}, {key2, undefined}, {key3, undefined}]),
    Buffer2 = BuildBuffer([{key1, ViewRows2}, {key2, undefined}, {key3, undefined}]),
    Buffer1Rest = gb_trees:delete(key1, Buffer1),
    Buffer2Rest = gb_trees:delete(key1, Buffer2),
    Language = <<"language">>,
    Collation = <<"raw">>,
    Counters1 = [{worker1, 3}, {worker2, 5}],
    Counters2 = [{worker1, 2}, {worker2, 3}],
    State1 = #collector{
        query_args = QueryArgs1,
        keys = Keys,
        rows = Buffer1,
        lang = Language,
        counters = Counters1,
        collation = Collation,
        reducer = reducer
    },
    State2 = #collector{
        query_args = QueryArgs2,
        keys = Keys,
        rows = Buffer2,
        lang = Language,
        counters = Counters1,
        collation = Collation,
        reducer = reducer
    },
    State3 = #collector{
        query_args = QueryArgs1,
        keys = KeysRest,
        rows = Buffer1Rest,
        lang = Language,
        collation = Collation,
        counters = Counters2,
        reducer = reducer
    },
    State4 = #collector{
        query_args = QueryArgs2,
        keys = KeysRest,
        rows = Buffer2Rest,
        lang = Language,
        collation = Collation,
        counters = Counters2,
        reducer = reducer
    },
    Row1 = #view_row{key = Key, id = reduced, value = finalized},
    Row2 = {view_row, #{key => Key, id => reduced, value => finalized}},
    meck:expect(rexi, stream_ack, ['_'], meck:val(ok)),
    meck:expect(
        couch_query_servers, rereduce, [Language, [reducer], Values, '_'], meck:val({ok, [reduced]})
    ),
    meck:expect(couch_query_servers, finalize, [reducer, reduced], meck:val({ok, finalized})),
    ?assertEqual({Row1, State3}, get_next_row(State1)),
    ?assertEqual({Row2, State4}, get_next_row(State2)).

% Build reduce buffer as handle_row may do it, one row at a time
red_buffer(Collation, KeyRows) ->
    lists:foldl(
        fun({Key, Rows}, Buffer) ->
            lists:foldl(
                fun(Row, Acc) -> reduce_buffer_add(Collation, Key, Row, Acc) end,
                Buffer,
                Rows
            )
        end,
        reduce_buffer_new(),
        KeyRows
    ).

reduce_take_empty_test() ->
    Empty = reduce_buffer_new(),
    ?assertThrow(complete, reduce_take(undefined, fwd, <<"raw">>, Empty)),
    ?assertThrow(complete, reduce_take([], fwd, <<"raw">>, Empty)).

reduce_take_min_raw_fwd_test() ->
    Buffer = red_buffer(<<"raw">>, [{3, [a]}, {1, [b]}, {2, [c]}]),
    {Key, Rows, RestKeys, Buffer1} = reduce_take(undefined, fwd, <<"raw">>, Buffer),
    ?assertEqual(1, Key),
    ?assertEqual([b], Rows),
    ?assertEqual(undefined, RestKeys),
    % the next smallest is 2
    ?assertEqual(2, element(1, reduce_take(undefined, fwd, <<"raw">>, Buffer1))).

reduce_take_min_raw_rev_test() ->
    Buffer = red_buffer(<<"raw">>, [{3, [a]}, {1, [b]}, {2, [c]}]),
    ?assertEqual(3, element(1, reduce_take(undefined, rev, <<"raw">>, Buffer))).

% With ICU "a" < "M" < "z". With raw "M" < "a" < "z"
reduce_take_collation_order_test() ->
    Pairs = [{K, [v]} || K <- [<<"z">>, <<"M">>, <<"a">>]],
    IcuMin = element(1, reduce_take(undefined, fwd, <<"icu">>, red_buffer(<<"icu">>, Pairs))),
    RawMin = element(1, reduce_take(undefined, fwd, <<"raw">>, red_buffer(<<"raw">>, Pairs))),
    ?assertEqual(<<"a">>, IcuMin),
    ?assertEqual(<<"M">>, RawMin),
    ?assertNotEqual(IcuMin, RawMin).

% Sorting and taking the head returns th same as our sortkey consturct
reduce_take_matches_sort_test() ->
    Keys = [<<"k5">>, <<"k1">>, <<"k3">>, <<"k2">>, <<"k4">>],
    Pairs = [{K, [v]} || K <- Keys],
    lists:foreach(
        fun(Col) ->
            Buffer = red_buffer(Col, Pairs),
            lists:foreach(
                fun(Dir) ->
                    Cmp = fun(A, B) -> compare(Dir, Col, A, B) end,
                    [Expected | _] = lists:sort(Cmp, Keys),
                    ?assertEqual(
                        Expected, element(1, reduce_take(undefined, Dir, Col, Buffer))
                    )
                end,
                [fwd, rev]
            )
        end,
        [<<"raw">>, <<"icu">>]
    ).

% Check explicit keys (user passes keys=...).
%  - They should emit in that order
%  - If any missing they should be skipped
%  - Empty keys throws `complete`
reduce_take_explicit_keys_test() ->
    Buffer = red_buffer(<<"raw">>, [{<<"a">>, [r1]}, {<<"b">>, [r2]}]),
    {Key, Rows, RestKeys, Buffer1} = reduce_take([<<"a">>, <<"b">>], fwd, <<"raw">>, Buffer),
    ?assertEqual(<<"a">>, Key),
    ?assertEqual([r1], Rows),
    ?assertEqual([<<"b">>], RestKeys),
    ?assertEqual({skip, [<<"b">>]}, reduce_take([<<"x">>, <<"b">>], fwd, <<"raw">>, Buffer)),
    ?assertThrow(complete, reduce_take([], fwd, <<"raw">>, Buffer1)).

% A proper ICU check. "é" under NFC and NFD collate equal but have different
% bytes
reduce_take_collation_grouping_test() ->
    Nfc = <<195, 169>>,
    Nfd = <<101, 204, 129>>,
    ?assertEqual(0, couch_ejson_compare:less(Nfc, Nfd)),
    Buffer = red_buffer(<<"icu">>, [{Nfc, [r1]}, {Nfd, [r2]}, {<<"z">>, [r3]}]),
    {Key, Rows, _, Buffer1} = reduce_take(undefined, fwd, <<"icu">>, Buffer),
    ?assertEqual(Nfc, Key),
    ?assertEqual([r1, r2], lists:sort(Rows)),
    % We should have taken both r1 and r2 and only z should be left
    {Key2, Rows2, _, _} = reduce_take(undefined, fwd, <<"icu">>, Buffer1),
    ?assertEqual(<<"z">>, Key2),
    ?assertEqual([r3], Rows2).

% With ICU and a key that only equals itself
reduce_take_collation_single_test() ->
    Buffer = red_buffer(<<"icu">>, [{<<"a">>, [r1]}, {<<"b">>, [r2]}]),
    {Key, Rows, _, _} = reduce_take(undefined, fwd, <<"icu">>, Buffer),
    ?assertEqual(<<"a">>, Key),
    ?assertEqual([r1], Rows),
    ?assertEqual({skip, []}, reduce_take([<<"zzz">>], fwd, <<"icu">>, Buffer)).

% Partitioned view keys look like {p, Partition, Key}. Our buf_key should handle that.
buf_key_partition_test() ->
    P = <<"foo">>,
    ?assertEqual({p, P, 2}, buf_key(<<"raw">>, {p, P, 2})),
    ?assertEqual({p, P, couch_ejson_compare:sort_key(2)}, buf_key(<<"icu">>, {p, P, 2})),
    lists:foreach(
        fun(Collation) ->
            ?assert(buf_key(Collation, {p, P, 2}) < buf_key(Collation, {p, P, 4})),
            ?assert(buf_key(Collation, {p, P, <<"a">>}) < buf_key(Collation, {p, P, <<"b">>}))
        end,
        [<<"raw">>, <<"icu">>]
    ).

-endif.
