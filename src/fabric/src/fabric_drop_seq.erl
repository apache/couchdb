-module(fabric_drop_seq2).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-export([go/1]).

go(DbName) ->
    Shards = mem3:shards(DbName),
    RangeToNodes = range_to_nodes(Shards),

    {ok, {PeerCheckpoints, ShardSyncHistory}} = parse_local_docs(DbName),

    Expanded = maps:fold( fun({Range, SrcNode}, {Uuid, Seq}, Acc1) ->
        OtherNodes = maps:get(Range, RangeToNodes, []) -- [SrcNode],
        lists:foldl(fun(TgtNode, Acc2) ->
            History = maps:get({Range, SrcNode, TgtNode}, ShardSyncHistory, []),
            case
                lists:search(
                    fun({SourceUuid, SourceSeq, _TargetUuid, _TargetSeq}) ->
                        Uuid == SourceUuid andalso SourceSeq =< Seq
                    end,
                    History
                )
            of
                {value, {_SourceUuid, _SourceSeq, TargetUuid, TargetSeq}} ->
                    Acc2#{{Range, TgtNode} => {TargetUuid, TargetSeq}};
                false ->
                    Acc2
            end
        end, Acc1, OtherNodes)
    end, PeerCheckpoints, PeerCheckpoints),

    Workers = lists:map(
        fun(#shard{} = Shard) ->
            {Shard, maps:get({Shard#shard.range, Shard#shard.node}, Expanded)}
        end,
        Shards
    ),

    Workers.

parse_local_docs(DbName) ->
    fabric:all_docs(DbName, fun parse_local_docs_cb/2, {#{}, #{}}, all_docs_mrargs()).

parse_local_docs_cb({row, Row}, Acc) ->
    case lists:keyfind(doc, 1, Row) of
        false ->
            {ok, Acc};
        {doc, Doc} ->
            parse_local_doc(couch_doc:from_json_obj(Doc), Acc)
    end;
parse_local_docs_cb(_Else, Acc) ->
    {ok, Acc}.

parse_local_doc(#doc{id = <<"_local/shard-sync-", _/binary>>} = Doc, Acc) ->
    {Props} = Doc#doc.body,
    case couch_util:get_value(<<"dbname">>, Props) of
        undefined ->
            %% not yet upgraded with new property
            {ok, Acc};
        DbName ->
            Range = mem3:range(DbName),
            {[{_SrcNode, History}]} = couch_util:get_value(<<"history">>, Props),
            KeyFun = fun({Item}) ->
                {Range, binary_to_existing_atom(couch_util:get_value(<<"source_node">>, Item)),
                    binary_to_existing_atom(couch_util:get_value(<<"target_node">>, Item))}
            end,
            ValueFun = fun({Item}) ->
                {
                    couch_util:get_value(<<"source_uuid">>, Item),
                    couch_util:get_value(<<"source_seq">>, Item),
                    couch_util:get_value(<<"target_uuid">>, Item),
                    couch_util:get_value(<<"target_seq">>, Item)
                }
            end,
            {PeerCheckpoints, ShardSyncHistory0} = Acc,
            ShardSyncHistory1 = maps:merge(
                maps:groups_from_list(KeyFun, ValueFun, History), ShardSyncHistory0
            ),
            {ok, {PeerCheckpoints, ShardSyncHistory1}}
    end;
parse_local_doc(#doc{id = <<"_local/peer-checkpoint-", _/binary>>} = Doc, Acc) ->
    {Props} = Doc#doc.body,
    case couch_util:get_value(<<"update_seq">>, Props) of
        undefined ->
            {ok, Acc};
        UpdateSeq ->
            {PeerCheckpoints0, ShardSyncHistory} = Acc,
            PeerCheckpoints1 = maps:merge_with(
                fun merge_peers/3, decode_seq(UpdateSeq), PeerCheckpoints0
            ),
            {ok, {PeerCheckpoints1, ShardSyncHistory}}
    end;
parse_local_doc(_Doc, Acc) ->
    {ok, Acc}.

range_to_nodes(Shards) ->
    maps:groups_from_list(fun range/1, fun node/1, Shards).

range(Shard) ->
    Shard#shard.range.

node(Shard) ->
    Shard#shard.node.

merge_peers(_Key, {Uuid, Val1}, {Uuid, Val2}) when is_integer(Val1), is_integer(Val2) ->
    {Uuid, min(Val1, Val2)}.

decode_seq(OpaqueSeq) ->
    Decoded = fabric_view_changes:decode_seq(OpaqueSeq),
    lists:foldl(
        fun
            ({_Node, [S, E], {Seq, Uuid, Node}}, Acc) when
                is_integer(S),
                is_integer(E),
                is_integer(Seq),
                S >= 0,
                E > S,
                Seq > 0,
                is_binary(Uuid),
                is_atom(Node)
            ->
                Acc#{{[S, E], Node} => {Uuid, Seq}};
            (_Else, Acc) ->
                Acc
        end,
        #{},
        Decoded
    ).

all_docs_mrargs() ->
    #mrargs{
        view_type = map,
        include_docs = true,
        extra = [
            {include_system, true},
            {namespace, <<"_local">>}
        ]
    }.
