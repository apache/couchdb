-module(fabric_drop_seq).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-export([go/1]).

-type range() :: [non_neg_integer()].

-type uuid() :: binary().

-type seq() :: non_neg_integer().

-type peer_checkpoints() :: #{{range(), Node :: node()} => {Uuid :: uuid(), Seq :: seq()}}.

-type history_item() :: {
    SourceUuid :: uuid(), SourceSeq :: seq(), TargetUuid :: uuid(), TargetSeq :: seq()
}.

-type shard_sync_history() :: #{
    {Range :: range(), SourceNode :: node(), TargetNode :: node()} => [history_item()]
}.

go(DbName) ->
    {PeerCheckpoints0, ShardSyncHistory} = parse_local_docs(DbName),
    ShardSyncCheckpoints = latest_shard_sync_checkpoints(ShardSyncHistory),
    PeerCheckpoints1 = maps:merge_with(fun merge_peers/3, PeerCheckpoints0, ShardSyncCheckpoints),
    PeerCheckpoints2 = crossref(PeerCheckpoints1, ShardSyncHistory),
    Shards = mem3:live_shards(DbName, [node() | nodes()]),
    RexiMon = fabric_util:create_monitors(Shards),
    Workers = lists:filtermap(
        fun(Shard) ->
            #shard{range = Range, node = Node, name = ShardName} = Shard,
            case maps:find({Range, Node}, PeerCheckpoints2) of
                {ok, {UuidPrefix, DropSeq}} ->
                    Ref = rexi:cast(
                        Node,
                        {fabric_rpc, set_drop_seq, [ShardName, UuidPrefix, DropSeq, [?ADMIN_CTX]]}
                    ),
                    {true, Shard#shard{ref = Ref}};
                error ->
                    false
            end
        end,
        Shards
    ),
    Acc0 = {Workers, length(Workers) - 1},
    try
        case fabric_util:recv(Workers, #shard.ref, fun handle_message/3, Acc0) of
            {ok, ok} ->
                ok;
            {timeout, {WorkersDict, _}} ->
                DefunctWorkers = fabric_util:remove_done_workers(
                    WorkersDict,
                    nil
                ),
                fabric_util:log_timeout(
                    DefunctWorkers,
                    "set_drop_seq"
                ),
                {error, timeout};
            {error, Reason} ->
                {error, Reason}
        end
    after
        rexi_monitor:stop(RexiMon)
    end.

handle_message(ok, _Worker, {_Workers, 0}) ->
    {stop, ok};
handle_message(ok, Worker, {Workers, Waiting}) ->
    {ok, {lists:delete(Worker, Workers), Waiting - 1}};
handle_message(Error, _, _Acc) ->
    {error, Error}.

crossref(PeerCheckpoints0, ShardSyncHistory) ->
    PeerCheckpoints1 = maps:fold(
        fun({Range, Node}, {Uuid, Seq}, Acc1) ->
            Others = maps:filter(
                fun({R, _S, T}, _V) -> R == Range andalso T /= Node end, ShardSyncHistory
            ),
            maps:fold(
                fun({R, _S, T}, H, Acc2) ->
                    case
                        lists:search(
                            fun({SU, SS, _TU, _TS}) -> Uuid == SU andalso SS =< Seq end,
                            H
                        )
                    of
                        {value, {_SU, _SS, TU, TS}} ->
                            maps:merge_with(fun merge_peers/3, #{{R, T} => {TU, TS}}, Acc2);
                        false ->
                            Acc2
                    end
                end,
                Acc1,
                Others
            )
        end,
        PeerCheckpoints0,
        PeerCheckpoints0
    ),

    %% mem3 sync is not hub-spoke, each iteration of crossref will add
    %% some crossreferences. we call it again if the map changes as new
    %% crossreferences may be possible.
    if
        PeerCheckpoints0 == PeerCheckpoints1 ->
            PeerCheckpoints1;
        true ->
            crossref(PeerCheckpoints1, ShardSyncHistory)
    end.

-spec parse_local_docs(DbName :: binary()) -> {peer_checkpoints(), shard_sync_history()}.
parse_local_docs(DbName) ->
    {ok, Result} = fabric:all_docs(
        DbName, fun parse_local_docs_cb/2, {#{}, #{}}, all_docs_mrargs()
    ),
    Result.

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
            {PeerCheckpoints, ShardSyncHistory0} = Acc,
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

merge_peers(_Key, {Uuid1, Val1}, {Uuid2, Val2}) when is_integer(Val1), is_integer(Val2) ->
    PrefixLen = min(byte_size(Uuid1), byte_size(Uuid2)),
    case binary:longest_common_prefix([Uuid1, Uuid2]) == PrefixLen of
        true ->
            {Uuid1, min(Val1, Val2)};
        false ->
            {Uuid2, Val2}
    end.

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

latest_shard_sync_checkpoints(ShardSyncHistory) ->
    maps:fold(
        fun({R, SN, _TN}, History, Acc) ->
            {SU, SS, _TU, _TS} = hd(History),
            maps:merge_with(fun merge_peers/3, #{{R, SN} => {SU, SS}}, Acc)
        end,
        #{},
        ShardSyncHistory
    ).
