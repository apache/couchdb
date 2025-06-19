-module(fabric_drop_seq).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-export([go/1]).

-export([
    create_peer_checkpoint_doc_if_missing/5,
    update_peer_checkpoint_doc/5,
    cleanup_peer_checkpoint_docs/3,
    peer_checkpoint_doc/4,
    peer_id_from_sig/2
]).

%% rpc
-export([gather_drop_seq_info_rpc/1]).

-type range() :: [non_neg_integer()].

-type uuid() :: binary().

-type seq() :: non_neg_integer().

-type uuid_map() :: #{{Range :: range(), Node :: node()} => uuid()}.

-type peer_checkpoints() :: #{{range(), Node :: node()} => {Uuid :: uuid(), Seq :: seq()}}.

-type history_item() :: {
    SourceUuid :: uuid(), SourceSeq :: seq(), TargetUuid :: uuid(), TargetSeq :: seq()
}.

-type shard_sync_history() :: #{
    {Range :: range(), SourceNode :: node(), TargetNode :: node()} => [history_item()]
}.

-define(START_KEY(SubType), <<?LOCAL_DOC_PREFIX, "peer-checkpoint-", SubType/binary, "-">>).
-define(END_KEY(SubType), <<?LOCAL_DOC_PREFIX, "peer-checkpoint-", SubType/binary, ".">>).

go(DbName) ->
    Shards0 = mem3:shards(DbName),
    case gather_drop_seq_info(Shards0) of
        {error, Reason} ->
            {error, Reason};
        {ok, #{
            uuid_map := UuidMap,
            peer_checkpoints := PeerCheckpoints,
            shard_sync_history := ShardSyncHistory
        }} ->
            Shards1 = fully_replicated_shards_only(Shards0, ShardSyncHistory),
            DropSeqs = calculate_drop_seqs(
                Shards0, UuidMap, PeerCheckpoints, ShardSyncHistory
            ),
            Workers = lists:filtermap(
                fun(Shard) ->
                    #shard{range = Range, node = Node, name = ShardName} = Shard,
                    case maps:find({Range, Node}, DropSeqs) of
                        {ok, {_UuidPrefix, 0}} ->
                            false;
                        {ok, {UuidPrefix, DropSeq}} ->
                            Ref = rexi:cast(
                                Node,
                                {fabric_rpc, set_drop_seq, [
                                    ShardName, UuidPrefix, DropSeq, [?ADMIN_CTX]
                                ]}
                            ),
                            {true, Shard#shard{ref = Ref, opts = [{drop_seq, DropSeq}]}};
                        error ->
                            false
                    end
                end,
                Shards1
            ),
            if
                Workers == [] ->
                    %% nothing to do
                    {ok, #{}};
                true ->
                    RexiMon = fabric_util:create_monitors(Shards1),
                    Acc0 = {#{}, length(Workers) - 1},
                    try
                        case
                            fabric_util:recv(
                                Workers, #shard.ref, fun handle_set_drop_seq_reply/3, Acc0
                            )
                        of
                            {ok, Results} ->
                                {ok, Results};
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
                    end
            end
    end.

-spec calculate_drop_seqs([#shard{}], uuid_map(), peer_checkpoints(), shard_sync_history()) ->
    peer_checkpoints().
calculate_drop_seqs(Shards, UuidMap, PeerCheckpoints0, ShardSyncHistory) ->
    PeerCheckpoints1 = substitute_splits(Shards, UuidMap, PeerCheckpoints0),
    PeerCheckpoints2 = crossref(PeerCheckpoints1, ShardSyncHistory),
    ShardSyncCheckpoints = latest_shard_sync_checkpoints(ShardSyncHistory),
    maps:merge_with(fun merge_peers/3, PeerCheckpoints2, ShardSyncCheckpoints).

handle_set_drop_seq_reply(ok, Worker, {Results0, Waiting}) ->
    DropSeq = proplists:get_value(drop_seq, Worker#shard.opts),
    [B, E] = Worker#shard.range,
    BHex = couch_util:to_hex(<<B:32/integer>>),
    EHex = couch_util:to_hex(<<E:32/integer>>),
    Range = list_to_binary([BHex, "-", EHex]),
    Results1 = maps:merge_with(
        fun(_Key, Val1, Val2) ->
            maps:merge(Val1, Val2)
        end,
        Results0,
        #{Range => #{Worker#shard.node => DropSeq}}
    ),
    if
        Waiting == 0 ->
            {stop, Results1};
        true ->
            {ok, {Results1, Waiting - 1}}
    end;
handle_set_drop_seq_reply(Error, _, _Acc) ->
    {error, Error}.

crossref(PeerCheckpoints0, ShardSyncHistory) ->
    PeerCheckpoints1 = maps:fold(
        fun({Range, Node}, {Uuid, Seq}, Acc1) ->
            Others = maps:filter(
                fun({R, S, _T}, _History) -> R == Range andalso S == Node end, ShardSyncHistory
            ),
            if
                Seq == 0 ->
                    %% propogate any 0 checkpoint as they would not be
                    %% matched in shard sync history.
                    maps:fold(
                        fun({R, _S, T}, _History, Acc2) ->
                            maps:merge_with(fun merge_peers/3, #{{R, T} => {<<>>, 0}}, Acc2)
                        end,
                        Acc1,
                        Others
                    );
                true ->
                    maps:fold(
                        fun({R, _S, T}, History, Acc2) ->
                            case
                                lists:search(
                                    fun({SU, SS, _TU, _TS}) ->
                                        uuids_match([Uuid, SU]) andalso SS =< Seq
                                    end,
                                    History
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
            end
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

%% return only the shards that have synced to by every other replica
fully_replicated_shards_only(Shards, ShardSyncHistory) ->
    lists:filter(
        fun(#shard{range = Range, node = Node}) ->
            ExpectedPeers = [
                S#shard.node
             || S <- Shards, S#shard.range == Range, S#shard.node /= Node
            ],
            ExpectedKeys = [{Range, Peer, Node} || Peer <- ExpectedPeers],
            lists:all(fun(Key) -> maps:is_key(Key, ShardSyncHistory) end, ExpectedKeys)
        end,
        Shards
    ).

-spec gather_drop_seq_info(Shards :: [#shard{}]) ->
    {ok, peer_checkpoints(), shard_sync_history()} | {error, term()}.
gather_drop_seq_info([#shard{} | _] = Shards) ->
    Workers = fabric_util:submit_jobs(
        Shards, ?MODULE, gather_drop_seq_info_rpc, []
    ),
    RexiMon = fabric_util:create_monitors(Workers),
    Acc0 = #{uuid_map => #{}, peer_checkpoints => #{}, shard_sync_history => #{}},
    try
        case
            rexi_utils:recv(
                Workers,
                #shard.ref,
                fun gather_drop_seq_info_cb/3,
                {Acc0, length(Workers) - 1},
                fabric_util:request_timeout(),
                infinity
            )
        of
            {ok, Result} ->
                {ok, Result};
            {timeout, _State} ->
                {error, timeout};
            {error, Reason} ->
                {error, Reason}
        end
    after
        rexi_monitor:stop(RexiMon),
        fabric_streams:cleanup(Workers)
    end.

gather_drop_seq_info_rpc(DbName) ->
    case couch_db:open_int(DbName, []) of
        {ok, Db} ->
            try
                Uuid = couch_db:get_uuid(Db),
                Seq = couch_db:get_committed_update_seq(Db),
                Range = mem3:range(DbName),
                Acc0 = {#{{Range, node()} => {Uuid, Seq}}, #{}},
                {ok, {PeerCheckpoints, ShardSyncHistory}} = couch_db:fold_local_docs(
                    Db, fun gather_drop_seq_info_fun/2, Acc0, []
                ),
                rexi:reply(
                    {ok, #{
                        uuid => Uuid,
                        seq => Seq,
                        peer_checkpoints => PeerCheckpoints,
                        shard_sync_history => ShardSyncHistory
                    }}
                )
            after
                couch_db:close(Db)
            end;
        Else ->
            rexi:reply(Else)
    end.

gather_drop_seq_info_fun(
    #doc{id = <<?LOCAL_DOC_PREFIX, "peer-checkpoint-", _/binary>>} = Doc,
    {PeerCheckpoints0, ShardSyncHistory} = Acc
) ->
    {Props} = Doc#doc.body,
    case couch_util:get_value(<<"update_seq">>, Props) of
        undefined ->
            {ok, Acc};
        UpdateSeq ->
            PeerCheckpoints1 = maps:merge_with(
                fun merge_peers/3, decode_seq(UpdateSeq), PeerCheckpoints0
            ),
            {ok, {PeerCheckpoints1, ShardSyncHistory}}
    end;
gather_drop_seq_info_fun(
    #doc{id = <<?LOCAL_DOC_PREFIX, "shard-sync-", _/binary>>} = Doc,
    {PeerCheckpoints, ShardSyncHistory0} = Acc
) ->
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
            ShardSyncHistory1 = maps:merge(
                maps:groups_from_list(KeyFun, ValueFun, History), ShardSyncHistory0
            ),
            {ok, {PeerCheckpoints, ShardSyncHistory1}}
    end;
gather_drop_seq_info_fun(#doc{}, Acc) ->
    %% ignored
    {ok, Acc}.

gather_drop_seq_info_cb({rexi_DOWN, _, _, _}, _Worker, {Acc, Count}) ->
    {ok, {Acc, Count - 1}};
gather_drop_seq_info_cb({rexi_EXIT, _Reason}, _Worker, {Acc, Count}) ->
    {ok, {Acc, Count - 1}};
gather_drop_seq_info_cb({ok, Info}, Worker, {Acc, Count}) ->
    MergedInfo = merge_info(Worker, Info, Acc),
    if
        Count == 0 ->
            {stop, MergedInfo};
        true ->
            {ok, {MergedInfo, Count - 1}}
    end;
gather_drop_seq_info_cb(_Error, _Worker, {Acc, Count}) ->
    {ok, {Acc, Count - 1}}.

merge_info(#shard{} = Shard, Info, Acc) ->
    #{
        uuid_map =>
            maps:put(
                {Shard#shard.range, Shard#shard.node}, maps:get(uuid, Info), maps:get(uuid_map, Acc)
            ),
        peer_checkpoints => maps:merge_with(
            fun merge_peers/3,
            maps:get(peer_checkpoints, Info),
            maps:get(peer_checkpoints, Acc)
        ),
        shard_sync_history => maps:merge(
            maps:get(shard_sync_history, Info), maps:get(shard_sync_history, Acc)
        )
    }.

merge_peers(_Key, {Uuid1, Val1}, {Uuid2, Val2}) when
    is_binary(Uuid1), is_binary(Uuid2), is_integer(Val1), is_integer(Val2)
->
    true = uuids_match([Uuid1, Uuid2]),
    {Uuid1, min(Val1, Val2)}.

uuids_match(Uuids) when is_list(Uuids) ->
    PrefixLen = lists:min([byte_size(Uuid) || Uuid <- Uuids]),
    binary:longest_common_prefix(Uuids) == PrefixLen.

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
                Seq >= 0,
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

latest_shard_sync_checkpoints(ShardSyncHistory) ->
    maps:fold(
        fun({R, SN, _TN}, History, Acc) ->
            {SU, SS, _TU, _TS} = hd(History),
            maps:merge_with(fun merge_peers/3, #{{R, SN} => {SU, SS}}, Acc)
        end,
        #{},
        ShardSyncHistory
    ).

%% A shard may have been split since a peer saw it.
-spec substitute_splits([#shard{}], uuid_map(), peer_checkpoints()) -> peer_checkpoints().
substitute_splits(Shards, UuidMap, PeerCheckpoints) ->
    maps:fold(
        fun({[PS, PE], Node}, {Uuid, Seq}, Acc) ->
            ShardsInRange = [
                S
             || #shard{range = [SS, SE]} = S <- Shards,
                Node == S#shard.node,
                SS >= PS andalso SE =< PE
            ],
            %% lookup uuid from map if substituted
            AsMap = maps:from_list(
                lists:filtermap(
                    fun(#shard{} = Shard) ->
                        Key = {Shard#shard.range, Shard#shard.node},
                        if
                            [PS, PE] == Shard#shard.range ->
                                {true, {Key, {Uuid, Seq}}};
                            true ->
                                case maps:find(Key, UuidMap) of
                                    {ok, SubstUuid} ->
                                        {true, {Key, {SubstUuid, Seq}}};
                                    error ->
                                        false
                                end
                        end
                    end,
                    ShardsInRange
                )
            ),
            maps:merge_with(fun merge_peers/3, AsMap, Acc)
        end,
        #{},
        PeerCheckpoints
    ).

create_peer_checkpoint_doc_if_missing(
    <<"shards/", _/binary>> = DbName, Subtype, Source, PeerId, UpdateSeq
) when
    is_binary(DbName), is_binary(PeerId), is_integer(UpdateSeq)
->
    create_peer_checkpoint_doc_if_missing(
        DbName, Subtype, Source, PeerId, pack_seq(DbName, UpdateSeq)
    );
create_peer_checkpoint_doc_if_missing(
    DbName, Subtype, Source, PeerId, UpdateSeq
) when
    is_binary(DbName),
    is_binary(Subtype),
    is_binary(Source),
    is_binary(PeerId),
    is_integer(UpdateSeq)
->
    ok;
create_peer_checkpoint_doc_if_missing(
    <<"shards/", _/binary>> = DbName, Subtype, Source, PeerId, UpdateSeq
) when
    is_binary(DbName),
    is_binary(Subtype),
    is_binary(Source),
    is_binary(PeerId),
    is_binary(UpdateSeq)
->
    {_, Ref} = spawn_monitor(fun() ->
        case
            fabric:open_doc(mem3:dbname(DbName), peer_checkpoint_id(Subtype, PeerId), [?ADMIN_CTX])
        of
            {ok, _} ->
                ok;
            {not_found, _} ->
                update_peer_checkpoint_doc(DbName, Subtype, Source, PeerId, UpdateSeq);
            {error, Reason} ->
                throw({checkpoint_commit_failure, Reason})
        end
    end),
    receive
        {'DOWN', Ref, _, _, ok} ->
            ok;
        {'DOWN', Ref, _, _, Else} ->
            Else
    end.

update_peer_checkpoint_doc(
    <<"shards/", _/binary>> = DbName, Subtype, Source, PeerId, UpdateSeq
) when
    is_binary(DbName),
    is_binary(Subtype),
    is_binary(Source),
    is_binary(PeerId),
    is_integer(UpdateSeq)
->
    update_peer_checkpoint_doc(DbName, Subtype, Source, PeerId, pack_seq(DbName, UpdateSeq));
update_peer_checkpoint_doc(
    DbName, Subtype, Source, PeerId, UpdateSeq
) when
    is_binary(DbName),
    is_binary(Subtype),
    is_binary(Source),
    is_binary(PeerId),
    is_integer(UpdateSeq)
->
    ok;
update_peer_checkpoint_doc(
    <<"shards/", _/binary>> = DbName, Subtype, Source, PeerId, UpdateSeq
) when
    is_binary(DbName),
    is_binary(Subtype),
    is_binary(Source),
    is_binary(PeerId),
    is_binary(UpdateSeq)
->
    {_, OpenRef} = spawn_monitor(fun() ->
        case
            fabric:open_doc(mem3:dbname(DbName), peer_checkpoint_id(Subtype, PeerId), [?ADMIN_CTX])
        of
            {ok, ExistingDoc} ->
                exit(ExistingDoc#doc.revs);
            {not_found, _Reason} ->
                exit({0, []});
            {error, Reason} ->
                throw({checkpoint_fetch_failure, Reason})
        end
    end),
    receive
        {'DOWN', OpenRef, _, _, Revs} ->
            NewDoc0 = peer_checkpoint_doc(PeerId, Subtype, Source, UpdateSeq),
            NewDoc1 = NewDoc0#doc{revs = Revs},
            {_, UpdateRef} = spawn_monitor(fun() ->
                case fabric:update_doc(mem3:dbname(DbName), NewDoc1, [?ADMIN_CTX]) of
                    {ok, _} ->
                        couch_log:notice(
                            "updated peer checkpoint for db:~s, subtype:~s, source:~s, peer_id:~s, update_seq:~s",
                            [
                                DbName, Subtype, Source, PeerId, UpdateSeq
                            ]
                        ),
                        ok;
                    {error, Reason} ->
                        throw({checkpoint_commit_failure, Reason})
                end
            end),
            receive
                {'DOWN', UpdateRef, _, _, ok} ->
                    ok;
                {'DOWN', UpdateRef, _, _, Else} ->
                    Else
            end;
        {'DOWN', OpenRef, _, _, not_found} ->
            ok;
        {'DOWN', OpenRef, _, _, Else} ->
            Else
    end.

peer_checkpoint_doc(PeerId, Subtype, Source, UpdateSeq) when
    is_binary(PeerId), is_binary(Subtype), is_binary(Source), is_binary(UpdateSeq)
->
    #doc{
        id = peer_checkpoint_id(Subtype, PeerId),
        body =
            {[
                {<<"type">>, <<"peer-checkpoint">>},
                {<<"subtype">>, Subtype},
                {<<"source">>, Source},
                {<<"update_seq">>, UpdateSeq},
                {<<"last_updated">>, ?l2b(couch_log_util:iso8601_timestamp())}
            ]}
    }.

peer_checkpoint_id(Subtype, PeerId) ->
    <<?LOCAL_DOC_PREFIX, "peer-checkpoint-", Subtype/binary, "-", PeerId/binary>>.

peer_id_from_sig(DbName, Sig) when is_binary(DbName), is_binary(Sig) ->
    Hash = couch_util:encodeBase64Url(
        crypto:hash(sha256, [atom_to_binary(node()), $0, DbName])
    ),
    <<Sig/binary, "$", Hash/binary>>.

pack_seq(DbName, UpdateSeq) ->
    PrefixLen = fabric_util:get_uuid_prefix_len(),
    DbUuid = couch_util:with_db(DbName, fun(Db) -> couch_db:get_uuid(Db) end),
    fabric_view_changes:pack_seqs(
        [
            {
                #shard{node = node(), range = mem3:range(DbName)},
                {UpdateSeq, binary:part(DbUuid, {0, PrefixLen}), node()}
            }
        ]
    ).

cleanup_peer_checkpoint_docs(DbName, SubType, KeepSigs) when
    is_binary(DbName), is_binary(SubType), is_list(KeepSigs)
->
    MrArgs = #mrargs{
        view_type = map,
        include_docs = true,
        start_key = ?START_KEY(SubType),
        end_key = ?END_KEY(SubType),
        inclusive_end = false,
        extra = [
            {include_system, true},
            {namespace, <<"_local">>}
        ]
    },
    {ok, {SubType, KeepSigs, DocsToDelete}} = fabric:all_docs(
        DbName, fun cleanup_peer_checkpoints_cb/2, {SubType, KeepSigs, []}, MrArgs
    ),
    {ok, _} = fabric:update_docs(DbName, DocsToDelete, [?ADMIN_CTX]).

cleanup_peer_checkpoints_cb({row, Row}, {SubType, KeepSigs, DocsToDelete} = Acc) ->
    {doc, JsonDoc} = lists:keyfind(doc, 1, Row),
    Doc = couch_doc:from_json_obj(JsonDoc),
    #doc{
        id =
            <<?LOCAL_DOC_PREFIX, "peer-checkpoint-", SubType:(byte_size(SubType))/binary, "-",
                SigHash/binary>>
    } = Doc,
    [Sig, _Hash] = binary:split(SigHash, <<"$">>),
    case lists:member(Sig, KeepSigs) of
        true ->
            {ok, Acc};
        false ->
            {ok, {SubType, KeepSigs, [Doc#doc{deleted = true, body = {[]}} | DocsToDelete]}}
    end;
cleanup_peer_checkpoints_cb(_Else, Acc) ->
    {ok, Acc}.

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

empty_sync_history_means_no_change_test() ->
    Range = [0, 10],
    Node1 = 'node1@127.0.0.1',
    Shards = [#shard{range = Range, node = Node1}],
    UuidMap = #{},
    PeerCheckpoints = #{{Range, Node1} => {<<"uuid1">>, 12}},
    ShardSyncHistory = #{},
    ?assertEqual(
        PeerCheckpoints,
        calculate_drop_seqs(
            Shards,
            UuidMap,
            PeerCheckpoints,
            ShardSyncHistory
        )
    ).

no_peer_checkpoints_mean_latest_shard_checkpoint_wins_test() ->
    Range = [0, 10],
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    Shards = [#shard{range = Range, node = Node1}, #shard{range = Range, node = Node2}],
    UuidMap = #{},
    PeerCheckpoints = #{},
    ShardSyncHistory = #{{Range, Node1, Node2} => [{<<"uuid1">>, 12, <<"uuid2">>, 5}]},
    ?assertEqual(
        #{{Range, Node1} => {<<"uuid1">>, 12}},
        calculate_drop_seqs(
            Shards,
            UuidMap,
            PeerCheckpoints,
            ShardSyncHistory
        )
    ).

matching_sync_history_expands_result_test() ->
    Range = [0, 10],
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    Shards = [#shard{range = Range, node = Node1}, #shard{range = Range, node = Node2}],
    UuidMap = #{},
    PeerCheckpoints = #{{Range, Node1} => {<<"uuid1">>, 12}},
    ShardSyncHistory = #{{Range, Node1, Node2} => [{<<"uuid1">>, 12, <<"uuid2">>, 5}]},
    ?assertEqual(
        #{
            {Range, Node1} => {<<"uuid1">>, 12},
            {Range, Node2} => {<<"uuid2">>, 5}
        },
        calculate_drop_seqs(
            Shards,
            UuidMap,
            PeerCheckpoints,
            ShardSyncHistory
        )
    ).

transitive_sync_history_expands_result_test() ->
    Range = [0, 10],
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    Node3 = 'node3@127.0.0.1',
    Shards = [
        #shard{range = Range, node = Node1},
        #shard{range = Range, node = Node2},
        #shard{range = Range, node = Node3}
    ],
    UuidMap = #{},
    PeerCheckpoints = #{{Range, Node1} => {<<"uuid1">>, 12}},
    ShardSyncHistory = #{
        {Range, Node1, Node2} => [{<<"uuid1">>, 12, <<"uuid2">>, 11}],
        {Range, Node2, Node3} => [{<<"uuid2">>, 11, <<"uuid3">>, 10}]
    },
    ?assertEqual(
        #{
            {Range, Node1} => {<<"uuid1">>, 12},
            {Range, Node2} => {<<"uuid2">>, 11},
            {Range, Node3} => {<<"uuid3">>, 10}
        },
        calculate_drop_seqs(
            Shards,
            UuidMap,
            PeerCheckpoints,
            ShardSyncHistory
        )
    ).

shard_sync_history_caps_peer_checkpoint_test() ->
    Range = [0, 10],
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    Shards = [#shard{range = Range, node = Node1}, #shard{range = Range, node = Node2}],
    UuidMap = #{},
    PeerCheckpoints = #{{Range, Node1} => {<<"uuid1">>, 12}},
    ShardSyncHistory = #{{Range, Node1, Node2} => [{<<"uuid1">>, 10, <<"uuid2">>, 5}]},
    ?assertEqual(
        #{
            {Range, Node1} => {<<"uuid1">>, 10},
            {Range, Node2} => {<<"uuid2">>, 5}
        },
        calculate_drop_seqs(
            Shards,
            UuidMap,
            PeerCheckpoints,
            ShardSyncHistory
        )
    ).

multiple_range_test() ->
    Range1 = [0, 10],
    Range2 = [11, 20],
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    Shards = [
        #shard{range = Range1, node = Node1},
        #shard{range = Range1, node = Node2},
        #shard{range = Range2, node = Node1},
        #shard{range = Range2, node = Node2}
    ],
    UuidMap = #{},
    PeerCheckpoints = #{{Range1, Node1} => {<<"r1n1">>, 12}, {Range2, Node2} => {<<"r2n2">>, 20}},
    ShardSyncHistory = #{
        {Range1, Node1, Node2} => [{<<"r1n1">>, 10, <<"r1n2">>, 5}],
        {Range2, Node2, Node1} => [{<<"r2n2">>, 19, <<"r2n1">>, 17}]
    },
    ?assertEqual(
        #{
            {Range1, Node1} => {<<"r1n1">>, 10},
            {Range1, Node2} => {<<"r1n2">>, 5},
            {Range2, Node2} => {<<"r2n2">>, 19},
            {Range2, Node1} => {<<"r2n1">>, 17}
        },
        calculate_drop_seqs(
            Shards,
            UuidMap,
            PeerCheckpoints,
            ShardSyncHistory
        )
    ).

search_history_for_latest_safe_crossover_test() ->
    Range = [0, 10],
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    Shards = [#shard{range = Range, node = Node1}, #shard{range = Range, node = Node2}],
    UuidMap = #{},
    PeerCheckpoints = #{{Range, Node1} => {<<"uuid1">>, 50}},
    ShardSyncHistory = #{
        {Range, Node1, Node2} => [
            {<<"uuid1">>, 100, <<"uuid2">>, 99},
            {<<"uuid1">>, 75, <<"uuid2">>, 76},
            {<<"uuid1">>, 50, <<"uuid2">>, 51},
            {<<"uuid1">>, 40, <<"uuid2">>, 41}
        ]
    },
    ?assertEqual(
        #{
            {Range, Node1} => {<<"uuid1">>, 50},
            {Range, Node2} => {<<"uuid2">>, 51}
        },
        calculate_drop_seqs(Shards, UuidMap, PeerCheckpoints, ShardSyncHistory)
    ).

fully_replicated_shards_only_test_() ->
    Range1 = [0, 1],
    Range2 = [1, 2],
    Shards = [
        #shard{node = node1, range = Range1},
        #shard{node = node2, range = Range1},
        #shard{node = node3, range = Range1},
        #shard{node = node1, range = Range2},
        #shard{node = node2, range = Range2}
    ],
    [
        %% n=1 edge case
        ?_assertEqual(
            [hd(Shards)],
            fully_replicated_shards_only([hd(Shards)], #{})
        ),

        %% empty history means no fully replicated shards
        ?_assertEqual([], fully_replicated_shards_only(Shards, #{})),
        %% some but not all peers
        ?_assertEqual(
            [],
            fully_replicated_shards_only(Shards, #{
                {Range1, node2, node1} => {0, <<>>}
            })
        ),
        %% all peers of one replica
        ?_assertEqual(
            [#shard{node = node1, range = Range1}],
            fully_replicated_shards_only(Shards, #{
                {Range1, node2, node1} => {0, <<>>},
                {Range1, node3, node1} => {0, <<>>}
            })
        ),
        %% all peers of one range
        ?_assertEqual(
            [
                #shard{node = node1, range = Range1},
                #shard{node = node2, range = Range1},
                #shard{node = node3, range = Range1}
            ],
            fully_replicated_shards_only(Shards, #{
                {Range1, node2, node1} => {0, <<>>},
                {Range1, node3, node1} => {0, <<>>},
                {Range1, node1, node2} => {0, <<>>},
                {Range1, node3, node2} => {0, <<>>},
                {Range1, node1, node3} => {0, <<>>},
                {Range1, node2, node3} => {0, <<>>}
            })
        )
    ].

substitute_splits_test_() ->
    Range = [0, 10],
    Node1 = 'node1@127.0.0.1',
    UuidMap = #{
        {[0, 5], Node1} => <<"uuid2">>,
        {[6, 10], Node1} => <<"uuid3">>,
        {[0, 3], Node1} => <<"uuid4">>,
        {[4, 5], Node1} => <<"uuid5">>
    },
    PeerCheckpoints = #{{Range, Node1} => {<<"uuid1">>, 12}},

    [
        %% preserve peer checkpoint if no subs
        ?_assertEqual(
            #{{[0, 5], Node1} => {<<"uuid2">>, 12}, {[6, 10], Node1} => {<<"uuid3">>, 12}},
            substitute_splits(
                [#shard{range = [0, 5], node = Node1}, #shard{range = [6, 10], node = Node1}],
                UuidMap,
                #{{[0, 5], Node1} => {<<"uuid2">>, 12}, {[6, 10], Node1} => {<<"uuid3">>, 12}}
            )
        ),
        ?_assertEqual(
            #{{[0, 5], Node1} => {<<"uuid2">>, 12}, {[6, 10], Node1} => {<<"uuid3">>, 12}},
            substitute_splits(
                [#shard{range = [0, 5], node = Node1}, #shard{range = [6, 10], node = Node1}],
                UuidMap,
                PeerCheckpoints
            )
        ),
        ?_assertEqual(
            #{
                {[0, 3], Node1} => {<<"uuid4">>, 12},
                {[4, 5], Node1} => {<<"uuid5">>, 12},
                {[6, 10], Node1} => {<<"uuid3">>, 12}
            },
            substitute_splits(
                [
                    #shard{range = [0, 3], node = Node1},
                    #shard{range = [4, 5], node = Node1},
                    #shard{range = [6, 10], node = Node1}
                ],
                UuidMap,
                PeerCheckpoints
            )
        )
    ].

crossref_test_() ->
    Range = [0, 10],
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    Node3 = 'node3@127.0.0.1',
    [
        ?_assertEqual(#{}, crossref(#{}, #{})),
        ?_assertEqual(
            #{
                {Range, Node1} => {<<"n1">>, 5},
                {Range, Node2} => {<<"n2">>, 4},
                {Range, Node3} => {<<"n3">>, 3}
            },
            crossref(
                #{{Range, Node1} => {<<"n1">>, 5}},
                #{
                    {Range, Node1, Node2} => [
                        {<<"n1">>, 10, <<"n2">>, 9},
                        {<<"n1">>, 5, <<"n2">>, 4},
                        {<<"n1">>, 2, <<"n2">>, 1}
                    ],
                    {Range, Node1, Node3} => [
                        {<<"n1">>, 9, <<"n3">>, 8},
                        {<<"n1">>, 4, <<"n3">>, 3},
                        {<<"n1">>, 3, <<"n3">>, 2}
                    ]
                }
            )
        ),
        ?_assertEqual(
            #{
                {Range, Node1} => {<<"n1">>, 5},
                {Range, Node2} => {<<"n2">>, 4},
                {Range, Node3} => {<<"n3">>, 3}
            },
            crossref(
                #{{Range, Node1} => {<<"n1">>, 5}},
                #{
                    {Range, Node1, Node2} => [
                        {<<"n1">>, 10, <<"n2">>, 9},
                        {<<"n1">>, 5, <<"n2">>, 4},
                        {<<"n1">>, 2, <<"n2">>, 1}
                    ],
                    {Range, Node2, Node3} => [
                        {<<"n2">>, 9, <<"n3">>, 8},
                        {<<"n2">>, 4, <<"n3">>, 3},
                        {<<"n2">>, 3, <<"n3">>, 2}
                    ]
                }
            )
        )
    ].

calculate_drop_seqs_test_() ->
    Range = [0, 10],
    Subrange1 = [0, 5],
    Subrange2 = [6, 10],
    Node1 = 'node1@127.0.0.1',
    Shards = [#shard{range = Subrange1, node = Node1}, #shard{range = Subrange2, node = Node1}],
    UuidMap = #{
        {Subrange1, Node1} => <<"uuid2">>,
        {Subrange2, Node1} => <<"uuid3">>
    },
    [
        ?_assertEqual(
            #{
                {Subrange1, Node1} => {<<"uuid2">>, 12}, {Subrange2, Node1} => {<<"uuid3">>, 12}
            },
            calculate_drop_seqs(Shards, UuidMap, #{{Range, Node1} => {<<"uuid1">>, 12}}, #{})
        ),
        ?_assertEqual(
            #{
                {Subrange1, Node1} => {<<"uuid2">>, 10}, {Subrange2, Node1} => {<<"uuid3">>, 12}
            },
            calculate_drop_seqs(
                Shards,
                UuidMap,
                #{{Range, Node1} => {<<"uuid1">>, 12}, {Subrange1, Node1} => {<<"uuid2">>, 10}},
                #{}
            )
        )
    ].

calculate_drop_seqs_split_test_() ->
    Range = [0, 10],
    Subrange1 = [0, 5],
    Subrange2 = [6, 10],
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    Shards = [
        #shard{range = Subrange1, node = Node1},
        #shard{range = Subrange2, node = Node1},
        #shard{range = Subrange1, node = Node2},
        #shard{range = Subrange2, node = Node2}
    ],
    UuidMap = #{
        {Subrange1, Node1} => <<"s1n1">>,
        {Subrange2, Node1} => <<"s2n1">>,
        {Subrange1, Node2} => <<"s1n2">>,
        {Subrange2, Node2} => <<"s2n2">>
    },
    ShardSyncHistory =
        #{
            {Subrange1, Node1, Node2} => [
                {<<"s1n1">>, 100, <<"s1n2">>, 99},
                {<<"s1n1">>, 75, <<"s1n2">>, 76},
                {<<"s1n1">>, 50, <<"s1n2">>, 51},
                {<<"s1n1">>, 12, <<"s1n2">>, 11}
            ],
            {Subrange1, Node2, Node1} => [
                {<<"s1n2">>, 101, <<"s1n1">>, 99},
                {<<"s1n2">>, 75, <<"s1n1">>, 76},
                {<<"s1n2">>, 50, <<"s1n1">>, 51},
                {<<"s1n2">>, 12, <<"s1n1">>, 11}
            ],
            {Subrange2, Node1, Node2} => [
                {<<"s2n1">>, 102, <<"s2n2">>, 99},
                {<<"s2n1">>, 75, <<"s2n2">>, 76},
                {<<"s2n1">>, 50, <<"s2n2">>, 51},
                {<<"s2n1">>, 12, <<"s2n2">>, 11}
            ],
            {Subrange2, Node2, Node1} => [
                {<<"s2n2">>, 103, <<"s2n1">>, 99},
                {<<"s2n2">>, 75, <<"s2n1">>, 76},
                {<<"s2n2">>, 50, <<"s2n1">>, 51},
                {<<"s2n2">>, 12, <<"s2n1">>, 11}
            ]
        },
    [
        ?_assertEqual(
            #{
                {Subrange1, Node1} => {<<"s1n1">>, 12},
                {Subrange1, Node2} => {<<"s1n2">>, 11},
                {Subrange2, Node1} => {<<"s2n1">>, 12},
                {Subrange2, Node2} => {<<"s2n2">>, 11}
            },
            calculate_drop_seqs(
                Shards,
                UuidMap,
                #{{Range, Node1} => {<<"ignored">>, 12}},
                ShardSyncHistory
            )
        )
    ].

-endif.
