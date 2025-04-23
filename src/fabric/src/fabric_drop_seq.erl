-module(fabric_drop_seq).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric.hrl").

-export([go/1]).

-export([
    create_peer_checkpoint_doc_if_missing/5,
    update_peer_checkpoint_doc/5,
    peer_checkpoint_doc/4
]).

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
    {ok, PeerCheckpoints0} = get_peer_checkpoint_docs(DbName),
    {ok, ShardSyncHistory} = get_all_shard_sync_docs(DbName),
    PeerCheckpoints1 = calculate_drop_seqs(PeerCheckpoints0, ShardSyncHistory),
    Shards = mem3:live_shards(DbName, [node() | nodes()]),
    Workers = lists:filtermap(
        fun(Shard) ->
            #shard{range = Range, node = Node, name = ShardName} = Shard,
            case maps:find({Range, Node}, PeerCheckpoints1) of
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
    if
        Workers == [] ->
            %% nothing to do
            ok;
        true ->
            RexiMon = fabric_util:create_monitors(Shards),
            Acc0 = {Workers, length(Workers) - 1},
            try
                case fabric_util:recv(Workers, #shard.ref, fun handle_set_drop_seq_reply/3, Acc0) of
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
            end
    end.

-spec calculate_drop_seqs(peer_checkpoints(), shard_sync_history()) -> peer_checkpoints().
calculate_drop_seqs(PeerCheckpoints0, ShardSyncHistory) ->
    ShardSyncCheckpoints = latest_shard_sync_checkpoints(ShardSyncHistory),
    PeerCheckpoints1 = maps:merge_with(fun merge_peers/3, PeerCheckpoints0, ShardSyncCheckpoints),
    crossref(PeerCheckpoints1, ShardSyncHistory).

handle_set_drop_seq_reply(ok, _Worker, {_Workers, 0}) ->
    {stop, ok};
handle_set_drop_seq_reply(ok, Worker, {Workers, Waiting}) ->
    {ok, {lists:delete(Worker, Workers), Waiting - 1}};
handle_set_drop_seq_reply(Error, _, _Acc) ->
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

-spec get_all_shard_sync_docs(DbName :: binary()) -> shard_sync_history().
get_all_shard_sync_docs(DbName) ->
    Shards = mem3:shards(DbName),
    Workers = fabric_util:submit_jobs(
        Shards, fabric_rpc, all_docs, [[], shard_sync_docs_mrargs()]
    ),
    Acc0 = {#{}, length(Workers) - 1},
    RexiMon = fabric_util:create_monitors(Workers),
    try
        rexi_utils:recv(
            Workers,
            #shard.ref,
            fun handle_shard_sync_docs_reply/3,
            Acc0,
            fabric_util:request_timeout(),
            infinity
        )
    after
        rexi_monitor:stop(RexiMon),
        fabric_streams:cleanup(Workers)
    end.

%% consult every copy of every range for shard sync information but ignore failures (otherwise
%% this only works when all nodes are up). We'll only update drop seq for a shard if we have
%% seen all other copies have synced to it.
handle_shard_sync_docs_reply({rexi_DOWN, _, _, _}, _Worker, {ShardSyncHistory, Count}) ->
    {ok, {ShardSyncHistory, Count - 1}};
handle_shard_sync_docs_reply({rexi_EXIT, _Reason}, _Worker, {ShardSyncHistory, Count}) ->
    {ok, {ShardSyncHistory, Count - 1}};
handle_shard_sync_docs_reply(rexi_STREAM_INIT, {_Worker, From}, Acc) ->
    gen_server:reply(From, rexi_STREAM_START),
    {ok, Acc};
handle_shard_sync_docs_reply({meta, _Meta}, _Worker, Acc) ->
    {ok, Acc};
handle_shard_sync_docs_reply(#view_row{} = Row, _Worker, {ShardSyncHistory, Count}) ->
    Doc = couch_doc:from_json_obj(Row#view_row.doc),
    {ok, {parse_shard_sync_doc(Doc, ShardSyncHistory), Count}};
handle_shard_sync_docs_reply(complete, _Worker, {ShardSyncHistory, 0}) ->
    {stop, ShardSyncHistory};
handle_shard_sync_docs_reply(complete, _Worker, {ShardSyncHistory, Count}) ->
    {ok, {ShardSyncHistory, Count - 1}}.

parse_shard_sync_doc(#doc{id = <<"_local/shard-sync-", _/binary>>} = Doc, Acc) ->
    {Props} = Doc#doc.body,
    case couch_util:get_value(<<"dbname">>, Props) of
        undefined ->
            %% not yet upgraded with new property
            Acc;
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
            maps:merge(
                maps:groups_from_list(KeyFun, ValueFun, History), Acc
            )
    end.

-spec get_peer_checkpoint_docs(DbName :: binary()) -> peer_checkpoints().
get_peer_checkpoint_docs(DbName) ->
    fabric:all_docs(
        DbName, fun parse_peer_checkpoint_docs_cb/2, #{}, peer_checkpoint_docs_mrargs()
    ).

parse_peer_checkpoint_docs_cb({row, Row}, PeerCheckpoints0) ->
    case lists:keyfind(doc, 1, Row) of
        false ->
            {ok, PeerCheckpoints0};
        {doc, Doc0} ->
            #doc{id = <<"_local/peer-checkpoint-", _/binary>>} =
                Doc1 = couch_doc:from_json_obj(Doc0),
            {Props} = Doc1#doc.body,
            case couch_util:get_value(<<"update_seq">>, Props) of
                undefined ->
                    {ok, PeerCheckpoints0};
                UpdateSeq ->
                    {ok,
                        maps:merge_with(
                            fun merge_peers/3, decode_seq(UpdateSeq), PeerCheckpoints0
                        )}
            end
    end;
parse_peer_checkpoint_docs_cb(_Else, Acc) ->
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

peer_checkpoint_docs_mrargs() ->
    (all_docs_mrargs())#mrargs{
        start_key = <<?LOCAL_DOC_PREFIX, "peer-checkpoint-">>,
        end_key = <<?LOCAL_DOC_PREFIX, "peer-checkpoint.">>
    }.

shard_sync_docs_mrargs() ->
    (all_docs_mrargs())#mrargs{
        start_key = <<?LOCAL_DOC_PREFIX, "shard-sync-">>,
        end_key = <<?LOCAL_DOC_PREFIX, "shard-sync.">>
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

create_peer_checkpoint_doc_if_missing(DbName, Subtype, Source, PeerId, UpdateSeq) when
    is_binary(DbName), is_binary(PeerId), is_integer(UpdateSeq)
->
    create_peer_checkpoint_doc_if_missing(
        DbName, Subtype, Source, PeerId, pack_seq(DbName, UpdateSeq)
    );
create_peer_checkpoint_doc_if_missing(DbName, Subtype, Source, PeerId, UpdateSeq) when
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

update_peer_checkpoint_doc(DbName, Subtype, Source, PeerId, UpdateSeq) when
    is_binary(DbName),
    is_binary(Subtype),
    is_binary(Source),
    is_binary(PeerId),
    is_integer(UpdateSeq)
->
    update_peer_checkpoint_doc(DbName, Subtype, Source, PeerId, pack_seq(DbName, UpdateSeq));
update_peer_checkpoint_doc(DbName, Subtype, Source, PeerId, UpdateSeq) when
    is_binary(DbName),
    is_binary(Subtype),
    is_binary(Source),
    is_binary(PeerId),
    is_binary(UpdateSeq)
->
    Doc = peer_checkpoint_doc(PeerId, Subtype, Source, UpdateSeq),
    {_, Ref} = spawn_monitor(fun() ->
        case fabric:update_doc(mem3:dbname(DbName), Doc, [?ADMIN_CTX]) of
            {ok, _} ->
                ok;
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

pack_seq(DbName, UpdateSeq) ->
    DbUuid = couch_util:with_db(DbName, fun(Db) -> couch_db:get_uuid(Db) end),
    Seq0 = [node(), mem3:range(DbName), {UpdateSeq, DbUuid, node()}],
    Seq1 = couch_util:encodeBase64Url(?term_to_bin(Seq0, [compressed])),
    <<(integer_to_binary(UpdateSeq))/binary, $-, Seq1/binary>>.

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

empty_sync_history_means_no_change_test() ->
    Range = [0, 10],
    Node1 = 'node1@127.0.0.1',
    PeerCheckpoints = #{{Range, Node1} => {<<"uuid1">>, 12}},
    ShardSyncHistory = #{},
    ?assertEqual(PeerCheckpoints, calculate_drop_seqs(PeerCheckpoints, ShardSyncHistory)).

matching_sync_history_expands_result_test() ->
    Range = [0, 10],
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    PeerCheckpoints = #{{Range, Node1} => {<<"uuid1">>, 12}},
    ShardSyncHistory = #{{Range, Node1, Node2} => [{<<"uuid1">>, 12, <<"uuid2">>, 5}]},
    ?assertEqual(
        #{
            {Range, Node1} => {<<"uuid1">>, 12},
            {Range, Node2} => {<<"uuid2">>, 5}
        },
        calculate_drop_seqs(PeerCheckpoints, ShardSyncHistory)
    ).

transitive_sync_history_expands_result_test() ->
    Range = [0, 10],
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    Node3 = 'node3@127.0.0.1',
    PeerCheckpoints = #{{Range, Node1} => {<<"uuid1">>, 12}},
    ShardSyncHistory = #{
        {Range, Node1, Node2} => [{<<"uuid1">>, 12, <<"uuid2">>, 5}],
        {Range, Node2, Node3} => [{<<"uuid2">>, 11, <<"uuid3">>, 11}]
    },
    ?assertEqual(
        #{
            {Range, Node1} => {<<"uuid1">>, 12},
            {Range, Node2} => {<<"uuid2">>, 5},
            {Range, Node3} => {<<"uuid3">>, 11}
        },
        calculate_drop_seqs(PeerCheckpoints, ShardSyncHistory)
    ).

shard_sync_history_caps_peer_checkpoint_test() ->
    Range = [0, 10],
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    PeerCheckpoints = #{{Range, Node1} => {<<"uuid1">>, 12}},
    ShardSyncHistory = #{{Range, Node1, Node2} => [{<<"uuid1">>, 10, <<"uuid2">>, 5}]},
    ?assertEqual(
        #{
            {Range, Node1} => {<<"uuid1">>, 10},
            {Range, Node2} => {<<"uuid2">>, 5}
        },
        calculate_drop_seqs(PeerCheckpoints, ShardSyncHistory)
    ).

multiple_range_test() ->
    Range1 = [0, 10],
    Range2 = [11, 20],
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
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
        calculate_drop_seqs(PeerCheckpoints, ShardSyncHistory)
    ).

search_history_for_latest_safe_crossover_test() ->
    Range = [0, 10],
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
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
        calculate_drop_seqs(PeerCheckpoints, ShardSyncHistory)
    ).

-endif.
