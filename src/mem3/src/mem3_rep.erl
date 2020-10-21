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

-module(mem3_rep).


-export([
    go/2,
    go/3,
    make_local_id/2,
    make_local_id/3,
    make_purge_id/2,
    verify_purge_checkpoint/2,
    find_source_seq/4,
    find_split_target_seq/4,
    local_id_hash/1
]).

-export([
    changes_enumerator/2
]).


-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-record(acc, {
    batch_size,
    batch_count,
    seq = 0,
    revcount = 0,
    source,
    targets,
    filter,
    db,
    hashfun,
    incomplete_ranges
}).

-record(tgt, {
    shard,
    seq = 0,
    infos = [],
    localid,
    purgeid,
    history = {[]},
    remaining = 0
}).

go(Source, Target) ->
    go(Source, Target, []).


go(DbName, Node, Opts) when is_binary(DbName), is_atom(Node) ->
    go(#shard{name=DbName, node=node()}, #shard{name=DbName, node=Node}, Opts);

go(#shard{} = Source, #shard{} = Target, Opts) ->
    case mem3:db_is_current(Source) of
        true ->
            go(Source, targets_map(Source, Target), Opts);
        false ->
            % Database could have been recreated
            {error, missing_source}
    end;

go(#shard{} = Source, #{} = Targets0, Opts) when map_size(Targets0) > 0 ->
    Targets = maps:map(fun(_, T) -> #tgt{shard = T} end, Targets0),
    case couch_server:exists(Source#shard.name) of
        true ->
            sync_security(Source, Targets),
            BatchSize = case proplists:get_value(batch_size, Opts) of
                BS when is_integer(BS), BS > 0 -> BS;
                _ -> 100
            end,
            BatchCount = case proplists:get_value(batch_count, Opts) of
                all -> all;
                BC when is_integer(BC), BC > 0 -> BC;
                _ -> 1
            end,
            IncompleteRanges = config:get_boolean("mem3", "incomplete_ranges",
                false),
            Filter = proplists:get_value(filter, Opts),
            Acc = #acc{
                batch_size = BatchSize,
                batch_count = BatchCount,
                source = Source,
                targets = Targets,
                filter = Filter,
                incomplete_ranges = IncompleteRanges
            },
            go(Acc);
        false ->
            {error, missing_source}
    end.


go(#acc{source=Source, batch_count=BC}=Acc) ->
    case couch_db:open(Source#shard.name, [?ADMIN_CTX]) of
    {ok, Db} ->
        Resp = try
            HashFun = mem3_hash:get_hash_fun(couch_db:name(Db)),
            repl(Acc#acc{db = Db, hashfun = HashFun})
        catch
            error:{error, missing_source} ->
                {error, missing_source};
            error:{not_found, no_db_file} ->
                {error, missing_target}
        after
            couch_db:close(Db)
        end,
        case Resp of
            {ok, P} when P > 0, BC == all ->
                go(Acc);
            {ok, P} when P > 0, BC > 1 ->
                go(Acc#acc{batch_count=BC-1});
            Else ->
                Else
        end;
    {not_found, no_db_file} ->
        {error, missing_source}
    end.


make_local_id(Source, Target) ->
    make_local_id(Source, Target, undefined).


make_local_id(#shard{node=SourceNode}, #shard{node=TargetNode}, Filter) ->
    make_local_id(SourceNode, TargetNode, Filter);

make_local_id(SourceThing, TargetThing, F) when is_binary(F) ->
    S = local_id_hash(SourceThing),
    T = local_id_hash(TargetThing),
    <<"_local/shard-sync-", S/binary, "-", T/binary, F/binary>>;

make_local_id(SourceThing, TargetThing, Filter) ->
    S = local_id_hash(SourceThing),
    T = local_id_hash(TargetThing),
    F = filter_hash(Filter),
    <<"_local/shard-sync-", S/binary, "-", T/binary, F/binary>>.


filter_hash(Filter) when is_function(Filter) ->
    {new_uniq, Hash} = erlang:fun_info(Filter, new_uniq),
    B = couch_util:encodeBase64Url(Hash),
    <<"-", B/binary>>;

filter_hash(_) ->
    <<>>.


local_id_hash(Thing) ->
    couch_util:encodeBase64Url(couch_hash:md5_hash(term_to_binary(Thing))).


make_purge_id(SourceUUID, TargetUUID) ->
    <<"_local/purge-mem3-", SourceUUID/binary, "-", TargetUUID/binary>>.


verify_purge_checkpoint(DbName, Props) ->
    try
        Type = couch_util:get_value(<<"type">>, Props),
        if Type =/= <<"internal_replication">> -> false; true ->
            SourceBin = couch_util:get_value(<<"source">>, Props),
            TargetBin = couch_util:get_value(<<"target">>, Props),
            Range = couch_util:get_value(<<"range">>, Props),

            Source = binary_to_existing_atom(SourceBin, latin1),
            Target = binary_to_existing_atom(TargetBin, latin1),

            try
                Nodes = lists:foldl(fun(Shard, Acc) ->
                    case Shard#shard.range == Range of
                        true -> [Shard#shard.node | Acc];
                        false -> Acc
                    end
                end, [], mem3:shards(DbName)),
                lists:member(Source, Nodes) andalso lists:member(Target, Nodes)
            catch
                error:database_does_not_exist ->
                    false
            end
        end
    catch _:_ ->
        false
    end.


%% @doc Find and return the largest update_seq in SourceDb
%% that the client has seen from TargetNode.
%%
%% When reasoning about this function it is very important to
%% understand the direction of replication for this comparison.
%% We're only interesting in internal replications initiated
%% by this node to the node being replaced. When doing a
%% replacement the most important thing is that the client doesn't
%% miss any updates. This means we can only fast-forward as far
%% as they've seen updates on this node. We can detect that by
%% looking for our push replication history and choosing the
%% largest source_seq that has a target_seq =< TgtSeq.
find_source_seq(SrcDb, TgtNode, TgtUUIDPrefix, TgtSeq) ->
    case find_repl_doc(SrcDb, TgtUUIDPrefix) of
    {ok, TgtUUID, Doc} ->
        SrcNode = atom_to_binary(node(), utf8),
        find_source_seq_int(Doc, SrcNode, TgtNode, TgtUUID, TgtSeq);
    {not_found, _} ->
        couch_log:warning("~p find_source_seq repl doc not_found "
            "src_db: ~p, tgt_node: ~p, tgt_uuid_prefix: ~p, tgt_seq: ~p",
            [?MODULE, SrcDb, TgtNode, TgtUUIDPrefix, TgtSeq]),
        0
    end.


find_source_seq_int(#doc{body={Props}}, SrcNode0, TgtNode0, TgtUUID, TgtSeq) ->
    SrcNode = case is_atom(SrcNode0) of
        true -> atom_to_binary(SrcNode0, utf8);
        false -> SrcNode0
    end,
    TgtNode = case is_atom(TgtNode0) of
        true -> atom_to_binary(TgtNode0, utf8);
        false -> TgtNode0
    end,
    % This is split off purely for the ability to run unit tests
    % against this bit of code without requiring all sorts of mocks.
    {History} = couch_util:get_value(<<"history">>, Props, {[]}),
    SrcHistory = couch_util:get_value(SrcNode, History, []),
    UseableHistory = lists:filter(fun({Entry}) ->
        couch_util:get_value(<<"target_node">>, Entry) =:= TgtNode andalso
        couch_util:get_value(<<"target_uuid">>, Entry) =:= TgtUUID andalso
        couch_util:get_value(<<"target_seq">>,  Entry) =<  TgtSeq
    end, SrcHistory),

    % This relies on SrcHistory being ordered desceding by source
    % sequence.
    case UseableHistory of
        [{Entry} | _] ->
            couch_util:get_value(<<"source_seq">>, Entry);
        [] ->
            couch_log:warning("~p find_source_seq_int nil useable history "
                "src_node: ~p, tgt_node: ~p, tgt_uuid: ~p, tgt_seq: ~p, "
                "src_history: ~p",
                [?MODULE, SrcNode, TgtNode, TgtUUID, TgtSeq, SrcHistory]),
            0
    end.


find_split_target_seq(TgtDb, SrcNode0, SrcUUIDPrefix, SrcSeq) ->
    SrcNode = case is_atom(SrcNode0) of
        true -> atom_to_binary(SrcNode0, utf8);
        false -> SrcNode0
    end,
    case find_split_target_seq_int(TgtDb, SrcNode, SrcUUIDPrefix) of
        {ok, [{BulkCopySeq, BulkCopySeq} | _]} when SrcSeq =< BulkCopySeq ->
            % Check if source sequence is at or below the initial bulk copy
            % checkpointed sequence. That sequence or anything lower than it
            % can be directly replaced with the same value for each target. For
            % extra safety we assert that the initial source and target
            % sequences are the same value
            SrcSeq;
        {ok, Seqs= [{_, _} | _]} ->
            % Pick the target sequence for the greatest source sequence that is
            % less than `SrcSeq`.
            case lists:takewhile(fun({Seq, _}) -> Seq < SrcSeq end, Seqs) of
                [] ->
                    couch_log:warning("~p find_split_target_seq target seq not found "
                       "tgt_db: ~p, src_uuid_prefix: ~p, src_seq: ~p",
                        [?MODULE, couch_db:name(TgtDb), SrcUUIDPrefix, SrcSeq]),
                    0;
                [{_, _} | _] = Seqs1 ->
                    {_, TSeq} = lists:last(Seqs1),
                    TSeq
            end;
        {not_found, _} ->
            couch_log:warning("~p find_split_target_seq target seq not found "
                "tgt_db: ~p, src_uuid_prefix: ~p, src_seq: ~p",
                [?MODULE, couch_db:name(TgtDb), SrcUUIDPrefix, SrcSeq]),
            0
    end.


repl(#acc{db = Db0} = Acc0) ->
    erlang:put(io_priority, {internal_repl, couch_db:name(Db0)}),
    Acc1 = calculate_start_seq_multi(Acc0),
    try
        Acc3 = case config:get_boolean("mem3", "replicate_purges", false) of
            true ->
                Acc2 = pull_purges_multi(Acc1),
                push_purges_multi(Acc2);
            false ->
                Acc1
        end,
        push_changes(Acc3)
    catch
        throw:{finished, Count} ->
            {ok, Count}
    end.


pull_purges_multi(#acc{source = Source} = Acc0) ->
    #acc{batch_size = Count, seq = UpdateSeq, targets = Targets0} = Acc0,
    with_src_db(Acc0, fun(Db) ->
        Targets = maps:map(fun(_, #tgt{} = T) ->
            pull_purges(Db, Count, Source, T)
        end, reset_remaining(Targets0)),
        Remaining = maps:fold(fun(_, #tgt{remaining = R}, Sum) ->
            Sum + R
        end, 0, Targets),
        if Remaining == 0 -> Acc0#acc{targets = Targets}; true ->
            PurgeSeq = couch_db:get_purge_seq(Db),
            OldestPurgeSeq = couch_db:get_oldest_purge_seq(Db),
            PurgesToPush = PurgeSeq - OldestPurgeSeq,
            Changes = couch_db:count_changes_since(Db, UpdateSeq),
            Pending = Remaining + PurgesToPush + Changes,
            throw({finished, Pending})
        end
    end).


pull_purges(Db, Count, SrcShard, #tgt{} = Tgt0) ->
    #tgt{shard = TgtShard} = Tgt0,
    SrcUUID = couch_db:get_uuid(Db),
    #shard{node = TgtNode, name = TgtDbName} = TgtShard,
    {LocalPurgeId, Infos, ThroughSeq, Remaining} =
        mem3_rpc:load_purge_infos(TgtNode, TgtDbName, SrcUUID, Count),
    Tgt = Tgt0#tgt{purgeid = LocalPurgeId},
    if Infos == [] -> ok; true ->
        {ok, _} = couch_db:purge_docs(Db, Infos, [replicated_edits]),
        Body = purge_cp_body(SrcShard, TgtShard, ThroughSeq),
        mem3_rpc:save_purge_checkpoint(TgtNode, TgtDbName, LocalPurgeId, Body)
    end,
    Tgt#tgt{remaining = max(0, Remaining)}.


push_purges_multi(#acc{source = SrcShard} = Acc) ->
    #acc{batch_size = BatchSize, seq = UpdateSeq, targets = Targets0} = Acc,
    with_src_db(Acc, fun(Db) ->
        Targets = maps:map(fun(_, #tgt{} = T) ->
            push_purges(Db, BatchSize, SrcShard, T)
        end, reset_remaining(Targets0)),
        Remaining = maps:fold(fun(_, #tgt{remaining = R}, Sum) ->
            Sum + R
        end, 0, Targets),
        if Remaining == 0 -> Acc#acc{targets = Targets}; true ->
            Changes = couch_db:count_changes_since(Db, UpdateSeq),
            throw({finished, Remaining + Changes})
        end
    end).


push_purges(Db, BatchSize, SrcShard, Tgt) ->
    #tgt{shard = TgtShard, purgeid = LocalPurgeId} = Tgt,
    #shard{node = TgtNode, name = TgtDbName} = TgtShard,
    StartSeq = case couch_db:open_doc(Db, LocalPurgeId, []) of
        {ok, #doc{body = {Props}}} ->
            couch_util:get_value(<<"purge_seq">>, Props);
        {not_found, _} ->
            Oldest = couch_db:get_oldest_purge_seq(Db),
            erlang:max(0, Oldest - 1)
    end,
    FoldFun = fun({PSeq, UUID, Id, Revs}, {Count, Infos, _}) ->
        NewCount = Count + length(Revs),
        NewInfos = [{UUID, Id, Revs} | Infos],
        Status = if NewCount < BatchSize -> ok; true -> stop end,
        {Status, {NewCount, NewInfos, PSeq}}
    end,
    InitAcc = {0, [], StartSeq},
    {ok, {_, Infos, ThroughSeq}} =
        couch_db:fold_purge_infos(Db, StartSeq, FoldFun, InitAcc),
    if Infos == [] -> ok; true ->
        ok = purge_on_target(TgtNode, TgtDbName, Infos),
        Body = purge_cp_body(SrcShard, TgtShard, ThroughSeq),
        Doc = #doc{id = LocalPurgeId, body = Body},
        {ok, _} = couch_db:update_doc(Db, Doc, [])
    end,
    Tgt#tgt{remaining = max(0, couch_db:get_purge_seq(Db) - ThroughSeq)}.


calculate_start_seq_multi(#acc{} = Acc) ->
    #acc{db = Db, targets = Targets0, filter = Filter} = Acc,
    FilterHash = filter_hash(Filter),
    Targets = maps:map(fun(_, #tgt{} = T) ->
        calculate_start_seq(Db, FilterHash, T)
    end, Targets0),
    % There will always be at least one target
    #tgt{seq = Seq0} = hd(maps:values(Targets)),
    Seq = maps:fold(fun(_, #tgt{seq = S}, M) -> min(S, M)  end, Seq0, Targets),
    Acc#acc{seq = Seq, targets = Targets}.


calculate_start_seq(Db, FilterHash, #tgt{shard = TgtShard} = Tgt) ->
    UUID = couch_db:get_uuid(Db),
    #shard{node = Node, name = Name} = TgtShard,
    {NewDocId, Doc} = mem3_rpc:load_checkpoint(Node, Name, node(), UUID,
        FilterHash),
    #doc{id=FoundId, body={TProps}} = Doc,
    Tgt1 = Tgt#tgt{localid = NewDocId},
    % NewDocId and FoundId may be different the first time
    % this code runs to save our newly named internal replication
    % checkpoints. We store NewDocId to use when saving checkpoints
    % but use FoundId to reuse the same docid that the target used.
    case couch_db:open_doc(Db, FoundId, [ejson_body]) of
        {ok, #doc{body = {SProps}}} ->
            SourceSeq = couch_util:get_value(<<"seq">>, SProps, 0),
            TargetSeq = couch_util:get_value(<<"seq">>, TProps, 0),
            % We resume from the lower update seq stored in the two
            % shard copies. We also need to be sure and use the
            % corresponding history. A difference here could result
            % from either a write failure on one of the nodes or if
            % either shard was truncated by an operator.
            case SourceSeq =< TargetSeq of
                true ->
                    Seq = SourceSeq,
                    History = couch_util:get_value(<<"history">>, SProps, {[]});
                false ->
                    Seq = TargetSeq,
                    History = couch_util:get_value(<<"history">>, TProps, {[]})
            end,
            Tgt1#tgt{seq = Seq, history = History};
        {not_found, _} ->
            compare_epochs(Db, Tgt1)
    end.


push_changes(#acc{} = Acc0) ->
    #acc{
        db = Db0,
        seq = Seq
    } = Acc0,

    % Avoid needless rewriting the internal replication
    % checkpoint document if nothing is replicated.
    UpdateSeq = couch_db:get_update_seq(Db0),
    if Seq < UpdateSeq -> ok; true ->
        throw({finished, 0})
    end,

    with_src_db(Acc0, fun(Db) ->
        Acc1 = Acc0#acc{db = Db},
        Fun = fun ?MODULE:changes_enumerator/2,
        {ok, Acc2} = couch_db:fold_changes(Db, Seq, Fun, Acc1),
        {ok, #acc{seq = LastSeq}} = replicate_batch_multi(Acc2),
        {ok, couch_db:count_changes_since(Db, LastSeq)}
    end).


compare_epochs(Db, #tgt{shard = TgtShard} = Tgt) ->
    #shard{node = Node, name = Name} = TgtShard,
    UUID = couch_db:get_uuid(Db),
    Epochs = couch_db:get_epochs(Db),
    Seq = mem3_rpc:find_common_seq(Node, Name, UUID, Epochs),
    Tgt#tgt{seq = Seq, history = {[]}}.


changes_enumerator(#doc_info{id=DocId}, #acc{db=Db}=Acc) ->
    {ok, FDI} = couch_db:get_full_doc_info(Db, DocId),
    changes_enumerator(FDI, Acc);
changes_enumerator(#full_doc_info{}=FDI, #acc{}=Acc0) ->
    #acc{
        revcount = C,
        targets = Targets0,
        hashfun = HashFun,
        incomplete_ranges = IncompleteRanges
    } = Acc0,
    #doc_info{high_seq=Seq, revs=Revs} = couch_doc:to_doc_info(FDI),
    {Count, Targets} = case filter_doc(Acc0#acc.filter, FDI) of
        keep ->
            NewTargets = changes_append_fdi(FDI, Targets0, HashFun,
                IncompleteRanges),
            {C + length(Revs), NewTargets};
        discard ->
            {C, Targets0}
    end,
    Acc1 = Acc0#acc{seq = Seq, revcount = Count, targets = Targets},
    Go = if Count < Acc1#acc.batch_size -> ok; true -> stop end,
    {Go, Acc1}.


changes_append_fdi(#full_doc_info{id = Id} = FDI, Targets, HashFun,
        IncompleteRanges) ->
    case mem3_reshard_job:pickfun(Id, maps:keys(Targets), HashFun) of
        not_in_range when IncompleteRanges ->
            Targets;
        not_in_range when not IncompleteRanges ->
            ErrMsg = "~p : ~p not in any target ranges: ~p",
            TShards = [TS || #tgt{shard = TS} <- maps:values(Targets)],
            TNames = [TN || #shard{name = TN} <- TShards],
            couch_log:error(ErrMsg, [?MODULE, Id, TNames]),
            error({error, {Id, not_in_target_ranges}});
        Key ->
            maps:update_with(Key, fun(#tgt{infos = Infos} = T) ->
                T#tgt{infos = [FDI | Infos]}
            end, Targets)
    end.


replicate_batch_multi(#acc{targets = Targets0, seq = Seq, db = Db} = Acc) ->
    Targets = maps:map(fun(_, #tgt{} = T) ->
        replicate_batch(T, Db, Seq)
    end, Targets0),
    {ok, Acc#acc{targets = Targets, revcount = 0}}.


replicate_batch(#tgt{shard = TgtShard, infos = Infos} = Target, Db, Seq) ->
    #shard{node = Node, name = Name} = TgtShard,
    case find_missing_revs(Target) of
        [] ->
            ok;
        Missing ->
            lists:map(fun(Chunk) ->
            Docs = open_docs(Db, Infos, Chunk),
            ok = save_on_target(Node, Name, Docs)
        end, chunk_revs(Missing))
    end,
    update_locals(Target, Db, Seq),
    Target#tgt{infos = []}.


find_missing_revs(#tgt{shard = TgtShard, infos = Infos}) ->
    #shard{node = Node, name = Name} = TgtShard,
    IdsRevs = lists:map(fun(FDI) ->
        #doc_info{id=Id, revs=RevInfos} = couch_doc:to_doc_info(FDI),
        {Id, [R || #rev_info{rev=R} <- RevInfos]}
    end, Infos),
    Missing = mem3_rpc:get_missing_revs(Node, Name, IdsRevs, [
        {io_priority, {internal_repl, Name}},
        ?ADMIN_CTX
    ]),
    lists:filter(fun
        ({_Id, [], _Ancestors}) -> false;
        ({_Id, _Revs, _Ancestors}) -> true
    end, Missing).


chunk_revs(Revs) ->
    Limit = list_to_integer(config:get("mem3", "rev_chunk_size", "5000")),
    chunk_revs(Revs, Limit).

chunk_revs(Revs, Limit) ->
    chunk_revs(Revs, {0, []}, [], Limit).

chunk_revs([], {_Count, Chunk}, Chunks, _Limit) ->
    [Chunk|Chunks];
chunk_revs([{Id, R, A}|Revs], {Count, Chunk}, Chunks, Limit) when length(R) =< Limit - Count ->
    chunk_revs(
        Revs,
        {Count + length(R), [{Id, R, A}|Chunk]},
        Chunks,
        Limit
    );
chunk_revs([{Id, R, A}|Revs], {Count, Chunk}, Chunks, Limit) ->
    {This, Next} = lists:split(Limit - Count, R),
    chunk_revs(
        [{Id, Next, A}|Revs],
        {0, []},
        [[{Id, This, A}|Chunk]|Chunks],
        Limit
    ).


open_docs(Db, Infos, Missing) ->
    lists:flatmap(fun({Id, Revs, _}) ->
        FDI = lists:keyfind(Id, #full_doc_info.id, Infos),
        #full_doc_info{rev_tree=RevTree} = FDI,
        {FoundRevs, _} = couch_key_tree:get_key_leafs(RevTree, Revs),
        lists:map(fun({#leaf{deleted=IsDel, ptr=SummaryPtr}, FoundRevPath}) ->
            couch_db:make_doc(Db, Id, IsDel, SummaryPtr, FoundRevPath)
        end, FoundRevs)
    end, Missing).


save_on_target(Node, Name, Docs) ->
    mem3_rpc:update_docs(Node, Name, Docs, [
        replicated_changes,
        full_commit,
        ?ADMIN_CTX,
        {io_priority, {internal_repl, Name}}
    ]),
    ok.


purge_on_target(Node, Name, PurgeInfos) ->
    mem3_rpc:purge_docs(Node, Name, PurgeInfos, [
        replicated_changes,
        full_commit,
        ?ADMIN_CTX,
        {io_priority, {internal_repl, Name}}
    ]),
    ok.


update_locals(Target, Db, Seq) ->
    #tgt{shard = TgtShard, localid = Id, history = History} = Target,
    #shard{node = Node, name = Name} = TgtShard,
    NewEntry = [
        {<<"source_node">>, atom_to_binary(node(), utf8)},
        {<<"source_uuid">>, couch_db:get_uuid(Db)},
        {<<"source_seq">>, Seq},
        {<<"timestamp">>, list_to_binary(mem3_util:iso8601_timestamp())}
    ],
    NewBody = mem3_rpc:save_checkpoint(Node, Name, Id, Seq, NewEntry, History),
    {ok, _} = couch_db:update_doc(Db, #doc{id = Id, body = NewBody}, []).


purge_cp_body(#shard{} = Source, #shard{} = Target, PurgeSeq) ->
    {Mega, Secs, _} = os:timestamp(),
    NowSecs = Mega * 1000000 + Secs,
    {[
        {<<"type">>, <<"internal_replication">>},
        {<<"updated_on">>, NowSecs},
        {<<"purge_seq">>, PurgeSeq},
        {<<"source">>, atom_to_binary(Source#shard.node, latin1)},
        {<<"target">>, atom_to_binary(Target#shard.node, latin1)},
        {<<"range">>, Source#shard.range}
    ]}.


find_repl_doc(SrcDb, TgtUUIDPrefix) ->
    SrcUUID = couch_db:get_uuid(SrcDb),
    S = local_id_hash(SrcUUID),
    DocIdPrefix = <<"_local/shard-sync-", S/binary, "-">>,
    FoldFun = fun(#doc{id = DocId, body = {BodyProps}} = Doc, _) ->
        TgtUUID = couch_util:get_value(<<"target_uuid">>, BodyProps, <<>>),
        case is_prefix(DocIdPrefix, DocId) of
            true ->
                case is_prefix(TgtUUIDPrefix, TgtUUID) of
                    true ->
                        {stop, {TgtUUID, Doc}};
                    false ->
                        {ok, not_found}
                end;
            _ ->
                {stop, not_found}
        end
    end,
    Options = [{start_key, DocIdPrefix}],
    case couch_db:fold_local_docs(SrcDb, FoldFun, not_found, Options) of
        {ok, {TgtUUID, Doc}} ->
            {ok, TgtUUID, Doc};
        {ok, not_found} ->
            {not_found, missing};
        Else ->
            couch_log:error("Error finding replication doc: ~w", [Else]),
            {not_found, missing}
    end.


find_split_target_seq_int(TgtDb, Node, SrcUUIDPrefix) ->
    TgtUUID = couch_db:get_uuid(TgtDb),
    FoldFun = fun(#doc{body = {Props}}, _) ->
        DocTgtUUID = couch_util:get_value(<<"target_uuid">>, Props, <<>>),
        case TgtUUID == DocTgtUUID of
            true ->
                {History} = couch_util:get_value(<<"history">>, Props, {[]}),
                HProps = couch_util:get_value(Node, History, []),
                case get_target_seqs(HProps, TgtUUID, Node, SrcUUIDPrefix, []) of
                    [] ->
                        % No replication found from source to target
                        {ok, not_found};
                    [{_, _} | _] = SeqPairs ->
                        % Found shared replicated history from source to target
                        % Return sorted list by the earliest source sequence
                        {stop, lists:sort(SeqPairs)}
                end;
            false ->
                {ok, not_found}
        end
    end,
    Options = [{start_key, <<"_local/shard-sync-">>}],
    case couch_db:fold_local_docs(TgtDb, FoldFun, not_found, Options) of
        {ok, Seqs} when is_list(Seqs) ->
            {ok, Seqs};
        {ok, not_found} ->
            {not_found, missing};
        Else ->
            couch_log:error("Error finding replication doc: ~w", [Else]),
            {not_found, missing}
    end.


% Get target sequences for each checkpoint when source replicated to the target
% The "target" is the current db where the history entry was read from and "source"
% is another, now possibly deleted, database.
get_target_seqs([], _TgtUUID, _Node, _SrcUUIDPrefix, Acc) ->
    lists:reverse(Acc);

get_target_seqs([{Entry} | HProps], TgtUUID, Node, SrcUUIDPrefix, Acc) ->
    SameTgt = couch_util:get_value(<<"target_uuid">>, Entry) =:= TgtUUID,
    SameNode = couch_util:get_value(<<"target_node">>, Entry) =:= Node,
    SrcUUID = couch_util:get_value(<<"source_uuid">>, Entry),
    IsPrefix = is_prefix(SrcUUIDPrefix, SrcUUID),
    Acc1 = case SameTgt andalso SameNode andalso IsPrefix of
        true ->
            EntrySourceSeq = couch_util:get_value(<<"source_seq">>, Entry),
            EntryTargetSeq = couch_util:get_value(<<"target_seq">>, Entry),
            [{EntrySourceSeq, EntryTargetSeq} | Acc];
        false ->
            Acc
    end,
    get_target_seqs(HProps, TgtUUID, Node, SrcUUIDPrefix, Acc1).


with_src_db(#acc{source = Source}, Fun) ->
    case couch_db:open(Source#shard.name, [?ADMIN_CTX]) of
        {ok, Db} ->
            try
                Fun(Db)
            after
                couch_db:close(Db)
            end;
         {not_found, _} ->
            error({error, missing_source})
    end.


is_prefix(Prefix, Subject) ->
    binary:longest_common_prefix([Prefix, Subject]) == size(Prefix).


filter_doc(Filter, FullDocInfo) when is_function(Filter) ->
    try Filter(FullDocInfo) of
        discard -> discard;
        _ -> keep
    catch _:_ ->
        keep
    end;
filter_doc(_, _) ->
    keep.


sync_security(#shard{} = Source, #{} = Targets) ->
    maps:map(fun(_, #tgt{shard = Target}) ->
        mem3_sync_security:maybe_sync(Source, Target)
    end, Targets).


targets_map(#shard{name = <<"shards/", _/binary>> = SrcName} = Src,
        #shard{name = <<"shards/", _/binary>>, node = TgtNode} = Tgt) ->
    % Parse range from name in case the passed shard is built with a name only
    SrcRange = mem3:range(SrcName),
    Shards0 = mem3:shards(mem3:dbname(SrcName)),
    Shards1 = [S || S <- Shards0, not shard_eq(S, Src)],
    Shards2 = [S || S <- Shards1, check_overlap(SrcRange, TgtNode, S)],
    case [{R, S} || #shard{range = R} = S <- Shards2] of
        [] ->
            % If target map is empty, create a target map with just
            % that one target. This is to support tooling which may be
            % moving / copying shards using mem3:go/2,3 before the
            % shards are present in the shard map
            #{mem3:range(SrcName) => Tgt};
        [_ | _] = TMapList->
            maps:from_list(TMapList)
    end;


targets_map(_Src, Tgt) ->
    #{[0, ?RING_END] => Tgt}.


shard_eq(#shard{name = Name, node = Node}, #shard{name = Name, node = Node}) ->
    true;

shard_eq(_, _) ->
    false.


check_overlap(SrcRange, Node, #shard{node = Node, range = TgtRange}) ->
    mem3_util:range_overlap(SrcRange, TgtRange);

check_overlap([_, _], _, #shard{}) ->
    false.


reset_remaining(#{} = Targets) ->
    maps:map(fun(_, #tgt{} = T) ->
        T#tgt{remaining = 0}
    end, Targets).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


find_source_seq_unknown_node_test() ->
    ?assertEqual(
        find_source_seq_int(doc_(), <<"foo">>, <<"bing">>, <<"bar_uuid">>, 10),
        0
    ).


find_source_seq_unknown_uuid_test() ->
    ?assertEqual(
        find_source_seq_int(doc_(), <<"foo">>, <<"bar">>, <<"teapot">>, 10),
        0
    ).


find_source_seq_ok_test() ->
    ?assertEqual(
        find_source_seq_int(doc_(), <<"foo">>, <<"bar">>, <<"bar_uuid">>, 100),
        100
    ).


find_source_seq_old_ok_test() ->
    ?assertEqual(
        find_source_seq_int(doc_(), <<"foo">>, <<"bar">>, <<"bar_uuid">>, 84),
        50
    ).


find_source_seq_different_node_test() ->
    ?assertEqual(
        find_source_seq_int(doc_(), <<"foo2">>, <<"bar">>, <<"bar_uuid">>, 92),
        31
    ).


-define(SNODE, <<"source_node">>).
-define(SUUID, <<"source_uuid">>).
-define(SSEQ, <<"source_seq">>).
-define(TNODE, <<"target_node">>).
-define(TUUID, <<"target_uuid">>).
-define(TSEQ, <<"target_seq">>).

doc_() ->
    Foo_Bar = [
        {[
            {?SNODE, <<"foo">>}, {?SUUID, <<"foo_uuid">>}, {?SSEQ, 100},
            {?TNODE, <<"bar">>}, {?TUUID, <<"bar_uuid">>}, {?TSEQ, 100}
        ]},
        {[
            {?SNODE, <<"foo">>}, {?SUUID, <<"foo_uuid">>}, {?SSEQ, 90},
            {?TNODE, <<"bar">>}, {?TUUID, <<"bar_uuid">>}, {?TSEQ, 85}
        ]},
        {[
            {?SNODE, <<"foo">>}, {?SUUID, <<"foo_uuid">>}, {?SSEQ, 50},
            {?TNODE, <<"bar">>}, {?TUUID, <<"bar_uuid">>}, {?TSEQ, 51}
        ]},
        {[
            {?SNODE, <<"foo">>}, {?SUUID, <<"foo_uuid">>}, {?SSEQ, 40},
            {?TNODE, <<"bar">>}, {?TUUID, <<"bar_uuid">>}, {?TSEQ, 45}
        ]},
        {[
            {?SNODE, <<"foo">>}, {?SUUID, <<"foo_uuid">>}, {?SSEQ, 2},
            {?TNODE, <<"bar">>}, {?TUUID, <<"bar_uuid">>}, {?TSEQ, 2}
        ]}
    ],
    Foo2_Bar = [
        {[
            {?SNODE, <<"foo2">>}, {?SUUID, <<"foo_uuid">>}, {?SSEQ, 100},
            {?TNODE, <<"bar">>}, {?TUUID, <<"bar_uuid">>}, {?TSEQ, 100}
        ]},
        {[
            {?SNODE, <<"foo2">>}, {?SUUID, <<"foo_uuid">>}, {?SSEQ, 92},
            {?TNODE, <<"bar">>}, {?TUUID, <<"bar_uuid">>}, {?TSEQ, 93}
        ]},
        {[
            {?SNODE, <<"foo2">>}, {?SUUID, <<"foo_uuid">>}, {?SSEQ, 31},
            {?TNODE, <<"bar">>}, {?TUUID, <<"bar_uuid">>}, {?TSEQ, 30}
        ]}
    ],
    History = {[
        {<<"foo">>, Foo_Bar},
        {<<"foo2">>, Foo2_Bar}
    ]},
    #doc{
        body={[{<<"history">>, History}]}
    }.


targets_map_test_() ->
    {
        setup,
        fun() -> meck:new(mem3, [passthrough]) end,
        fun(_) -> meck:unload() end,
        [
            target_not_a_shard(),
            source_contained_in_target(),
            multiple_targets(),
            uneven_overlap(),
            target_not_in_shard_map()
        ]
    }.


target_not_a_shard() ->
    ?_assertEqual(#{[0, ?RING_END] => <<"t">>}, targets_map(<<"s">>, <<"t">>)).


source_contained_in_target() ->
    ?_test(begin
        R07 = [16#00000000, 16#7fffffff],
        R8f = [16#80000000, 16#ffffffff],
        R0f = [16#00000000, 16#ffffffff],

        Shards = [
            #shard{node = 'n1', range = R07},
            #shard{node = 'n1', range = R8f},
            #shard{node = 'n2', range = R07},
            #shard{node = 'n2', range = R8f},
            #shard{node = 'n3', range = R0f}
        ],
        meck:expect(mem3, shards, 1, Shards),

        SrcName1 = <<"shards/00000000-7fffffff/d.1551893552">>,
        TgtName1 = <<"shards/00000000-7fffffff/d.1551893552">>,

        Src1 = #shard{name = SrcName1, node = 'n1'},
        Tgt1 = #shard{name = TgtName1, node = 'n2'},
        Map1 = targets_map(Src1, Tgt1),
        ?assertEqual(1, map_size(Map1)),
        ?assertMatch(#{R07 := #shard{node = 'n2'}}, Map1),

        Tgt2 =  #shard{name = TgtName1, node = 'n3'},
        Map2 =  targets_map(Src1, Tgt2),
        ?assertEqual(1, map_size(Map2)),
        ?assertMatch(#{R0f := #shard{node = 'n3'}}, Map2)
    end).


multiple_targets() ->
    ?_test(begin
        R07 = [16#00000000, 16#7fffffff],
        R8f = [16#80000000, 16#ffffffff],
        R0f = [16#00000000, 16#ffffffff],

        Shards = [
            #shard{node = 'n1', range = R07},
            #shard{node = 'n1', range = R8f},
            #shard{node = 'n2', range = R0f}
        ],
        meck:expect(mem3, shards, 1, Shards),

        SrcName = <<"shards/00000000-ffffffff/d.1551893552">>,
        TgtName = <<"shards/00000000-7fffffff/d.1551893552">>,

        Src = #shard{name = SrcName, node = 'n2'},
        Tgt = #shard{name = TgtName, node = 'n1'},
        Map = targets_map(Src, Tgt),
        ?assertEqual(2, map_size(Map)),
        ?assertMatch(#{R07 := #shard{node = 'n1'}}, Map),
        ?assertMatch(#{R8f := #shard{node = 'n1'}}, Map)
    end).


uneven_overlap() ->
    ?_test(begin
        R04 = [16#00000000, 16#4fffffff],
        R26 = [16#20000000, 16#6fffffff],
        R58 = [16#50000000, 16#8fffffff],
        R9f = [16#90000000, 16#ffffffff],
        Shards = [
            #shard{node = 'n1', range = R04},
            #shard{node = 'n1', range = R58},
            #shard{node = 'n1', range = R9f},
            #shard{node = 'n2', range = R26}
        ],

        meck:expect(mem3, shards, 1, Shards),

        SrcName = <<"shards/20000000-6fffffff/d.1551893552">>,
        TgtName = <<"shards/20000000-6fffffff/d.1551893552">>,

        Src = #shard{name = SrcName, node = 'n2'},
        Tgt = #shard{name = TgtName, node = 'n1'},
        Map = targets_map(Src, Tgt),
        ?assertEqual(2, map_size(Map)),
        ?assertMatch(#{R04 := #shard{node = 'n1'}}, Map),
        ?assertMatch(#{R58 := #shard{node = 'n1'}}, Map)
    end).


target_not_in_shard_map() ->
    ?_test(begin
        R0f = [16#00000000, 16#ffffffff],
        Name = <<"shards/00000000-ffffffff/d.1551893552">>,
        Shards = [
            #shard{name = Name, node = 'n1', range = R0f},
            #shard{name = Name, node = 'n2', range = R0f}
        ],
        meck:expect(mem3, shards, 1, Shards),
        Src = #shard{name = Name, node = 'n1'},
        Tgt = #shard{name = Name, node = 'n3'},
        Map = targets_map(Src, Tgt),
        ?assertEqual(1, map_size(Map)),
        ?assertMatch(#{R0f := #shard{name = Name, node = 'n3'}}, Map)
    end).

-endif.
