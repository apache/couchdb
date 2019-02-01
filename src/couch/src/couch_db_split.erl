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

-module(couch_db_split).


-export([
    split/3,
    copy_local_docs/3,
    cleanup_target/2
]).


-include_lib("couch/include/couch_db.hrl").


-define(DEFAULT_BUFFER_SIZE, 16777216).


-record(state, {
    source_db,
    source_uuid,
    targets,
    pickfun,
    max_buffer_size = ?DEFAULT_BUFFER_SIZE,
    hashfun
}).

-record(target, {
    db,
    uuid,
    buffer = [],
    buffer_size = 0
}).

-record(racc, {
    id,
    source_db,
    target_db,
    active = 0,
    external = 0,
    atts = []
}).


% Public API

split(Source, #{} = Targets, PickFun) when is_function(PickFun, 3) ->
    {ok, SourceDb} = couch_db:open_int(Source, [?ADMIN_CTX]),
    Engine = get_engine(SourceDb),
    Partitioned = couch_db:is_partitioned(SourceDb),
    {_M, _F, _A} = HashFun = mem3_hash:get_hash_fun(couch_db:name(SourceDb)),
    try
        try
            split(SourceDb, Partitioned, Engine, Targets, PickFun, HashFun)
        catch
            throw:{target_create_error, DbName, Error, CreatedTargets} ->
                cleanup_targets(CreatedTargets, Engine),
                {error, {target_create_error, DbName, Error}}
        end
    after
        couch_db:close(SourceDb)
    end.


copy_local_docs(Source, #{} = Targets0, PickFun) when
        is_binary(Source), is_function(PickFun, 3) ->
    {ok, SourceDb} = couch_db:open_int(Source, [?ADMIN_CTX]),
    try
        Targets = maps:map(fun(_, DbName) ->
            {ok, Db} = couch_db:open_int(DbName, [?ADMIN_CTX]),
            #target{db = Db, uuid = couch_db:get_uuid(Db)}
        end, Targets0),
        try
            State = #state{
                source_db = SourceDb,
                source_uuid = couch_db:get_uuid(SourceDb),
                targets = Targets,
                pickfun = PickFun,
                hashfun = mem3_hash:get_hash_fun(couch_db:name(SourceDb))
            },
            copy_local_docs(State),
            ok
        after
            maps:map(fun(_, #target{db = Db} = T) ->
                couch_db:close(Db),
                T#target{db = undefined}
            end, Targets)
        end
    after
        couch_db:close(SourceDb)
    end.


cleanup_target(Source, Target) when is_binary(Source), is_binary(Target) ->
    {ok, SourceDb} = couch_db:open_int(Source, [?ADMIN_CTX]),
    try
        delete_target(Target, get_engine(SourceDb))
    after
        couch_db:close(SourceDb)
    end.


% Private Functions

split(SourceDb, Partitioned, Engine, Targets0, PickFun, {M, F, A} = HashFun) ->
    Targets = maps:fold(fun(Key, DbName, Map) ->
        {ok, Filepath} = couch_server:get_engine_path(DbName, Engine),
        Opts = [create, ?ADMIN_CTX] ++ case Partitioned of
            true -> [{partitioned, true}, {hash, [M, F, A]}];
            false -> []
        end,
        case couch_db:start_link(Engine, DbName, Filepath, Opts) of
            {ok, Db} ->
                Map#{Key => #target{db = Db}};
            {error, Error} ->
                throw({target_create_error, DbName, Error, Map})
        end
    end, #{}, Targets0),
    Seq = couch_db:get_update_seq(SourceDb),
    State1 = #state{
        source_db = SourceDb,
        targets = Targets,
        pickfun = PickFun,
        hashfun = HashFun,
        max_buffer_size = get_max_buffer_size()
    },
    State2 = copy_docs(State1),
    State3 = copy_checkpoints(State2),
    State4 = copy_meta(State3),
    State5 = copy_purge_info(State4),
    State6 = set_targets_update_seq(State5),
    stop_targets(State6#state.targets),
    {ok, Seq}.


cleanup_targets(#{} = Targets, Engine) ->
    maps:map(fun(_, #target{db = Db} = T) ->
        stop_target_db(Db),
        delete_target(couch_db:name(Db), Engine),
        T
    end, Targets).


stop_targets(#{} = Targets) ->
    maps:map(fun(_, #target{db = Db} = T) ->
        {ok, Db1} = couch_db_engine:commit_data(Db),
        ok = stop_target_db(Db1),
        T
    end, Targets).


stop_target_db(Db) ->
    couch_db:close(Db),
    Pid = couch_db:get_pid(Db),
    catch unlink(Pid),
    catch exit(Pid, kill),
    ok.


delete_target(DbName, Engine) ->
    RootDir = config:get("couchdb", "database_dir", "."),
    {ok, Filepath} = couch_server:get_engine_path(DbName, Engine),
    DelOpt = [{context, compaction}, sync],
    couch_db_engine:delete(Engine, RootDir, Filepath, DelOpt).


pick_target(DocId, #state{} = State, #{} = Targets) ->
    #state{pickfun = PickFun, hashfun = HashFun} = State,
    Key = PickFun(DocId, maps:keys(Targets), HashFun),
    {Key, maps:get(Key, Targets)}.


set_targets_update_seq(#state{targets = Targets} = State) ->
    Seq = couch_db:get_update_seq(State#state.source_db),
    Targets1 = maps:map(fun(_, #target{db = Db} = Target) ->
        {ok, Db1} = couch_db_engine:set_update_seq(Db, Seq),
        Target#target{db = Db1}
    end, Targets),
    State#state{targets = Targets1}.


copy_checkpoints(#state{} = State) ->
    #state{source_db = Db, source_uuid = SrcUUID,  targets = Targets} = State,
    FoldFun = fun(#doc{id = Id} = Doc, Acc) ->
        UpdatedAcc = case Id of
            <<?LOCAL_DOC_PREFIX, "shard-sync-", _/binary>> ->
                % Transform mem3 internal replicator checkpoints to avoid
                % rewinding the changes feed when it sees the new shards
                maps:map(fun(_, #target{uuid = TgtUUID, buffer = Docs} = T) ->
                    Doc1 = update_checkpoint_doc(SrcUUID, TgtUUID, Doc),
                    T#target{buffer = [Doc1 | Docs]}
                end, Acc);
            <<?LOCAL_DOC_PREFIX, "purge-", _/binary>> ->
                % Copy purge checkpoints to all shards
                maps:map(fun(_, #target{buffer = Docs} = T) ->
                    T#target{buffer = [Doc | Docs]}
                end, Acc);
            <<?LOCAL_DOC_PREFIX, _/binary>> ->
                % Skip copying these that will be done during
                % local docs top off right before the shards are switched
                Acc
        end,
        {ok, UpdatedAcc}
    end,
    {ok, Targets1} = couch_db_engine:fold_local_docs(Db, FoldFun, Targets, []),
    Targets2 = maps:map(fun(_, #target{db = TDb, buffer = Docs} = T) ->
        case Docs of [] -> T; [_ | _] ->
            Docs1 = lists:reverse(Docs),
            {ok, TDb1} = couch_db_engine:write_doc_infos(TDb, [], Docs1),
            {ok, TDb2} = couch_db_engine:commit_data(TDb1),
            T#target{db = TDb2, buffer = []}
        end
    end, Targets1),
    State#state{targets = Targets2}.


update_checkpoint_doc(Old, New, #doc{body = {Props}} = Doc) ->
    NewProps = case couch_util:get_value(<<"target_uuid">>, Props) of
        Old ->
            replace_kv(Props, {<<"target_uuid">>, Old, New});
        Other when is_binary(Other) ->
            replace_kv(Props, {<<"source_uuid">>, Old, New})
    end,
    NewId = update_checkpoint_id(Doc#doc.id, Old, New),
    Doc#doc{id = NewId, body = {NewProps}}.


update_checkpoint_id(Id, Old, New) ->
    OldHash = mem3_rep:local_id_hash(Old),
    NewHash = mem3_rep:local_id_hash(New),
    binary:replace(Id, OldHash, NewHash).


replace_kv({[]}, _) ->
     {[]};
replace_kv({KVs}, Replacement) ->
    {[replace_kv(KV, Replacement) || KV <- KVs]};
replace_kv([], _) ->
     [];
replace_kv(List, Replacement) when is_list(List) ->
     [replace_kv(V, Replacement) || V <- List];
replace_kv({K, V}, {K, V, NewV}) ->
    {K, NewV};
replace_kv({K, V}, Replacement) ->
    {K, replace_kv(V, Replacement)};
replace_kv(V, _) ->
    V.


copy_meta(#state{source_db = SourceDb, targets = Targets} = State) ->
    RevsLimit = couch_db:get_revs_limit(SourceDb),
    {SecProps} = couch_db:get_security(SourceDb),
    PurgeLimit = couch_db:get_purge_infos_limit(SourceDb),
    Targets1 = maps:map(fun(_, #target{db = Db} = T) ->
        {ok, Db1} = couch_db_engine:set_revs_limit(Db, RevsLimit),
        {ok, Db2} = couch_db_engine:set_security(Db1, SecProps),
        {ok, Db3} = couch_db_engine:set_purge_infos_limit(Db2, PurgeLimit),
        T#target{db = Db3}
    end, Targets),
    State#state{targets = Targets1}.


copy_purge_info(#state{source_db = Db} = State) ->
    {ok, NewState} = couch_db:fold_purge_infos(Db, 0, fun purge_cb/2, State),
    Targets = maps:map(fun(_, #target{} = T) ->
        commit_purge_infos(T)
    end, NewState#state.targets),
    NewState#state{targets = Targets}.


purge_cb({_PSeq, _UUID, Id, _Revs} = PI, #state{targets = Targets} = State) ->
    {Key, Target} = pick_target(Id, State, Targets),
    #target{buffer = Buffer, buffer_size = BufferSize} = Target,
    Target1 = Target#target{
        buffer = [PI | Buffer],
        buffer_size = BufferSize + ?term_size(PI)
    },
    Target2 = case Target1#target.buffer_size > State#state.max_buffer_size of
        true -> commit_purge_infos(Target1);
        false -> Target1
    end,
    {ok, State#state{targets = Targets#{Key => Target2}}}.


commit_purge_infos(#target{buffer = [], db = Db} = Target) ->
    Target#target{db = Db};

commit_purge_infos(#target{buffer = PIs0, db = Db} = Target) ->
    PIs = lists:reverse(PIs0),
    {ok, Db1} = couch_db_engine:copy_purge_infos(Db, PIs),
    {ok, Db2} = couch_db_engine:commit_data(Db1),
    Target#target{buffer = [], buffer_size = 0, db = Db2}.


copy_docs(#state{source_db = Db} = State) ->
    {ok, NewState} = couch_db:fold_changes(Db, 0, fun changes_cb/2, State),
    CommitTargets = maps:map(fun(_, #target{} = T) ->
        commit_docs(T)
     end, NewState#state.targets),
    NewState#state{targets = CommitTargets}.


% Backwards compatibility clause. Seq trees used to hold #doc_infos at one time
changes_cb(#doc_info{id = Id}, #state{source_db = Db} = State) ->
    [FDI = #full_doc_info{}] = couch_db_engine:open_docs(Db, [Id]),
    changes_cb(FDI, State);

changes_cb(#full_doc_info{id = Id} = FDI, #state{} = State) ->
    #state{source_db = SourceDb, targets = Targets} = State,
    {Key, Target} = pick_target(Id, State, Targets),
    #target{db = TargetDb, buffer = Buffer} = Target,
    FDI1 = process_fdi(FDI, SourceDb, TargetDb),
    Target1 = Target#target{
        buffer = [FDI1 | Buffer],
        buffer_size = Target#target.buffer_size + ?term_size(FDI1)
    },
    Target2 = case Target1#target.buffer_size > State#state.max_buffer_size of
        true -> commit_docs(Target1);
        false -> Target1
    end,
    {ok, State#state{targets = Targets#{Key => Target2}}}.


commit_docs(#target{buffer = [], db = Db} = Target) ->
    Target#target{db = Db};

commit_docs(#target{buffer = FDIs, db = Db} = Target) ->
    Pairs = [{not_found, FDI} || FDI <- lists:reverse(FDIs)],
    {ok, Db1} = couch_db_engine:write_doc_infos(Db, Pairs, []),
    {ok, Db2} = couch_db_engine:commit_data(Db1),
    Target#target{buffer = [], buffer_size = 0, db = Db2}.


process_fdi(FDI, SourceDb, TargetDb) ->
    #full_doc_info{id = Id, rev_tree = RTree} = FDI,
    Acc = #racc{id = Id, source_db = SourceDb, target_db = TargetDb},
    {NewRTree, NewAcc} = couch_key_tree:mapfold(fun revtree_cb/4, Acc, RTree),
    {Active, External} = total_sizes(NewAcc),
    FDI#full_doc_info{
        rev_tree = NewRTree,
        sizes = #size_info{active = Active, external = External}
    }.


revtree_cb(_Rev, _Leaf, branch, Acc) ->
    {[], Acc};

revtree_cb({Pos, RevId}, Leaf, leaf, Acc) ->
    #racc{id = Id, source_db = SourceDb, target_db = TargetDb} = Acc,
    #leaf{deleted = Deleted, ptr = Ptr} = Leaf,
    Doc0 = #doc{
        id = Id,
        revs = {Pos, [RevId]},
        deleted = Deleted,
        body = Ptr
    },
    Doc1 = couch_db_engine:read_doc_body(SourceDb, Doc0),
    #doc{body = Body, atts = AttInfos0} = Doc1,
    External = if not is_binary(Body) -> ?term_size(body); true ->
        couch_compress:uncompressed_size(Body)
    end,
    AttInfos = if not is_binary(AttInfos0) -> AttInfos0; true ->
        couch_compress:decompress(AttInfos0)
    end,
    Atts = [process_attachment(Att, SourceDb, TargetDb) || Att <- AttInfos],
    Doc2 = Doc1#doc{atts = Atts},
    Doc3 = couch_db_engine:serialize_doc(TargetDb, Doc2),
    {ok, Doc4, Active} = couch_db_engine:write_doc_body(TargetDb, Doc3),
    AttSizes = [{element(3, A), element(4, A)} || A <- Atts],
    NewLeaf = Leaf#leaf{
        ptr = Doc4#doc.body,
        sizes = #size_info{active = Active, external = External},
        atts = AttSizes
    },
    {NewLeaf, add_sizes(Active, External, AttSizes, Acc)}.


% This is copied almost verbatim from the compactor
process_attachment({Name, Type, BinSp, AttLen, RevPos, ExpectedMd5}, SourceDb,
        TargetDb) ->
    % 010 upgrade code
    {ok, SrcStream} = couch_db_engine:open_read_stream(SourceDb, BinSp),
    {ok, DstStream} = couch_db_engine:open_write_stream(TargetDb, []),
    ok = couch_stream:copy(SrcStream, DstStream),
    {NewStream, AttLen, AttLen, ActualMd5, _IdentityMd5} =
            couch_stream:close(DstStream),
    {ok, NewBinSp} = couch_stream:to_disk_term(NewStream),
    couch_util:check_md5(ExpectedMd5, ActualMd5),
    {Name, Type, NewBinSp, AttLen, AttLen, RevPos, ExpectedMd5, identity};

process_attachment({Name, Type, BinSp, AttLen, DiskLen, RevPos, ExpectedMd5,
        Enc1}, SourceDb, TargetDb) ->
    {ok, SrcStream} = couch_db_engine:open_read_stream(SourceDb, BinSp),
    {ok, DstStream} = couch_db_engine:open_write_stream(TargetDb, []),
    ok = couch_stream:copy(SrcStream, DstStream),
    {NewStream, AttLen, _, ActualMd5, _IdentityMd5} =
            couch_stream:close(DstStream),
    {ok, NewBinSp} = couch_stream:to_disk_term(NewStream),
    couch_util:check_md5(ExpectedMd5, ActualMd5),
    Enc = case Enc1 of
        true -> gzip;  % 0110 upgrade code
        false -> identity;  % 0110 upgrade code
        _ -> Enc1
    end,
    {Name, Type, NewBinSp, AttLen, DiskLen, RevPos, ExpectedMd5, Enc}.


get_engine(Db) ->
    {ok, DbInfoProps} = couch_db:get_db_info(Db),
    proplists:get_value(engine, DbInfoProps).


add_sizes(Active, External, Atts, #racc{} = Acc) ->
    #racc{active = ActiveAcc, external = ExternalAcc, atts = AttsAcc} = Acc,
    NewActiveAcc = ActiveAcc + Active,
    NewExternalAcc = ExternalAcc + External,
    NewAttsAcc = lists:umerge(Atts, AttsAcc),
    Acc#racc{
        active = NewActiveAcc,
        external = NewExternalAcc,
        atts = NewAttsAcc
    }.


total_sizes(#racc{active = Active, external = External, atts = Atts}) ->
    TotalAtts = lists:foldl(fun({_, S}, A) -> S + A end, 0, Atts),
    {Active + TotalAtts, External + TotalAtts}.


get_max_buffer_size() ->
    config:get_integer("shard_splitting", "buffer_size", ?DEFAULT_BUFFER_SIZE).


copy_local_docs(#state{source_db = Db, targets = Targets} = State) ->
    FoldFun = fun(#doc{id = Id} = Doc, Acc) ->
        UpdatedAcc = case Id of
            <<?LOCAL_DOC_PREFIX, "shard-sync-", _/binary>> ->
                Acc;
            <<?LOCAL_DOC_PREFIX, "purge-", _/binary>> ->
                Acc;
            <<?LOCAL_DOC_PREFIX, _/binary>> ->
                % Users' and replicator app's checkpoints go to their
                % respective shards based on the general hashing algorithm
                {Key, #target{buffer = Docs} = T} = pick_target(Id, State, Acc),
                Acc#{Key => T#target{buffer = [Doc | Docs]}}
        end,
        {ok, UpdatedAcc}
    end,
    {ok, Targets1} = couch_db:fold_local_docs(Db, FoldFun, Targets, []),
    Targets2 = maps:map(fun(_, #target{db = TDb, buffer = Docs} = T) ->
        case Docs of [] -> T; [_ | _] ->
            Docs1 = lists:reverse(Docs),
            {ok, _} = couch_db:update_docs(TDb, Docs1),
            {ok, _} = couch_db:ensure_full_commit(TDb),
            T#target{buffer = []}
        end
    end, Targets1),
    State#state{targets = Targets2}.
