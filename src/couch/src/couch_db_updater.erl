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

-module(couch_db_updater).
-behaviour(gen_server).
-vsn(1).

-export([btree_by_id_split/1, btree_by_id_join/2, btree_by_id_reduce/2]).
-export([btree_by_seq_split/1, btree_by_seq_join/2, btree_by_seq_reduce/2]).
-export([make_doc_summary/2]).
-export([init/1,terminate/2,handle_call/3,handle_cast/2,code_change/3,handle_info/2]).

-include_lib("couch/include/couch_db.hrl").

-define(IDLE_LIMIT_DEFAULT, 61000).

-record(comp_st, {
    old_db,
    new_db,
    docid_fd,
    docid_mod,
    docid_st,
    retry
}).

-record(comp_header, {
    vsn = 1,
    db_header,
    docid_st
}).

-record(merge_st, {
    id_tree,
    seq_tree,
    curr,
    rem_seqs,
    infos
}).

init({DbName, Filepath, Fd, Options}) ->
    erlang:put(io_priority, {db_update, DbName}),
    update_idle_limit_from_config(),
    case lists:member(create, Options) of
    true ->
        % create a new header and writes it to the file
        Header =  couch_db_header:new(),
        ok = couch_file:write_header(Fd, Header),
        % delete any old compaction files that might be hanging around
        RootDir = config:get("couchdb", "database_dir", "."),
        couch_file:delete(RootDir, Filepath ++ ".compact"),
        couch_file:delete(RootDir, Filepath ++ ".compact.data"),
        couch_file:delete(RootDir, Filepath ++ ".compact.meta");
    false ->
        case couch_file:read_header(Fd) of
        {ok, Header} ->
            ok;
        no_valid_header ->
            % create a new header and writes it to the file
            Header =  couch_db_header:new(),
            ok = couch_file:write_header(Fd, Header),
            % delete any old compaction files that might be hanging around
            file:delete(Filepath ++ ".compact"),
            file:delete(Filepath ++ ".compact.data"),
            file:delete(Filepath ++ ".compact.meta")
        end
    end,
    Db = init_db(DbName, Filepath, Fd, Header, Options),
    case lists:member(sys_db, Options) of
        false ->
            couch_stats_process_tracker:track([couchdb, open_databases]);
        true ->
            ok
    end,
    % we don't load validation funs here because the fabric query is liable to
    % race conditions.  Instead see couch_db:validate_doc_update, which loads
    % them lazily
    {ok, Db#db{main_pid = self()}, idle_limit()}.


terminate(_Reason, Db) ->
    % If the reason we died is because our fd disappeared
    % then we don't need to try closing it again.
    if Db#db.fd_monitor == closed -> ok; true ->
        ok = couch_file:close(Db#db.fd)
    end,
    couch_util:shutdown_sync(Db#db.compactor_pid),
    couch_util:shutdown_sync(Db#db.fd),
    ok.

handle_call(get_db, _From, Db) ->
    {reply, {ok, Db}, Db, idle_limit()};
handle_call(full_commit, _From, #db{waiting_delayed_commit=nil}=Db) ->
    {reply, ok, Db, idle_limit()}; % no data waiting, return ok immediately
handle_call(full_commit, _From,  Db) ->
    {reply, ok, commit_data(Db), idle_limit()};
handle_call({full_commit, RequiredSeq}, _From, Db)
        when RequiredSeq =< Db#db.committed_update_seq ->
    {reply, ok, Db, idle_limit()};
handle_call({full_commit, _}, _, Db) ->
    {reply, ok, commit_data(Db), idle_limit()}; % commit the data and return ok
handle_call(start_compact, _From, Db) ->
    {noreply, NewDb, _Timeout} = handle_cast(start_compact, Db),
    {reply, {ok, NewDb#db.compactor_pid}, NewDb, idle_limit()};
handle_call(compactor_pid, _From, #db{compactor_pid = Pid} = Db) ->
    {reply, Pid, Db, idle_limit()};
handle_call(cancel_compact, _From, #db{compactor_pid = nil} = Db) ->
    {reply, ok, Db, idle_limit()};
handle_call(cancel_compact, _From, #db{compactor_pid = Pid} = Db) ->
    unlink(Pid),
    exit(Pid, kill),
    RootDir = config:get("couchdb", "database_dir", "."),
    ok = couch_file:delete(RootDir, Db#db.filepath ++ ".compact"),
    Db2 = Db#db{compactor_pid = nil},
    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    {reply, ok, Db2, idle_limit()};
handle_call(increment_update_seq, _From, Db) ->
    Db2 = commit_data(Db#db{update_seq=Db#db.update_seq+1}),
    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    couch_event:notify(Db#db.name, updated),
    {reply, {ok, Db2#db.update_seq}, Db2, idle_limit()};

handle_call({set_security, NewSec}, _From, #db{compression = Comp} = Db) ->
    {ok, Ptr, _} = couch_file:append_term(
        Db#db.fd, NewSec, [{compression, Comp}]),
    Db2 = commit_data(Db#db{security=NewSec, security_ptr=Ptr,
            update_seq=Db#db.update_seq+1}),
    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    {reply, ok, Db2, idle_limit()};

handle_call({set_revs_limit, Limit}, _From, Db) ->
    Db2 = commit_data(Db#db{revs_limit=Limit,
            update_seq=Db#db.update_seq+1}),
    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    {reply, ok, Db2, idle_limit()};

handle_call({purge_docs, _IdRevs}, _From,
        #db{compactor_pid=Pid}=Db) when Pid /= nil ->
    {reply, {error, purge_during_compaction}, Db, idle_limit()};
handle_call({purge_docs, IdRevs}, _From, Db) ->
    #db{
        fd = Fd,
        id_tree = DocInfoByIdBTree,
        seq_tree = DocInfoBySeqBTree,
        update_seq = LastSeq,
        header = Header,
        compression = Comp
        } = Db,
    DocLookups = couch_btree:lookup(DocInfoByIdBTree,
            [Id || {Id, _Revs} <- IdRevs]),

    NewDocInfos = lists:zipwith(
        fun({_Id, Revs}, {ok, #full_doc_info{rev_tree=Tree}=FullDocInfo}) ->
            case couch_key_tree:remove_leafs(Tree, Revs) of
            {_, []=_RemovedRevs} -> % no change
                nil;
            {NewTree, RemovedRevs} ->
                {FullDocInfo#full_doc_info{rev_tree=NewTree},RemovedRevs}
            end;
        (_, not_found) ->
            nil
        end,
        IdRevs, DocLookups),

    SeqsToRemove = [Seq
            || {#full_doc_info{update_seq=Seq},_} <- NewDocInfos],

    FullDocInfoToUpdate = [FullInfo
            || {#full_doc_info{rev_tree=Tree}=FullInfo,_}
            <- NewDocInfos, Tree /= []],

    IdRevsPurged = [{Id, Revs}
            || {#full_doc_info{id=Id}, Revs} <- NewDocInfos],

    {DocInfoToUpdate, NewSeq} = lists:mapfoldl(
        fun(#full_doc_info{rev_tree=Tree}=FullInfo, SeqAcc) ->
            Tree2 = couch_key_tree:map_leafs(
                fun(_RevId, Leaf) ->
                    Leaf#leaf{seq=SeqAcc+1}
                end, Tree),
            {FullInfo#full_doc_info{rev_tree=Tree2}, SeqAcc + 1}
        end, LastSeq, FullDocInfoToUpdate),

    IdsToRemove = [Id || {#full_doc_info{id=Id,rev_tree=[]},_}
            <- NewDocInfos],

    {ok, DocInfoBySeqBTree2} = couch_btree:add_remove(DocInfoBySeqBTree,
            DocInfoToUpdate, SeqsToRemove),
    {ok, DocInfoByIdBTree2} = couch_btree:add_remove(DocInfoByIdBTree,
            FullDocInfoToUpdate, IdsToRemove),
    {ok, Pointer, _} = couch_file:append_term(
            Fd, IdRevsPurged, [{compression, Comp}]),

    NewHeader = couch_db_header:set(Header, [
        {purge_seq, couch_db_header:purge_seq(Header) + 1},
        {purged_docs, Pointer}
    ]),
    Db2 = commit_data(
        Db#db{
            id_tree = DocInfoByIdBTree2,
            seq_tree = DocInfoBySeqBTree2,
            update_seq = NewSeq + 1,
            header=NewHeader}),

    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    couch_event:notify(Db#db.name, updated),
    {reply, {ok, couch_db_header:purge_seq(NewHeader), IdRevsPurged}, Db2,
        idle_limit()}.


handle_cast({load_validation_funs, ValidationFuns}, Db) ->
    Db2 = Db#db{validate_doc_funs = ValidationFuns},
    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    {noreply, Db2, idle_limit()};
handle_cast(start_compact, Db) ->
    case Db#db.compactor_pid of
    nil ->
        couch_log:info("Starting compaction for db \"~s\"", [Db#db.name]),
        Pid = spawn_link(fun() -> start_copy_compact(Db) end),
        Db2 = Db#db{compactor_pid=Pid},
        ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
        {noreply, Db2, idle_limit()};
    _ ->
        % compact currently running, this is a no-op
        {noreply, Db, idle_limit()}
    end;
handle_cast({compact_done, CompactFilepath}, #db{filepath=Filepath,fd=Fd}=Db) ->
    {ok, NewFd} = couch_file:open(CompactFilepath),
    {ok, NewHeader0} = couch_file:read_header(NewFd),
    NewHeader = couch_db_header:set(NewHeader0, [
        {compacted_seq, Db#db.update_seq}
    ]),
    #db{update_seq=NewSeq} = NewDb =
        init_db(Db#db.name, Filepath, NewFd, NewHeader, Db#db.options),
    unlink(NewFd),
    case Db#db.update_seq == NewSeq of
    true ->
        % suck up all the local docs into memory and write them to the new db
        {ok, _, LocalDocs} = couch_btree:foldl(Db#db.local_tree,
                fun(Value, _Offset, Acc) -> {ok, [Value | Acc]} end, []),
        {ok, NewLocalBtree} = couch_btree:add(NewDb#db.local_tree, LocalDocs),

        NewDb2 = commit_data(NewDb#db{
            local_tree = NewLocalBtree,
            main_pid = self(),
            filepath = Filepath,
            instance_start_time = Db#db.instance_start_time,
            revs_limit = Db#db.revs_limit
        }),

        {ok, Pre} = couch_file:bytes(Fd),
        {ok, Post} = couch_file:bytes(NewFd),

        couch_log:notice("Compaction swap for db: ~s ~p ~p", [Filepath,
                Pre, Post]),
        ok = file:rename(CompactFilepath, Filepath ++ ".compact"),
        RootDir = config:get("couchdb", "database_dir", "."),
        couch_file:delete(RootDir, Filepath),
        ok = file:rename(Filepath ++ ".compact", Filepath),
        % Delete the old meta compaction file after promoting
        % the compaction file.
        couch_file:delete(RootDir, Filepath ++ ".compact.meta"),
        close_db(Db),
        NewDb3 = refresh_validate_doc_funs(NewDb2),
        ok = gen_server:call(couch_server, {db_updated, NewDb3}, infinity),
        couch_event:notify(NewDb3#db.name, compacted),
        couch_log:info("Compaction for db \"~s\" completed.", [Db#db.name]),
        {noreply, NewDb3#db{compactor_pid=nil}, idle_limit()};
    false ->
        couch_log:info("Compaction file still behind main file "
                       "(update seq=~p. compact update seq=~p). Retrying.",
                       [Db#db.update_seq, NewSeq]),
        close_db(NewDb),
        Pid = spawn_link(fun() -> start_copy_compact(Db) end),
        Db2 = Db#db{compactor_pid=Pid},
        ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
        {noreply, Db2, idle_limit()}
    end;

handle_cast(wakeup, Db) ->
    {noreply, Db, idle_limit()};

handle_cast(Msg, #db{name = Name} = Db) ->
    couch_log:error("Database `~s` updater received unexpected cast: ~p",
                    [Name, Msg]),
    {stop, Msg, Db}.


handle_info({update_docs, Client, GroupedDocs, NonRepDocs, MergeConflicts,
        FullCommit}, Db) ->
    GroupedDocs2 = sort_and_tag_grouped_docs(Client, GroupedDocs),
    if NonRepDocs == [] ->
        {GroupedDocs3, Clients, FullCommit2} = collect_updates(GroupedDocs2,
                [Client], MergeConflicts, FullCommit);
    true ->
        GroupedDocs3 = GroupedDocs2,
        FullCommit2 = FullCommit,
        Clients = [Client]
    end,
    NonRepDocs2 = [{Client, NRDoc} || NRDoc <- NonRepDocs],
    try update_docs_int(Db, GroupedDocs3, NonRepDocs2, MergeConflicts,
                FullCommit2) of
    {ok, Db2, UpdatedDDocIds} ->
        ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
        if Db2#db.update_seq /= Db#db.update_seq ->
            couch_event:notify(Db2#db.name, updated);
        true -> ok
        end,
        if NonRepDocs2 /= [] ->
            couch_event:notify(Db2#db.name, local_updated);
        true -> ok
        end,
        [catch(ClientPid ! {done, self()}) || ClientPid <- Clients],
        Db3 = case length(UpdatedDDocIds) > 0 of
            true ->
                % Ken and ddoc_cache are the only things that
                % use the unspecified ddoc_updated message. We
                % should update them to use the new message per
                % ddoc.
                lists:foreach(fun(DDocId) ->
                    couch_event:notify(Db2#db.name, {ddoc_updated, DDocId})
                end, UpdatedDDocIds),
                couch_event:notify(Db2#db.name, ddoc_updated),
                ddoc_cache:refresh(Db2#db.name, UpdatedDDocIds),
                refresh_validate_doc_funs(Db2);
            false ->
                Db2
        end,
        {noreply, Db3, hibernate_if_no_idle_limit()}
    catch
        throw: retry ->
            [catch(ClientPid ! {retry, self()}) || ClientPid <- Clients],
            {noreply, Db, hibernate_if_no_idle_limit()}
    end;
handle_info(delayed_commit, #db{waiting_delayed_commit=nil}=Db) ->
    %no outstanding delayed commits, ignore
    {noreply, Db, idle_limit()};
handle_info(delayed_commit, Db) ->
    case commit_data(Db) of
        Db ->
            {noreply, Db, idle_limit()};
        Db2 ->
            ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
            {noreply, Db2, idle_limit()}
    end;
handle_info({'EXIT', _Pid, normal}, Db) ->
    {noreply, Db, idle_limit()};
handle_info({'EXIT', _Pid, Reason}, Db) ->
    {stop, Reason, Db};
handle_info({'DOWN', Ref, _, _, Reason}, #db{fd_monitor=Ref, name=Name} = Db) ->
    couch_log:error("DB ~s shutting down - Fd ~p", [Name, Reason]),
    {stop, normal, Db#db{fd=undefined, fd_monitor=closed}};
handle_info(timeout, #db{fd=Fd, name=DbName} = Db) ->
    IdleLimitMSec = update_idle_limit_from_config(),
    case couch_db:is_idle(Db) of
        true ->
            MSecSinceLastRead = couch_file:msec_since_last_read(Fd),
            case MSecSinceLastRead > IdleLimitMSec of
                true ->
                    ok = couch_server:close_db_if_idle(DbName);
                false ->
                    ok
            end;
        false ->
            ok
    end,
    % Send a message to wake up and then hibernate. Hibernation here is done to
    % force a thorough garbage collection.
    gen_server:cast(self(), wakeup),
    {noreply, Db, hibernate}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

sort_and_tag_grouped_docs(Client, GroupedDocs) ->
    % These groups should already be sorted but sometimes clients misbehave.
    % The merge_updates function will fail and the database can end up with
    % duplicate documents if the incoming groups are not sorted, so as a sanity
    % check we sort them again here. See COUCHDB-2735.
    Cmp = fun([#doc{id=A}|_], [#doc{id=B}|_]) -> A < B end,
    lists:map(fun(DocGroup) ->
        [{Client, maybe_tag_doc(D)} || D <- DocGroup]
    end, lists:sort(Cmp, GroupedDocs)).

maybe_tag_doc(#doc{id=Id, revs={Pos,[_Rev|PrevRevs]}, meta=Meta0}=Doc) ->
    case lists:keymember(ref, 1, Meta0) of
        true ->
            Doc;
        false ->
            Key = {Id, {Pos-1, PrevRevs}},
            Doc#doc{meta=[{ref, Key} | Meta0]}
    end.

merge_updates([[{_,#doc{id=X}}|_]=A|RestA], [[{_,#doc{id=X}}|_]=B|RestB]) ->
    [A++B | merge_updates(RestA, RestB)];
merge_updates([[{_,#doc{id=X}}|_]|_]=A, [[{_,#doc{id=Y}}|_]|_]=B) when X < Y ->
    [hd(A) | merge_updates(tl(A), B)];
merge_updates([[{_,#doc{id=X}}|_]|_]=A, [[{_,#doc{id=Y}}|_]|_]=B) when X > Y ->
    [hd(B) | merge_updates(A, tl(B))];
merge_updates([], RestB) ->
    RestB;
merge_updates(RestA, []) ->
    RestA.

collect_updates(GroupedDocsAcc, ClientsAcc, MergeConflicts, FullCommit) ->
    receive
        % Only collect updates with the same MergeConflicts flag and without
        % local docs. It's easier to just avoid multiple _local doc
        % updaters than deal with their possible conflicts, and local docs
        % writes are relatively rare. Can be optmized later if really needed.
        {update_docs, Client, GroupedDocs, [], MergeConflicts, FullCommit2} ->
            GroupedDocs2 = sort_and_tag_grouped_docs(Client, GroupedDocs),
            GroupedDocsAcc2 =
                merge_updates(GroupedDocsAcc, GroupedDocs2),
            collect_updates(GroupedDocsAcc2, [Client | ClientsAcc],
                    MergeConflicts, (FullCommit or FullCommit2))
    after 0 ->
        {GroupedDocsAcc, ClientsAcc, FullCommit}
    end.

rev_tree(DiskTree) ->
    couch_key_tree:map(fun
        (_RevId, {Del, Ptr, Seq}) ->
            #leaf{
                deleted = ?i2b(Del),
                ptr = Ptr,
                seq = Seq
            };
        (_RevId, {Del, Ptr, Seq, Size}) ->
            #leaf{
                deleted = ?i2b(Del),
                ptr = Ptr,
                seq = Seq,
                sizes = upgrade_sizes(Size)
            };
        (_RevId, {Del, Ptr, Seq, Sizes, Atts}) ->
            #leaf{
                deleted = ?i2b(Del),
                ptr = Ptr,
                seq = Seq,
                sizes = upgrade_sizes(Sizes),
                atts = Atts
            };
        (_RevId, ?REV_MISSING) ->
            ?REV_MISSING
    end, DiskTree).

disk_tree(RevTree) ->
    couch_key_tree:map(fun
        (_RevId, ?REV_MISSING) ->
            ?REV_MISSING;
        (_RevId, #leaf{} = Leaf) ->
            #leaf{
                deleted = Del,
                ptr = Ptr,
                seq = Seq,
                sizes = Sizes,
                atts = Atts
            } = Leaf,
            {?b2i(Del), Ptr, Seq, split_sizes(Sizes), Atts}
    end, RevTree).

upgrade_sizes(#size_info{}=SI) ->
    SI;
upgrade_sizes({D, E}) ->
    #size_info{active=D, external=E};
upgrade_sizes(S) when is_integer(S) ->
    #size_info{active=S, external=0}.

split_sizes(#size_info{}=SI) ->
    {SI#size_info.active, SI#size_info.external}.

join_sizes({Active, External}) when is_integer(Active), is_integer(External) ->
    #size_info{active=Active, external=External}.

btree_by_seq_split(#full_doc_info{}=Info) ->
    #full_doc_info{
        id = Id,
        update_seq = Seq,
        deleted = Del,
        sizes = SizeInfo,
        rev_tree = Tree
    } = Info,
    {Seq, {Id, ?b2i(Del), split_sizes(SizeInfo), disk_tree(Tree)}}.

btree_by_seq_join(Seq, {Id, Del, DiskTree}) when is_integer(Del) ->
    btree_by_seq_join(Seq, {Id, Del, {0, 0}, DiskTree});
btree_by_seq_join(Seq, {Id, Del, Sizes, DiskTree}) when is_integer(Del) ->
    #full_doc_info{
        id = Id,
        update_seq = Seq,
        deleted = ?i2b(Del),
        sizes = join_sizes(Sizes),
        rev_tree = rev_tree(DiskTree)
    };
btree_by_seq_join(KeySeq, {Id, RevInfos, DeletedRevInfos}) ->
    % Older versions stored #doc_info records in the seq_tree.
    % Compact to upgrade.
    #doc_info{
        id = Id,
        high_seq=KeySeq,
        revs =
            [#rev_info{rev=Rev,seq=Seq,deleted=false,body_sp = Bp} ||
                {Rev, Seq, Bp} <- RevInfos] ++
            [#rev_info{rev=Rev,seq=Seq,deleted=true,body_sp = Bp} ||
                {Rev, Seq, Bp} <- DeletedRevInfos]}.

btree_by_id_split(#full_doc_info{}=Info) ->
    #full_doc_info{
        id = Id,
        update_seq = Seq,
        deleted = Deleted,
        sizes = SizeInfo,
        rev_tree = Tree
    } = Info,
    {Id, {Seq, ?b2i(Deleted), split_sizes(SizeInfo), disk_tree(Tree)}}.

% Handle old formats before data_size was added
btree_by_id_join(Id, {HighSeq, Deleted, DiskTree}) ->
    btree_by_id_join(Id, {HighSeq, Deleted, #size_info{}, DiskTree});

btree_by_id_join(Id, {HighSeq, Deleted, Sizes, DiskTree}) ->
    #full_doc_info{
        id = Id,
        update_seq = HighSeq,
        deleted = ?i2b(Deleted),
        sizes = upgrade_sizes(Sizes),
        rev_tree = rev_tree(DiskTree)
    }.

btree_by_id_reduce(reduce, FullDocInfos) ->
    lists:foldl(
        fun(Info, {NotDeleted, Deleted, Sizes}) ->
            Sizes2 = reduce_sizes(Sizes, Info#full_doc_info.sizes),
            case Info#full_doc_info.deleted of
            true ->
                {NotDeleted, Deleted + 1, Sizes2};
            false ->
                {NotDeleted + 1, Deleted, Sizes2}
            end
        end,
        {0, 0, #size_info{}}, FullDocInfos);
btree_by_id_reduce(rereduce, Reds) ->
    lists:foldl(
        fun({NotDeleted, Deleted}, {AccNotDeleted, AccDeleted, _AccSizes}) ->
            % pre 1.2 format, will be upgraded on compaction
            {AccNotDeleted + NotDeleted, AccDeleted + Deleted, nil};
        ({NotDeleted, Deleted, Sizes}, {AccNotDeleted, AccDeleted, AccSizes}) ->
            AccSizes2 = reduce_sizes(AccSizes, Sizes),
            {AccNotDeleted + NotDeleted, AccDeleted + Deleted, AccSizes2}
        end,
        {0, 0, #size_info{}}, Reds).

reduce_sizes(nil, _) ->
    nil;
reduce_sizes(_, nil) ->
    nil;
reduce_sizes(#size_info{}=S1, #size_info{}=S2) ->
    #size_info{
        active = S1#size_info.active + S2#size_info.active,
        external = S1#size_info.external + S2#size_info.external
    };
reduce_sizes(S1, S2) ->
    reduce_sizes(upgrade_sizes(S1), upgrade_sizes(S2)).

btree_by_seq_reduce(reduce, DocInfos) ->
    % count the number of documents
    length(DocInfos);
btree_by_seq_reduce(rereduce, Reds) ->
    lists:sum(Reds).

init_db(DbName, Filepath, Fd, Header0, Options) ->
    Header = couch_db_header:upgrade(Header0),

    {ok, FsyncOptions} = couch_util:parse_term(
            config:get("couchdb", "fsync_options",
                    "[before_header, after_header, on_file_open]")),

    case lists:member(on_file_open, FsyncOptions) of
    true -> ok = couch_file:sync(Fd);
    _ -> ok
    end,

    Compression = couch_compress:get_compression_method(),

    IdTreeState = couch_db_header:id_tree_state(Header),
    SeqTreeState = couch_db_header:seq_tree_state(Header),
    LocalTreeState = couch_db_header:local_tree_state(Header),
    {ok, IdBtree} = couch_btree:open(IdTreeState, Fd,
        [{split, fun ?MODULE:btree_by_id_split/1},
        {join, fun ?MODULE:btree_by_id_join/2},
        {reduce, fun ?MODULE:btree_by_id_reduce/2},
        {compression, Compression}]),
    {ok, SeqBtree} = couch_btree:open(SeqTreeState, Fd,
            [{split, fun ?MODULE:btree_by_seq_split/1},
            {join, fun ?MODULE:btree_by_seq_join/2},
            {reduce, fun ?MODULE:btree_by_seq_reduce/2},
            {compression, Compression}]),
    {ok, LocalDocsBtree} = couch_btree:open(LocalTreeState, Fd,
        [{compression, Compression}]),
    case couch_db_header:security_ptr(Header) of
    nil ->
        Security = default_security_object(DbName),
        SecurityPtr = nil;
    SecurityPtr ->
        {ok, Security} = couch_file:pread_term(Fd, SecurityPtr)
    end,
    % convert start time tuple to microsecs and store as a binary string
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    StartTime = ?l2b(io_lib:format("~p",
            [(MegaSecs*1000000*1000000) + (Secs*1000000) + MicroSecs])),
    ok = couch_file:set_db_pid(Fd, self()),
    Db = #db{
        fd=Fd,
        fd_monitor = erlang:monitor(process, Fd),
        header=Header,
        id_tree = IdBtree,
        seq_tree = SeqBtree,
        local_tree = LocalDocsBtree,
        committed_update_seq = couch_db_header:update_seq(Header),
        update_seq = couch_db_header:update_seq(Header),
        name = DbName,
        filepath = Filepath,
        security = Security,
        security_ptr = SecurityPtr,
        instance_start_time = StartTime,
        revs_limit = couch_db_header:revs_limit(Header),
        fsync_options = FsyncOptions,
        options = Options,
        compression = Compression,
        before_doc_update = couch_util:get_value(before_doc_update, Options, nil),
        after_doc_read = couch_util:get_value(after_doc_read, Options, nil)
    },

    % If we just created a new UUID while upgrading a
    % database then we want to flush that to disk or
    % we risk sending out the uuid and having the db
    % crash which would result in it generating a new
    % uuid each time it was reopened.
    case Header /= Header0 of
        true ->
            sync_header(Db, Header);
        false ->
            Db
    end.


close_db(#db{fd_monitor = Ref}) ->
    erlang:demonitor(Ref).


refresh_validate_doc_funs(#db{name = <<"shards/", _/binary>> = Name} = Db) ->
    spawn(fabric, reset_validation_funs, [mem3:dbname(Name)]),
    Db#db{validate_doc_funs = undefined};
refresh_validate_doc_funs(Db0) ->
    Db = Db0#db{user_ctx=?ADMIN_USER},
    {ok, DesignDocs} = couch_db:get_design_docs(Db),
    ProcessDocFuns = lists:flatmap(
        fun(DesignDocInfo) ->
            {ok, DesignDoc} = couch_db:open_doc_int(
                Db, DesignDocInfo, [ejson_body]),
            case couch_doc:get_validate_doc_fun(DesignDoc) of
            nil -> [];
            Fun -> [Fun]
            end
        end, DesignDocs),
    Db#db{validate_doc_funs=ProcessDocFuns}.

% rev tree functions

flush_trees(_Db, [], AccFlushedTrees) ->
    {ok, lists:reverse(AccFlushedTrees)};
flush_trees(#db{fd = Fd} = Db,
        [InfoUnflushed | RestUnflushed], AccFlushed) ->
    #full_doc_info{update_seq=UpdateSeq, rev_tree=Unflushed} = InfoUnflushed,
    {Flushed, FinalAcc} = couch_key_tree:mapfold(
        fun(_Rev, Value, Type, SizesAcc) ->
            case Value of
            #doc{deleted = IsDeleted, body = {summary, _, _, _} = DocSummary} ->
                {summary, Summary, AttSizeInfo, AttsFd} = DocSummary,
                ExternalSize = get_meta_body_size(Value#doc.meta, Summary),
                % this node value is actually an unwritten document summary,
                % write to disk.
                % make sure the Fd in the written bins is the same Fd we are
                % and convert bins, removing the FD.
                % All bins should have been written to disk already.
                case {AttsFd, Fd} of
                {nil, _} ->
                    ok;
                {SameFd, SameFd} ->
                    ok;
                _ ->
                    % Fd where the attachments were written to is not the same
                    % as our Fd. This can happen when a database is being
                    % switched out during a compaction.
                    couch_log:debug("File where the attachments are written has"
                                    " changed. Possibly retrying.", []),
                    throw(retry)
                end,
                {ok, NewSummaryPointer, SummarySize} =
                    couch_file:append_raw_chunk(Fd, Summary),
                Leaf = #leaf{
                    deleted = IsDeleted,
                    ptr = NewSummaryPointer,
                    seq = UpdateSeq,
                    sizes = #size_info{
                        active = SummarySize,
                        external = ExternalSize
                    },
                    atts = AttSizeInfo
                },
                {Leaf, add_sizes(Type, Leaf, SizesAcc)};
            #leaf{} ->
                {Value, add_sizes(Type, Value, SizesAcc)};
            _ ->
                {Value, SizesAcc}
            end
        end, {0, 0, []}, Unflushed),
    {FinalAS, FinalES, FinalAtts} = FinalAcc,
    TotalAttSize = lists:foldl(fun({_, S}, A) -> S + A end, 0, FinalAtts),
    NewInfo = InfoUnflushed#full_doc_info{
        rev_tree = Flushed,
        sizes = #size_info{
            active = FinalAS + TotalAttSize,
            external = FinalES + TotalAttSize
        }
    },
    flush_trees(Db, RestUnflushed, [NewInfo | AccFlushed]).

add_sizes(Type, #leaf{sizes=Sizes, atts=AttSizes}, Acc) ->
    % Maybe upgrade from disk_size only
    #size_info{
        active = ActiveSize,
        external = ExternalSize
    } = upgrade_sizes(Sizes),
    {ASAcc, ESAcc, AttsAcc} = Acc,
    NewASAcc = ActiveSize + ASAcc,
    NewESAcc = ESAcc + if Type == leaf -> ExternalSize; true -> 0 end,
    NewAttsAcc = lists:umerge(AttSizes, AttsAcc),
    {NewASAcc, NewESAcc, NewAttsAcc}.

send_result(Client, Doc, NewResult) ->
    % used to send a result to the client
    catch(Client ! {result, self(), {doc_tag(Doc), NewResult}}).

doc_tag(#doc{meta=Meta}) ->
    case lists:keyfind(ref, 1, Meta) of
        {ref, Ref} -> Ref;
        false -> throw(no_doc_tag);
        Else -> throw({invalid_doc_tag, Else})
    end.

merge_rev_trees(_Limit, _Merge, [], [], AccNewInfos, AccRemoveSeqs, AccSeq) ->
    {ok, lists:reverse(AccNewInfos), AccRemoveSeqs, AccSeq};
merge_rev_trees(Limit, MergeConflicts, [NewDocs|RestDocsList],
        [OldDocInfo|RestOldInfo], AccNewInfos, AccRemoveSeqs, AccSeq) ->
    erlang:put(last_id_merged, OldDocInfo#full_doc_info.id), % for debugging
    NewDocInfo0 = lists:foldl(fun({Client, NewDoc}, OldInfoAcc) ->
        merge_rev_tree(OldInfoAcc, NewDoc, Client, Limit, MergeConflicts)
    end, OldDocInfo, NewDocs),
    % When MergeConflicts is false, we updated #full_doc_info.deleted on every
    % iteration of merge_rev_tree. However, merge_rev_tree does not update
    % #full_doc_info.deleted when MergeConflicts is true, since we don't need
    % to know whether the doc is deleted between iterations. Since we still
    % need to know if the doc is deleted after the merge happens, we have to
    % set it here.
    NewDocInfo1 = case MergeConflicts of
        true ->
            NewDocInfo0#full_doc_info{
                deleted = couch_doc:is_deleted(NewDocInfo0)
            };
        false ->
            NewDocInfo0
    end,
    if NewDocInfo1 == OldDocInfo ->
        % nothing changed
        merge_rev_trees(Limit, MergeConflicts, RestDocsList, RestOldInfo,
            AccNewInfos, AccRemoveSeqs, AccSeq);
    true ->
        % We have updated the document, give it a new update_seq. Its
        % important to note that the update_seq on OldDocInfo should
        % be identical to the value on NewDocInfo1.
        OldSeq = OldDocInfo#full_doc_info.update_seq,
        NewDocInfo2 = NewDocInfo1#full_doc_info{
            update_seq = AccSeq + 1
        },
        RemoveSeqs = case OldSeq of
            0 -> AccRemoveSeqs;
            _ -> [OldSeq | AccRemoveSeqs]
        end,
        merge_rev_trees(Limit, MergeConflicts, RestDocsList, RestOldInfo,
            [NewDocInfo2|AccNewInfos], RemoveSeqs, AccSeq+1)
    end.

merge_rev_tree(OldInfo, NewDoc, Client, Limit, false)
        when OldInfo#full_doc_info.deleted ->
    % We're recreating a document that was previously
    % deleted. To check that this is a recreation from
    % the root we assert that the new document has a
    % revision depth of 1 (this is to avoid recreating a
    % doc from a previous internal revision) and is also
    % not deleted. To avoid expanding the revision tree
    % unnecessarily we create a new revision based on
    % the winning deleted revision.

    {RevDepth, _} = NewDoc#doc.revs,
    NewDeleted = NewDoc#doc.deleted,
    case RevDepth == 1 andalso not NewDeleted of
        true ->
            % Update the new doc based on revisions in OldInfo
            #doc_info{revs=[WinningRev | _]} = couch_doc:to_doc_info(OldInfo),
            #rev_info{rev={OldPos, OldRev}} = WinningRev,
            NewRevId = couch_db:new_revid(NewDoc#doc{revs={OldPos, [OldRev]}}),
            NewDoc2 = NewDoc#doc{revs={OldPos + 1, [NewRevId, OldRev]}},

            % Merge our modified new doc into the tree
            #full_doc_info{rev_tree=OldTree} = OldInfo,
            NewTree0 = couch_doc:to_path(NewDoc2),
            case couch_key_tree:merge(OldTree, NewTree0, Limit) of
                {NewTree1, new_leaf} ->
                    % We changed the revision id so inform the caller
                    send_result(Client, NewDoc, {ok, {OldPos+1, NewRevId}}),
                    OldInfo#full_doc_info{
                        rev_tree = NewTree1,
                        deleted = false
                    };
                _ ->
                    throw(doc_recreation_failed)
            end;
        _ ->
            send_result(Client, NewDoc, conflict),
            OldInfo
    end;
merge_rev_tree(OldInfo, NewDoc, Client, Limit, false) ->
    % We're attempting to merge a new revision into an
    % undeleted document. To not be a conflict we require
    % that the merge results in extending a branch.

    OldTree = OldInfo#full_doc_info.rev_tree,
    NewTree0 = couch_doc:to_path(NewDoc),
    NewDeleted = NewDoc#doc.deleted,
    case couch_key_tree:merge(OldTree, NewTree0, Limit) of
        {NewTree, new_leaf} when not NewDeleted ->
            OldInfo#full_doc_info{
                rev_tree = NewTree,
                deleted = false
            };
        {NewTree, new_leaf} when NewDeleted ->
            % We have to check if we just deleted this
            % document completely or if it was a conflict
            % resolution.
            OldInfo#full_doc_info{
                rev_tree = NewTree,
                deleted = couch_doc:is_deleted(NewTree)
            };
        _ ->
            send_result(Client, NewDoc, conflict),
            OldInfo
    end;
merge_rev_tree(OldInfo, NewDoc, _Client, Limit, true) ->
    % We're merging in revisions without caring about
    % conflicts. Most likely this is a replication update.
    OldTree = OldInfo#full_doc_info.rev_tree,
    NewTree0 = couch_doc:to_path(NewDoc),
    {NewTree, _} = couch_key_tree:merge(OldTree, NewTree0, Limit),
    OldInfo#full_doc_info{rev_tree = NewTree}.

stem_full_doc_infos(#db{revs_limit=Limit}, DocInfos) ->
    [Info#full_doc_info{rev_tree=couch_key_tree:stem(Tree, Limit)} ||
            #full_doc_info{rev_tree=Tree}=Info <- DocInfos].

update_docs_int(Db, DocsList, NonRepDocs, MergeConflicts, FullCommit) ->
    #db{
        id_tree = DocInfoByIdBTree,
        seq_tree = DocInfoBySeqBTree,
        update_seq = LastSeq,
        revs_limit = RevsLimit
        } = Db,
    Ids = [Id || [{_Client, #doc{id=Id}}|_] <- DocsList],
    % lookup up the old documents, if they exist.
    OldDocLookups = couch_btree:lookup(DocInfoByIdBTree, Ids),
    OldDocInfos = lists:zipwith(
        fun(_Id, {ok, FullDocInfo}) ->
            FullDocInfo;
        (Id, not_found) ->
            #full_doc_info{id=Id}
        end,
        Ids, OldDocLookups),
    % Merge the new docs into the revision trees.
    {ok, NewFullDocInfos, RemoveSeqs, NewSeq} = merge_rev_trees(RevsLimit,
            MergeConflicts, DocsList, OldDocInfos, [], [], LastSeq),

    % All documents are now ready to write.

    {ok, Db2}  = update_local_docs(Db, NonRepDocs),

    % Write out the document summaries (the bodies are stored in the nodes of
    % the trees, the attachments are already written to disk)
    {ok, IndexFullDocInfos} = flush_trees(Db2, NewFullDocInfos, []),

    % and the indexes
    {ok, DocInfoByIdBTree2} = couch_btree:add_remove(DocInfoByIdBTree, IndexFullDocInfos, []),
    {ok, DocInfoBySeqBTree2} = couch_btree:add_remove(DocInfoBySeqBTree, IndexFullDocInfos, RemoveSeqs),


    WriteCount = length(IndexFullDocInfos),
    couch_stats:increment_counter([couchdb, document_inserts],
         WriteCount - length(RemoveSeqs)),
    couch_stats:increment_counter([couchdb, document_writes], WriteCount),
    couch_stats:increment_counter(
        [couchdb, local_document_writes],
        length(NonRepDocs)
    ),

    Db3 = Db2#db{
        id_tree = DocInfoByIdBTree2,
        seq_tree = DocInfoBySeqBTree2,
        update_seq = NewSeq},

    % Check if we just updated any design documents, and update the validation
    % funs if we did.
    UpdatedDDocIds = lists:flatmap(fun
        (<<"_design/", _/binary>> = Id) -> [Id];
        (_) -> []
    end, Ids),

    {ok, commit_data(Db3, not FullCommit), UpdatedDDocIds}.

update_local_docs(Db, []) ->
    {ok, Db};
update_local_docs(#db{local_tree=Btree}=Db, Docs) ->
    BtreeEntries = lists:map(
        fun({Client, NewDoc}) ->
            #doc{
                id = Id,
                deleted = Delete,
                revs = {0, PrevRevs},
                body = Body
            } = NewDoc,
            case PrevRevs of
            [RevStr|_] ->
                PrevRev = list_to_integer(?b2l(RevStr));
            [] ->
                PrevRev = 0
            end,
            case Delete of
                false ->
                    send_result(Client, NewDoc, {ok,
                        {0, ?l2b(integer_to_list(PrevRev + 1))}}),
                    {update, {Id, {PrevRev + 1, Body}}};
                true  ->
                    send_result(Client, NewDoc,
                        {ok, {0, <<"0">>}}),
                    {remove, Id}
            end
        end, Docs),

    BtreeIdsRemove = [Id || {remove, Id} <- BtreeEntries],
    BtreeIdsUpdate = [{Key, Val} || {update, {Key, Val}} <- BtreeEntries],

    {ok, Btree2} =
        couch_btree:add_remove(Btree, BtreeIdsUpdate, BtreeIdsRemove),

    {ok, Db#db{local_tree = Btree2}}.

db_to_header(Db, Header) ->
    couch_db_header:set(Header, [
        {update_seq, Db#db.update_seq},
        {seq_tree_state, couch_btree:get_state(Db#db.seq_tree)},
        {id_tree_state, couch_btree:get_state(Db#db.id_tree)},
        {local_tree_state, couch_btree:get_state(Db#db.local_tree)},
        {security_ptr, Db#db.security_ptr},
        {revs_limit, Db#db.revs_limit}
    ]).

commit_data(Db) ->
    commit_data(Db, false).

commit_data(#db{waiting_delayed_commit=nil} = Db, true) ->
    TRef = erlang:send_after(1000,self(),delayed_commit),
    Db#db{waiting_delayed_commit=TRef};
commit_data(Db, true) ->
    Db;
commit_data(Db, _) ->
    #db{
        header = OldHeader,
        waiting_delayed_commit = Timer
    } = Db,
    if is_reference(Timer) -> erlang:cancel_timer(Timer); true -> ok end,
    case db_to_header(Db, OldHeader) of
        OldHeader -> Db#db{waiting_delayed_commit=nil};
        NewHeader -> sync_header(Db, NewHeader)
    end.

sync_header(Db, NewHeader) ->
    #db{
        fd = Fd,
        filepath = FilePath,
        fsync_options = FsyncOptions,
        waiting_delayed_commit = Timer
    } = Db,

    if is_reference(Timer) -> erlang:cancel_timer(Timer); true -> ok end,

    Before = lists:member(before_header, FsyncOptions),
    After = lists:member(after_header, FsyncOptions),

    if Before -> couch_file:sync(FilePath); true -> ok end,
    ok = couch_file:write_header(Fd, NewHeader),
    if After -> couch_file:sync(FilePath); true -> ok end,

    Db#db{
        header=NewHeader,
        committed_update_seq=Db#db.update_seq,
        waiting_delayed_commit=nil
    }.


merge_lookups(Infos, []) ->
    Infos;
merge_lookups([], _) ->
    [];
merge_lookups([#doc_info{}=DI | RestInfos], [{ok, FDI} | RestLookups]) ->
    % Assert we've matched our lookups
    if DI#doc_info.id == FDI#full_doc_info.id -> ok; true ->
        erlang:error({mismatched_doc_infos, DI#doc_info.id})
    end,
    [FDI | merge_lookups(RestInfos, RestLookups)];
merge_lookups([FDI | RestInfos], Lookups) ->
    [FDI | merge_lookups(RestInfos, Lookups)].

check_md5(Md5, Md5) -> ok;
check_md5(_, _) -> throw(md5_mismatch).


-ifdef(TEST).
-define(COMP_EVENT(Name),
        couch_db_updater_ev:event(Name)).
-else.
-define(COMP_EVENT(Name), ignore).
-endif.


start_copy_compact(#db{}=Db) ->
    erlang:put(io_priority, {db_compact, Db#db.name}),
    couch_log:debug("Compaction process spawned for db \"~s\"", [Db#db.name]),

    ?COMP_EVENT(init),
    {ok, InitCompSt} = open_compaction_files(Db),
    ?COMP_EVENT(files_opened),

    Stages = [
        fun copy_purge_info/1,
        fun copy_compact/1,
        fun commit_compaction_data/1,
        fun copy_doc_ids/1,
        fun compact_final_sync/1,
        fun verify_compaction/1
    ],

    FinalCompSt = lists:foldl(fun(Stage, CompSt) ->
        Stage(CompSt)
    end, InitCompSt, Stages),

    #comp_st{
        new_db = FinalNewDb,
        docid_fd = DocIdFd
    } = FinalCompSt,

    close_db(FinalNewDb),
    ok = couch_file:close(DocIdFd),

    ?COMP_EVENT(before_notify),
    gen_server:cast(Db#db.main_pid, {compact_done, FinalNewDb#db.filepath}).


open_compaction_files(OldDb) ->
    #db{
        name = DbName,
        filepath = DbFilePath,
        options = Options,
        header = SrcHdr
    } = OldDb,

    DataFile = DbFilePath ++ ".compact.data",
    DocIdFile = DbFilePath ++ ".compact.meta",
    {ok, DataFd, DataHdr} = open_compaction_file(DataFile),
    {ok, DocIdFd, DocIdHdr} = open_compaction_file(DocIdFile),
    DataHdrIsDbHdr = couch_db_header:is_header(DataHdr),
    InitCompSt = case {DataHdr, DocIdHdr} of
        {#comp_header{}=A, #comp_header{}=A} ->
            % We're restarting a compaction that did not finish
            % before trying to swap out with the original db
            DbHeader = A#comp_header.db_header,
            DocIdSt = A#comp_header.docid_st,
            NewDb = init_db(DbName, DataFile, DataFd, DbHeader, Options),
            CompSt = #comp_st{
                old_db = OldDb,
                new_db = NewDb,
                docid_fd = DocIdFd,
                docid_st = DocIdSt,
                retry = check_is_retry_compaction(NewDb)
            },
            open_docids(CompSt);
        _ when DataHdrIsDbHdr ->
            % We tried to swap out the compaction but there were
            % writes to the database during compaction. Start
            % a compaction retry.
            ok = reset_compaction_file(DocIdFd, couch_db_header:from(SrcHdr)),
            NewDb = init_db(DbName, DataFile, DataFd, DataHdr, Options),
            CompSt = #comp_st{
                old_db = OldDb,
                new_db = NewDb,
                docid_fd = DocIdFd,
                retry = check_is_retry_compaction(NewDb)
            },
            open_docids(CompSt);
        _ ->
            % We're starting a compaction from scratch
            Header = couch_db_header:from(SrcHdr),
            ok = reset_compaction_file(DataFd, Header),
            ok = reset_compaction_file(DocIdFd, Header),
            NewDb = init_db(DbName, DataFile, DataFd, Header, Options),
            CompSt = #comp_st{
                old_db = OldDb,
                new_db = NewDb,
                docid_fd = DocIdFd,
                retry = false
            },
            open_docids(CompSt)
    end,
    unlink(DataFd),
    erlang:monitor(process, DocIdFd),
    {ok, InitCompSt}.


open_compaction_file(FilePath) ->
    case couch_file:open(FilePath, [nologifmissing]) of
        {ok, Fd} ->
            case couch_file:read_header(Fd) of
                {ok, Header} -> {ok, Fd, Header};
                no_valid_header -> {ok, Fd, nil}
            end;
        {error, enoent} ->
            {ok, Fd} = couch_file:open(FilePath, [create]),
            {ok, Fd, nil}
    end.


check_is_retry_compaction(Db) ->
    {ok, Reds} = couch_btree:full_reduce(Db#db.id_tree),
    element(1, Reds) + element(2, Reds) > 0.


open_docids(#comp_st{} = CompSt) ->
    #comp_st{
        docid_fd = DocIdFd,
        docid_st = DocIdSt,
        retry = Retry
    } = CompSt,
    DocIdMod = case Retry of
        true -> couch_emsort;
        false -> couch_ehamt
    end,
    Opts = case DocIdSt of
        undefined -> [];
        _ -> [{root, DocIdSt}]
    end,
    {ok, NewSt} = DocIdMod:open(DocIdFd, Opts),
    CompSt#comp_st{
        docid_mod = DocIdMod,
        docid_st = NewSt
    }.


reset_compaction_file(Fd, Header) ->
    ok = couch_file:truncate(Fd, 0),
    ok = couch_file:write_header(Fd, Header).


copy_purge_info(#comp_st{old_db = OldDb, new_db = NewDb} = CompSt) ->
    ?COMP_EVENT(purge_init),
    OldHdr = OldDb#db.header,
    NewHdr = NewDb#db.header,
    OldPurgeSeq = couch_db_header:purge_seq(OldHdr),
    NewPurgeSeq = couch_db_header:purge_seq(NewHdr),
    if OldPurgeSeq > NewPurgeSeq ->
        {ok, PurgedIdsRevs} = couch_db:get_last_purged(OldDb),
        Opts = [{compression, NewDb#db.compression}],
        {ok, Ptr, _} = couch_file:append_term(NewDb#db.fd, PurgedIdsRevs, Opts),
        ?COMP_EVENT(purge_done),
        CompSt#comp_st{
            new_db = NewDb#db{
                header = couch_db_header:set(NewHdr, [
                    {purge_seq, OldPurgeSeq},
                    {purged_docs, Ptr}
                ])
            }
        };
    true ->
        ?COMP_EVENT(purge_done),
        CompSt
    end.


-record(comp_acc, {
    comp_st,
    fdis,
    uncopied,
    copied,
    buffer_size,
    checkpoint_after
}).


copy_compact(#comp_st{} = CompSt) ->
    #comp_st{
        old_db = OldDb,
        new_db = NewDb,
        retry = Retry
    } = CompSt,
    TotalChanges = couch_db:count_changes_since(OldDb, NewDb#db.update_seq),
    BufferSize = list_to_integer(
        config:get("database_compaction", "doc_buffer_size", "524288")),
    CheckpointAfter = couch_util:to_integer(
        config:get("database_compaction", "checkpoint_after",
            BufferSize * 10)),

    TaskProps0 = [
        {type, database_compaction},
        {phase, copy_docs},
        {retry, Retry},
        {database, OldDb#db.name},
        {progress, 0},
        {changes_done, 0},
        {total_changes, TotalChanges}
    ],
    case Retry and couch_task_status:is_task_added() of
    true ->
        couch_task_status:update([
            {phase, copy_docs},
            {retry, true},
            {progress, 0},
            {changes_done, 0},
            {total_changes, TotalChanges}
        ]);
    false ->
        couch_task_status:add_task(TaskProps0),
        couch_task_status:set_update_frequency(500)
    end,

    ?COMP_EVENT(seq_init),

    SeqTree = OldDb#db.seq_tree,
    EnumBySeqFun = fun copy_enum_by_seq/3,
    InitAcc = #comp_acc{
        comp_st = CompSt,
        fdis = [],
        uncopied = 0,
        copied = 0,
        buffer_size = BufferSize,
        checkpoint_after = CheckpointAfter
    },
    FoldOpts = [{start_key, NewDb#db.update_seq + 1}],
    {ok, _, FinalAcc} =
            couch_btree:foldl(SeqTree, EnumBySeqFun, InitAcc, FoldOpts),

    #comp_acc{
        comp_st = NewCompSt1,
        fdis = FDIs
    } = FinalAcc,
    NewCompSt2 = copy_docs(NewCompSt1, lists:reverse(FDIs)),

    #comp_st{
        new_db = NewDb1
    } = NewCompSt2,

    ?COMP_EVENT(seq_done),

    % copy misc header values
    if NewDb1#db.security /= OldDb#db.security ->
        {ok, Ptr, _} = couch_file:append_term(
            NewDb1#db.fd, OldDb#db.security,
            [{compression, NewDb1#db.compression}]),
        NewDb2 = NewDb1#db{security=OldDb#db.security, security_ptr=Ptr};
    true ->
        NewDb2 = NewDb1
    end,

    NewCompSt2#comp_st{
        new_db = NewDb2#db{
            update_seq = OldDb#db.update_seq
        }
    }.


copy_enum_by_seq(DocInfo, _Offset, #comp_acc{} = AccIn) ->
    #comp_acc{
        comp_st = CompSt,
        fdis = FDIs,
        uncopied = Uncopied,
        copied = Copied,
        buffer_size = BufferSize,
        checkpoint_after = CheckpointAfter
    } = AccIn,

    #comp_st{
        new_db = NewDb
    } = CompSt,

    Seq = case DocInfo of
        #full_doc_info{} -> DocInfo#full_doc_info.update_seq;
        #doc_info{} -> DocInfo#doc_info.high_seq
    end,

    NewUncopied = Uncopied + ?term_size(DocInfo),
    NewAcc = case NewUncopied >= BufferSize of
        true ->
            ToFlush = lists:reverse(FDIs, [DocInfo]),
            NewCompSt = copy_docs(CompSt, ToFlush),
            case Copied + NewUncopied >= CheckpointAfter of
                true ->
                    TmpCompSt = NewCompSt#comp_st{
                        new_db = NewDb#db{update_seq = Seq}
                    },
                    #comp_acc{
                        comp_st = commit_compaction_data(TmpCompSt),
                        fdis = [],
                        uncopied = 0,
                        copied = 0
                    };
                false ->
                    #comp_acc{
                        comp_st = NewCompSt,
                        fdis = [],
                        uncopied = 0,
                        copied = Copied + NewUncopied
                    }
            end;
        false ->
            AccIn#comp_acc{
                fdis = [DocInfo | FDIs],
                uncopied = NewUncopied
            }
    end,
    {ok, NewAcc}.


copy_docs(CompSt, MixedInfos) ->
    #comp_st{
        old_db = Db,
        new_db = NewDb,
        docid_mod = DocIdMod,
        docid_st = DocIdSt
    } = CompSt,
    #db{
        fd = DestFd
    } = NewDb,
    DocInfoIds = [Id || #doc_info{id=Id} <- MixedInfos],
    LookupResults = couch_btree:lookup(Db#db.id_tree, DocInfoIds),
    % COUCHDB-968, make sure we prune duplicates during compaction
    NewInfos0 = lists:usort(fun(#full_doc_info{id=A}, #full_doc_info{id=B}) ->
        A =< B
    end, merge_lookups(MixedInfos, LookupResults)),

    NewInfos1 = lists:map(fun(Info) ->
        {NewRevTree, FinalAcc} = couch_key_tree:mapfold(fun
            (_Rev, #leaf{ptr=Sp}=Leaf, leaf, SizesAcc) ->
                {Body, AttInfos} = copy_doc_attachments(Db, Sp, DestFd),
                % In the future, we should figure out how to do this for
                % upgrade purposes.
                EJsonBody = case is_binary(Body) of
                    true ->
                        couch_compress:decompress(Body);
                    false ->
                        Body
                end,
                SummaryChunk = make_doc_summary(NewDb, {Body, AttInfos}),
                ExternalSize = ?term_size(EJsonBody),
                {ok, Pos, SummarySize} = couch_file:append_raw_chunk(
                    DestFd, SummaryChunk),
                AttSizes = [{element(3,A), element(4,A)} || A <- AttInfos],
                NewLeaf = Leaf#leaf{
                    ptr = Pos,
                    sizes = #size_info{
                        active = SummarySize,
                        external = ExternalSize
                    },
                    atts = AttSizes
                },
                {NewLeaf, add_sizes(leaf, NewLeaf, SizesAcc)};
            (_Rev, _Leaf, branch, SizesAcc) ->
                {?REV_MISSING, SizesAcc}
        end, {0, 0, []}, Info#full_doc_info.rev_tree),
        {FinalAS, FinalES, FinalAtts} = FinalAcc,
        TotalAttSize = lists:foldl(fun({_, S}, A) -> S + A end, 0, FinalAtts),
        NewActiveSize = FinalAS + TotalAttSize,
        NewExternalSize = FinalES + TotalAttSize,
        ?COMP_EVENT(seq_copy),
        Info#full_doc_info{
            rev_tree = NewRevTree,
            sizes = #size_info{
                active = NewActiveSize,
                external = NewExternalSize
            }
        }
    end, NewInfos0),

    NewInfos = stem_full_doc_infos(Db, NewInfos1),
    RemoveSeqs =
    case CompSt#comp_st.retry of
    true ->
        NewDocIdTree = NewDb#db.id_tree,
        % Compaction is being rerun to catch up to writes during the
        % first pass. This means we may have docs that already exist
        % in the seq_tree in the .data file. Here we lookup any old
        % update_seqs so that they can be removed.
        Ids = [Id || #full_doc_info{id=Id} <- NewInfos],
        Existing = couch_btree:lookup(NewDocIdTree, Ids),
        [Seq || {ok, #full_doc_info{update_seq=Seq}} <- Existing];
    false ->
        []
    end,

    {ok, SeqTree} = couch_btree:add_remove(
            NewDb#db.seq_tree, NewInfos, RemoveSeqs),

    FDIKVs = lists:map(fun(#full_doc_info{id=Id, update_seq=Seq}=FDI) ->
        {{Id, Seq}, FDI}
    end, NewInfos),
    {ok, NewDocIdSt} = DocIdMod:add(DocIdSt, FDIKVs),
    update_compact_task(length(NewInfos)),
    CompSt#comp_st{
        new_db = NewDb#db{seq_tree = SeqTree},
        docid_st = NewDocIdSt
    }.


copy_doc_attachments(#db{fd = SrcFd} = SrcDb, SrcSp, DestFd) ->
    {ok, {BodyData, BinInfos0}} = couch_db:read_doc(SrcDb, SrcSp),
    BinInfos = case BinInfos0 of
    _ when is_binary(BinInfos0) ->
        couch_compress:decompress(BinInfos0);
    _ when is_list(BinInfos0) ->
        % pre 1.2 file format
        BinInfos0
    end,
    % copy the bin values
    NewBinInfos = lists:map(
        fun({Name, Type, BinSp, AttLen, RevPos, ExpectedMd5}) ->
            % 010 UPGRADE CODE
            {NewBinSp, AttLen, AttLen, ActualMd5, _IdentityMd5} =
                couch_stream:copy_to_new_stream(SrcFd, BinSp, DestFd),
            check_md5(ExpectedMd5, ActualMd5),
            {Name, Type, NewBinSp, AttLen, AttLen, RevPos, ExpectedMd5, identity};
        ({Name, Type, BinSp, AttLen, DiskLen, RevPos, ExpectedMd5, Enc1}) ->
            {NewBinSp, AttLen, _, ActualMd5, _IdentityMd5} =
                couch_stream:copy_to_new_stream(SrcFd, BinSp, DestFd),
            check_md5(ExpectedMd5, ActualMd5),
            Enc = case Enc1 of
            true ->
                % 0110 UPGRADE CODE
                gzip;
            false ->
                % 0110 UPGRADE CODE
                identity;
            _ ->
                Enc1
            end,
            {Name, Type, NewBinSp, AttLen, DiskLen, RevPos, ExpectedMd5, Enc}
        end, BinInfos),
    {BodyData, NewBinInfos}.


commit_compaction_data(#comp_st{} = CompSt) ->
    % Compaction needs to write headers to both the data file
    % and the docid file so if we need to restart we can pick
    % back up from where we left off.
    #comp_st{
        new_db = NewDb,
        docid_fd = DocIdFd,
        docid_mod = DocIdMod,
        docid_st = DocIdSt
    } = CompSt,
    #db{
        header = OldHeader
    } = NewDb,
    NewHeader = db_to_header(NewDb, OldHeader),
    CompHeader = #comp_header{
        db_header = NewHeader,
        docid_st = DocIdMod:get_state(DocIdSt)
    },
    sync_compaction_data(DocIdFd, CompHeader),
    sync_compaction_data(NewDb#db.fd, CompHeader),
    CompSt#comp_st{
        new_db = NewDb#db{
            waiting_delayed_commit = nil,
            header = NewHeader,
            committed_update_seq = NewDb#db.update_seq
        }
    }.


sync_compaction_data(Fd, #comp_header{} = CompHeader) ->
    ok = couch_file:sync(Fd),
    ok = couch_file:write_header(Fd, CompHeader).


copy_doc_ids(#comp_st{retry = false} = CompSt) ->
    copy_from_tree(CompSt);
copy_doc_ids(#comp_st{retry = true} = CompSt) ->
    Stages = [
        fun sort_meta_data/1,
        fun commit_compaction_data/1,
        fun copy_meta_data/1
    ],
    lists:foldl(fun(Stage, St) -> Stage(St) end, CompSt, Stages).


copy_from_tree(#comp_st{} = CompSt) ->
    #comp_st{
        old_db = OldDb,
        new_db = NewDb,
        docid_st = DocIdSt
    } = CompSt,
    #db{
        fd = OldFd,
        header = OldHdr
    } = OldDb,

    TotalChanges = couch_db:count_changes_since(OldDb, 0),
    couch_task_status:update([
        {phase, copy_doc_ids},
        {progress, 0},
        {changes_done, 0},
        {total_changes, TotalChanges}
    ]),

    % We're reopening a custom view of the id_tree to
    % avoid the work of creating complete `#full_doc_info{}`
    % records.
    Compression = couch_compress:get_compression_method(),
    OldIdTreeState = couch_btree:get_state(OldDb#db.id_tree),
    {ok, OldIdTree} = couch_btree:open(OldIdTreeState, OldFd, [
        {split, fun ?MODULE:btree_by_id_split/1},
        {join, fun compact_id_join/2},
        {reduce, fun ?MODULE:btree_by_id_reduce/2},
        {compression, Compression}
    ]),

    NewIdTree = NewDb#db.id_tree,

    EnumByIdFun = fun({DocId, UpdateSeq}, _, {DstIdTree, Batch, Count}) ->
        case Count >= 1000 of
            true ->
                DstIdTree2 = flush_docid_batch(Batch, DocIdSt, DstIdTree),
                update_compact_task(Count),
                {ok, {DstIdTree2, [{DocId, UpdateSeq}], 1}};
            false ->
                {ok, {DstIdTree, [{DocId, UpdateSeq} | Batch], Count + 1}}
        end
    end,

    ?COMP_EVENT(id_init),
    {ok, _, {NewIdTree2, LastBatch, LastCount}} =
        couch_btree:foldl(OldIdTree, EnumByIdFun, {NewIdTree, [], 0}, []),
    NewIdTree3 = flush_docid_batch(LastBatch, DocIdSt, NewIdTree2),
    ?COMP_EVENT(id_done),
    update_compact_task(LastCount),
    CompSt#comp_st{
        new_db = NewDb#db{id_tree = NewIdTree3}
    }.


flush_docid_batch([], _, IdTree) ->
    IdTree;

flush_docid_batch(DocIds, EHamt, IdTree) ->
    FDIs = lists:map(fun(DocIdSeq) ->
        {ok, #full_doc_info{} = FDI} = couch_ehamt:lookup(EHamt, DocIdSeq),
        ?COMP_EVENT(id_copy),
        FDI
    end, DocIds),
    {ok, NewIdTree} = couch_btree:add(IdTree, FDIs),
    NewIdTree.


compact_id_join(Id, {HighSeq, _, _}) ->
    {Id, HighSeq};
compact_id_join(Id, {HighSeq, _, _, _}) ->
    {Id, HighSeq}.


sort_meta_data(#comp_st{docid_st = DocIdSt} = CompSt) ->
    ?COMP_EVENT(md_sort_init),
    {ok, NewSt} = couch_emsort:merge(DocIdSt),
    ?COMP_EVENT(md_sort_done),
    CompSt#comp_st{
        docid_st = NewSt
    }.


copy_meta_data(#comp_st{new_db = Db} = CompSt) ->
    #db{
        id_tree = IdTree0
    } = Db,
    Src = CompSt#comp_st.docid_st,
    {ok, Iter} = couch_emsort:iter(Src),
    Acc0 = #merge_st{
        id_tree=IdTree0,
        seq_tree=Db#db.seq_tree,
        rem_seqs=[],
        infos=[]
    },
    ?COMP_EVENT(md_copy_init),
    Acc = merge_docids(Iter, Acc0),
    {ok, IdTree} = couch_btree:add(Acc#merge_st.id_tree, Acc#merge_st.infos),
    {ok, SeqTree} = couch_btree:add_remove(
        Acc#merge_st.seq_tree, [], Acc#merge_st.rem_seqs
    ),
    ?COMP_EVENT(md_copy_done),
    CompSt#comp_st{
        new_db = Db#db{
            id_tree = IdTree,
            seq_tree = SeqTree
        }
    }.


compact_final_sync(#comp_st{new_db = NewDb0} = CompSt) ->
    ?COMP_EVENT(before_final_sync),
    NewHdr = db_to_header(NewDb0, NewDb0#db.header),
    NewDb1 = sync_header(NewDb0, NewHdr),
    ?COMP_EVENT(after_final_sync),
    CompSt#comp_st{
        new_db = NewDb1
    }.


verify_compaction(#comp_st{old_db = OldDb, new_db = NewDb} = CompSt) ->
    {ok, OldIdReds0} = couch_btree:full_reduce(OldDb#db.id_tree),
    {ok, OldSeqReds} = couch_btree:full_reduce(OldDb#db.seq_tree),
    {ok, NewIdReds0} = couch_btree:full_reduce(NewDb#db.id_tree),
    {ok, NewSeqReds} = couch_btree:full_reduce(NewDb#db.seq_tree),
    {
        OldDocCount,
        OldDelDocCount,
        #size_info{external = OldExternalSize}
    } = OldIdReds0,
    OldIdReds = {OldDocCount, OldDelDocCount, OldExternalSize},
    {
        NewDocCount,
        NewDelDocCount,
        #size_info{external = NewExternalSize}
    } = NewIdReds0,
    NewIdReds = {NewDocCount, NewDelDocCount, NewExternalSize},
    if NewIdReds == OldIdReds -> ok; true ->
        Fmt1 = "Compacted id tree for ~s differs from source: ~p /= ~p",
        couch_log:error(Fmt1, [couch_db:name(OldDb), NewIdReds, OldIdReds]),
        exit({compaction_error, id_tree})
    end,
    if NewSeqReds == OldSeqReds -> ok; true ->
        Fmt2 = "Compacted seq tree for ~s differs from source: ~p /= ~p",
        couch_log:error(Fmt2, [couch_db:name(OldDb), NewSeqReds, OldSeqReds]),
        exit({compaction_error, seq_tree})
    end,
    CompSt.


merge_docids(Iter, #merge_st{infos=Infos}=Acc) when length(Infos) > 1000 ->
    #merge_st{
        id_tree=IdTree0,
        seq_tree=SeqTree0,
        rem_seqs=RemSeqs
    } = Acc,
    {ok, IdTree1} = couch_btree:add(IdTree0, Infos),
    {ok, SeqTree1} = couch_btree:add_remove(SeqTree0, [], RemSeqs),
    Acc1 = Acc#merge_st{
        id_tree=IdTree1,
        seq_tree=SeqTree1,
        rem_seqs=[],
        infos=[]
    },
    merge_docids(Iter, Acc1);
merge_docids(Iter, #merge_st{curr=Curr}=Acc) ->
    case next_info(Iter, Curr, []) of
        {NextIter, NewCurr, FDI, Seqs} ->
            Acc1 = Acc#merge_st{
                infos = [FDI | Acc#merge_st.infos],
                rem_seqs = Seqs ++ Acc#merge_st.rem_seqs,
                curr = NewCurr
            },
            ?COMP_EVENT(md_copy_row),
            merge_docids(NextIter, Acc1);
        {finished, FDI, Seqs} ->
            Acc#merge_st{
                infos = [FDI | Acc#merge_st.infos],
                rem_seqs = Seqs ++ Acc#merge_st.rem_seqs,
                curr = undefined
            };
        empty ->
            Acc
    end.


next_info(Iter, undefined, []) ->
    case couch_emsort:next(Iter) of
        {ok, {{Id, Seq}, FDI}, NextIter} ->
            next_info(NextIter, {Id, Seq, FDI}, []);
        finished ->
            empty
    end;
next_info(Iter, {Id, Seq, FDI}, Seqs) ->
    case couch_emsort:next(Iter) of
        {ok, {{Id, NSeq}, NFDI}, NextIter} ->
            next_info(NextIter, {Id, NSeq, NFDI}, [Seq | Seqs]);
        {ok, {{NId, NSeq}, NFDI}, NextIter} ->
            {NextIter, {NId, NSeq, NFDI}, FDI, Seqs};
        finished ->
            {finished, FDI, Seqs}
    end.


update_compact_task(NumChanges) ->
    [Changes, Total] = couch_task_status:get([changes_done, total_changes]),
    Changes2 = Changes + NumChanges,
    Progress = case Total of
    0 ->
        0;
    _ ->
        (Changes2 * 100) div Total
    end,
    couch_task_status:update([{changes_done, Changes2}, {progress, Progress}]).


make_doc_summary(#db{compression = Comp}, {Body0, Atts0}) ->
    Body = case couch_compress:is_compressed(Body0, Comp) of
    true ->
        Body0;
    false ->
        % pre 1.2 database file format
        couch_compress:compress(Body0, Comp)
    end,
    Atts = case couch_compress:is_compressed(Atts0, Comp) of
    true ->
        Atts0;
    false ->
        couch_compress:compress(Atts0, Comp)
    end,
    SummaryBin = ?term_to_bin({Body, Atts}),
    couch_file:assemble_file_chunk(SummaryBin, crypto:hash(md5, SummaryBin)).


get_meta_body_size(Meta, Summary) ->
    case lists:keyfind(ejson_size, 1, Meta) of
        {ejson_size, ExternalSize} ->
            ExternalSize;
        false ->
            ?term_size(couch_compress:decompress(Summary))
    end.


default_security_object(<<"shards/", _/binary>>) ->
    case config:get("couchdb", "default_security", "everyone") of
        "admin_only" ->
            [{<<"members">>,{[{<<"roles">>,[<<"_admin">>]}]}},
             {<<"admins">>,{[{<<"roles">>,[<<"_admin">>]}]}}];
        Everyone when Everyone == "everyone"; Everyone == "admin_local" ->
            []
    end;
default_security_object(_DbName) ->
    case config:get("couchdb", "default_security", "everyone") of
        Admin when Admin == "admin_only"; Admin == "admin_local" ->
            [{<<"members">>,{[{<<"roles">>,[<<"_admin">>]}]}},
             {<<"admins">>,{[{<<"roles">>,[<<"_admin">>]}]}}];
        "everyone" ->
            []
    end.

% These functions rely on using the process dictionary. This is
% usually frowned upon however in this case it is done to avoid
% changing to a different server state record. Once PSE (Pluggable
% Storage Engine) code lands this should be moved to the #db{} record.
update_idle_limit_from_config() ->
    Default = integer_to_list(?IDLE_LIMIT_DEFAULT),
    IdleLimit = case config:get("couchdb", "idle_check_timeout", Default) of
        "infinity" ->
            infinity;
        Milliseconds ->
            list_to_integer(Milliseconds)
    end,
    put(idle_limit, IdleLimit),
    IdleLimit.

idle_limit() ->
    get(idle_limit).

hibernate_if_no_idle_limit() ->
    case idle_limit() of
        infinity ->
            hibernate;
        Timeout when is_integer(Timeout) ->
            Timeout
    end.
