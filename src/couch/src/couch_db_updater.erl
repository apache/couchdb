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

-export([add_sizes/3, upgrade_sizes/1]).
-export([init/1,terminate/2,handle_call/3,handle_cast/2,code_change/3,handle_info/2]).

-include_lib("couch/include/couch_db.hrl").
-include("couch_db_int.hrl").

-define(IDLE_LIMIT_DEFAULT, 61000).


init({Engine, DbName, FilePath, Options0}) ->
    erlang:put(io_priority, {db_update, DbName}),
    update_idle_limit_from_config(),
    DefaultSecObj = default_security_object(DbName),
    Options = [{default_security_object, DefaultSecObj} | Options0],
    try
        {ok, EngineState} = couch_db_engine:init(Engine, FilePath, Options),
        Db = init_db(DbName, FilePath, EngineState, Options),
        case lists:member(sys_db, Options) of
            false ->
                couch_stats_process_tracker:track([couchdb, open_databases]);
            true ->
                ok
        end,
        % Don't load validation funs here because the fabric query is
        % liable to race conditions. Instead see
        % couch_db:validate_doc_update, which loads them lazily.
        NewDb = Db#db{main_pid = self()},
        proc_lib:init_ack({ok, NewDb}),
        gen_server:enter_loop(?MODULE, [], NewDb, idle_limit())
    catch
        throw:InitError ->
            proc_lib:init_ack(InitError)
    end.


terminate(Reason, Db) ->
    couch_util:shutdown_sync(Db#db.compactor_pid),
    couch_db_engine:terminate(Reason, Db),
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
    couch_server:delete_compaction_files(Db#db.name),
    Db2 = Db#db{compactor_pid = nil},
    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    {reply, ok, Db2, idle_limit()};

handle_call({set_security, NewSec}, _From, #db{} = Db) ->
    {ok, NewDb} = couch_db_engine:set_security(Db, NewSec),
    NewSecDb = NewDb#db{
        security = NewSec
    },
    ok = gen_server:call(couch_server, {db_updated, NewSecDb}, infinity),
    {reply, ok, NewSecDb, idle_limit()};

handle_call({set_revs_limit, Limit}, _From, Db) ->
    {ok, Db2} = couch_db_engine:set_revs_limit(Db, Limit),
    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    {reply, ok, Db2, idle_limit()};

handle_call({set_purged_docs_limit, Limit}, _From, Db) ->
    {ok, Db2} = couch_db_engine:set_purged_docs_limit(Db, Limit),
    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    {reply, ok, Db2};

handle_call({purge_docs, UUIDsIdsRevs}, _From, Db) ->
    UpdateSeq = couch_db_engine:get_update_seq(Db),
    Ids = [Id||{_UUID, Id, _Revs} <- UUIDsIdsRevs],
    OldDocInfos = couch_db_engine:open_docs(Db, Ids),
    DocInfosPurges = lists:zip(OldDocInfos, UUIDsIdsRevs),

    FoldFun = fun(
        {OldDocInfo, {UUId, Id, Revs}}, {UpdSeq, RAcc, PaAcc, PuAcc}) ->
        case purge_doc(UpdSeq, OldDocInfo, Id, Revs) of
            not_found ->
                {UpdSeq,
                [{ok, []}| RAcc],
                PaAcc,
                PuAcc};
            {NewUpdSeq, Pair, {Id, PurgedRevs}} ->
                {NewUpdSeq,
                [{ok, PurgedRevs}| RAcc],
                [Pair| PaAcc],
                [{UUId, Id, PurgedRevs}| PuAcc]}
        end
    end,
    InitAcc = {UpdateSeq, [], [], []},
    {_USeq, Replies, Pairs, Purges} =
            lists:foldl(FoldFun, InitAcc, DocInfosPurges),

    Db2 = if Pairs == [] -> Db; true ->
        {ok, Db1} = couch_db_engine:purge_doc_revs(
            Db, lists:reverse(Pairs), lists:reverse(Purges)
        ),
        ok = gen_server:call(couch_server, {db_updated, Db1}, infinity),
        couch_event:notify(Db#db.name, updated),
        Db1
    end,
    couch_stats:increment_counter([couchdb, document_purges], length(Pairs)),
    {reply, {ok, lists:reverse(Replies)}, Db2};

handle_call(get_disposable_purge_seq, _From, Db) ->
    NewOldestPurgeSeq = get_disposable_purge_seq(Db),
    {reply, {ok, NewOldestPurgeSeq}, Db};

handle_call(Msg, From, Db) ->
    case couch_db_engine:handle_db_updater_call(Msg, From, Db) of
        {reply, Resp, NewDb} ->
            {reply, Resp, NewDb, idle_limit()};
        Else ->
            Else
    end.


handle_cast({load_validation_funs, ValidationFuns}, Db) ->
    Db2 = Db#db{validate_doc_funs = ValidationFuns},
    ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
    {noreply, Db2, idle_limit()};
handle_cast(start_compact, Db) ->
    case Db#db.compactor_pid of
        nil ->
            % For now we only support compacting to the same
            % storage engine. After the first round of patches
            % we'll add a field that sets the target engine
            % type to compact to with a new copy compactor.
            UpdateSeq = couch_db_engine:get_update_seq(Db),
            Args = [Db#db.name, UpdateSeq],
            couch_log:info("Starting compaction for db \"~s\" at ~p", Args),
            {ok, Db2} = couch_db_engine:start_compaction(Db),
            ok = gen_server:call(couch_server, {db_updated, Db2}, infinity),
            {noreply, Db2, idle_limit()};
        _ ->
            % compact currently running, this is a no-op
            {noreply, Db, idle_limit()}
    end;
handle_cast({compact_done, _Engine, CompactInfo}, #db{} = OldDb) ->
    {ok, NewDb} = couch_db_engine:finish_compaction(OldDb, CompactInfo),
    {noreply, NewDb};

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
        case {couch_db:get_update_seq(Db), couch_db:get_update_seq(Db2)} of
            {Seq, Seq} -> ok;
            _ -> couch_event:notify(Db2#db.name, updated)
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
handle_info(timeout, #db{name=DbName} = Db) ->
    IdleLimitMSec = update_idle_limit_from_config(),
    case couch_db:is_idle(Db) of
        true ->
            LastActivity = couch_db_engine:last_activity(Db),
            DtMSec = timer:now_diff(os:timestamp(), LastActivity) div 1000,
            MSecSinceLastActivity = max(0, DtMSec),
            case MSecSinceLastActivity > IdleLimitMSec of
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
    {noreply, Db, hibernate};

handle_info(Msg, Db) ->
    case couch_db_engine:handle_db_updater_info(Msg, Db) of
        {noreply, NewDb} ->
            {noreply, NewDb, idle_limit()};
        Else ->
            Else
    end.


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


init_db(DbName, FilePath, EngineState, Options) ->
    % convert start time tuple to microsecs and store as a binary string
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    StartTime = ?l2b(io_lib:format("~p",
            [(MegaSecs*1000000*1000000) + (Secs*1000000) + MicroSecs])),

    BDU = couch_util:get_value(before_doc_update, Options, nil),
    ADR = couch_util:get_value(after_doc_read, Options, nil),

    CleanedOpts = [Opt || Opt <- Options, Opt /= create],

    InitDb = #db{
        name = DbName,
        filepath = FilePath,
        engine = EngineState,
        instance_start_time = StartTime,
        options = CleanedOpts,
        before_doc_update = BDU,
        after_doc_read = ADR
    },

    InitDb#db{
        committed_update_seq = couch_db_engine:get_update_seq(InitDb),
        security = couch_db_engine:get_security(InitDb)
    }.


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
flush_trees(#db{} = Db,
        [InfoUnflushed | RestUnflushed], AccFlushed) ->
    #full_doc_info{update_seq=UpdateSeq, rev_tree=Unflushed} = InfoUnflushed,
    {Flushed, FinalAcc} = couch_key_tree:mapfold(
        fun(_Rev, Value, Type, SizesAcc) ->
            case Value of
                % This node is a document summary that needs to be
                % flushed to disk.
                #doc{} = Doc ->
                    check_doc_atts(Db, Doc),
                    ExternalSize = get_meta_body_size(Value#doc.meta),
                    {size_info, AttSizeInfo} =
                            lists:keyfind(size_info, 1, Doc#doc.meta),
                    {ok, NewDoc, WrittenSize} =
                            couch_db_engine:write_doc_body(Db, Doc),
                    Leaf = #leaf{
                        deleted = Doc#doc.deleted,
                        ptr = NewDoc#doc.body,
                        seq = UpdateSeq,
                        sizes = #size_info{
                            active = WrittenSize,
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


check_doc_atts(Db, Doc) ->
    {atts_stream, Stream} = lists:keyfind(atts_stream, 1, Doc#doc.meta),
    % Make sure that the attachments were written to the currently
    % active attachment stream. If compaction swaps during a write
    % request we may have to rewrite our attachment bodies.
    if Stream == nil -> ok; true ->
        case couch_db:is_active_stream(Db, Stream) of
            true ->
                ok;
            false ->
                % Stream where the attachments were written to is
                % no longer the current attachment stream. This
                % can happen when a database is switched at
                % compaction time.
                couch_log:debug("Stream where the attachments were"
                                " written has changed."
                                " Possibly retrying.", []),
                throw(retry)
        end
    end.


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


upgrade_sizes(#size_info{}=SI) ->
    SI;
upgrade_sizes({D, E}) ->
    #size_info{active=D, external=E};
upgrade_sizes(S) when is_integer(S) ->
    #size_info{active=S, external=0}.


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
            Body = case couch_util:get_value(comp_body, NewDoc#doc.meta) of
                CompBody when is_binary(CompBody) ->
                    couch_compress:decompress(CompBody);
                _ ->
                    NewDoc#doc.body
            end,
            RevIdDoc = NewDoc#doc{
                revs = {OldPos, [OldRev]},
                body = Body
            },
            NewRevId = couch_db:new_revid(RevIdDoc),
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

update_docs_int(Db, DocsList, LocalDocs, MergeConflicts, FullCommit) ->
    UpdateSeq = couch_db_engine:get_update_seq(Db),
    RevsLimit = couch_db_engine:get_revs_limit(Db),

    Ids = [Id || [{_Client, #doc{id=Id}}|_] <- DocsList],
    % lookup up the old documents, if they exist.
    OldDocLookups = couch_db_engine:open_docs(Db, Ids),
    OldDocInfos = lists:zipwith(fun
        (_Id, #full_doc_info{} = FDI) ->
            FDI;
        (Id, not_found) ->
            #full_doc_info{id=Id}
    end, Ids, OldDocLookups),
    % Merge the new docs into the revision trees.
    {ok, NewFullDocInfos, RemSeqs, _} = merge_rev_trees(RevsLimit,
            MergeConflicts, DocsList, OldDocInfos, [], [], UpdateSeq),

    % Write out the document summaries (the bodies are stored in the nodes of
    % the trees, the attachments are already written to disk)
    {ok, IndexFDIs} = flush_trees(Db, NewFullDocInfos, []),
    Pairs = pair_write_info(OldDocLookups, IndexFDIs),
    LocalDocs2 = update_local_doc_revs(LocalDocs),

    {ok, Db1} = couch_db_engine:write_doc_infos(Db, Pairs, LocalDocs2),

    WriteCount = length(IndexFDIs),
    couch_stats:increment_counter([couchdb, document_inserts],
         WriteCount - length(RemSeqs)),
    couch_stats:increment_counter([couchdb, document_writes], WriteCount),
    couch_stats:increment_counter(
        [couchdb, local_document_writes],
        length(LocalDocs2)
    ),

    % Check if we just updated any design documents, and update the validation
    % funs if we did.
    UpdatedDDocIds = lists:flatmap(fun
        (<<"_design/", _/binary>> = Id) -> [Id];
        (_) -> []
    end, Ids),

    {ok, commit_data(Db1, not FullCommit), UpdatedDDocIds}.


update_local_doc_revs(Docs) ->
    lists:map(fun({Client, NewDoc}) ->
        #doc{
            deleted = Delete,
            revs = {0, PrevRevs}
        } = NewDoc,
        case PrevRevs of
            [RevStr | _] ->
                PrevRev = binary_to_integer(RevStr);
            [] ->
                PrevRev = 0
        end,
        NewRev = case Delete of
            false ->
                PrevRev + 1;
            true  ->
                0
        end,
        send_result(Client, NewDoc, {ok, {0, integer_to_binary(NewRev)}}),
        NewDoc#doc{
            revs = {0, [NewRev]}
        }
    end, Docs).


purge_doc(UpdateSeq, OldDocInfo, Id, Revs) ->
    case OldDocInfo of
        #full_doc_info{rev_tree = Tree} = FDI ->
            case couch_key_tree:remove_leafs(Tree, Revs) of
                {_, [] = _RemovedRevs} -> % no change
                    not_found;
                {NewTree, RemovedRevs} ->
                    case NewTree of
                    [] ->
                        % If we purged every #leaf{} in the doc record
                        % then we're removing it completely from the
                        % database.
                        {UpdateSeq, {FDI, not_found}, {Id, RemovedRevs}};
                    _ ->
                        % Its possible to purge the #leaf{} that contains
                        % the update_seq where this doc sits in the
                        % update_seq sequence. Rather than do a bunch of
                        % complicated checks we just re-label every #leaf{}
                        % and reinsert it into the update_seq sequence.
                        {NewTree2, NewUpdateSeq} = couch_key_tree:mapfold(fun
                            (_RevId, Leaf, leaf, InnerSeqAcc) ->
                                {Leaf#leaf{seq = InnerSeqAcc + 1},
                                    InnerSeqAcc + 1};
                            (_RevId, Value, _Type, InnerSeqAcc) ->
                                {Value, InnerSeqAcc}
                        end, UpdateSeq, NewTree),

                        NewFDI = FDI#full_doc_info{
                            update_seq = NewUpdateSeq,
                            rev_tree = NewTree2
                        },
                        {NewUpdateSeq, {FDI, NewFDI}, {Id, RemovedRevs}}
                    end
            end;
        not_found ->
            not_found
    end.


% find purge seq such that all purge requests that happen before or
% during it can be removed from purge trees
get_disposable_purge_seq(#db{name=DbName} = Db) ->
    PSeq = couch_db_engine:get_purge_seq(Db),
    OldestPSeq = couch_db_engine:get_oldest_purge_seq(Db),
    PDocsLimit = couch_db_engine:get_purged_docs_limit(Db),
    ExpectedDispPSeq = PSeq - PDocsLimit,
    % client's purge_seq can be up to "allowed_purge_seq_lag"
    % behind ExpectedDispPSeq
    AllowedPSeqLag = config:get_integer("purge", "allowed_purge_seq_lag", 100),
    ClientAllowedMinPSeq = ExpectedDispPSeq - AllowedPSeqLag,
    DisposablePSeq = if OldestPSeq > ClientAllowedMinPSeq ->
        % DisposablePSeq is the last pseq we can remove;
        % it should be one less than OldestPSeq when #purges is within limit
        OldestPSeq - 1;
    true ->
        % Find the smallest checkpointed purge_seq among clients
        V = "v" ++ config:get("purge", "version", "1") ++ "-",
        Opts = [
            {start_key, list_to_binary(?LOCAL_DOC_PREFIX ++ "purge-" ++ V)},
            {end_key_gt, list_to_binary(?LOCAL_DOC_PREFIX ++ "purge1")}
        ],
        FoldFun = fun(#doc{id=DocID, body={Props}}, MinPSeq) ->
            ClientPSeq = couch_util:get_value(<<"purge_seq">>, Props),
            MinPSeq2 = if ClientPSeq >= ClientAllowedMinPSeq ->
                erlang:min(MinPSeq, ClientPSeq);
            true ->
                case check_client_exists(DbName, DocID, Props) of
                    true ->  erlang:min(MinPSeq, ClientPSeq);
                    false -> MinPSeq % ignore nonexisting clients
                end
            end,
            {ok, MinPSeq2}
        end,
        {ok, ClientPSeq} = couch_db_engine:fold_local_docs(
            Db, FoldFun, PSeq, Opts),
        erlang:min(ClientPSeq, ExpectedDispPSeq)
    end,
    DisposablePSeq.


check_client_exists(DbName, DocID, Props) ->
    % will warn about clients that have not
    % checkpointed more than "allowed_purge_time_lag"
    AllowedPTimeLag = config:get_integer("purge",
        "allowed_purge_time_lag", 86400), % secs in 1 day
    M0 = couch_util:get_value(<<"verify_module">>, Props),
    F0 = couch_util:get_value(<<"verify_function">>, Props),
    M = binary_to_atom(M0, latin1),
    F = binary_to_atom(F0, latin1),
    {A} = couch_util:get_value(<<"verify_options">>, Props),
    ClientExists = try erlang:apply(M, F, [A]) of
        true ->
            % warn if we haven't heard of this client more than AllowedPTimeLag
            ClientTime = ?b2l(couch_util:get_value(<<"timestamp_utc">>, Props)),
            {ok, [Y, Mon, D, H, Min, S], [] }=
                io_lib:fread("~4d-~2d-~2dT~2d:~2d:~2dZ", ClientTime),
            SecsClient = calendar:datetime_to_gregorian_seconds(
                {{Y, Mon, D}, {H, Min, S}}),
            SecsNow = calendar:datetime_to_gregorian_seconds(
                calendar:now_to_universal_time(os:timestamp())),
            if SecsClient + AllowedPTimeLag > SecsNow -> ok; true ->
                couch_log:warning(
                    "Client: ~p hasn't processed purge requests for more than"
                    " ~p secs. Check this client, as it prevents compaction of "
                    "purge trees on db:~p.", [A, AllowedPTimeLag, DbName]
                )
            end,
            true;
        false ->
            couch_log:warning(
                "Client ~p doesn't exist, "
                "but its checkpoint purge doc: ~p is still available. "
                "Remove this doc from: ~p", [A, DocID, DbName]
            ),
            false
    catch
        error:Error ->
            couch_log:error(
                "error in evaluating if client: ~p exists: ~p", [A, Error]
            ),
        false
    end,
    ClientExists.


commit_data(Db) ->
    commit_data(Db, false).

commit_data(#db{waiting_delayed_commit=nil} = Db, true) ->
    TRef = erlang:send_after(1000,self(),delayed_commit),
    Db#db{waiting_delayed_commit=TRef};
commit_data(Db, true) ->
    Db;
commit_data(Db, _) ->
    #db{
        waiting_delayed_commit = Timer
    } = Db,
    if is_reference(Timer) -> erlang:cancel_timer(Timer); true -> ok end,
    {ok, Db1} = couch_db_engine:commit_data(Db),
    Db1#db{
        waiting_delayed_commit = nil,
        committed_update_seq = couch_db_engine:get_update_seq(Db)
    }.


pair_write_info(Old, New) ->
    lists:map(fun(FDI) ->
        case lists:keyfind(FDI#full_doc_info.id, #full_doc_info.id, Old) of
            #full_doc_info{} = OldFDI -> {OldFDI, FDI};
            false -> {not_found, FDI}
        end
    end, New).


get_meta_body_size(Meta) ->
    {ejson_size, ExternalSize} = lists:keyfind(ejson_size, 1, Meta),
    ExternalSize.


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
