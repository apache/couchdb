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
-export([init/1, terminate/2, handle_call/3, handle_cast/2, code_change/3, handle_info/2]).

-include_lib("couch/include/couch_db.hrl").
-include("couch_db_int.hrl").

-define(IDLE_LIMIT_DEFAULT, 61000).
% 10 GiB
-define(DEFAULT_MAX_PARTITION_SIZE, 16#280000000).

-record(merge_acc, {
    revs_limit,
    merge_conflicts,
    add_infos = [],
    rem_seqs = [],
    cur_seq,
    full_partitions = []
}).

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
    ok = couch_server:db_updated(Db2),
    {reply, ok, Db2, idle_limit()};
handle_call({set_security, NewSec}, _From, #db{} = Db) ->
    {ok, NewDb} = couch_db_engine:set_security(Db, NewSec),
    NewSecDb = commit_data(NewDb#db{
        security = NewSec
    }),
    ok = couch_server:db_updated(NewSecDb),
    {reply, ok, NewSecDb, idle_limit()};
handle_call({set_revs_limit, Limit}, _From, Db) ->
    {ok, Db2} = couch_db_engine:set_revs_limit(Db, Limit),
    Db3 = commit_data(Db2),
    ok = couch_server:db_updated(Db3),
    {reply, ok, Db3, idle_limit()};
handle_call({set_purge_infos_limit, Limit}, _From, Db) ->
    {ok, Db2} = couch_db_engine:set_purge_infos_limit(Db, Limit),
    ok = couch_server:db_updated(Db2),
    {reply, ok, Db2, idle_limit()};
handle_call({purge_docs, [], _}, _From, Db) ->
    {reply, {ok, []}, Db, idle_limit()};
handle_call({purge_docs, PurgeReqs0, Options}, _From, Db) ->
    % Filter out any previously applied updates during
    % internal replication
    IsRepl = lists:member(replicated_changes, Options),
    PurgeReqs =
        if
            not IsRepl ->
                PurgeReqs0;
            true ->
                UUIDs = [UUID || {UUID, _Id, _Revs} <- PurgeReqs0],
                PurgeInfos = couch_db_engine:load_purge_infos(Db, UUIDs),
                lists:flatmap(
                    fun
                        ({not_found, PReq}) -> [PReq];
                        ({{_, _, _, _}, _}) -> []
                    end,
                    lists:zip(PurgeInfos, PurgeReqs0)
                )
        end,
    {ok, NewDb, Replies} = purge_docs(Db, PurgeReqs),
    {reply, {ok, Replies}, NewDb, idle_limit()};
handle_call({raft_insert, Entries}, _From, Db) ->
    {ok, Db2} = couch_db_engine:raft_insert(Db, Entries),
    Db3 = commit_data(Db2),
    ok = couch_server:db_updated(Db3),
    {reply, ok, Db3, idle_limit()};
handle_call({raft_discard, UpTo}, _From, Db) ->
    {ok, Db2} = couch_db_engine:raft_discard(Db, UpTo),
    Db3 = commit_data(Db2),
    ok = couch_server:db_updated(Db3),
    {reply, ok, Db3, idle_limit()};
handle_call(Msg, From, Db) ->
    case couch_db_engine:handle_db_updater_call(Msg, From, Db) of
        {reply, Resp, NewDb} ->
            {reply, Resp, NewDb, idle_limit()};
        Else ->
            Else
    end.

handle_cast({load_validation_funs, ValidationFuns}, Db) ->
    Db2 = Db#db{validate_doc_funs = ValidationFuns},
    ok = couch_server:db_updated(Db2),
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
            Level = list_to_existing_atom(
                config:get(
                    "couchdb", "compaction_log_level", "info"
                )
            ),
            couch_log:Level("Starting compaction for db \"~s\" at ~p", Args),
            {ok, Db2} = couch_db_engine:start_compaction(Db),
            ok = couch_server:db_updated(Db2),
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
    couch_log:error(
        "Database `~s` updater received unexpected cast: ~p",
        [Name, Msg]
    ),
    {stop, Msg, Db}.

handle_info(
    {update_docs, Client, GroupedDocs, NonRepDocs, MergeConflicts},
    Db
) ->
    GroupedDocs2 = sort_and_tag_grouped_docs(Client, GroupedDocs),
    if
        NonRepDocs == [] ->
            {GroupedDocs3, Clients} = collect_updates(
                GroupedDocs2,
                [Client],
                MergeConflicts
            );
        true ->
            GroupedDocs3 = GroupedDocs2,
            Clients = [Client]
    end,
    NonRepDocs2 = [{Client, NRDoc} || NRDoc <- NonRepDocs],
    try update_docs_int(Db, GroupedDocs3, NonRepDocs2, MergeConflicts) of
        {ok, Db2, UpdatedDDocIds} ->
            ok = couch_server:db_updated(Db2),
            case {couch_db:get_update_seq(Db), couch_db:get_update_seq(Db2)} of
                {Seq, Seq} -> ok;
                _ -> couch_event:notify(Db2#db.name, updated)
            end,
            if
                NonRepDocs2 /= [] ->
                    couch_event:notify(Db2#db.name, local_updated);
                true ->
                    ok
            end,
            [catch (ClientPid ! {done, self()}) || ClientPid <- Clients],
            Db3 =
                case length(UpdatedDDocIds) > 0 of
                    true ->
                        % Ken and ddoc_cache are the only things that
                        % use the unspecified ddoc_updated message. We
                        % should update them to use the new message per
                        % ddoc.
                        lists:foreach(
                            fun(DDocId) ->
                                couch_event:notify(Db2#db.name, {ddoc_updated, DDocId})
                            end,
                            UpdatedDDocIds
                        ),
                        couch_event:notify(Db2#db.name, ddoc_updated),
                        ddoc_cache:refresh(Db2#db.name, UpdatedDDocIds),
                        refresh_validate_doc_funs(Db2);
                    false ->
                        Db2
                end,
            {noreply, Db3, hibernate_if_no_idle_limit()}
    catch
        throw:retry ->
            [catch (ClientPid ! {retry, self()}) || ClientPid <- Clients],
            {noreply, Db, hibernate_if_no_idle_limit()}
    end;
handle_info({'EXIT', _Pid, normal}, Db) ->
    {noreply, Db, idle_limit()};
handle_info({'EXIT', _Pid, Reason}, Db) ->
    {stop, Reason, Db};
handle_info(timeout, #db{name = DbName} = Db) ->
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
    Cmp = fun([#doc{id = A} | _], [#doc{id = B} | _]) -> A < B end,
    lists:map(
        fun(DocGroup) ->
            [{Client, maybe_tag_doc(D)} || D <- DocGroup]
        end,
        lists:sort(Cmp, GroupedDocs)
    ).

maybe_tag_doc(#doc{id = Id, revs = {Pos, [_Rev | PrevRevs]}, meta = Meta0} = Doc) ->
    case lists:keymember(ref, 1, Meta0) of
        true ->
            Doc;
        false ->
            Key = {Id, {Pos - 1, PrevRevs}},
            Doc#doc{meta = [{ref, Key} | Meta0]}
    end.

merge_updates([[{_, #doc{id = X}} | _] = A | RestA], [[{_, #doc{id = X}} | _] = B | RestB]) ->
    [A ++ B | merge_updates(RestA, RestB)];
merge_updates([[{_, #doc{id = X}} | _] | _] = A, [[{_, #doc{id = Y}} | _] | _] = B) when X < Y ->
    [hd(A) | merge_updates(tl(A), B)];
merge_updates([[{_, #doc{id = X}} | _] | _] = A, [[{_, #doc{id = Y}} | _] | _] = B) when X > Y ->
    [hd(B) | merge_updates(A, tl(B))];
merge_updates([], RestB) ->
    RestB;
merge_updates(RestA, []) ->
    RestA.

collect_updates(GroupedDocsAcc, ClientsAcc, MergeConflicts) ->
    receive
        % Only collect updates with the same MergeConflicts flag and without
        % local docs. It's easier to just avoid multiple _local doc
        % updaters than deal with their possible conflicts, and local docs
        % writes are relatively rare. Can be optmized later if really needed.
        {update_docs, Client, GroupedDocs, [], MergeConflicts} ->
            GroupedDocs2 = sort_and_tag_grouped_docs(Client, GroupedDocs),
            GroupedDocsAcc2 =
                merge_updates(GroupedDocsAcc, GroupedDocs2),
            collect_updates(
                GroupedDocsAcc2,
                [Client | ClientsAcc],
                MergeConflicts
            )
    after 0 ->
        {GroupedDocsAcc, ClientsAcc}
    end.

init_db(DbName, FilePath, EngineState, Options) ->
    % convert start time tuple to microsecs and store as a binary string
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    StartTime = ?l2b(
        io_lib:format(
            "~p",
            [(MegaSecs * 1000000 * 1000000) + (Secs * 1000000) + MicroSecs]
        )
    ),

    BDU = couch_util:get_value(before_doc_update, Options, nil),
    ADR = couch_util:get_value(after_doc_read, Options, nil),

    NonCreateOpts = [Opt || Opt <- Options, Opt /= create],

    InitDb = #db{
        name = DbName,
        filepath = FilePath,
        engine = EngineState,
        instance_start_time = StartTime,
        options = NonCreateOpts,
        before_doc_update = BDU,
        after_doc_read = ADR
    },

    DbProps = couch_db_engine:get_props(InitDb),

    InitDb#db{
        committed_update_seq = couch_db_engine:get_update_seq(InitDb),
        security = couch_db_engine:get_security(InitDb),
        options = lists:keystore(props, 1, NonCreateOpts, {props, DbProps})
    }.

refresh_validate_doc_funs(#db{name = <<"shards/", _/binary>> = Name} = Db) ->
    spawn(fabric, reset_validation_funs, [mem3:dbname(Name)]),
    Db#db{validate_doc_funs = undefined};
refresh_validate_doc_funs(Db0) ->
    Db = Db0#db{user_ctx = ?ADMIN_USER},
    {ok, DesignDocs} = couch_db:get_design_docs(Db),
    ProcessDocFuns = lists:flatmap(
        fun(DesignDocInfo) ->
            {ok, DesignDoc} = couch_db:open_doc_int(
                Db, DesignDocInfo, [ejson_body]
            ),
            case couch_doc:get_validate_doc_fun(DesignDoc) of
                nil -> [];
                Fun -> [Fun]
            end
        end,
        DesignDocs
    ),
    Db#db{validate_doc_funs = ProcessDocFuns}.

% rev tree functions

flush_trees(_Db, [], AccFlushedTrees) ->
    {ok, lists:reverse(AccFlushedTrees)};
flush_trees(
    #db{} = Db,
    [InfoUnflushed | RestUnflushed],
    AccFlushed
) ->
    #full_doc_info{update_seq = UpdateSeq, rev_tree = Unflushed} = InfoUnflushed,
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
        end,
        {0, 0, []},
        Unflushed
    ),
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
    if
        Stream == nil ->
            ok;
        true ->
            case couch_db:is_active_stream(Db, Stream) of
                true ->
                    ok;
                false ->
                    % Stream where the attachments were written to is
                    % no longer the current attachment stream. This
                    % can happen when a database is switched at
                    % compaction time.
                    couch_log:debug(
                        "Stream where the attachments were"
                        " written has changed."
                        " Possibly retrying.",
                        []
                    ),
                    throw(retry)
            end
    end.

add_sizes(Type, #leaf{sizes = Sizes, atts = AttSizes}, Acc) ->
    % Maybe upgrade from disk_size only
    #size_info{
        active = ActiveSize,
        external = ExternalSize
    } = upgrade_sizes(Sizes),
    {ASAcc, ESAcc, AttsAcc} = Acc,
    NewASAcc = ActiveSize + ASAcc,
    NewESAcc =
        ESAcc +
            if
                Type == leaf -> ExternalSize;
                true -> 0
            end,
    NewAttsAcc = lists:umerge(AttSizes, AttsAcc),
    {NewASAcc, NewESAcc, NewAttsAcc}.

upgrade_sizes(#size_info{} = SI) ->
    SI;
upgrade_sizes({D, E}) ->
    #size_info{active = D, external = E};
upgrade_sizes(S) when is_integer(S) ->
    #size_info{active = S, external = 0}.

send_result(Client, Doc, NewResult) ->
    % used to send a result to the client
    catch (Client ! {result, self(), {doc_tag(Doc), NewResult}}).

doc_tag(#doc{meta = Meta}) ->
    case lists:keyfind(ref, 1, Meta) of
        {ref, Ref} -> Ref;
        false -> throw(no_doc_tag);
        Else -> throw({invalid_doc_tag, Else})
    end.

merge_rev_trees([], [], Acc) ->
    {ok, Acc#merge_acc{
        add_infos = lists:reverse(Acc#merge_acc.add_infos)
    }};
merge_rev_trees([NewDocs | RestDocsList], [OldDocInfo | RestOldInfo], Acc) ->
    #merge_acc{
        revs_limit = Limit,
        merge_conflicts = MergeConflicts,
        full_partitions = FullPartitions
    } = Acc,

    % Track doc ids so we can debug large revision trees
    erlang:put(last_id_merged, OldDocInfo#full_doc_info.id),
    NewDocInfo0 = lists:foldl(
        fun({Client, NewDoc}, OldInfoAcc) ->
            NewInfo = merge_rev_tree(OldInfoAcc, NewDoc, Client, MergeConflicts),
            case is_overflowed(NewInfo, OldInfoAcc, FullPartitions) of
                true when not MergeConflicts ->
                    DocId = NewInfo#full_doc_info.id,
                    send_result(Client, NewDoc, {partition_overflow, DocId}),
                    OldInfoAcc;
                _ ->
                    NewInfo
            end
        end,
        OldDocInfo,
        NewDocs
    ),
    NewDocInfo1 = maybe_stem_full_doc_info(NewDocInfo0, Limit),
    % When MergeConflicts is false, we updated #full_doc_info.deleted on every
    % iteration of merge_rev_tree. However, merge_rev_tree does not update
    % #full_doc_info.deleted when MergeConflicts is true, since we don't need
    % to know whether the doc is deleted between iterations. Since we still
    % need to know if the doc is deleted after the merge happens, we have to
    % set it here.
    NewDocInfo2 =
        case MergeConflicts of
            true ->
                NewDocInfo1#full_doc_info{
                    deleted = couch_doc:is_deleted(NewDocInfo1)
                };
            false ->
                NewDocInfo1
        end,
    if
        NewDocInfo2 == OldDocInfo ->
            % nothing changed
            merge_rev_trees(RestDocsList, RestOldInfo, Acc);
        true ->
            % We have updated the document, give it a new update_seq. Its
            % important to note that the update_seq on OldDocInfo should
            % be identical to the value on NewDocInfo1.
            OldSeq = OldDocInfo#full_doc_info.update_seq,
            NewDocInfo3 = NewDocInfo2#full_doc_info{
                update_seq = Acc#merge_acc.cur_seq + 1
            },
            RemoveSeqs =
                case OldSeq of
                    0 -> Acc#merge_acc.rem_seqs;
                    _ -> [OldSeq | Acc#merge_acc.rem_seqs]
                end,
            NewAcc = Acc#merge_acc{
                add_infos = [NewDocInfo3 | Acc#merge_acc.add_infos],
                rem_seqs = RemoveSeqs,
                cur_seq = Acc#merge_acc.cur_seq + 1
            },
            merge_rev_trees(RestDocsList, RestOldInfo, NewAcc)
    end.

merge_rev_tree(OldInfo, NewDoc, Client, false) when
    OldInfo#full_doc_info.deleted
->
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
            #doc_info{revs = [WinningRev | _]} = couch_doc:to_doc_info(OldInfo),
            #rev_info{rev = {OldPos, OldRev}} = WinningRev,
            Body =
                case couch_util:get_value(comp_body, NewDoc#doc.meta) of
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
            NewDoc2 = NewDoc#doc{revs = {OldPos + 1, [NewRevId, OldRev]}},

            % Merge our modified new doc into the tree
            #full_doc_info{rev_tree = OldTree} = OldInfo,
            NewTree0 = couch_doc:to_path(NewDoc2),
            case couch_key_tree:merge(OldTree, NewTree0) of
                {NewTree1, new_leaf} ->
                    % We changed the revision id so inform the caller
                    send_result(Client, NewDoc, {ok, {OldPos + 1, NewRevId}}),
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
merge_rev_tree(OldInfo, NewDoc, Client, false) ->
    % We're attempting to merge a new revision into an
    % undeleted document. To not be a conflict we require
    % that the merge results in extending a branch.

    OldTree = OldInfo#full_doc_info.rev_tree,
    NewTree0 = couch_doc:to_path(NewDoc),
    NewDeleted = NewDoc#doc.deleted,
    case couch_key_tree:merge(OldTree, NewTree0) of
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
merge_rev_tree(OldInfo, NewDoc, _Client, true) ->
    % We're merging in revisions without caring about
    % conflicts. Most likely this is a replication update.
    OldTree = OldInfo#full_doc_info.rev_tree,
    NewTree0 = couch_doc:to_path(NewDoc),
    {NewTree, _} = couch_key_tree:merge(OldTree, NewTree0),
    OldInfo#full_doc_info{rev_tree = NewTree}.

is_overflowed(_New, _Old, []) ->
    false;
is_overflowed(Old, Old, _FullPartitions) ->
    false;
is_overflowed(New, Old, FullPartitions) ->
    case New#full_doc_info.id of
        <<"_design/", _/binary>> ->
            false;
        DDocId ->
            Partition = couch_partition:from_docid(DDocId),
            case lists:member(Partition, FullPartitions) of
                true ->
                    estimate_size(New) > estimate_size(Old);
                false ->
                    false
            end
    end.

maybe_stem_full_doc_info(#full_doc_info{rev_tree = Tree} = Info, Limit) ->
    case config:get_boolean("couchdb", "stem_interactive_updates", true) of
        true ->
            Stemmed = couch_key_tree:stem(Tree, Limit),
            Info#full_doc_info{rev_tree = Stemmed};
        false ->
            Info
    end.

update_docs_int(Db, DocsList, LocalDocs, MergeConflicts) ->
    UpdateSeq = couch_db_engine:get_update_seq(Db),
    RevsLimit = couch_db_engine:get_revs_limit(Db),

    Ids = [Id || [{_Client, #doc{id = Id}} | _] <- DocsList],
    % lookup up the old documents, if they exist.
    OldDocLookups = couch_db_engine:open_docs(Db, Ids),
    OldDocInfos = lists:zipwith(
        fun
            (_Id, #full_doc_info{} = FDI) ->
                FDI;
            (Id, not_found) ->
                #full_doc_info{id = Id}
        end,
        Ids,
        OldDocLookups
    ),

    %% Get the list of full partitions
    FullPartitions =
        case couch_db:is_partitioned(Db) of
            true ->
                case max_partition_size() of
                    N when N =< 0 ->
                        [];
                    Max ->
                        Partitions = lists:usort(
                            lists:flatmap(
                                fun(Id) ->
                                    case couch_partition:extract(Id) of
                                        undefined -> [];
                                        {Partition, _} -> [Partition]
                                    end
                                end,
                                Ids
                            )
                        ),
                        [P || P <- Partitions, partition_size(Db, P) >= Max]
                end;
            false ->
                []
        end,

    % Merge the new docs into the revision trees.
    AccIn = #merge_acc{
        revs_limit = RevsLimit,
        merge_conflicts = MergeConflicts,
        add_infos = [],
        rem_seqs = [],
        cur_seq = UpdateSeq,
        full_partitions = FullPartitions
    },
    {ok, AccOut} = merge_rev_trees(DocsList, OldDocInfos, AccIn),
    #merge_acc{
        add_infos = NewFullDocInfos,
        rem_seqs = RemSeqs
    } = AccOut,

    % Write out the document summaries (the bodies are stored in the nodes of
    % the trees, the attachments are already written to disk)
    {ok, IndexFDIs} = flush_trees(Db, NewFullDocInfos, []),
    Pairs = pair_write_info(OldDocLookups, IndexFDIs),
    LocalDocs2 = update_local_doc_revs(LocalDocs),

    {ok, Db1} = couch_db_engine:write_doc_infos(Db, Pairs, LocalDocs2),

    WriteCount = length(IndexFDIs),
    couch_stats:increment_counter(
        [couchdb, document_inserts],
        WriteCount - length(RemSeqs)
    ),
    couch_stats:increment_counter([couchdb, document_writes], WriteCount),
    couch_stats:increment_counter(
        [couchdb, local_document_writes],
        length(LocalDocs2)
    ),

    % Check if we just updated any design documents, and update the validation
    % funs if we did.
    UpdatedDDocIds = lists:flatmap(
        fun
            (<<"_design/", _/binary>> = Id) -> [Id];
            (_) -> []
        end,
        Ids
    ),

    {ok, commit_data(Db1), UpdatedDDocIds}.

update_local_doc_revs(Docs) ->
    lists:foldl(
        fun({Client, Doc}, Acc) ->
            case increment_local_doc_revs(Doc) of
                {ok, #doc{revs = {0, [NewRev]}} = NewDoc} ->
                    send_result(Client, Doc, {ok, {0, integer_to_binary(NewRev)}}),
                    [NewDoc | Acc];
                {error, Error} ->
                    send_result(Client, Doc, {error, Error}),
                    Acc
            end
        end,
        [],
        Docs
    ).

increment_local_doc_revs(#doc{deleted = true} = Doc) ->
    {ok, Doc#doc{revs = {0, [0]}}};
increment_local_doc_revs(#doc{revs = {0, []}} = Doc) ->
    {ok, Doc#doc{revs = {0, [1]}}};
increment_local_doc_revs(#doc{revs = {0, [RevStr | _]}} = Doc) ->
    try
        PrevRev = binary_to_integer(RevStr),
        {ok, Doc#doc{revs = {0, [PrevRev + 1]}}}
    catch
        error:badarg ->
            {error, <<"Invalid rev format">>}
    end;
increment_local_doc_revs(#doc{}) ->
    {error, <<"Invalid rev format">>}.

max_partition_size() ->
    config:get_integer(
        "couchdb",
        "max_partition_size",
        ?DEFAULT_MAX_PARTITION_SIZE
    ).

partition_size(Db, Partition) ->
    {ok, Info} = couch_db:get_partition_info(Db, Partition),
    Sizes = couch_util:get_value(sizes, Info),
    couch_util:get_value(external, Sizes).

estimate_size(#full_doc_info{} = FDI) ->
    #full_doc_info{rev_tree = RevTree} = FDI,
    Fun = fun
        (_Rev, Value, leaf, SizesAcc) ->
            case Value of
                #doc{} = Doc ->
                    ExternalSize = get_meta_body_size(Value#doc.meta),
                    {size_info, AttSizeInfo} =
                        lists:keyfind(size_info, 1, Doc#doc.meta),
                    Leaf = #leaf{
                        sizes = #size_info{
                            external = ExternalSize
                        },
                        atts = AttSizeInfo
                    },
                    add_sizes(leaf, Leaf, SizesAcc);
                #leaf{} ->
                    add_sizes(leaf, Value, SizesAcc)
            end;
        (_Rev, _Value, branch, SizesAcc) ->
            SizesAcc
    end,
    {_, FinalES, FinalAtts} = couch_key_tree:fold(Fun, {0, 0, []}, RevTree),
    TotalAttSize = lists:foldl(fun({_, S}, A) -> S + A end, 0, FinalAtts),
    FinalES + TotalAttSize.

purge_docs(Db, []) ->
    {ok, Db, []};
purge_docs(Db, PurgeReqs) ->
    Ids = lists:usort(lists:map(fun({_UUID, Id, _Revs}) -> Id end, PurgeReqs)),
    FDIs = couch_db_engine:open_docs(Db, Ids),
    USeq = couch_db_engine:get_update_seq(Db),

    IdFDIs = lists:zip(Ids, FDIs),
    {NewIdFDIs, Replies} = apply_purge_reqs(PurgeReqs, IdFDIs, USeq, []),

    Pairs = lists:flatmap(
        fun({DocId, OldFDI}) ->
            {DocId, NewFDI} = lists:keyfind(DocId, 1, NewIdFDIs),
            case {OldFDI, NewFDI} of
                {not_found, not_found} ->
                    [];
                {#full_doc_info{} = A, #full_doc_info{} = A} ->
                    [];
                {#full_doc_info{}, _} ->
                    [{OldFDI, NewFDI}]
            end
        end,
        IdFDIs
    ),

    PSeq = couch_db_engine:get_purge_seq(Db),
    {RevPInfos, _} = lists:foldl(
        fun({UUID, DocId, Revs}, {PIAcc, PSeqAcc}) ->
            Info = {PSeqAcc + 1, UUID, DocId, Revs},
            {[Info | PIAcc], PSeqAcc + 1}
        end,
        {[], PSeq},
        PurgeReqs
    ),
    PInfos = lists:reverse(RevPInfos),

    {ok, Db1} = couch_db_engine:purge_docs(Db, Pairs, PInfos),
    Db2 = commit_data(Db1),
    ok = couch_server:db_updated(Db2),
    couch_event:notify(Db2#db.name, updated),
    {ok, Db2, Replies}.

apply_purge_reqs([], IdFDIs, _USeq, Replies) ->
    {IdFDIs, lists:reverse(Replies)};
apply_purge_reqs([Req | RestReqs], IdFDIs, USeq, Replies) ->
    {_UUID, DocId, Revs} = Req,
    {value, {_, FDI0}, RestIdFDIs} = lists:keytake(DocId, 1, IdFDIs),
    {NewFDI, RemovedRevs, NewUSeq} =
        case FDI0 of
            #full_doc_info{rev_tree = Tree} ->
                case couch_key_tree:remove_leafs(Tree, Revs) of
                    {_, []} ->
                        % No change
                        {FDI0, [], USeq};
                    {[], Removed} ->
                        % Completely purged
                        {not_found, Removed, USeq};
                    {NewTree, Removed} ->
                        % Its possible to purge the #leaf{} that contains
                        % the update_seq where this doc sits in the
                        % update_seq sequence. Rather than do a bunch of
                        % complicated checks we just re-label every #leaf{}
                        % and reinsert it into the update_seq sequence.
                        {NewTree2, NewUpdateSeq} = couch_key_tree:mapfold(
                            fun
                                (_RevId, Leaf, leaf, SeqAcc) ->
                                    {Leaf#leaf{seq = SeqAcc + 1}, SeqAcc + 1};
                                (_RevId, Value, _Type, SeqAcc) ->
                                    {Value, SeqAcc}
                            end,
                            USeq,
                            NewTree
                        ),

                        FDI1 = FDI0#full_doc_info{
                            update_seq = NewUpdateSeq,
                            rev_tree = NewTree2
                        },
                        {FDI1, Removed, NewUpdateSeq}
                end;
            not_found ->
                % Not found means nothing to change
                {not_found, [], USeq}
        end,
    NewIdFDIs = [{DocId, NewFDI} | RestIdFDIs],
    NewReplies = [{ok, RemovedRevs} | Replies],
    apply_purge_reqs(RestReqs, NewIdFDIs, NewUSeq, NewReplies).

commit_data(Db) ->
    {ok, Db1} = couch_db_engine:commit_data(Db),
    Db1#db{
        committed_update_seq = couch_db_engine:get_update_seq(Db)
    }.

pair_write_info(Old, New) ->
    lists:map(
        fun(FDI) ->
            case lists:keyfind(FDI#full_doc_info.id, #full_doc_info.id, Old) of
                #full_doc_info{} = OldFDI -> {OldFDI, FDI};
                false -> {not_found, FDI}
            end
        end,
        New
    ).

get_meta_body_size(Meta) ->
    {ejson_size, ExternalSize} = lists:keyfind(ejson_size, 1, Meta),
    ExternalSize.

default_security_object(<<"shards/", _/binary>>) ->
    case config:get("couchdb", "default_security", "admin_only") of
        "admin_only" ->
            [
                {<<"members">>, {[{<<"roles">>, [<<"_admin">>]}]}},
                {<<"admins">>, {[{<<"roles">>, [<<"_admin">>]}]}}
            ];
        Everyone when Everyone == "everyone"; Everyone == "admin_local" ->
            []
    end;
default_security_object(_DbName) ->
    case config:get("couchdb", "default_security", "admin_only") of
        Admin when Admin == "admin_only"; Admin == "admin_local" ->
            [
                {<<"members">>, {[{<<"roles">>, [<<"_admin">>]}]}},
                {<<"admins">>, {[{<<"roles">>, [<<"_admin">>]}]}}
            ];
        "everyone" ->
            []
    end.

% These functions rely on using the process dictionary. This is
% usually frowned upon however in this case it is done to avoid
% changing to a different server state record. Once PSE (Pluggable
% Storage Engine) code lands this should be moved to the #db{} record.
update_idle_limit_from_config() ->
    Default = integer_to_list(?IDLE_LIMIT_DEFAULT),
    IdleLimit =
        case config:get("couchdb", "idle_check_timeout", Default) of
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

update_local_doc_revs_test_() ->
    {inparallel, [
        {"Test local doc with valid rev", fun t_good_local_doc/0},
        {"Test local doc with invalid rev", fun t_bad_local_doc/0},
        {"Test deleted local doc", fun t_dead_local_doc/0}
    ]}.

t_good_local_doc() ->
    Doc = #doc{
        id = <<"_local/alice">>,
        revs = {0, [<<"1">>]},
        meta = [{ref, make_ref()}]
    },
    [NewDoc] = update_local_doc_revs([{self(), Doc}]),
    ?assertEqual({0, [2]}, NewDoc#doc.revs),
    {ok, Result} = receive_result(Doc),
    ?assertEqual({ok, {0, <<"2">>}}, Result).

t_bad_local_doc() ->
    lists:foreach(
        fun(BadRevs) ->
            Doc = #doc{
                id = <<"_local/alice">>,
                revs = BadRevs,
                meta = [{ref, make_ref()}]
            },
            NewDocs = update_local_doc_revs([{self(), Doc}]),
            ?assertEqual([], NewDocs),
            {ok, Result} = receive_result(Doc),
            ?assertEqual({error, <<"Invalid rev format">>}, Result)
        end,
        [{0, [<<"a">>]}, {1, [<<"1">>]}]
    ).

t_dead_local_doc() ->
    Doc = #doc{
        id = <<"_local/alice">>,
        revs = {0, [<<"122">>]},
        deleted = true,
        meta = [{ref, make_ref()}]
    },
    [NewDoc] = update_local_doc_revs([{self(), Doc}]),
    ?assertEqual({0, [0]}, NewDoc#doc.revs),
    {ok, Result} = receive_result(Doc),
    ?assertEqual({ok, {0, <<"0">>}}, Result).

receive_result(#doc{meta = Meta}) ->
    Ref = couch_util:get_value(ref, Meta),
    receive
        {result, _, {Ref, Result}} -> {ok, Result}
    end.

-endif.
