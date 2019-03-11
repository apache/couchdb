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

-module(fabric2_doc).


-export([
    get_fdi/2,
    open/3,
    update/3
]).


-include_lib("couch/include/couch_db.hrl").


-define(RETURN(Term), throw({?MODULE, Term})).


get_fdi(TxDb, DocId) ->
    Future = fabric2_db:get(TxDb, {<<"revs">>, DocId}),
    fdb_to_fdi(TxDb, DocId, erlfdb:wait(Future)).


open(TxDb, DocId, {Pos, [Rev | _] = Path}) ->
    Key = {<<"docs">>, DocId, Pos, Rev},
    Future = fabric2_db:get(TxDb, Key),
    fdb_to_doc(TxDb, DocId, Pos, Path, erlfdb:wait(Future)).


% TODO: Handle _local docs separately.
update(TxDb, #doc{} = Doc0, Options) ->
    UpdateType = case lists:member(replicated_changes, Options) of
        true -> replicated_changes;
        false -> interactive_edit
    end,

    try
        FDI1 = get_fdi(TxDb, Doc0#doc.id),
        Doc1 = prep_and_validate(TxDb, FDI1, Doc0, UpdateType),
        Doc2 = case UpdateType of
            interactive_edit -> new_revid(Doc1);
            replicated_changes -> Doc1
        end,
        FDI2 = if FDI1 /= not_found -> FDI1; true ->
            #full_doc_info{id = Doc2#doc.id}
        end,
        {FDI3, Doc3} = merge_rev_tree(FDI2, Doc2, UpdateType),

        #{tx := Tx} = TxDb,

        % Delete old entry in changes feed
        OldSeqKey = {<<"changes">>, FDI3#full_doc_info.update_seq},
        OldSeqKeyBin = fabric2_db:pack(TxDb, OldSeqKey),
        erlfdb:clear(Tx, OldSeqKeyBin),

        % Add new entry to changes feed
        NewSeqKey = {<<"changes">>, {versionstamp, 16#FFFFFFFFFFFFFFFF, 16#FFFF}},
        NewSeqKeyBin = fabric2_db:pack_vs(TxDb, NewSeqKey),
        erlfdb:set_versionstamped_key(Tx, NewSeqKeyBin, Doc3#doc.id),

        {RevStart, [Rev | _]} = Doc3#doc.revs,

        % Write document data
        {NewDocKey, NewDocVal} = doc_to_fdb(TxDb, Doc3),
        erlfdb:set(Tx, NewDocKey, NewDocVal),

        % Update revision tree entry
        {NewFDIKey, NewFDIVal} = fdi_to_fdb(TxDb, FDI3),
        erlfdb:set_versionstamped_value(Tx, NewFDIKey, NewFDIVal),

        {DocIncr, DocDelIncr} = case {FDI1, FDI3} of
            {not_found, _} ->
                {1, 0};
            {#full_doc_info{deleted = true}, #full_doc_info{deleted = false}} ->
                {1, -1};
            {#full_doc_info{deleted = false}, #full_doc_info{deleted = true}} ->
                {-1, 1};
            _ ->
                {0, 0}
        end,

        DocCountKey = {<<"meta">>, <<"stats">>, <<"doc_count">>},
        DocCountKeyBin = fabric2_db:pack(TxDb, DocCountKey),
        DocDelCountKey = {<<"meta">>, <<"stats">>, <<"doc_del_count">>},
        DocDelCountKeyBin = fabric2_db:pack(TxDb, DocDelCountKey),

        if DocIncr == 0 -> ok; true ->
            erlfdb:add(Tx, DocCountKeyBin, DocIncr)
        end,

        if DocDelIncr == 0 -> ok; true ->
            erlfdb:add(Tx, DocDelCountKeyBin, DocDelIncr)
        end,

        % TODO: Count design docs separately

        % And done.
        {ok, {RevStart, Rev}}
    catch throw:{?MODULE, Return} ->
        Return
    end.


prep_and_validate(TxDb, not_found, Doc, UpdateType) ->
    case Doc#doc.revs of
        {0, []} ->
            ok;
        _ when UpdateType == replicated_changes ->
            ok;
        _ ->
            ?RETURN({error, conflict})
    end,
    prep_and_validate(TxDb, Doc, fun() -> nil end);

prep_and_validate(TxDb, FDI, Doc, interactive_edit) ->
    #doc{
        revs = {Start, Revs}
    } = Doc,

    Leafs = couch_key_tree:get_all_leafs(FDI#full_doc_info.rev_tree),
    LeafRevs = lists:map(fun({_Leaf, {LeafStart, [LeafRev | _] = Path}}) ->
        {{LeafStart, LeafRev}, Path}
    end, Leafs),

    GetDocFun = case Revs of
        [PrevRev | _] ->
            case lists:keyfind({Start, PrevRev}, 1, LeafRevs) of
                {{Start, PrevRev}, Path} ->
                    fun() -> open(TxDb, Doc#doc.id, {Start, Path}) end;
                false ->
                    ?RETURN({error, conflict})
            end;
        [] ->
            case FDI#full_doc_info.deleted of
                true ->
                    fun() -> nil end;
                false ->
                    ?RETURN({error, conflict})
            end
    end,
    prep_and_validate(TxDb, Doc, GetDocFun);

prep_and_validate(TxDb, FDI, Doc, replicated_changes) ->
    #full_doc_info{
        rev_tree = RevTree
    } = FDI,
    OldLeafs = couch_key_tree:get_all_leafs_full(RevTree),
    OldLeafsLU = [{Start, RevId} || {Start, [{RevId, _} | _]} <- OldLeafs],

    NewPath = couch_doc:to_path(Doc),
    NewRevTree = couch_key_tree:merge(RevTree, NewPath),

    Leafs = couch_key_tree:get_all_leafs_full(NewRevTree),
    LeafRevsFull = lists:map(fun({Start, [{RevId, _} | _]} = FullPath) ->
        [{{Start, RevId}, FullPath}]
    end, Leafs),
    LeafRevsFullDict = dict:from_list(LeafRevsFull),

    #doc{
        revs = {DocStart, [DocRev | _]}
    } = Doc,
    DocRevId = {DocStart, DocRev},

    IsOldLeaf = lists:member(DocRevId, OldLeafsLU),
    GetDocFun = case dict:find(DocRevId, LeafRevsFullDict) of
        {ok, {DocStart, RevPath}} when not IsOldLeaf ->
            % An incoming replicated edit only sends us leaf
            % nodes which may have included multiple updates
            % we haven't seen locally. Thus we have to search
            % back through the tree to find the first edit
            % we do know about.
            case find_prev_known_rev(DocStart, RevPath) of
                not_found -> fun() -> nil end;
                PrevRevs -> fun() -> open(TxDb, Doc#doc.id, PrevRevs) end
            end;
        _ ->
            % The update merged to an internal node that we
            % already know about which means we're done with
            % this update.
            ?RETURN({ok, []})
    end,

    prep_and_validate(TxDb, Doc, GetDocFun).


prep_and_validate(TxDb, Doc, GetDocBody) ->
    NewDoc = case couch_doc:has_stubs(Doc) of
        true ->
            case GetDocBody() of
                #doc{} = PrevDoc ->
                    couch_doc:merge_stubs(Doc, PrevDoc);
                _ ->
                    % Force a missing stubs error
                    couch_doc:mege_stubs(Doc, #doc{})
            end;
        false ->
            Doc
    end,
    validate_doc_update(TxDb, NewDoc, GetDocBody),
    NewDoc.


merge_rev_tree(FDI, Doc, interactive_edit) when FDI#full_doc_info.deleted ->
    % We're recreating a document that was previously
    % deleted. To check that this is a recreation from
    % the root we assert that the new document has a
    % revision depth of 1 (this is to avoid recreating a
    % doc from a previous internal revision) and is also
    % not deleted. To avoid expanding the revision tree
    % unnecessarily we create a new revision based on
    % the winning deleted revision.

    {RevDepth, _} = Doc#doc.revs,
    case RevDepth == 1 andalso not Doc#doc.deleted of
        true ->
            % Update the new doc based on revisions in OldInfo
            #doc_info{revs=[WinningRev | _]} = couch_doc:to_doc_info(FDI),
            #rev_info{rev={OldPos, OldRev}} = WinningRev,
            Body = case couch_util:get_value(comp_body, Doc#doc.meta) of
                CompBody when is_binary(CompBody) ->
                    couch_compress:decompress(CompBody);
                _ ->
                    Doc#doc.body
            end,
            NewDoc = new_revid(Doc#doc{
                revs = {OldPos, [OldRev]},
                body = Body
            }),

            % Merge our modified new doc into the tree
            #full_doc_info{rev_tree = RevTree} = FDI,
            case couch_key_tree:merge(RevTree, couch_doc:to_path(NewDoc)) of
                {NewRevTree, new_leaf} ->
                    % We changed the revision id so inform the caller
                    NewFDI = FDI#full_doc_info{
                        rev_tree = NewRevTree,
                        deleted = false
                    },
                    {NewFDI, NewDoc};
                _ ->
                    throw(doc_recreation_failed)
            end;
        _ ->
            ?RETURN({error, conflict})
    end;
merge_rev_tree(FDI, Doc, interactive_edit) ->
    % We're attempting to merge a new revision into an
    % undeleted document. To not be a conflict we require
    % that the merge results in extending a branch.

    RevTree = FDI#full_doc_info.rev_tree,
    case couch_key_tree:merge(RevTree, couch_doc:to_path(Doc)) of
        {NewRevTree, new_leaf} when not Doc#doc.deleted ->
            NewFDI = FDI#full_doc_info{
                rev_tree = NewRevTree,
                deleted = false
            },
            {NewFDI, Doc};
        {NewRevTree, new_leaf} when Doc#doc.deleted ->
            % We have to check if we just deleted this
            % document completely or if it was a conflict
            % resolution.
            NewFDI = FDI#full_doc_info{
                rev_tree = NewRevTree,
                deleted = couch_doc:is_deleted(NewRevTree)
            },
            {NewFDI, Doc};
        _ ->
            ?RETURN({error, conflict})
    end;
merge_rev_tree(FDI, Doc, replicated_changes) ->
    % We're merging in revisions without caring about
    % conflicts. Most likely this is a replication update.
    RevTree = FDI#full_doc_info.rev_tree,
    {NewRevTree, _} = couch_key_tree:merge(RevTree, couch_doc:to_path(Doc)),
    NewFDI = FDI#full_doc_info{
        rev_tree = NewRevTree,
        deleted = couch_doc:is_deleted(NewRevTree)
    },
    % If a replicated change did not modify the revision
    % tree then we've got nothing else to do.
    if NewFDI /= FDI -> ok; true ->
        ?RETURN({ok, []})
    end,
    {NewFDI, Doc}.



validate_doc_update(TxDb, #doc{id = <<"_design/", _/binary>>} = Doc, _) ->
    case catch fabric2_db:check_is_admin(TxDb) of
        ok -> validate_ddoc(TxDb, Doc);
        Error -> ?RETURN(Error)
    end;
validate_doc_update(_TxDb, #doc{id = <<"_local/", _/binary>>}, _) ->
    ok;
validate_doc_update(TxDb, Doc, GetDiskDocFun) ->
    Fun = fun() ->
        DiskDoc = GetDiskDocFun(),
        JsonCtx = couch_util:json_user_ctx(TxDb),
        SecObj = fabric2_db:get_security(TxDb),
        try
            lists:map(fun(VDU) ->
                case VDU(Doc, DiskDoc, JsonCtx, SecObj) of
                    ok -> ok;
                    Error -> ?RETURN(Error)
                end
            end, fabric2_db:get_vdus(TxDb)),
            ok
        catch
            throw:Error ->
                Error
        end
    end,
    Stat = [couchdb, query_server, vdu_process_time],
    case fabric2_db:get_vdus(TxDb) of
        [] ->
            ok;
        _ ->
            couch_stats:update_histogram(Stat, Fun)
    end.


validate_ddoc(TxDb, DDoc) ->
    try
        ok = couch_index_server:validate(TxDb, couch_doc:with_ejson_body(DDoc))
    catch
        throw:{invalid_design_doc, Reason} ->
            throw({bad_request, invalid_design_doc, Reason});
        throw:{compilation_error, Reason} ->
            throw({bad_request, compilation_error, Reason});
        throw:Error ->
            ?RETURN(Error)
    end.


find_prev_known_rev(_Pos, []) ->
    not_found;
find_prev_known_rev(Pos, [{_Rev, #doc{}} | RestPath]) ->
    % doc records are skipped because these are the result
    % of replication sending us an update. We're only interested
    % in what we have locally since we're comparing attachment
    % stubs. The replicator should never do this because it
    % should only ever send leaves but the possibility exists
    % so we account for it.
    find_prev_known_rev(Pos - 1, RestPath);
find_prev_known_rev(Pos, [{_Rev, ?REV_MISSING} | RestPath]) ->
    find_prev_known_rev(Pos - 1, RestPath);
find_prev_known_rev(Pos, [{_Rev, #leaf{}} | _] = DocPath) ->
    {Pos, [Rev || {Rev, _Val} <- DocPath]}.


new_revid(Doc) ->
    #doc{
        body = Body,
        revs = {OldStart, OldRevs},
        atts = Atts,
        deleted = Deleted
    } = Doc,

    DigestedAtts = lists:foldl(fun(Att, Acc) ->
        [N, T, M] = couch_att:fetch([name, type, md5], Att),
        case M == <<>> of
            true -> Acc;
            false -> [{N, T, M} | Acc]
        end
    end, [], Atts),

    Rev = case DigestedAtts of
        Atts2 when length(Atts) =/= length(Atts2) ->
            % We must have old style non-md5 attachments
            list_to_binary(integer_to_list(couch_util:rand32()));
        Atts2 ->
            OldRev = case OldRevs of [] -> 0; [OldRev0 | _] -> OldRev0 end,
            SigTerm = [Deleted, OldStart, OldRev, Body, Atts2],
            couch_hash:md5_hash(term_to_binary(SigTerm, [{minor_version, 1}]))
    end,

    Doc#doc{revs = {OldStart + 1, [Rev | OldRevs]}}.


doc_to_fdb(TxDb, #doc{} = Doc) ->
    #doc{
        id = Id,
        revs = {Start, [Rev | _]},
        body = Body,
        atts = Atts,
        deleted = Deleted
    } = Doc,
    Key = {<<"docs">>, Id, Start, Rev},
    KeyBin = fabric2_db:pack(TxDb, Key),
    Val = {Body, Atts, Deleted},
    {KeyBin, term_to_binary(Val, [{minor_version, 1}])}.


fdb_to_doc(_TxDb, DocId, Pos, Path, Bin) when is_binary(Bin) ->
    {Body, Atts, Deleted} = binary_to_term(Bin, [safe]),
    #doc{
        id = DocId,
        revs = {Pos, Path},
        body = Body,
        atts = Atts,
        deleted = Deleted
    };
fdb_to_doc(_TxDb, _DocId, _Pos, _Path, not_found) ->
    {not_found, missing}.


fdi_to_fdb(TxDb, #full_doc_info{} = FDI) ->
    #full_doc_info{
        id = Id,
        deleted = Deleted,
        rev_tree = RevTree
    } = flush_tree(FDI),

    Key = {<<"revs">>, Id},
    KeyBin = fabric2_db:pack(TxDb, Key),
    RevTreeBin = term_to_binary(RevTree, [{minor_version, 1}]),
    ValTuple = {Deleted, RevTreeBin, {versionstamp, 16#FFFFFFFFFFFFFFFF, 16#FFFF}},
    Val = fabric2_db:pack_vs(TxDb, ValTuple),
    {KeyBin, Val}.


fdb_to_fdi(TxDb, Id, Bin) when is_binary(Bin) ->
    {Deleted, RevTreeBin, {versionstamp, V, B}} = fabric2_db:unpack(TxDb, Bin),
    RevTree = binary_to_term(RevTreeBin, [safe]),
    UpdateSeq = <<V:64/big, B:16/big>>,
    #full_doc_info{
        id = Id,
        deleted = Deleted,
        rev_tree = RevTree,
        update_seq = UpdateSeq
    };
fdb_to_fdi(_TxDb, _Id, not_found) ->
    not_found.


flush_tree(FDI) ->
    #full_doc_info{
        rev_tree = Unflushed
    } = FDI,

    Flushed = couch_key_tree:map(fun(_Rev, Value) ->
        case Value of
            #doc{deleted = Del} -> #leaf{deleted = Del};
            _ -> Value
        end
    end, Unflushed),

    FDI#full_doc_info{
        rev_tree = Flushed
    }.
