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

-module(fabric2_db).


-export([
    create/2,
    open/2,
    delete/2,

    is_admin/1,
    check_is_admin/1,
    check_is_member/1,

    name/1,
    get_after_doc_read_fun/1,
    get_before_doc_update_fun/1,
    get_committed_update_seq/1,
    get_compacted_seq/1,
    get_compactor_pid/1,
    get_db_info/1,
    %% get_partition_info/2,
    get_del_doc_count/1,
    get_doc_count/1,
    get_doc_count/2,
    %% get_epochs/1,
    %% get_filepath/1,
    get_instance_start_time/1,
    get_pid/1,
    get_revs_limit/1,
    get_security/1,
    get_update_seq/1,
    get_user_ctx/1,
    get_uuid/1,
    %% get_purge_seq/1,
    %% get_oldest_purge_seq/1,
    %% get_purge_infos_limit/1,

    is_clustered/1,
    is_db/1,
    is_partitioned/1,
    is_system_db/1,
    is_system_db_name/1,

    set_revs_limit/2,
    %% set_purge_infos_limit/2,
    set_security/2,
    set_user_ctx/2,

    ensure_full_commit/1,
    ensure_full_commit/2,

    %% load_validation_funs/1,
    %% reload_validation_funs/1,

    open_doc/2,
    open_doc/3,
    open_doc_revs/4,
    %% open_doc_int/3,
    %% get_doc_info/2,
    %% get_full_doc_info/2,
    %% get_full_doc_infos/2,
    %% get_missing_revs/2,
    %% get_design_doc/2,
    %% get_design_docs/1,
    %% get_design_doc_count/1,
    %% get_purge_infos/2,

    %% get_minimum_purge_seq/1,
    %% purge_client_exists/3,

    %% validate_docid/2,
    %% doc_from_json_obj_validate/2,

    update_doc/2,
    update_doc/3,
    update_docs/2,
    update_docs/3,
    %% delete_doc/3,

    %% purge_docs/2,
    %% purge_docs/3,

    %% with_stream/3,
    %% open_write_stream/2,
    %% open_read_stream/2,
    %% is_active_stream/2,

    %% fold_docs/3,
    %% fold_docs/4,
    %% fold_local_docs/4,
    %% fold_design_docs/4,
    %% fold_changes/4,
    %% fold_changes/5,
    %% count_changes_since/2,
    %% fold_purge_infos/4,
    %% fold_purge_infos/5,

    %% calculate_start_seq/3,
    %% owner_of/2,

    %% start_compact/1,
    %% cancel_compact/1,
    %% wait_for_compaction/1,
    %% wait_for_compaction/2,

    %% dbname_suffix/1,
    %% normalize_dbname/1,
    %% validate_dbname/1,

    %% make_doc/5,
    new_revid/1
]).


-include_lib("couch/include/couch_db.hrl").
-include("fabric2.hrl").


-define(DBNAME_REGEX,
    "^[a-z][a-z0-9\\_\\$()\\+\\-\\/]*" % use the stock CouchDB regex
    "(\\.[0-9]{10,})?$" % but allow an optional shard timestamp at the end
).


-define(RETURN(Term), throw({?MODULE, Term})).


create(DbName, Options) ->
    Result = transactional(DbName, Options, fun(TxDb) ->
        case fabric2_fdb:exists(TxDb) of
            true ->
                {error, file_exists};
            false ->
                fabric2_fdb:create(TxDb, Options)
        end
    end),
    % We cache outside of the transaction so that we're sure
    % that this request created the database
    case Result of
        #{} = Db ->
            ok = fabric2_server:store(Db),
            {ok, Db#{tx => undefined}};
        Error ->
            Error
    end.


open(DbName, Options) ->
    case fabric2_server:fetch(DbName) of
        #{} = Db ->
            with_tx(Db, fun(TxDb) ->
                case fabric2_fdb:is_current(TxDb) of
                    true ->
                        {ok, maybe_set_user_ctx(Db, Options)};
                    false ->
                        Reopened = fabric2_fdb:open(TxDb, Options),
                        ok = fabric2_server:store(Reopened),
                        {ok, Reopened}
                end
            end);
        undefined ->
            transactional(DbName, Options, fun(TxDb) ->
                Opened = fabric2_fdb:open(TxDb, Options),
                ok = fabric2_server:store(Opened),
                {ok, Opened#{tx => undefined}}
            end)
    end.


delete(DbName, Options) ->
    % This will throw if the db does not exist
    {ok, Db} = open(DbName, Options),
    with_tx(Db, fun(TxDb) ->
        fabric2_fdb:delete(TxDb)
    end).


is_admin(Db) ->
    % TODO: Need to re-consider couch_db_plugin:check_is_admin/1
    {SecProps} = get_security(Db),
    UserCtx = get_user_ctx(Db),
    {Admins} = get_admins(SecProps),
    is_authorized(Admins, UserCtx).


check_is_admin(Db) ->
    case is_admin(Db) of
        true ->
            ok;
        false ->
            UserCtx = get_user_ctx(Db),
            Reason = <<"You are not a db or server admin.">>,
            throw_security_error(UserCtx, Reason)
    end.


check_is_member(Db) ->
    case is_member(Db) of
        true ->
            ok;
        false ->
            UserCtx = get_user_ctx(Db),
            throw_security_error(UserCtx)
    end.


name(#{name := DbName}) ->
    DbName.


get_after_doc_read_fun(#{after_doc_read := AfterDocRead}) ->
    AfterDocRead.


get_before_doc_update_fun(#{before_doc_update := BeforeDocUpdate}) ->
    BeforeDocUpdate.

get_committed_update_seq(#{} = Db) ->
    get_update_seq(Db).


get_compacted_seq(#{} = Db) ->
    get_update_seq(Db).


get_compactor_pid(#{} = _Db) ->
    nil.


get_db_info(#{} = Db) ->
    DbProps = with_tx(Db, fun(TxDb) ->
        fabric2_fdb:get_info(TxDb)
    end),

    BaseProps = [
        {cluster, {[{n, 0}, {q, 0}, {r, 0}, {w, 0}]}},
        {compact_running, false},
        {data_size, 0},
        {db_name, name(Db)},
        {disk_format_version, 0},
        {disk_size, 0},
        {instance_start_time, <<"0">>},
        {purge_seq, 0}
    ],

    {ok, lists:foldl(fun({Key, Val}, Acc) ->
        lists:keystore(Key, 1, Acc, {Key, Val})
    end, BaseProps, DbProps)}.


get_del_doc_count(#{} = Db) ->
    get_doc_count(Db, <<"doc_del_count">>).


get_doc_count(Db) ->
    get_doc_count(Db, <<"doc_count">>).


get_doc_count(Db, <<"_all_docs">>) ->
    get_doc_count(Db, <<"doc_count">>);

get_doc_count(DbName, <<"_design">>) ->
    get_doc_count(DbName, <<"doc_design_count">>);

get_doc_count(DbName, <<"_local">>) ->
    get_doc_count(DbName, <<"doc_local_count">>);

get_doc_count(Db, Key) ->
    with_tx(Db, fun(TxDb) ->
        fabric2_fdb:get_stat(TxDb, Key)
    end).


get_instance_start_time(#{}) ->
    0.


get_pid(#{}) ->
    nil.


get_revs_limit(#{revs_limit := RevsLimit}) ->
    RevsLimit.


get_security(#{security_doc := SecurityDoc}) ->
    SecurityDoc.


get_update_seq(#{} = Db) ->
    case fabric2_fdb:get_changes(Db, [{limit, 1}, {reverse, true}]) of
        [] ->
            fabric2_util:to_hex(<<0:80>>);
        [{Seq, _}] ->
            Seq
    end.


get_user_ctx(#{user_ctx := UserCtx}) ->
    UserCtx.


get_uuid(#{uuid := UUID}) ->
    UUID.


is_clustered(#{}) ->
    false.


is_db(#{name := _}) ->
    true;
is_db(_) ->
    false.


is_partitioned(#{}) ->
    false.


is_system_db(#{name := DbName}) ->
    is_system_db_name(DbName).


is_system_db_name(DbName) when is_list(DbName) ->
    is_system_db_name(?l2b(DbName));
is_system_db_name(DbName) when is_binary(DbName) ->
    Suffix = filename:basename(DbName),
    case {filename:dirname(DbName), lists:member(Suffix, ?SYSTEM_DATABASES)} of
        {<<".">>, Result} -> Result;
        {_Prefix, false} -> false;
        {Prefix, true} ->
            ReOpts =  [{capture,none}, dollar_endonly],
            re:run(Prefix, ?DBNAME_REGEX, ReOpts) == match
    end.


set_revs_limit(#{} = Db, RevsLimit) ->
    RevsLimBin = ?uint2bin(RevsLimit),
    with_tx(Db, fun(TxDb) ->
        fabric2_fdb:set_config(TxDb, <<"revs_limit">>, RevsLimBin)
    end).


set_security(#{} = Db, Security) ->
    SecBin = ?JSON_ENCODE(Security),
    with_tx(Db, fun(TxDb) ->
        fabric2_fdb:set_config(TxDb, <<"security_doc">>, SecBin)
    end).


set_user_ctx(#{} = Db, UserCtx) ->
    Db#{user_ctx => UserCtx}.


ensure_full_commit(#{}) ->
    {ok, 0}.


ensure_full_commit(#{}, _Timeout) ->
    {ok, 0}.


open_doc(#{} = Db, DocId) ->
    open_doc(Db, DocId, []).


open_doc(#{} = Db, DocId, _Options) ->
    with_tx(Db, fun(TxDb) ->
        case fabric2_fdb:get_full_doc_info(TxDb, DocId) of
            not_found ->
                {not_found, missing};
            #full_doc_info{} = FDI ->
                {_, Path} = couch_doc:to_doc_info_path(FDI),
                case fabric2_fdb:get_doc_body(TxDb, DocId, Path) of
                    #doc{} = Doc -> {ok, Doc};
                    Error -> Error
                end
        end
    end).


open_doc_revs(Db, DocId, Revs, Options) ->
    Latest = lists:member(latest, Options),
    with_tx(Db, fun(TxDb) ->
        #full_doc_info{
            rev_tree = RevTree
        } = fabric2_db:get_full_doc_info(TxDb, DocId),
        {Found, Missing} = case Revs of
            all ->
                {couch_key_tree:get_all_leafs(RevTree), []};
            _ when Latest ->
                couch_key_tree:get_key_leafs(RevTree, Revs);
            _ ->
                couch_key_tree:get(RevTree, Revs)
        end,
        Docs = lists:map(fun({Value, {Pos, [Rev | _]} = RevPath}) ->
            case Value of
                ?REV_MISSING ->
                    % We have the rev in our list but know nothing about it
                    {{not_found, missing}, {Pos, Rev}};
                _ ->
                    case fabric2_fdb:get_doc_body(TxDb, DocId, RevPath) of
                        #doc{} = Doc -> {ok, Doc};
                        Else -> {Else, {Pos, Rev}}
                    end
            end
        end, Found),
        MissingDocs = [{{not_found, missing}, MRev} || MRev <- Missing],
        {ok, Docs ++ MissingDocs}
    end).


update_doc(Db, Doc) ->
    update_doc(Db, Doc, []).


update_doc(Db, Doc, Options) ->
    with_tx(Db, fun(TxDb) ->
        update_doc_int(TxDb, Doc, Options)
    end).


update_docs(Db, Docs) ->
    update_docs(Db, Docs, []).


update_docs(Db, Docs, Options) ->
    with_tx(Db, fun(TxDb) ->
        {Resps, Status} = lists:mapfoldl(fun(Doc, Acc) ->
            case update_doc_int(TxDb, Doc, Options) of
                {ok, _} = Resp ->
                    {Resp, Acc};
                {error, _} = Resp ->
                    {Resp, error}
            end
        end, ok, Docs),
        {Status, Resps}
    end).


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


maybe_set_user_ctx(Db, Options) ->
    case fabric2_util:get_value(user_ctx, Options) of
        #user_ctx{} = UserCtx ->
            set_user_ctx(Db, UserCtx);
        undefined ->
            Db
    end.


is_member(Db) ->
    {SecProps} = get_security(Db),
    case is_admin(Db) of
        true ->
            true;
        false ->
            case is_public_db(SecProps) of
                true ->
                    true;
                false ->
                    {Members} = get_members(SecProps),
                    UserCtx = get_user_ctx(Db),
                    is_authorized(Members, UserCtx)
            end
    end.


is_authorized(Group, UserCtx) ->
    #user_ctx{
        name = UserName,
        roles = UserRoles
    } = UserCtx,
    Names = fabric2_util:get_value(<<"names">>, Group, []),
    Roles = fabric2_util:get_value(<<"roles">>, Group, []),
    case check_security(roles, UserRoles, [<<"_admin">> | Roles]) of
        true ->
            true;
        false ->
            check_security(names, UserName, Names)
    end.


check_security(roles, [], _) ->
    false;
check_security(roles, UserRoles, Roles) ->
    UserRolesSet = ordsets:from_list(UserRoles),
    RolesSet = ordsets:from_list(Roles),
    not ordsets:is_disjoint(UserRolesSet, RolesSet);
check_security(names, _, []) ->
    false;
check_security(names, null, _) ->
    false;
check_security(names, UserName, Names) ->
    lists:member(UserName, Names).


throw_security_error(#user_ctx{name = null} = UserCtx) ->
    Reason = <<"You are not authorized to access this db.">>,
    throw_security_error(UserCtx, Reason);
throw_security_error(#user_ctx{name = _} = UserCtx) ->
    Reason = <<"You are not allowed to access this db.">>,
    throw_security_error(UserCtx, Reason).


throw_security_error(#user_ctx{} = UserCtx, Reason) ->
    Error = security_error_type(UserCtx),
    throw({Error, Reason}).


security_error_type(#user_ctx{name = null}) ->
    unauthorized;
security_error_type(#user_ctx{name = _}) ->
    forbidden.


is_public_db(SecProps) ->
    {Members} = get_members(SecProps),
    Names = fabric2_util:get_value(<<"names">>, Members, []),
    Roles = fabric2_util:get_value(<<"roles">>, Members, []),
    Names =:= [] andalso Roles =:= [].


get_admins(SecProps) ->
    fabric2_util:get_value(<<"admins">>, SecProps, {[]}).


get_members(SecProps) ->
    % we fallback to readers here for backwards compatibility
    case fabric2_util:get_value(<<"members">>, SecProps) of
        undefined ->
            fabric2_util:get_value(<<"readers">>, SecProps, {[]});
        Members ->
            Members
    end.


% TODO: Handle _local docs separately.
update_doc_int(#{} = Db, #doc{} = Doc0, Options) ->
    UpdateType = case lists:member(replicated_changes, Options) of
        true -> replicated_changes;
        false -> interactive_edit
    end,

    try
        FDI1 = fabric2_fdb:get_full_doc_info(Db, Doc0#doc.id),
        Doc1 = prep_and_validate(Db, FDI1, Doc0, UpdateType),
        Doc2 = case UpdateType of
            interactive_edit -> new_revid(Doc1);
            replicated_changes -> Doc1
        end,
        FDI2 = if FDI1 /= not_found -> FDI1; true ->
            #full_doc_info{id = Doc2#doc.id}
        end,
        {FDI3, Doc3} = merge_rev_tree(FDI2, Doc2, UpdateType),

        OldExists = case FDI1 of
            not_found -> false;
            #full_doc_info{deleted = true} -> false;
            _ -> true
        end,
        NewExists = not FDI3#full_doc_info.deleted,

        ok = fabric2_fdb:store_doc(Db, FDI3, Doc3),

        case {OldExists, NewExists} of
            {false, true} ->
                fabric2_fdb:incr_stat(Db, <<"doc_count">>, 1);
            {true, false} ->
                fabric2_fdb:incr_stat(Db, <<"doc_count">>, -1),
                fabric2_fdb:incr_stat(Db, <<"doc_del_count">>, 1);
            {Exists, Exists} ->
                % No change
                ok
        end,

        % Need to count design documents
        % Need to track db size changes
        % Need to update VDUs on ddoc change

        #doc{
            revs = {RevStart, [Rev | _]}
        } = Doc3,
        {ok, {RevStart, Rev}}
    catch throw:{?MODULE, Return} ->
        Return
    end.


prep_and_validate(Db, not_found, Doc, UpdateType) ->
    case Doc#doc.revs of
        {0, []} ->
            ok;
        _ when UpdateType == replicated_changes ->
            ok;
        _ ->
            ?RETURN({error, conflict})
    end,
    prep_and_validate(Db, Doc, fun() -> nil end);

prep_and_validate(Db, FDI, Doc, interactive_edit) ->
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
                    fun() ->
                        fabric2_fdb:get_doc_body(Db, Doc#doc.id, {Start, Path})
                    end;
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
    prep_and_validate(Db, Doc, GetDocFun);

prep_and_validate(Db, FDI, Doc, replicated_changes) ->
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
                PrevRevs -> fun() ->
                    fabric2_fdb:get_doc_body(Db, Doc#doc.id, PrevRevs)
                end
            end;
        _ ->
            % The update merged to an internal node that we
            % already know about which means we're done with
            % this update.
            ?RETURN({ok, []})
    end,

    prep_and_validate(Db, Doc, GetDocFun).


prep_and_validate(Db, Doc, GetDocBody) ->
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
    validate_doc_update(Db, NewDoc, GetDocBody),
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
            Body = case fabric2_util:get_value(comp_body, Doc#doc.meta) of
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


validate_doc_update(Db, #doc{id = <<"_design/", _/binary>>} = Doc, _) ->
    case catch check_is_admin(Db) of
        ok -> validate_ddoc(Db, Doc);
        Error -> ?RETURN(Error)
    end;
validate_doc_update(_Db, #doc{id = <<"_local/", _/binary>>}, _) ->
    ok;
validate_doc_update(Db, Doc, GetDiskDocFun) ->
    #{
        security_doc := Security,
        user_ctx := UserCtx,
        validate_doc_update_funs := VDUs
    } = Db,
    Fun = fun() ->
        DiskDoc = GetDiskDocFun(),
        JsonCtx = fabric2_util:user_ctx_to_json(UserCtx),
        try
            lists:map(fun(VDU) ->
                case VDU(Doc, DiskDoc, JsonCtx, Security) of
                    ok -> ok;
                    Error -> ?RETURN(Error)
                end
            end, VDUs),
            ok
        catch
            throw:Error ->
                Error
        end
    end,
    Stat = [couchdb, query_server, vdu_process_time],
    if VDUs == [] -> ok; true ->
        couch_stats:update_histogram(Stat, Fun)
    end.


validate_ddoc(Db, DDoc) ->
    try
        ok = couch_index_server:validate(Db, couch_doc:with_ejson_body(DDoc))
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


transactional(DbName, Options, Fun) ->
    fabric2_util:transactional(fun(Tx) ->
        Fun(fabric2_fdb:init(Tx, DbName, Options))
    end).


with_tx(#{tx := undefined} = Db, Fun) ->
    fabric2_util:transactional(fun(Tx) ->
        Fun(Db#{tx => Tx})
    end);

with_tx(#{tx := {erlfdb_transaction, _}} = Db, Fun) ->
    Fun(Db).
