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

    list_dbs/0,
    list_dbs/1,

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

    fold_docs/3,
    fold_docs/4,
    %% fold_local_docs/4,
    %% fold_design_docs/4,
    fold_changes/4,
    fold_changes/5,
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
    Result = fabric2_fdb:transactional(DbName, Options, fun(TxDb) ->
        case fabric2_fdb:exists(TxDb) of
            true ->
                {error, file_exists};
            false ->
                fabric2_fdb:create(TxDb, Options)
        end
    end),
    % We cache outside of the transaction so that we're sure
    % that the transaction was committed.
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
            {ok, maybe_set_user_ctx(Db, Options)};
        undefined ->
            fabric2_fdb:transactional(DbName, Options, fun(TxDb) ->
                Opened = fabric2_fdb:open(TxDb, Options),
                ok = fabric2_server:store(Opened),
                {ok, Opened#{tx => undefined}}
            end)
    end.


delete(DbName, Options) ->
    % This will throw if the db does not exist
    {ok, Db} = open(DbName, Options),
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        fabric2_fdb:delete(TxDb)
    end).


list_dbs() ->
    list_dbs([]).


list_dbs(Options) ->
    fabric2_util:transactional(fun(Tx) ->
        fabric2_fdb:list_dbs(Tx, Options)
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
    DbProps = fabric2_fdb:transactional(Db, fun(TxDb) ->
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
    fabric2_fdb:transactional(Db, fun(TxDb) ->
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
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        fabric2_fdb:set_config(TxDb, <<"revs_limit">>, RevsLimBin)
    end).


set_security(#{} = Db, Security) ->
    SecBin = ?JSON_ENCODE(Security),
    fabric2_fdb:transactional(Db, fun(TxDb) ->
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
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        case fabric2_fdb:get_winning_revs(TxDb, DocId, 1) of
            [] ->
                {not_found, missing};
            [#{winner := true} = RevInfo] ->
                case fabric2_fdb:get_doc_body(TxDb, DocId, RevInfo) of
                    #doc{} = Doc -> {ok, Doc};
                    Else -> Else
                end
        end
    end).


open_doc_revs(Db, DocId, Revs, Options) ->
    Latest = lists:member(latest, Options),
    fabric2_fdb:transactional(Db, fun(TxDb) ->
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
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        update_doc_int(TxDb, Doc, Options)
    end).


update_docs(Db, Docs) ->
    update_docs(Db, Docs, []).


update_docs(Db, Docs, Options) ->
    {Resps, Status} = lists:mapfoldl(fun(Doc, Acc) ->
        fabric2_fdb:transactional(Db, fun(TxDb) ->
            case update_doc_int(TxDb, Doc, Options) of
                {ok, _} = Resp ->
                    {Resp, Acc};
                {error, _} = Resp ->
                    {Resp, error}
            end
        end)
    end, ok, Docs),
    {Status, Resps}.


fold_docs(Db, UserFun, UserAcc) ->
    fold_docs(Db, UserFun, UserAcc, []).


fold_docs(Db, UserFun, UserAcc, Options) ->
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        fabric2_fdb:fold_docs(TxDb, UserFun, UserAcc, Options)
    end).


fold_changes(Db, SinceSeq, UserFun, UserAcc) ->
    fold_changes(Db, SinceSeq, UserFun, UserAcc, []).


fold_changes(Db, SinceSeq, UserFun, UserAcc, Options) ->
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        fabric2_fdb:fold_changes(TxDb, SinceSeq, UserFun, UserAcc, Options)
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
update_doc_int(#{} = Db, #doc{} = Doc, Options) ->
    try
        case lists:member(replicated_changes, Options) of
            false -> update_doc_interactive(Db, Doc, Options);
            true -> update_doc_replicated(Db, Doc, Options)
        end
    catch throw:{?MODULE, Return} ->
        Return
    end.


update_doc_interactive(Db, Doc0, _Options) ->
    % Get the current winning revision. This is needed
    % regardless of which branch we're updating. The extra
    % revision we're grabbing is an optimization to
    % save us a round trip if we end up deleting
    % the winning revision branch.
    NumRevs = if Doc0#doc.deleted -> 2; true -> 1 end,
    RevInfos = fabric2_fdb:get_winning_revs(Db, Doc0#doc.id, NumRevs),
    {Winner, SecondPlace} = case RevInfos of
        [] -> {not_found, not_found};
        [WRI] -> {WRI, not_found};
        [WRI, SPRI] -> {WRI, SPRI}
    end,
    WinnerRevId = case Winner of
        not_found ->
            {0, <<>>};
        _ ->
            case maps:get(deleted, Winner) of
                true -> {0, <<>>};
                false -> maps:get(rev_id, Winner)
            end
    end,

    % Check that a revision was specified if required
    Doc0RevId = doc_to_revid(Doc0),
    if Doc0RevId /= {0, <<>>} orelse WinnerRevId == {0, <<>>} -> ok; true ->
        ?RETURN({error, conflict})
    end,

    % Check that we're not trying to create a deleted doc
    if Doc0RevId /= {0, <<>>} orelse not Doc0#doc.deleted -> ok; true ->
        ?RETURN({error, conflict})
    end,

    % Get the target revision to update
    Target = case Doc0RevId == WinnerRevId of
        true ->
            Winner;
        false ->
            case fabric2_fdb:get_non_deleted_rev(Db, Doc0#doc.id, Doc0RevId) of
                #{deleted := false} = Target0 ->
                    Target0;
                not_found ->
                    % Either a missing revision or a deleted
                    % revision. Either way a conflict. Note
                    % that we get not_found for a deleted revision
                    % because we only check for the non-deleted
                    % key in fdb
                    ?RETURN({error, conflict})
            end
    end,

    % When recreating a deleted document we want to extend
    % the winning revision branch rather than create a
    % new branch. If we did not do this we could be
    % recreating into a state that previously existed.
    Doc1 = case Winner of
        #{deleted := true} when not Doc0#doc.deleted ->
            {WinnerRevPos, WinnerRev} = maps:get(rev_id, Winner),
            Doc0#doc{revs = {WinnerRevPos, [WinnerRev]}};
        #{deleted := true} when Doc0#doc.deleted ->
            % We disable extending deleted revisions with
            % new deletions during interactive updates
            ?RETURN({error, conflict});
        _ ->
            Doc0
    end,

    % Validate the doc update and create the
    % new revinfo map
    Doc2 = prep_and_validate(Db, Doc1, Target),
    #doc{
        deleted = NewDeleted,
        revs = {NewRevPos, [NewRev | NewRevPath]}
    } = Doc3 = new_revid(Doc2),

    NewRevInfo = #{
        winner => undefined,
        deleted => NewDeleted,
        rev_id => {NewRevPos, NewRev},
        rev_path => NewRevPath,
        sequence => undefined,
        branch_count => undefined
    },

    % Gather the list of possible winnig revisions
    Possible = case Target == Winner of
        true when not Doc3#doc.deleted ->
            [NewRevInfo];
        true when Doc3#doc.deleted ->
            case SecondPlace of
                #{} -> [NewRevInfo, SecondPlace];
                not_found -> [NewRevInfo]
            end;
        false ->
            [NewRevInfo, Winner]
    end,

    % Sort the rev infos such that the winner is first
    {NewWinner0, NonWinner} = case fabric2_util:sort_revinfos(Possible) of
        [W] -> {W, not_found};
        [W, NW] -> {W, NW}
    end,

    NewWinner = NewWinner0#{branch_count := maps:get(branch_count, Winner)},
    ToUpdate = if NonWinner == not_found -> []; true -> [NonWinner] end,
    ToRemove = if Target == not_found -> []; true -> [Target] end,

    ok = fabric2_fdb:write_doc(
            Db,
            Doc3,
            NewWinner,
            Winner,
            ToUpdate,
            ToRemove
        ),

    {ok, {NewRevPos, NewRev}}.


update_doc_replicated(Db, Doc0, _Options) ->
    #doc{
        id = DocId,
        deleted = Deleted,
        revs = {RevPos, [Rev | RevPath]}
    } = Doc0,

    DocRevInfo = #{
        winner => undefined,
        deleted => Deleted,
        rev_id => {RevPos, Rev},
        rev_path => RevPath,
        sequence => undefined,
        branch_count => undefined
    },

    AllRevInfos = fabric2_fdb:get_all_revs(Db, DocId),

    RevTree = lists:foldl(fun(RI, TreeAcc) ->
        RIPath = fabric2_util:revinfo_to_path(RI),
        {Merged, _} = couch_key_tree:merge(TreeAcc, RIPath),
        Merged
    end, [], AllRevInfos),

    DocRevPath = fabric2_util:revinfo_to_path(DocRevInfo),
    {NewTree, Status} = couch_key_tree:merge(RevTree, DocRevPath),
    if Status /= internal_node -> ok; true ->
        % We already know this revision so nothing
        % left to do.
        ?RETURN({ok, []})
    end,

    AllLeafsFull = couch_key_tree:get_all_leafs_full(NewTree),
    LeafPath = get_leaf_path(RevPos, Rev, AllLeafsFull),
    PrevRevInfo = find_prev_revinfo(RevPos, LeafPath),
    Doc1 = prep_and_validate(Db, Doc0, PrevRevInfo),

    % Possible winners are the previous winner and
    % the new DocRevInfo
    Winner = case fabric2_util:sort_revinfos(AllRevInfos) of
        [#{winner := true} = WRI | _] -> WRI;
        [] -> not_found
    end,
    {NewWinner0, NonWinner} = case Winner == PrevRevInfo of
        true ->
            {DocRevInfo, not_found};
        false ->
            [W, NW] = fabric2_util:sort_revinfos([Winner, DocRevInfo]),
            {W, NW}
    end,

    NewWinner = NewWinner0#{branch_count := length(AllLeafsFull)},
    ToUpdate = if NonWinner == not_found -> []; true -> [NonWinner] end,
    ToRemove = if PrevRevInfo == not_found -> []; true -> [PrevRevInfo] end,

    ok = fabric2_fdb:write_doc(
            Db,
            Doc1,
            NewWinner,
            Winner,
            ToUpdate,
            ToRemove
        ),

    #doc{
        revs = {NewRevPos, [NewRev | _]}
    } = Doc1,
    {ok, {NewRevPos, NewRev}}.


prep_and_validate(Db, Doc, PrevRevInfo) ->
    HasStubs = couch_doc:has_stubs(Doc),
    HasVDUs = [] /= maps:get(validate_doc_update_funs, Db),
    IsDDoc = case Doc#doc.id of
        <<?DESIGN_DOC_PREFIX, _/binary>> -> true;
        _ -> false
    end,

    PrevDoc = case HasStubs orelse (HasVDUs and not IsDDoc) of
        true when PrevRevInfo /= not_found ->
            case fabric2_fdb:get_doc_body(Db, Doc#doc.id, PrevRevInfo) of
                #doc{} = Doc -> Doc;
                {not_found, _} -> nil
            end;
        _ ->
            nil
    end,

    MergedDoc = if not HasStubs -> Doc; true ->
        % This will throw an error if we have any
        % attachment stubs missing data
        couch_doc:merge_stubs(Doc, PrevDoc)
    end,
    validate_doc_update(Db, MergedDoc, PrevDoc),
    MergedDoc.


validate_doc_update(Db, #doc{id = <<"_design/", _/binary>>} = Doc, _) ->
    case catch check_is_admin(Db) of
        ok -> validate_ddoc(Db, Doc);
        Error -> ?RETURN(Error)
    end;
validate_doc_update(_Db, #doc{id = <<"_local/", _/binary>>}, _) ->
    ok;
validate_doc_update(Db, Doc, PrevDoc) ->
    #{
        security_doc := Security,
        user_ctx := UserCtx,
        validate_doc_update_funs := VDUs
    } = Db,
    Fun = fun() ->
        JsonCtx = fabric2_util:user_ctx_to_json(UserCtx),
        try
            lists:map(fun(VDU) ->
                case VDU(Doc, PrevDoc, JsonCtx, Security) of
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


get_leaf_path(Pos, Rev, [{Pos, [{Rev, _RevInfo} | LeafPath]} | _]) ->
    LeafPath;
get_leaf_path(Pos, Rev, [_WrongLeaf | RestLeafs]) ->
    get_leaf_path(Pos, Rev, RestLeafs).


find_prev_revinfo(_Pos, []) ->
    not_found;
find_prev_revinfo(Pos, [{_Rev, ?REV_MISSING} | RestPath]) ->
    find_prev_revinfo(Pos - 1, RestPath);
find_prev_revinfo(_Pos, [{_Rev, #{} = RevInfo} | _]) ->
    RevInfo.


doc_to_revid(#doc{revs = Revs}) ->
    case Revs of
        {0, []} -> {0, <<>>};
        {RevPos, [Rev | _]} -> {RevPos, Rev}
    end.
