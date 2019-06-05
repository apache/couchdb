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

-module(fabric2_fdb).


-export([
    transactional/1,
    transactional/3,
    transactional/2,

    create/2,
    open/2,
    reopen/1,
    delete/1,
    exists/1,

    list_dbs/2,

    get_info/1,
    get_config/1,
    set_config/3,

    get_stat/2,
    incr_stat/3,

    get_all_revs/2,
    get_winning_revs/3,
    get_winning_revs_future/3,
    get_winning_revs_wait/2,
    get_non_deleted_rev/3,

    get_doc_body/3,
    get_doc_body_future/3,
    get_doc_body_wait/4,
    get_local_doc/2,

    write_doc/6,
    write_local_doc/2,

    read_attachment/3,
    write_attachment/3,

    fold_docs/4,
    fold_changes/5,
    get_last_change/1,

    vs_to_seq/1,

    debug_cluster/0,
    debug_cluster/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("fabric2.hrl").


transactional(Fun) ->
    do_transaction(Fun, undefined).


transactional(DbName, Options, Fun) when is_binary(DbName) ->
    transactional(fun(Tx) ->
        Fun(init_db(Tx, DbName, Options))
    end).


transactional(#{tx := undefined} = Db, Fun) ->
    #{layer_prefix := LayerPrefix} = Db,
    do_transaction(fun(Tx) ->
        Fun(Db#{tx => Tx})
    end, LayerPrefix);

transactional(#{tx := {erlfdb_transaction, _}} = Db, Fun) ->
    Fun(Db).


do_transaction(Fun, LayerPrefix) when is_function(Fun, 1) ->
    Db = get_db_handle(),
    try
        erlfdb:transactional(Db, fun(Tx) ->
            case get(erlfdb_trace) of
                Name when is_binary(Name) ->
                    erlfdb:set_option(Tx, transaction_logging_enable, Name);
                _ ->
                    ok
            end,
            case is_transaction_applied(Tx) of
                true ->
                    get_previous_transaction_result();
                false ->
                    execute_transaction(Tx, Fun, LayerPrefix)
            end
        end)
    after
        clear_transaction()
    end.


create(#{} = Db0, Options) ->
    #{
        name := DbName,
        tx := Tx,
        layer_prefix := LayerPrefix
    } = Db = ensure_current(Db0, false),

    % Eventually DbPrefix will be HCA allocated. For now
    % we're just using the DbName so that debugging is easier.
    DbKey = erlfdb_tuple:pack({?ALL_DBS, DbName}, LayerPrefix),
    DbPrefix = erlfdb_tuple:pack({?DBS, DbName}, LayerPrefix),
    erlfdb:set(Tx, DbKey, DbPrefix),

    % This key is responsible for telling us when something in
    % the database cache (i.e., fabric2_server's ets table) has
    % changed and requires re-loading. This currently includes
    % revs_limit and validate_doc_update functions. There's
    % no order to versioning here. Its just a value that changes
    % that is used in the ensure_current check.
    DbVersionKey = erlfdb_tuple:pack({?DB_VERSION}, DbPrefix),
    DbVersion = fabric2_util:uuid(),
    erlfdb:set(Tx, DbVersionKey, DbVersion),

    UUID = fabric2_util:uuid(),

    Defaults = [
        {?DB_CONFIG, <<"uuid">>, UUID},
        {?DB_CONFIG, <<"revs_limit">>, ?uint2bin(1000)},
        {?DB_CONFIG, <<"security_doc">>, <<"{}">>},
        {?DB_STATS, <<"doc_count">>, ?uint2bin(0)},
        {?DB_STATS, <<"doc_del_count">>, ?uint2bin(0)},
        {?DB_STATS, <<"doc_design_count">>, ?uint2bin(0)},
        {?DB_STATS, <<"doc_local_count">>, ?uint2bin(0)},
        {?DB_STATS, <<"size">>, ?uint2bin(2)}
    ],
    lists:foreach(fun({P, K, V}) ->
        Key = erlfdb_tuple:pack({P, K}, DbPrefix),
        erlfdb:set(Tx, Key, V)
    end, Defaults),

    UserCtx = fabric2_util:get_value(user_ctx, Options, #user_ctx{}),

    Db#{
        uuid => UUID,
        db_prefix => DbPrefix,
        db_version => DbVersion,

        revs_limit => 1000,
        security_doc => {[]},
        user_ctx => UserCtx,

        validate_doc_update_funs => [],
        before_doc_update => undefined,
        after_doc_read => undefined,
        % All other db things as we add features,

        db_options => Options
    }.


open(#{} = Db0, Options) ->
    #{
        name := DbName,
        tx := Tx,
        layer_prefix := LayerPrefix
    } = Db1 = ensure_current(Db0, false),

    DbKey = erlfdb_tuple:pack({?ALL_DBS, DbName}, LayerPrefix),
    DbPrefix = case erlfdb:wait(erlfdb:get(Tx, DbKey)) of
        Bin when is_binary(Bin) -> Bin;
        not_found -> erlang:error(database_does_not_exist)
    end,

    DbVersionKey = erlfdb_tuple:pack({?DB_VERSION}, DbPrefix),
    DbVersion = erlfdb:wait(erlfdb:get(Tx, DbVersionKey)),

    UserCtx = fabric2_util:get_value(user_ctx, Options, #user_ctx{}),

    Db2 = Db1#{
        db_prefix => DbPrefix,
        db_version => DbVersion,

        revs_limit => 1000,
        security_doc => {[]},
        user_ctx => UserCtx,

        % Place holders until we implement these
        % bits.
        validate_doc_update_funs => [],
        before_doc_update => undefined,
        after_doc_read => undefined,

        db_options => Options
    },

    Db3 = lists:foldl(fun({Key, Val}, DbAcc) ->
        case Key of
            <<"uuid">> ->
                DbAcc#{uuid => Val};
            <<"revs_limit">> ->
                DbAcc#{revs_limit => ?bin2uint(Val)};
            <<"security_doc">> ->
                DbAcc#{security_doc => ?JSON_DECODE(Val)}
        end
    end, Db2, get_config(Db2)),

    load_validate_doc_funs(Db3).


reopen(#{} = OldDb) ->
    require_transaction(OldDb),
    #{
        tx := Tx,
        name := DbName,
        db_options := Options
    } = OldDb,
    open(init_db(Tx, DbName, Options), Options).


delete(#{} = Db) ->
    #{
        name := DbName,
        tx := Tx,
        layer_prefix := LayerPrefix,
        db_prefix := DbPrefix
    } = ensure_current(Db),

    DbKey = erlfdb_tuple:pack({?ALL_DBS, DbName}, LayerPrefix),
    erlfdb:clear(Tx, DbKey),
    erlfdb:clear_range_startswith(Tx, DbPrefix),
    bump_metadata_version(Tx),
    ok.


exists(#{name := DbName} = Db) when is_binary(DbName) ->
    #{
        tx := Tx,
        layer_prefix := LayerPrefix
    } = ensure_current(Db, false),

    DbKey = erlfdb_tuple:pack({?ALL_DBS, DbName}, LayerPrefix),
    case erlfdb:wait(erlfdb:get(Tx, DbKey)) of
        Bin when is_binary(Bin) -> true;
        not_found -> false
    end.


list_dbs(Tx, _Options) ->
    Root = erlfdb_directory:root(),
    CouchDB = erlfdb_directory:create_or_open(Tx, Root, [<<"couchdb">>]),
    LayerPrefix = erlfdb_directory:get_name(CouchDB),
    {Start, End} = erlfdb_tuple:range({?ALL_DBS}, LayerPrefix),
    Future = erlfdb:get_range(Tx, Start, End),
    lists:map(fun({K, _V}) ->
        {?ALL_DBS, DbName} = erlfdb_tuple:unpack(K, LayerPrefix),
        DbName
    end, erlfdb:wait(Future)).


get_info(#{} = Db) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = ensure_current(Db),

    {CStart, CEnd} = erlfdb_tuple:range({?DB_CHANGES}, DbPrefix),
    ChangesFuture = erlfdb:get_range(Tx, CStart, CEnd, [
        {streaming_mode, exact},
        {limit, 1},
        {reverse, true}
    ]),

    StatsPrefix = erlfdb_tuple:pack({?DB_STATS}, DbPrefix),
    MetaFuture = erlfdb:get_range_startswith(Tx, StatsPrefix),

    RawSeq = case erlfdb:wait(ChangesFuture) of
        [] ->
            vs_to_seq(fabric2_util:seq_zero_vs());
        [{SeqKey, _}] ->
            {?DB_CHANGES, SeqVS} = erlfdb_tuple:unpack(SeqKey, DbPrefix),
            vs_to_seq(SeqVS)
    end,
    CProp = {update_seq, RawSeq},

    MProps = lists:flatmap(fun({K, V}) ->
        case erlfdb_tuple:unpack(K, DbPrefix) of
            {?DB_STATS, <<"doc_count">>} ->
                [{doc_count, ?bin2uint(V)}];
            {?DB_STATS, <<"doc_del_count">>} ->
                [{doc_del_count, ?bin2uint(V)}];
            {?DB_STATS, <<"size">>} ->
                Val = ?bin2uint(V),
                [
                    {other, {[{data_size, Val}]}},
                    {sizes, {[
                        {active, 0},
                        {external, Val},
                        {file, 0}
                    ]}}
                ];
            {?DB_STATS, _} ->
                []
        end
    end, erlfdb:wait(MetaFuture)),

    [CProp | MProps].


get_config(#{} = Db) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db = ensure_current(Db),

    {Start, End} = erlfdb_tuple:range({?DB_CONFIG}, DbPrefix),
    Future = erlfdb:get_range(Tx, Start, End),

    lists:map(fun({K, V}) ->
        {?DB_CONFIG, Key} = erlfdb_tuple:unpack(K, DbPrefix),
        {Key, V}
    end, erlfdb:wait(Future)).


set_config(#{} = Db, ConfigKey, ConfigVal) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = ensure_current(Db),

    Key = erlfdb_tuple:pack({?DB_CONFIG, ConfigKey}, DbPrefix),
    erlfdb:set(Tx, Key, ConfigVal),
    bump_metadata_version(Tx).


get_stat(#{} = Db, StatKey) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = ensure_current(Db),

    Key = erlfdb_tuple:pack({?DB_STATS, StatKey}, DbPrefix),

    % Might need to figure out some sort of type
    % system here. Uints are because stats are all
    % atomic op adds for the moment.
    ?bin2uint(erlfdb:wait(erlfdb:get(Tx, Key))).


incr_stat(_Db, _StatKey, 0) ->
    ok;

incr_stat(#{} = Db, StatKey, Increment) when is_integer(Increment) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = ensure_current(Db),

    Key = erlfdb_tuple:pack({?DB_STATS, StatKey}, DbPrefix),
    erlfdb:add(Tx, Key, Increment).


get_all_revs(#{} = Db, DocId) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = ensure_current(Db),

    Prefix = erlfdb_tuple:pack({?DB_REVS, DocId}, DbPrefix),
    Options = [{streaming_mode, want_all}],
    Future = erlfdb:get_range_startswith(Tx, Prefix, Options),
    lists:map(fun({K, V}) ->
        Key = erlfdb_tuple:unpack(K, DbPrefix),
        Val = erlfdb_tuple:unpack(V),
        fdb_to_revinfo(Key, Val)
    end, erlfdb:wait(Future)).


get_winning_revs(Db, DocId, NumRevs) ->
    Future = get_winning_revs_future(Db, DocId, NumRevs),
    get_winning_revs_wait(Db, Future).


get_winning_revs_future(#{} = Db, DocId, NumRevs) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = ensure_current(Db),

    {StartKey, EndKey} = erlfdb_tuple:range({?DB_REVS, DocId}, DbPrefix),
    Options = [{reverse, true}, {limit, NumRevs}],
    erlfdb:get_range_raw(Tx, StartKey, EndKey, Options).


get_winning_revs_wait(#{} = Db, Future) ->
    #{
        db_prefix := DbPrefix
    } = ensure_current(Db),
    {Rows, _, _} = erlfdb:wait(Future),
    lists:map(fun({K, V}) ->
        Key = erlfdb_tuple:unpack(K, DbPrefix),
        Val = erlfdb_tuple:unpack(V),
        fdb_to_revinfo(Key, Val)
    end, Rows).


get_non_deleted_rev(#{} = Db, DocId, RevId) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = ensure_current(Db),

    {RevPos, Rev} = RevId,

    BaseKey = {?DB_REVS, DocId, true, RevPos, Rev},
    Key = erlfdb_tuple:pack(BaseKey, DbPrefix),
    case erlfdb:wait(erlfdb:get(Tx, Key)) of
        not_found ->
            not_found;
        Val ->
            fdb_to_revinfo(BaseKey, erlfdb_tuple:unpack(Val))
    end.


get_doc_body(Db, DocId, RevInfo) ->
    Future = get_doc_body_future(Db, DocId, RevInfo),
    get_doc_body_wait(Db, DocId, RevInfo, Future).


get_doc_body_future(#{} = Db, DocId, RevInfo) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = ensure_current(Db),

    #{
        rev_id := {RevPos, Rev}
    } = RevInfo,

    Key = erlfdb_tuple:pack({?DB_DOCS, DocId, RevPos, Rev}, DbPrefix),
    erlfdb:get(Tx, Key).


get_doc_body_wait(#{} = Db0, DocId, RevInfo, Future) ->
    Db = ensure_current(Db0),

    #{
        rev_id := {RevPos, Rev},
        rev_path := RevPath
    } = RevInfo,

    Val = erlfdb:wait(Future),
    fdb_to_doc(Db, DocId, RevPos, [Rev | RevPath], Val).


get_local_doc(#{} = Db0, <<?LOCAL_DOC_PREFIX, _/binary>> = DocId) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db = ensure_current(Db0),

    Key = erlfdb_tuple:pack({?DB_LOCAL_DOCS, DocId}, DbPrefix),
    Val = erlfdb:wait(erlfdb:get(Tx, Key)),
    fdb_to_local_doc(Db, DocId, Val).


write_doc(#{} = Db0, Doc, NewWinner0, OldWinner, ToUpdate, ToRemove) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db = ensure_current(Db0),

    #doc{
        id = DocId,
        deleted = Deleted
    } = Doc,

    % Revision tree

    NewWinner = NewWinner0#{winner := true},
    NewRevId = maps:get(rev_id, NewWinner),

    {WKey, WVal, WinnerVS} = revinfo_to_fdb(Tx, DbPrefix, DocId, NewWinner),
    ok = erlfdb:set_versionstamped_value(Tx, WKey, WVal),

    lists:foreach(fun(RI0) ->
        RI = RI0#{winner := false},
        {K, V, undefined} = revinfo_to_fdb(Tx, DbPrefix, DocId, RI),
        ok = erlfdb:set(Tx, K, V)
    end, ToUpdate),

    lists:foreach(fun(RI0) ->
        RI = RI0#{winner := false},
        {K, _, undefined} = revinfo_to_fdb(Tx, DbPrefix, DocId, RI),
        ok = erlfdb:clear(Tx, K)
    end, ToRemove),

    % _all_docs

    UpdateStatus = case {OldWinner, NewWinner} of
        {not_found, #{deleted := false}} ->
            created;
        {#{deleted := true}, #{deleted := false}} ->
            recreated;
        {#{deleted := false}, #{deleted := false}} ->
            updated;
        {#{deleted := false}, #{deleted := true}} ->
            deleted
    end,

    case UpdateStatus of
        Status when Status == created orelse Status == recreated ->
            ADKey = erlfdb_tuple:pack({?DB_ALL_DOCS, DocId}, DbPrefix),
            ADVal = erlfdb_tuple:pack(NewRevId),
            ok = erlfdb:set(Tx, ADKey, ADVal);
        deleted ->
            ADKey = erlfdb_tuple:pack({?DB_ALL_DOCS, DocId}, DbPrefix),
            ok = erlfdb:clear(Tx, ADKey);
        updated ->
            ok
    end,

    % _changes

    if OldWinner == not_found -> ok; true ->
        OldSeq = maps:get(sequence, OldWinner),
        OldSeqKey = erlfdb_tuple:pack({?DB_CHANGES, OldSeq}, DbPrefix),
        erlfdb:clear(Tx, OldSeqKey)
    end,

    NewSeqKey = erlfdb_tuple:pack_vs({?DB_CHANGES, WinnerVS}, DbPrefix),
    NewSeqVal = erlfdb_tuple:pack({DocId, Deleted, NewRevId}),
    erlfdb:set_versionstamped_key(Tx, NewSeqKey, NewSeqVal),

    % And all the rest...

    ok = write_doc_body(Db, Doc),

    IsDDoc = case Doc#doc.id of
        <<?DESIGN_DOC_PREFIX, _/binary>> -> true;
        _ -> false
    end,

    if not IsDDoc -> ok; true ->
        bump_db_version(Db)
    end,

    case UpdateStatus of
        created ->
            if not IsDDoc -> ok; true ->
                incr_stat(Db, <<"doc_design_count">>, 1)
            end,
            incr_stat(Db, <<"doc_count">>, 1);
        recreated ->
            if not IsDDoc -> ok; true ->
                incr_stat(Db, <<"doc_design_count">>, 1)
            end,
            incr_stat(Db, <<"doc_count">>, 1),
            incr_stat(Db, <<"doc_del_count">>, -1);
        deleted ->
            if not IsDDoc -> ok; true ->
                incr_stat(Db, <<"doc_design_count">>, -1)
            end,
            incr_stat(Db, <<"doc_count">>, -1),
            incr_stat(Db, <<"doc_del_count">>, 1);
        updated ->
            ok
    end,

    ok.


write_local_doc(#{} = Db0, Doc) ->
    #{
        tx := Tx
    } = Db = ensure_current(Db0),

    {LDocKey, LDocVal} = local_doc_to_fdb(Db, Doc),

    WasDeleted = case erlfdb:wait(erlfdb:get(Tx, LDocKey)) of
        <<_/binary>> -> false;
        not_found -> true
    end,

    case Doc#doc.deleted of
        true -> erlfdb:clear(Tx, LDocKey);
        false -> erlfdb:set(Tx, LDocKey, LDocVal)
    end,

    case {WasDeleted, Doc#doc.deleted} of
        {true, false} ->
            incr_stat(Db, <<"doc_local_count">>, 1);
        {false, true} ->
            incr_stat(Db, <<"doc_local_count">>, -1);
        _ ->
            ok
    end,

    ok.


read_attachment(#{} = Db, DocId, AttId) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = ensure_current(Db),

    AttKey = erlfdb_tuple:pack({?DB_ATTS, DocId, AttId}, DbPrefix),
    case erlfdb:wait(erlfdb:get_range_startswith(Tx, AttKey)) of
        not_found ->
            throw({not_found, missing});
        KVs ->
            Vs = [V || {_K, V} <- KVs],
            iolist_to_binary(Vs)
    end.


write_attachment(#{} = Db, DocId, Data) when is_binary(Data) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = ensure_current(Db),

    AttId = fabric2_util:uuid(),
    Chunks = chunkify_attachment(Data),

    lists:foldl(fun(Chunk, ChunkId) ->
        AttKey = erlfdb_tuple:pack({?DB_ATTS, DocId, AttId, ChunkId}, DbPrefix),
        ok = erlfdb:set(Tx, AttKey, Chunk),
        ChunkId + 1
    end, 0, Chunks),
    {ok, AttId}.


fold_docs(#{} = Db, UserFun, UserAcc0, Options) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = ensure_current(Db),

    {Reverse, Start, End} = get_dir_and_bounds(DbPrefix, Options),

    DocCountKey = erlfdb_tuple:pack({?DB_STATS, <<"doc_count">>}, DbPrefix),
    DocCountBin = erlfdb:wait(erlfdb:get(Tx, DocCountKey)),

    try
        UserAcc1 = maybe_stop(UserFun({meta, [
            {total, ?bin2uint(DocCountBin)},
            {offset, null}
        ]}, UserAcc0)),

        UserAcc2 = erlfdb:fold_range(Tx, Start, End, fun({K, V}, UserAccIn) ->
            {?DB_ALL_DOCS, DocId} = erlfdb_tuple:unpack(K, DbPrefix),
            RevId = erlfdb_tuple:unpack(V),
            maybe_stop(UserFun({row, [
                {id, DocId},
                {key, DocId},
                {value, couch_doc:rev_to_str(RevId)}
            ]}, UserAccIn))
        end, UserAcc1, [{reverse, Reverse}] ++ Options),

        {ok, maybe_stop(UserFun(complete, UserAcc2))}
    catch throw:{stop, FinalUserAcc} ->
        {ok, FinalUserAcc}
    end.


fold_changes(#{} = Db, SinceSeq0, UserFun, UserAcc0, Options) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = ensure_current(Db),

    SinceSeq1 = get_since_seq(SinceSeq0),

    Reverse = case fabric2_util:get_value(dir, Options, fwd) of
        fwd -> false;
        rev -> true
    end,

    {Start0, End0} = case Reverse of
        false -> {SinceSeq1, fabric2_util:seq_max_vs()};
        true -> {fabric2_util:seq_zero_vs(), SinceSeq1}
    end,

    Start1 = erlfdb_tuple:pack({?DB_CHANGES, Start0}, DbPrefix),
    End1 = erlfdb_tuple:pack({?DB_CHANGES, End0}, DbPrefix),

    {Start, End} = case Reverse of
        false -> {erlfdb_key:first_greater_than(Start1), End1};
        true -> {Start1, erlfdb_key:first_greater_than(End1)}
    end,

    try
        {ok, erlfdb:fold_range(Tx, Start, End, fun({K, V}, UserAccIn) ->
            {?DB_CHANGES, SeqVS} = erlfdb_tuple:unpack(K, DbPrefix),
            {DocId, Deleted, RevId} = erlfdb_tuple:unpack(V),

            Change = #{
                id => DocId,
                sequence => vs_to_seq(SeqVS),
                rev_id => RevId,
                deleted => Deleted
            },

            maybe_stop(UserFun(Change, UserAccIn))
        end, UserAcc0, [{reverse, Reverse}] ++ Options)}
    catch throw:{stop, FinalUserAcc} ->
        {ok, FinalUserAcc}
    end.


get_last_change(#{} = Db) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = ensure_current(Db),

    {Start, End} = erlfdb_tuple:range({?DB_CHANGES}, DbPrefix),
    Options = [{limit, 1}, {reverse, true}],
    case erlfdb:get_range(Tx, Start, End, Options) of
        [] ->
            vs_to_seq(fabric2_util:seq_zero_vs());
        [{K, _V}] ->
            {?DB_CHANGES, SeqVS} = erlfdb_tuple:unpack(K, DbPrefix),
            vs_to_seq(SeqVS)
    end.


maybe_stop({ok, Acc}) ->
    Acc;
maybe_stop({stop, Acc}) ->
    throw({stop, Acc}).


vs_to_seq(VS) ->
    <<51:8, SeqBin:12/binary>> = erlfdb_tuple:pack({VS}),
    fabric2_util:to_hex(SeqBin).


debug_cluster() ->
    debug_cluster(<<>>, <<16#FE, 16#FF, 16#FF>>).


debug_cluster(Start, End) ->
    transactional(fun(Tx) ->
        lists:foreach(fun({Key, Val}) ->
            io:format("~s => ~s~n", [
                    string:pad(erlfdb_util:repr(Key), 60),
                    erlfdb_util:repr(Val)
                ])
        end, erlfdb:get_range(Tx, Start, End))
    end).


init_db(Tx, DbName, Options) ->
    Root = erlfdb_directory:root(),
    CouchDB = erlfdb_directory:create_or_open(Tx, Root, [<<"couchdb">>]),
    Prefix = erlfdb_directory:get_name(CouchDB),
    Version = erlfdb:wait(erlfdb:get(Tx, ?METADATA_VERSION_KEY)),
    #{
        name => DbName,
        tx => Tx,
        layer_prefix => Prefix,
        md_version => Version,

        db_options => Options
    }.


load_validate_doc_funs(#{} = Db) ->
    FoldFun = fun
        ({row, Row}, Acc) ->
            DDocInfo = #{id => fabric2_util:get_value(id, Row)},
            {ok, [DDocInfo | Acc]};
        (_, Acc) ->
            {ok, Acc}
    end,

    Options = [
        {start_key, <<"_design/">>},
        {end_key, <<"_design0">>}
    ],

    {ok, Infos1} = fold_docs(Db, FoldFun, [], Options),

    Infos2 = lists:map(fun(Info) ->
        #{
            id := DDocId = <<"_design/", _/binary>>
        } = Info,
        Info#{
            rev_info => get_winning_revs_future(Db, DDocId, 1)
        }
    end, Infos1),

    Infos3 = lists:flatmap(fun(Info) ->
        #{
            id := DDocId,
            rev_info := RevInfoFuture
        } = Info,
        [RevInfo] = get_winning_revs_wait(Db, RevInfoFuture),
        #{deleted := Deleted} = RevInfo,
        if Deleted -> []; true ->
            [Info#{
                rev_info := RevInfo,
                body => get_doc_body_future(Db, DDocId, RevInfo)
            }]
        end
    end, Infos2),

    VDUs = lists:flatmap(fun(Info) ->
        #{
            id := DDocId,
            rev_info := RevInfo,
            body := BodyFuture
        } = Info,
        #doc{} = Doc = get_doc_body_wait(Db, DDocId, RevInfo, BodyFuture),
        case couch_doc:get_validate_doc_fun(Doc) of
            nil -> [];
            Fun -> [Fun]
        end
    end, Infos3),

    Db#{
        validate_doc_update_funs := VDUs
    }.


bump_metadata_version(Tx) ->
    % The 14 zero bytes is pulled from the PR for adding the
    % metadata version key. Not sure why 14 bytes when version
    % stamps are only 80, but whatever for now.
    erlfdb:set_versionstamped_value(Tx, ?METADATA_VERSION_KEY, <<0:112>>).


bump_db_version(#{} = Db) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    DbVersionKey = erlfdb_tuple:pack({?DB_VERSION}, DbPrefix),
    DbVersion = fabric2_util:uuid(),
    ok = erlfdb:set(Tx, DbVersionKey, DbVersion).


write_doc_body(#{} = Db0, #doc{} = Doc) ->
    #{
        tx := Tx
    } = Db = ensure_current(Db0),

    {NewDocKey, NewDocVal} = doc_to_fdb(Db, Doc),
    erlfdb:set(Tx, NewDocKey, NewDocVal).


revinfo_to_fdb(Tx, DbPrefix, DocId, #{winner := true} = RevId) ->
    #{
        deleted := Deleted,
        rev_id := {RevPos, Rev},
        rev_path := RevPath,
        branch_count := BranchCount
    } = RevId,
    VS = new_versionstamp(Tx),
    Key = {?DB_REVS, DocId, not Deleted, RevPos, Rev},
    Val = {?CURR_REV_FORMAT, VS, BranchCount, list_to_tuple(RevPath)},
    KBin = erlfdb_tuple:pack(Key, DbPrefix),
    VBin = erlfdb_tuple:pack_vs(Val),
    {KBin, VBin, VS};

revinfo_to_fdb(_Tx, DbPrefix, DocId, #{} = RevId) ->
    #{
        deleted := Deleted,
        rev_id := {RevPos, Rev},
        rev_path := RevPath
    } = RevId,
    Key = {?DB_REVS, DocId, not Deleted, RevPos, Rev},
    Val = {?CURR_REV_FORMAT, list_to_tuple(RevPath)},
    KBin = erlfdb_tuple:pack(Key, DbPrefix),
    VBin = erlfdb_tuple:pack(Val),
    {KBin, VBin, undefined}.


fdb_to_revinfo(Key, {?CURR_REV_FORMAT, _, _, _} = Val) ->
    {?DB_REVS, _DocId, NotDeleted, RevPos, Rev} = Key,
    {_RevFormat, Sequence, BranchCount, RevPath} = Val,
    #{
        winner => true,
        deleted => not NotDeleted,
        rev_id => {RevPos, Rev},
        rev_path => tuple_to_list(RevPath),
        sequence => Sequence,
        branch_count => BranchCount
    };

fdb_to_revinfo(Key, {?CURR_REV_FORMAT, _} = Val)  ->
    {?DB_REVS, _DocId, NotDeleted, RevPos, Rev} = Key,
    {_RevFormat, RevPath} = Val,
    #{
        winner => false,
        deleted => not NotDeleted,
        rev_id => {RevPos, Rev},
        rev_path => tuple_to_list(RevPath),
        sequence => undefined,
        branch_count => undefined
    }.


doc_to_fdb(Db, #doc{} = Doc) ->
    #{
        db_prefix := DbPrefix
    } = Db,

    #doc{
        id = Id,
        revs = {Start, [Rev | _]},
        body = Body,
        atts = Atts,
        deleted = Deleted
    } = doc_flush_atts(Db, Doc),

    Key = erlfdb_tuple:pack({?DB_DOCS, Id, Start, Rev}, DbPrefix),
    Val = {Body, Atts, Deleted},
    {Key, term_to_binary(Val, [{minor_version, 1}])}.


fdb_to_doc(_Db, DocId, Pos, Path, Bin) when is_binary(Bin) ->
    {Body, Atts, Deleted} = binary_to_term(Bin, [safe]),
    #doc{
        id = DocId,
        revs = {Pos, Path},
        body = Body,
        atts = Atts,
        deleted = Deleted
    };
fdb_to_doc(_Db, _DocId, _Pos, _Path, not_found) ->
    {not_found, missing}.


local_doc_to_fdb(Db, #doc{} = Doc) ->
    #{
        db_prefix := DbPrefix
    } = Db,

    #doc{
        id = Id,
        revs = {0, [Rev]},
        body = Body
    } = Doc,

    StoreRev = case Rev of
        _ when is_integer(Rev) -> integer_to_binary(Rev);
        _ when is_binary(Rev) -> Rev
    end,

    Key = erlfdb_tuple:pack({?DB_LOCAL_DOCS, Id}, DbPrefix),
    Val = {StoreRev, Body},
    {Key, term_to_binary(Val, [{minor_version, 1}])}.


fdb_to_local_doc(_Db, DocId, Bin) when is_binary(Bin) ->
    {Rev, Body} = binary_to_term(Bin, [safe]),
    #doc{
        id = DocId,
        revs = {0, [Rev]},
        deleted = false,
        body = Body
    };
fdb_to_local_doc(_Db, _DocId, not_found) ->
    {not_found, missing}.


doc_flush_atts(Db, Doc) ->
    Atts = lists:map(fun(Att) ->
        couch_att:flush(Db, Doc#doc.id, Att)
    end, Doc#doc.atts),
    Doc#doc{atts = Atts}.


chunkify_attachment(Data) ->
    case Data of
        <<>> ->
            [];
        <<Head:?ATTACHMENT_CHUNK_SIZE/binary, Rest/binary>> ->
            [Head | chunkify_attachment(Rest)];
        <<_/binary>> when size(Data) < ?ATTACHMENT_CHUNK_SIZE ->
            [Data]
    end.


get_dir_and_bounds(DbPrefix, Options) ->
    Reverse = case fabric2_util:get_value(dir, Options, fwd) of
        fwd -> false;
        rev -> true
    end,
    StartKey0 = fabric2_util:get_value(start_key, Options),
    EndKeyGt = fabric2_util:get_value(end_key_gt, Options),
    EndKey0 = fabric2_util:get_value(end_key, Options, EndKeyGt),
    InclusiveEnd = EndKeyGt == undefined,

    % CouchDB swaps the key meanings based on the direction
    % of the fold. FoundationDB does not so we have to
    % swap back here.
    {StartKey1, EndKey1} = case Reverse of
        false -> {StartKey0, EndKey0};
        true -> {EndKey0, StartKey0}
    end,

    % Set the maximum bounds for the start and endkey
    StartKey2 = case StartKey1 of
        undefined -> {?DB_ALL_DOCS};
        SK2 when is_binary(SK2) -> {?DB_ALL_DOCS, SK2}
    end,

    EndKey2 = case EndKey1 of
        undefined -> {?DB_ALL_DOCS, <<16#FF>>};
        EK2 when is_binary(EK2) -> {?DB_ALL_DOCS, EK2}
    end,

    StartKey3 = erlfdb_tuple:pack(StartKey2, DbPrefix),
    EndKey3 = erlfdb_tuple:pack(EndKey2, DbPrefix),

    % FoundationDB ranges are applied as SK <= key < EK
    % By default, CouchDB is SK <= key <= EK with the
    % optional inclusive_end=false option changing that
    % to SK <= key < EK. Also, remember that CouchDB
    % swaps the meaning of SK and EK based on direction.
    %
    % Thus we have this wonderful bit of logic to account
    % for all of those combinations.

    StartKey4 = case {Reverse, InclusiveEnd} of
        {true, false} ->
            erlfdb_key:first_greater_than(StartKey3);
        _ ->
            StartKey3
    end,

    EndKey4 = case {Reverse, InclusiveEnd} of
        {false, true} when EndKey0 /= undefined ->
            erlfdb_key:first_greater_than(EndKey3);
        {true, _} ->
            erlfdb_key:first_greater_than(EndKey3);
        _ ->
            EndKey3
    end,

    {Reverse, StartKey4, EndKey4}.


get_since_seq(Seq) when Seq == <<>>; Seq == <<"0">>; Seq == 0->
    fabric2_util:seq_zero_vs();

get_since_seq(Seq) when Seq == now; Seq == <<"now">> ->
    fabric2_util:seq_max_vs();

get_since_seq(Seq) when is_binary(Seq), size(Seq) == 24 ->
    Seq1 = fabric2_util:from_hex(Seq),
    Seq2 = <<51:8, Seq1/binary>>,
    {SeqVS} = erlfdb_tuple:unpack(Seq2),
    SeqVS;

get_since_seq(List) when is_list(List) ->
    get_since_seq(list_to_binary(List));

get_since_seq(Seq) ->
    erlang:error({invalid_since_seq, Seq}).


get_db_handle() ->
    case get(?PDICT_DB_KEY) of
        undefined ->
            {ok, Db} = application:get_env(fabric, db),
            put(?PDICT_DB_KEY, Db),
            Db;
        Db ->
            Db
    end.


require_transaction(#{tx := {erlfdb_transaction, _}} = _Db) ->
    ok;
require_transaction(#{} = _Db) ->
    erlang:error(transaction_required).


ensure_current(Db) ->
    ensure_current(Db, true).


ensure_current(#{} = Db, CheckDbVersion) ->
    require_transaction(Db),

    #{
        tx := Tx,
        md_version := MetaDataVersion
    } = Db,

    case erlfdb:wait(erlfdb:get(Tx, ?METADATA_VERSION_KEY)) of
        MetaDataVersion -> Db;
        _NewVersion -> reopen(Db)
    end,

    AlreadyChecked = get(?PDICT_CHECKED_DB_IS_CURRENT),
    if not CheckDbVersion orelse AlreadyChecked == true -> Db; true ->
        #{
            db_prefix := DbPrefix,
            db_version := DbVersion
        } = Db,

        DbVersionKey = erlfdb_tuple:pack({?DB_VERSION}, DbPrefix),

        case erlfdb:wait(erlfdb:get(Tx, DbVersionKey)) of
            DbVersion ->
                put(?PDICT_CHECKED_DB_IS_CURRENT, true),
                Db;
            _NewDBVersion ->
                fabric2_server:remove(maps:get(name, Db)),
                reopen(Db)
        end
    end.


is_transaction_applied(Tx) ->
    is_commit_unknown_result()
        andalso has_transaction_id()
        andalso transaction_id_exists(Tx).


get_previous_transaction_result() ->
    get(?PDICT_TX_RES_KEY).


execute_transaction(Tx, Fun, LayerPrefix) ->
    put(?PDICT_CHECKED_DB_IS_CURRENT, false),
    Result = Fun(Tx),
    case erlfdb:is_read_only(Tx) of
        true ->
            ok;
        false ->
            erlfdb:set(Tx, get_transaction_id(Tx, LayerPrefix), <<>>),
            put(?PDICT_TX_RES_KEY, Result)
    end,
    Result.


clear_transaction() ->
    fabric2_txids:remove(get(?PDICT_TX_ID_KEY)),
    erase(?PDICT_CHECKED_DB_IS_CURRENT),
    erase(?PDICT_TX_ID_KEY),
    erase(?PDICT_TX_RES_KEY).


is_commit_unknown_result() ->
    erlfdb:get_last_error() == ?COMMIT_UNKNOWN_RESULT.


has_transaction_id() ->
    is_binary(get(?PDICT_TX_ID_KEY)).


transaction_id_exists(Tx) ->
    erlfdb:wait(erlfdb:get(Tx, get(?PDICT_TX_ID_KEY))) == <<>>.


get_transaction_id(Tx, LayerPrefix) ->
    case get(?PDICT_TX_ID_KEY) of
        undefined ->
            TxId = fabric2_txids:create(Tx, LayerPrefix),
            put(?PDICT_TX_ID_KEY, TxId),
            TxId;
        TxId when is_binary(TxId) ->
            TxId
    end.


new_versionstamp(Tx) ->
    TxId = erlfdb:get_next_tx_id(Tx),
    {versionstamp, 16#FFFFFFFFFFFFFFFF, 16#FFFF, TxId}.

