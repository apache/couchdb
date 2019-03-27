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
    init/3,

    create/2,
    open/2,
    delete/1,
    exists/1,
    is_current/1,

    list_dbs/2,

    get_info/1,
    get_config/1,
    set_config/3,

    get_stat/2,
    incr_stat/3,

    get_full_doc_info/2,
    get_doc_body/3,

    store_doc/3,

    add_to_all_docs/3,
    rem_from_all_docs/2,

    fold_docs/4,
    fold_changes/5,

    get_changes/2
]).


-include_lib("couch/include/couch_db.hrl").
-include("fabric2.hrl").


% This will eventually be the `\xFFmetadataVersion` key that is
% currently only available in FoundationDB master.
%
%  https://forums.foundationdb.org/t/a-new-tool-for-managing-layer-metadata/1191
%
% Until then we'll fake the same behavior using a randomish
% key for tracking metadata changse. Once we get to the
% new feature this will be more performant by updating
% this define.
-define(METADATA_VERSION_KEY, <<"$metadata_version_key$">>).


% Prefix Definitions

-define(CLUSTER_CONFIG, 0).
-define(ALL_DBS, 1).
-define(DBS, 15).

-define(DB_CONFIG, 16).
-define(DB_STATS, 17).
-define(DB_ALL_DOCS, 18).
-define(DB_CHANGES, 19).
-define(DB_REVS, 20).
-define(DB_DOCS, 21).
-define(DB_LOCAL_DOCS, 22).


% Various utility macros

-define(REQUIRE_TX(Db), {erlfdb_transaction, _} = maps:get(tx, Db)).
-define(REQUIRE_CURRENT(Db), true = is_current(Db)).

-define(UNSET_VS, {versionstamp, 16#FFFFFFFFFFFFFFFF, 16#FFFF}).


init(Tx, DbName, Options) ->
    Root = erlfdb_directory:root(),
    CouchDB = erlfdb_directory:create_or_open(Tx, Root, [<<"couchdb">>]),
    Prefix = erlfdb_directory:get_name(CouchDB),
    #{
        name => DbName,
        tx => Tx,
        layer_prefix => Prefix,
        options => Options
    }.


create(#{} = Db, Options) ->
    ?REQUIRE_TX(Db),
    #{
        name := DbName,
        tx := Tx,
        layer_prefix := LayerPrefix
    } = Db,

    % Eventually DbPrefix will be HCA allocated. For now
    % we're just using the DbName so that debugging is easier.
    DbKey = erlfdb_tuple:pack({?ALL_DBS, DbName}, LayerPrefix),
    DbPrefix = erlfdb_tuple:pack({?DBS, DbName}, LayerPrefix),
    erlfdb:set(Tx, DbKey, DbPrefix),

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
    Version = erlfdb:wait(erlfdb:get(Tx, ?METADATA_VERSION_KEY)),

    Db#{
        uuid => UUID,
        db_prefix => DbPrefix,
        md_version => Version,
        revs_limit => 1000,
        security_doc => {[]},
        validate_doc_update_funs => [],

        user_ctx => UserCtx,

        before_doc_update => undefined,
        after_doc_update => undefined
        % All other db things as we add features
    }.


open(#{} = Db0, Options) ->
    ?REQUIRE_TX(Db0),
    #{
        name := DbName,
        tx := Tx,
        layer_prefix := LayerPrefix
    } = Db0,

    DbKey = erlfdb_tuple:pack({?ALL_DBS, DbName}, LayerPrefix),
    DbPrefix = case erlfdb:wait(erlfdb:get(Tx, DbKey)) of
        Bin when is_binary(Bin) -> Bin;
        not_found -> erlang:error(database_does_not_exist)
    end,

    UserCtx = fabric2_util:get_value(user_ctx, Options, #user_ctx{}),
    Version = erlfdb:wait(erlfdb:get(Tx, ?METADATA_VERSION_KEY)),

    Db1 = Db0#{
        db_prefix => DbPrefix,
        md_version => Version,

        user_ctx => UserCtx,

        % Place holders until we implement these
        % bits.
        validate_doc_upate_funs => [],
        before_doc_update => undefined,
        after_doc_read => undefined
    },

    lists:foldl(fun({Key, Val}, DbAcc) ->
        case Key of
            <<"uuid">> ->
                DbAcc#{uuid => Val};
            <<"revs_limit">> ->
                DbAcc#{revs_limit => ?bin2uint(Val)};
            <<"security_doc">> ->
                DbAcc#{security_doc => ?JSON_DECODE(Val)}
        end
    end, Db1, get_config(Db1)).


delete(#{} = Db) ->
    ?REQUIRE_CURRENT(Db),
    #{
        name := DbName,
        tx := Tx,
        layer_prefix := LayerPrefix,
        db_prefix := DbPrefix
    } = Db,

    DbKey = erlfdb_tuple:pack({?ALL_DBS, DbName}, LayerPrefix),
    erlfdb:clear(Tx, DbKey),
    erlfdb:clear_range_startswith(Tx, DbPrefix),
    bump_metadata_version(Tx),
    ok.


exists(#{name := DbName} = Db) when is_binary(DbName) ->
    ?REQUIRE_TX(Db),
    #{
        tx := Tx,
        layer_prefix := LayerPrefix
    } = Db,

    DbKey = erlfdb_tuple:pack({?ALL_DBS, DbName}, LayerPrefix),
    case erlfdb:wait(erlfdb:get(Tx, DbKey)) of
        Bin when is_binary(Bin) -> true;
        not_found -> false
    end.


is_current(#{} = Db) ->
    ?REQUIRE_TX(Db),
    #{
        tx := Tx,
        md_version := MetaDataVersion
    } = Db,

    case erlfdb:wait(erlfdb:get(Tx, ?METADATA_VERSION_KEY)) of
        MetaDataVersion -> true;
        _NewVersion -> false
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
    ?REQUIRE_CURRENT(Db),
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

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
            <<0:80>>;
        [{SeqKey, _}] ->
            {?DB_CHANGES, SeqBin} = erlfdb_tuple:unpack(SeqKey, DbPrefix),
            SeqBin
    end,
    CProp = {update_seq, fabric2_util:to_hex(RawSeq)},

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
    ?REQUIRE_CURRENT(Db),
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    {Start, End} = erlfdb_tuple:range({?DB_CONFIG}, DbPrefix),
    Future = erlfdb:get_range(Tx, Start, End),

    lists:map(fun({K, V}) ->
        {?DB_CONFIG, Key} = erlfdb_tuple:unpack(K, DbPrefix),
        {Key, V}
    end, erlfdb:wait(Future)).


set_config(#{} = Db, ConfigKey, ConfigVal) ->
    ?REQUIRE_CURRENT(Db),
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    Key = erlfdb_tuple:pack({?DB_CONFIG, ConfigKey}, DbPrefix),
    erlfdb:set(Tx, Key, ConfigVal),
    bump_metadata_version(Tx).


get_stat(#{} = Db, StatKey) ->
    ?REQUIRE_CURRENT(Db),
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    Key = erlfdb_tuple:pack({?DB_STATS, StatKey}, DbPrefix),

    % Might need to figure out some sort of type
    % system here. Uints are because stats are all
    % atomic op adds for the moment.
    ?bin2uint(erlfdb:wait(erlfdb:get(Tx, Key))).


incr_stat(_Db, _StatKey, 0) ->
    ok;

incr_stat(#{} = Db, StatKey, Increment) when is_integer(Increment) ->
    ?REQUIRE_CURRENT(Db),
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    Key = erlfdb_tuple:pack({?DB_STATS, StatKey}, DbPrefix),
    erlfdb:add(Tx, Key, Increment).


get_full_doc_info(#{} = Db, DocId) ->
    ?REQUIRE_CURRENT(Db),
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    Key = erlfdb_tuple:pack({?DB_DOCS, DocId}, DbPrefix),
    Val = erlfdb:wait(erlfdb:get(Tx, Key)),
    fdb_to_fdi(Db, DocId, Val).


get_doc_body(#{} = Db, DocId, {Pos, [Rev | _] = Path}) ->
    ?REQUIRE_CURRENT(Db),
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    Key = erlfdb_tuple:pack({?DB_REVS, DocId, Pos, Rev}, DbPrefix),
    Val = erlfdb:wait(erlfdb:get(Tx, Key)),
    fdb_to_doc(Db, DocId, Pos, Path, Val).


store_doc(#{} = Db, #full_doc_info{} = FDI, #doc{} = Doc) ->
    ?REQUIRE_CURRENT(Db),
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    #full_doc_info{
        id = DocId,
        update_seq = OldUpdateSeq
    } = FDI,

    #doc{
        revs = {Pos, [Rev | _]},
        deleted = Deleted
    } = Doc,

    % Delete old entry in changes feed
    OldSeqKey = erlfdb_tuple:pack({?DB_CHANGES, OldUpdateSeq}, DbPrefix),
    erlfdb:clear(Tx, OldSeqKey),

    % Add new entry to changes feed
    NewSeqKey = erlfdb_tuple:pack_vs({?DB_CHANGES, ?UNSET_VS}, DbPrefix),
    NewSeqVal = erlfdb_tuple:pack({DocId, Deleted, {Pos, Rev}}),
    erlfdb:set_versionstamped_key(Tx, NewSeqKey, NewSeqVal),

    % Write document data
    {NewDocKey, NewDocVal} = doc_to_fdb(Db, Doc),
    erlfdb:set(Tx, NewDocKey, NewDocVal),

    % Update revision tree entry
    {NewFDIKey, NewFDIVal} = fdi_to_fdb(Db, FDI),
    erlfdb:set_versionstamped_value(Tx, NewFDIKey, NewFDIVal).


add_to_all_docs(#{} = Db, DocId, {Pos, Rev}) ->
    ?REQUIRE_CURRENT(Db),
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    Key = erlfdb_tuple:pack({?DB_ALL_DOCS, DocId}, DbPrefix),
    Val = erlfdb_tuple:pack({Pos, Rev}),
    erlfdb:set(Tx, Key, Val).


rem_from_all_docs(#{} = Db, DocId) ->
    ?REQUIRE_CURRENT(Db),
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    Key = erlfdb_tuple:pack({?DB_ALL_DOCS, DocId}, DbPrefix),
    ok = erlfdb:clear(Tx, Key).


fold_docs(#{} = Db, UserFun, UserAcc0, _Options) ->
    ?REQUIRE_CURRENT(Db),
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    DocCountKey = erlfdb_tuple:pack({?DB_STATS, <<"doc_count">>}, DbPrefix),
    DocCountFuture = erlfdb:get(Tx, DocCountKey),

    {Start, End} = erlfdb_tuple:range({?DB_ALL_DOCS}, DbPrefix),
    RangeFuture = erlfdb:get_range(Tx, Start, End),

    DocCount = ?bin2uint(erlfdb:wait(DocCountFuture)),

    {ok, UserAcc1} = UserFun({meta, [
        {total, DocCount},
        {offset, null}
    ]}, UserAcc0),

    UserAcc2 = lists:foldl(fun({K, V}, Acc) ->
        {?DB_ALL_DOCS, DocId} = erlfdb_tuple:unpack(K, DbPrefix),
        RevId = erlfdb_tuple:unpack(V),

        {ok, NewAcc} = UserFun({row, [
            {id, DocId},
            {key, DocId},
            {value, couch_doc:rev_to_str(RevId)}
        ]}, Acc),

        NewAcc
    end, UserAcc1, erlfdb:wait(RangeFuture)),

    UserFun(complete, UserAcc2).


fold_changes(#{} = Db, SinceSeq, UserFun, UserAcc0, _Options) ->
    ?REQUIRE_CURRENT(Db),
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    {Start0, End} = erlfdb_tuple:range({?DB_CHANGES}, DbPrefix),
    Start = erlang:max(Start0, SinceSeq),
    Future = erlfdb:get_range(Tx, Start, End),

    {ok, UserAcc1} = UserFun(start, UserAcc0),

    UserAcc2 = lists:foldl(fun({K, V}, Acc) ->
        {?DB_CHANGES, UpdateSeq} = erlfdb_tuple:unpack(K, DbPrefix),
        {DocId, Deleted, RevId} = erlfdb_tuple:unpack(V),

        UpdateSeqEJson = fabric2_util:to_hex(erlfdb_tuple:pack({UpdateSeq})),

        DelMember = if not Deleted -> []; true ->
            [{deleted, true}]
        end,

        {ok, NewAcc} = UserFun({change, {[
            {seq, UpdateSeqEJson},
            {id, DocId},
            {changes, [{[{rev, couch_doc:rev_to_str(RevId)}]}]}
        ] ++ DelMember}}, Acc),

        NewAcc
    end, UserAcc1, erlfdb:wait(Future)),

    {LastKey, _} = lists:last(erlfdb:wait(Future)),
    {?DB_CHANGES, LastSeq} = erlfdb_tuple:unpack(LastKey, DbPrefix),
    LastSeqEJson = fabric2_util:to_hex(erlfdb_tuple:pack({LastSeq})),

    UserFun({stop, LastSeqEJson, 0}, UserAcc2).


get_changes(#{} = Db, Options) ->
    ?REQUIRE_CURRENT(Db),
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    {CStart, CEnd} = erlfdb_tuple:range({?DB_CHANGES}, DbPrefix),
    Future = erlfdb:get_range(Tx, CStart, CEnd, Options),
    lists:map(fun({Key, Val}) ->
        {?DB_CHANGES, Seq} = erlfdb_tuple:unpack(Key, DbPrefix),
        {fabric2_util:to_hex(Seq), Val}
    end, erlfdb:wait(Future)).


bump_metadata_version(Tx) ->
    % The 14 zero bytes is pulled from the PR for adding the
    % metadata version key. Not sure why 14 bytes when version
    % stamps are only 80, but whatever for now.
    erlfdb:set_versionstamped_value(Tx, ?METADATA_VERSION_KEY, <<0:112>>).


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
    } = Doc,

    Key = erlfdb_tuple:pack({?DB_REVS, Id, Start, Rev}, DbPrefix),
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


fdi_to_fdb(Db, #full_doc_info{} = FDI) ->
    #{
        db_prefix := DbPrefix
    } = Db,

    #full_doc_info{
        id = Id,
        deleted = Deleted,
        rev_tree = RevTree
    } = flush_tree(FDI),

    Key = erlfdb_tuple:pack({?DB_DOCS, Id}, DbPrefix),
    RevTreeBin = term_to_binary(RevTree, [{minor_version, 1}]),
    ValTuple = {
        Deleted,
        RevTreeBin,
        ?UNSET_VS
    },
    Val = erlfdb_tuple:pack_vs(ValTuple),
    {Key, Val}.


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


fdb_to_fdi(_Db, Id, Bin) when is_binary(Bin) ->
    {Deleted, RevTreeBin, {versionstamp, V, B}} = erlfdb_tuple:unpack(Bin),
    RevTree = binary_to_term(RevTreeBin, [safe]),
    UpdateSeq = <<V:64/big, B:16/big>>,
    #full_doc_info{
        id = Id,
        deleted = Deleted,
        rev_tree = RevTree,
        update_seq = UpdateSeq
    };
fdb_to_fdi(_Db, _Id, not_found) ->
    not_found.
