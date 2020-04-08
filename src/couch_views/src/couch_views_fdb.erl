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

-module(couch_views_fdb).

-export([
    new_interactive_index/3,
    new_creation_vs/3,
    get_creation_vs/2,
    get_build_status/2,
    set_build_status/3,

    get_update_seq/2,
    set_update_seq/3,

    get_row_count/3,
    get_kv_size/3,

    fold_map_idx/6,

    write_doc/4
]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

-define(LIST_VALUE, 0).
-define(JSON_VALUE, 1).
-define(VALUE, 2).


-include("couch_views.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric2.hrl").


new_interactive_index(Db, Mrst, VS) ->
    couch_views_fdb:new_creation_vs(Db, Mrst, VS),
    couch_views_fdb:set_build_status(Db, Mrst, ?INDEX_BUILDING).


%Interactive View Creation Versionstamp
%(<db>, ?DB_VIEWS, ?VIEW_INFO, ?VIEW_CREATION_VS, Sig) = VS

new_creation_vs(TxDb, #mrst{} = Mrst, VS) ->
    #{
        tx := Tx
    } = TxDb,
    Key = creation_vs_key(TxDb, Mrst#mrst.sig),
    Value = erlfdb_tuple:pack_vs({VS}),
    ok = erlfdb:set_versionstamped_value(Tx, Key, Value).


get_creation_vs(TxDb, #mrst{} = Mrst) ->
    get_creation_vs(TxDb, Mrst#mrst.sig);

get_creation_vs(TxDb, Sig) ->
    #{
        tx := Tx
    } = TxDb,
    Key = creation_vs_key(TxDb, Sig),
    case erlfdb:wait(erlfdb:get(Tx, Key)) of
        not_found ->
            not_found;
        EK ->
            {VS} = erlfdb_tuple:unpack(EK),
            VS
    end.


%Interactive View Build Status
%(<db>, ?DB_VIEWS, ?VIEW_INFO, ?VIEW_BUILD_STATUS, Sig) = INDEX_BUILDING | INDEX_READY

get_build_status(TxDb, #mrst{sig = Sig}) ->
    #{
        tx := Tx
    } = TxDb,
    Key = build_status_key(TxDb, Sig),
    erlfdb:wait(erlfdb:get(Tx, Key)).


set_build_status(TxDb, #mrst{sig = Sig}, State) ->
    #{
        tx := Tx
    } = TxDb,

    Key = build_status_key(TxDb, Sig),
    ok = erlfdb:set(Tx, Key, State).


% View Build Sequence Access
% (<db>, ?DB_VIEWS, Sig, ?VIEW_UPDATE_SEQ) = Sequence


get_update_seq(TxDb, #mrst{sig = Sig}) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    case erlfdb:wait(erlfdb:get(Tx, seq_key(DbPrefix, Sig))) of
        not_found -> <<>>;
        UpdateSeq -> UpdateSeq
    end.


set_update_seq(TxDb, Sig, Seq) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    ok = erlfdb:set(Tx, seq_key(DbPrefix, Sig), Seq).


get_row_count(TxDb, #mrst{sig = Sig}, ViewId) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    case erlfdb:wait(erlfdb:get(Tx, row_count_key(DbPrefix, Sig, ViewId))) of
        not_found -> 0; % Can this happen?
        CountBin -> ?bin2uint(CountBin)
    end.


get_kv_size(TxDb, #mrst{sig = Sig}, ViewId) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    case erlfdb:wait(erlfdb:get(Tx, kv_size_key(DbPrefix, Sig, ViewId))) of
        not_found -> 0; % Can this happen?
        SizeBin -> ?bin2uint(SizeBin)
    end.


fold_map_idx(TxDb, Sig, ViewId, Options, Callback, Acc0) ->
    #{
        db_prefix := DbPrefix
    } = TxDb,

    MapIdxPrefix = map_idx_prefix(DbPrefix, Sig, ViewId),
    FoldAcc = #{
        prefix => MapIdxPrefix,
        callback => Callback,
        acc => Acc0
        },
    Fun = aegis:wrap_fold_fun(TxDb, fun fold_fwd/2),

    #{
        acc := Acc1
    } = fabric2_fdb:fold_range(TxDb, MapIdxPrefix, Fun, FoldAcc, Options),

    Acc1.


write_doc(TxDb, Sig, _ViewIds, #{deleted := true} = Doc) ->
    #{
        id := DocId
    } = Doc,

    ExistingViewKeys = get_view_keys(TxDb, Sig, DocId),

    clear_id_idx(TxDb, Sig, DocId),
    lists:foreach(fun({ViewId, TotalKeys, TotalSize, UniqueKeys}) ->
        clear_map_idx(TxDb, Sig, ViewId, DocId, UniqueKeys),
        update_row_count(TxDb, Sig, ViewId, -TotalKeys),
        update_kv_size(TxDb, Sig, ViewId, -TotalSize)
    end, ExistingViewKeys);

write_doc(TxDb, Sig, ViewIds, Doc) ->
    #{
        id := DocId,
        results := Results,
        kv_sizes := KVSizes
    } = Doc,

    ExistingViewKeys = get_view_keys(TxDb, Sig, DocId),

    clear_id_idx(TxDb, Sig, DocId),

    lists:foreach(fun({ViewId, NewRows, KVSize}) ->
        update_id_idx(TxDb, Sig, ViewId, DocId, NewRows, KVSize),

        ExistingKeys = case lists:keyfind(ViewId, 1, ExistingViewKeys) of
            {ViewId, TotalRows, TotalSize, EKeys} ->
                RowChange = length(NewRows) - TotalRows,
                update_row_count(TxDb, Sig, ViewId, RowChange),
                update_kv_size(TxDb, Sig, ViewId, KVSize - TotalSize),
                EKeys;
            false ->
                RowChange = length(NewRows),
                update_row_count(TxDb, Sig, ViewId, RowChange),
                update_kv_size(TxDb, Sig, ViewId, KVSize),
                []
        end,
        update_map_idx(TxDb, Sig, ViewId, DocId, ExistingKeys, NewRows)
    end, lists:zip3(ViewIds, Results, KVSizes)).


% For each row in a map view we store the the key/value
% in FoundationDB:
%
%   `(EncodedSortKey, (EncodedKey, EncodedValue))`
%
% The difference between `EncodedSortKey` and `EndcodedKey` is
% the use of `couch_util:get_sort_key/1` which turns UTF-8
% strings into binaries that are byte comparable. Given a sort
% key binary we cannot recover the input so to return unmodified
% user data we are forced to store the original.

fold_fwd({RowKey, PackedKeyValue}, Acc) ->
    #{
        prefix := Prefix,
        callback := UserCallback,
        acc := UserAcc0
    } = Acc,

    {{_SortKey, DocId}, _DupeId} =
            erlfdb_tuple:unpack(RowKey, Prefix),

    {EncodedOriginalKey, EncodedValue} = erlfdb_tuple:unpack(PackedKeyValue),
    Value = couch_views_encoding:decode(EncodedValue),
    Key = couch_views_encoding:decode(EncodedOriginalKey),

    UserAcc1 = UserCallback(DocId, Key, Value, UserAcc0),

    Acc#{
        acc := UserAcc1
    }.


clear_id_idx(TxDb, Sig, DocId) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    {Start, End} = id_idx_range(DbPrefix, Sig, DocId),
    ok = erlfdb:clear_range(Tx, Start, End).


clear_map_idx(TxDb, Sig, ViewId, DocId, ViewKeys) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    lists:foreach(fun(ViewKey) ->
        {Start, End} = map_idx_range(DbPrefix, Sig, ViewId, ViewKey, DocId),
        ok = erlfdb:clear_range(Tx, Start, End)
    end, ViewKeys).


update_id_idx(TxDb, Sig, ViewId, DocId, [], _KVSize) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    Key = id_idx_key(DbPrefix, Sig, DocId, ViewId),
    ok = erlfdb:clear(Tx, Key);

update_id_idx(TxDb, Sig, ViewId, DocId, NewRows, KVSize) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    Unique = lists:usort([K || {K, _V} <- NewRows]),

    Key = id_idx_key(DbPrefix, Sig, DocId, ViewId),
    Val = couch_views_encoding:encode([length(NewRows), KVSize, Unique]),
    ok = erlfdb:set(Tx, Key, aegis:encrypt(TxDb, Key, Val)).


update_map_idx(TxDb, Sig, ViewId, DocId, ExistingKeys, NewRows) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    lists:foreach(fun(RemKey) ->
        {Start, End} = map_idx_range(DbPrefix, Sig, ViewId, RemKey, DocId),
        ok = erlfdb:clear_range(Tx, Start, End)
    end, ExistingKeys),

    KVsToAdd = process_rows(NewRows),
    MapIdxPrefix = map_idx_prefix(DbPrefix, Sig, ViewId),

    lists:foreach(fun({DupeId, Key1, Key2, EV}) ->
        KK = map_idx_key(MapIdxPrefix, {Key1, DocId}, DupeId),
        Val = erlfdb_tuple:pack({Key2, EV}),
        ok = erlfdb:set(Tx, KK, aegis:encrypt(TxDb, KK, Val))
    end, KVsToAdd).


get_view_keys(TxDb, Sig, DocId) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    {Start, End} = id_idx_range(DbPrefix, Sig, DocId),
    lists:map(fun({K, V}) ->
        {?DB_VIEWS, ?VIEW_DATA, Sig, ?VIEW_ID_RANGE, DocId, ViewId} =
                erlfdb_tuple:unpack(K, DbPrefix),
        [TotalKeys, TotalSize, UniqueKeys] = couch_views_encoding:decode(V),
        {ViewId, TotalKeys, TotalSize, UniqueKeys}
    end, aegis:decrypt(TxDb, erlfdb:get_range(Tx, Start, End, []))).


update_row_count(TxDb, Sig, ViewId, Increment) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    Key = row_count_key(DbPrefix, Sig, ViewId),
    erlfdb:add(Tx, Key, Increment).


update_kv_size(TxDb, Sig, ViewId, Increment) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    % Track a view specific size for calls to
    % GET /dbname/_design/doc/_info`
    IdxKey = kv_size_key(DbPrefix, Sig, ViewId),
    erlfdb:add(Tx, IdxKey, Increment),

    % Track a database level rollup for calls to
    % GET /dbname
    DbKey = db_kv_size_key(DbPrefix),
    erlfdb:add(Tx, DbKey, Increment).


seq_key(DbPrefix, Sig) ->
    Key = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_UPDATE_SEQ, Sig},
    erlfdb_tuple:pack(Key, DbPrefix).


row_count_key(DbPrefix, Sig, ViewId) ->
    Key = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_ROW_COUNT, Sig, ViewId},
    erlfdb_tuple:pack(Key, DbPrefix).


kv_size_key(DbPrefix, Sig, ViewId) ->
    Key = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_KV_SIZE, Sig, ViewId},
    erlfdb_tuple:pack(Key, DbPrefix).


db_kv_size_key(DbPrefix) ->
    Key = {?DB_STATS, <<"sizes">>, <<"views">>},
    erlfdb_tuple:pack(Key, DbPrefix).


id_idx_key(DbPrefix, Sig, DocId, ViewId) ->
    Key = {?DB_VIEWS, ?VIEW_DATA, Sig, ?VIEW_ID_RANGE, DocId, ViewId},
    erlfdb_tuple:pack(Key, DbPrefix).


id_idx_range(DbPrefix, Sig, DocId) ->
    Key = {?DB_VIEWS, ?VIEW_DATA, Sig, ?VIEW_ID_RANGE, DocId},
    erlfdb_tuple:range(Key, DbPrefix).


map_idx_prefix(DbPrefix, Sig, ViewId) ->
    Key = {?DB_VIEWS, ?VIEW_DATA, Sig, ?VIEW_MAP_RANGE, ViewId},
    erlfdb_tuple:pack(Key, DbPrefix).


map_idx_key(MapIdxPrefix, MapKey, DupeId) ->
    Key = {MapKey, DupeId},
    erlfdb_tuple:pack(Key, MapIdxPrefix).


map_idx_range(DbPrefix, Sig, ViewId, MapKey, DocId) ->
    Encoded = couch_views_encoding:encode(MapKey, key),
    Key = {
        ?DB_VIEWS,
        ?VIEW_DATA,
        Sig,
        ?VIEW_MAP_RANGE,
        ViewId,
        {Encoded, DocId}
    },
    erlfdb_tuple:range(Key, DbPrefix).


creation_vs_key(Db, Sig) ->
    #{
        db_prefix := DbPrefix
    } = Db,
    Key = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_CREATION_VS, Sig},
    erlfdb_tuple:pack(Key, DbPrefix).


build_status_key(Db, Sig) ->
    #{
        db_prefix := DbPrefix
    } = Db,
    Key = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_BUILD_STATUS, Sig},
    erlfdb_tuple:pack(Key, DbPrefix).


process_rows(Rows) ->
    Encoded = lists:map(fun({K, V}) ->
        EK1 = couch_views_encoding:encode(K, key),
        EK2 = couch_views_encoding:encode(K, value),
        EV = couch_views_encoding:encode(V, value),
        {EK1, EK2, EV}
    end, Rows),

    Grouped = lists:foldl(fun({K1, K2, V}, Acc) ->
        dict:append(K1, {K2, V}, Acc)
    end, dict:new(), Encoded),

    dict:fold(fun(K1, Vals, DAcc) ->
        Vals1 = lists:keysort(2, Vals),
        {_, Labeled} = lists:foldl(fun({K2, V}, {Count, Acc}) ->
            {Count + 1, [{Count, K1, K2, V} | Acc]}
        end, {0, []}, Vals1),
        Labeled ++ DAcc
    end, [], Grouped).
