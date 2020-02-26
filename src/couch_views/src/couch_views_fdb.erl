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
-define(MAX_KEY_SIZE_LIMIT, 8000).
-define(MAX_VALUE_SIZE_LIMIT, 64000).


-include("couch_views.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric2.hrl").


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
    Fun = fun fold_fwd/2,

    #{
        acc := Acc1
    } = fabric2_fdb:fold_range(TxDb, MapIdxPrefix, Fun, FoldAcc, Options),

    Acc1.


write_doc(TxDb, Sig, _Views, #{deleted := true} = Doc) ->
    #{
        id := DocId
    } = Doc,

    ExistingViewKeys = get_view_keys(TxDb, Sig, DocId),

    clear_id_idx(TxDb, Sig, DocId),
    lists:foreach(fun(ExistingViewKey) ->
        remove_doc_from_idx(TxDb, Sig, DocId, ExistingViewKey)
    end, ExistingViewKeys);

write_doc(TxDb, Sig, Views, Doc) ->
    #{
        id := DocId,
        results := Results
    } = Doc,

    ExistingViewKeys = get_view_keys(TxDb, Sig, DocId),

    clear_id_idx(TxDb, Sig, DocId),
    lists:foreach(fun({View, NewRows}) ->
        #mrview{
            map_names = MNames,
            id_num = ViewId
        } = View,

        try
            NewRowSize = calculate_row_size(NewRows),
            update_id_idx(TxDb, Sig, ViewId, DocId, NewRows),

            ExistingKeys = case lists:keyfind(ViewId, 1, ExistingViewKeys) of
                {ViewId, TotalRows, TotalSize, EKeys} ->
                    RowChange = length(NewRows) - TotalRows,
                    SizeChange = NewRowSize - TotalSize,
                    update_row_count(TxDb, Sig, ViewId, RowChange),
                    update_kv_size(TxDb, Sig, ViewId, SizeChange),
                    EKeys;
                false ->
                    RowChange = length(NewRows),
                    SizeChange = NewRowSize,
                    update_row_count(TxDb, Sig, ViewId, RowChange),
                    update_kv_size(TxDb, Sig, ViewId, SizeChange),
                    []
            end,
            update_map_idx(TxDb, Sig, ViewId, DocId, ExistingKeys, NewRows)
        catch
            throw:{size_exceeded, Type}  ->
                case lists:keyfind(ViewId, 1, ExistingViewKeys) of
                    false ->
                        ok;
                    ExistingViewKey ->
                        remove_doc_from_idx(TxDb, Sig, DocId, ExistingViewKey)
                end,
                #{
                    name := DbName
                } = TxDb,
                couch_log:error("Db `~s` Doc `~s` exceeded the ~s size "
                    "for view `~s` and was not indexed.",
                    [DbName, DocId, Type, MNames])
        end
    end, lists:zip(Views, Results)).


remove_doc_from_idx(TxDb, Sig, DocId, {ViewId, TotalKeys, TotalSize,
    UniqueKeys}) ->
    clear_map_idx(TxDb, Sig, ViewId, DocId, UniqueKeys),
    update_row_count(TxDb, Sig, ViewId, -TotalKeys),
    update_kv_size(TxDb, Sig, ViewId, -TotalSize).


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


update_id_idx(TxDb, Sig, ViewId, DocId, NewRows) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    Unique = lists:usort([K || {K, _V} <- NewRows]),

    Key = id_idx_key(DbPrefix, Sig, DocId, ViewId),
    RowSize = calculate_row_size(NewRows),
    Val = couch_views_encoding:encode([length(NewRows), RowSize, Unique]),
    ok = erlfdb:set(Tx, Key, Val).


update_map_idx(TxDb, Sig, ViewId, DocId, ExistingKeys, NewRows) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    Unique = lists:usort([K || {K, _V} <- NewRows]),

    KeysToRem = ExistingKeys -- Unique,
    lists:foreach(fun(RemKey) ->
        {Start, End} = map_idx_range(DbPrefix, Sig, ViewId, RemKey, DocId),
        ok = erlfdb:clear_range(Tx, Start, End)
    end, KeysToRem),

    KVsToAdd = process_rows(NewRows),
    MapIdxPrefix = map_idx_prefix(DbPrefix, Sig, ViewId),

    lists:foreach(fun({DupeId, Key1, Key2, EV}) ->
        KK = map_idx_key(MapIdxPrefix, {Key1, DocId}, DupeId),
        Val = erlfdb_tuple:pack({Key2, EV}),
        ok = erlfdb:set(Tx, KK, Val)
    end, KVsToAdd).


get_view_keys(TxDb, Sig, DocId) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    {Start, End} = id_idx_range(DbPrefix, Sig, DocId),
    lists:map(fun({K, V}) ->
        {?DB_VIEWS, Sig, ?VIEW_ID_RANGE, DocId, ViewId} =
                erlfdb_tuple:unpack(K, DbPrefix),
        [TotalKeys, TotalSize, UniqueKeys] = couch_views_encoding:decode(V),
        {ViewId, TotalKeys, TotalSize, UniqueKeys}
    end, erlfdb:get_range(Tx, Start, End, [])).


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
    Key = {?DB_VIEWS, Sig, ?VIEW_UPDATE_SEQ},
    erlfdb_tuple:pack(Key, DbPrefix).


row_count_key(DbPrefix, Sig, ViewId) ->
    Key = {?DB_VIEWS, Sig, ?VIEW_ID_INFO, ViewId, ?VIEW_ROW_COUNT},
    erlfdb_tuple:pack(Key, DbPrefix).


kv_size_key(DbPrefix, Sig, ViewId) ->
    Key = {?DB_VIEWS, Sig, ?VIEW_ID_INFO, ViewId, ?VIEW_KV_SIZE},
    erlfdb_tuple:pack(Key, DbPrefix).


db_kv_size_key(DbPrefix) ->
    Key = {?DB_STATS, <<"sizes">>, <<"views">>},
    erlfdb_tuple:pack(Key, DbPrefix).


id_idx_key(DbPrefix, Sig, DocId, ViewId) ->
    Key = {?DB_VIEWS, Sig, ?VIEW_ID_RANGE, DocId, ViewId},
    erlfdb_tuple:pack(Key, DbPrefix).


id_idx_range(DbPrefix, Sig, DocId) ->
    Key = {?DB_VIEWS, Sig, ?VIEW_ID_RANGE, DocId},
    erlfdb_tuple:range(Key, DbPrefix).


map_idx_prefix(DbPrefix, Sig, ViewId) ->
    Key = {?DB_VIEWS, Sig, ?VIEW_MAP_RANGE, ViewId},
    erlfdb_tuple:pack(Key, DbPrefix).


map_idx_key(MapIdxPrefix, MapKey, DupeId) ->
    Key = {MapKey, DupeId},
    erlfdb_tuple:pack(Key, MapIdxPrefix).


map_idx_range(DbPrefix, Sig, ViewId, MapKey, DocId) ->
    Encoded = couch_views_encoding:encode(MapKey, key),
    Key = {?DB_VIEWS, Sig, ?VIEW_MAP_RANGE, ViewId, {Encoded, DocId}},
    erlfdb_tuple:range(Key, DbPrefix).


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


calculate_row_size(Rows) ->
    KeyLimit = key_size_limit(),
    ValLimit = value_size_limit(),

    lists:foldl(fun({K, V}, Acc) ->
        KeySize = erlang:external_size(K),
        ValSize = erlang:external_size(V),

        if KeySize =< KeyLimit -> ok; true ->
            throw({size_exceeded, key})
        end,

        if ValSize =< ValLimit -> ok; true ->
            throw({size_exceeded, value})
        end,

        Acc + KeySize + ValSize
    end, 0, Rows).


key_size_limit() ->
    config:get_integer("couch_views", "key_size_limit", ?MAX_KEY_SIZE_LIMIT).


value_size_limit() ->
    config:get_integer("couch_views", "value_size_limit", ?MAX_VALUE_SIZE_LIMIT).
