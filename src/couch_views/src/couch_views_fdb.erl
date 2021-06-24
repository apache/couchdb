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
    get_view_state/2,

    new_interactive_index/3,
    new_creation_vs/3,
    get_creation_vs/2,
    get_build_status/2,
    set_build_status/3,

    get_update_seq/2,
    set_update_seq/3,

    list_signatures/1,
    clear_index/2,

    persist_chunks/3,
    update_kv_size/4
]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

-include("couch_views.hrl").
-include_lib("fabric/include/fabric2.hrl").

get_view_state(Db, #mrst{} = Mrst) ->
    get_view_state(Db, Mrst#mrst.sig);
get_view_state(Db, Sig) when is_binary(Sig) ->
    #{
        tx := Tx
    } = Db,

    VersionF = erlfdb:get(Tx, version_key(Db, Sig)),
    ViewSeqF = erlfdb:get(Tx, seq_key(Db, Sig)),
    ViewVSF = erlfdb:get(Tx, creation_vs_key(Db, Sig)),
    BuildStatusF = erlfdb:get(Tx, build_status_key(Db, Sig)),

    Version =
        case erlfdb:wait(VersionF) of
            not_found -> not_found;
            VsnVal -> element(1, erlfdb_tuple:unpack(VsnVal))
        end,

    ViewSeq =
        case erlfdb:wait(ViewSeqF) of
            not_found -> <<>>;
            SeqVal -> SeqVal
        end,

    ViewVS =
        case erlfdb:wait(ViewVSF) of
            not_found -> not_found;
            VSVal -> element(1, erlfdb_tuple:unpack(VSVal))
        end,

    State = #{
        version => Version,
        view_seq => ViewSeq,
        view_vs => ViewVS,
        build_status => erlfdb:wait(BuildStatusF)
    },

    maybe_upgrade_view(Db, Sig, State).

new_interactive_index(Db, #mrst{} = Mrst, VS) ->
    new_interactive_index(Db, Mrst#mrst.sig, VS);
new_interactive_index(Db, Sig, VS) ->
    set_version(Db, Sig),
    new_creation_vs(Db, Sig, VS),
    set_build_status(Db, Sig, ?INDEX_BUILDING).

%Interactive View Creation Versionstamp
%(<db>, ?DB_VIEWS, ?VIEW_INFO, ?VIEW_CREATION_VS, Sig) = VS

new_creation_vs(TxDb, #mrst{} = Mrst, VS) ->
    new_creation_vs(TxDb, Mrst#mrst.sig, VS);
new_creation_vs(TxDb, Sig, VS) ->
    #{
        tx := Tx
    } = TxDb,
    Key = creation_vs_key(TxDb, Sig),
    Value = erlfdb_tuple:pack_vs({VS}),
    ok = erlfdb:set_versionstamped_value(Tx, Key, Value).

get_creation_vs(TxDb, MrstOrSig) ->
    #{
        view_vs := ViewVS
    } = get_view_state(TxDb, MrstOrSig),
    ViewVS.

%Interactive View Build Status
%(<db>, ?DB_VIEWS, ?VIEW_INFO, ?VIEW_BUILD_STATUS, Sig) = INDEX_BUILDING | INDEX_READY

get_build_status(TxDb, MrstOrSig) ->
    #{
        build_status := BuildStatus
    } = get_view_state(TxDb, MrstOrSig),
    BuildStatus.

set_build_status(TxDb, #mrst{} = Mrst, State) ->
    set_build_status(TxDb, Mrst#mrst.sig, State);
set_build_status(TxDb, Sig, State) ->
    #{
        tx := Tx
    } = TxDb,

    Key = build_status_key(TxDb, Sig),
    ok = erlfdb:set(Tx, Key, State).

% View Build Sequence Access
% (<db>, ?DB_VIEWS, Sig, ?VIEW_UPDATE_SEQ) = Sequence

get_update_seq(TxDb, MrstOrSig) ->
    #{
        view_seq := ViewSeq
    } = get_view_state(TxDb, MrstOrSig),
    ViewSeq.

set_update_seq(TxDb, Sig, Seq) ->
    #{
        tx := Tx
    } = TxDb,
    ok = erlfdb:set(Tx, seq_key(TxDb, Sig), Seq).

list_signatures(Db) ->
    #{
        db_prefix := DbPrefix
    } = Db,
    ViewSeqRange = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_UPDATE_SEQ},
    RangePrefix = erlfdb_tuple:pack(ViewSeqRange, DbPrefix),
    fabric2_fdb:fold_range(
        Db,
        RangePrefix,
        fun({Key, _Val}, Acc) ->
            {Sig} = erlfdb_tuple:unpack(Key, RangePrefix),
            [Sig | Acc]
        end,
        [],
        []
    ).

clear_index(Db, Signature) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    % Get view size to remove from global counter
    SizeTuple = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_KV_SIZE, Signature},
    SizeKey = erlfdb_tuple:pack(SizeTuple, DbPrefix),
    ViewSize =
        case erlfdb:wait(erlfdb:get(Tx, SizeKey)) of
            not_found -> 0;
            SizeVal -> ?bin2uint(SizeVal)
        end,

    % Clear index info keys
    Keys = [
        {?DB_VIEWS, ?VIEW_INFO, ?VIEW_UPDATE_SEQ, Signature},
        {?DB_VIEWS, ?VIEW_INFO, ?VIEW_ROW_COUNT, Signature},
        {?DB_VIEWS, ?VIEW_INFO, ?VIEW_KV_SIZE, Signature}
    ],
    lists:foreach(
        fun(Key) ->
            FDBKey = erlfdb_tuple:pack(Key, DbPrefix),
            erlfdb:clear(Tx, FDBKey)
        end,
        Keys
    ),

    % Clear index data
    DataTuple = {?DB_VIEWS, ?VIEW_DATA, Signature},
    DataPrefix = erlfdb_tuple:pack(DataTuple, DbPrefix),
    erlfdb:clear_range_startswith(Tx, DataPrefix),

    % Clear tree data
    TreeTuple = {?DB_VIEWS, ?VIEW_TREES, Signature},
    TreePrefix = erlfdb_tuple:pack(TreeTuple, DbPrefix),
    erlfdb:clear_range_startswith(Tx, TreePrefix),

    % Decrement db wide view size counter
    DbSizeTuple = {?DB_STATS, <<"sizes">>, <<"views">>},
    DbSizeKey = erlfdb_tuple:pack(DbSizeTuple, DbPrefix),
    erlfdb:add(Tx, DbSizeKey, -ViewSize).

persist_chunks(Tx, set, [Key, Value]) ->
    Chunks = fabric2_fdb:chunkify_binary(Value),
    LastId = lists:foldl(
        fun(Chunk, Id) ->
            ChunkKey = erlfdb_tuple:pack({Id}, Key),
            erlfdb:set(Tx, ChunkKey, Chunk),
            Id + 1
        end,
        0,
        Chunks
    ),

    % We update nodes in place, so its possible that
    % a node shrank. This clears any keys that we haven't
    % just overwritten for the provided key.
    LastIdKey = erlfdb_tuple:pack({LastId}, Key),
    EndRange = <<Key/binary, 16#FF>>,
    erlfdb:clear_range(Tx, LastIdKey, EndRange);
persist_chunks(Tx, get, Key) ->
    Rows = erlfdb:get_range_startswith(Tx, Key),
    Values = [V || {_K, V} <- Rows],
    iolist_to_binary(Values);
persist_chunks(Tx, clear, Key) ->
    erlfdb:clear_range_startswith(Tx, Key).

update_kv_size(TxDb, Sig, OldSize, NewSize) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    ViewTuple = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_KV_SIZE, Sig},
    ViewKey = erlfdb_tuple:pack(ViewTuple, DbPrefix),
    erlfdb:set(Tx, ViewKey, ?uint2bin(NewSize)),

    DbTuple = {?DB_STATS, <<"sizes">>, <<"views">>},
    DbKey = erlfdb_tuple:pack(DbTuple, DbPrefix),
    erlfdb:add(Tx, DbKey, NewSize - OldSize).

maybe_upgrade_view(_Db, _Sig, #{version := ?CURRENT_VIEW_IMPL_VERSION} = St) ->
    St;
maybe_upgrade_view(Db, Sig, #{version := not_found, view_seq := <<>>} = St) ->
    % If we haven't started building the view yet
    % then we don't change view_vs and build_status
    % as they're still correct.
    set_version(Db, Sig),
    St#{
        version => ?CURRENT_VIEW_IMPL_VERSION,
        view_seq => <<>>
    };
maybe_upgrade_view(Db, Sig, #{version := not_found} = St) ->
    clear_index(Db, Sig),
    set_version(Db, Sig),
    {ViewVS, BuildStatus} = reset_interactive_index(Db, Sig, St),
    #{
        version => ?CURRENT_VIEW_IMPL_VERSION,
        view_seq => <<>>,
        view_vs => ViewVS,
        build_status => BuildStatus
    }.

set_version(Db, Sig) ->
    #{
        tx := Tx
    } = Db,
    Key = version_key(Db, Sig),
    Val = erlfdb_tuple:pack({?CURRENT_VIEW_IMPL_VERSION}),
    erlfdb:set(Tx, Key, Val).

reset_interactive_index(_Db, _Sig, #{view_vs := not_found}) ->
    % Not an interactive index
    {not_found, not_found};
reset_interactive_index(Db, Sig, _St) ->
    % We have to reset the creation versionstamp
    % to the current update seq of the database
    % or else we'll not have indexed any documents
    % inserted since the creation of the interactive
    % index.
    #{
        tx := Tx
    } = Db,

    DbSeq = fabric2_db:get_update_seq(Db),
    VS = fabric2_fdb:seq_to_vs(DbSeq),
    Key = creation_vs_key(Db, Sig),
    Val = erlfdb_tuple:pack({VS}),
    ok = erlfdb:set(Tx, Key, Val),

    set_build_status(Db, Sig, ?INDEX_BUILDING),

    {VS, ?INDEX_BUILDING}.

version_key(Db, Sig) ->
    #{
        db_prefix := DbPrefix
    } = Db,
    Key = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_IMPL_VERSION, Sig},
    erlfdb_tuple:pack(Key, DbPrefix).

seq_key(Db, Sig) ->
    #{
        db_prefix := DbPrefix
    } = Db,
    Key = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_UPDATE_SEQ, Sig},
    erlfdb_tuple:pack(Key, DbPrefix).

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
