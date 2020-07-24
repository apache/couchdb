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

    list_signatures/1,
    clear_index/2,

    persist_chunks/3
]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.


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


list_signatures(Db) ->
    #{
        db_prefix := DbPrefix
    } = Db,
    ViewSeqRange = {?DB_VIEWS, ?VIEW_INFO, ?VIEW_UPDATE_SEQ},
    RangePrefix = erlfdb_tuple:pack(ViewSeqRange, DbPrefix),
    fabric2_fdb:fold_range(Db, RangePrefix, fun({Key, _Val}, Acc) ->
        {Sig} = erlfdb_tuple:unpack(Key, RangePrefix),
        [Sig | Acc]
    end, [], []).


clear_index(Db, Signature) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = Db,

    % Clear index info keys
    Keys = [
        {?DB_VIEWS, ?VIEW_INFO, ?VIEW_UPDATE_SEQ, Signature},
        {?DB_VIEWS, ?VIEW_INFO, ?VIEW_ROW_COUNT, Signature},
        {?DB_VIEWS, ?VIEW_INFO, ?VIEW_KV_SIZE, Signature}
    ],
    lists:foreach(fun(Key) ->
        FDBKey = erlfdb_tuple:pack(Key, DbPrefix),
        erlfdb:clear(Tx, FDBKey)
    end, Keys),

    % Clear index data
    DataTuple = {?DB_VIEWS, ?VIEW_DATA, Signature},
    DataPrefix = erlfdb_tuple:pack(DataTuple, DbPrefix),
    erlfdb:clear_range_startswith(Tx, DataPrefix),

    % Clear tree data
    TreeTuple = {?DB_VIEWS, ?VIEW_TREES, Signature},
    TreePrefix = erlfdb_tuple:pack(TreeTuple, DbPrefix),
    erlfdb:clear_range_startswith(Tx, TreePrefix).


persist_chunks(Tx, set, [Key, Value]) ->
    Chunks = fabric2_fdb:chunkify_binary(Value),
    LastId = lists:foldl(fun(Chunk, Id) ->
        ChunkKey = erlfdb_tuple:pack({Id}, Key),
        erlfdb:set(Tx, ChunkKey, Chunk),
        Id + 1
    end, 0, Chunks),

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


seq_key(DbPrefix, Sig) ->
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
