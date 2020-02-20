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


-module(mango_fdb).


-include_lib("fabric/include/fabric2.hrl").
-include("mango.hrl").
-include("mango_idx.hrl").
-include("mango_cursor.hrl").


-export([
    create_build_vs/2,
    set_build_vs/4,
    get_build_vs/2,
    get_build_state/2,
    get_update_seq/2,
    set_update_seq/3,
    remove_doc/3,
    write_doc/3,
    query/4,
    base_fold_opts/1,
    mango_idx_prefix/2
]).


create_build_vs(TxDb, #idx{} = Idx) ->
    #{
        tx := Tx
    } = TxDb,
    Key = build_vs_key(TxDb, Idx#idx.ddoc),
    VS = fabric2_fdb:new_versionstamp(Tx),
    Value = erlfdb_tuple:pack_vs({VS, ?MANGO_INDEX_BUILDING}),
    ok = erlfdb:set_versionstamped_value(Tx, Key, Value).


set_build_vs(TxDb, #idx{} = Idx, VS, State) ->
    #{
        tx := Tx
    } = TxDb,

    Key = build_vs_key(TxDb, Idx#idx.ddoc),
    Value = erlfdb_tuple:pack({VS, State}),
    ok = erlfdb:set(Tx, Key, Value).


get_build_vs(TxDb, #idx{} = Idx) ->
    get_build_vs(TxDb, Idx#idx.ddoc);

get_build_vs(TxDb, DDoc) ->
    #{
        tx := Tx
    } = TxDb,
    Key = build_vs_key(TxDb, DDoc),
    EV = erlfdb:wait(erlfdb:get(Tx, Key)),
    if EV == not_found -> not_found; true ->
        erlfdb_tuple:unpack(EV)
    end.


get_build_state(TxDb, DDoc) ->
    case get_build_vs(TxDb, DDoc) of
        not_found -> ?MANGO_INDEX_BUILDING;
        {_, BuildState} -> BuildState
    end.


get_update_seq(TxDb, #idx{ddoc = DDoc}) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    case erlfdb:wait(erlfdb:get(Tx, seq_key(DbPrefix, DDoc))) of
        not_found -> <<>>;
        UpdateSeq -> UpdateSeq
    end.


set_update_seq(TxDb, #idx{ddoc = DDoc}, Seq) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    ok = erlfdb:set(Tx, seq_key(DbPrefix, DDoc), Seq).


remove_doc(TxDb, DocId, IdxResults) ->
    lists:foreach(fun (IdxResult) ->
        #{
            ddoc_id := DDocId,
            results := Results
        } = IdxResult,
        MangoIdxPrefix = mango_idx_prefix(TxDb, DDocId),
        clear_key(TxDb, MangoIdxPrefix, Results, DocId)
    end, IdxResults).


write_doc(TxDb, DocId, IdxResults) ->
    lists:foreach(fun (IdxResult) ->
        #{
            ddoc_id := DDocId,
            results := Results
        } = IdxResult,
        MangoIdxPrefix = mango_idx_prefix(TxDb, DDocId),
        add_key(TxDb, MangoIdxPrefix, Results, DocId)
    end, IdxResults).


query(Db, CallBack, Cursor, Args) ->
    #cursor{
        index = Idx
    } = Cursor,
    Mod = mango_idx:fdb_mod(Idx),
    Mod:query(Db, CallBack, Cursor, Args).


base_fold_opts(Args) ->
    #{
        dir := Direction,
        skip := Skip
    } = Args,

    [
        {skip, Skip},
        {dir, Direction},
        {streaming_mode, want_all},
        {restart_tx, true}
    ].


mango_idx_prefix(TxDb, Id) ->
    #{
        db_prefix := DbPrefix
    } = TxDb,
    Key = {?DB_MANGO, Id, ?MANGO_IDX_RANGE},
    erlfdb_tuple:pack(Key, DbPrefix).


seq_key(DbPrefix, DDoc) ->
    Key = {?DB_MANGO, DDoc, ?MANGO_UPDATE_SEQ},
    erlfdb_tuple:pack(Key, DbPrefix).


build_vs_key(Db, DDoc) ->
    #{
        db_prefix := DbPrefix
    } = Db,
    Key = {?DB_MANGO, DDoc, ?MANGO_IDX_BUILD_STATUS},
    erlfdb_tuple:pack(Key, DbPrefix).


create_key(MangoIdxPrefix, Results, DocId) ->
    EncodedResults = couch_views_encoding:encode(Results, key),
    erlfdb_tuple:pack({{EncodedResults, DocId}}, MangoIdxPrefix).


clear_key(TxDb, MangoIdxPrefix, Results, DocId) ->
    #{
        tx := Tx
    } = TxDb,
    Key = create_key(MangoIdxPrefix, Results, DocId),
    erlfdb:clear(Tx, Key).


add_key(TxDb, MangoIdxPrefix, Results, DocId) ->
    #{
        tx := Tx
    } = TxDb,
    Key = create_key(MangoIdxPrefix, Results, DocId),
    Val = couch_views_encoding:encode(Results),
    erlfdb:set(Tx, Key, Val).

