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
    write_doc/3,
    query/4
]).


query(Db, CallBack, Cursor, Args) ->
    #cursor{
        index = Idx
    } = Cursor,
    MangoIdxPrefix = mango_idx_prefix(Db, Idx#idx.ddoc),
    fabric2_fdb:transactional(Db, fun (TxDb) ->
        Acc0 = #{
            cursor => Cursor,
            prefix => MangoIdxPrefix,
            db => TxDb,
            callback => CallBack
        },
        io:format("DB ~p ~n", [TxDb]),
        Acc1 = fabric2_fdb:fold_range(TxDb, MangoIdxPrefix, fun fold_cb/2, Acc0, []),
        #{
            cursor := Cursor1
        } = Acc1,
        {ok, Cursor1}
    end).


fold_cb({Key, _}, Acc) ->
    #{
        prefix := MangoIdxPrefix,
        db := Db,
        callback := Callback,
        cursor := Cursor

    } = Acc,
    {_, DocId} = erlfdb_tuple:unpack(Key, MangoIdxPrefix),
    {ok, Doc} = fabric2_db:open_doc(Db, DocId),
    io:format("PRINT ~p ~p ~n", [DocId, Doc]),
    {ok, Cursor1} = Callback(Doc, Cursor),
    Acc#{
        cursor := Cursor1
    }.


write_doc(TxDb, DocId, IdxResults) ->
    lists:foreach(fun (IdxResult) ->
        #{
            ddoc_id := DDocId,
            results := Results
        } = IdxResult,
        MangoIdxPrefix = mango_idx_prefix(TxDb, DDocId),
        add_key(TxDb, MangoIdxPrefix, Results, DocId)
        end, IdxResults).


mango_idx_prefix(TxDb, Id) ->
    #{
        db_prefix := DbPrefix
    } = TxDb,
    Key = {?DB_MANGO, Id, ?MANGO_IDX_RANGE},
    erlfdb_tuple:pack(Key, DbPrefix).


add_key(TxDb, MangoIdxPrefix, Results, DocId) ->
    #{
        tx := Tx
    } = TxDb,
    EncodedResults = couch_views_encoding:encode(Results, key),
    Key = erlfdb_tuple:pack({EncodedResults, DocId}, MangoIdxPrefix),
    erlfdb:set(Tx, Key, <<0>>).

