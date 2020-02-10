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
-include("mango_idx_view.hrl").


-export([
    create_build_vs/2,
    set_build_vs/4,
    get_build_vs/2,
    get_build_status/2,
    get_update_seq/2,
    set_update_seq/3,
    remove_doc/3,
    write_doc/3,
    query_all_docs/4,
    query/4
]).


create_build_vs(TxDb, #idx{} = Idx) ->
    #{
        tx := Tx
    } = TxDb,
    Key = build_vs_key(TxDb, Idx#idx.ddoc),
    VS = fabric2_fdb:new_versionstamp(Tx),
    Value = erlfdb_tuple:pack_vs({VS, ?MANGO_INDEX_BUILDING}),
    erlfdb:set_versionstamped_value(Tx, Key, Value).


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
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,
    Key = build_vs_key(TxDb, DDoc),
    EV = erlfdb:wait(erlfdb:get(Tx, Key)),
    case EV of
        not_found -> not_found;
        EV -> erlfdb_tuple:unpack(EV)
    end.


get_build_status(TxDb, DDoc) ->
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


query_all_docs(Db, CallBack, Cursor, Args) ->
    Opts = args_to_fdb_opts(Args, true) ++ [{include_docs, true}],
    io:format("ALL DOC OPTS ~p ~n", [Opts]),
    fabric2_db:fold_docs(Db, CallBack, Cursor, Opts).


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

        Opts = args_to_fdb_opts(Args, false),
        io:format("OPTS ~p ~n", [Opts]),
        try
            Acc1 = fabric2_fdb:fold_range(TxDb, MangoIdxPrefix, fun fold_cb/2, Acc0, Opts),
            #{
                cursor := Cursor1
            } = Acc1,
            {ok, Cursor1}
        catch
            throw:{stop, StopCursor}  ->
                {ok, StopCursor}
        end
    end).


args_to_fdb_opts(Args, AllDocs) ->
    #{
        start_key := StartKey0,
        start_key_docid := StartKeyDocId,
        end_key := EndKey0,
        end_key_docid := EndKeyDocId,
        dir := Direction,
        skip := Skip
    } = Args,

    io:format("ARGS ~p ~n", [Args]),
    io:format("START ~p ~n End ~p ~n", [StartKey0, EndKey0]),

    StartKeyOpts = case {StartKey0, StartKeyDocId} of
        {[], _} ->
            [];
        {null, _} ->
            %% all_docs no startkey
            [];
        {StartKey0, _} when AllDocs == true ->
            StartKey1 = if is_binary(StartKey0) -> StartKey0; true ->
                %% couch_views_encoding:encode(StartKey0, key)
                couch_util:to_binary(StartKey0)
            end,
            io:format("START SEction ~p ~n", [StartKey1]),
            [{start_key, StartKey1}];
        {StartKey0, StartKeyDocId} ->
            StartKey1 = couch_views_encoding:encode(StartKey0, key),
            [{start_key, {StartKey1, StartKeyDocId}}]
    end,

    InclusiveEnd = true,

    EndKeyOpts = case {EndKey0, EndKeyDocId, Direction} of
        {<<255>>, _, _} ->
            %% all_docs no endkey
            [];
        {[], _, _} ->
            %% mango index no endkey
            [];
        {[<<255>>], _, _} ->
            %% mango index no endkey with a $lt in selector
            [];
        {EndKey0, EndKeyDocId, _} when AllDocs == true ->
            EndKey1 = if is_binary(EndKey0) -> EndKey0; true ->
                couch_util:to_binary(EndKey0)
                end,
            io:format("ENDKEY ~p ~n", [EndKey1]),
            [{end_key, EndKey1}];
        {EndKey0, EndKeyDocId, _} when InclusiveEnd ->
            EndKey1 = couch_views_encoding:encode(EndKey0, key),
            [{end_key, {EndKey1, EndKeyDocId}}]
    end,


    [
        {skip, Skip},
        {dir, Direction},
        {streaming_mode, want_all}
    ] ++ StartKeyOpts ++ EndKeyOpts.


fold_cb({Key, Val}, Acc) ->
    #{
        prefix := MangoIdxPrefix,
        db := Db,
        callback := Callback,
        cursor := Cursor

    } = Acc,
    {{_, DocId}} = erlfdb_tuple:unpack(Key, MangoIdxPrefix),
    SortKeys = couch_views_encoding:decode(Val),
    {ok, Doc} = fabric2_db:open_doc(Db, DocId),
    JSONDoc = couch_doc:to_json_obj(Doc, []),
    io:format("PRINT ~p ~p ~n", [DocId, JSONDoc]),
    case Callback({doc, SortKeys, JSONDoc}, Cursor) of
        {ok, Cursor1} ->
            Acc#{
                cursor := Cursor1
            };
        {stop, Cursor1} ->
            throw({stop, Cursor1})
    end.


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

