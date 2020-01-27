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

        Opts = args_to_fdb_opts(Args),
        Acc1 = fabric2_fdb:fold_range(TxDb, MangoIdxPrefix, fun fold_cb/2, Acc0, Opts),
        #{
            cursor := Cursor1
        } = Acc1,
        {ok, Cursor1}
    end).


args_to_fdb_opts(Args) ->
    #{
        start_key := StartKey0,
        start_key_docid := StartKeyDocId,
        end_key := EndKey0,
        end_key_docid := EndKeyDocId,
        direction := Direction,
        skip := Skip
    } = Args,

    StartKey1 = if StartKey0 == undefined -> undefined; true ->
        couch_views_encoding:encode(StartKey0, key)
    end,

    StartKeyOpts = case {StartKey1, StartKeyDocId} of
        {undefined, _} ->
            [];
        {StartKey1, StartKeyDocId} ->
            [{start_key, {StartKey1, StartKeyDocId}}]
    end,

    {EndKey1, InclusiveEnd} = get_endkey_inclusive(EndKey0),

    EndKeyOpts = case {EndKey1, EndKeyDocId, Direction} of
        {undefined, _, _} ->
            [];
        {EndKey1, <<>>, rev} when not InclusiveEnd ->
            % When we iterate in reverse with
            % inclusive_end=false we have to set the
            % EndKeyDocId to <<255>> so that we don't
            % include matching rows.
            [{end_key_gt, {EndKey1, <<255>>}}];
        {EndKey1, <<255>>, _} when not InclusiveEnd ->
            % When inclusive_end=false we need to
            % elide the default end_key_docid so as
            % to not sort past the docids with the
            % given end key.
            [{end_key_gt, {EndKey1}}];
        {EndKey1, EndKeyDocId, _} when not InclusiveEnd ->
            [{end_key_gt, {EndKey1, EndKeyDocId}}];
        {EndKey1, EndKeyDocId, _} when InclusiveEnd ->
            [{end_key, {EndKey1, EndKeyDocId}}]
    end,
    [
        {skip, Skip},
        {dir, Direction},
        {streaming_mode, want_all}
    ] ++ StartKeyOpts ++ EndKeyOpts.


get_endkey_inclusive(undefined) ->
    {undefined, true};

get_endkey_inclusive(EndKey) when is_list(EndKey) ->
    {EndKey1, InclusiveEnd} = case lists:member(less_than, EndKey) of
        false ->
            {EndKey, true};
        true ->
            Filtered = lists:filter(fun (Key) -> Key /= less_than end, EndKey),
            io:format("FIL be ~p after ~p ~n", [EndKey, Filtered]),
            {Filtered, false}
    end,
    {couch_views_encoding:encode(EndKey1, key), InclusiveEnd}.


fold_cb({Key, _}, Acc) ->
    #{
        prefix := MangoIdxPrefix,
        db := Db,
        callback := Callback,
        cursor := Cursor

    } = Acc,
    {{_, DocId}} = erlfdb_tuple:unpack(Key, MangoIdxPrefix),
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
    Key = erlfdb_tuple:pack({{EncodedResults, DocId}}, MangoIdxPrefix),
    erlfdb:set(Tx, Key, <<0>>).

