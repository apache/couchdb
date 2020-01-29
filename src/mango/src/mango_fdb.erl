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
    query_all_docs/4,
    write_doc/3,
    query/4
]).


query_all_docs(Db, CallBack, Cursor, Args) ->
    Opts = args_to_fdb_opts(Args) ++ [{include_docs, true}],
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

        Opts = args_to_fdb_opts(Args),
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


args_to_fdb_opts(Args) ->
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
%%    StartKey1 = if StartKey0 == undefined -> undefined; true ->
%%        couch_views_encoding:encode(StartKey0, key)
%%    end,

    % fabric2_fdb:fold_range switches keys around because map/reduce switches them
    % but we do need to switch them. So we do this fun dance
    {StartKeyName, EndKeyName} = case Direction of
        rev -> {end_key, start_key};
        _ -> {start_key, end_key}
    end,

    StartKeyOpts = case {StartKey0, StartKeyDocId} of
        {[], _} ->
            [];
        {null, _} ->
            %% all_docs no startkey
            [];
%%        {undefined, _} ->
%%            [];
        {StartKey0, StartKeyDocId} ->
            StartKey1 = couch_views_encoding:encode(StartKey0, key),
            [{StartKeyName, {StartKey1, StartKeyDocId}}]
    end,

    InclusiveEnd = true,

    EndKeyOpts = case {EndKey0, EndKeyDocId, Direction} of
        {<<255>>, _, _} ->
            %% all_docs no endkey
            [];
        {[<<255>>], _, _} ->
            %% mango index no endkey
            [];
%%        {undefined, _, _} ->
%%            [];
%%        {EndKey1, <<>>, rev} when not InclusiveEnd ->
%%            % When we iterate in reverse with
%%            % inclusive_end=false we have to set the
%%            % EndKeyDocId to <<255>> so that we don't
%%            % include matching rows.
%%            [{end_key_gt, {EndKey1, <<255>>}}];
%%        {EndKey1, <<255>>, _} when not InclusiveEnd ->
%%            % When inclusive_end=false we need to
%%            % elide the default end_key_docid so as
%%            % to not sort past the docids with the
%%            % given end key.
%%            [{end_key_gt, {EndKey1}}];
%%        {EndKey1, EndKeyDocId, _} when not InclusiveEnd ->
%%            [{end_key_gt, {EndKey1, EndKeyDocId}}];
        {EndKey0, EndKeyDocId, _} when InclusiveEnd ->
            EndKey1 = couch_views_encoding:encode(EndKey0, key),
            [{EndKeyName, {EndKey1, EndKeyDocId}}]
    end,

    [
        {skip, Skip},
        {dir, Direction},
        {streaming_mode, want_all}
    ] ++ StartKeyOpts ++ EndKeyOpts.


fold_cb({Key, _}, Acc) ->
    #{
        prefix := MangoIdxPrefix,
        db := Db,
        callback := Callback,
        cursor := Cursor

    } = Acc,
    {{_, DocId}} = erlfdb_tuple:unpack(Key, MangoIdxPrefix),
    {ok, Doc} = fabric2_db:open_doc(Db, DocId),
    JSONDoc = couch_doc:to_json_obj(Doc, []),
    io:format("PRINT ~p ~p ~n", [DocId, JSONDoc]),
    case Callback({doc, JSONDoc}, Cursor) of
        {ok, Cursor1} ->
            Acc#{
                cursor := Cursor1
            };
        {stop, Cursor1} ->
            throw({stop, Cursor1})
    end.


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

