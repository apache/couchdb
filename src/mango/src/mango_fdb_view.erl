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


-module(mango_fdb_view).


-export([
    query/4
]).


-include("mango_idx.hrl").
-include("mango_cursor.hrl").


query(Db, CallBack, Cursor, Args) ->
    #cursor{
        index = Idx
    } = Cursor,
    MangoIdxPrefix = mango_fdb:mango_idx_prefix(Db, Idx#idx.ddoc),
    fabric2_fdb:transactional(Db, fun (TxDb) ->
        Acc0 = #{
            cursor => Cursor,
            prefix => MangoIdxPrefix,
            db => TxDb,
            callback => CallBack
        },

        Opts = args_to_fdb_opts(Args),
        try
            Acc1 = fabric2_fdb:fold_range(TxDb, MangoIdxPrefix,
                fun fold_cb/2, Acc0, Opts),
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
        start_key := StartKey,
        start_key_docid := StartKeyDocId,
        end_key := EndKey,
        end_key_docid := EndKeyDocId
    } = Args,

    mango_fdb:base_fold_opts(Args)
        ++ start_key_opts(StartKey, StartKeyDocId)
        ++ end_key_opts(EndKey, EndKeyDocId).


start_key_opts([], _StartKeyDocId) ->
    [];

start_key_opts(StartKey, StartKeyDocId) ->
    StartKey1 = couch_views_encoding:encode(StartKey, key),
    [{start_key, {StartKey1, StartKeyDocId}}].


end_key_opts([], _EndKeyDocId) ->
    [];

end_key_opts(EndKey, EndKeyDocId) ->
    EndKey1 = couch_views_encoding:encode(EndKey, key),
    [{end_key, {EndKey1, EndKeyDocId}}].

fold_cb({Key, Val}, Acc) ->
    #{
        prefix := MangoIdxPrefix,
        db := Db,
        callback := Callback,
        cursor := Cursor

    } = Acc,
    {{_, DocId}} = erlfdb_tuple:unpack(Key, MangoIdxPrefix),
    SortKeys = couch_views_encoding:decode(Val),
    {ok, Doc} = fabric2_db:open_doc(Db, DocId, [{conflicts, true}]),
    JSONDoc = couch_doc:to_json_obj(Doc, []),
    case Callback({doc, SortKeys, JSONDoc}, Cursor) of
        {ok, Cursor1} ->
            Acc#{
                cursor := Cursor1
            };
        {stop, Cursor1} ->
            throw({stop, Cursor1})
    end.
