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


-module(couch_views_reduce_fdb).


-export([
    write_doc_reduce/5,


    idx_prefix/3,
    fold_level0/8,
    add_kv/4,


    create_key/2,
    get_value/3
]).


-include("couch_views.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric2.hrl").


write_doc_reduce(TxDb, Sig, Views, Doc, ExistingViewKeys) ->
    #{
        id := DocId,
%%        results := Results,
%%        kv_sizes := KVSizes,
        reduce_results := ReduceResults
    } = Doc,

    lists:foreach(fun({View, ViewReduceResult}) ->
        #mrview{
            reduce_funs = ViewReduceFuns,
            id_num = ViewId
        } = View,

        ExistingKeys = case lists:keyfind(ViewId, 1, ExistingViewKeys) of
            {_ViewId, _TotalRows, _TotalSize, EKeys} ->
                EKeys;
            false ->
                []
        end,
        update_reduce_idx(TxDb, Sig, ViewId, ViewReduceFuns, DocId,
            ExistingKeys, ViewReduceResult)
    end, lists:zip(Views, ReduceResults)).


update_reduce_idx(TxDb, Sig, ViewId, ViewReduceFuns, DocId, ExistingKeys,
    ViewReduceResult) ->
    lists:foreach(fun({ViewReduceFun, ReduceResult}) ->
        {_, ReduceFun} = ViewReduceFun,
        ReduceId = couch_views_util:reduce_id(ViewId, ReduceFun),
        couch_views_skiplist:update_idx(TxDb, Sig, ReduceId,
            ReduceFun, DocId, ExistingKeys, ReduceResult)
    end, lists:zip(ViewReduceFuns, ViewReduceResult)).


idx_prefix(DbPrefix, Sig, ViewId) ->
    Key = {?DB_VIEWS, ?VIEW_DATA, Sig, ?VIEW_REDUCE_RANGE, ViewId},
    erlfdb_tuple:pack(Key, DbPrefix).



% Folds over level 0 and reduces all keys in memory
fold_level0(Db, Sig, ViewId, Reducer, GroupLevel, Opts, UserCallback,
    UserAcc0) ->
    #{
        db_prefix := DbPrefix
    } = Db,

    ReduceIdxPrefix = idx_prefix(DbPrefix, Sig, ViewId),
    Acc = #{
        sig => Sig,
        view_id => ViewId,
        user_acc => UserAcc0,
        callback => UserCallback,
        reduce_idx_prefix => ReduceIdxPrefix,
        reducer => Reducer,
        group_level => GroupLevel,
        rows => []
    },

    try
        fabric2_fdb:transactional(Db, fun(TxDb) ->
            Fun = fun fold_level0_cb/2,
            #{
                user_acc := UserAcc1,
                rows := Rows
            } = fabric2_fdb:fold_range(TxDb, ReduceIdxPrefix, Fun, Acc, Opts),

            couch_views_skiplist:rereduce_and_reply(Reducer, Rows, GroupLevel,
                UserCallback, UserAcc1)
        end)
    catch
        error:{erlfdb_error, 1007}  ->
            % This error happens if we are streaming results for longer than
            % 5 seconds. We catch this error in couch_views_reduce and start a
            % new transaction then continue streaming results
            % from where we left off
            throw(reduce_transaction_ended)
    end.


add_kv(TxDb, ReduceIdxPrefix, Key, Val) ->
    #{
        tx := Tx
    } = TxDb,

    LevelKey = create_key(ReduceIdxPrefix, Key),
    EV = create_value(Key, Val),

    ok = erlfdb:set(Tx, LevelKey, EV).


create_key(ReduceIdxPrefix, Key) ->
    EK = couch_views_encoding:encode(Key, key),
    erlfdb_tuple:pack({EK}, ReduceIdxPrefix).


get_value(TxDb, ReduceIdxPrefix, Key) ->
    #{
        tx := Tx
    } = TxDb,
    EK = create_key(ReduceIdxPrefix, Key),
    case erlfdb:wait(erlfdb:get(Tx, EK)) of
        not_found ->
            not_found;
        PackedValue ->
            {_, Value} = get_key_value(PackedValue),
            Value
    end.


create_value(Key, Val) ->
    Val1 = couch_views_reducer:encode(Val),

    EK = couch_views_encoding:encode(Key),
    EV = couch_views_encoding:encode(Val1),
    erlfdb_tuple:pack({EK, EV}).


get_key_value(PackedValue) ->
    {EncodedKey, EncodedValue} = erlfdb_tuple:unpack(PackedValue),

    Key = couch_views_encoding:decode(EncodedKey),
    Value = couch_views_encoding:decode(EncodedValue),
    Value1 = couch_views_reducer:decode(Value),
    {Key, Value1}.


fold_level0_cb({_FullEncodedKey, EV}, Acc) ->
    #{
        callback := Callback,
        user_acc := UserAcc,
        group_level := GroupLevel,
        rows := Rows,
        reducer := Reducer
    } = Acc,

    {Key, Val} = get_key_value(EV),

    LastKey = if Rows == [] -> false; true ->
        {LastKey0, _} = lists:last(Rows),
        LastKey0
    end,

    GroupLevelKey = couch_views_util:group_level_key(Key, GroupLevel),
    GroupKV = [{GroupLevelKey, Val}],

    Acc1 = case GroupLevelKey == LastKey of
        true ->
            Acc#{
                rows := Rows ++ GroupKV
            };
        false ->
            UserAcc1 = couch_views_skiplist:rereduce_and_reply(Reducer, Rows,
                GroupLevel, Callback, UserAcc),
            put(reduce_acc, {Key, UserAcc1}),
            Acc#{
                user_acc := UserAcc1,
                rows := GroupKV
            }
    end,
    Acc1.
