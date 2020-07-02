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
    write_doc/5,

    idx_prefix/3,
    fold_level/8
]).


-include("couch_views.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric2.hrl").

write_doc(TxDb, Sig, Views, #{deleted := true} = Doc, ExistingViewKeys) ->
    #{
        id := DocId
    } = Doc,

    lists:foreach(fun(View) ->
        #mrview{
            reduce_funs = ViewReduceFuns,
            id_num = ViewId
        } = View,

        ReduceResults = case lists:keyfind(ViewId, 1, ExistingViewKeys) of
            {ViewId, _TotalRows, _TotalSize, EKeys} ->
                ExistingResults = fetch_existing_results(TxDb, Sig, ViewId,
                    ViewReduceFuns, DocId, EKeys),
                create_view_deltas([], ExistingResults, []);
            false ->
                []
        end,
        update_indexes(TxDb, Sig, ViewId, ViewReduceFuns, DocId, ReduceResults)
    end, Views);

write_doc(TxDb, Sig, Views, Doc, ExistingViewKeys) ->
    #{
        id := DocId,
        reduce_results := ReduceResults
    } = Doc,

    lists:foreach(fun({View, NewResult}) ->
        #mrview{
            reduce_funs = ViewReduceFuns,
            id_num = ViewId
        } = View,

        ReduceDelta = case lists:keyfind(ViewId, 1, ExistingViewKeys) of
            {_ViewId, _TotalRows, _TotalSize, EKeys} ->
                ExistingResults = fetch_existing_results(TxDb, Sig, ViewId,
                    ViewReduceFuns, DocId, EKeys),
                create_view_deltas(NewResult, ExistingResults, []);
            false ->
                NewResult
        end,
        update_indexes(TxDb, Sig, ViewId, ViewReduceFuns, DocId, ReduceDelta)
    end, lists:zip(Views, ReduceResults)).


update_indexes(TxDb, Sig, ViewId, ViewReduceFuns, DocId, ViewReduceResult) ->
    lists:foreach(fun({ViewReduceFun, ReduceResult}) ->
        ReduceId = couch_views_util:reduce_id(ViewId, ViewReduceFun),
        update_index(TxDb, Sig, ReduceId, DocId, ReduceResult)
    end, lists:zip(ViewReduceFuns, ViewReduceResult)).


create_view_deltas([], [], Acc) ->
    Acc;

create_view_deltas([], [ExistingResult | Rest], Acc) ->
    Acc1 = Acc ++ create_reduce_fun_delta([], ExistingResult),
    create_view_deltas([], Rest, Acc1);

create_view_deltas([NewResult | NewRest], [ExistingResult | ExistingRest], Acc) ->
    Acc1 = Acc ++ create_reduce_fun_delta(NewResult, ExistingResult),
    create_view_deltas(NewRest, ExistingRest, Acc1).


create_reduce_fun_delta(NewResults, ExistingResults) ->
    {NewResults2, DeltaResults} = lists:foldl(fun ({Key, ExistingVal},
            {NewResults0, Acc}) ->

        {RemainingNew, DeltaResult} = case lists:keytake(Key, 1, NewResults0) of
            false ->
                {NewResults0, [{Key, -ExistingVal}]};
            {value, {Key, NewVal}, NewResult1} ->
                Delta = NewVal - ExistingVal,

                % If the difference is 0 not need to update the reduce
                case Delta of
                    0 -> {NewResult1, []};
                    Val -> {NewResult1, [{Key, Val}]}
                end
        end,

        {RemainingNew, Acc ++ DeltaResult}
    end, {NewResults, []}, ExistingResults),
    [DeltaResults ++ NewResults2].


fetch_existing_results(TxDb, Sig, ViewId, ViewReduceFuns, DocId,
        ExistingKeys) ->
    CB = fun(_DocId, Key, Value, Acc) ->
        Acc ++ [{Key, Value}]
    end,

    Rows = lists:foldl(fun (Key, Acc) ->
        EK = couch_views_encoding:encode(Key, key),
        Opts = [
            {start_key, {EK, DocId}},
            {end_key, {EK, DocId, <<255>>}}
        ],
        Rows = couch_views_fdb:fold_map_idx(TxDb, Sig, ViewId, Opts, CB, []),
        Acc ++ Rows
    end, [], ExistingKeys),

    lists:map(fun ({_, Reducer}) ->
        couch_views_reducer:rereduce(Reducer, Rows, ?GROUP_TRUE)
    end, ViewReduceFuns).


update_index(TxDb, Sig, ReduceId, _DocId, ReduceResult) ->
    #{
        db_prefix := DbPrefix
    } = TxDb,

    ReduceIdxPrefix = idx_prefix(DbPrefix, Sig, ReduceId),
    lists:foreach(fun ({Key, Val}) ->
        GroupLevels = couch_views_util:key_group_levels(Key),
        lists:foreach(fun (GroupLevel) ->
            GroupKey = couch_views_util:group_level_key(Key, GroupLevel),
            add_kv_to_group_level(TxDb, ReduceIdxPrefix, GroupLevel, GroupKey, Val)
        end, GroupLevels)
    end, ReduceResult).


add_kv_to_group_level(Db, ReduceIdxPrefix, GroupLevel, Key, Val) ->
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        Val1 = case get_value(TxDb, ReduceIdxPrefix, GroupLevel, Key) of
            not_found ->
                Val;
            ExistingVal ->
                ExistingVal + Val
        end,
        add_kv(TxDb, ReduceIdxPrefix, GroupLevel, Key, Val1)
    end).


idx_prefix(DbPrefix, Sig, ViewId) ->
    Key = {?DB_VIEWS, ?VIEW_DATA, Sig, ?VIEW_REDUCE_RANGE, ViewId},
    erlfdb_tuple:pack(Key, DbPrefix).


fold_level(Db, Sig, ViewId, Reducer, GroupLevel, Opts, UserCallback,
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

            rereduce_and_reply(Reducer, Rows, GroupLevel,
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


rereduce_and_reply(_Reducer, [], _GroupLevel, _Callback, Acc) ->
    Acc;

rereduce_and_reply(Reducer, Rows, GroupLevel, Callback, Acc0) ->
    ReReduced = couch_views_reducer:rereduce(Reducer, Rows,
        GroupLevel),
    lists:foldl(fun ({ReducedKey, ReducedVal}, Acc) ->
        {ok, FinalizedVal} = couch_views_reducer:finalize(Reducer, ReducedVal),
        Callback(ReducedKey, FinalizedVal, Acc)
    end, Acc0, ReReduced).


add_kv(TxDb, ReduceIdxPrefix, GroupLevel, Key, Val) ->
    #{
        tx := Tx
    } = TxDb,

    LevelKey = create_key(ReduceIdxPrefix, GroupLevel, Key),
    EV = create_value(Key, Val),

    ok = erlfdb:set(Tx, LevelKey, EV).


create_key(ReduceIdxPrefix, GroupLevel, Key) ->
    EK = couch_views_encoding:encode(Key, key),
    erlfdb_tuple:pack({{GroupLevel, EK}}, ReduceIdxPrefix).


get_value(TxDb, ReduceIdxPrefix, GroupLevel, Key) ->
    #{
        tx := Tx
    } = TxDb,
    EK = create_key(ReduceIdxPrefix, GroupLevel, Key),
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
            UserAcc1 = rereduce_and_reply(Reducer, Rows,
                GroupLevel, Callback, UserAcc),
            put(reduce_acc, {Key, UserAcc1}),
            Acc#{
                user_acc := UserAcc1,
                rows := GroupKV
            }
    end,
    Acc1.
