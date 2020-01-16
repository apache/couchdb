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
    log_levels/3,
    idx_prefix/3,
    create_skip_list/3,
    fold_level0/8,
    add_kv/5,

    skiplist_start_key/3,
    skiplist_end_key/3,

    get_next/4,
    get_previous_key/4,
    get_key_after/5,
    get_key_or_nearest/6,
    get_group_level_endkey/6,
    get_level_range/6,
    wait_and_get_key/1,

    create_key/3,
    get_value/4
]).


-include("couch_views.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric2.hrl").


% Debug to show what the skip list levels look like, this will be removed
% before the PR merges
log_levels(Db, Sig, ViewId) ->
    #{
        db_prefix := DbPrefix
    } = Db,

    Levels = lists:seq(0, 6),
    ReduceIdxPrefix = idx_prefix(DbPrefix, Sig, ViewId),
    Opts = [{streaming_mode, want_all}],

    SumRows = fun (Rows) ->
        lists:foldl(fun ({_, Val}, Sum) ->
            case is_number(Val) of
                true -> Val + Sum;
                false -> Sum
            end
        end, 0, Rows)
    end,

    fabric2_fdb:transactional(Db, fun(#{tx := Tx}) ->
        lists:foldl(fun (Level, Level0Total) ->
            {StartKey, EndKey} = erlfdb_tuple:range({Level},
                ReduceIdxPrefix),

            Future = erlfdb:get_range(Tx, StartKey, EndKey, Opts),
            Rows = lists:map(fun ({_Key, EV}) ->
                get_key_value(EV)
            end, erlfdb:wait(Future)),

            io:format("~n LEVEL ~p rows ~p ~n", [Level, Rows]),
            case Level == 0 of
                true ->
                    SumRows(Rows);
                false ->
                    Total = SumRows(Rows),
                    if Total == Level0Total -> Level0Total; true ->
                        io:format("~n ~nLEVEL ~p /= Level 0 = ~p Total ~p ~n",
                            [Level, Level0Total, Total]),
                        throw(level_total_error)
                    end
            end

        end, 0, Levels)
    end).


idx_prefix(DbPrefix, Sig, ViewId) ->
    Key = {?DB_VIEWS, Sig, ?VIEW_REDUCE_RANGE, ViewId},
    erlfdb_tuple:pack(Key, DbPrefix).


create_skip_list(Db, MaxLevel, #{} = ViewOpts) ->
    #{
        db_prefix := DbPrefix,
        sig := Sig,
        view_id := ViewId,
        reduce_fun := ReduceFun
    } = ViewOpts,

    Levels = lists:seq(0, MaxLevel),
    ReduceIdxPrefix = idx_prefix(DbPrefix, Sig, ViewId),
    StartValue = couch_views_reducer:start_value(ReduceFun),

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        #{
            tx := Tx
        } = TxDb,
        lists:foreach(fun(Level) ->
            Key = create_key(ReduceIdxPrefix, Level, skip_start),
            Val = create_value(skip_start, StartValue),
            ok = erlfdb:set(Tx, Key, Val)
        end, Levels)
    end).


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


add_kv(TxDb, ReduceIdxPrefix, Level, Key, Val) ->
    #{
        tx := Tx
    } = TxDb,

    LevelKey = create_key(ReduceIdxPrefix, Level, Key),
    EV = create_value(Key, Val),

    ok = erlfdb:set(Tx, LevelKey, EV).


skiplist_start_key(TxDb, undefined, ReduceIdxPrefix) ->
    #{
        tx := Tx
    } = TxDb,

    {_, EndKey} = erlfdb_tuple:range({0},
        ReduceIdxPrefix),

    StartKey = create_key(ReduceIdxPrefix, 0, 0),
    StartKey1 = erlfdb_key:first_greater_or_equal(StartKey),

    Future = erlfdb:get_range(Tx, StartKey1, EndKey, [{limit, 1}]),
    wait_and_get_key(Future);

skiplist_start_key(_TxDb, StartKey, _ReduceIdxPrefix) ->
    StartKey.


skiplist_end_key(TxDb, undefined, ReduceIdxPrefix) ->
    #{
        tx := Tx
    } = TxDb,

    {StartKey, EndKey} = erlfdb_tuple:range({0},
        ReduceIdxPrefix),

    StartKey1 = erlfdb_key:first_greater_than(StartKey),

    Opts = [{reverse, true}, {limit, 1}],
    Future = erlfdb:get_range(Tx, StartKey1, EndKey, Opts),
    wait_and_get_key(Future);

skiplist_end_key(_TxDb, EndKey, _ReduceIdxPrefix) ->
    EndKey.


get_next(TxDb, StartKey, Level, ReduceIdxPrefix) ->
    #{
        tx := Tx
    } = TxDb,

    StartKey1 = create_key(ReduceIdxPrefix, Level, StartKey),
    StartKey2 = erlfdb_key:first_greater_than(StartKey1),

    {_, EndKey} = erlfdb_tuple:range({Level},
        ReduceIdxPrefix),

    Future = erlfdb:get_range(Tx, StartKey2, EndKey, [{limit, 1}]),
    wait_and_get_key(Future).


get_previous_key(TxDb, ReduceIdxPrefix, Level, Key) ->
    #{
        tx := Tx
    } = TxDb,

    Opts = [{limit, 1}, {streaming_mode, want_all}],
    NoPrevKV = {null, 0},

    StartKey = create_key(ReduceIdxPrefix, Level, Key),
    StartKeySel = erlfdb_key:last_less_than(StartKey),
    EndKeySel = erlfdb_key:first_greater_or_equal(StartKey),

    Future = erlfdb:get_range(Tx, StartKeySel, EndKeySel, Opts),
    case erlfdb:wait(Future) of
        [] ->
            NoPrevKV;
        [{FullEncodedKey, PackedValue}] ->
            % TODO: I think I can remove this level check
            {LevelCheck, _} = erlfdb_tuple:unpack(FullEncodedKey,
                ReduceIdxPrefix),
            if LevelCheck /= Level -> NoPrevKV; true ->
                get_key_value(PackedValue)
            end
    end.


get_key_after(TxDb, StartKey, EndKey, Level, ReduceIdxPrefix) ->
    #{
        tx := Tx
    } = TxDb,

    StartKey1 = create_key(ReduceIdxPrefix, Level, StartKey),
    StartKey2 = erlfdb_key:first_greater_than(StartKey1),

    EndKey1 = create_range_endkey(ReduceIdxPrefix, Level, EndKey),
    EndKey2 = erlfdb_key:first_greater_than(EndKey1),

    Future = erlfdb:get_range(Tx, StartKey2, EndKey2, [{limit, 1}]),
    wait_and_get_key(Future).


get_key_or_nearest(TxDb, Level, StartKey, EndKey, Reverse, ReduceIdxPrefix) ->
    #{
        tx := Tx
    } = TxDb,

    StartKey1 = create_key(ReduceIdxPrefix, Level, StartKey),
    StartKey2 = erlfdb_key:first_greater_or_equal(StartKey1),

    EndKey1 = create_range_endkey(ReduceIdxPrefix, Level, EndKey),
    EndKey2 = erlfdb_key:first_greater_than(EndKey1),

    Opts = [{limit, 1}, {reverse, Reverse}],
    Future = erlfdb:get_range(Tx, StartKey2, EndKey2, Opts),
    wait_and_get_key(Future).


get_group_level_endkey(TxDb, GroupLevel, Level, StartKey, Reverse,
        ReduceIdxPrefix) when is_list(StartKey) ->
    #{
        tx := Tx
    } = TxDb,
    GroupLevelKey = couch_views_util:group_level_key(StartKey, GroupLevel),
    StartKey1 = create_key(ReduceIdxPrefix, Level, GroupLevelKey),
    StartKey2 = erlfdb_key:first_greater_or_equal(StartKey1),
    EndKey = create_range_endkey(ReduceIdxPrefix, Level, GroupLevelKey),
    EndKey1 = erlfdb_key:first_greater_or_equal(EndKey),

    Opts = [{reverse, not Reverse}, {limit, 1}],
    Future = erlfdb:get_range(Tx, StartKey2, EndKey1, Opts),
    wait_and_get_key(Future);

get_group_level_endkey(_TxDb, _GroupLevel, _Level, Key, _Reverse,
        _ReduceIdxPrefix) ->
    Key.


get_level_range(TxDb, StartKey, EndKey, Level, Opts, ReduceIdxPrefix) ->
    #{
        tx := Tx
    } = TxDb,
    Reverse = maps:get(reverse, Opts, false),
    InclusiveEnd = maps:get(inclusive_end, Opts, true),
    InclusiveStart = maps:get(inclusive_start, Opts, true),

    StartKey1 = create_key(ReduceIdxPrefix, Level, StartKey),
    StartKey2 = case InclusiveStart of
        true -> erlfdb_key:first_greater_or_equal(StartKey1);
        false -> erlfdb_key:first_greater_than(StartKey1)
    end,

    EndKey1 = case EndKey of
        not_found ->
            {_, EndKeyLevel} = erlfdb_tuple:range({Level}, ReduceIdxPrefix),
            EndKeyLevel;
        EndKey ->
            create_key(ReduceIdxPrefix, Level, EndKey)
    end,

    EndKey2 = if InclusiveEnd == false -> EndKey1; true ->
        erlfdb_key:first_greater_than(EndKey1)
    end,

    Fun = fun ({_FullEncodedKey, PackedValue}, Acc0) ->
        KV = get_key_value(PackedValue),
        Acc0 ++ [KV]
    end,

    erlfdb:fold_range(Tx, StartKey2, EndKey2, Fun, [], [{reverse, Reverse}]).


wait_and_get_key(Future) ->
    case erlfdb:wait(Future) of
        [] ->
            not_found;
        [{_FullEncodedKey, PackedValue}] ->
            {Key, _} = get_key_value(PackedValue),
            Key
    end.


create_key(ReduceIdxPrefix, SkipLevel, skip_start) ->
    erlfdb_tuple:pack({SkipLevel, null}, ReduceIdxPrefix);

create_key(ReduceIdxPrefix, SkipLevel, Key) ->
    EK = couch_views_encoding:encode(Key, key),
    LevelKey = {SkipLevel, EK},
    erlfdb_tuple:pack(LevelKey, ReduceIdxPrefix).


get_value(TxDb, ReduceIdxPrefix, Level, Key) ->
    #{
        tx := Tx
    } = TxDb,
    EK = create_key(ReduceIdxPrefix, Level, Key),
    case erlfdb:wait(erlfdb:get(Tx, EK)) of
        not_found ->
            not_found;
        PackedValue ->
            {_, Value} = get_key_value(PackedValue),
            Value
    end.


create_range_endkey(ReduceIdxPrefix, Level, Key) when is_list(Key) ->
    EK = couch_views_encoding:encode(Key ++ [{[]}], key),
    LevelKey = {Level, EK},
    erlfdb_tuple:pack(LevelKey, ReduceIdxPrefix);

create_range_endkey(ReduceIdxPrefix, Level, Key) ->
    EK = case Key == null of
        true -> 16#FF;
        false -> couch_views_encoding:encode(Key, key)
    end,
    LevelKey = {Level, EK},
    {_, EndKey} = erlfdb_tuple:range(LevelKey, ReduceIdxPrefix),
    EndKey.


create_value(skip_start, Val) ->
    create_value(<<"couch_skip_start">>, Val);

create_value(Key, Val) ->
    Val1 = couch_views_reducer:encode(Val),

    EK = couch_views_encoding:encode(Key),
    EV = couch_views_encoding:encode(Val1),
    erlfdb_tuple:pack({EK, EV}).


get_key_value(PackedValue) ->
    {EncodedKey, EncodedValue}
            = erlfdb_tuple:unpack(PackedValue),

    Key = couch_views_encoding:decode(EncodedKey),
    Key1 = if Key /= <<"couch_skip_start">> -> Key; true ->
        skip_start
    end,

    Value = couch_views_encoding:decode(EncodedValue),
    Value1 = couch_views_reducer:decode(Value),
    {Key1, Value1}.


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
