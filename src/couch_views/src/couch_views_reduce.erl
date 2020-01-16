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

-module(couch_views_reduce).


-export([
    run_reduce/2,
    read_reduce/7
]).


-include("couch_views.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("fabric/include/fabric2.hrl").


run_reduce(#mrst{views = Views}, MappedResults) ->
    lists:map(fun
        (#{deleted := true} = MappedResults0) ->
            MappedResults0;
        (MappedResult) ->
            #{
                results := Results
            } = MappedResult,
            ReduceResults = lists:map(fun ({View, Result}) ->
                #mrview{
                    reduce_funs = ViewReduceFuns
                } = View,
                lists:map(fun({_, ReduceFun}) ->
                    couch_views_reducer:reduce(ReduceFun, Result)
                end, ViewReduceFuns)

            end, lists:zip(Views, Results)),

            MappedResult#{
                reduce_results => ReduceResults
            }
    end, MappedResults).


read_reduce(Db, Sig, ViewId, Reducer, UserCallback, UserAcc0, Args) ->
    #{
        db_prefix := DbPrefix
    } = Db,

    ReduceIdxPrefix = couch_views_reduce_fdb:idx_prefix(
        DbPrefix, Sig, ViewId),

    #mrargs{
        limit = Limit,
        group = Group,
        group_level = GroupLevel,
        use_skiplist = UseSkipList,
        skip = Skip
    } = Args,
    GroupLevel1 = case Group of
        true -> group_true;
        _ -> GroupLevel
    end,
    Opts = case UseSkipList of
        true -> args_to_skiplist_opts(Args);
        false -> args_to_fdb_opts(Args)
    end,

    Acc0 = #{
        sig => Sig,
        view_id => ViewId,
        user_acc => UserAcc0,
        args => Args,
        callback => UserCallback,
        reduce_idx_prefix => ReduceIdxPrefix,
        limit => Limit,
        row_count => 0,
        skip => Skip
    },
    read_reduce_int(Db, Sig, ViewId, Reducer, GroupLevel1, Acc0, Opts,
        UseSkipList).


read_reduce_int(Db, Sig, ViewId, Reducer, GroupLevel, Acc0, Opts,
    UseSkiplist) ->
    try
        Fun = fun handle_row/3,
        Acc1 = fabric2_fdb:transactional(Db, fun(TxDb) ->
            case UseSkiplist of
                true ->
                    couch_views_skiplist:fold(TxDb, Sig, ViewId,
                        Reducer, GroupLevel, Opts, Fun, Acc0);
                false ->
                    couch_views_reduce_fdb:fold_level0(TxDb, Sig, ViewId,
                        Reducer, GroupLevel, Opts, Fun, Acc0)
            end
        end),
        #{
            callback := UserCallback,
            user_acc := UserAcc1
        } = Acc1,
        UserAcc2 = maybe_stop(UserCallback(complete, UserAcc1)),
        {ok, UserAcc2}
    catch
        throw:reduce_transaction_ended ->
            {ContinueStartKey, Acc} = get(reduce_acc),
            ReduceIdxPrefix = maps:get(reduce_idx_prefix, Acc),

            StartKey0 = couch_views_reduce_fdb:create_key(ReduceIdxPrefix, 0,
                ContinueStartKey),
            StartKey = erlfdb_key:first_greater_or_equal(StartKey0),
            FdbOpts1 = lists:keyreplace(startkey, 1, Opts,
                {startkey, StartKey}),

            read_reduce_int(Db, Sig, ViewId, Reducer, GroupLevel, Acc,
                FdbOpts1, UseSkiplist);
        throw:{done, Out} ->
            {ok, Out}
    end.


args_to_skiplist_opts(#mrargs{} = Args) ->
    #mrargs{
        start_key = StartKey,
        end_key = EndKey,
        direction = Direction,
        inclusive_end = InclusiveEnd
    } = Args,

    StartKey1 = case StartKey of
        undefined ->
            undefined;
        StartKey ->
            StartKey
    end,

    Reverse = Direction == rev,
    % CouchDB swaps the key meanings based on the direction
    % of the fold. FoundationDB does not so we have to
    % swap back here.
    {StartKey2, EndKey2} = case Reverse of
        true -> {EndKey, StartKey1};
        false -> {StartKey1, EndKey}
    end,

    #{
        startkey => StartKey2,
        endkey => EndKey2,
        reverse => Reverse,
        inclusive_end => InclusiveEnd
    }.


args_to_fdb_opts(#mrargs{} = Args) ->
    #mrargs{
        start_key = StartKey,
        end_key = EndKey,
        direction = Direction,
        inclusive_end = InclusiveEnd
    } = Args,


    StartKeyOpts = case {StartKey, Direction} of
        {undefined, rev}  ->
            [{start_key, {0, <<255>>}}];
        {undefined, fwd}  ->
            [{start_key, {0, null, <<255>>}}];
        {StartKey, _} ->
            [{start_key, {0, couch_views_encoding:encode(StartKey, key)}}]
    end,

    EndKeyOpts = case {EndKey, Direction} of
        {undefined, rev} ->
            [{end_key_gt, {0, null}}];
        {undefined, fwd} ->
            [{end_key, {0, <<255>>}}];
        {EndKey, _} when InclusiveEnd ->
            [{end_key, {0, couch_views_encoding:encode(EndKey, key)}}];
        {EndKey, _} when not InclusiveEnd ->
            [{end_key_gt, {0, couch_views_encoding:encode(EndKey, key)}}]
    end,

    [
        {dir, Direction},
        {streaming_mode, want_all},
        {add_tuple, false}
    ] ++ StartKeyOpts ++ EndKeyOpts.


handle_row(_Key, _Value, #{skip := Skip} = Acc) when Skip > 0 ->
    Acc#{skip := Skip - 1};

handle_row(Key, Value, Acc) ->
    #{
        callback := UserCallback,
        user_acc := UserAcc0,
        row_count := RowCount,
        limit := Limit
    } = Acc,

    Row = [
        {key, Key},
        {value, Value}
    ],

    RowCountNext = RowCount + 1,

    UserAcc1 = maybe_stop(UserCallback({row, Row}, UserAcc0)),
    Acc1 = Acc#{user_acc := UserAcc1, row_count := RowCountNext},

    case RowCountNext == Limit of
        true ->
            UserAcc2 = maybe_stop(UserCallback(complete, UserAcc1)),
            maybe_stop({stop, UserAcc2});
        false ->
            Acc1
    end.


maybe_stop({ok, Acc}) -> Acc;
maybe_stop({stop, Acc}) -> throw({done, Acc}).
