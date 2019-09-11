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
% the License.-author("garren").


-module(couch_views_skiplist).


-export([
    fold/8,
    rereduce_and_reply/5,
    create_indexes/3,
    update_idx/7
]).


-include_lib("couch_mrview/include/couch_mrview.hrl").

-define(MAX_SKIP_LIST_LEVELS, 6).

% QUERYING SKIPLIST

% Main entry point to read the full skip list.
% See traverse_skip_list for the main algorithmn and explanation
fold(Db, Sig, ViewId, Reducer, GroupLevel, Opts, UserCallback, UserAcc0) ->
    #{
        db_prefix := DbPrefix
    } = Db,

    #{
        startkey := StartKey,
        endkey := EndKey,
        reverse := Reverse,
        inclusive_end := InclusiveEnd
    } = Opts,

    Stats0 = create_stats(),
    ReduceIdxPrefix = couch_views_reduce_fdb:idx_prefix(DbPrefix, Sig,
        ViewId),
    Acc = #{
        sig => Sig,
        view_id => ViewId,
        user_acc => UserAcc0,
        callback => UserCallback,
        reduce_idx_prefix => ReduceIdxPrefix,
        reducer => Reducer,
        group_level => GroupLevel,
        rows => [],
        stats => Stats0,
        reverse => Reverse,
        inclusive_end => InclusiveEnd
    },

    try
        fabric2_fdb:transactional(Db, fun(TxDb) ->
            StartKey1 = couch_views_reduce_fdb:skiplist_start_key(TxDb,
                StartKey, ReduceIdxPrefix),
            EndKey1 = couch_views_reduce_fdb:skiplist_end_key(TxDb, EndKey,
                ReduceIdxPrefix),

            % TODO: Add optimisation to read from level 6 for group_level = 0
            % with no startkey/endkey
            Acc1 = traverse_skip_list(TxDb, 0, StartKey1, EndKey1, Acc),
            #{
                user_acc := UserAcc1,
                rows := Rows1,
                stats := Stats
            } = Acc1,
            rereduce_and_reply(Reducer, Rows1, GroupLevel, UserCallback,
                UserAcc1)
        end)
    catch
        error:{erlfdb_error, Code} ->
            couch_log:error("FDB ERROR ~p ~n", [Code])
    end.


rereduce_and_reply(_Reducer, [], _GroupLevel, _Callback, Acc) ->
    Acc;

rereduce_and_reply(Reducer, Rows, GroupLevel, Callback, Acc) ->
    {ReducedKey, ReducedVal} = couch_views_reducer:rereduce(Reducer, Rows,
        GroupLevel),
    {ok, FinalizedVal} = couch_views_reducer:finalize(Reducer, ReducedVal),
    Callback(ReducedKey, FinalizedVal, Acc).


% INSERTING INTO SKIPLIST
create_indexes(Db, Sig, Views) ->
    #{
        db_prefix := DbPrefix
    } = Db,

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        lists:foreach(fun (View) ->
            #mrview{
                id_num = Id,
                reduce_funs = ReduceFuns
            } = View,

            lists:foreach(fun ({_, ReduceFun}) ->
                ReduceId = couch_views_util:reduce_id(Id, ReduceFun),
                ViewOpts = #{
                    db_prefix => DbPrefix,
                    sig => Sig,
                    view_id => ReduceId,
                    reduce_fun => ReduceFun
                },
                couch_views_reduce_fdb:create_skip_list(TxDb,
                    ?MAX_SKIP_LIST_LEVELS, ViewOpts)
            end, ReduceFuns)

        end, Views)
    end).


update_idx(TxDb, Sig, ViewId, Reducer, _DocId, _ExistingKeys, ReduceResult) ->
    #{
        db_prefix := DbPrefix
    } = TxDb,

    ViewOpts = #{
        db_prefix => DbPrefix,
        sig => Sig,
        view_id => ViewId,
        reducer => Reducer
    },
    ReduceIdxPrefix = couch_views_reduce_fdb:idx_prefix(DbPrefix, Sig, ViewId),
    lists:foreach(fun ({Key, Val}) ->
        add_kv_to_skip_list(TxDb, ReduceIdxPrefix, ?MAX_SKIP_LIST_LEVELS,
            ViewOpts, Key, Val)
    end, ReduceResult).


% The algorithmn for traversing the skiplist and finding all the key/values
% for a reduce query is as follows:
%
% * Using the current key and the endkey find the next range of keys for a
%   given group level that we can fetch. We always start at level 0 and then
%   keep looking up higher levels to try and find the highest level we can
%   search
% * Fetch the range of keys and use the final key in the list as the possible
%   next key to start searching from
% * Process the results and return the possible next key to start from
% * Determine if the skiplist traversal is finished, otherwise continue
%   scanning with the new start key
traverse_skip_list(_TxDb, Level, _CurrentKey, _EndKey, _Acc) when Level < 0 ->
    throw(skip_list_gone_to_low);

traverse_skip_list(TxDb, _Level, CurrentKey, EndKey, Acc) ->
    #{
        reduce_idx_prefix := ReduceIdxPrefix,
        group_level := GroupLevel,
        stats := Stats,
        reverse := Reverse,
        inclusive_end := InclusiveEnd
    } = Acc,

    {
        RangeLevel,
        RangeStart,
        RangeEnd
    } = find_next_range_and_level(TxDb, ReduceIdxPrefix, GroupLevel,
        Reverse, CurrentKey, EndKey),

    % For inclusive_end = false when Reverse == true
    InclusiveStart = case {InclusiveEnd, Reverse, CurrentKey == RangeStart} of
        {false, true, true} -> false;
        _ -> true
    end,

    RangeOpts = #{
        inclusive_end => true,
        inclusive_start => InclusiveStart,
        reverse => Reverse
    },

    Results = couch_views_reduce_fdb:get_level_range(TxDb, RangeStart,
        RangeEnd, RangeLevel, RangeOpts, ReduceIdxPrefix),

    LastResultKey = if Results == [] -> not_found; true ->
        {LastKey, _} = lists:last(Results),
        LastKey
    end,

    NextKey = skiplist_next_key(TxDb, ReduceIdxPrefix, Reverse, RangeLevel,
        LastResultKey, CurrentKey, EndKey),
    {NextStart, Acc1} = process_reduce_results(Results, LastResultKey, NextKey,
        Acc, RangeLevel),

    Stats1 = update_stats(Stats, RangeLevel, RangeStart, RangeEnd),
    Acc2 = Acc1#{stats := Stats1},

    Finished = case Reverse of
        true ->
            CurrentKey == RangeStart;
        false ->
            ExclusiveEnd = InclusiveEnd == false andalso NextStart == EndKey,
            RangeEnd == EndKey orelse ExclusiveEnd
    end,

    case Finished orelse NextStart == not_found of
        true ->
            Acc2;
        false when Reverse == true ->
            traverse_skip_list(TxDb, 0, CurrentKey, NextStart, Acc2);
        false ->
            traverse_skip_list(TxDb, 0, NextStart, EndKey, Acc2)
    end.


skiplist_next_key(TxDb, ReduceIdxPrefix, true, RangeLevel,
    LastResultKey, CurrentKey, _EndKey) ->

    {PreviousKey, _} = couch_views_reduce_fdb:get_previous_key(TxDb,
        ReduceIdxPrefix, RangeLevel, LastResultKey),
    if PreviousKey < CurrentKey -> CurrentKey; true ->
        PreviousKey
    end;

skiplist_next_key(TxDb, ReduceIdxPrefix, false, RangeLevel, LastResultKey,
    _CurrentKey, EndKey) ->
    couch_views_reduce_fdb:get_key_after(TxDb, LastResultKey, EndKey,
        RangeLevel, ReduceIdxPrefix).


% Basic stats that records which level the range reads were performed at
create_stats() ->
    lists:foldl(fun(Level, Acc) ->
        Acc#{Level => []}
    end, #{}, lists:seq(0, ?MAX_SKIP_LIST_LEVELS)).


update_stats(Stats, RangeLevel, RangeStart, RangeEnd) ->
    maps:update_with(RangeLevel, fun (RangeStats) ->
        RangeStats ++ [{RangeStart, RangeEnd}]
    end, Stats).


% Used to determine the level and startkey/endkey that can be used for
% the startkey and grouplevel.
% Start at level 0 and then keep looking at the level above to find the highest
% possible level to read from.
% If the level above does not contain the Startkey. We use the Key above as the
% Endkey and will then read to that key at the current level so that the next
% read can then read at the higher level
find_next_range_and_level(TxDb, ReduceIdxPrefix, GroupLevel, false, StartKey,
    EndKey) ->
    GroupEndKey = couch_views_reduce_fdb:get_group_level_endkey(TxDb,
        GroupLevel, 0, StartKey, false, ReduceIdxPrefix),
    % Do not exceed the set endkey
    GroupEndKey1 = if GroupEndKey < EndKey -> GroupEndKey; true -> EndKey end,

    LevelRanges = [{0, StartKey, GroupEndKey1}],
    LevelRanges1 = find_level_ranges(TxDb, 0, GroupLevel, StartKey,
        GroupEndKey1, false, ReduceIdxPrefix, LevelRanges),
    lists:last(LevelRanges1);

% Going in reverse direction
find_next_range_and_level(TxDb, ReduceIdxPrefix, GroupLevel, true, StartKey,
    EndKey) ->
    GroupStartKey = couch_views_reduce_fdb:get_group_level_endkey(TxDb,
        GroupLevel, 0, EndKey, true, ReduceIdxPrefix),

    % Do not exceed the set startkey
    GroupStartKey1 = if GroupStartKey < StartKey -> StartKey; true ->
        GroupStartKey
    end,

    LevelRanges = [{0, GroupStartKey1, EndKey}],
    LevelRanges1 = find_level_ranges(TxDb, 0, GroupLevel, GroupStartKey1,
        EndKey, true, ReduceIdxPrefix, LevelRanges),
    lists:last(LevelRanges1).


% at end of this specific grouplevel, so have to do final scan at level 0
find_level_ranges(_TxDb, _Level, _GroupLevel, Key, Key, _Reverse,
    _ReduceIdxPrefix, _Acc) ->
    [{0, Key, Key}];

% scanned all levels
find_level_ranges(_TxDb, ?MAX_SKIP_LIST_LEVELS + 1, _GroupLevel, StartKey,
    StartKey, _Reverse, _ReduceIdxPrefix, Acc) ->
    Acc;

find_level_ranges(TxDb, Level, GroupLevel, StartKey, EndKey, Reverse,
    ReduceIdxPrefix, Acc) ->
    NextLevel = Level + 1,
    NearestNextLevelKey = couch_views_reduce_fdb:get_key_or_nearest(TxDb,
        NextLevel, StartKey, EndKey, Reverse, ReduceIdxPrefix),

    KeysEqual = case Reverse of
        true -> EndKey =:= NearestNextLevelKey;
        false -> StartKey =:= NearestNextLevelKey
    end,

    case KeysEqual of
        true ->
            GroupLevelEndKey =
                    couch_views_reduce_fdb:get_group_level_endkey(TxDb,
                        GroupLevel, NextLevel, StartKey, Reverse,
                        ReduceIdxPrefix),

            ToFar = case Reverse of
                true -> GroupLevelEndKey < StartKey;
                false -> GroupLevelEndKey > EndKey
            end,

            EndOfLevel = GroupLevelEndKey == NearestNextLevelKey,

            case ToFar orelse EndOfLevel of
                true ->
                    Acc;
                false ->
                    Acc1 = Acc ++ [{NextLevel, StartKey, GroupLevelEndKey}],
                    find_level_ranges(TxDb, NextLevel, GroupLevel, StartKey,
                        EndKey, Reverse, ReduceIdxPrefix, Acc1)
            end;
        false ->
            case couch_views_util:group_level_equal(StartKey,
                NearestNextLevelKey, GroupLevel) of
                true ->
                    [{Level, StartKey, NearestNextLevelKey}];
                false ->
                    Acc
            end
    end.


% Compare the last key from the results with the next key in the skiplist at
% the specific skip list level. If they are group level equal,
% we need to keep scanning before returning the results. If they not equal
% and the rangelevel = 0 then rereduce and return the results. We have reached
% the end of a group level.
% If its not equal and not at skiplevel 0 contine to fetch more results before
% returning them
process_reduce_results(Results, LastResultKey, NextKey, Acc, RangeLevel) ->
    #{
        user_acc := UserAcc,
        callback := UserCallback,
        reducer := Reducer,
        group_level := GroupLevel,
        rows := Rows
    } = Acc,

    case couch_views_util:group_level_equal(LastResultKey, NextKey, GroupLevel)
    of
        true ->
            AccNext = Acc#{rows := Rows ++ Results},
            {NextKey, AccNext};
        false when RangeLevel == 0 ->
            AllResults = Rows ++ Results,
            UserAcc1 = rereduce_and_reply(Reducer, AllResults, GroupLevel,
                UserCallback, UserAcc),
            AccNext = Acc#{
                user_acc := UserAcc1,
                rows := []
            },
            {NextKey, AccNext};
        % Need to traverse at level 0 to make sure we have all keys for
        % the current group_level keys
        false ->
            UsableResults = lists:sublist(Results, length(Results) - 1),
            AccNext = Acc#{rows := Rows ++ UsableResults},
            {LastResultKey, AccNext}
    end.


% The insert algorithm
% Works as follows:
% Level 0:
%   * Always insert,
%   * if key already exists at level 0, then rereduce the two values and insert

% At level's > 0
%   * Get previous kv at level
%   * If hashCalc is true, key should be inserted at level
%   * So need to recalculate previous keys value,
%       * Get range from level below from previous key to current key
%       * Rereduce those kvs and update previous key's value
%   * Then get next key after current key at level
%       * Use that to get range from current key to next key at level below
%       * Rereduce those values to create value for current key
%   * If hashCalc is false, key is not inserted at level
%       * So rereduce previous key's value with current key's value
%         and update previous kv

add_kv_to_skip_list(Db, ReduceIdxPrefix, MaxLevel, #{} = ViewOpts, Key, Val) ->
    #{
        reducer := Reducer
    } = ViewOpts,

    Levels = lists:seq(1, MaxLevel),
    KeyHash = couch_views_util:hash_key(Key),

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        add_to_level0(TxDb, ReduceIdxPrefix, Key, Val, Reducer),

        lists:foreach(fun(Level) ->
            {PrevKey, PrevVal} = couch_views_reduce_fdb:get_previous_key(TxDb,
                ReduceIdxPrefix, Level, Key),
            case should_add_key_to_level(Level, KeyHash) of
                true ->
                    RangeOpts = #{
                        inclusive_end => false
                    },

                    update_previous_level_key(TxDb, ReduceIdxPrefix, PrevKey,
                        PrevVal, Key, Level, Reducer, RangeOpts),

                    insert_new_level_key(TxDb, ReduceIdxPrefix, Key, Level,
                        Reducer, RangeOpts);
                false ->
                    {_, PrevVal1} = couch_views_reducer:rereduce(Reducer,
                        [{PrevKey, PrevVal}, {Key, Val}], 0),
                    couch_views_reduce_fdb:add_kv(TxDb, ReduceIdxPrefix, Level,
                        PrevKey, PrevVal1)
            end
        end, Levels)
    end).


% All keys are added to level 0.
% Adding a key to level 0 involves first checking if that key already exists
% and rereduce the two k/v's together and then save.
add_to_level0(TxDb, ReduceIdxPrefix, Key, Val, Reducer) ->
    Val1 = case couch_views_reduce_fdb:get_value(TxDb, ReduceIdxPrefix, 0, Key)
    of
        not_found ->
            Val;
        ExistingVal ->
            {_, NewReducedVal} = couch_views_reducer:rereduce(Reducer,
                [{Key, ExistingVal}, {Key, Val}], group_true),
            NewReducedVal
    end,
    couch_views_reduce_fdb:add_kv(TxDb, ReduceIdxPrefix, 0, Key, Val1).


% Update previous key by looking at the lower level and
% fetching all keys from PrevKey -> Key
update_previous_level_key(TxDb, ReduceIdxPrefix, PrevKey, PrevVal, Key, Level,
    Reducer, RangeOpts) ->
    NewPrevRange = couch_views_reduce_fdb:get_level_range(TxDb, PrevKey, Key,
        Level - 1, RangeOpts, ReduceIdxPrefix),

    {_, NewPrevVal} = couch_views_reducer:rereduce(Reducer,
        NewPrevRange, 0),

    if NewPrevVal == PrevVal -> ok; true ->
        couch_views_reduce_fdb:add_kv(TxDb, ReduceIdxPrefix, Level,
            PrevKey, NewPrevVal)
    end.


% get range of values from Key to next in skiplist from level below
% update key
insert_new_level_key(TxDb, ReduceIdxPrefix, Key, Level, Reducer, RangeOpts) ->
    NextKey = couch_views_reduce_fdb:get_next(TxDb, Key, Level,
        ReduceIdxPrefix),
    KeyRange = couch_views_reduce_fdb:get_level_range(TxDb, Key, NextKey,
        Level - 1, RangeOpts, ReduceIdxPrefix),
    {_, ReducedVal} = couch_views_reducer:rereduce(Reducer, KeyRange, 0),
    couch_views_reduce_fdb:add_kv(TxDb, ReduceIdxPrefix, Level,
        Key, ReducedVal).


should_add_key_to_level(0, _KeyHash) ->
    true;

should_add_key_to_level(Level, KeyHash) ->
    LevelFanPow = skip_list_level_fan(),
    (KeyHash band ((1 bsl (Level * LevelFanPow)) -1)) == 0.


skip_list_levels() ->
    config:get_integer("couch_views", "skip_list_levels", 6).


skip_list_level_fan() ->
    config:get_integer("couch_views", "skip_list_level_fan", 4).
