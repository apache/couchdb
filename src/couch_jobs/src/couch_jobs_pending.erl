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

-module(couch_jobs_pending).


-export([
    enqueue/4,
    dequeue/4,
    remove/4,
    pending_count/4
]).


-include("couch_jobs.hrl").


-define(RANGE_LIMIT, 1024).


enqueue(#{jtx := true} = JTx, Type, STime, JobId) ->
    #{tx := Tx, jobs_path := Jobs} = JTx,
    Key = erlfdb_tuple:pack({?PENDING, Type, STime, JobId}, Jobs),
    erlfdb:set(Tx, Key, <<>>),
    WatchKey = erlfdb_tuple:pack({?WATCHES_PENDING, Type}, Jobs),
    erlfdb:add(Tx, WatchKey, 1),
    ok.


dequeue(#{jtx := true} = JTx, Type, _, true) ->
    #{tx := Tx, jobs_path := Jobs} = JTx,
    Prefix = erlfdb_tuple:pack({?PENDING, Type, 0}, Jobs),
    case get_random_item(Tx, Prefix) of
        {error, not_found} ->
            {not_found, get_pending_watch(JTx, Type)};
        {ok, PendingKey} ->
            erlfdb:clear(Tx, PendingKey),
            {JobId} = erlfdb_tuple:unpack(PendingKey, Prefix),
            {ok, JobId}
    end;

dequeue(#{jtx := true} = JTx, Type, MaxSTime, _) ->
    #{tx := Tx, jobs_path := Jobs} = JTx,
    {StartKeySel, EndKeySel} = get_range_selectors(JTx, Type, MaxSTime),
    case clear_random_key_from_range(Tx, StartKeySel, EndKeySel) of
        {error, not_found} ->
            {not_found, get_pending_watch(JTx, Type)};
        {ok, PendingKey} ->
            Prefix = erlfdb_tuple:pack({?PENDING, Type}, Jobs),
            {_, JobId} = erlfdb_tuple:unpack(PendingKey, Prefix),
            {ok, JobId}
    end.


remove(#{jtx := true} = JTx, Type, JobId, STime) ->
    #{tx := Tx, jobs_path := Jobs} = JTx,
    Key = erlfdb_tuple:pack({?PENDING, Type, STime, JobId}, Jobs),
    erlfdb:clear(Tx, Key).


pending_count(#{jtx := true} = JTx, Type, MaxSTime, Limit) ->
    #{tx := Tx} = JTx,
    Opts = [
        {limit, Limit},
        {snapshot, true},
        {streaming_mode, want_all}
    ],
    {StartSel, EndSel} = get_range_selectors(JTx, Type, MaxSTime),
    FoldFun = fun(_Row, Cnt) -> Cnt + 1 end,
    erlfdb:fold_range(Tx, StartSel, EndSel, FoldFun, 0, Opts).


%% Private functions

% Get pending key selectors, taking into account max scheduled time value.
get_range_selectors(#{jtx := true} = JTx, Type, MaxSTime) ->
    #{jobs_path := Jobs} = JTx,
    Prefix = erlfdb_tuple:pack({?PENDING, Type}, Jobs),
    StartKeySel = erlfdb_key:first_greater_than(Prefix),
    End = erlfdb_tuple:pack({MaxSTime, <<16#FF>>}, Prefix),
    EndKeySel = erlfdb_key:first_greater_or_equal(End),
    {StartKeySel, EndKeySel}.


% Pick a random item from the range without reading the keys in first. However
% the constraint it that IDs should looks like random UUIDs
get_random_item(Tx, Prefix) ->
    Id = fabric2_util:uuid(),
    Snapshot = erlfdb:snapshot(Tx),
    % Try to be fair and switch evently between trying ids before or after the
    % randomly generated one. Otherwise, trying before first, will leave a lot
    % of <<"fff...">> IDs in the queue for too long and trying "after" first
    % will leave a lot of <"00...">> ones waiting.
    case rand:uniform() > 0.5 of
        true ->
            case get_after(Snapshot, Prefix, Id) of
                {error, not_found} -> get_before(Snapshot, Prefix, Id);
                {ok, Key} -> {ok, Key}
            end;
        false ->
            case get_before(Snapshot, Prefix, Id) of
                {error, not_found} -> get_after(Snapshot, Prefix, Id);
                {ok, Key} -> {ok, Key}
            end
    end.


get_before(Snapshot, Prefix, Id) ->
    KSel = erlfdb_key:last_less_or_equal(erlfdb_tuple:pack({Id}, Prefix)),
    PrefixSize = byte_size(Prefix),
    case erlfdb:wait(erlfdb:get_key(Snapshot, KSel)) of
        <<Prefix:PrefixSize/binary, _/binary>> = Key ->  {ok, Key};
        _ -> {error, not_found}
    end.


get_after(Snapshot, Prefix, Id) ->
    KSel = erlfdb_key:first_greater_or_equal(erlfdb_tuple:pack({Id}, Prefix)),
    PrefixSize = byte_size(Prefix),
    case erlfdb:wait(erlfdb:get_key(Snapshot, KSel)) of
        <<Prefix:PrefixSize/binary, _/binary>> = Key -> {ok, Key};
        _ -> {error, not_found}
    end.


% Pick a random key from the range snapshot. Then radomly pick a key to clear.
% Before clearing, ensure there is a read conflict on the key in in case other
% workers have picked the same key.
%
clear_random_key_from_range(Tx, Start, End) ->
    Opts = [
        {limit, ?RANGE_LIMIT},
        {snapshot, true}
    ],
    case erlfdb:wait(erlfdb:get_range(Tx, Start, End, Opts)) of
        [] ->
            {error, not_found};
        [{Key, _}] ->
            erlfdb:add_read_conflict_key(Tx, Key),
            erlfdb:clear(Tx, Key),
            {ok, Key};
        [{_, _} | _] = KVs ->
            Index = rand:uniform(length(KVs)),
            {Key, _} = lists:nth(Index, KVs),
            erlfdb:add_read_conflict_key(Tx, Key),
            erlfdb:clear(Tx, Key),
            {ok, Key}
    end.


get_pending_watch(#{jtx := true} = JTx, Type) ->
    #{tx := Tx, jobs_path := Jobs} = couch_jobs_fdb:get_jtx(JTx),
    Key = erlfdb_tuple:pack({?WATCHES_PENDING, Type}, Jobs),
    erlfdb:watch(Tx, Key).
