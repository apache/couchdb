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

-module(couch_lru).
-export([new/0, insert/2, update/2, close/1]).

-include("couch_server_int.hrl").

-type cache() :: {gb_trees:tree(), #{binary() => pos_integer()}}.

-spec new() -> cache().
new() ->
    {gb_trees:empty(), #{}}.

-spec insert(binary(), cache()) -> cache().
insert(DbName, {Tree, #{} = Map} = Cache) ->
    case Map of
        #{DbName := Old} ->
            update_int(Old, DbName, Cache);
        #{} ->
            New = couch_util:unique_monotonic_integer(),
            {gb_trees:insert(New, DbName, Tree), Map#{DbName => New}}
    end.

%% Update bumps the entry but only if it already exists. If it doesn't exist,
%% it won't be inserted.

-spec update(binary(), cache()) -> cache().
update(DbName, {Tree, #{} = Map} = Cache) ->
    case Map of
        #{DbName := Old} ->
            update_int(Old, DbName, Cache);
        #{} ->
            % We closed this database before processing the update.  Ignore
            {Tree, Map}
    end.

%% Attempt to close the oldest idle database. This function also cleans deleted
%% and locked entries from the Lru and also bumps busy entries until the first
%% idle entry is found. If no entry is found, it returns `false`. In that case
%% all bumped entries are lost as heap garbage.

-spec close(cache()) -> {true, cache()} | false.
close({Tree, _} = Cache) ->
    close_int(gb_trees:next(gb_trees:iterator(Tree)), Cache).

%% internals

update_int(Old, DbName, {Tree, #{} = Map}) ->
    New = couch_util:unique_monotonic_integer(),
    Tree1 = gb_trees:insert(New, DbName, gb_trees:delete(Old, Tree)),
    {Tree1, Map#{DbName := New}}.

close_int(none, {_Tree, #{}}) ->
    false;
close_int({Lru, DbName, Iter}, {Tree, #{} = Map} = Cache) ->
    CouchDbs = couch_server:couch_dbs(DbName),
    CouchDbsPidToName = couch_server:couch_dbs_pid_to_name(DbName),

    case couch_server:try_lock(CouchDbs, DbName) of
        {ok, #entry{db = Db, pid = Pid}} ->
            case couch_db:is_idle(Db) of
                true ->
                    true = ets:delete(CouchDbs, DbName),
                    true = ets:delete(CouchDbsPidToName, Pid),
                    exit(Pid, kill),
                    {true, {gb_trees:delete(Lru, Tree), maps:remove(DbName, Map)}};
                false ->
                    true = couch_server:unlock(CouchDbs, DbName),
                    couch_stats:increment_counter([couchdb, couch_server, lru_skip]),
                    close_int(gb_trees:next(Iter), update(DbName, Cache))
            end;
        false ->
            NewTree = gb_trees:delete(Lru, Tree),
            NewIter = gb_trees:iterator(NewTree),
            close_int(gb_trees:next(NewIter), {NewTree, maps:remove(DbName, Map)})
    end.

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

-define(DB1, <<"db1">>).
-define(DB2, <<"db2">>).

couch_lru_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_new),
            ?TDEF_FE(t_insert),
            ?TDEF_FE(t_insert_duplicate),
            ?TDEF_FE(t_update),
            ?TDEF_FE(t_update_non_existent),
            ?TDEF_FE(t_close_empty),
            ?TDEF_FE(t_close_unlocked_idle),
            ?TDEF_FE(t_close_bump_busy_all),
            ?TDEF_FE(t_close_bump_busy_one),
            ?TDEF_FE(t_close_entry_one_is_missing),
            ?TDEF_FE(t_multiple_inserts_and_close)
        ]
    }.

t_new(_) ->
    Cache = new(),
    ?assertMatch({_, _}, Cache),
    {Tree, Map} = Cache,
    ?assert(gb_trees:is_empty(Tree)),
    ?assert(is_map(Map) andalso map_size(Map) == 0).

t_insert(_) ->
    {Tree, Map} = insert(?DB1, new()),
    ?assertEqual(1, gb_trees:size(Tree)),
    ?assertEqual(1, map_size(Map)),
    ?assertMatch(#{?DB1 := _}, Map),
    #{?DB1 := Int} = Map,
    ?assert(is_integer(Int)),
    ?assert(Int > 0),
    ?assertEqual([{Int, ?DB1}], gb_trees:to_list(Tree)).

t_insert_duplicate(_) ->
    % We technically allow this, but is this right? Should we always use update
    % instead which would reap the old LRU entry
    %
    {Tree, Map} = insert(?DB1, insert(?DB1, new())),
    ?assertEqual(1, gb_trees:size(Tree)),
    ?assertEqual(1, map_size(Map)),
    ?assertMatch(#{?DB1 := _}, Map),
    ?assertMatch([{_, ?DB1}], gb_trees:to_list(Tree)).

t_update(_) ->
    % Insert followed by update.
    {Tree, Map} = update(?DB1, insert(?DB1, new())),
    ?assertEqual(1, gb_trees:size(Tree)),
    ?assertEqual(1, map_size(Map)),
    ?assertMatch(#{?DB1 := _}, Map),
    #{?DB1 := Int} = Map,
    ?assertEqual([{Int, ?DB1}], gb_trees:to_list(Tree)).

t_update_non_existent(_) ->
    % Updating a non-existent item is a no-op
    {Tree, Map} = update(?DB2, insert(?DB1, new())),
    ?assertEqual(1, gb_trees:size(Tree)),
    ?assertEqual(1, map_size(Map)),
    ?assertMatch(#{?DB1 := _}, Map),
    #{?DB1 := Int} = Map,
    ?assertEqual([{Int, ?DB1}], gb_trees:to_list(Tree)).

t_close_empty(_) ->
    ?assertEqual(false, close(new())).

t_close_unlocked_idle({Dbs, DbsPids, [Pid1, _]}) ->
    % There is one db handle and it's idle. It should get closed.
    Cache = insert(?DB1, new()),
    Res = close(Cache),
    ?assertMatch({true, {_, #{}}}, Res),
    {true, {Tree, Map}} = Res,
    ?assert(gb_trees:is_empty(Tree)),
    ?assert(is_map(Map) andalso map_size(Map) == 0),
    ?assertNot(is_process_alive(Pid1)),
    ?assertEqual([], ets:lookup(Dbs, ?DB1)),
    ?assertEqual([], ets:lookup(DbsPids, Pid1)).

t_close_bump_busy_all({_Dbs, _DbsPids, [Pid1, _]}) ->
    % There is one db handle and it's busy. We should stay up.
    Cache = insert(?DB1, new()),
    meck:expect(couch_db, is_idle, 1, false),
    % Yeah, it is odd that we're throwing away the updated cache by returning
    % just false if we don't end up finding an idle Db.
    meck:reset(couch_stats),
    ?assertEqual(false, close(Cache)),
    ?assertEqual(1, meck:num_calls(couch_stats, increment_counter, 1)),
    ?assert(is_process_alive(Pid1)).

t_close_bump_busy_one({Dbs, DbsPids, [Pid1, Pid2]}) ->
    % One busy handle gets bumped, the idle one closed.
    {_, Map} = Cache = insert(?DB2, insert(?DB1, new())),
    meck:expect(couch_db, is_idle, fun
        (?DB1) -> false;
        (?DB2) -> true
    end),
    meck:reset(couch_stats),
    {true, {Tree1, Map1}} = close(Cache),
    ?assert(is_process_alive(Pid1)),
    ?assertNot(is_process_alive(Pid2)),
    ?assertEqual(1, ets:info(Dbs, size)),
    ?assertEqual(1, ets:info(DbsPids, size)),
    ?assertEqual(1, meck:num_calls(couch_stats, increment_counter, 1)),
    ?assert(is_process_alive(Pid1)),
    ?assertEqual(1, gb_trees:size(Tree1)),
    ?assertEqual(1, map_size(Map1)),
    % The ?DB1 entry should have been bumped
    #{?DB1 := OldInt} = Map,
    #{?DB1 := NewInt} = Map1,
    ?assert(NewInt > OldInt),
    ?assertEqual([{NewInt, ?DB1}], gb_trees:to_list(Tree1)).

t_close_entry_one_is_missing({Dbs, _, [_Pid1, Pid2]}) ->
    % Two entries but one is missing from db ets
    % it should be auto-removed from LRU
    Cache = insert(?DB2, insert(?DB1, new())),
    ets:delete(Dbs, ?DB1),
    {true, {Tree1, Map1}} = close(Cache),
    % One entry was removed, one was closed. There should be
    % nothing left in the LRU.
    ?assert(gb_trees:is_empty(Tree1)),
    ?assert(is_map(Map1)),
    ?assertEqual(0, map_size(Map1)),
    ?assertNot(is_process_alive(Pid2)).

t_multiple_inserts_and_close(_) ->
    % Insert same entry twice, then close. Previously insert had a bug where a
    % double insert would add two tree entries for a single map entry. Then,
    % during the close traversal, if one instances was busy and the other idle,
    % we'd crash with function clause in a function clause in gb_trees:delete/2
    % (See issue #5166 for details)
    {_, Map} = Cache = insert(?DB1, insert(?DB1, new())),
    meck:expect(couch_db, is_idle, 1, meck:seq([meck:val(false), meck:val(true)])),
    ?assertEqual(false, close(Cache)).

setup() ->
    Pid1 = spawn(fun() -> timer:sleep(9999999) end),
    Pid2 = spawn(fun() -> timer:sleep(9999999) end),
    Dbs = ets:new(unique_name(), [named_table, public, {keypos, #entry.name}]),
    DbsPids = ets:new(unique_name(), [named_table, public]),
    ets:insert(Dbs, #entry{name = ?DB1, db = ?DB1, pid = Pid1, lock = unlocked}),
    ets:insert(Dbs, #entry{name = ?DB2, db = ?DB2, pid = Pid2, lock = unlocked}),
    ets:insert(DbsPids, {Pid1, ?DB1}),
    ets:insert(DbsPids, {Pid2, ?DB2}),
    meck:new(couch_server, [passthrough]),
    meck:expect(couch_server, couch_dbs, fun(_) -> Dbs end),
    meck:expect(couch_server, couch_dbs_pid_to_name, fun(_) -> DbsPids end),
    meck:expect(couch_db, is_idle, 1, true),
    meck:expect(couch_stats, increment_counter, 1, ok),
    {Dbs, DbsPids, [Pid1, Pid2]}.

teardown({Dbs, DbsPids, [Pid1, Pid2]}) ->
    ets:delete(Dbs),
    ets:delete(DbsPids),
    exit(Pid1, kill),
    exit(Pid2, kill),
    meck:unload().

unique_name() ->
    Mod = atom_to_list(?MODULE),
    Unique = erlang:unique_integer([positive, monotonic]),
    list_to_atom(Mod ++ "_" ++ integer_to_list(Unique)).

-endif.
