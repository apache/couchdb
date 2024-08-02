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

new() ->
    % {gb_trees(UniqueInt -> DbName), #{DbName => UniqueInt}}
    {gb_trees:empty(), #{}}.

insert(DbName, {Tree0, #{} = Dict0}) ->
    Lru = couch_util:unique_monotonic_integer(),
    {gb_trees:insert(Lru, DbName, Tree0), Dict0#{DbName => Lru}}.

update(DbName, {Tree0, #{} = Dict0}) ->
    case Dict0 of
        #{DbName := Old} ->
            New = couch_util:unique_monotonic_integer(),
            Tree = gb_trees:insert(New, DbName, gb_trees:delete(Old, Tree0)),
            {Tree, Dict0#{DbName := New}};
        #{} ->
            {Tree0, Dict0}
    end.

%% Attempt to close the oldest idle database.
close({Tree, #{}} = Cache) ->
    close_int(gb_trees:smallest(Tree), Cache).

%% internals

% none is returned by the gb_trees iterator when it reaches the end
%
close_int(none, {_Tree, #{}} = _Cache) ->
    false;
close_int({Lru, DbName}, {Tree, #{} = Dict} = Cache) ->
    CouchDbs = couch_server:couch_dbs(DbName),
    CouchDbsPidToName = couch_server:couch_dbs_pid_to_name(DbName),

    case couch_server:try_lock(CouchDbs, DbName) of
        {ok, #entry{db = Db, pid = Pid}} ->
            case couch_db:is_idle(Db) of
                true ->
                    true = ets:delete(CouchDbs, DbName),
                    true = ets:delete(CouchDbsPidToName, Pid),
                    exit(Pid, kill),
                    {true, {gb_trees:delete(Lru, Tree), maps:remove(DbName, Dict)}};
                false ->
                    true = couch_server:unlock(CouchDbs, DbName),
                    couch_stats:increment_counter([couchdb, couch_server, lru_skip]),
                    NewCache = {NewTree, _} = update(DbName, Cache),
                    close_int(larger(Lru, NewTree), NewCache)
            end;
        false ->
            NewTree = gb_trees:delete(Lru, Tree),
            close_int(larger(Lru, NewTree), {NewTree, maps:remove(DbName, Dict)})
    end.

% Compat function. In OTP 27 can use gb_tree:larger/2
%
larger(Key, Tree) ->
    gb_trees:next(gb_trees:iterator_from(Key, Tree)).
