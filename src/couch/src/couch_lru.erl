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
    {gb_trees:empty(), dict:new()}.

insert(DbName, {Tree0, Dict0}) ->
    Lru = couch_util:unique_monotonic_integer(),
    {gb_trees:insert(Lru, DbName, Tree0), dict:store(DbName, Lru, Dict0)}.

update(DbName, {Tree0, Dict0}) ->
    case dict:find(DbName, Dict0) of
    {ok, Old} ->
        New = couch_util:unique_monotonic_integer(),
        Tree = gb_trees:insert(New, DbName, gb_trees:delete(Old, Tree0)),
        Dict = dict:store(DbName, New, Dict0),
        {Tree, Dict};
    error ->
        % We closed this database before processing the update.  Ignore
        {Tree0, Dict0}
    end.

%% Attempt to close the oldest idle database.
close({Tree, _} = Cache) ->
    close_int(gb_trees:next(gb_trees:iterator(Tree)), Cache).

%% internals

close_int(none, _) ->
    false;
close_int({Lru, DbName, Iter}, {Tree, Dict} = Cache) ->
    case ets:update_element(couch_dbs, DbName, {#entry.lock, locked}) of
    true ->
        [#entry{db = Db, pid = Pid}] = ets:lookup(couch_dbs, DbName),
        case couch_db:is_idle(Db) of true ->
            true = ets:delete(couch_dbs, DbName),
            true = ets:delete(couch_dbs_pid_to_name, Pid),
            exit(Pid, kill),
            {true, {gb_trees:delete(Lru, Tree), dict:erase(DbName, Dict)}};
        false ->
            ElemSpec = {#entry.lock, unlocked},
            true = ets:update_element(couch_dbs, DbName, ElemSpec),
            couch_stats:increment_counter([couchdb, couch_server, lru_skip]),
            close_int(gb_trees:next(Iter), update(DbName, Cache))
        end;
    false ->
        NewTree = gb_trees:delete(Lru, Tree),
        NewIter = gb_trees:iterator(NewTree),
        close_int(gb_trees:next(NewIter), {NewTree, dict:erase(DbName, Dict)})
end.
