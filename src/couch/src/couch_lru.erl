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
    Updates = ets:new(couch_lru_updates, [ordered_set]),
    Dbs = ets:new(couch_lru_dbs, [set]),
    {0, Updates, Dbs}.

insert(DbName, {Count, Updates, Dbs}) ->
    update(DbName, {Count, Updates, Dbs}).

update(DbName, {Count, Updates, Dbs}) ->
    case ets:lookup(Dbs, DbName) of
        [] ->
            true = ets:insert(Dbs, {DbName, Count});
        [{DbName, OldCount}] ->
            true = ets:update_element(Dbs, DbName, {2, Count}),
            true = ets:delete(Updates, {OldCount, DbName})
    end,
    true = ets:insert(Updates, {{Count, DbName}}),
    {Count + 1, Updates, Dbs}.


close({Count, Updates, Dbs}) ->
    case close_int(ets:next(Updates, {-1, <<>>}), Updates, Dbs) of
        true ->
            {true, {Count, Updates, Dbs}};
        false ->
            false
    end.


%% internals

close_int('$end_of_table', _Updates, _Dbs) ->
    false;
close_int({_Count, DbName} = Key, Updates, Dbs) ->
    case ets:update_element(couch_dbs, DbName, {#entry.lock, locked}) of
    true ->
        [#entry{db = Db, pid = DbPid}] = ets:lookup(couch_dbs, DbName),
        case couch_db:is_idle(Db) of true ->
            true = ets:delete(couch_dbs, DbName),
            true = ets:delete(couch_dbs_pid_to_name, DbPid),
            exit(DbPid, kill),
            true = ets:delete(Updates, Key),
            true = ets:delete(Dbs, DbName),
            true;
        false ->
            ElemSpec = {#entry.lock, unlocked},
            true = ets:update_element(couch_dbs, DbName, ElemSpec),
            couch_stats:increment_counter([couchdb, couch_server, lru_skip]),
            close_int(ets:next(Updates, Key), Updates, Dbs)
        end;
    false ->
        true = ets:delete(Updates, Key),
        true = ets:delete(Dbs, DbName),
        close_int(ets:next(Updates, Key), Updates, Dbs)
    end.
