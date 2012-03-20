-module(couch_lru).
-export([new/0, insert/2, update/2, close/1]).

-include_lib("couch/include/couch_db.hrl").

new() ->
    {gb_trees:empty(), dict:new()}.

insert(DbName, {Tree0, Dict0}) ->
    Lru = now(),
    {gb_trees:insert(Lru, DbName, Tree0), dict:store(DbName, Lru, Dict0)}.

update(DbName, {Tree0, Dict0}) ->
    case dict:find(DbName, Dict0) of
    {ok, Old} ->
        New = now(),
        Tree = gb_trees:insert(New, DbName, gb_trees:delete(Old, Tree0)),
        Dict = dict:store(DbName, New, Dict0),
        {Tree, Dict};
    error ->
        % We closed this database before processing the update.  Ignore
        {Tree0, Dict0}
    end.

close({Tree, _} = Cache) ->
    close_int(gb_trees:next(gb_trees:iterator(Tree)), Cache).

%% internals

close_int(none, _) ->
    erlang:error(all_dbs_active);
close_int({Lru, DbName, Iter}, {Tree, Dict} = Cache) ->
    case ets:update_element(couch_dbs, DbName, {#db.fd_monitor, locked}) of
    true ->
        [#db{main_pid = Pid} = Db] = ets:lookup(couch_dbs, DbName),
        case couch_db:is_idle(Db) of true ->
            true = ets:delete(couch_dbs, DbName),
            exit(Pid, kill),
            {gb_trees:delete(Lru, Tree), dict:erase(DbName, Dict)};
        false ->
            true = ets:update_element(couch_dbs, DbName, {#db.fd_monitor, nil}),
            close_int(gb_trees:next(Iter), update(DbName, Cache))
        end;
    false ->
        NewTree = gb_trees:delete(Lru, Tree),
        NewIter = gb_trees:iterator(NewTree),
        close_int(gb_trees:next(NewIter), {NewTree, dict:erase(DbName, Dict)})
    end.
