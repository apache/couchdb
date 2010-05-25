-module(fabric_info).

-export([all_databases/1]).

-include("../../couch/src/couch_db.hrl").

%% @doc gets all databases in the cluster.
-spec all_databases(binary() | []) -> [binary()].
all_databases([]) ->
    Dbs = ets:foldl(fun({DbName, _}, Acc0) ->
        [DbName | Acc0]
    end, [], dbs_cache),
    {ok, Dbs};
all_databases(Customer) ->
    ?debugFmt("~nCustomer: ~p~n", [Customer]),
    Dbs = ets:foldl(fun({DbName, _}, Acc0) ->
        DbNameStr = ?b2l(DbName),
        case string:str(DbNameStr, Customer) of
        1 -> [DbNameStr | Acc0];
        _ -> Acc0
        end
    end, [], dbs_cache),
    {ok, Dbs}.
