-module(fabric_all_databases).
-author(brad@cloudant.com).

-export([all_databases/1]).

-include("fabric.hrl").

%% @doc gets all databases in the cluster.
-spec all_databases(binary() | []) -> [binary()].
all_databases([]) ->
    Dbs = ets:foldl(fun(#shard{dbname=DbName}, AccIn) ->
        new_acc(DbName, AccIn)
    end, [], partitions),
    {ok, Dbs};
all_databases(Customer) ->
    ?debugFmt("~nCustomer: ~p~n", [Customer]),
    Dbs = ets:foldl(fun(#shard{dbname=DbName}, AccIn) ->
        DbNameStr = ?b2l(DbName),
        case string:str(DbNameStr, Customer) of
        1 ->
            new_acc(DbNameStr, AccIn);
        _ -> AccIn
        end
    end, [], dbs_cache),
    {ok, Dbs}.


%% =====================
%%   internal
%% =====================

new_acc(DbName, Acc) ->
    case lists:member(DbName, Acc) of
    true -> Acc;
    _ ->[DbName | Acc]
    end.
