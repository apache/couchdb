-module(fabric_all_databases).

-export([all_databases/1]).

-include("fabric.hrl").
-include_lib("mem3/include/mem3.hrl").

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
        DbNameStr = binary_to_list(DbName),
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
