% Copyright 2013 Cloudant. All rights reserved.

-module(global_changes_util).


-export([get_dbname/0]).


get_dbname() ->
    case application:get_env(global_changes, dbname) of
        {ok, DbName} when is_binary(DbName) ->
            DbName;
        {ok, DbName} when is_list(DbName) ->
            iolist_to_binary(DbName);
        _ ->
            <<"global_changes">>
    end.
