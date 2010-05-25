-module(fabric).

-export([all_databases/1, create_db/2]).


all_databases(Customer) ->
    fabric_info:all_databases(Customer).

create_db(DbName, Options) ->
  fabric_create:create_db(DbName, Options).
