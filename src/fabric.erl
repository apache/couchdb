-module(fabric).

-export([create_db/2]).

create_db(DbName, Options) ->
  fabric_create:create_db(DbName, Options).
