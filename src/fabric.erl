-module(fabric).

-export([all_databases/1, create_db/2, delete_db/2, open_db/2, open_doc/4,
         get_db_info/2]).

%% maybe this goes away, and these are called directly in their own modules in
%% fabric_api ??

all_databases(Customer) ->
    fabric_info:all_databases(Customer).

create_db(DbName, Options) ->
  fabric_create:create_db(DbName, Options).

delete_db(DbName, Options) ->
    fabric_delete:delete_db(DbName, Options).

open_db(DbName, Options) ->
    fabric_open:open_db(DbName, Options).

open_doc(Db, DocId, Revs, Options) ->
    fabric_open:open_doc(Db, DocId, Revs, Options).

get_db_info(DbName, Customer) ->
    fabric_info:get_db_info(DbName, Customer).
