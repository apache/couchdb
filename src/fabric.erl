-module(fabric).

-export([all_databases/1, create_db/2, delete_db/2, open_doc/3, open_doc/4,
         get_db_info/2]).


% db operations
all_databases(Customer) ->
    fabric_db:all_databases(Customer).

get_db_info(DbName, Customer) ->
    fabric_db:get_db_info(DbName, Customer).

create_db(DbName, Options) ->
  fabric_db:create_db(DbName, Options).

delete_db(DbName, Options) ->
    fabric_db:delete_db(DbName, Options).

% doc operations
open_doc(Db, DocId, Options) ->
    fabric_doc:open_doc(Db, DocId, Options).

open_doc(Db, DocId, Revs, Options) ->
    fabric_open:open_doc(Db, DocId, Revs, Options).
