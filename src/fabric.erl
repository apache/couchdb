-module(fabric).

-export([all_databases/1, create_db/2, delete_db/2, open_doc/3, open_doc/4,
         get_db_info/2, db_path/2]).

-include("../../couch/src/couch_db.hrl").


% db operations
-spec db_path(bstring(), bstring()) -> bstring().
db_path(RawUri, Customer) ->
    CustomerUri = generate_customer_path(RawUri, Customer),
    {Path, _, _} = mochiweb_util:urlsplit_path(CustomerUri),
    Path.

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
    fabric_doc:open_doc(Db, DocId, Revs, Options).



%%
%% internal
%%
generate_customer_path("/", _Customer) ->
    "";
generate_customer_path("/favicon.ico", _Customer) ->
    "favicon.ico";
generate_customer_path([$/,$_|Rest], _Customer) ->
    lists:flatten([$_|Rest]);
generate_customer_path([$/|RawPath], Customer) ->
    case Customer of
    "" ->
        RawPath;
    Else ->
        lists:flatten([Else, "%2F", RawPath])
    end.
