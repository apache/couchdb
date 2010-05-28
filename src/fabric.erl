-module(fabric).

-export([all_databases/1, create_db/2, delete_db/2, get_db_info/2,
	db_path/2]).
-export([open_doc/3, open_revs/4, update_docs/3]).

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
    fabric_db:get_db_info(dbname(DbName), Customer).

create_db(DbName, Options) ->
    fabric_db:create_db(dbname(DbName), Options).

delete_db(DbName, Options) ->
    fabric_db:delete_db(dbname(DbName), Options).



open_doc(DbName, Id, Options) ->
    fabric_doc:open_doc(dbname(DbName), docid(Id), Options).

open_revs(DbName, Id, Revs, Options) ->
    fabric_doc:open_revs(dbname(DbName), docid(Id), Revs, Options).

update_docs(DbName, Docs, Options) ->
    fabric_doc:update_docs(dbname(DbName), docs(Docs), Options).


%% some simple type validation and transcoding

dbname(DbName) when is_list(DbName) ->
    list_to_binary(DbName);
dbname(DbName) when is_binary(DbName) ->
    DbName;
dbname(DbName) ->
    erlang:error({illegal_database_name, DbName}).

docid(DocId) when is_list(DocId) ->
    list_to_binary(DocId);
docid(DocId) when is_binary(DocId) ->
    DocId;
docid(DocId) ->
    erlang:error({illegal_docid, DocId}).

docs(Docs) when is_list(Docs) ->
    [doc(D) || D <- Docs];
docs(#doc{} = Doc) ->
    [Doc];
docs({_} = Doc) ->
    [couch_doc:from_json_obj(Doc)];
docs(Docs) ->
    erlang:error({illegal_docs_list, Docs}).

doc(#doc{} = Doc) ->
    Doc;
doc({_} = Doc) ->
    couch_doc:from_json_obj(Doc);
doc(Doc) ->
    erlang:error({illegal_doc_format, Doc}).

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
