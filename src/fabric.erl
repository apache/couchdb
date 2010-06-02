-module(fabric).

% DBs
-export([all_dbs/0, all_dbs/1, create_db/2, delete_db/2, get_db_info/2,
    get_doc_count/1]).

% Documents
-export([open_doc/3, open_revs/4, get_missing_revs/2, update_doc/3,
    update_docs/3]).

% Views
-export([all_docs/4]).

% miscellany
-export([db_path/2]).

-include("fabric.hrl").

% db operations
-spec db_path(bstring(), bstring()) -> bstring().
db_path(RawUri, Customer) ->
    CustomerUri = generate_customer_path(RawUri, Customer),
    {Path, _, _} = mochiweb_util:urlsplit_path(CustomerUri),
    Path.

all_dbs() ->
    fabric_all_databases:all_databases("").

all_dbs(Customer) ->
    fabric_all_databases:all_databases(Customer).

get_db_info(DbName, Customer) ->
    fabric_get_db_info:get_db_info(dbname(DbName), Customer).

get_doc_count(DbName) ->
    fabric_doc_count:go(dbname(DbName)).

create_db(DbName, Options) ->
    fabric_create_db:create_db(dbname(DbName), Options).

delete_db(DbName, Options) ->
    fabric_delete_db:delete_db(dbname(DbName), Options).



open_doc(DbName, Id, Options) ->
    fabric_open_doc:go(dbname(DbName), docid(Id), Options).

open_revs(DbName, Id, Revs, Options) ->
    fabric_open_revs:go(dbname(DbName), docid(Id), Revs, Options).

get_missing_revs(DbName, IdsRevs) when is_list(IdsRevs) ->
    Sanitized = [idrevs(IdR) || IdR <- IdsRevs],
    fabric_missing_revs:go(dbname(DbName), Sanitized).

update_doc(DbName, Doc, Options) ->
    {ok, [Result]} = update_docs(DbName, [Doc], Options),
    Result.

update_docs(DbName, Docs, Options) ->
    fabric_update_docs:go(dbname(DbName), docs(Docs), Options).


all_docs(DbName, #view_query_args{} = QueryArgs, Callback, Acc0) when
        is_function(Callback, 2) ->
    fabric_all_docs:go(dbname(DbName), QueryArgs, Callback, Acc0).

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
docs(Docs) ->
    erlang:error({illegal_docs_list, Docs}).

doc(#doc{} = Doc) ->
    Doc;
doc({_} = Doc) ->
    couch_doc:from_json_obj(Doc);
doc(Doc) ->
    erlang:error({illegal_doc_format, Doc}).

idrevs({Id, Revs}) when is_list(Revs) ->
    {docid(Id), [rev(R) || R <- Revs]}.

rev(Rev) when is_list(Rev); is_binary(Rev) ->
    couch_doc:parse_rev(Rev);
rev({Seq, Hash} = Rev) when is_integer(Seq), is_binary(Hash) ->
    Rev.

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
