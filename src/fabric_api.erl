%% This is a raw Erlang API for CouchDB in a Cloudant cluster
%% It makes use of clustering facilities

-module(fabric_api).
-author('adam@cloudant.com').
-author('brad@cloudant.com').

-include("../../couch/src/couch_db.hrl").

-compile(export_all).

-type response() :: any().

%% dialyzer doesn't have recursive types, so this is necessarily wrong
-type ejson_value() :: true | false | number() | bstring() | list().
-type ejson() :: {[{bstring(), ejson_value()}]}.

%% Database

-spec db_path(bstring(), bstring()) -> bstring().
db_path(RawUri, Customer) ->
    showroom_db:db_path(RawUri, Customer).

-spec all_databases(string()) -> {ok, [bstring()]}.
all_databases(Customer) ->
    fabric:all_databases(Customer).

-spec create_db(bstring(), [any()]) -> {ok, #db{}} | {error, any()}.
create_db(DbName, Options) ->
    fabric:create_db(DbName, Options).

-spec delete_db(bstring(), [any()]) -> ok | not_found | {error, atom()}.
delete_db(DbName, Options) ->
    fabric:delete_db(DbName, Options).

-spec open_db(bstring(), [any()]) -> {ok, #db{}} | {error, any()}.
open_db(DbName, Options) ->
    fabric:open_db(DbName, Options).

-spec close_db(#db{}) -> ok.
close_db(Db) ->
    showroom_db:close_db(Db).

-spec get_db_info(#db{}, bstring()) -> {ok, [{atom(), any()}]}.
get_db_info(Db, Customer) ->
    showroom_db:get_db_info(Db, Customer).

-spec replicate_db(ejson(), #user_ctx{}) -> {ok, ejson()}.
replicate_db(PostBody, UserCtx) ->
    showroom_rep:replicate(PostBody, UserCtx).

-spec ensure_full_commit(#db{}) -> {ok, InstanceStartTime::bstring()}.
ensure_full_commit(Db) ->
    showroom_db:ensure_full_commit(Db),
    {ok, <<"0">>}.


%% Document

att_receiver(Req, Length) ->
    showroom_att:receiver(Req, Length).

-spec get_missing_revs(#db{}, [{docid(), [revision()]}]) ->
    {ok, [{docid(), [revision()]}]}.
get_missing_revs(Db, IdsRevs) ->
    showroom_doc:get_missing_revs(Db, IdsRevs).

open_doc(DbName, DocId) ->
    open_doc(DbName, DocId, nil, []).

-spec open_doc(bstring(), docid(), [any()]) ->
    {ok, #doc{}} | {not_found, deleted | missing}.
open_doc(DbName, DocId, Options) ->
    open_doc(DbName, DocId, nil, Options).

open_doc(DbName, DocId, Revs, Options) ->
    fabric:open_doc(DbName, DocId, Revs, Options).

-spec open_doc_revs(bstring(), docid(), [revision()], [any()]) ->
    {ok, [{ok, #doc{}}
    | {{not_found, deleted | missing}, revision()}]}.
open_doc_revs(DbName, DocId, Revs, Options) ->
    open_doc(DbName, DocId, Revs, Options).

update_doc(Db, Doc) ->
    update_doc(Db, Doc, []).

-spec update_doc(#db{}, #doc{}, [any()]) -> {ok, revision()}.
update_doc(Db, Doc, Options) ->
    showroom_doc:update_doc(Db, Doc, Options).

update_docs(Db, Docs, Options) ->
    update_docs(Db, Docs, Options, interactive_edit).

-spec update_docs(#db{}, [#doc{}], [any()], interactive_edit |
    replicated_changes) -> {ok, [{ok, revision()}]}.
update_docs(Db, Docs, Options, Type) ->
    showroom_doc:update_docs(Db, Docs, Options, Type).


%% View

-spec all_docs_view(response(), #db{}, nil | list(), #view_query_args{}) ->
    {ok, any()}.
all_docs_view(Resp, Db, Keys, QueryArgs) ->
    showroom_view:all_docs(Resp, Db, Keys, QueryArgs).

-spec design_view(response(), #db{}, bstring(), bstring(), nil | list(),
    #view_query_args{}) -> any().
design_view(Resp, Db, Id, Name, Keys, QueryArgs) ->
    showroom_view:design(Resp, Db, Id, Name, Keys, QueryArgs).

list_view(Req, Db, DesignId, ViewName, Keys, QueryArgs, QueryServer) ->
    showroom_view:list(Req, Db, DesignId, ViewName, Keys, QueryArgs, QueryServer).

-spec get_view_group_info(#db{}, bstring()) -> {ok, [{atom(), any()}]}.
get_view_group_info(Db, DesignId) ->
    showroom_view:group_info(Db, DesignId).
