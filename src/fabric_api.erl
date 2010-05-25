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
    showroom_db:all_databases(Customer).

-spec create_db(bstring(), [any()]) -> {ok, #db{}} | {error, any()}.
create_db(DbName, Options) ->
    fabric:create_db(DbName, Options).

-spec delete_db(bstring(), [any()]) -> ok | not_found | {error, atom()}.
delete_db(DbName, Options) ->
    showroom_db:delete_db(DbName, Options).

-spec open_db(bstring(), [any()]) -> {ok, #db{}} | {error, any()}.
open_db(DbName, Options) ->
    showroom_db:open_db(DbName, Options).

-spec close_db(#db{}) -> ok.
close_db(Db) ->
    showroom_db:close_db(Db).

-spec get_db_info(#db{}, bstring()) -> {ok, [{atom(), any()}]}.
get_db_info(Db, Customer) ->
    showroom_db:get_db_info(Db, Customer).

-spec get_committed_update_seq(#db{}) -> update_seq().
get_committed_update_seq(_Db) ->
%%     couch_db:get_committed_update_seq(Db).
    not_implemented.

-spec get_purge_seq(#db{}) -> update_seq().
get_purge_seq(_Db) ->
%%     couch_db:get_purge_seq(Db).
    not_implemented.

-spec get_update_seq(#db{}) -> update_seq().
get_update_seq(_Db) ->
%%     couch_db:get_update_seq(Db).
    not_implemented.

-spec compact_db(#db{}) -> ok.
compact_db(_Db) ->
%%     couch_db:start_compact(Db).
    not_implemented.

-spec replicate_db(ejson(), #user_ctx{}) -> {ok, ejson()}.
replicate_db(PostBody, UserCtx) ->
    showroom_rep:replicate(PostBody, UserCtx).

-spec ensure_full_commit(#db{}) -> {ok, InstanceStartTime::bstring()}.
ensure_full_commit(Db) ->
    showroom_db:ensure_full_commit(Db),
    {ok, <<"0">>}.

-spec increment_update_seq(#db{}) -> {ok, update_seq()}.
increment_update_seq(_Db) ->
%%     couch_db:increment_update_seq(Db).
    not_implemented.

-spec get_admins(#db{}) -> [any()].
get_admins(_Db) ->
%%     couch_db:get_admins(Db).
    not_implemented.

-spec set_admins(#db{}, [any()]) -> ok.
set_admins(_Db, _Admins) ->
%%     couch_db:set_admins(Db, Admins).
    not_implemented.

-spec get_revs_limit(#db{}) -> pos_integer().
get_revs_limit(_Db) ->
%%     couch_db:get_revs_limit(Db).
    not_implemented.

-spec set_revs_limit(#db{}, pos_integer()) -> ok.
set_revs_limit(_Db, _Limit) ->
%%     couch_db:set_revs_limit(Db, Limit).
    not_implemented.


%% Document

att_receiver(Req, Length) ->
    showroom_att:receiver(Req, Length).

-spec changes_since(#db{}, main_only | all_docs, update_seq(),
    fun(([#doc_info{}], Acc) -> {ok, Acc}), Acc) -> {ok, Acc}.
changes_since(_Db, _Style, _StartSeq, _Fun, _Acc0) ->
%%     couch_db:changes_since(Db, Style, StartSeq, Fun, Acc0).
    not_implemented.

-spec enum_docs(#db{}, docid(), fwd | rev, fun((#full_doc_info{},
    Offset::integer(), Acc) -> {ok|stop, Acc}), Acc) -> {ok, Acc}.
enum_docs(_Db, _StartId, _Dir, _Fun, _Acc0) ->
%%     couch_db:enum_docs(Db, StartId, Dir, Fun, Acc0).
    not_implemented.

-spec enum_docs_reduce_to_count({any(), any()}) -> non_neg_integer().
enum_docs_reduce_to_count(_Reductions) ->
%%     couch_db:enum_docs_reduce_to_count(Reductions).
    not_implemented.

-spec enum_docs_since(#db{}, update_seq(), fwd | rev, fun((#doc_info{},
    Offset::integer(), Acc) -> {ok|stop, Acc}), Acc) -> {ok, Acc}.
enum_docs_since(_Db, _StartKey, _Dir, _Fun, _Acc0) ->
%%     couch_db:enum_docs_since(Db, StartKey, Dir, Fun, Acc0).
    not_implemented.

-spec enum_docs_since_reduce_to_count({any(), any()}) -> non_neg_integer().
enum_docs_since_reduce_to_count(_Reductions) ->
%%     couch_db:enum_docs_since_reduce_to_count(Reductions).
    not_implemented.

-spec get_doc_info(#db{}, docid()) -> {ok, #doc_info{}}.
get_doc_info(_Db, _DocId) ->
%%     couch_db:get_doc_info(Db, DocId).
    not_implemented.

-spec get_missing_revs(#db{}, [{docid(), [revision()]}]) ->
    {ok, [{docid(), [revision()]}]}.
get_missing_revs(Db, IdsRevs) ->
    showroom_doc:get_missing_revs(Db, IdsRevs).

open_doc(Db, DocId) ->
    open_doc(Db, DocId, nil, []).

-spec open_doc(#db{}, docid(), [any()]) -> {ok, #doc{}} | {not_found, deleted |
    missing}.
open_doc(Db, DocId, Options) ->
    open_doc(Db, DocId, nil, Options).

open_doc(Db, DocId, Revs, Options) ->
    showroom_doc:open_doc(Db, DocId, Revs, Options).

-spec open_doc_revs(#db{}, docid(), [revision()], [any()]) -> {ok, [{ok, #doc{}}
    | {{not_found, deleted | missing}, revision()}]}.
open_doc_revs(Db, DocId, Revs, Options) ->
    open_doc(Db, DocId, Revs, Options).

-spec purge_docs(#db{}, [{docid(), [revision()]}]) ->
    {ok, update_seq(), [{docid(),[revision()]} | {error,purge_during_compaction}]}.
purge_docs(_Db, _IdsRevs) ->
%%     couch_db:purge_docs(Db, IdsRevs).
    not_implemented.

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

-spec compact_view_group(#db{}, bstring()) -> ok.
compact_view_group(_Db, _DesignId) ->
%%     couch_view_compactor:start_compact(Db, DesignId).
    not_implemented.

-spec cleanup_view_index_files(#db{}) -> any().
cleanup_view_index_files(_Db) ->
%%     couch_view:cleanup_index_files(Db).
    not_implemented.

-spec design_view(response(), #db{}, bstring(), bstring(), nil | list(),
    #view_query_args{}) -> any().
design_view(Resp, Db, Id, Name, Keys, QueryArgs) ->
    showroom_view:design(Resp, Db, Id, Name, Keys, QueryArgs).

list_view(Req, Db, DesignId, ViewName, Keys, QueryArgs, QueryServer) ->
    showroom_view:list(Req, Db, DesignId, ViewName, Keys, QueryArgs, QueryServer).

-spec extract_map_view({reduce, any(), bstring(), #view{}}) -> {ok, #view{}}.
extract_map_view(_ReduceView) ->
%%     couch_view:extract_map_view(ReduceView).
    not_implemented.

-spec get_map_view(#db{}, bstring(), bstring(), true | false) -> any().
get_map_view(_Db, _DesignId, _ViewName, _Stale) ->
%%     couch_view:get_map_view(Db, DesignId, ViewName, Stale).
    not_implemented.

-spec get_reduce_view(#db{}, bstring(), bstring(), true | false) -> any().
get_reduce_view(_Db, _DesignId, _ViewName, _Stale) ->
%%     couch_view:get_reduce_view(Db, DesignId, ViewName, Stale).
    not_implemented.

-spec get_row_count(#view{}) -> {ok, non_neg_integer()}.
get_row_count(_View) ->
%%     couch_view:get_row_count(View).
    not_implemented.

-spec get_temp_map_view(#db{}, bstring(), [any()], bstring()) -> {ok, #view{},
    #group{}}.
get_temp_map_view(_Db, _Language, _DesignOptions, _MapSrc) ->
%%     couch_view:get_temp_map_view(Db, Language, DesignOptions, MapSrc).
    not_implemented.

-spec get_temp_reduce_view(#db{}, bstring(), [any()], bstring(), bstring()) ->
    {ok, {temp_reduce, #view{}}, #group{}}.
get_temp_reduce_view(_Db, _Language, _DesignOptions, _MapSrc, _RedSrc) ->
%%     couch_view:get_temp_reduce_view(Db, Language, DesignOptions, MapSrc, RedSrc).
    not_implemented.

-spec get_view_group_info(#db{}, bstring()) -> {ok, [{atom(), any()}]}.
get_view_group_info(Db, DesignId) ->
    showroom_view:group_info(Db, DesignId).

-spec reduce_to_count({any(), any()}) -> non_neg_integer().
reduce_to_count(_Reductions) ->
%%     couch_view:reduce_to_count(Reductions).
    not_implemented.

-spec view_fold(#view{}, any(), fwd | rev, fun(({{Key::any(), docid()},
    Value::any()}, OffsetReds::any(), Acc) -> {ok|stop, Acc}), Acc) -> {ok, Acc}.
view_fold(_View, _Start, _Dir, _Fun, _Acc0) ->
%%     couch_view:fold(View, Start, Dir, Fun, Acc0).
    not_implemented.

-spec view_fold_reduce({reduce | temp_reduce, #view{}}, fwd | rev, any(), any(),
    fun(({Key1::any(), any()}, {Key2::any(), any()}) -> boolean()),
    fun(({Key::any(), Reduction::any(), Acc}) -> {ok|stop, Acc}), Acc) ->
    {ok, Acc}.
view_fold_reduce(_View, _Dir, _Start, _End, _GroupFun, _ResponseFun, _Acc0) ->
%%     couch_view:fold_reduce(View, Dir, Start, End, GroupFun, ResponseFun, Acc0).
    not_implemented.
