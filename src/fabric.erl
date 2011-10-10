% Copyright 2010 Cloudant
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(fabric).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(ADMIN_CTX, {user_ctx, #user_ctx{roles = [<<"_admin">>]}}).

% DBs
-export([all_dbs/0, all_dbs/1, create_db/1, create_db/2, delete_db/1,
    delete_db/2, get_db_info/1, get_doc_count/1, set_revs_limit/3,
    set_security/3, get_revs_limit/1, get_security/1, get_security/2]).

% Documents
-export([open_doc/3, open_revs/4, get_missing_revs/2, get_missing_revs/3,
    update_doc/3, update_docs/3, purge_docs/2, att_receiver/2]).

% Views
-export([all_docs/4, changes/4, query_view/3, query_view/4, query_view/6,
    get_view_group_info/2]).

% miscellany
-export([design_docs/1, reset_validation_funs/1, cleanup_index_files/0,
    cleanup_index_files/1]).

-include("fabric.hrl").

-type dbname() :: (iodata() | #db{}).
-type docid() :: iodata().
-type revision() :: {integer(), binary()}.
-type callback() :: fun((any(), any()) -> {ok | stop, any()}).
-type json_obj() :: {[{binary() | atom(), any()}]}.
-type option() :: atom() | {atom(), any()}.

%% db operations
%% @equiv all_dbs(<<>>)
all_dbs() ->
    all_dbs(<<>>).

%% @doc returns a list of all database names
-spec all_dbs(Prefix::iodata()) -> {ok, [binary()]}.
all_dbs(Prefix) when is_binary(Prefix) ->
    Length = byte_size(Prefix),
    MatchingDbs = ets:foldl(fun(#shard{dbname=DbName}, Acc) ->
        case DbName of
        <<Prefix:Length/binary, _/binary>> ->
            [DbName | Acc];
        _ ->
            Acc
        end
    end, [], partitions),
    {ok, lists:usort(MatchingDbs)};

%% @equiv all_dbs(list_to_binary(Prefix))
all_dbs(Prefix) when is_list(Prefix) ->
    all_dbs(list_to_binary(Prefix)).

%% @doc returns a property list of interesting properties
%%      about the database such as `doc_count', `disk_size',
%%      etc.
-spec get_db_info(dbname()) ->
    {ok, [
        {instance_start_time, binary()} |
        {doc_count, non_neg_integer()} |
        {doc_del_count, non_neg_integer()} |
        {purge_seq, non_neg_integer()} |
        {compact_running, boolean()} |
        {disk_size, non_neg_integer()} |
        {disk_format_version, pos_integer()}
    ]}.
get_db_info(DbName) ->
    fabric_db_info:go(dbname(DbName)).

%% @doc the number of docs in a database
-spec get_doc_count(dbname()) -> {ok, non_neg_integer()}.
get_doc_count(DbName) ->
    fabric_db_doc_count:go(dbname(DbName)).

%% @equiv create_db(DbName, [])
create_db(DbName) ->
    create_db(DbName, []).

%% @doc creates a database with the given name.
%%
%% Options can include values for q and n,
%% for example `{q, "8"}' and `{n, "3"}', which
%% control how many shards to split a database into
%% and how many nodes each doc is copied to respectively.
%%
-spec create_db(dbname(), [option()]) -> ok | accepted | {error, atom()}.
create_db(DbName, Options) ->
    fabric_db_create:go(dbname(DbName), opts(Options)).

%% @equiv delete_db([])
delete_db(DbName) ->
    delete_db(DbName, []).

%% @doc delete a database
-spec delete_db(dbname(), [option()]) -> ok | accepted | {error, atom()}.
delete_db(DbName, Options) ->
    fabric_db_delete:go(dbname(DbName), opts(Options)).

%% @doc provide an upper bound for the number of tracked document revisions
-spec set_revs_limit(dbname(), pos_integer(), [option()]) -> ok.
set_revs_limit(DbName, Limit, Options) when is_integer(Limit), Limit > 0 ->
    fabric_db_meta:set_revs_limit(dbname(DbName), Limit, opts(Options)).

%% @doc retrieves the maximum number of document revisions
-spec get_revs_limit(dbname()) -> pos_integer() | no_return().
get_revs_limit(DbName) ->
    {ok, Db} = fabric_util:get_db(dbname(DbName), [?ADMIN_CTX]),
    try couch_db:get_revs_limit(Db) after catch couch_db:close(Db) end.

%% @doc sets the readers/writers/admin permissions for a database
-spec set_security(dbname(), SecObj::json_obj(), [option()]) -> ok.
set_security(DbName, SecObj, Options) ->
    fabric_db_meta:set_security(dbname(DbName), SecObj, opts(Options)).

get_security(DbName) ->
    get_security(DbName, [?ADMIN_CTX]).

%% @doc retrieve the security object for a database
-spec get_security(dbname()) -> json_obj() | no_return().
get_security(DbName, Options) ->
    {ok, Db} = fabric_util:get_db(dbname(DbName), opts(Options)),
    try couch_db:get_security(Db) after catch couch_db:close(Db) end.

% doc operations

%% @doc retrieve the doc with a given id
-spec open_doc(dbname(), docid(), [option()]) ->
    {ok, #doc{}} |
    {not_found, missing | deleted} |
    {timeout, any()} |
    {error, any()} |
    {error, any() | any()}.
open_doc(DbName, Id, Options) ->
    fabric_doc_open:go(dbname(DbName), docid(Id), opts(Options)).

%% @doc retrieve a collection of revisions, possible all
-spec open_revs(dbname(), docid(), [revision()] | all, [option()]) ->
    {ok, [{ok, #doc{}} | {{not_found,missing}, revision()}]} |
    {timeout, any()} |
    {error, any()} |
    {error, any(), any()}.
open_revs(DbName, Id, Revs, Options) ->
    fabric_doc_open_revs:go(dbname(DbName), docid(Id), Revs, opts(Options)).

%% @equiv get_missing_revs(DbName, IdsRevs, [])
get_missing_revs(DbName, IdsRevs) ->
    get_missing_revs(DbName, IdsRevs, []).

%% @doc retrieve missing revisions for a list of `{Id, Revs}'
-spec get_missing_revs(dbname(),[{docid(), [revision()]}], [option()]) ->
    {ok, [{docid(), any(), [any()]}]}.
get_missing_revs(DbName, IdsRevs, Options) when is_list(IdsRevs) ->
    Sanitized = [idrevs(IdR) || IdR <- IdsRevs],
    fabric_doc_missing_revs:go(dbname(DbName), Sanitized, opts(Options)).

%% @doc update a single doc
%% @equiv update_docs(DbName,[Doc],Options)
-spec update_doc(dbname(), #doc{}, [option()]) ->
    {ok, any()} | any().
update_doc(DbName, Doc, Options) ->
    case update_docs(DbName, [Doc], opts(Options)) of
    {ok, [{ok, NewRev}]} ->
        {ok, NewRev};
    {accepted, [{accepted, NewRev}]} ->
        {accepted, NewRev};
    {ok, [{{_Id, _Rev}, Error}]} ->
        throw(Error);
    {ok, [Error]} ->
        throw(Error);
    {ok, []} ->
        % replication success
        #doc{revs = {Pos, [RevId | _]}} = doc(Doc),
        {ok, {Pos, RevId}}
    end.

%% @doc update a list of docs
-spec update_docs(dbname(), [#doc{}], [option()]) ->
    {ok, any()} | any().
update_docs(DbName, Docs, Options) ->
    try
        fabric_doc_update:go(dbname(DbName), docs(Docs), opts(Options)) of
        {ok, Results} ->
            {ok, Results};
        {accepted, Results} ->
            {accepted, Results};
        Error ->
            throw(Error)
    catch {aborted, PreCommitFailures} ->
        {aborted, PreCommitFailures}
    end.

purge_docs(_DbName, _IdsRevs) ->
    not_implemented.

%% @doc spawns a process to upload attachment data and
%%      returns a function that shards can use to communicate
%%      with the spawned middleman process
-spec att_receiver(#httpd{}, Length :: undefined | chunked | pos_integer() |
        {unknown_transfer_encoding, any()}) ->
    function() | binary().
att_receiver(Req, Length) ->
    fabric_doc_attachments:receiver(Req, Length).

%% @doc retrieves all docs. Additional query parameters, such as `limit',
%%      `start_key' and `end_key', `descending', and `include_docs', can
%%      also be passed to further constrain the query. See <a href=
%%      "http://wiki.apache.org/couchdb/HTTP_Document_API#All_Documents">
%%      all_docs</a> for details
-spec all_docs(dbname(), callback(), [] | tuple(), #view_query_args{}) ->
    {ok, [any()]}.
all_docs(DbName, Callback, Acc0, #view_query_args{} = QueryArgs) when
        is_function(Callback, 2) ->
    fabric_view_all_docs:go(dbname(DbName), QueryArgs, Callback, Acc0);

%% @doc convenience function that takes a keylist rather than a record
%% @equiv all_docs(DbName, Callback, Acc0, kl_to_query_args(QueryArgs))
all_docs(DbName, Callback, Acc0, QueryArgs) ->
    all_docs(DbName, Callback, Acc0, kl_to_query_args(QueryArgs)).


-spec changes(dbname(), callback(), any(), #changes_args{} | [{atom(),any()}]) ->
    {ok, any()}.
changes(DbName, Callback, Acc0, #changes_args{}=Options) ->
    Feed = Options#changes_args.feed,
    fabric_view_changes:go(dbname(DbName), Feed, Options, Callback, Acc0);

%% @doc convenience function, takes keylist instead of record
%% @equiv changes(DbName, Callback, Acc0, kl_to_changes_args(Options))
changes(DbName, Callback, Acc0, Options) ->
    changes(DbName, Callback, Acc0, kl_to_changes_args(Options)).

%% @equiv query_view(DbName, DesignName, ViewName, #view_query_args{})
query_view(DbName, DesignName, ViewName) ->
    query_view(DbName, DesignName, ViewName, #view_query_args{}).

%% @equiv query_view(DbName, DesignName,
%%                     ViewName, fun default_callback/2, [], QueryArgs)
query_view(DbName, DesignName, ViewName, QueryArgs) ->
    Callback = fun default_callback/2,
    query_view(DbName, DesignName, ViewName, Callback, [], QueryArgs).

%% @doc execute a given view.
%%      There are many additional query args that can be passed to a view,
%%      see <a href="http://wiki.apache.org/couchdb/HTTP_view_API#Querying_Options">
%%      query args</a> for details.
-spec query_view(dbname(), #doc{} | binary(), iodata(), callback(), any(),
        #view_query_args{}) ->
    any().
query_view(DbName, Design, ViewName, Callback, Acc0, QueryArgs) ->
    Db = dbname(DbName), View = name(ViewName),
    case is_reduce_view(Db, Design, View, QueryArgs) of
    true ->
        Mod = fabric_view_reduce;
    false ->
        Mod = fabric_view_map
    end,
    Mod:go(Db, Design, View, QueryArgs, Callback, Acc0).

%% @doc retrieve info about a view group, disk size, language, whether compaction
%%      is running and so forth
-spec get_view_group_info(dbname(), #doc{} | docid()) ->
    {ok, [
        {signature, binary()} |
        {language, binary()} |
        {disk_size, non_neg_integer()} |
        {compact_running, boolean()} |
        {updater_running, boolean()} |
        {waiting_commit, boolean()} |
        {waiting_clients, non_neg_integer()} |
        {update_seq, pos_integer()} |
        {purge_seq, non_neg_integer()}
    ]}.
get_view_group_info(DbName, DesignId) ->
    fabric_group_info:go(dbname(DbName), design_doc(DesignId)).

%% @doc retrieve all the design docs from a database
-spec design_docs(dbname()) -> {ok, [json_obj()]}.
design_docs(DbName) ->
    QueryArgs = #view_query_args{start_key = <<"_design/">>, include_docs=true},
    Callback = fun({total_and_offset, _, _}, []) ->
        {ok, []};
    ({row, {Props}}, Acc) ->
        case couch_util:get_value(id, Props) of
        <<"_design/", _/binary>> ->
            {ok, [couch_util:get_value(doc, Props) | Acc]};
        _ ->
            {stop, Acc}
        end;
    (complete, Acc) ->
        {ok, lists:reverse(Acc)}
    end,
    fabric:all_docs(dbname(DbName), Callback, [], QueryArgs).

%% @doc forces a reload of validation functions, this is performed after
%%      design docs are update
%% NOTE: This function probably doesn't belong here as part fo the API
-spec reset_validation_funs(dbname()) -> [reference()].
reset_validation_funs(DbName) ->
    [rexi:cast(Node, {fabric_rpc, reset_validation_funs, [Name]}) ||
        #shard{node=Node, name=Name} <-  mem3:shards(DbName)].

%% @doc clean up index files for all Dbs
-spec cleanup_index_files() -> [ok].
cleanup_index_files() ->
    {ok, Dbs} = fabric:all_dbs(),
    [cleanup_index_files(Db) || Db <- Dbs].

%% @doc clean up index files for a specific db
-spec cleanup_index_files(dbname()) -> ok.
cleanup_index_files(DbName) ->
    {ok, DesignDocs} = fabric:design_docs(DbName),

    ActiveSigs = lists:map(fun(#doc{id = GroupId}) ->
        {ok, Info} = fabric:get_view_group_info(DbName, GroupId),
        binary_to_list(couch_util:get_value(signature, Info))
    end, [couch_doc:from_json_obj(DD) || DD <- DesignDocs]),

    FileList = filelib:wildcard([couch_config:get("couchdb", "view_index_dir"),
        "/.shards/*/", couch_util:to_list(dbname(DbName)), ".[0-9]*_design/*"]),

    DeleteFiles = if ActiveSigs =:= [] -> FileList; true ->
        {ok, RegExp} = re:compile([$(, string:join(ActiveSigs, "|"), $)]),
        lists:filter(fun(FilePath) ->
            re:run(FilePath, RegExp, [{capture, none}]) == nomatch
        end, FileList)
    end,
    [file:delete(File) || File <- DeleteFiles],
    ok.

%% some simple type validation and transcoding

dbname(DbName) when is_list(DbName) ->
    list_to_binary(DbName);
dbname(DbName) when is_binary(DbName) ->
    DbName;
dbname(#db{name=Name}) ->
    Name;
dbname(DbName) ->
    erlang:error({illegal_database_name, DbName}).

name(Thing) ->
    couch_util:to_binary(Thing).

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

design_doc(#doc{} = DDoc) ->
    DDoc;
design_doc(DocId) when is_list(DocId) ->
    design_doc(list_to_binary(DocId));
design_doc(<<"_design/", _/binary>> = DocId) ->
    DocId;
design_doc(GroupName) ->
    <<"_design/", GroupName/binary>>.

idrevs({Id, Revs}) when is_list(Revs) ->
    {docid(Id), [rev(R) || R <- Revs]}.

rev(Rev) when is_list(Rev); is_binary(Rev) ->
    couch_doc:parse_rev(Rev);
rev({Seq, Hash} = Rev) when is_integer(Seq), is_binary(Hash) ->
    Rev.

%% @doc convenience method, useful when testing or calling fabric from the shell
opts(Options) ->
    add_option(user_ctx, add_option(io_priority, Options)).

add_option(Key, Options) ->
    case couch_util:get_value(Key, Options) of
    undefined ->
        case erlang:get(Key) of
        undefined ->
            Options;
        Value ->
            [{Key, Value} | Options]
        end;
    _ ->
        Options
    end.

default_callback(complete, Acc) ->
    {ok, lists:reverse(Acc)};
default_callback(Row, Acc) ->
    {ok, [Row | Acc]}.

is_reduce_view(_, _, _, #view_query_args{view_type=Reduce}) ->
    Reduce =:= reduce.

%% @doc convenience method for use in the shell, converts a keylist
%%      to a `changes_args' record
kl_to_changes_args(KeyList) ->
    kl_to_record(KeyList, changes_args).

%% @doc convenience method for use in the shell, converts a keylist
%%      to a `view_query_args' record
kl_to_query_args(KeyList) ->
    kl_to_record(KeyList, view_query_args).

%% @doc finds the index of the given Key in the record.
%%      note that record_info is only known at compile time
%%      so the code must be written in this way. For each new
%%      record type add a case clause
lookup_index(Key,RecName) ->
    Indexes =
        case RecName of
        changes_args ->
            lists:zip(record_info(fields, changes_args),
                        lists:seq(2, record_info(size, changes_args)));
        view_query_args ->
            lists:zip(record_info(fields, view_query_args),
                        lists:seq(2, record_info(size, view_query_args)))
        end,
    couch_util:get_value(Key, Indexes).

%% @doc convert a keylist to record with given `RecName'
%% @see lookup_index
kl_to_record(KeyList,RecName) ->
    Acc0 = case RecName of
          changes_args -> #changes_args{};
          view_query_args -> #view_query_args{}
          end,
    lists:foldl(fun({Key, Value}, Acc) ->
                    Index = lookup_index(couch_util:to_existing_atom(Key),RecName),
                    setelement(Index, Acc, Value)
                        end, Acc0, KeyList).
