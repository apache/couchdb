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

% DBs
-export([all_dbs/0, all_dbs/1, create_db/1, create_db/2, delete_db/1,
    delete_db/2, get_db_info/1, get_doc_count/1, set_revs_limit/3,
    set_security/3, get_revs_limit/1, get_security/1]).

% Documents
-export([open_doc/3, open_revs/4, get_missing_revs/2, update_doc/3,
    update_docs/3, att_receiver/2]).

% Views
-export([all_docs/4, changes/4, query_view/3, query_view/4, query_view/6,
    get_view_group_info/2]).

% miscellany
-export([design_docs/1, reset_validation_funs/1, cleanup_index_files/0,
    cleanup_index_files/1]).

-include("fabric.hrl").

% db operations

all_dbs() ->
    all_dbs(<<>>).

all_dbs(Prefix) when is_list(Prefix) ->
    all_dbs(list_to_binary(Prefix));
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
    {ok, lists:usort(MatchingDbs)}.

get_db_info(DbName) ->
    fabric_db_info:go(dbname(DbName)).

get_doc_count(DbName) ->
    fabric_db_doc_count:go(dbname(DbName)).

create_db(DbName) ->
    create_db(DbName, []).

create_db(DbName, Options) ->
    fabric_db_create:go(dbname(DbName), opts(Options)).

delete_db(DbName) ->
    delete_db(DbName, []).

delete_db(DbName, Options) ->
    fabric_db_delete:go(dbname(DbName), opts(Options)).

set_revs_limit(DbName, Limit, Options) when is_integer(Limit), Limit > 0 ->
    fabric_db_meta:set_revs_limit(dbname(DbName), Limit, opts(Options)).

get_revs_limit(DbName) ->
    {ok, Db} = fabric_util:get_db(dbname(DbName)),
    try couch_db:get_revs_limit(Db) after catch couch_db:close(Db) end.

set_security(DbName, SecObj, Options) ->
    fabric_db_meta:set_security(dbname(DbName), SecObj, opts(Options)).

get_security(DbName) ->
    {ok, Db} = fabric_util:get_db(dbname(DbName)),
    try couch_db:get_security(Db) after catch couch_db:close(Db) end.

% doc operations
open_doc(DbName, Id, Options) ->
    fabric_doc_open:go(dbname(DbName), docid(Id), opts(Options)).

open_revs(DbName, Id, Revs, Options) ->
    fabric_doc_open_revs:go(dbname(DbName), docid(Id), Revs, opts(Options)).

get_missing_revs(DbName, IdsRevs) when is_list(IdsRevs) ->
    Sanitized = [idrevs(IdR) || IdR <- IdsRevs],
    fabric_doc_missing_revs:go(dbname(DbName), Sanitized).

update_doc(DbName, Doc, Options) ->
    case update_docs(DbName, [Doc], opts(Options)) of
    {ok, [{ok, NewRev}]} ->
        {ok, NewRev};
    {ok, [Error]} ->
        throw(Error);
    {ok, []} ->
        % replication success
        #doc{revs = {Pos, [RevId | _]}} = doc(Doc),
        {ok, {Pos, RevId}}
    end.

update_docs(DbName, Docs, Options) ->
    try fabric_doc_update:go(dbname(DbName), docs(Docs), opts(Options))
    catch {aborted, PreCommitFailures} ->
        {aborted, PreCommitFailures}
    end.

att_receiver(Req, Length) ->
    fabric_doc_attachments:receiver(Req, Length).

all_docs(DbName, Callback, Acc0, #view_query_args{} = QueryArgs) when
        is_function(Callback, 2) ->
    fabric_view_all_docs:go(dbname(DbName), QueryArgs, Callback, Acc0).

changes(DbName, Callback, Acc0, Options) ->
    % TODO use a keylist for Options instead of #changes_args, BugzID 10281
    Feed = Options#changes_args.feed,
    fabric_view_changes:go(dbname(DbName), Feed, Options, Callback, Acc0).

query_view(DbName, DesignName, ViewName) ->
    query_view(DbName, DesignName, ViewName, #view_query_args{}).

query_view(DbName, DesignName, ViewName, QueryArgs) ->
    Callback = fun default_callback/2,
    query_view(DbName, DesignName, ViewName, Callback, [], QueryArgs).

query_view(DbName, Design, ViewName, Callback, Acc0, QueryArgs) ->
    Db = dbname(DbName), View = name(ViewName),
    case is_reduce_view(Db, Design, View, QueryArgs) of
    true ->
        Mod = fabric_view_reduce;
    false ->
        Mod = fabric_view_map
    end,
    Mod:go(Db, Design, View, QueryArgs, Callback, Acc0).

get_view_group_info(DbName, DesignId) ->
    fabric_group_info:go(dbname(DbName), design_doc(DesignId)).

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

reset_validation_funs(DbName) ->
    [rexi:cast(Node, {fabric_rpc, reset_validation_funs, [Name]}) ||
        #shard{node=Node, name=Name} <-  mem3:shards(DbName)].

cleanup_index_files() ->
    {ok, DbNames} = fabric:all_dbs(),
    [cleanup_index_files(Name) || Name <- DbNames].

cleanup_index_files(DbName) ->
    {ok, DesignDocs} = fabric:design_docs(DbName),

    ActiveSigs = lists:map(fun(#doc{id = GroupId}) ->
        {ok, Info} = fabric:get_view_group_info(DbName, GroupId),
        binary_to_list(couch_util:get_value(signature, Info))
    end, [couch_doc:from_json_obj(DD) || DD <- DesignDocs]),

    FileList = filelib:wildcard([couch_config:get("couchdb", "view_index_dir"),
        "/.shards/*/", couch_util:to_list(DbName), "_design/*"]),

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

opts(Options) ->
    case couch_util:get_value(user_ctx, Options) of
    undefined ->
        case erlang:get(user_ctx) of
        #user_ctx{} = Ctx ->
            [{user_ctx, Ctx} | Options];
        _ ->
            Options
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
