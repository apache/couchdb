% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_httpd).
-include("couch_db.hrl").

-export([start_link/0, stop/0, handle_request/1, handle_request/2]).

% Maximum size of document PUT request body (4GB)
-define(MAX_DOC_SIZE, (4*1024*1024*1024)).

-record(doc_query_args, {
    options = [],
    rev = "",
    open_revs = ""
}).

-record(view_query_args, {
    start_key = nil,
    end_key = {},
    count = 10000000000, % a huge huge default number. Picked so we don't have
                         % to do different logic for when there is no count
                         % limit
    update = true,
    direction = fwd,
    start_docid = nil,
    end_docid = {},
    skip = 0,
    group_level = 0,
    reduce = true
}).

start_link() ->
    % read config and register for configuration changes

    % just stop if one of the config settings change. couch_server_sup
    % will restart us and then we will pick up the new settings.

    BindAddress = couch_config:get("httpd", "bind_address", any),
    Port = couch_config:get("httpd", "port", "5984"),
    DocumentRoot = couch_config:get("httpd", "utils_dir", "../../share/www"),

    % and off we go
    Loop = fun (Req) -> apply(couch_httpd, handle_request,
                [Req, DocumentRoot]) end,
    {ok, Pid} = mochiweb_http:start([
        {loop, Loop},
        {name, ?MODULE},
        {ip, BindAddress},
        {port, Port}
    ]),
    ok = couch_config:register(
        fun("httpd", "bind_address") ->
            ?MODULE:stop();
        ("httpd", "port") ->
            ?MODULE:stop();
        ("httpd", "utils_dir") ->
            ?MODULE:stop()
        end, Pid),

    {ok, Pid}.

stop() ->
    mochiweb_http:stop(?MODULE).

handle_request(config_change) ->
    stop().

handle_request(Req, DocumentRoot) ->
    % alias HEAD to GET as mochiweb takes care of stripping the body
    Method = case Req:get(method) of
        'HEAD' -> 'GET';
        
        % handling of non standard HTTP verbs. Should be fixed in gen_tcp:recv()
        "COPY" -> 'COPY';
        "MOVE" -> 'MOVE';
        StandardMethod -> StandardMethod
    end,

    % for the path, use the raw path with the query string and fragment
    % removed, but URL quoting left intact
    {Path, QueryString, _} = mochiweb_util:urlsplit_path(Req:get(raw_path)),

    ?LOG_DEBUG("~p ~s ~p~nQuery String: ~p~nHeaders: ~p", [
        Req:get(method),
        Path,
        Req:get(version),
        QueryString,
        mochiweb_headers:to_list(Req:get(headers))
    ]),

    {ok, Resp} = case catch(handle_request(Req, DocumentRoot, Method, Path)) of
        {ok, Resp0} ->
            {ok, Resp0};
        Error ->
            send_error(Req, Error)
    end,

    ?LOG_INFO("~s - - ~p ~s ~B", [
        Req:get(peer),
        Method,
        Path,
        Resp:get(code)
    ]).

handle_request(Req, DocumentRoot, Method, Path) ->
    % Start = erlang:now(),
    X = handle_request0(Req, DocumentRoot, Method, Path),
    % io:format("now_diff:~p~n", [timer:now_diff(erlang:now(), Start)]),
    X.

handle_request0(Req, DocumentRoot, Method, Path) ->
    case Path of
        "/" ->
            handle_welcome_request(Req, Method);
        "/_all_dbs" ->
            handle_all_dbs_request(Req, Method);
        "/_replicate" ->
            handle_replicate_request(Req, Method);
        "/_restart" ->
            handle_restart_request(Req, Method);
        "/_uuids" ->
            handle_uuids_request(Req, Method);
        "/_utils" ->
            {ok, Req:respond({301, [
                {"Location", "/_utils/"}
            ] ++ server_header(), <<>>})};
        "/_utils/" ++ PathInfo ->
            {ok, Req:serve_file(PathInfo, DocumentRoot, server_header())};
        "/_config/" ++ Config ->
            handle_config_request(Req, Method, {config, Config});
        "/_" ++ _Path ->
            throw({not_found, unknown_private_path});
        "/favicon.ico" ->
            {ok, Req:serve_file("favicon.ico", DocumentRoot)};
        _Else ->
            handle_db_request(Req, Method, {Path})
    end.

% Global request handlers

handle_welcome_request(Req, 'GET') ->
    send_json(Req, {[
        {couchdb, <<"Welcome">>},
        {version, list_to_binary(couch_server:get_version())}
    ]});

handle_welcome_request(_Req, _Method) ->
    throw({method_not_allowed, "GET,HEAD"}).

handle_all_dbs_request(Req, 'GET') ->
    {ok, DbNames} = couch_server:all_databases(),
    send_json(Req, DbNames);

handle_all_dbs_request(_Req, _Method) ->
    throw({method_not_allowed, "GET,HEAD"}).

handle_replicate_request(Req, 'POST') ->
    {Props} = ?JSON_DECODE(Req:recv_body()),
    Source = proplists:get_value(<<"source">>, Props),
    Target = proplists:get_value(<<"target">>, Props),
    {Options} = proplists:get_value(<<"options">>, Props, {[]}),
    {ok, {JsonResults}} = couch_rep:replicate(Source, Target, Options),
    send_json(Req, {[{ok, true} | JsonResults]});

handle_replicate_request(_Req, _Method) ->
    throw({method_not_allowed, "POST"}).

handle_restart_request(Req, 'POST') ->
    Response = send_json(Req, {[{ok, true}]}),
    spawn(fun() -> couch_server:remote_restart() end),
    Response;

handle_restart_request(_Req, _Method) ->
    throw({method_not_allowed, "POST"}).

handle_uuids_request(Req, 'POST') ->
    Count = list_to_integer(proplists:get_value("count", Req:parse_qs(), "1")),
    % generate the uuids
    UUIDs = [ couch_util:new_uuid() || _ <- lists:seq(1,Count)],
    % send a JSON response
    send_json(Req, {[{"uuids", UUIDs}]});

handle_uuids_request(_Req, _Method) ->
    throw({method_not_allowed, "POST"}).


% Database request handlers

handle_db_request(Req, Method, {Path}) ->
    UriParts = string:tokens(Path, "/"),
    [DbName|Rest] =
        [list_to_binary(mochiweb_util:unquote(Part)) || Part <- UriParts],
    handle_db_request(Req, Method, {DbName, Rest});

handle_db_request(Req, 'PUT', {DbName, []}) ->
    case couch_server:create(DbName, []) of
        {ok, Db} ->
            couch_db:close(Db),
            send_json(Req, 201, {[{ok, true}]});
        {error, database_already_exists} ->
            Msg = io_lib:format("Database ~p already exists.", [
                binary_to_list(DbName)
            ]),
            throw({database_already_exists, Msg});
        Error ->
            Msg = io_lib:format("Error creating database ~p: ~p", [
                binary_to_list(DbName), Error
            ]),
            throw({unknown_error, Msg})
    end;

handle_db_request(Req, 'DELETE', {DbName, []}) ->
    case couch_server:delete(DbName) of
    ok ->
        send_json(Req, 200, {[
            {ok, true}
        ]});
    Error ->
        throw(Error)
    end;

handle_db_request(Req, Method, {DbName, Rest}) ->
    case couch_db:open(DbName, []) of
        {ok, Db} ->
            try
                handle_db_request(Req, Method, {DbName, Db, Rest})
            after
                couch_db:close(Db)
            end;
        Error ->
            throw(Error)
    end;

handle_db_request(Req, 'GET', {DbName, Db, []}) ->
    {ok, DbInfo} = couch_db:get_db_info(Db),
    send_json(Req, {[{db_name, DbName} | DbInfo]});

handle_db_request(Req, 'POST', {_DbName, Db, []}) ->
    % TODO: Etag handling
    Json = ?JSON_DECODE(Req:recv_body(?MAX_DOC_SIZE)),
    Doc = couch_doc:from_json_obj(Json),
    DocId = couch_util:new_uuid(),
    {ok, NewRev} = couch_db:update_doc(Db, Doc#doc{id=DocId, revs=[]}, []),
    send_json(Req, 201, {[
        {ok, true},
        {id, DocId},
        {rev, NewRev}
    ]});

handle_db_request(_Req, _Method, {_DbName, _Db, []}) ->
    throw({method_not_allowed, "DELETE,GET,HEAD,POST"});

handle_db_request(Req, 'POST', {_DbName, Db, [<<"_bulk_docs">>]}) ->
    Options = [], % put options here.
    {JsonProps} = ?JSON_DECODE(Req:recv_body(?MAX_DOC_SIZE)),
    DocsArray = proplists:get_value(<<"docs">>, JsonProps),
    % convert all the doc elements to native docs
    case proplists:get_value(<<"new_edits">>, JsonProps, true) of
    true ->
        Docs = lists:map(
            fun({ObjProps} = JsonObj) ->
                Doc = couch_doc:from_json_obj(JsonObj),
                Id = case Doc#doc.id of
                    <<>> -> couch_util:new_uuid();
                    Id0 -> Id0
                end,
                Revs = case proplists:get_value(<<"_rev">>, ObjProps) of
                    undefined -> [];
                    Rev  -> [Rev]
                end,
                Doc#doc{id=Id,revs=Revs}
            end,
            DocsArray),
        {ok, ResultRevs} = couch_db:update_docs(Db, Docs, Options),

        % output the results
        DocResults = lists:zipwith(
            fun(Doc, NewRev) ->
                {[{"id", Doc#doc.id}, {"rev", NewRev}]}
            end,
            Docs, ResultRevs),
        send_json(Req, 201, {[
            {ok, true},
            {new_revs, DocResults}
        ]});

    false ->
        Docs = [couch_doc:from_json_obj(JsonObj) || JsonObj <- DocsArray],
        ok = couch_db:save_docs(Db, Docs, Options),
        send_json(Req, 201, {[
            {ok, true}
        ]})
    end;

handle_db_request(_Req, _Method, {_DbName, _Db, [<<"_bulk_docs">>]}) ->
    throw({method_not_allowed, "POST"});

handle_db_request(Req, 'POST', {_DbName, Db, [<<"_compact">>]}) ->
    ok = couch_db:start_compact(Db),
    send_json(Req, 202, {[
        {ok, true}
    ]});

handle_db_request(_Req, _Method, {_DbName, _Db, [<<"_compact">>]}) ->
    throw({method_not_allowed, "POST"});

handle_db_request(Req, 'POST', {_DbName, Db, [<<"_purge">>]}) ->
    {IdsRevs} = ?JSON_DECODE(Req:recv_body(?MAX_DOC_SIZE)),
    % validate the json input
    [{_Id, [_|_]=_Revs} = IdRevs || IdRevs <- IdsRevs],
    
    case couch_db:purge_docs(Db, IdsRevs) of
    {ok, PurgeSeq, PurgedIdsRevs} ->
        send_json(Req, 200, {[{<<"purge_seq">>, PurgeSeq}, {<<"purged">>, {PurgedIdsRevs}}]});
    Error ->
        throw(Error)
    end;

handle_db_request(_Req, _Method, {_DbName, _Db, [<<"_purge">>]}) ->
    throw({method_not_allowed, "POST"});

% View request handlers

handle_db_request(Req, 'GET', {_DbName, Db, [<<"_all_docs">>]}) ->
    #view_query_args{
        start_key = StartKey,
        start_docid = StartDocId,
        count = Count,
        skip = SkipCount,
        direction = Dir
    } = QueryArgs = parse_view_query(Req),
    {ok, Info} = couch_db:get_db_info(Db),
    TotalRowCount = proplists:get_value(doc_count, Info),

    StartId = if is_binary(StartKey) -> StartKey;
    true -> StartDocId
    end,

    FoldlFun = make_view_fold_fun(Req, QueryArgs, TotalRowCount,
        fun couch_db:enum_docs_reduce_to_count/1),
    AdapterFun = fun(#full_doc_info{id=Id}=FullDocInfo, Offset, Acc) ->
        case couch_doc:to_doc_info(FullDocInfo) of
        #doc_info{deleted=false, rev=Rev} ->
            FoldlFun({{Id, Id}, {[{rev, Rev}]}}, Offset, Acc);
        #doc_info{deleted=true} ->
            {ok, Acc}
        end
    end,
    {ok, FoldResult} = couch_db:enum_docs(Db, StartId, Dir, AdapterFun,
            {Count, SkipCount, undefined, []}),
    finish_view_fold(Req, TotalRowCount, {ok, FoldResult});

handle_db_request(_Req, _Method, {_DbName, _Db, [<<"_all_docs">>]}) ->
    throw({method_not_allowed, "GET,HEAD"});

handle_db_request(Req, 'GET', {_DbName, Db, [<<"_all_docs_by_seq">>]}) ->
    #view_query_args{
        start_key = StartKey,
        count = Count,
        skip = SkipCount,
        direction = Dir
    } = QueryArgs = parse_view_query(Req),

    {ok, Info} = couch_db:get_db_info(Db),
    TotalRowCount = proplists:get_value(doc_count, Info),

    FoldlFun = make_view_fold_fun(Req, QueryArgs, TotalRowCount,
            fun couch_db:enum_docs_since_reduce_to_count/1),
    StartKey2 = case StartKey of
        nil -> 0;
        <<>> -> 100000000000;
        StartKey when is_integer(StartKey) -> StartKey
    end,
    {ok, FoldResult} = couch_db:enum_docs_since(Db, StartKey2, Dir,
        fun(DocInfo, Offset, Acc) ->
            #doc_info{
                id=Id,
                rev=Rev,
                update_seq=UpdateSeq,
                deleted=Deleted,
                conflict_revs=ConflictRevs,
                deleted_conflict_revs=DelConflictRevs
            } = DocInfo,
            Json = {
                [{"rev", Rev}] ++
                case ConflictRevs of
                    []  ->  [];
                    _   ->  [{"conflicts", ConflictRevs}]
                end ++
                case DelConflictRevs of
                    []  ->  [];
                    _   ->  [{"deleted_conflicts", DelConflictRevs}]
                end ++
                case Deleted of
                    true -> [{"deleted", true}];
                    false -> []
                end
            },
            FoldlFun({{UpdateSeq, Id}, Json}, Offset, Acc)
        end, {Count, SkipCount, undefined, []}),
    finish_view_fold(Req, TotalRowCount, {ok, FoldResult});

handle_db_request(_Req, _Method, {_DbName, _Db, ["_all_docs_by_seq"]}) ->
    throw({method_not_allowed, "GET,HEAD"});

handle_db_request(Req, 'GET', {DbName, _Db, [<<"_view">>, DocId, ViewName]}) ->
    #view_query_args{
        start_key = StartKey,
        count = Count,
        skip = SkipCount,
        direction = Dir,
        start_docid = StartDocId,
        reduce = Reduce
    } = QueryArgs = parse_view_query(Req),
    
    case couch_view:get_map_view({DbName, <<"_design/", DocId/binary>>, ViewName}) of
    {ok, View} ->    
        {ok, RowCount} = couch_view:get_row_count(View),
        Start = {StartKey, StartDocId},
        FoldlFun = make_view_fold_fun(Req, QueryArgs, RowCount,
                fun couch_view:reduce_to_count/1),
        FoldAccInit = {Count, SkipCount, undefined, []},
        FoldResult = couch_view:fold(View, Start, Dir, FoldlFun, FoldAccInit),
        finish_view_fold(Req, RowCount, FoldResult);
    {not_found, Reason} ->
        case couch_view:get_reduce_view({DbName, <<"_design/", DocId/binary>>, ViewName}) of
        {ok, View} ->
            case Reduce of
            false ->
                {reduce, _N, _Lang, MapView} = View,
                {ok, RowCount} = couch_view:get_row_count(MapView),
                Start = {StartKey, StartDocId},
                FoldlFun = make_view_fold_fun(Req, QueryArgs, RowCount,
                    fun couch_view:reduce_to_count/1),
                FoldAccInit = {Count, SkipCount, undefined, []},
                FoldResult = couch_view:fold(MapView, Start, Dir, FoldlFun, FoldAccInit),
                finish_view_fold(Req, RowCount, FoldResult);
            _ ->
                output_reduce_view(Req, View)
            end;
        _ ->
            throw({not_found, Reason})
        end
    end;

handle_db_request(_Req, _Method, {_DbName, _Db, [<<"_view">>, _DocId, _ViewName]}) ->
    throw({method_not_allowed, "GET,HEAD"});

handle_db_request(Req, 'POST', {_DbName, Db, [<<"_missing_revs">>]}) ->
    {JsonDocIdRevs} = ?JSON_DECODE(Req:recv_body()),
    {ok, Results} = couch_db:get_missing_revs(Db, JsonDocIdRevs),
    send_json(Req, {[
        {missing_revs, {Results}}
    ]});

handle_db_request(Req, 'POST', {_DbName, Db, [<<"_increment_update_seq">>]}) ->
    % NOTE, use at own risk. This functionality is experimental
    % and might go away entirely.
    {ok, NewSeq} = couch_db:increment_update_seq(Db),
    send_json(Req, {[{ok, true},
        {update_seq, NewSeq}
    ]});

handle_db_request(Req, 'POST', {DbName, _Db, [<<"_temp_view">>]}) ->
    #view_query_args{
        start_key = StartKey,
        count = Count,
        skip = SkipCount,
        direction = Dir,
        start_docid = StartDocId
    } = QueryArgs = parse_view_query(Req),

    case Req:get_primary_header_value("content-type") of
        undefined -> ok;
        "application/json" -> ok;
        Else -> throw({incorrect_mime_type, Else})
    end,
    {Props} = ?JSON_DECODE(Req:recv_body()),    
    Language = proplists:get_value(<<"language">>, Props, <<"javascript">>),
    MapSrc = proplists:get_value(<<"map">>, Props),
    case proplists:get_value(<<"reduce">>, Props, null) of
    null ->
        {ok, View} = couch_view:get_map_view({temp, DbName, Language, MapSrc}),
        Start = {StartKey, StartDocId},
        
        {ok, TotalRows} = couch_view:get_row_count(View),
        
        FoldlFun = make_view_fold_fun(Req, QueryArgs, TotalRows,
                fun couch_view:reduce_to_count/1),
        FoldAccInit = {Count, SkipCount, undefined, []},
        FoldResult = couch_view:fold(View, Start, Dir, fun(A, B, C) ->
            FoldlFun(A, B, C)
        end, FoldAccInit),
        finish_view_fold(Req, TotalRows, FoldResult);

    RedSrc ->
        {ok, View} = couch_view:get_reduce_view(
                {temp, DbName, Language, MapSrc, RedSrc}),
        output_reduce_view(Req, View)
    end;

handle_db_request(_Req, _Method, {_DbName, _Db, [<<"_temp_view">>]}) ->
    throw({method_not_allowed, "POST"});

% Document request handlers

handle_db_request(Req, Method, {DbName, Db, [<<"_design">>, Name]}) ->
    % Special case to enable using an unencoded in the URL of design docs, as
    % slashes in document IDs must otherwise be URL encoded
    handle_db_request(Req, Method, {DbName, Db, [<<"_design/", Name/binary>>]});

handle_db_request(Req, Method, {DbName, Db, [DocId]}) ->
    handle_doc_request(Req, Method, DbName, Db,DocId);

handle_db_request(Req, Method, {DbName, Db, [DocId, FileName]}) ->
    handle_attachment_request(Req, Method, DbName, Db, DocId,
                              FileName).

output_reduce_view(Req, View) ->
    #view_query_args{
        start_key = StartKey,
        end_key = EndKey,
        count = Count,
        skip = Skip,
        direction = Dir,
        start_docid = StartDocId,
        end_docid = EndDocId,
        group_level = GroupLevel
    } = parse_view_query(Req),
    GroupRowsFun =
        fun({_Key1,_}, {_Key2,_}) when GroupLevel == 0 ->
            true;
        ({Key1,_}, {Key2,_})
                when is_integer(GroupLevel) and is_list(Key1) and is_list(Key2) ->
            lists:sublist(Key1, GroupLevel) == lists:sublist(Key2, GroupLevel);
        ({Key1,_}, {Key2,_}) ->
            Key1 == Key2
        end,
    Resp = start_json_response(Req, 200),
    Resp:write_chunk("{\"rows\":["),
    {ok, _} = couch_view:fold_reduce(View, Dir, {StartKey, StartDocId}, {EndKey, EndDocId},
        GroupRowsFun,
        fun(_Key, _Red, {AccSeparator,AccSkip,AccCount}) when AccSkip > 0 ->
            {ok, {AccSeparator,AccSkip-1,AccCount}};
        (_Key, _Red, {AccSeparator,0,AccCount}) when AccCount == 0 ->
            {stop, {AccSeparator,0,AccCount}};
        (_Key, Red, {AccSeparator,0,AccCount}) when GroupLevel == 0 ->
            Json = ?JSON_ENCODE({[{key, null}, {value, Red}]}),
            Resp:write_chunk(AccSeparator ++ Json),
            {ok, {",",0,AccCount-1}};
        (Key, Red, {AccSeparator,0,AccCount})
                when is_integer(GroupLevel) 
                andalso is_list(Key) ->
            Json = ?JSON_ENCODE(
                {[{key, lists:sublist(Key, GroupLevel)},{value, Red}]}),
            Resp:write_chunk(AccSeparator ++ Json),
            {ok, {",",0,AccCount-1}};
        (Key, Red, {AccSeparator,0,AccCount}) ->
            Json = ?JSON_ENCODE({[{key, Key}, {value, Red}]}),
            Resp:write_chunk(AccSeparator ++ Json),
            {ok, {",",0,AccCount-1}}
        end, {"", Skip, Count}),
    Resp:write_chunk("]}"),
    end_json_response(Resp).

    
handle_doc_request(Req, 'DELETE', _DbName, Db, DocId) ->
    case extract_header_rev(Req, proplists:get_value("rev", Req:parse_qs())) of
    missing_rev ->
        {missing_rev, "Document rev/etag must be specified to delete"};
    RevToDelete ->
        {ok, NewRev} = couch_db:delete_doc(Db, DocId, [RevToDelete]),
        send_json(Req, 200, {[
            {ok, true},
            {id, DocId},
            {rev, NewRev}
            ]})
    end;

handle_doc_request(Req, 'GET', _DbName, Db, DocId) ->
    #doc_query_args{
        rev = Rev,
        open_revs = Revs,
        options = Options
    } = parse_doc_query(Req),
    case Revs of
    [] ->
        {Doc, DocRev} = couch_doc_open(Db, DocId, Rev, Options),
        Etag = none_match(Req, DocRev),
        AdditionalHeaders = case Doc#doc.meta of
            [] -> [{"Etag", Etag}]; % output etag when we have no meta
            _ -> []
        end,
        JsonDoc = couch_doc:to_json_obj(Doc, Options),
        send_json(Req, 200, AdditionalHeaders, JsonDoc);
    _ ->
        {ok, Results} = couch_db:open_doc_revs(Db, DocId, Revs, Options),
        Resp = start_json_response(Req, 200),
        Resp:write_chunk("["),
        % We loop through the docs. The first time through the separator
        % is whitespace, then a comma on subsequent iterations.
        lists:foldl(
            fun(Result, AccSeparator) ->
                case Result of
                {ok, Doc} ->
                    JsonDoc = couch_doc:to_json_obj(Doc, Options),
                    Json = ?JSON_ENCODE({[{ok, JsonDoc}]}),
                    Resp:write_chunk(AccSeparator ++ Json);
                {{not_found, missing}, RevId} ->
                    Json = ?JSON_ENCODE({[{"missing", RevId}]}),
                    Resp:write_chunk(AccSeparator ++ Json)
                end,
                "," % AccSeparator now has a comma
            end,
            "", Results),
        Resp:write_chunk("]"),
        end_json_response(Resp)
    end;

handle_doc_request(Req, 'POST', _DbName, Db, DocId) ->
    Form = mochiweb_multipart:parse_form(Req),
    Rev = list_to_binary(proplists:get_value("_rev", Form)),
    Doc = case couch_db:open_doc_revs(Db, DocId, [Rev], []) of
        {ok, [{ok, Doc0}]}  -> Doc0#doc{revs=[Rev]};
        {ok, [Error]}       -> throw(Error)
    end,

    NewAttachments = [
        {list_to_binary(Name), {list_to_binary(ContentType), Content}} ||
        {Name, {ContentType, _}, Content} <-
        proplists:get_all_values("_attachments", Form)
    ],
    #doc{attachments=Attachments} = Doc,
    NewDoc = Doc#doc{
        attachments = Attachments ++ NewAttachments
    },
    {ok, NewRev} = couch_db:update_doc(Db, NewDoc, []),

    send_json(Req, 201, [{"Etag", "\"" ++ NewRev ++ "\""}], {obj, [
        {ok, true},
        {id, DocId},
        {rev, NewRev}
    ]});

handle_doc_request(Req, 'PUT', _DbName, Db, DocId) ->
    Json = ?JSON_DECODE(Req:recv_body(?MAX_DOC_SIZE)),
    Doc = couch_doc:from_json_obj(Json),
    ExplicitRev =
    case Doc#doc.revs of
        [Rev0|_] -> Rev0;
        [] -> undefined
    end,
    case extract_header_rev(Req, ExplicitRev) of
    missing_rev ->
        Revs = [];
    Rev ->
        Revs = [Rev]
    end,
    {ok, NewRev} = couch_db:update_doc(Db, Doc#doc{id=DocId, revs=Revs}, []),
    send_json(Req, 201, [{"Etag", <<"\"", NewRev/binary, "\"">>}], {[
        {ok, true},
        {id, DocId},
        {rev, NewRev}
    ]});

handle_doc_request(Req, 'COPY', _DbName, Db, SourceDocId) ->
    SourceRev =
    case extract_header_rev(Req, proplists:get_value("rev", Req:parse_qs())) of
        missing_rev -> [];
        Rev -> Rev
    end,

    {TargetDocId, TargetRev} = parse_copy_destination_header(Req),

    % open revision Rev or Current  
    {Doc, _DocRev} = couch_doc_open(Db, SourceDocId, SourceRev, []),

    % save new doc
    {ok, NewTargetRev} = couch_db:update_doc(Db, Doc#doc{id=TargetDocId, revs=TargetRev}, []),

    send_json(Req, 201, [{"Etag", "\"" ++ binary_to_list(NewTargetRev) ++ "\""}], {[
        {ok, true},
        {id, TargetDocId},
        {rev, NewTargetRev}
    ]});

handle_doc_request(Req, 'MOVE', _DbName, Db, SourceDocId) ->
    SourceRev =
    case extract_header_rev(Req, proplists:get_value("rev", Req:parse_qs())) of
    missing_rev -> 
        throw({
            bad_request,
            "MOVE requires a specified rev parameter for the origin resource."}
            );
    Rev -> Rev
    end,

    {TargetDocId, TargetRev} = parse_copy_destination_header(Req),
    % open revision Rev or Current
    {Doc, _DocRev} = couch_doc_open(Db, SourceDocId, SourceRev, []),

    % save new doc & delete old doc in one operation
    Docs = [
        Doc#doc{id=TargetDocId, revs=TargetRev},
        #doc{id=SourceDocId, revs=[SourceRev], deleted=true}
        ],

    {ok, ResultRevs} = couch_db:update_docs(Db, Docs, []),

    DocResults = lists:zipwith(
        fun(FDoc, NewRev) ->
            {[{id, FDoc#doc.id}, {rev, NewRev}]}
        end,
        Docs, ResultRevs),
    send_json(Req, 201, {[
        {ok, true},
        {new_revs, DocResults}
    ]});

handle_doc_request(_Req, _Method, _DbName, _Db, _DocId) ->
    throw({method_not_allowed, "DELETE,GET,HEAD,POST,PUT,COPY,MOVE"}).

% Useful for debugging
% couch_doc_open(Db, DocId) ->
%   couch_doc_open(Db, DocId, [], []).

couch_doc_open(Db, DocId, Rev, Options) ->
    case Rev of
    "" -> % open most recent rev
        case couch_db:open_doc(Db, DocId, Options) of
        {ok, #doc{revs=[DocRev|_]}=Doc} ->
            {Doc, DocRev};
         Error ->
             throw(Error)
         end;
  _ -> % open a specific rev (deletions come back as stubs)
      case couch_db:open_doc_revs(Db, DocId, [Rev], Options) of
          {ok, [{ok, Doc}]} ->
              {Doc, Rev};
          {ok, [Else]} ->
              throw(Else)
      end
  end.

% Attachment request handlers

handle_attachment_request(Req, 'GET', _DbName, Db, DocId, FileName) ->
    case couch_db:open_doc(Db, DocId, []) of
    {ok, #doc{attachments=Attachments}} ->
        case proplists:get_value(FileName, Attachments) of
        undefined ->
            throw({not_found, missing});
        {Type, Bin} ->
            Resp = Req:respond({200, [
                {"Cache-Control", "must-revalidate"},
                {"Content-Type", binary_to_list(Type)},
                {"Content-Length", integer_to_list(couch_doc:bin_size(Bin))}
            ] ++ server_header(), chunked}),
            couch_doc:bin_foldl(Bin,
                fun(BinSegment, []) ->
                    ok = Resp:write_chunk(BinSegment),
                    {ok, []}
                end,
                []
            ),
            Resp:write_chunk(""),
            {ok, Resp}
        end;
    Error ->
        throw(Error)
    end;

handle_attachment_request(Req, Method, _DbName, Db, DocId, FileName)
    when (Method == 'PUT') or (Method == 'DELETE') ->

    NewAttachment = case Method of
        'DELETE' ->
            [];
        _ ->
            [{FileName, {
                list_to_binary(Req:get_header_value("Content-Type")),
                Req:recv_body(?MAX_DOC_SIZE)
            }}]
    end,

    Doc = case extract_header_rev(Req, proplists:get_value("rev", Req:parse_qs())) of
        missing_rev -> % make the new doc
            #doc{id=DocId};
        Rev ->
            case couch_db:open_doc_revs(Db, DocId, [Rev], []) of
            {ok, [{ok, Doc0}]}  -> Doc0#doc{revs=[Rev]};
            {ok, [Error]}       -> throw(Error)
            end
    end,

    #doc{attachments=Attachments} = Doc,
    DocEdited = Doc#doc{
        attachments = NewAttachment ++ proplists:delete(FileName, Attachments)
    },
    {ok, UpdatedRev} = couch_db:update_doc(Db, DocEdited, []),
    send_json(Req, case Method of 'DELETE' -> 200; _ -> 201 end, {[
        {ok, true},
        {id, DocId},
        {rev, UpdatedRev}
    ]});

handle_attachment_request(_Req, _Method, _DbName, _Db, _DocId, _FileName) ->
    throw({method_not_allowed, "GET,HEAD,DELETE,PUT"}).

% Config request handlers

handle_config_request(_Req, Method, {config, Config}) ->
    Parts = string:tokens(Config, "/"),
    handle_config_request(_Req, Method, {Parts});

% GET /_config
handle_config_request(Req, 'GET', {[]}) ->
    send_json(Req, 200, {dict:to_list(dict:map(
        fun(_, Value) -> {Value} end,
        lists:foldl(
            fun({{Section, Option}, Value}, Acc) ->
                SecBin = list_to_binary(Section),
                OptBin = list_to_binary(Option),
                ValBin = list_to_binary(Value),
                dict:append(SecBin, {OptBin, ValBin}, Acc)
            end,
            dict:new(),
            couch_config:all()
        )
    ))});

% GET /_config/Section
handle_config_request(Req, 'GET', {[Section]}) ->
    KVs = [
        {list_to_binary(Key), list_to_binary(Value)} ||
        {Key, Value} <-
        couch_config:get(Section)
    ],
    send_json(Req, 200, {KVs});

% PUT /_config/Section/Key
% "value"
handle_config_request(Req, 'PUT', {[Section, Key]}) ->
    Value = binary_to_list(Req:recv_body()),
    ok = couch_config:set(Section, Key, Value),
    send_json(Req, 200, {[
        {ok, true}
    ]});

% GET /_config/Section/Key
handle_config_request(Req, 'GET', {[Section, Key]}) ->
    case couch_config:get(Section, Key, null) of
    null ->
        throw({not_found, unknown_config_value});
    Value ->
        send_json(Req, 200, list_to_binary(Value))
    end;

% DELETE /_config/Section/Key
handle_config_request(Req, 'DELETE', {[Section, Key]}) ->
    case couch_config:get(Section, Key, null) of
    null ->
        throw({not_found, unknown_config_value});
    OldValue ->
        couch_config:delete(Section, Key),
        send_json(Req, 200, list_to_binary(OldValue))
    end.


% View request handling internals

reverse_key_default(nil) -> {};
reverse_key_default({}) -> nil;
reverse_key_default(Key) -> Key.

parse_view_query(Req) ->
    QueryList = Req:parse_qs(),
    lists:foldl(fun({Key,Value}, Args) ->
        case {Key, Value} of
        {"", _} ->
            Args;
        {"key", Value} ->
            JsonKey = ?JSON_DECODE(Value),
            Args#view_query_args{start_key=JsonKey,end_key=JsonKey};
        {"startkey_docid", DocId} ->
            Args#view_query_args{start_docid=list_to_binary(DocId)};
        {"endkey_docid", DocId} ->
            Args#view_query_args{end_docid=list_to_binary(DocId)};
        {"startkey", Value} ->
            Args#view_query_args{start_key=?JSON_DECODE(Value)};
        {"endkey", Value} ->
            Args#view_query_args{end_key=?JSON_DECODE(Value)};
        {"count", Value} ->
            case (catch list_to_integer(Value)) of
            Count when is_integer(Count) ->
                if Count < 0 ->
                    Args#view_query_args {
                        direction =
                        if Args#view_query_args.direction == rev -> fwd;
                        true -> rev
                        end,
                        count=Count,
                        start_key = reverse_key_default(Args#view_query_args.start_key),
                        start_docid = reverse_key_default(Args#view_query_args.start_docid),
                        end_key = reverse_key_default(Args#view_query_args.end_key),
                        end_docid =  reverse_key_default(Args#view_query_args.end_docid)};
                true ->
                    Args#view_query_args{count=Count}
                end;
            _Error ->
                Msg = io_lib:format("Bad URL query value, number expected: count=~s", [Value]),
                throw({query_parse_error, Msg})
            end;
        {"update", "false"} ->
            Args#view_query_args{update=false};
        {"descending", "true"} ->
            case Args#view_query_args.direction of
            fwd ->
                Args#view_query_args {
                    direction = rev,
                    start_key = reverse_key_default(Args#view_query_args.start_key),
                    start_docid = reverse_key_default(Args#view_query_args.start_docid),
                    end_key = reverse_key_default(Args#view_query_args.end_key),
                    end_docid =  reverse_key_default(Args#view_query_args.end_docid)};
            _ ->
                Args %already reversed
            end;
        {"descending", "false"} ->
          % The descending=false behaviour is the default behaviour, so we
          % simpply ignore it. This is only for convenience when playing with
          % the HTTP API, so that a user doesn't get served an error when
          % flipping true to false in the descending option.
          Args;
        {"skip", Value} ->
            case (catch list_to_integer(Value)) of
            Count when is_integer(Count) ->
                Args#view_query_args{skip=Count};
            _Error ->
                Msg = lists:flatten(io_lib:format(
                "Bad URL query value, number expected: skip=~s", [Value])),
                throw({query_parse_error, Msg})
            end;
        {"group", "true"} ->
            Args#view_query_args{group_level=exact};
        {"group_level", LevelStr} ->
            Args#view_query_args{group_level=list_to_integer(LevelStr)};
        {"reduce", "true"} ->
            Args#view_query_args{reduce=true};
        {"reduce", "false"} ->
            Args#view_query_args{reduce=false};
        _ -> % unknown key
            Msg = lists:flatten(io_lib:format(
                "Bad URL query key:~s", [Key])),
            throw({query_parse_error, Msg})
        end
    end, #view_query_args{}, QueryList).


make_view_fold_fun(Req, QueryArgs, TotalViewCount, ReduceCountFun) ->
    #view_query_args{
        end_key = EndKey,
        end_docid = EndDocId,
        direction = Dir,
        count = Count
    } = QueryArgs,

    PassedEndFun =
    case Dir of
    fwd ->
        fun(ViewKey, ViewId) ->
            couch_view:less_json([EndKey, EndDocId], [ViewKey, ViewId])
        end;
    rev->
        fun(ViewKey, ViewId) ->
            couch_view:less_json([ViewKey, ViewId], [EndKey, EndDocId])
        end
    end,

    NegCountFun = fun({{Key, DocId}, Value}, OffsetReds,
                      {AccCount, AccSkip, Resp, AccRevRows}) ->
        Offset = ReduceCountFun(OffsetReds),
        PassedEnd = PassedEndFun(Key, DocId),
        case {PassedEnd, AccCount, AccSkip, Resp} of
        {true, _, _, _} -> % The stop key has been passed, stop looping.
            {stop, {AccCount, AccSkip, Resp, AccRevRows}};
        {_, 0, _, _} -> % we've done "count" rows, stop foldling
            {stop, {0, 0, Resp, AccRevRows}};
        {_, _, AccSkip, _} when AccSkip > 0 ->
            {ok, {AccCount, AccSkip - 1, Resp, AccRevRows}};
        {_, _, _, undefined} ->
            Resp2 = start_json_response(Req, 200),
            Offset2 = TotalViewCount - Offset -
                lists:min([TotalViewCount - Offset, - AccCount]),
            JsonBegin = io_lib:format("{\"total_rows\":~w,\"offset\":~w,\"rows\":[\r\n",
                    [TotalViewCount, Offset2]),
            Resp2:write_chunk(JsonBegin),
            JsonObj = {[{id, DocId}, {key, Key}, {value, Value}]},
            {ok, {AccCount + 1, 0, Resp2, [?JSON_ENCODE(JsonObj) | AccRevRows]}};
        {_, AccCount, _, Resp} ->
            
            JsonObj = {[{id, DocId}, {key, Key}, {value, Value}]},
            {ok, {AccCount + 1, 0, Resp, [?JSON_ENCODE(JsonObj), ",\r\n" | AccRevRows]}}
        end
    end,

    PosCountFun = fun({{Key, DocId}, Value}, OffsetReds,
                      {AccCount, AccSkip, Resp, AccRevRows}) ->
        Offset = ReduceCountFun(OffsetReds), % I think we only need this call once per view
        PassedEnd = PassedEndFun(Key, DocId),
        case {PassedEnd, AccCount, AccSkip, Resp} of
        {true, _, _, _} ->
            % The stop key has been passed, stop looping.
            {stop, {AccCount, AccSkip, Resp, AccRevRows}};
        {_, 0, _, _} ->
            % we've done "count" rows, stop foldling
            {stop, {0, 0, Resp, AccRevRows}};
        {_, _, AccSkip, _} when AccSkip > 0 ->
            {ok, {AccCount, AccSkip - 1, Resp, AccRevRows}};
        {_, _, _, undefined} ->
            Resp2 = start_json_response(Req, 200),
            JsonBegin = io_lib:format("{\"total_rows\":~w,\"offset\":~w,\"rows\":[\r\n",
                    [TotalViewCount, Offset]),
            JsonObj = {[{id, DocId}, {key, Key}, {value, Value}]},
            
            Resp2:write_chunk(JsonBegin ++ ?JSON_ENCODE(JsonObj)),
            {ok, {AccCount - 1, 0, Resp2, AccRevRows}};
        {_, AccCount, _, Resp} when (AccCount > 0) ->
            JsonObj = {[{id, DocId}, {key, Key}, {value, Value}]},
            Resp:write_chunk(",\r\n" ++  ?JSON_ENCODE(JsonObj)),
            {ok, {AccCount - 1, 0, Resp, AccRevRows}}
        end
    end,
    case Count > 0 of
    true ->     PosCountFun;
    false ->    NegCountFun
    end.

finish_view_fold(Req, TotalRows, FoldResult) ->
    case FoldResult of
    {ok, {_, _, undefined, _}} ->
        % nothing found in the view, nothing has been returned
        % send empty view
        send_json(Req, 200, {[
            {total_rows, TotalRows},
            {rows, []}
        ]});
    {ok, {_, _, Resp, AccRevRows}} ->
        % end the view
        Resp:write_chunk(AccRevRows ++ "\r\n]}"),
        end_json_response(Resp);
    Error ->
        throw(Error)
    end.

% Document request handling internals

parse_doc_query(Req) ->
    lists:foldl(fun({Key,Value}, Args) ->
        case {Key, Value} of
        {"attachments", "true"} ->
            Options = [attachments | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"meta", "true"} ->
            Options = [revs_info, conflicts, deleted_conflicts | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"revs", "true"} ->
            Options = [revs | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"revs_info", "true"} ->
            Options = [revs_info | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"conflicts", "true"} ->
            Options = [conflicts | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"deleted_conflicts", "true"} ->
            Options = [deleted_conflicts | Args#doc_query_args.options],
            Args#doc_query_args{options=Options};
        {"rev", Rev} ->
            Args#doc_query_args{rev=list_to_binary(Rev)};
        {"open_revs", "all"} ->
            Args#doc_query_args{open_revs=all};
        {"open_revs", RevsJsonStr} ->
            JsonArray = ?JSON_DECODE(RevsJsonStr),
            Args#doc_query_args{open_revs=JsonArray};
        _Else -> % unknown key value pair, ignore.
            Args
        end
    end, #doc_query_args{}, Req:parse_qs()).

% Utilities

none_match(Req, Tag) ->
    Etag = "\"" ++ binary_to_list(Tag) ++ "\"",
    Etags = case Req:get_header_value("If-None-Match") of
        undefined ->
            [];
        Tags ->
            string:tokens(Tags, ", ")
    end,
    case lists:member(Etag, Etags) of
        true ->
            throw({not_modified, Etag});
        false ->
            Etag
    end.

error_to_json(Error) ->
    {HttpCode, Atom, Reason} = error_to_json0(Error),
    FormattedReason =
        case (catch io_lib:format("~s", [Reason])) of
        List when is_list(List) ->
            lists:flatten(List);
        _ ->
            lists:flatten(io_lib:format("~p", [Reason])) % else term to text
        end,
    Json = {[
        {error, Atom},
        {reason, list_to_binary(FormattedReason)}
    ]},
    {HttpCode, Json}.

error_to_json0(bad_request) ->
    {400, bad_request, "Bad request"};
error_to_json0({bad_request, Reason}) ->
    {400, bad_request, Reason};
error_to_json0(not_found) ->
    {404, not_found, "missing"};
error_to_json0({missing_rev, Msg}) ->
    {412, missing_rev, Msg};
error_to_json0({not_found, Reason}) ->
    {404, not_found, Reason};
error_to_json0({database_already_exists, Reason}) ->
    {409, database_already_exists, Reason};
error_to_json0(conflict) ->
    {412, conflict, "Update conflict"};
error_to_json0({doc_validation, Msg}) ->
    {406, doc_validation, Msg};
error_to_json0({Id, Reason}) when is_atom(Id) ->
    {500, Id, Reason};
error_to_json0(Error) ->
    {500, error, Error}.

extract_header_rev(Req, ExplictRev) when is_list(ExplictRev)->
    extract_header_rev(Req, list_to_binary(ExplictRev));
extract_header_rev(Req, ExplictRev) ->
    Etag = case Req:get_header_value("If-Match") of
        undefined -> undefined;
        Tag -> string:strip(Tag, both, $")
    end,
    case {ExplictRev, Etag} of
    {undefined, undefined} -> missing_rev;
    {_, undefined} -> ExplictRev;
    {undefined, _} -> list_to_binary(Etag);
    _ when ExplictRev == Etag -> list_to_binary(Etag);
    _ ->
        throw({bad_request, "Document rev and etag have different values"})
    end.

parse_copy_destination_header(Req) ->
    Destination = Req:get_header_value("Destination"),
    case regexp:match(Destination, "\\?") of
    nomatch -> 
        {list_to_binary(Destination), []};
    {match, _, _} ->
        {ok, [DocId, RevQueryOptions]} = regexp:split(Destination, "\\?"),
        {ok, [_RevQueryKey, Rev]} = regexp:split(RevQueryOptions, "="),
        {list_to_binary(DocId), [list_to_binary(Rev)]}
    end.

send_error(Req, {method_not_allowed, Methods}) ->
    {ok, Req:respond({405, [{"Allow", Methods}] ++ server_header(), <<>>})};
send_error(Req, {modified, Etag}) ->
    {ok, Req:respond({412, [{"Etag", Etag}] ++ server_header(), <<>>})};
send_error(Req, {not_modified, Etag}) ->
    {ok, Req:respond({304, [{"Etag", Etag}] ++ server_header(), <<>>})};
send_error(Req, Error) ->
    {Code, Json} = error_to_json(Error),
    ?LOG_INFO("HTTP Error (code ~w): ~p", [Code, Error]),
    send_error(Req, Code, Json).

send_error(Req, Code, Json) ->
    send_json(Req, Code, Json).

send_json(Req, Value) ->
    send_json(Req, 200, Value).

send_json(Req, Code, Value) ->
    send_json(Req, Code, [], Value).

send_json(Req, Code, Headers, Value) ->
    DefaultHeaders = [
        {"Content-Type", negotiate_content_type(Req)},
        {"Cache-Control", "must-revalidate"}
    ] ++ server_header(),
    Body = ?JSON_ENCODE(Value),
    Resp = Req:respond({Code, DefaultHeaders ++ Headers, Body}),
    {ok, Resp}.

start_json_response(Req, Code) ->
    start_json_response(Req, Code, []).

start_json_response(Req, Code, Headers) ->
    DefaultHeaders = [
        {"Content-Type", negotiate_content_type(Req)},
        {"Cache-Control", "must-revalidate"}
    ] ++ server_header(),
    Req:respond({Code, DefaultHeaders ++ Headers, chunked}).

end_json_response(Resp) ->
    Resp:write_chunk(""),
    {ok, Resp}.

negotiate_content_type(Req) ->
    %% Determine the appropriate Content-Type header for a JSON response
    %% depending on the Accept header in the request. A request that explicitly
    %% lists the correct JSON MIME type will get that type, otherwise the
    %% response will have the generic MIME type "text/plain"
    AcceptedTypes = case Req:get_header_value("Accept") of
        undefined       -> [];
        AcceptHeader    -> string:tokens(AcceptHeader, ", ")
    end,
    case lists:member("application/json", AcceptedTypes) of
        true  -> "application/json";
        false -> "text/plain;charset=utf-8"
    end.

server_header() ->
    OTPVersion = "R" ++ integer_to_list(erlang:system_info(compat_rel)) ++ "B",
    [{"Server", "CouchDB/" ++ couch_server:get_version() ++
                " (Erlang OTP/" ++ OTPVersion ++ ")"}].
