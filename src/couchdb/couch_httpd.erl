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

-export([start_link/3, stop/0, handle_request/2]).

% Maximum size of document PUT request body (4GB)
-define(MAX_DOC_SIZE, (4*1024*1024*1024)).

-record(doc_query_args, {
    options = [],
    rev = "",
    open_revs = ""
}).

-record(view_query_args, {
    start_key = nil,
    end_key = <<>>,
    count = 10000000000, % a huge huge default number. Picked so we don't have
                         % to do different logic for when there is no count
                         % limit
    update = true,
    direction = fwd,
    start_docid = nil,
    end_docid = <<>>,
    skip = 0,
    group_level = 0
}).

start_link(BindAddress, Port, DocumentRoot) ->
    Loop = fun (Req) -> apply(couch_httpd, handle_request, [Req, DocumentRoot]) end,
    mochiweb_http:start([
        {loop, Loop},
        {name, ?MODULE},
        {ip, BindAddress},
        {port, Port}
    ]).

stop() ->
    mochiweb_http:stop(?MODULE).

handle_request(Req, DocumentRoot) ->
    % alias HEAD to GET as mochiweb takes care of stripping the body
    Method = case Req:get(method) of
        'HEAD' -> 'GET';
        Other -> Other
    end,

    % for the path, use the raw path with the query string and fragment
    % removed, but URL quoting left intact
    {Path, _, _} = mochiweb_util:urlsplit_path(Req:get(raw_path)),

    ?LOG_DEBUG("~s ~s ~p~nHeaders: ~p", [
        atom_to_list(Req:get(method)),
        Path,
        Req:get(version),
        mochiweb_headers:to_list(Req:get(headers))
    ]),

    {ok, Resp} = case catch(handle_request(Req, DocumentRoot, Method, Path)) of
        {ok, Resp0} ->
            {ok, Resp0};
        Error ->
            send_error(Req, Error)
    end,

    ?LOG_INFO("~s - - ~p ~B", [
        Req:get(peer),
        atom_to_list(Req:get(method)) ++ " " ++ Path,
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
        "/_utils" ->
            {ok, Req:respond({301, [
                {"Location", "/_utils/"}
            ] ++ server_header(), <<>>})};
        "/_utils/" ++ PathInfo ->
            {ok, Req:serve_file(PathInfo, DocumentRoot, server_header())};
        "/_" ++ _Path ->
            throw({not_found, unknown_private_path});
        "/favicon.ico" ->
            {ok, Req:serve_file("favicon.ico", DocumentRoot)};
        _Else ->
            handle_db_request(Req, Method, {Path})
    end.

% Global request handlers

handle_welcome_request(Req, 'GET') ->
    send_json(Req, {obj, [
        {"couchdb", "Welcome"},
        {"version", couch_server:get_version()}
    ]});

handle_welcome_request(_Req, _Method) ->
    throw({method_not_allowed, "GET,HEAD"}).

handle_all_dbs_request(Req, 'GET') ->
    {ok, DbNames} = couch_server:all_databases(),
    send_json(Req, list_to_tuple(DbNames));

handle_all_dbs_request(_Req, _Method) ->
    throw({method_not_allowed, "GET,HEAD"}).

handle_replicate_request(Req, 'POST') ->
    {obj, Props} = cjson:decode(Req:recv_body()),
    Source = proplists:get_value("source", Props),
    Target = proplists:get_value("target", Props),
    {obj, Options} = proplists:get_value("options", Props, {obj, []}),
    {ok, {obj, JsonResults}} = couch_rep:replicate(Source, Target, Options),
    send_json(Req, {obj, [{ok, true} | JsonResults]});

handle_replicate_request(_Req, _Method) ->
    throw({method_not_allowed, "POST"}).

handle_restart_request(Req, 'POST') ->
    couch_server:remote_restart(),
    send_json(Req, {obj, [{ok, true}]});

handle_restart_request(_Req, _Method) ->
    throw({method_not_allowed, "POST"}).

% Database request handlers

handle_db_request(Req, Method, {Path}) ->
    UriParts = string:tokens(Path, "/"),
    [DbName|Rest] = UriParts,
    handle_db_request(Req, Method, {mochiweb_util:unquote(DbName), Rest});

handle_db_request(Req, 'PUT', {DbName, []}) ->
    case couch_server:create(DbName, []) of
        {ok, _Db} ->
            send_json(Req, 201, {obj, [{ok, true}]});
        {error, database_already_exists} ->
            Msg = io_lib:format("Database ~p already exists.", [DbName]),
            throw({database_already_exists, Msg});
        Error ->
            Msg = io_lib:format("Error creating database ~p: ~p", [DbName, Error]),
            throw({unknown_error, Msg})
    end;

handle_db_request(Req, Method, {DbName, Rest}) ->
    case couch_server:open(DbName) of
        {ok, Db} ->
            handle_db_request(Req, Method, {DbName, Db, Rest});
        Error ->
            throw(Error)
    end;

handle_db_request(Req, 'DELETE', {DbName, _Db, []}) ->
    ok = couch_server:delete(DbName),
    send_json(Req, 200, {obj, [
        {ok, true}
    ]});

handle_db_request(Req, 'GET', {DbName, Db, []}) ->
    {ok, DbInfo} = couch_db:get_db_info(Db),
    send_json(Req, {obj, [{db_name, DbName} | DbInfo]});

handle_db_request(Req, 'POST', {_DbName, Db, []}) ->
    % TODO: Etag handling
    Json = cjson:decode(Req:recv_body(?MAX_DOC_SIZE)),
    Doc = couch_doc:from_json_obj(Json),
    DocId = couch_util:new_uuid(),
    {ok, NewRev} = couch_db:update_doc(Db, Doc#doc{id=DocId, revs=[]}, []),
    send_json(Req, 201, {obj, [
        {ok, true},
        {id, DocId},
        {rev, NewRev}
    ]});

handle_db_request(_Req, _Method, {_DbName, _Db, []}) ->
    throw({method_not_allowed, "DELETE,GET,HEAD,POST"});

handle_db_request(Req, 'POST', {_DbName, Db, ["_bulk_docs"]}) ->
    Options = [], % put options here.
    {obj, JsonProps} = cjson:decode(Req:recv_body(?MAX_DOC_SIZE)),
    DocsArray = proplists:get_value("docs", JsonProps),
    % convert all the doc elements to native docs
    case proplists:get_value("new_edits", JsonProps, true) of
    true ->
        Docs = lists:map(
            fun({obj, ObjProps} = JsonObj) ->
                Doc = couch_doc:from_json_obj(JsonObj),
                Id = case Doc#doc.id of
                    "" -> couch_util:new_uuid();
                    Id0 -> Id0
                end,
                Revs = case proplists:get_value("_rev", ObjProps) of
                    undefined -> [];
                    Rev  -> [Rev]
                end,
                Doc#doc{id=Id,revs=Revs}
            end,
            tuple_to_list(DocsArray)),
        {ok, ResultRevs} = couch_db:update_docs(Db, Docs, Options),

        % output the results
        DocResults = lists:zipwith(
            fun(Doc, NewRev) ->
                {obj, [{"id", Doc#doc.id}, {"rev", NewRev}]}
            end,
            Docs, ResultRevs),
        send_json(Req, 201, {obj, [
            {ok, true},
            {new_revs, list_to_tuple(DocResults)}
        ]});

    false ->
        Docs = [couch_doc:from_json_obj(JsonObj) || JsonObj <- tuple_to_list(DocsArray)],
        ok = couch_db:save_docs(Db, Docs, Options),
        send_json(Req, 201, {obj, [
            {ok, true}
        ]})
    end;

handle_db_request(_Req, _Method, {_DbName, _Db, ["_bulk_docs"]}) ->
    throw({method_not_allowed, "POST"});

handle_db_request(Req, 'POST', {_DbName, Db, ["_compact"]}) ->
    ok = couch_db:start_compact(Db),
    send_json(Req, 202, {obj, [
        {ok, true}
    ]});

handle_db_request(_Req, _Method, {_DbName, _Db, ["_compact"]}) ->
    throw({method_not_allowed, "POST"});

% View request handlers

handle_db_request(Req, 'GET', {_DbName, Db, ["_all_docs"]}) ->
    #view_query_args{
        start_key = StartKey,
        start_docid = StartDocId,
        count = Count,
        skip = SkipCount,
        direction = Dir
    } = QueryArgs = parse_view_query(Req),
    {ok, Info} = couch_db:get_db_info(Db),
    TotalRowCount = proplists:get_value(doc_count, Info),

    StartId = if is_list(StartKey) -> StartKey;
    true -> StartDocId
    end,

    FoldlFun = make_view_fold_fun(Req, QueryArgs, TotalRowCount,
        fun couch_db:enum_docs_reduce_to_count/1),
    AdapterFun = fun(#full_doc_info{id=Id}=FullDocInfo, Offset, Acc) ->
        case couch_doc:to_doc_info(FullDocInfo) of
        #doc_info{deleted=false, rev=Rev} ->
            FoldlFun({{Id, Id}, {obj, [{rev, Rev}]}}, Offset, Acc);
        #doc_info{deleted=true} ->
            {ok, Acc}
        end
    end,
    {ok, FoldResult} = couch_db:enum_docs(Db, StartId, Dir, AdapterFun,
            {Count, SkipCount, undefined, []}),
    finish_view_fold(Req, TotalRowCount, {ok, FoldResult});

handle_db_request(_Req, _Method, {_DbName, _Db, ["_all_docs"]}) ->
    throw({method_not_allowed, "GET,HEAD"});

handle_db_request(Req, 'GET', {_DbName, Db, ["_all_docs_by_seq"]}) ->
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
            Json = {obj,
                [{"rev", Rev}] ++
                case ConflictRevs of
                    []  ->  [];
                    _   ->  [{"conflicts", list_to_tuple(ConflictRevs)}]
                end ++
                case DelConflictRevs of
                    []  ->  [];
                    _   ->  [{"deleted_conflicts", list_to_tuple(DelConflictRevs)}]
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

handle_db_request(Req, 'GET', {DbName, _Db, ["_view", DocId, ViewName]}) ->
    #view_query_args{
        start_key = StartKey,
        count = Count,
        skip = SkipCount,
        direction = Dir,
        start_docid = StartDocId
    } = QueryArgs = parse_view_query(Req),
    case couch_view:get_map_view({DbName, "_design/" ++ DocId, ViewName}) of
    {ok, View} ->
        {ok, RowCount} = couch_view:get_row_count(View),
        Start = {StartKey, StartDocId},
        FoldlFun = make_view_fold_fun(Req, QueryArgs, RowCount,
                fun couch_view:reduce_to_count/1),
        FoldAccInit = {Count, SkipCount, undefined, []},
        FoldResult = couch_view:fold(View, Start, Dir, FoldlFun, FoldAccInit),
        finish_view_fold(Req, RowCount, FoldResult);
    {not_found, Reason} ->
        case couch_view:get_reduce_view({DbName, "_design/" ++ DocId, ViewName}) of
        {ok, View} ->
            output_reduce_view(Req, View);
        _ ->
            throw({not_found, Reason})
        end
    end;

handle_db_request(_Req, _Method, {_DbName, _Db, ["_view", _DocId, _ViewName]}) ->
    throw({method_not_allowed, "GET,HEAD"});

handle_db_request(Req, 'POST', {_DbName, Db, ["_missing_revs"]}) ->
    {obj, JsonDocIdRevs} = cjson:decode(Req:recv_body()),
    DocIdRevs = [{Id, tuple_to_list(Revs)} || {Id, Revs} <- JsonDocIdRevs],
    {ok, Results} = couch_db:get_missing_revs(Db, DocIdRevs),
    JsonResults = [{Id, list_to_tuple(Revs)} || {Id, Revs} <- Results],
    send_json(Req, {obj, [
        {missing_revs, {obj, JsonResults}}
    ]});

handle_db_request(Req, 'POST', {_DbName, Db, ["_increment_update_seq"]}) ->
    % NOTE, use at own risk. This functionality is experimental
    % and might go away entirely.
    {ok, NewSeq} = couch_db:increment_update_seq(Db),
    send_json(Req, {obj, [{ok, true},
        {update_seq, NewSeq}
    ]});

handle_db_request(Req, 'POST', {DbName, _Db, ["_temp_view"]}) ->
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
    {obj, Props} = cjson:decode(Req:recv_body()),
    Language = proplists:get_value("language", Props, "javascript"),
    MapSrc = proplists:get_value("map", Props),
    case proplists:get_value("reduce", Props, null) of
    null ->
        {ok, View} = couch_view:get_map_view({temp, DbName, Language, MapSrc}),
        Start = {StartKey, StartDocId},
        {ok, TotalRows} = couch_view:get_row_count(View),
        FoldlFun = make_view_fold_fun(Req, QueryArgs, TotalRows,
                fun couch_view:reduce_to_count/1),
        FoldAccInit = {Count, SkipCount, undefined, []},
        FoldResult = couch_view:fold(View, Start, Dir, FoldlFun, FoldAccInit),
        finish_view_fold(Req, TotalRows, FoldResult);

    RedSrc ->
        {ok, View} = couch_view:get_reduce_view(
                {temp, DbName, Language, MapSrc, RedSrc}),
        output_reduce_view(Req, View)
    end;

handle_db_request(_Req, _Method, {_DbName, _Db, ["_temp_view"]}) ->
    throw({method_not_allowed, "POST"});

% Document request handlers

handle_db_request(Req, Method, {DbName, Db, ["_design", DesignName]}) ->
    % Special case to enable using an unencoded in the URL of design docs, as
    % slashes in document IDs must otherwise be URL encoded
    DocId = mochiweb_util:join(["_design", DesignName], "/"),
    handle_db_request(Req, Method, {DbName, Db, [DocId]});

handle_db_request(Req, Method, {DbName, Db, [DocId]}) ->
    UnquotedDocId = mochiweb_util:unquote(DocId),
    handle_doc_request(Req, Method, DbName, Db, UnquotedDocId);

handle_db_request(Req, Method, {DbName, Db, [DocId, FileName]}) ->
    UnquotedDocId = mochiweb_util:unquote(DocId),
    UnquotedFileName = mochiweb_util:unquote(FileName),
    handle_attachment_request(Req, Method, DbName, Db, UnquotedDocId,
                              UnquotedFileName).

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
                when is_integer(GroupLevel) and is_tuple(Key1) and is_tuple(Key2) ->
            lists:sublist(tuple_to_list(Key1), GroupLevel) == lists:sublist(tuple_to_list(Key2), GroupLevel);
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
            Json = lists:flatten(cjson:encode({obj, [{key, null}, {value, Red}]})),
            Resp:write_chunk(AccSeparator ++ Json),
            {ok, {",",0,AccCount-1}};
        (Key, Red, {AccSeparator,0,AccCount})
                when is_integer(GroupLevel) 
                andalso is_tuple(Key) 
                andalso element(1, Key) /= obj  ->
            Json = lists:flatten(cjson:encode(
                {obj, [{key, list_to_tuple(lists:sublist(tuple_to_list(Key), GroupLevel))},
                        {value, Red}]})),
            Resp:write_chunk(AccSeparator ++ Json),
            {ok, {",",0,AccCount-1}};
        (Key, Red, {AccSeparator,0,AccCount}) ->
            Json = lists:flatten(cjson:encode({obj, [{key, Key}, {value, Red}]})),
            Resp:write_chunk(AccSeparator ++ Json),
            {ok, {",",0,AccCount-1}}
        end, {"", Skip, Count}),
    Resp:write_chunk("]}"),
    end_json_response(Resp).

handle_doc_request(Req, 'DELETE', _DbName, Db, DocId) ->
    QueryRev = proplists:get_value("rev", Req:parse_qs()),
    Etag = case Req:get_header_value("If-Match") of
        undefined ->
            undefined;
        Tag ->
            string:strip(Tag, both, $")
    end,
    RevToDelete = case {QueryRev, Etag} of
    {undefined, undefined} ->
        throw({missing_rev, "Document rev/etag must be specified to delete"});
    {_, undefined} ->
        QueryRev;
    {undefined, _} ->
        Etag;
    _ when QueryRev == Etag ->
        Etag;
    _ ->
        throw({bad_request, "Document rev and etag have different values"})
    end,
    {ok, NewRev} = couch_db:delete_doc(Db, DocId, [RevToDelete]),
    send_json(Req, 200, {obj, [
        {ok, true},
        {id, DocId},
        {rev, NewRev}
    ]});

handle_doc_request(Req, 'GET', _DbName, Db, DocId) ->
    #doc_query_args{
        rev = Rev,
        open_revs = Revs,
        options = Options
    } = parse_doc_query(Req),
    case Revs of
    [] ->
        case Rev of
        "" -> % open most recent rev
            case couch_db:open_doc(Db, DocId, Options) of
                {ok, #doc{revs=[DocRev|_]}=Doc} ->
                    true;
                Error ->
                    Doc = DocRev = undefined,
                    throw(Error)
            end;
        _ -> % open a specific rev (deletions come back as stubs)
            case couch_db:open_doc_revs(Db, DocId, [Rev], Options) of
                {ok, [{ok, Doc}]} ->
                    DocRev = Rev;
                {ok, [Else]} ->
                    Doc = DocRev = undefined,
                    throw(Else)
            end
        end,
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
                    Json = lists:flatten(cjson:encode({obj, [{ok, JsonDoc}]})),
                    Resp:write_chunk(AccSeparator ++ Json);
                {{not_found, missing}, RevId} ->
                    Json = lists:flatten(cjson:encode({obj, [{"missing", RevId}]})),
                    Resp:write_chunk(AccSeparator ++ Json)
                end,
                "," % AccSeparator now has a comma
            end,
            "", Results),
        Resp:write_chunk("]"),
        end_json_response(Resp)
    end;

handle_doc_request(Req, 'PUT', _DbName, Db, DocId) ->
    Json = {obj, DocProps} = cjson:decode(Req:recv_body(?MAX_DOC_SIZE)),
    DocRev = proplists:get_value("_rev", DocProps),
    Etag = case Req:get_header_value("If-Match") of
        undefined ->
            undefined;
        Tag ->
            string:strip(Tag, both, $")
    end,
    Revs = case {DocRev, Etag} of
    {undefined, undefined} ->
        [];
    {_, undefined} ->
        [DocRev];
    {undefined, _} ->
        [Etag];
    _ when DocRev == Etag ->
        [Etag];
    _ ->
        throw({bad_request, "Document rev and etag have different values"})
    end,

    Doc = couch_doc:from_json_obj(Json),

    {ok, NewRev} = couch_db:update_doc(Db, Doc#doc{id=DocId, revs=Revs}, []),
    send_json(Req, 201, [{"Etag", "\"" ++ NewRev ++ "\""}], {obj, [
        {ok, true},
        {id, DocId},
        {rev, NewRev}
    ]});

handle_doc_request(_Req, _Method, _DbName, _Db, _DocId) ->
    throw({method_not_allowed, "DELETE,GET,HEAD,PUT"}).

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
                {"Content-Type", Type},
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

handle_attachment_request(_Req, _Method, _DbName, _Db, _DocId, _FileName) ->
    throw({method_not_allowed, "GET,HEAD"}).

% View request handling internals

reverse_key_default(nil) -> <<>>;
reverse_key_default(<<>>) -> nil;
reverse_key_default(Key) -> Key.

parse_view_query(Req) ->
    QueryList = Req:parse_qs(),
    lists:foldl(fun({Key,Value}, Args) ->
        case {Key, Value} of
        {"", _} ->
            Args;
        {"key", Value} ->
            JsonKey = cjson:decode(Value),
            Args#view_query_args{start_key=JsonKey,end_key=JsonKey};
        {"startkey_docid", DocId} ->
            Args#view_query_args{start_docid=DocId};
        {"startkey", Value} ->
            Args#view_query_args{start_key=cjson:decode(Value)};
        {"endkey", Value} ->
            Args#view_query_args{end_key=cjson:decode(Value)};
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
            couch_view:less_json({EndKey, EndDocId}, {ViewKey, ViewId})
        end;
    rev->
        fun(ViewKey, ViewId) ->
            couch_view:less_json({ViewKey, ViewId}, {EndKey, EndDocId})
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
            Resp2:write_chunk(lists:flatten(JsonBegin)),
            JsonObj = {obj, [{id, DocId}, {key, Key}, {value, Value}]},
            {ok, {AccCount + 1, 0, Resp2, [cjson:encode(JsonObj) | AccRevRows]}};
        {_, AccCount, _, Resp} ->
            JsonObj = {obj, [{id, DocId}, {key, Key}, {value, Value}]},
            {ok, {AccCount + 1, 0, Resp, [cjson:encode(JsonObj), ",\r\n" | AccRevRows]}}
        end
    end,

    PosCountFun = fun({{Key, DocId}, Value}, OffsetReds,
                      {AccCount, AccSkip, Resp, AccRevRows}) ->
        Offset = ReduceCountFun(OffsetReds),
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
            JsonObj = {obj, [{id, DocId}, {key, Key}, {value, Value}]},
            Resp2:write_chunk(lists:flatten(JsonBegin ++ cjson:encode(JsonObj))),
            {ok, {AccCount - 1, 0, Resp2, AccRevRows}};
        {_, AccCount, _, Resp} when (AccCount > 0) ->
            JsonObj = {obj, [{"id", DocId}, {"key", Key}, {"value", Value}]},
            Resp:write_chunk(",\r\n" ++  lists:flatten(cjson:encode(JsonObj))),
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
        send_json(Req, 200, {obj, [
            {total_rows, TotalRows},
            {rows, {}}
        ]});
    {ok, {_, _, Resp, AccRevRows}} ->
        % end the view
        Resp:write_chunk(lists:flatten(AccRevRows) ++ "\r\n]}"),
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
            Args#doc_query_args{rev=Rev};
        {"open_revs", "all"} ->
            Args#doc_query_args{open_revs=all};
        {"open_revs", RevsJsonStr} ->
            JsonArray = cjson:decode(RevsJsonStr),
            Args#doc_query_args{open_revs=tuple_to_list(JsonArray)};
        _Else -> % unknown key value pair, ignore.
            Args
        end
    end, #doc_query_args{}, Req:parse_qs()).

% Utilities

none_match(Req, Tag) ->
    Etag = "\"" ++ Tag ++ "\"",
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
    Json = {obj, [
        {error, atom_to_list(Atom)},
        {reason, FormattedReason}
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
    Body = cjson:encode(Value),
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
