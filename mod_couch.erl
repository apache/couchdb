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

-module(mod_couch).

-include("couch_db.hrl").

-export([do/1, load/2, url_decode/1]).

-include_lib("../couch_inets/httpd.hrl").

-record(uri_parts,
    {db = "",
    doc = "",
    attachment = "",
    view = "",
    querystr = ""}).

-record(doc_query_args,
    {
    options = [],
    rev = "",
    open_revs = ""
    }).

%% do. This is the main entry point into Apache CouchDB from the HTTP server

do(Mod) ->
    #mod{request_uri=Uri,request_line=Request, parsed_header=Header,entity_body=Body} = Mod,
    PrevTrapExit = process_flag(trap_exit, true),
    Resp =
    case Uri of
    "/_utils/" ++ RestURI ->
        % if the URI is the utils directory, then this
        % tells mod_get (a std HTTP module) where to serve the file from
        DocumentRoot = httpd_util:lookup(Mod#mod.config_db, document_root, ""),
        {Path, AfterPath} = httpd_util:split_path(DocumentRoot ++ "/" ++ RestURI),

        case RestURI of
        "" ->
            Paths = httpd_util:split_path(DocumentRoot ++ "/index.html"),
            {proceed, [{real_name, Paths} | Mod#mod.data]};
        _ ->
            case filelib:is_file(Path) of
            true ->
                {proceed, [{real_name, {Path, AfterPath}} | Mod#mod.data]};
            false ->
                case filelib:is_dir(Path) of
                true ->
                    % this ends up causing a "Internal Server Error", need to fix.
                    {proceed, [{response,{403,"Forbidden"}}]};
                false ->
                    {proceed, [{response,{404,"Not found"}}]}
                end
            end
        end;
    "/favicon.ico" ->
        DocumentRoot = httpd_util:lookup(Mod#mod.config_db, document_root, ""),
        RealName = DocumentRoot ++ "/" ++ Uri,
        {Path, AfterPath} = httpd_util:split_path(RealName),
        {proceed, [{real_name, {Path, AfterPath}} | Mod#mod.data]};
    _ ->
        couch_log:info("HTTP Request: ~s", [Request]),
        couch_log:debug("Headers: ~p", [Header]),
        couch_log:debug("Body: ~P", [Body, 100]),
        case (catch parse_uri(Uri)) of
        {ok, Parts} ->
            {ok, ResponseCode} =
            case (catch do(Mod, Parts)) of
            {ok, ResponseCode0} ->
                {ok, ResponseCode0};
            Error ->
                send_error(Mod, Error)
            end;
        Error ->
            {ok, ResponseCode} = send_error(Mod, Error)
        end,
        couch_log:info("HTTP Response Code:~p~n", [ResponseCode]),
        {proceed, [{response, {already_sent, ResponseCode, 0}} | Mod#mod.data]}
    end,
    process_flag(trap_exit, PrevTrapExit),
    Resp.


parse_uri(RequestUri) ->
    % seperate out the path and query portions and
    % strip out leading slash and question mark.
    case regexp:split(RequestUri, "\\?") of
    {ok, [[$/|UriPath], QueryStr]} -> ok;
    {ok, [[$/|UriPath]]} -> QueryStr = ""
    end,
    % lets try to parse out the UriPath.
    {ok, UrlParts} = regexp:split(UriPath, "/"),

    {DbName, Id, Attachment, View} =
    case UrlParts of
    [Db] ->
        {Db, "", "", ""};
    [Db, "_design", Doc] ->
        {Db, "_design/" ++ Doc, "", ""};
    [Db, "_design", Doc, Attachment0] ->
        {Db, "_design/" ++ Doc, Attachment0, ""};
    [Db, "_view", Doc, ViewName] ->
        {Db, "_design/" ++ Doc, "", ViewName};
    [Db, "_view%2f" ++ Doc, ViewName] ->
        {Db, "_design/" ++ Doc, "", ViewName};
    [Db, Doc] ->
        {Db, Doc, "", ""};
    [Db, Doc, Attachment0] ->
        {Db, Doc, Attachment0, ""};
    _ ->
        throw({invalid_uri, lists:flatten(io_lib:format("Uri has too many parts: ~p", [UrlParts]))})
    end,
    {ok, #uri_parts{db=url_decode(DbName),
        doc=url_decode(Id),
        attachment=url_decode(Attachment),
        view=url_decode(View),
        querystr=url_decode(QueryStr)}}.

resp_json_header(Mod) ->
    resp_json_header(Mod, []).

% return json doc header values list
resp_json_header(Mod, Options) ->
    Types = string:tokens(proplists:get_value("accept", Mod#mod.parsed_header, ""), ", "),
    case lists:member("application/json", Types) of
    true ->
        resp_header(Mod, Options) ++ [{"content-type","application/json"}];
    false ->
        resp_header(Mod, Options) ++ [{"content-type","text/plain;charset=utf-8"}]
    end.

% return doc header values list
resp_header(#mod{http_version=Version}, Options) ->
    [{"cache-control", "no-cache"},
    {"pragma", "no-cache"},
    {"expires", httpd_util:rfc1123_date()}] ++
    case lists:member(no_body, Options) of
    true -> [];
    false ->
        case Version == "HTTP/1.1" of
        true ->
            [{"transfer-encoding", "chunked"}];
        false ->
            [{"connection", "close"}]
        end
    end.


url_decode([$%, Hi, Lo | Tail]) ->
    Hex = erlang:list_to_integer([Hi, Lo], 16),
    xmerl_ucs:to_utf8([Hex]) ++ url_decode(Tail);
url_decode([H|T]) ->
    [H |url_decode(T)];
url_decode([]) ->
    [].


send_header(Mod, RespCode, Headers) ->
    couch_log:debug("HTTP Response Headers (code ~w): ~p", [RespCode, Headers]),
    httpd_response:send_header(Mod, RespCode, Headers).

send_chunk(Mod, Data) ->
    httpd_response:send_chunk(Mod, Data, false).

send_final_chunk(Mod) ->
    httpd_response:send_final_chunk(Mod, false).

show_couch_welcome(Mod) ->
    send_header(Mod, 200, resp_json_header(Mod)),
    send_chunk(Mod, "{\"couchdb\": \"Welcome\", "),
    send_chunk(Mod, "\"version\": \"" ++ couch_server:get_version()),
    send_chunk(Mod, "\"}\n"),
    send_final_chunk(Mod),
    {ok, 200}.


do(#mod{method="GET"}=Mod, #uri_parts{db=""}) ->
    show_couch_welcome(Mod);
do(#mod{method="GET"}=Mod, #uri_parts{db="_all_dbs", doc=""}=Parts) ->
    send_all_dbs(Mod, Parts);
do(#mod{method="POST"}=Mod, #uri_parts{db="_replicate", doc=""}) ->
    handle_replication_request(Mod);
do(#mod{method="POST"}=Mod, #uri_parts{db="_restart", doc=""}) ->
    couch_server:remote_restart(),
    send_ok(Mod, 201);
do(#mod{method="POST"}=Mod, #uri_parts{doc="_missing_revs"}=Parts) ->
    handle_missing_revs_request(Mod, Parts);
do(#mod{method="PUT"}=Mod, #uri_parts{doc=""}=Parts) ->
    handle_db_create(Mod, Parts);
do(#mod{method="DELETE"}=Mod, #uri_parts{doc=""}=Parts) ->
    handle_db_delete(Mod, Parts);
do(#mod{method="POST"}=Mod, #uri_parts{doc="_bulk_docs"}=Parts) ->
    handle_bulk_doc_update(Mod, Parts);
do(#mod{method="POST"}=Mod, #uri_parts{doc=""}=Parts) ->
    handle_doc_post(Mod, Parts);
do(#mod{method="PUT"}=Mod, Parts) ->
    handle_doc_put(Mod, Parts);
do(#mod{method="DELETE"}=Mod, Parts) ->
    handle_doc_delete(Mod, Parts);
do(#mod{method="POST"}=Mod, #uri_parts{doc="_temp_view"}=Parts) ->
    send_temp_view(Mod, Parts);
do(#mod{method="GET"}=Mod, #uri_parts{doc="_all_docs"}=Parts) ->
    send_all_docs(Mod, Parts);
do(#mod{method="GET"}=Mod, #uri_parts{doc="_all_docs_by_seq"}=Parts) ->
    send_all_docs_by_seq(Mod, Parts);
do(#mod{method="GET"}=Mod, #uri_parts{doc=""}=Parts) ->
    send_database_info(Mod, Parts);
do(#mod{method=Method}=Mod, #uri_parts{attachment="",view=""}=Parts)
        when Method == "GET" orelse Method == "HEAD" ->
    #doc_query_args{open_revs=Revs} = doc_parse_query(Parts#uri_parts.querystr),
    case Revs of
    [] ->
        send_doc(Mod, Parts);
    _ ->
        send_doc_revs(Mod, Parts)
    end;
do(#mod{method=Method}=Mod, #uri_parts{attachment=Att}=Parts)
        when Att /= "", Method == "GET" orelse Method == "HEAD" ->
    send_attachment(Mod, Parts);
do(#mod{method="GET"}=Mod, #uri_parts{view=View}=Parts) when View /= "" ->
    send_view(Mod, Parts).

handle_db_create(Mod, #uri_parts{db=DbName}) ->
    case couch_server:create(DbName, []) of
    {ok, _Db} ->
        send_ok(Mod, 201);
    {error, database_already_exists} ->
        Msg = io_lib:format("Database ~p already exists.", [DbName]),
        throw({database_already_exists, Msg});
    Error ->
        Msg = io_lib:format("Error creating database ~p: ~p", [DbName, Error]),
        throw({unknown_error, Msg})
    end.

handle_db_delete(Mod, #uri_parts{db=DbName}) ->
    % delete with no doc specified, therefore database delete
    case couch_server:delete(DbName) of
    ok ->
        send_ok(Mod, 202);
    Error ->
        throw(Error)
    end.

handle_bulk_doc_update(#mod{entity_body=RawBody}=Mod, Parts) ->
    Options = [], % put options here.
    Db = open_db(Parts),
    {obj, JsonProps} = cjson:decode(RawBody),
    DocsArray = proplists:get_value("docs", JsonProps),
    % convert all the doc elements to native docs
    case proplists:get_value("new_edits", JsonProps, true) of
    true ->
        Docs = lists:map(
            fun({obj, ObjProps} = JsonObj) ->
                Doc = couch_doc:from_json_obj(JsonObj),

                Id =
                case Doc#doc.id of
                    "" -> couch_util:new_uuid();
                    Id0 -> Id0
                end,
                Revs =
                case proplists:get_value("_rev", ObjProps) of
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
        send_ok(Mod, 201, [{new_revs, list_to_tuple(DocResults)}]);

    false ->
        Docs = [couch_doc:from_json_obj(JsonObj) || JsonObj <-  tuple_to_list(DocsArray)],
        ok = couch_db:save_docs(Db, Docs, Options),
        send_ok(Mod, 201)
    end.




doc_parse_query(QueryStr) ->
    QueryList = httpd:parse_query(QueryStr),
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
        end,
        #doc_query_args{}, QueryList).


handle_doc_post(#mod{entity_body=RawBody}=Mod, Parts) ->
    Db = open_db(Parts),
    Json = cjson:decode(RawBody),
    Doc = couch_doc:from_json_obj(Json),
    Id = couch_util:new_uuid(),
    {ok, NewRevId} = couch_db:update_doc(Db, Doc#doc{id=Id, revs=[]}, []),
    send_ok(Mod, 201, [{"id", Id}, {"rev", NewRevId}], [{"etag", NewRevId}]).

handle_doc_put(#mod{parsed_header=Headers}=Mod,
        #uri_parts{doc=Id, querystr=QueryStr}=Parts) ->
    #doc_query_args{options=SaveOptions} = doc_parse_query(QueryStr),
    Db = open_db(Parts),
    {obj, ObjProps} = Json = cjson:decode(Mod#mod.entity_body),
    Doc = couch_doc:from_json_obj(Json),
    Etag = proplists:get_value("if-match", Headers, ""),
    DocRev = proplists:get_value("_rev", ObjProps, ""),

    if DocRev /= "" andalso Etag /= "" andalso DocRev /= Etag ->
        throw({invalid_request, "Document rev and etag have different values"});
    true -> ok
    end,
    Revs =
    if DocRev /= "" -> [DocRev];
    Etag /= "" -> [Etag];
    true -> []
    end,

    {ok, NewRevId} = couch_db:update_doc(Db, Doc#doc{id=Id, revs=Revs}, SaveOptions),
    send_ok(Mod, 201, [{"id", Id}, {"rev", NewRevId}],[{"etag", NewRevId}]).

handle_doc_delete(#mod{parsed_header=Headers}=Mod,
        #uri_parts{doc=Id, querystr=QueryStr}=Parts) ->
    Db = open_db(Parts),
    #doc_query_args{rev=QueryRev} = doc_parse_query(QueryStr),
    Etag = proplists:get_value("if-match", Headers, ""),
    RevToDelete =
    case {QueryRev, Etag} of
    {"", ""} ->
        throw({missing_rev, "Document rev/etag must be specified to delete"});
    {_, ""} ->
        QueryRev;
    {"", _} ->
        Etag;
    _ when QueryRev == Etag ->
        Etag;
    _ ->
        throw({invalid_request, "Document rev and etag have different values"})
    end,
    {ok, NewRev} = couch_db:delete_doc(Db, Id, [RevToDelete]),
    send_ok(Mod, 202, [{"id", Id}, {"rev", NewRev}]).


-record(query_args,
    {start_key = nil,
    end_key = <<>>,
    count = 10000000000,    % a huge huge default number. Picked so we don't have
                            % to do different logic for when there is no count limit
    update = true,
    direction = fwd,
    start_docid = nil,
    end_docid = <<>>,
    skip = 0
    }).

reverse_key_default(nil) -> <<>>;
reverse_key_default(<<>>) -> nil;
reverse_key_default(Key) -> Key.

view_parse_query(QueryStr) ->
    QueryList = httpd:parse_query(QueryStr),
    lists:foldl(fun({Key,Value}, Args) ->
            case {Key, Value} of
            {"", _} ->
                Args;
            {"key", Value} ->
                JsonKey = cjson:decode(Value),
                Args#query_args{start_key=JsonKey,end_key=JsonKey};
            {"startkey_docid", DocId} ->
                Args#query_args{start_docid=DocId};
            {"startkey", Value} ->
                Args#query_args{start_key=cjson:decode(Value)};
            {"endkey", Value} ->
                Args#query_args{end_key=cjson:decode(Value)};
            {"count", Value} ->
                case (catch list_to_integer(Value)) of
                Count when is_integer(Count) ->
                    if Count < 0 ->
                        Args#query_args {
                            direction =
                            if Args#query_args.direction == rev -> fwd;
                            true -> rev
                            end,
                            count=Count,
                            start_key = reverse_key_default(Args#query_args.start_key),
                            start_docid = reverse_key_default(Args#query_args.start_docid),
                            end_key = reverse_key_default(Args#query_args.end_key),
                            end_docid =  reverse_key_default(Args#query_args.end_docid)};
                    true ->
                        Args#query_args{count=Count}
                    end;
                _Error ->
                    Msg = io_lib:format("Bad URL query value, number expected: count=~s", [Value]),
                    throw({query_parse_error, Msg})
                end;
            {"update", "false"} ->
                Args#query_args{update=false};
            {"descending", "true"} ->
                case Args#query_args.direction of
                fwd ->
                    Args#query_args {
                        direction = rev,
                        start_key = reverse_key_default(Args#query_args.start_key),
                        start_docid = reverse_key_default(Args#query_args.start_docid),
                        end_key = reverse_key_default(Args#query_args.end_key),
                        end_docid =  reverse_key_default(Args#query_args.end_docid)};
                _ ->
                    Args %already reversed
                end;
            {"skip", Value} ->
                case (catch list_to_integer(Value)) of
                Count when is_integer(Count) ->
                    Args#query_args{skip=Count};
                _Error ->
                    Msg = lists:flatten(io_lib:format(
                    "Bad URL query value, number expected: skip=~s", [Value])),
                    throw({query_parse_error, Msg})
                end;
            _ -> % unknown key
                Msg = lists:flatten(io_lib:format(
                    "Bad URL query key:~s", [Key])),
                throw({query_parse_error, Msg})
            end
        end,
        #query_args{}, QueryList).


% returns db, otherwise throws exception. Note: no {ok,_}.
open_db(#uri_parts{db=DbName}) ->
    open_db(DbName);
open_db(DbName) when is_list(DbName)->
    case couch_server:open(DbName) of
    {ok, Db} ->
        Db;
    Error ->
        throw(Error)
    end.

handle_missing_revs_request(#mod{entity_body=RawJson}=Mod, Parts) ->
    Db = open_db(Parts),
    {obj, JsonDocIdRevs} = cjson:decode(RawJson),
    DocIdRevs = [{Id, tuple_to_list(Revs)} || {Id, Revs} <- JsonDocIdRevs],
    {ok, Results} = couch_db:get_missing_revs(Db, DocIdRevs),
    JsonResults = [{Id, list_to_tuple(Revs)} || {Id, Revs} <- Results],
    send_json(Mod, 200, {obj, [{missing_revs, {obj, JsonResults}}]}).

handle_replication_request(#mod{entity_body=RawJson}=Mod) ->
    {obj, Props} = cjson:decode(RawJson),
    Src = proplists:get_value("source", Props),
    Tgt = proplists:get_value("target", Props),
    {obj, Options} = proplists:get_value("options", Props, {obj, []}),
    {ok, {obj, JsonResults}} = couch_rep:replicate(Src, Tgt, Options),
    send_ok(Mod, 200, JsonResults).



send_database_info(Mod, #uri_parts{db=DbName}=Parts) ->
    Db = open_db(Parts),
    {ok, InfoList} = couch_db:get_db_info(Db),
    ok = send_header(Mod, 200, resp_json_header(Mod)),
    DocCount = proplists:get_value(doc_count, InfoList),
    LastUpdateSequence = proplists:get_value(last_update_seq, InfoList),
    ok = send_chunk(Mod, "{\"db_name\": \"" ++ DbName ++
        "\", \"doc_count\":" ++ integer_to_list(DocCount) ++
        ", \"update_seq\":" ++ integer_to_list(LastUpdateSequence)++"}"),
    ok = send_final_chunk(Mod),
    {ok, 200}.

send_doc(#mod{parsed_header=Headers}=Mod,
        #uri_parts{doc=DocId,querystr=QueryStr}=Parts) ->
    Db = open_db(Parts),
    #doc_query_args{rev=Rev, options=Options} = doc_parse_query(QueryStr),
    case Rev of
    "" ->
        % open most recent rev
        case couch_db:open_doc(Db, DocId, Options) of
        {ok, #doc{revs=[DocRev|_]}=Doc} ->
            Etag = proplists:get_value("if-none-match", Headers),
            if Options == [] andalso Etag == DocRev ->
                ok = send_header(Mod, 304,
                        resp_header(Mod, [no_body]) ++ [{"etag", DocRev}]),
                {ok, 304};
            true ->
                send_json(Mod, 200, couch_doc:to_json_obj(Doc, Options),
                        if Options == [] -> [{"etag", DocRev}]; true -> [] end)
            end;
        Error ->
            throw(Error)
        end;
    _ ->
        % open a specific rev (deletions come back as stubs)
        case couch_db:open_doc_revs(Db, DocId, [Rev], Options) of
        {ok, [{ok, Doc}]} ->
            send_json(Mod, 200, couch_doc:to_json_obj(Doc, Options), [{"etag", Rev}]);
        {ok, [Else]} ->
            throw(Else)
        end
    end.

send_doc_revs(Mod, #uri_parts{doc=DocId,querystr=QueryStr}=Parts) ->
    Db = open_db(Parts),
    #doc_query_args{options=Options, open_revs=Revs} = doc_parse_query(QueryStr),
    {ok, Results} = couch_db:open_doc_revs(Db, DocId, Revs, Options),
    ok = send_header(Mod, 200, resp_json_header(Mod)),
    ok = send_chunk(Mod, "["),
    % We loop through the docs. The first time through the separator
    % is whitespace, then a comma on subsequent iterations.
    lists:foldl(
        fun(Result, AccSeparator) ->
            case Result of
            {ok, Doc} ->
                JsonDoc= couch_doc:to_json_obj(Doc, Options),
                ok = send_chunk(Mod, AccSeparator ++ lists:flatten(cjson:encode({obj, [{ok, JsonDoc}]})));
            {{not_found, missing}, RevId} ->
                Json = {obj, [{"missing", RevId}]},
                ok = send_chunk(Mod, AccSeparator ++ lists:flatten(cjson:encode(Json)))
            end,
            "," % AccSeparator now has a comma
        end,
        "", Results),
    ok = send_chunk(Mod, "]"),
    ok = send_final_chunk(Mod),
    {ok, 200}.

send_attachment(#mod{method=Method} = Mod,
        #uri_parts{doc=DocId,attachment=Attachment}=Parts) ->
    Db = open_db(Parts),
    case couch_db:open_doc(Db, DocId, []) of
    {ok, #doc{attachments=Attachments}} ->
        case proplists:get_value(Attachment, Attachments) of
        undefined ->
            throw({not_found, missing});
        {Type, Bin} ->
            ok = send_header(Mod, 200, resp_header(Mod, Type) ++
                [{"content-type", Type},
                {"content-length", integer_to_list(couch_doc:bin_size(Bin))}]),
            case Method of
            "GET" ->
                couch_doc:bin_foldl(Bin,
                        fun(BinSegment, []) ->
                            ok = send_chunk(Mod, BinSegment),
                            {ok, []}
                        end,
                        []);
            "HEAD" ->
                ok
            end,
            ok = send_final_chunk(Mod),
            {ok, 200}
        end;
    Error ->
        throw(Error)
    end.


send_json(Mod, Code, JsonData) ->
    send_json(Mod, Code, JsonData, []).

send_json(#mod{method=Method}=Mod, Code, JsonData, Headers) ->
    case Method of
    "HEAD" ->
        ok = send_header(Mod, Code, resp_json_header(Mod, [no_body]) ++ Headers);
    _ ->
        ok = send_header(Mod, Code, resp_json_header(Mod) ++ Headers),
        ok = send_chunk(Mod, lists:flatten([cjson:encode(JsonData) | "\n"])),
        ok = send_final_chunk(Mod)
    end,
    {ok, Code}.


send_ok(Mod, Code) ->
        send_ok(Mod, Code, []).

send_ok(Mod, Code, AdditionalProps) ->
    send_ok(Mod, Code, AdditionalProps, []).

send_ok(Mod, Code, AdditionalProps, AdditionalHeaders) ->
    send_json(Mod, Code, {obj, [{ok, true}|AdditionalProps]}, AdditionalHeaders).


make_view_fold_fun(Mod, QueryArgs) ->
    #query_args{
        end_key=EndKey,
        end_docid=EndDocId,
        direction=Dir,
        count=Count
        } = QueryArgs,

    PassedEndFun =
    case Dir of
    fwd ->
        fun(ViewKey, ViewId) ->
            couch_view:less_json({EndKey,EndDocId}, {ViewKey,ViewId})
        end;
    rev->
        fun(ViewKey, ViewId) ->
            couch_view:less_json({ViewKey, ViewId}, {EndKey,EndDocId})
        end
    end,

    NegCountFun =
    fun(Id, Key, Value, Offset, TotalViewCount, {AccCount, AccSkip, HeaderSent, AccRevRows}) ->
        PassedEnd = PassedEndFun(Key, Id),
        case {PassedEnd, AccCount, AccSkip, HeaderSent} of
        {true,_,_,_} ->
            % The stop key has been passed, stop looping.
            {stop, {AccCount, AccSkip, HeaderSent, AccRevRows}};
        {_,0,_,_} ->
            {stop, {0, 0, HeaderSent, AccRevRows}}; % we've done "count" rows, stop foldling
        {_,_,AccSkip,_} when AccSkip > 0 ->
            {ok, {AccCount, AccSkip - 1, HeaderSent, AccRevRows}};
        {_,AccCount,_,header_sent} ->
            JsonObj = {obj, [{"key",Key},{"id",Id},{"value",Value}]},
            {ok, {AccCount + 1, 0, header_sent, [cjson:encode(JsonObj), "," | AccRevRows]}};
        {_,_,_,header_not_sent} ->
            ok = send_header(Mod, 200, resp_json_header(Mod)),
            Offset2= TotalViewCount - Offset -
                lists:min([TotalViewCount - Offset, - AccCount]),
            JsonBegin = io_lib:format("{\"total_rows\":~w,\"offset\":~w,\"rows\":[",
                    [TotalViewCount, Offset2]),
            JsonObj = {obj, [{"key",Key},{"id",Id},{"value",Value}]},
            ok = send_chunk(Mod, lists:flatten(JsonBegin)),
            {ok, {AccCount + 1, 0, header_sent, [cjson:encode(JsonObj) | AccRevRows]}}
        end
    end,

    PosCountFun =
    fun(Id, Key, Value, Offset, TotalViewCount, {AccCount, AccSkip, HeaderSent, AccRevRows}) ->
        PassedEnd = PassedEndFun(Key, Id),
        case {PassedEnd, AccCount, AccSkip, HeaderSent} of
        {true,_,_,_} ->
            % The stop key has been passed, stop looping.
            {stop, {AccCount, AccSkip, HeaderSent, AccRevRows}};
        {_,0,_,_} ->
            {stop, {0, 0, HeaderSent, AccRevRows}}; % we've done "count" rows, stop foldling
        {_,_,AccSkip,_} when AccSkip > 0 ->
            {ok, {AccCount, AccSkip - 1, HeaderSent, AccRevRows}};
        {_,AccCount,_,header_sent} when (AccCount > 0) ->
            JsonObj = {obj, [{"key",Key},{"id",Id},{"value",Value}]},
            ok = send_chunk(Mod, "," ++  lists:flatten(cjson:encode(JsonObj))),
            {ok, {AccCount - 1, 0, header_sent, AccRevRows}};
        {_,_,_,header_not_sent} ->
            ok = send_header(Mod, 200, resp_json_header(Mod)),
            JsonBegin = io_lib:format("{\"total_rows\":~w,\"offset\":~w,\"rows\":[",
                    [TotalViewCount, Offset]),
            JsonObj = {obj, [{"key",Key},{"id",Id},{"value",Value}]},
            ok = send_chunk(Mod, lists:flatten(JsonBegin ++ cjson:encode(JsonObj))),
            {ok, {AccCount - 1, 0, header_sent, AccRevRows}}
        end
    end,
    case Count > 0 of
    true ->     PosCountFun;
    false ->    NegCountFun
    end.

finish_view_fold(Mod, FoldResult) ->
    case FoldResult of
    {ok, TotalRows, {_, _, header_not_sent, _}} ->
        % nothing found in the view, nothing has been returned
        % send empty view
        ok = send_header(Mod, 200, resp_json_header(Mod)),
        JsonEmptyView = lists:flatten(
            io_lib:format("{\"total_rows\":~w,\"rows\":[]}\n",
                [TotalRows])),
        ok = send_chunk(Mod, JsonEmptyView),
        ok = send_final_chunk(Mod),
        {ok, 200};
    {ok, _TotalRows, {_, _, header_sent, AccRevRows}} ->
        % end the view
        ok = send_chunk(Mod, lists:flatten(AccRevRows) ++ "]}\n"),
        ok = send_final_chunk(Mod),
        {ok, 200};
    Error ->
        throw(Error)
    end.
    

send_temp_view(#mod{entity_body=Body,parsed_header=Headers}=Mod,
        #uri_parts{db=DbName, querystr=QueryStr}) ->
    #query_args{
        start_key=StartKey,
        count=Count,
        skip=SkipCount,
        direction=Dir,
        start_docid=StartDocId} = QueryArgs = view_parse_query(QueryStr),
    Type0 = proplists:get_value("content-type", Headers, "text/javascript"),
    % remove the charset ("...;charset=foo") if its there
    {ok, [Type|_]} = regexp:split(Type0, ";"),
    View = {temp, DbName, Type, Body},
    Start = {StartKey, StartDocId},
    FoldlFun = make_view_fold_fun(Mod, QueryArgs),
    FoldAccInit = {Count, SkipCount, header_not_sent, []},
    FoldResult = couch_view:fold(View, Start, Dir, FoldlFun, FoldAccInit),
    finish_view_fold(Mod, FoldResult).


send_view(Mod, #uri_parts{db=DbName, doc=DesignDocId, view=ViewId, querystr=QueryStr}) ->
    #query_args{
        start_key=StartKey,
        count=Count,
        skip=SkipCount,
        direction=Dir,
        start_docid=StartDocId} = QueryArgs = view_parse_query(QueryStr),
    View = {DbName, DesignDocId, ViewId},
    Start = {StartKey, StartDocId},
    FoldlFun = make_view_fold_fun(Mod, QueryArgs),
    FoldAccInit = {Count, SkipCount, header_not_sent, []},
    Result = couch_view:fold(View, Start, Dir, FoldlFun, FoldAccInit),
    finish_view_fold(Mod, Result).


send_all_docs(Mod, #uri_parts{querystr=QueryStr}=Parts) ->
    Db = open_db(Parts),
    #query_args{
        start_key=StartKey,
        start_docid=StartDocId,
        count=Count,
        skip=SkipCount,
        direction=Dir} = QueryArgs = view_parse_query(QueryStr),
    {ok, Info} = couch_db:get_db_info(Db),
    TotalRowCount = proplists:get_value(doc_count, Info),

    StartId =
    if is_list(StartKey) -> StartKey;
    true -> StartDocId
    end,

    FoldlFun = make_view_fold_fun(Mod, QueryArgs),
    AdapterFun =
        fun(#full_doc_info{id=Id}=FullDocInfo, Offset, Acc) ->
            case couch_doc:to_doc_info(FullDocInfo) of
            #doc_info{deleted=false, rev=Rev} ->
                FoldlFun(Id, Id, {obj, [{rev, Rev}]}, Offset, TotalRowCount, Acc);
            #doc_info{deleted=true} ->
                {ok, Acc}
            end
        end,
    {ok, FoldResult} = couch_db:enum_docs(Db, StartId, Dir, AdapterFun,
            {Count, SkipCount, header_not_sent, []}),
    finish_view_fold(Mod, {ok, TotalRowCount, FoldResult}).

send_all_docs_by_seq(Mod, #uri_parts{querystr=QueryStr}=Parts) ->
    Db = open_db(Parts),
    QueryArgs = view_parse_query(QueryStr),
    #query_args{
        start_key=StartKey,
        count=Count,
        skip=SkipCount,
        direction=Dir} = QueryArgs,

    {ok, Info} = couch_db:get_db_info(Db),
    TotalRowCount = proplists:get_value(doc_count, Info),

    FoldlFun = make_view_fold_fun(Mod, QueryArgs),

    StartKey2 =
    case StartKey of
    nil -> 0;
    <<>> -> 100000000000;
    StartKey when is_integer(StartKey) -> StartKey
    end,
    {ok, FoldResult} =
    couch_db:enum_docs_since(Db, StartKey2, Dir,
        fun(DocInfo, Offset, Acc) ->
            #doc_info{
                id=Id,
                rev=Rev,
                update_seq=UpdateSeq,
                deleted=Deleted,
                conflict_revs=ConflictRevs,
                deleted_conflict_revs=DelConflictRevs} = DocInfo,
            Json =
            {obj,
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
            FoldlFun(Id, UpdateSeq, Json, Offset, TotalRowCount, Acc)
        end, {Count, SkipCount, header_not_sent, []}),
    finish_view_fold(Mod, {ok, TotalRowCount, FoldResult}).



send_all_dbs(Mod, _Parts)->
    {ok, DbNames} = couch_server:all_databases(),
    ok = send_header(Mod, 200, resp_json_header(Mod)),
    ok = send_chunk(Mod, lists:flatten(cjson:encode(list_to_tuple(DbNames)))),
    ok = send_final_chunk(Mod),
    {ok, 200}.

send_error(Mod, Error) ->
    {Json, Code} = error_to_json(Error),
    couch_log:info("HTTP Error (code ~w): ~p", [Code,  Json]),
    send_json(Mod, Code, Json).



% convert an error response into a json object and http error code.
error_to_json(Error) ->
    {HttpCode, Atom, Reason} = error_to_json0(Error),
    Reason1 =
        case (catch io_lib:format("~s", [Reason])) of
        Reason0 when is_list(Reason0) ->
            lists:flatten(Reason0);
        _ ->
            lists:flatten(io_lib:format("~p", [Reason])) % else term to text
        end,
    Json =
        {obj,
            [{error, atom_to_list(Atom)},
            {reason, Reason1}]},
    {Json, HttpCode}.

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

%%
%% Configuration
%%

%% load

load("Foo Bar", []) ->
    {ok, [], {script_alias, {"foo", "bar"}}}.
