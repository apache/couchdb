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

-module(couch_httpd_show).

-export([handle_doc_show_req/3, handle_doc_update_req/3, handle_view_list_req/3,
        handle_view_list/6, get_fun_key/3]).

-include("couch_db.hrl").

-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,last_chunk/1,send_chunked_error/2,
    start_chunked_response/3, send_error/4]).


% /db/_design/foo/_show/bar/docid
% show converts a json doc to a response of any content-type.
% it looks up the doc an then passes it to the query server.
% then it sends the response from the query server to the http client.

maybe_open_doc(Db, DocId) ->
    case catch couch_httpd_db:couch_doc_open(Db, DocId, nil, [conflicts]) of
        {not_found, missing} -> nil;
        {not_found,deleted} -> nil;
        Doc -> Doc
    end.
handle_doc_show_req(#httpd{
        path_parts=[_, _, _, _, ShowName, DocId]
    }=Req, Db, DDoc) ->

    % open the doc
    Doc = maybe_open_doc(Db, DocId),

    % we don't handle revs here b/c they are an internal api
    % returns 404 if there is no doc with DocId
    handle_doc_show(Req, Db, DDoc, ShowName, Doc, DocId);

handle_doc_show_req(#httpd{
        path_parts=[_, _, _, _, ShowName, DocId|Rest]
    }=Req, Db, DDoc) ->
    
    DocParts = [DocId|Rest],
    DocId1 = ?l2b(string:join([?b2l(P)|| P <- DocParts], "/")),

    % open the doc
    Doc = maybe_open_doc(Db, DocId1),

    % we don't handle revs here b/c they are an internal api
    % pass 404 docs to the show function
    handle_doc_show(Req, Db, DDoc, ShowName, Doc, DocId1);

handle_doc_show_req(#httpd{
        path_parts=[_, _, _, _, ShowName]
    }=Req, Db, DDoc) ->
    % with no docid the doc is nil
    handle_doc_show(Req, Db, DDoc, ShowName, nil);

handle_doc_show_req(Req, _Db, _DDoc) ->
    send_error(Req, 404, <<"show_error">>, <<"Invalid path.">>).

handle_doc_show(Req, Db, DDoc, ShowName, Doc) ->
    handle_doc_show(Req, Db, DDoc, ShowName, Doc, null).

handle_doc_show(Req, Db, DDoc, ShowName, Doc, DocId) ->
    % get responder for ddoc/showname
    CurrentEtag = show_etag(Req, Doc, DDoc, []),
    couch_httpd:etag_respond(Req, CurrentEtag, fun() ->
        JsonReq = couch_httpd_external:json_req_obj(Req, Db, DocId),
        JsonDoc = couch_query_servers:json_doc(Doc),
        [<<"resp">>, ExternalResp] =
            couch_query_servers:ddoc_prompt(DDoc, [<<"shows">>, ShowName], [JsonDoc, JsonReq]),
        JsonResp = apply_etag(ExternalResp, CurrentEtag),
        couch_httpd_external:send_external_response(Req, JsonResp)
    end).



show_etag(#httpd{user_ctx=UserCtx}=Req, Doc, DDoc, More) ->
    Accept = couch_httpd:header_value(Req, "Accept"),
    DocPart = case Doc of
        nil -> nil;
        Doc -> couch_httpd:doc_etag(Doc)
    end,
    couch_httpd:make_etag({couch_httpd:doc_etag(DDoc), DocPart, Accept, UserCtx#user_ctx.roles, More}).

get_fun_key(DDoc, Type, Name) ->
    #doc{body={Props}} = DDoc,
    Lang = couch_util:get_value(<<"language">>, Props, <<"javascript">>),
    Src = couch_util:get_nested_json_value({Props}, [Type, Name]),
    {Lang, Src}.

% /db/_design/foo/_update/bar/docid
% updates a doc based on a request
% handle_doc_update_req(#httpd{method = 'GET'}=Req, _Db, _DDoc) ->
%     % anything but GET
%     send_method_not_allowed(Req, "POST,PUT,DELETE,ETC");
    
handle_doc_update_req(#httpd{
        path_parts=[_, _, _, _, UpdateName, DocId|Rest]
    }=Req, Db, DDoc) ->
    DocParts = [DocId|Rest],
    DocId1 = ?l2b(string:join([?b2l(P)|| P <- DocParts], "/")),
    Doc = try couch_httpd_db:couch_doc_open(Db, DocId1, nil, [conflicts])
    catch
      _ -> nil
    end,
    send_doc_update_response(Req, Db, DDoc, UpdateName, Doc, DocId1);

handle_doc_update_req(#httpd{
        path_parts=[_, _, _, _, UpdateName]
    }=Req, Db, DDoc) ->
    send_doc_update_response(Req, Db, DDoc, UpdateName, nil, null);

handle_doc_update_req(Req, _Db, _DDoc) ->
    send_error(Req, 404, <<"update_error">>, <<"Invalid path.">>).

send_doc_update_response(Req, Db, DDoc, UpdateName, Doc, DocId) ->
    JsonReq = couch_httpd_external:json_req_obj(Req, Db, DocId),
    JsonDoc = couch_query_servers:json_doc(Doc),
    JsonResp1 = case couch_query_servers:ddoc_prompt(DDoc,
                [<<"updates">>, UpdateName], [JsonDoc, JsonReq]) of
        [<<"up">>, {NewJsonDoc}, {JsonResp}] ->
            Options = case couch_httpd:header_value(Req, "X-Couch-Full-Commit",
                "false") of
            "true" ->
                [full_commit];
            _ ->
                []
            end,
            NewDoc = couch_doc:from_json_obj({NewJsonDoc}),
            {ok, NewRev} = couch_db:update_doc(Db, NewDoc, Options),
            NewRevStr = couch_doc:rev_to_str(NewRev),
            {[{<<"code">>, 201}, {<<"headers">>,
                {[{<<"X-Couch-Update-NewRev">>, NewRevStr}]}} | JsonResp]};
        [<<"up">>, _Other, {JsonResp}] ->
            {[{<<"code">>, 200} | JsonResp]}
    end,

    % todo set location field
    couch_httpd_external:send_external_response(Req, JsonResp1).


% view-list request with view and list from same design doc.
handle_view_list_req(#httpd{method=Method,
        path_parts=[_, _, DesignName, _, ListName, ViewName]}=Req, Db, DDoc)
  when Method =:= 'GET' orelse Method =:= 'OPTIONS' ->
    Keys = couch_httpd:qs_json_value(Req, "keys", nil),
    handle_view_list(Req, Db, DDoc, ListName, {DesignName, ViewName}, Keys);

% view-list request with view and list from different design docs.
handle_view_list_req(#httpd{method=Method,
        path_parts=[_, _, _, _, ListName, ViewDesignName, ViewName]}=Req, Db, DDoc)
  when Method =:= 'GET' orelse Method =:= 'OPTIONS' ->
    Keys = couch_httpd:qs_json_value(Req, "keys", nil),
    handle_view_list(Req, Db, DDoc, ListName, {ViewDesignName, ViewName}, Keys);

handle_view_list_req(#httpd{method=Method}=Req, _Db, _DDoc)
    when Method =:= 'GET' orelse Method =:= 'OPTIONS' ->
    send_error(Req, 404, <<"list_error">>, <<"Invalid path.">>);

handle_view_list_req(#httpd{method='POST',
        path_parts=[_, _, DesignName, _, ListName, ViewName]}=Req, Db, DDoc) ->
    % {Props2} = couch_httpd:json_body(Req),
    ReqBody = couch_httpd:body(Req),
    {Props2} = ?JSON_DECODE(ReqBody),
    Keys = couch_util:get_value(<<"keys">>, Props2, nil),
    handle_view_list(Req#httpd{req_body=ReqBody}, Db, DDoc, ListName, {DesignName, ViewName}, Keys);

handle_view_list_req(#httpd{method='POST',
        path_parts=[_, _, _, _, ListName, ViewDesignName, ViewName]}=Req, Db, DDoc) ->
    % {Props2} = couch_httpd:json_body(Req),
    ReqBody = couch_httpd:body(Req),
    {Props2} = ?JSON_DECODE(ReqBody),
    Keys = couch_util:get_value(<<"keys">>, Props2, nil),
    handle_view_list(Req#httpd{req_body=ReqBody}, Db, DDoc, ListName, {ViewDesignName, ViewName}, Keys);

handle_view_list_req(#httpd{method='POST'}=Req, _Db, _DDoc) ->
    send_error(Req, 404, <<"list_error">>, <<"Invalid path.">>);

handle_view_list_req(Req, _Db, _DDoc) ->
    send_method_not_allowed(Req, "GET,POST,HEAD").

handle_view_list(Req, Db, DDoc, LName, {ViewDesignName, ViewName}, Keys) ->
    ViewDesignId = <<"_design/", ViewDesignName/binary>>,
    {ViewType, View, Group, QueryArgs} = couch_httpd_view:load_view(Req, Db, {ViewDesignId, ViewName}, Keys),
    Etag = list_etag(Req, Db, Group, View, QueryArgs, {couch_httpd:doc_etag(DDoc), Keys}),
    couch_httpd:etag_respond(Req, Etag, fun() ->
            output_list(ViewType, Req, Db, DDoc, LName, View, QueryArgs, Etag, Keys, Group)
        end).

list_etag(#httpd{user_ctx=UserCtx}=Req, Db, Group, View, QueryArgs, More) ->
    Accept = couch_httpd:header_value(Req, "Accept"),
    couch_httpd_view:view_etag(Db, Group, View, QueryArgs, {More, Accept, UserCtx#user_ctx.roles}).

output_list(map, Req, Db, DDoc, LName, View, QueryArgs, Etag, Keys, Group) ->
    output_map_list(Req, Db, DDoc, LName, View, QueryArgs, Etag, Keys, Group);
output_list(reduce, Req, Db, DDoc, LName, View, QueryArgs, Etag, Keys, Group) ->
    output_reduce_list(Req, Db, DDoc, LName, View, QueryArgs, Etag, Keys, Group).
    
% next step:
% use with_ddoc_proc/2 to make this simpler
output_map_list(Req, Db, DDoc, LName, View, QueryArgs, Etag, Keys, Group) ->
    #view_query_args{
        limit = Limit,
        skip = SkipCount
    } = QueryArgs,

    FoldAccInit = {Limit, SkipCount, undefined, []},
    {ok, RowCount} = couch_view:get_row_count(View),
    

    couch_query_servers:with_ddoc_proc(DDoc, fun(QServer) ->

        ListFoldHelpers = #view_fold_helper_funs{
            reduce_count = fun couch_view:reduce_to_count/1,
            start_response = StartListRespFun = make_map_start_resp_fun(QServer, Db, LName),
            send_row = make_map_send_row_fun(QServer)
        },
        CurrentSeq = Group#group.current_seq,

        {ok, _, FoldResult} = case Keys of
            nil ->
                FoldlFun = couch_httpd_view:make_view_fold_fun(Req, QueryArgs, Etag, Db, CurrentSeq, RowCount, ListFoldHelpers),
                    couch_view:fold(View, FoldlFun, FoldAccInit,
                    couch_httpd_view:make_key_options(QueryArgs));
            Keys ->
                lists:foldl(
                    fun(Key, {ok, _, FoldAcc}) ->
                        QueryArgs2 = QueryArgs#view_query_args{
                                start_key = Key,
                                end_key = Key
                            },
                        FoldlFun = couch_httpd_view:make_view_fold_fun(Req, QueryArgs2, Etag, Db, CurrentSeq, RowCount, ListFoldHelpers),
                        couch_view:fold(View, FoldlFun, FoldAcc,
                            couch_httpd_view:make_key_options(QueryArgs2))
                    end, {ok, nil, FoldAccInit}, Keys)
            end,
        finish_list(Req, QServer, Etag, FoldResult, StartListRespFun, CurrentSeq, RowCount)
    end).


output_reduce_list(Req, Db, DDoc, LName, View, QueryArgs, Etag, Keys, Group) ->
    #view_query_args{
        limit = Limit,
        skip = SkipCount,
        group_level = GroupLevel
    } = QueryArgs,

    CurrentSeq = Group#group.current_seq,

    couch_query_servers:with_ddoc_proc(DDoc, fun(QServer) ->
        StartListRespFun = make_reduce_start_resp_fun(QServer, Db, LName),
        SendListRowFun = make_reduce_send_row_fun(QServer, Db),
        {ok, GroupRowsFun, RespFun} = couch_httpd_view:make_reduce_fold_funs(Req,
            GroupLevel, QueryArgs, Etag, CurrentSeq,
            #reduce_fold_helper_funs{
                start_response = StartListRespFun,
                send_row = SendListRowFun
            }),
        FoldAccInit = {Limit, SkipCount, undefined, []},
        {ok, FoldResult} = case Keys of
            nil ->
                couch_view:fold_reduce(View, RespFun, FoldAccInit, [{key_group_fun, GroupRowsFun} |
                    couch_httpd_view:make_key_options(QueryArgs)]);
            Keys ->
                lists:foldl(
                    fun(Key, {ok, FoldAcc}) ->
                        couch_view:fold_reduce(View, RespFun, FoldAcc,
                            [{key_group_fun, GroupRowsFun} |
                                couch_httpd_view:make_key_options(
                                QueryArgs#view_query_args{start_key=Key, end_key=Key})]
                            )
                    end, {ok, FoldAccInit}, Keys)
            end,
        finish_list(Req, QServer, Etag, FoldResult, StartListRespFun, CurrentSeq, null)
    end).


make_map_start_resp_fun(QueryServer, Db, LName) ->
    fun(Req, Etag, TotalRows, Offset, _Acc, UpdateSeq) ->
        Head = {[{<<"total_rows">>, TotalRows}, {<<"offset">>, Offset}, {<<"update_seq">>, UpdateSeq}]},
        start_list_resp(QueryServer, LName, Req, Db, Head, Etag)
    end.

make_reduce_start_resp_fun(QueryServer, Db, LName) ->
    fun(Req2, Etag, _Acc, UpdateSeq) ->
        start_list_resp(QueryServer, LName, Req2, Db, {[{<<"update_seq">>, UpdateSeq}]}, Etag)
    end.

start_list_resp(QServer, LName, Req, Db, Head, Etag) ->
    JsonReq = couch_httpd_external:json_req_obj(Req, Db),
    [<<"start">>,Chunks,JsonResp] = couch_query_servers:ddoc_proc_prompt(QServer,
        [<<"lists">>, LName], [Head, JsonReq]),
    JsonResp2 = apply_etag(JsonResp, Etag),
    #extern_resp_args{
        code = Code,
        ctype = CType,
        headers = ExtHeaders
    } = couch_httpd_external:parse_external_response(JsonResp2),
    JsonHeaders = couch_httpd_external:default_or_content_type(CType, ExtHeaders),
    {ok, Resp} = start_chunked_response(Req, Code, JsonHeaders),
    {ok, Resp, ?b2l(?l2b(Chunks))}.

make_map_send_row_fun(QueryServer) ->
    fun(Resp, Db, Row, IncludeDocs, Conflicts, RowFront) ->
        send_list_row(
            Resp, QueryServer, Db, Row, RowFront, IncludeDocs, Conflicts)
    end.

make_reduce_send_row_fun(QueryServer, Db) ->
    fun(Resp, Row, RowFront) ->
        send_list_row(Resp, QueryServer, Db, Row, RowFront, false, false)
    end.

send_list_row(Resp, QueryServer, Db, Row, RowFront, IncludeDoc, Conflicts) ->
    try
        [Go,Chunks] = prompt_list_row(
            QueryServer, Db, Row, IncludeDoc, Conflicts),
        Chunk = RowFront ++ ?b2l(?l2b(Chunks)),
        send_non_empty_chunk(Resp, Chunk),
        case Go of
            <<"chunks">> ->
                {ok, ""};
            <<"end">> ->
                {stop, stop}
        end
    catch
        throw:Error ->
            send_chunked_error(Resp, Error),
            throw({already_sent, Resp, Error})
    end.


prompt_list_row({Proc, _DDocId}, Db, {{_Key, _DocId}, _} = Kv,
                IncludeDoc, Conflicts) ->
    JsonRow = couch_httpd_view:view_row_obj(Db, Kv, IncludeDoc, Conflicts),
    couch_query_servers:proc_prompt(Proc, [<<"list_row">>, JsonRow]);

prompt_list_row({Proc, _DDocId}, _, {Key, Value}, _IncludeDoc, _Conflicts) ->
    JsonRow = {[{key, Key}, {value, Value}]},
    couch_query_servers:proc_prompt(Proc, [<<"list_row">>, JsonRow]).

send_non_empty_chunk(Resp, Chunk) ->
    case Chunk of
        [] -> ok;
        _ -> send_chunk(Resp, Chunk)
    end.

finish_list(Req, {Proc, _DDocId}, Etag, FoldResult, StartFun, CurrentSeq, TotalRows) ->
    FoldResult2 = case FoldResult of
        {Limit, SkipCount, Response, RowAcc} ->
            {Limit, SkipCount, Response, RowAcc, nil};
        Else ->
            Else
    end,
    case FoldResult2 of
        {_, _, undefined, _, _} ->
            {ok, Resp, BeginBody} =
                render_head_for_empty_list(StartFun, Req, Etag, CurrentSeq, TotalRows),
            [<<"end">>, Chunks] = couch_query_servers:proc_prompt(Proc, [<<"list_end">>]),
            Chunk = BeginBody ++ ?b2l(?l2b(Chunks)),
            send_non_empty_chunk(Resp, Chunk);
        {_, _, Resp, stop, _} ->
            ok;
        {_, _, Resp, _, _} ->
            [<<"end">>, Chunks] = couch_query_servers:proc_prompt(Proc, [<<"list_end">>]),
            send_non_empty_chunk(Resp, ?b2l(?l2b(Chunks)))
    end,
    last_chunk(Resp).


render_head_for_empty_list(StartListRespFun, Req, Etag, CurrentSeq, null) ->
    StartListRespFun(Req, Etag, [], CurrentSeq); % for reduce
render_head_for_empty_list(StartListRespFun, Req, Etag, CurrentSeq, TotalRows) ->
    StartListRespFun(Req, Etag, TotalRows, null, [], CurrentSeq).

apply_etag({ExternalResponse}, CurrentEtag) ->
    % Here we embark on the delicate task of replacing or creating the
    % headers on the JsonResponse object. We need to control the Etag and
    % Vary headers. If the external function controls the Etag, we'd have to
    % run it to check for a match, which sort of defeats the purpose.
    case couch_util:get_value(<<"headers">>, ExternalResponse, nil) of
    nil ->
        % no JSON headers
        % add our Etag and Vary headers to the response
        {[{<<"headers">>, {[{<<"ETag">>, CurrentEtag}, {<<"Vary">>, <<"Accept">>}]}} | ExternalResponse]};
    JsonHeaders ->
        {[case Field of
        {<<"headers">>, JsonHeaders} -> % add our headers
            JsonHeadersEtagged = couch_util:json_apply_field({<<"ETag">>, CurrentEtag}, JsonHeaders),
            JsonHeadersVaried = couch_util:json_apply_field({<<"Vary">>, <<"Accept">>}, JsonHeadersEtagged),
            {<<"headers">>, JsonHeadersVaried};
        _ -> % skip non-header fields
            Field
        end || Field <- ExternalResponse]}
    end.

