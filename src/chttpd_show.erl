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

-module(chttpd_show).

-export([handle_doc_show_req/3, handle_doc_update_req/3, handle_view_list_req/3,
        handle_view_list/7, get_fun_key/3]).

-include("chttpd.hrl").

-import(chttpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,last_chunk/1,send_chunked_error/2,
    start_chunked_response/3, send_error/4]).

% /db/_design/foo/_show/bar/docid
% show converts a json doc to a response of any content-type. 
% it looks up the doc an then passes it to the query server.
% then it sends the response from the query server to the http client.

maybe_open_doc(Db, DocId) ->
    case fabric:open_doc(Db, DocId, [conflicts]) of
    {ok, Doc} ->
        Doc;
    {not_found, _} ->
        nil
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
    chttpd:etag_respond(Req, CurrentEtag, fun() ->
        JsonReq = chttpd_external:json_req_obj(Req, Db, DocId),
        JsonDoc = couch_query_servers:json_doc(Doc),
        [<<"resp">>, ExternalResp] = 
            couch_query_servers:ddoc_prompt(DDoc, [<<"shows">>, ShowName],
                [JsonDoc, JsonReq]),
        JsonResp = apply_etag(ExternalResp, CurrentEtag),
        chttpd_external:send_external_response(Req, JsonResp)
    end).


show_etag(#httpd{user_ctx=UserCtx}=Req, Doc, DDoc, More) ->
    Accept = chttpd:header_value(Req, "Accept"),
    DocPart = case Doc of
        nil -> nil;
        Doc -> chttpd:doc_etag(Doc)
    end,
    couch_httpd:make_etag({couch_httpd:doc_etag(DDoc), DocPart, Accept,
        UserCtx#user_ctx.roles, More}).

get_fun_key(#doc{body={Props}}, Type, Name) ->
    Lang = couch_util:get_value(<<"language">>, Props, <<"javascript">>),
    Src = couch_util:get_nested_json_value({Props}, [Type, Name]),
    {Lang, Src}.

% /db/_design/foo/update/bar/docid
% updates a doc based on a request
% handle_doc_update_req(#httpd{method = 'GET'}=Req, _Db, _DDoc) ->
%     % anything but GET
%     send_method_not_allowed(Req, "POST,PUT,DELETE,ETC");

handle_doc_update_req(#httpd{
        path_parts=[_, _, _, _, UpdateName, DocId]
    }=Req, Db, DDoc) ->
    Doc = maybe_open_doc(Db, DocId),
    send_doc_update_response(Req, Db, DDoc, UpdateName, Doc, DocId);

handle_doc_update_req(#httpd{
        path_parts=[_, _, _, _, UpdateName]
    }=Req, Db, DDoc) ->
    send_doc_update_response(Req, Db, DDoc, UpdateName, nil, null);

handle_doc_update_req(Req, _Db, _DDoc) ->
    send_error(Req, 404, <<"update_error">>, <<"Invalid path.">>).

send_doc_update_response(Req, Db, DDoc, UpdateName, Doc, DocId) ->
    JsonReq = chttpd_external:json_req_obj(Req, Db, DocId),
    JsonDoc = couch_query_servers:json_doc(Doc),
    Cmd = [<<"updates">>, UpdateName],
    case couch_query_servers:ddoc_prompt(DDoc, Cmd, [JsonDoc, JsonReq]) of
    [<<"up">>, {NewJsonDoc}, JsonResp] ->
        case chttpd:header_value(Req, "X-Couch-Full-Commit", "false") of
        "true" ->
            Options = [full_commit, {user_ctx, Req#httpd.user_ctx}];
        _ ->
            Options = [{user_ctx, Req#httpd.user_ctx}]
        end,
        NewDoc = couch_doc:from_json_obj({NewJsonDoc}),
        Code = 201,
        {ok, _NewRev} = fabric:update_doc(Db, NewDoc, Options);
    [<<"up">>, _Other, JsonResp] ->
        Code = 200
    end,
    JsonResp2 = json_apply_field({<<"code">>, Code}, JsonResp),
    % todo set location field
    chttpd_external:send_external_response(Req, JsonResp2).


% view-list request with view and list from same design doc.
handle_view_list_req(#httpd{method='GET',
        path_parts=[_DbName, _Design, DesignName, _List, ListName, ViewName]}=Req, Db, _) ->
    handle_view_list(Req, DesignName, ListName, DesignName, ViewName, Db, nil);

% view-list request with view and list from different design docs.
handle_view_list_req(#httpd{method='GET',
        path_parts=[_DbName, _Design, DesignName, _List, ListName, ViewDesignName, ViewName]}=Req, Db, _) ->
    handle_view_list(Req, DesignName, ListName, ViewDesignName, ViewName, Db, nil);

handle_view_list_req(#httpd{method='GET'}=Req, _Db, _) ->
    send_error(Req, 404, <<"list_error">>, <<"Invalid path.">>);

handle_view_list_req(#httpd{method='POST',
        path_parts=[_DbName, _Design, DesignName, _List, ListName, ViewName]}=Req, Db, _) ->
    ReqBody = chttpd:body(Req),
    {Props2} = ?JSON_DECODE(ReqBody),
    Keys = couch_util:get_value(<<"keys">>, Props2, nil),
    handle_view_list(Req#httpd{req_body=ReqBody}, DesignName, ListName, DesignName, ViewName, Db, Keys);

handle_view_list_req(Req, _Db, _) ->
    send_method_not_allowed(Req, "GET,POST,HEAD").

handle_view_list(Req, ListDesignName, ListName, ViewDesignName, ViewName, Db, Keys) ->
    ListDesignId = <<"_design/", ListDesignName/binary>>,
    #doc{body={ListProps}} = chttpd_db:couch_doc_open(Db, ListDesignId, nil, []),
    if
    ViewDesignName == ListDesignName ->
        ViewProps = ListProps,
        ViewDesignId = ListDesignId;
    true ->
        ViewDesignId = <<"_design/", ViewDesignName/binary>>,
        #doc{body={ViewProps}} = chttpd_db:couch_doc_open(Db, ViewDesignId, nil, [])
    end,

    ViewLang = couch_util:get_value(<<"language">>, ViewProps, <<"javascript">>),
    ListSrc = couch_util:get_nested_json_value({ListProps}, [<<"lists">>, ListName]),
    Group = couch_view_group:design_doc_to_view_group(Db, #doc{id=ViewDesignId,
        body={ViewProps}}),
    send_view_list_response(ViewLang, ListSrc, ViewName, ViewDesignId, Req, Db,
        Group, Keys).
    % send_view_list_response(ViewLang, ListSrc, ViewName, ViewDesignId, Req, Db, Keys).


send_view_list_response(Lang, ListSrc, ViewName, DesignId, Req, Db, Group, Keys) ->
    IsReduce = chttpd_view:get_reduce_type(Req),
    ViewType = chttpd_view:extract_view_type(ViewName, Group#group.views,
        IsReduce),
    QueryArgs = chttpd_view:parse_view_params(Req, Keys, ViewType),
    {ok, QueryServer} = couch_query_servers:start_view_list(Lang, ListSrc),
    StartListRespFun = make_map_start_resp_fun(QueryServer, Db),
    Etag = couch_uuids:new(),
    chttpd:etag_respond(Req, Etag, fun() ->
        {ok, Total, Result} = ?COUCH:list_view(Req, Db, DesignId, ViewName,
            Keys, QueryArgs, QueryServer),
        finish_list(Req, QueryServer, Etag, Result, StartListRespFun, Total)
    end).

send_view_list_response(Lang, ListSrc, ViewName, DesignId, Req, Db, Keys) ->
    Stale = chttpd_view:get_stale_type(Req),
    Reduce = chttpd_view:get_reduce_type(Req),
    case ?COUCH:get_map_view(Db, DesignId, ViewName, Stale) of
    {ok, View, Group} ->
        QueryArgs = chttpd_view:parse_view_params(Req, Keys, map),
        output_map_list(Req, Lang, ListSrc, View, Group, Db, QueryArgs, Keys);
    {not_found, _Reason} ->
        case ?COUCH:get_reduce_view(Db, DesignId, ViewName, Stale) of
        {ok, ReduceView, Group} ->
            case Reduce of
            false ->
                QueryArgs = chttpd_view:parse_view_params(
                    Req, Keys, map_red
                ),
                MapView = ?COUCH:extract_map_view(ReduceView),
                output_map_list(Req, Lang, ListSrc, MapView, Group, Db, QueryArgs, Keys);
            _ ->
                QueryArgs = chttpd_view:parse_view_params(
                    Req, Keys, reduce
                ),
                output_reduce_list(Req, Lang, ListSrc, ReduceView, Group, Db, QueryArgs, Keys)
            end;
        {not_found, Reason} ->
            throw({not_found, Reason})
        end
    end.


output_map_list(#httpd{mochi_req=MReq, user_ctx=UserCtx}=Req, Lang, ListSrc, View, Group, Db, QueryArgs, nil) ->
    #view_query_args{
        limit = Limit,
        direction = Dir,
        skip = SkipCount,
        start_key = StartKey,
        start_docid = StartDocId
    } = QueryArgs,
    {ok, RowCount} = ?COUCH:get_row_count(View),
    Start = {StartKey, StartDocId},
    Headers = MReq:get(headers),
    Hlist = mochiweb_headers:to_list(Headers),
    Accept = couch_util:get_value('Accept', Hlist),
    CurrentEtag = chttpd_view:view_group_etag(Group, Db, {Lang, ListSrc, Accept, UserCtx}),
    chttpd:etag_respond(Req, CurrentEtag, fun() ->
        % get the os process here
        % pass it into the view fold with closures
        {ok, QueryServer} = couch_query_servers:start_view_list(Lang, ListSrc),

        StartListRespFun = make_map_start_resp_fun(QueryServer, Db),
        SendListRowFun = make_map_send_row_fun(QueryServer),

        FoldlFun = chttpd_view:make_view_fold_fun(Req, QueryArgs, CurrentEtag, Db, RowCount,
            #view_fold_helper_funs{
                reduce_count = fun ?COUCH:reduce_to_count/1,
                start_response = StartListRespFun,
                send_row = SendListRowFun
            }),
        FoldAccInit = {Limit, SkipCount, undefined, [], nil},
        {ok, FoldResult} = ?COUCH:view_fold(View, Start, Dir, FoldlFun, FoldAccInit),
        finish_list(Req, QueryServer, CurrentEtag, FoldResult, StartListRespFun, RowCount)
    end);

output_map_list(#httpd{mochi_req=MReq, user_ctx=UserCtx}=Req, Lang, ListSrc, View, Group, Db, QueryArgs, Keys) ->
    #view_query_args{
        limit = Limit,
        direction = Dir,
        skip = SkipCount,
        start_docid = StartDocId
    } = QueryArgs,
    {ok, RowCount} = ?COUCH:get_row_count(View),
    Headers = MReq:get(headers),
    Hlist = mochiweb_headers:to_list(Headers),
    Accept = couch_util:get_value('Accept', Hlist),
    CurrentEtag = chttpd_view:view_group_etag(Group, Db, {Lang, ListSrc, Accept, UserCtx}),
    chttpd:etag_respond(Req, CurrentEtag, fun() ->
        % get the os process here
        % pass it into the view fold with closures
        {ok, QueryServer} = couch_query_servers:start_view_list(Lang, ListSrc),

        StartListRespFun = make_map_start_resp_fun(QueryServer, Db),
        SendListRowFun = make_map_send_row_fun(QueryServer),

        FoldAccInit = {Limit, SkipCount, undefined, [], nil},
        {ok, FoldResult} = lists:foldl(
            fun(Key, {ok, FoldAcc}) ->
                FoldlFun = chttpd_view:make_view_fold_fun(Req, QueryArgs#view_query_args{
                        start_key = Key,
                        end_key = Key
                    }, CurrentEtag, Db, RowCount,
                    #view_fold_helper_funs{
                        reduce_count = fun ?COUCH:reduce_to_count/1,
                        start_response = StartListRespFun,
                        send_row = SendListRowFun
                    }),
                ?COUCH:view_fold(View, {Key, StartDocId}, Dir, FoldlFun, FoldAcc)
            end, {ok, FoldAccInit}, Keys),
        finish_list(Req, QueryServer, CurrentEtag, FoldResult, StartListRespFun, RowCount)
    end).

make_map_start_resp_fun(QueryServer, Db) ->
    fun(Req, Etag, TotalRows, Offset, _Acc) ->
        Head = {[{<<"total_rows">>, TotalRows}, {<<"offset">>, Offset}]},
        start_list_resp(QueryServer, Req, Db, Head, Etag)
    end.

make_reduce_start_resp_fun(QueryServer, _Req, Db, _CurrentEtag) ->
    fun(Req2, Etag, _Acc) ->
        start_list_resp(QueryServer, Req2, Db, {[]}, Etag)
    end.

start_list_resp(QueryServer, Req, Db, Head, Etag) ->
    [<<"start">>,Chunks,JsonResp] = couch_query_servers:render_list_head(QueryServer,
        Req, Db, Head),
    JsonResp2 = apply_etag(JsonResp, Etag),
    #extern_resp_args{
        code = Code,
        ctype = CType,
        headers = ExtHeaders
    } = chttpd_external:parse_external_response(JsonResp2),
    JsonHeaders = chttpd_external:default_or_content_type(CType, ExtHeaders),
    {ok, Resp} = start_chunked_response(Req, Code, JsonHeaders),
    {ok, Resp, ?b2l(?l2b(Chunks))}.

make_map_send_row_fun(QueryServer) ->
    fun(Resp, Db, Row, IncludeDocs, RowFront) ->
        send_list_row(Resp, QueryServer, Db, Row, RowFront, IncludeDocs)
    end.

make_reduce_send_row_fun(QueryServer, Db) ->
    fun(Resp, Row, RowFront) ->
        send_list_row(Resp, QueryServer, Db, Row, RowFront, false)
    end.

send_list_row(Resp, QueryServer, Db, Row, RowFront, IncludeDoc) ->
    try
        [Go,Chunks] = couch_query_servers:render_list_row(QueryServer, Db, Row, IncludeDoc),
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

send_non_empty_chunk(Resp, Chunk) ->
    case Chunk of
        [] -> ok;
        _ -> send_chunk(Resp, Chunk)
    end.

output_reduce_list(#httpd{mochi_req=MReq, user_ctx=UserCtx}=Req, Lang, ListSrc, View, Group, Db, QueryArgs, nil) ->
    #view_query_args{
        limit = Limit,
        direction = Dir,
        skip = SkipCount,
        start_key = StartKey,
        start_docid = StartDocId,
        end_key = EndKey,
        end_docid = EndDocId,
        group_level = GroupLevel
    } = QueryArgs,
    Headers = MReq:get(headers),
    Hlist = mochiweb_headers:to_list(Headers),
    Accept = couch_util:get_value('Accept', Hlist),
    CurrentEtag = chttpd_view:view_group_etag(Group, Db, {Lang, ListSrc, Accept, UserCtx}),
    chttpd:etag_respond(Req, CurrentEtag, fun() ->
        % get the os process here
        % pass it into the view fold with closures
        {ok, QueryServer} = couch_query_servers:start_view_list(Lang, ListSrc),
        StartListRespFun = make_reduce_start_resp_fun(QueryServer, Req, Db, CurrentEtag),
        SendListRowFun = make_reduce_send_row_fun(QueryServer, Db),

        {ok, GroupRowsFun, RespFun} = chttpd_view:make_reduce_fold_funs(Req,
            GroupLevel, QueryArgs, CurrentEtag,
            #reduce_fold_helper_funs{
                start_response = StartListRespFun,
                send_row = SendListRowFun
            }),
        FoldAccInit = {Limit, SkipCount, undefined, []},
        {ok, FoldResult} = ?COUCH:view_fold_reduce(View, Dir, {StartKey, StartDocId},
            {EndKey, EndDocId}, GroupRowsFun, RespFun,
            FoldAccInit),
        finish_list(Req, QueryServer, CurrentEtag, FoldResult, StartListRespFun, null)
    end);

output_reduce_list(#httpd{mochi_req=MReq, user_ctx=UserCtx}=Req, Lang, ListSrc, View, Group, Db, QueryArgs, Keys) ->
    #view_query_args{
        limit = Limit,
        direction = Dir,
        skip = SkipCount,
        start_docid = StartDocId,
        end_docid = EndDocId,
        group_level = GroupLevel
    } = QueryArgs,
    Headers = MReq:get(headers),
    Hlist = mochiweb_headers:to_list(Headers),
    Accept = couch_util:get_value('Accept', Hlist),
    CurrentEtag = chttpd_view:view_group_etag(Group, Db, {Lang, ListSrc, Accept, UserCtx, Keys}),

    chttpd:etag_respond(Req, CurrentEtag, fun() ->
        % get the os process here
        % pass it into the view fold with closures
        {ok, QueryServer} = couch_query_servers:start_view_list(Lang, ListSrc),
        StartListRespFun = make_reduce_start_resp_fun(QueryServer, Req, Db, CurrentEtag),
        SendListRowFun = make_reduce_send_row_fun(QueryServer, Db),

        {ok, GroupRowsFun, RespFun} = chttpd_view:make_reduce_fold_funs(Req,
            GroupLevel, QueryArgs, CurrentEtag,
            #reduce_fold_helper_funs{
                start_response = StartListRespFun,
                send_row = SendListRowFun
            }),
        FoldAccInit = {Limit, SkipCount, undefined, []},
        {ok, FoldResult} = lists:foldl(
            fun(Key, {ok, FoldAcc}) ->
                ?COUCH:view_fold_reduce(View, Dir, {Key, StartDocId},
                    {Key, EndDocId}, GroupRowsFun, RespFun, FoldAcc)
            end, {ok, FoldAccInit}, Keys),
        finish_list(Req, QueryServer, CurrentEtag, FoldResult, StartListRespFun, null)
    end).

finish_list(Req, QueryServer, Etag, FoldResult, StartFun, TotalRows) ->
    FoldResult2 = case FoldResult of
        {Limit, SkipCount, Response, RowAcc} ->
            {Limit, SkipCount, Response, RowAcc, nil};
        Else ->
            Else
    end,
    case FoldResult2 of
        {_, _, undefined, _, _} ->
            {ok, Resp, BeginBody} =
                render_head_for_empty_list(StartFun, Req, Etag, TotalRows),
            [<<"end">>, Chunks] = couch_query_servers:render_list_tail(QueryServer),
            Chunk = BeginBody ++ ?b2l(?l2b(Chunks)),
            send_non_empty_chunk(Resp, Chunk);
        {_, _, Resp, stop, _} ->
            ok;
        {_, _, Resp, _, _} ->
            [<<"end">>, Chunks] = couch_query_servers:render_list_tail(QueryServer),
            send_non_empty_chunk(Resp, ?b2l(?l2b(Chunks)))
    end,
    couch_query_servers:stop_doc_map(QueryServer),
    send_chunk(Resp, []).


render_head_for_empty_list(StartListRespFun, Req, Etag, null) ->
    StartListRespFun(Req, Etag, []); % for reduce
render_head_for_empty_list(StartListRespFun, Req, Etag, TotalRows) ->
    StartListRespFun(Req, Etag, TotalRows, null, []).

% Maybe this is in the proplists API
% todo move to couch_util
json_apply_field(H, {L}) ->
    json_apply_field(H, L, []).
json_apply_field({Key, NewValue}, [{Key, _OldVal} | Headers], Acc) ->
    % drop matching keys
    json_apply_field({Key, NewValue}, Headers, Acc);
json_apply_field({Key, NewValue}, [{OtherKey, OtherVal} | Headers], Acc) ->
    % something else is next, leave it alone.
    json_apply_field({Key, NewValue}, Headers, [{OtherKey, OtherVal} | Acc]);
json_apply_field({Key, NewValue}, [], Acc) ->
    % end of list, add ours
    {[{Key, NewValue}|Acc]}.

apply_etag({ExternalResponse}, CurrentEtag) ->
    % Here we embark on the delicate task of replacing or creating the
    % headers on the JsonResponse object. We need to control the Etag and
    % Vary headers. If the external function controls the Etag, we'd have to
    % run it to check for a match, which sort of defeats the purpose.
    case couch_util:get_value(<<"headers">>, ExternalResponse, nil) of
    nil ->
        % no JSON headers
        % add our Etag and Vary headers to the response
        {[{<<"headers">>, {[{<<"Etag">>, CurrentEtag}, {<<"Vary">>, <<"Accept">>}]}} | ExternalResponse]};
    JsonHeaders ->
        {[case Field of
        {<<"headers">>, JsonHeaders} -> % add our headers
            JsonHeadersEtagged = json_apply_field({<<"Etag">>, CurrentEtag}, JsonHeaders),
            JsonHeadersVaried = json_apply_field({<<"Vary">>, <<"Accept">>}, JsonHeadersEtagged),
            {<<"headers">>, JsonHeadersVaried};
        _ -> % skip non-header fields
            Field
        end || Field <- ExternalResponse]}
    end.

