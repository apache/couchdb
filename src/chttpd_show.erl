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

-export([handle_doc_show_req/3, handle_doc_update_req/3, handle_view_list_req/3]).

-include_lib("couch/include/couch_db.hrl").

-record(lacc, {
    req,
    resp = nil,
    qserver,
    lname,
    db,
    etag
}).

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
    chttpd:send_error(Req, 404, <<"show_error">>, <<"Invalid path.">>).

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
    chttpd:send_error(Req, 404, <<"update_error">>, <<"Invalid path.">>).

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
        path_parts=[_, _, DesignName, _, ListName, ViewName]}=Req, Db, DDoc) ->
    handle_view_list(Req, Db, DDoc, ListName, {DesignName, ViewName}, nil);

% view-list request with view and list from different design docs.
handle_view_list_req(#httpd{method='GET',
        path_parts=[_, _, _, _, ListName, DesignName, ViewName]}=Req, Db, DDoc) ->
    handle_view_list(Req, Db, DDoc, ListName, {DesignName, ViewName}, nil);

handle_view_list_req(#httpd{method='GET'}=Req, _Db, _DDoc) ->
    chttpd:send_error(Req, 404, <<"list_error">>, <<"Invalid path.">>);

handle_view_list_req(#httpd{method='POST',
        path_parts=[_, _, DesignName, _, ListName, ViewName]}=Req, Db, DDoc) ->
    ReqBody = couch_httpd:body(Req),
    {Props2} = ?JSON_DECODE(ReqBody),
    Keys = proplists:get_value(<<"keys">>, Props2, nil),
    handle_view_list(Req#httpd{req_body=ReqBody}, Db, DDoc, ListName,
        {DesignName, ViewName}, Keys);

handle_view_list_req(#httpd{method='POST',
        path_parts=[_, _, _, _, ListName, DesignName, ViewName]}=Req, Db, DDoc) ->
    ReqBody = couch_httpd:body(Req),
    {Props2} = ?JSON_DECODE(ReqBody),
    Keys = proplists:get_value(<<"keys">>, Props2, nil),
    handle_view_list(Req#httpd{req_body=ReqBody}, Db, DDoc, ListName,
        {DesignName, ViewName}, Keys);

handle_view_list_req(#httpd{method='POST'}=Req, _Db, _DDoc) ->
    chttpd:send_error(Req, 404, <<"list_error">>, <<"Invalid path.">>);

handle_view_list_req(Req, _Db, _DDoc) ->
    chttpd:send_method_not_allowed(Req, "GET,POST,HEAD").

handle_view_list(Req, Db, DDoc, LName, {ViewDesignName, ViewName}, Keys) ->
    {ok, VDoc} = fabric:open_doc(Db, <<"_design/", ViewDesignName/binary>>, []),
    Group = couch_view_group:design_doc_to_view_group(VDoc),
    IsReduce = chttpd_view:get_reduce_type(Req),
    ViewType = chttpd_view:extract_view_type(ViewName, Group#group.views,
        IsReduce),
    QueryArgs = chttpd_view:parse_view_params(Req, Keys, ViewType),
    CB = fun list_callback/2,
    Etag = couch_uuids:new(),
    chttpd:etag_respond(Req, Etag, fun() ->
        couch_query_servers:with_ddoc_proc(DDoc, fun(QServer) ->
            Acc0 = #lacc{
                lname = LName,
                req = Req,
                qserver = QServer,
                db = Db,
                etag = Etag
            },
            fabric:query_view(Db, VDoc, ViewName, CB, Acc0, QueryArgs)
        end)
    end).

list_callback({total_and_offset, Total, Offset}, #lacc{resp=nil} = Acc) ->
    start_list_resp({[{<<"total_rows">>, Total}, {<<"offset">>, Offset}]}, Acc);
list_callback({total_and_offset, _, _}, Acc) ->
    % a sorted=false view where the message came in late.  Ignore.
    {ok, Acc};
list_callback({row, Row}, #lacc{resp=nil} = Acc) ->
    % first row of a reduce view, or a sorted=false view
    {ok, NewAcc} = start_list_resp({[]}, Acc),
    send_list_row(Row, NewAcc);
list_callback({row, Row}, Acc) ->
    send_list_row(Row, Acc);
list_callback(complete, Acc) ->
    #lacc{qserver = {Proc, _}, resp = Resp0} = Acc,
    if Resp0 =:= nil ->
        {ok, #lacc{resp = Resp}} = start_list_resp({[]}, Acc);
    true ->
        Resp = Resp0
    end,
    try couch_query_servers:proc_prompt(Proc, [<<"list_end">>]) of
    [<<"end">>, Chunk] ->
        {ok, Resp1} = send_non_empty_chunk(Resp, Chunk),
        chttpd:send_delayed_last_chunk(Resp1)
    catch Error ->
        {ok, Resp1} = chttpd:send_delayed_error(Resp, Error),
        {stop, Resp1}
    end;
list_callback({error, Reason}, #lacc{resp=Resp}) ->
    chttpd:send_delayed_error(Resp, Reason).

start_list_resp(Head, Acc) ->
    #lacc{
        req = Req,
        db = Db,
        qserver = QServer,
        lname = LName,
        etag = Etag
    } = Acc,

    % use a separate process because we're already in a receive loop, and
    % json_req_obj calls fabric:get_db_info()
    spawn_monitor(fun() -> exit(chttpd_external:json_req_obj(Req, Db)) end),
    receive {'DOWN', _, _, _, JsonReq} -> ok end,

    [<<"start">>,Chunk,JsonResp] = couch_query_servers:ddoc_proc_prompt(QServer,
        [<<"lists">>, LName], [Head, JsonReq]),
    JsonResp2 = apply_etag(JsonResp, Etag),
    #extern_resp_args{
        code = Code,
        ctype = CType,
        headers = ExtHeaders
    } = couch_httpd_external:parse_external_response(JsonResp2),
    JsonHeaders = couch_httpd_external:default_or_content_type(CType, ExtHeaders),
    {ok, Resp} = chttpd:start_delayed_chunked_response(Req, Code,
        JsonHeaders, Chunk),
    {ok, Acc#lacc{resp=Resp}}.

send_list_row(Row, #lacc{qserver = {Proc, _}, resp = Resp} = Acc) ->
    try couch_query_servers:proc_prompt(Proc, [<<"list_row">>, Row]) of
    [<<"chunks">>, Chunk] ->
        {ok, Resp1} = send_non_empty_chunk(Resp, Chunk),
        {ok, Acc#lacc{resp=Resp1}};
    [<<"end">>, Chunk] ->
        {ok, Resp1} = send_non_empty_chunk(Resp, Chunk),
        {ok, Resp2} = chttpd:send_delayed_last_chunk(Resp1),
        {stop, Resp2}
    catch Error ->
        {ok, Resp1} = chttpd:send_delayed_error(Resp, Error),
        {stop, Resp1}
    end.

send_non_empty_chunk(Resp, []) ->
    {ok, Resp};
send_non_empty_chunk(Resp, Chunk) ->
    chttpd:send_delayed_chunk(Resp, Chunk).

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

