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

-module(couch_mrview_show).

-export([
    handle_doc_show_req/3,
    handle_doc_update_req/3,
    handle_view_list_req/3
]).

-include("couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-record(lacc, {
    db,
    req,
    resp,
    qserver,
    lname,
    etag
}).

% /db/_design/foo/_show/bar/docid
% show converts a json doc to a response of any content-type.
% it looks up the doc an then passes it to the query server.
% then it sends the response from the query server to the http client.

maybe_open_doc(Db, DocId) ->
    case catch couch_httpd_db:couch_doc_open(Db, DocId, nil, [conflicts]) of
        #doc{} = Doc -> Doc;
        {not_found, _} -> nil
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
    couch_httpd:send_error(Req, 404, <<"show_error">>, <<"Invalid path.">>).

handle_doc_show(Req, Db, DDoc, ShowName, Doc) ->
    handle_doc_show(Req, Db, DDoc, ShowName, Doc, null).

handle_doc_show(Req, Db, DDoc, ShowName, Doc, DocId) ->
    % get responder for ddoc/showname
    CurrentEtag = show_etag(Req, Doc, DDoc, []),
    couch_httpd:etag_respond(Req, CurrentEtag, fun() ->
        JsonReq = couch_httpd_external:json_req_obj(Req, Db, DocId),
        JsonDoc = couch_query_servers:json_doc(Doc),
        [<<"resp">>, ExternalResp] =
            couch_query_servers:ddoc_prompt(DDoc, [<<"shows">>, ShowName],
                [JsonDoc, JsonReq]),
        JsonResp = apply_etag(ExternalResp, CurrentEtag),
        couch_httpd_external:send_external_response(Req, JsonResp)
    end).


show_etag(#httpd{user_ctx=UserCtx}=Req, Doc, DDoc, More) ->
    Accept = couch_httpd:header_value(Req, "Accept"),
    DocPart = case Doc of
        nil -> nil;
        Doc -> couch_httpd:doc_etag(Doc)
    end,
    couch_httpd:make_etag({couch_httpd:doc_etag(DDoc), DocPart, Accept,
        {UserCtx#user_ctx.name, UserCtx#user_ctx.roles}, More}).

% updates a doc based on a request
% handle_doc_update_req(#httpd{method = 'GET'}=Req, _Db, _DDoc) ->
%     % anything but GET
%     send_method_not_allowed(Req, "POST,PUT,DELETE,ETC");

% This call is creating a new doc using an _update function to
% modify the provided request body.
% /db/_design/foo/_update/bar
handle_doc_update_req(#httpd{
        path_parts=[_, _, _, _, UpdateName]
    }=Req, Db, DDoc) ->
    send_doc_update_response(Req, Db, DDoc, UpdateName, nil, null);

% /db/_design/foo/_update/bar/docid
handle_doc_update_req(#httpd{
        path_parts=[_, _, _, _, UpdateName | DocIdParts]
    }=Req, Db, DDoc) ->
    DocId = ?l2b(string:join([?b2l(P) || P <- DocIdParts], "/")),
    Doc = maybe_open_doc(Db, DocId),
    send_doc_update_response(Req, Db, DDoc, UpdateName, Doc, DocId);


handle_doc_update_req(Req, _Db, _DDoc) ->
    couch_httpd:send_error(Req, 404, <<"update_error">>, <<"Invalid path.">>).

send_doc_update_response(Req, Db, DDoc, UpdateName, Doc, DocId) ->
    JsonReq = couch_httpd_external:json_req_obj(Req, Db, DocId),
    JsonDoc = couch_query_servers:json_doc(Doc),
    Cmd = [<<"updates">>, UpdateName],
    UpdateResp = couch_query_servers:ddoc_prompt(DDoc, Cmd, [JsonDoc, JsonReq]),
    JsonResp = case UpdateResp of
        [<<"up">>, {NewJsonDoc}, {JsonResp0}] ->
            case couch_httpd:header_value(
                    Req, "X-Couch-Full-Commit", "false") of
                "true" ->
                    Options = [full_commit, {user_ctx, Req#httpd.user_ctx}];
                _ ->
                    Options = [{user_ctx, Req#httpd.user_ctx}]
            end,
            NewDoc = couch_doc:from_json_obj({NewJsonDoc}),
            couch_doc:validate_docid(NewDoc#doc.id),
            {ok, NewRev} = couch_db:update_doc(Db, NewDoc, Options),
            NewRevStr = couch_doc:rev_to_str(NewRev),
            DocIdHeader = case DocId of
                              null -> 
                                  [{<<"json">>, {Props}}] = JsonResp0,
                                  case lists:keyfind(<<"id">>, 1, Props) of
                                      {_, NewDocId} -> 
                                          [{<<"X-Couch-Id">>, NewDocId}];
                                      false ->
                                          []
                                  end;
                              DocId -> 
                                  [{<<"X-Couch-Id">>, DocId}]
                          end,    
            {[
                {<<"code">>, 201},
                {<<"headers">>, {[{<<"X-Couch-Update-NewRev">>, NewRevStr}] ++ DocIdHeader}}
                | JsonResp0]};
        [<<"up">>, _Other, {JsonResp0}] ->
            {[{<<"code">>, 200} | JsonResp0]}
    end,
    % todo set location field
    couch_httpd_external:send_external_response(Req, JsonResp).


handle_view_list_req(#httpd{method=Method}=Req, Db, DDoc)
    when Method =:= 'GET' orelse Method =:= 'OPTIONS' ->
    case Req#httpd.path_parts of
        [_, _, _DName, _, LName, VName] ->
            % Same design doc for view and list
            handle_view_list(Req, Db, DDoc, LName, DDoc, VName, undefined);
        [_, _, _, _, LName, DName, VName] ->
            % Different design docs for view and list
            VDocId = <<"_design/", DName/binary>>,
            {ok, VDDoc} = couch_db:open_doc(Db, VDocId, [ejson_body]),
            handle_view_list(Req, Db, DDoc, LName, VDDoc, VName, undefined);
        _ ->
            couch_httpd:send_error(Req, 404, <<"list_error">>, <<"Bad path.">>)
    end;
handle_view_list_req(#httpd{method='POST'}=Req, Db, DDoc) ->
    {Props} = couch_httpd:json_body_obj(Req),
    Keys = proplists:get_value(<<"keys">>, Props),
    case Req#httpd.path_parts of
        [_, _, _DName, _, LName, VName] ->
            handle_view_list(Req, Db, DDoc, LName, DDoc, VName, Keys);
        [_, _, _, _, LName, DName, VName] ->
            % Different design docs for view and list
            VDocId = <<"_design/", DName/binary>>,
            {ok, VDDoc} = couch_db:open_doc(Db, VDocId, [ejson_body]),
            handle_view_list(Req, Db, DDoc, LName, VDDoc, VName, Keys);
        _ ->
            couch_httpd:send_error(Req, 404, <<"list_error">>, <<"Bad path.">>)
    end;
handle_view_list_req(Req, _Db, _DDoc) ->
    couch_httpd:send_method_not_allowed(Req, "GET,POST,HEAD").


handle_view_list(Req, Db, DDoc, LName, VDDoc, VName, Keys) ->
    Args0 = couch_mrview_http:parse_qs(Req, Keys),
    ETagFun = fun(BaseSig, Acc0) ->
        UserCtx = Req#httpd.user_ctx,
        Name = UserCtx#user_ctx.name,
        Roles = UserCtx#user_ctx.roles,
        Accept = couch_httpd:header_value(Req, "Accept"),
        Parts = {couch_httpd:doc_etag(DDoc), Accept, {Name, Roles}},
        ETag = couch_httpd:make_etag({BaseSig, Parts}),
        case couch_httpd:etag_match(Req, ETag) of
            true -> throw({etag_match, ETag});
            false -> {ok, Acc0#lacc{etag=ETag}}
        end
    end,
    Args = Args0#mrargs{preflight_fun=ETagFun},
    couch_httpd:etag_maybe(Req, fun() ->
        couch_query_servers:with_ddoc_proc(DDoc, fun(QServer) ->
            Acc = #lacc{db=Db, req=Req, qserver=QServer, lname=LName},
            couch_mrview:query_view(Db, VDDoc, VName, Args, fun list_cb/2, Acc)
        end)
    end).


list_cb({meta, Meta}, #lacc{resp=undefined} = Acc) ->
    MetaProps = case couch_util:get_value(total, Meta) of
        undefined -> [];
        Total -> [{total_rows, Total}]
    end ++ case couch_util:get_value(offset, Meta) of
        undefined -> [];
        Offset -> [{offset, Offset}]
    end ++ case couch_util:get_value(update_seq, Meta) of
        undefined -> [];
        UpdateSeq -> [{update_seq, UpdateSeq}]
    end,
    start_list_resp({MetaProps}, Acc);
list_cb({row, Row}, #lacc{resp=undefined} = Acc) ->
    {ok, NewAcc} = start_list_resp({[]}, Acc),
    send_list_row(Row, NewAcc);
list_cb({row, Row}, Acc) ->
    send_list_row(Row, Acc);
list_cb(complete, Acc) ->
    #lacc{qserver = {Proc, _}, resp = Resp0} = Acc,
    if Resp0 =:= nil ->
        {ok, #lacc{resp = Resp}} = start_list_resp({[]}, Acc);
    true ->
        Resp = Resp0
    end,
    [<<"end">>, Data] = couch_query_servers:proc_prompt(Proc, [<<"list_end">>]),
    send_non_empty_chunk(Resp, Data),
    couch_httpd:last_chunk(Resp),
    {ok, Resp}.

start_list_resp(Head, Acc) ->
    #lacc{db=Db, req=Req, qserver=QServer, lname=LName, etag=ETag} = Acc,
    JsonReq = couch_httpd_external:json_req_obj(Req, Db),

    [<<"start">>,Chunk,JsonResp] = couch_query_servers:ddoc_proc_prompt(QServer,
        [<<"lists">>, LName], [Head, JsonReq]),
    JsonResp2 = apply_etag(JsonResp, ETag),
    #extern_resp_args{
        code = Code,
        ctype = CType,
        headers = ExtHeaders
    } = couch_httpd_external:parse_external_response(JsonResp2),
    JsonHeaders = couch_httpd_external:default_or_content_type(CType, ExtHeaders),
    {ok, Resp} = couch_httpd:start_chunked_response(Req, Code, JsonHeaders),
    send_non_empty_chunk(Resp, Chunk),
    {ok, Acc#lacc{resp=Resp}}.

send_list_row(Row, #lacc{qserver = {Proc, _}, resp = Resp} = Acc) ->
    RowObj = case couch_util:get_value(id, Row) of
        undefined -> [];
        Id -> [{id, Id}]
    end ++ case couch_util:get_value(key, Row) of
        undefined -> [];
        Key -> [{key, Key}]
    end ++ case couch_util:get_value(value, Row) of
        undefined -> [];
        Val -> [{value, Val}]
    end ++ case couch_util:get_value(doc, Row) of
        undefined -> [];
        Doc -> [{doc, Doc}]
    end,
    try couch_query_servers:proc_prompt(Proc, [<<"list_row">>, {RowObj}]) of
    [<<"chunks">>, Chunk] ->
        send_non_empty_chunk(Resp, Chunk),
        {ok, Acc};
    [<<"end">>, Chunk] ->
        send_non_empty_chunk(Resp, Chunk),
        couch_httpd:last_chunk(Resp),
        {stop, Acc}
    catch Error ->
        couch_httpd:send_chunked_error(Resp, Error),
        {stop, Acc}
    end.

send_non_empty_chunk(_, []) ->
    ok;
send_non_empty_chunk(Resp, Chunk) ->
    couch_httpd:send_chunk(Resp, Chunk).


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

