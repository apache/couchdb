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
    handle_view_list_req/3,
    list_cb/2
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

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
    chttpd:make_etag({chttpd:doc_etag(DDoc), DocPart, Accept,
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
    chttpd:send_error(Req, 404, <<"update_error">>, <<"Invalid path.">>).

send_doc_update_response(Req, Db, DDoc, UpdateName, Doc, DocId) ->
    JsonReq = chttpd_external:json_req_obj(Req, Db, DocId),
    JsonDoc = couch_query_servers:json_doc(Doc),
    Cmd = [<<"updates">>, UpdateName],
    UpdateResp = couch_query_servers:ddoc_prompt(DDoc, Cmd, [JsonDoc, JsonReq]),
    JsonResp = case UpdateResp of
        [<<"up">>, {NewJsonDoc}, {JsonResp0}] ->
            case chttpd:header_value(
                    Req, "X-Couch-Full-Commit", "false") of
                "true" ->
                    Options = [full_commit, {user_ctx, Req#httpd.user_ctx}];
                _ ->
                    Options = [{user_ctx, Req#httpd.user_ctx}]
            end,
            NewDoc = couch_doc:from_json_obj_validate({NewJsonDoc}),
            couch_doc:validate_docid(NewDoc#doc.id),
            {ok, NewRev} = couch_db:update_doc(Db, NewDoc, Options),
            NewRevStr = couch_doc:rev_to_str(NewRev),
            {JsonResp1} = apply_headers(JsonResp0, [
                {<<"X-Couch-Update-NewRev">>, NewRevStr},
                {<<"X-Couch-Id">>, NewDoc#doc.id}
            ]),
            {[{<<"code">>, 201} | JsonResp1]};
        [<<"up">>, _Other, {JsonResp0}] ->
            {[{<<"code">>, 200} | JsonResp0]}
    end,
    % todo set location field
    chttpd_external:send_external_response(Req, JsonResp).


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
            chttpd:send_error(Req, 404, <<"list_error">>, <<"Bad path.">>)
    end;
handle_view_list_req(#httpd{method='POST'}=Req, Db, DDoc) ->
    chttpd:validate_ctype(Req, "application/json"),
    {Props} = chttpd:json_body_obj(Req),
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
            chttpd:send_error(Req, 404, <<"list_error">>, <<"Bad path.">>)
    end;
handle_view_list_req(Req, _Db, _DDoc) ->
    chttpd:send_method_not_allowed(Req, "GET,POST,HEAD").


handle_view_list(Req, Db, DDoc, LName, VDDoc, VName, Keys) ->
    Args0 = couch_mrview_http:parse_params(Req, Keys),
    ETagFun = fun(BaseSig, Acc0) ->
        UserCtx = Req#httpd.user_ctx,
        Name = UserCtx#user_ctx.name,
        Roles = UserCtx#user_ctx.roles,
        Accept = chttpd:header_value(Req, "Accept"),
        Parts = {chttpd:doc_etag(DDoc), Accept, {Name, Roles}},
        ETag = chttpd:make_etag({BaseSig, Parts}),
        case chttpd:etag_match(Req, ETag) of
            true -> throw({etag_match, ETag});
            false -> {ok, Acc0#lacc{etag=ETag}}
        end
    end,
    Args = Args0#mrargs{preflight_fun=ETagFun},
    couch_httpd:etag_maybe(Req, fun() ->
        couch_query_servers:with_ddoc_proc(DDoc, fun(QServer) ->
            Acc = #lacc{db=Db, req=Req, qserver=QServer, lname=LName},
            case VName of
              <<"_all_docs">> ->
                couch_mrview:query_all_docs(Db, Args, fun list_cb/2, Acc);
              _ ->
                couch_mrview:query_view(Db, VDDoc, VName, Args, fun list_cb/2, Acc)
            end
        end)
    end).


list_cb({meta, Meta}, #lacc{code=undefined} = Acc) ->
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
list_cb({row, Row}, #lacc{code=undefined} = Acc) ->
    {ok, NewAcc} = start_list_resp({[]}, Acc),
    send_list_row(Row, NewAcc);
list_cb({row, Row}, Acc) ->
    send_list_row(Row, Acc);
list_cb(complete, Acc) ->
    #lacc{qserver = {Proc, _}, req = Req, resp = Resp0} = Acc,
    if Resp0 =:= nil ->
        {ok, #lacc{resp = Resp}} = start_list_resp({[]}, Acc);
    true ->
        Resp = Resp0
    end,
    case couch_query_servers:proc_prompt(Proc, [<<"list_end">>]) of
        [<<"end">>, Data, Headers] ->
            Acc2 = fixup_headers(Headers, Acc#lacc{resp=Resp}),
            #lacc{resp = Resp2} = send_non_empty_chunk(Acc2, Data);
        [<<"end">>, Data] ->
            #lacc{resp = Resp2} = send_non_empty_chunk(Acc#lacc{resp=Resp}, Data)
    end,
    last_chunk(Req, Resp2),
    {ok, Resp2}.

start_list_resp(Head, Acc) ->
    #lacc{db=Db, req=Req, qserver=QServer, lname=LName} = Acc,
    JsonReq = json_req_obj(Req, Db),

    [<<"start">>,Chunk,JsonResp] = couch_query_servers:ddoc_proc_prompt(QServer,
        [<<"lists">>, LName], [Head, JsonReq]),
    Acc2 = send_non_empty_chunk(fixup_headers(JsonResp, Acc), Chunk),
    {ok, Acc2}.

fixup_headers(Headers, #lacc{etag=ETag} = Acc) ->
    Headers2 = apply_etag(Headers, ETag),
    #extern_resp_args{
        code = Code,
        ctype = CType,
        headers = ExtHeaders
    } = chttpd_external:parse_external_response(Headers2),
    Headers3 = chttpd_external:default_or_content_type(CType, ExtHeaders),
    Acc#lacc{code=Code, headers=Headers3}.

send_list_row(Row, #lacc{qserver = {Proc, _}, req = Req, resp = Resp} = Acc) ->
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
    [<<"chunks">>, Chunk, Headers] ->
        Acc2 = send_non_empty_chunk(fixup_headers(Headers, Acc), Chunk),
        {ok, Acc2};
    [<<"chunks">>, Chunk] ->
        Acc2 = send_non_empty_chunk(Acc, Chunk),
        {ok, Acc2};
    [<<"end">>, Chunk, Headers] ->
        #lacc{resp = Resp2} = send_non_empty_chunk(fixup_headers(Headers, Acc), Chunk),
        {ok, Resp3} = last_chunk(Req, Resp2),
        {stop, Resp3};
    [<<"end">>, Chunk] ->
        #lacc{resp = Resp2} = send_non_empty_chunk(Acc, Chunk),
        {ok, Resp3} = last_chunk(Req, Resp2),
        {stop, Resp3}
    catch Error ->
        {ok, Resp2} = case Resp of
            undefined ->
                {Code, _, _} = chttpd:error_info(Error),
                #lacc{req=Req, headers=Headers} = Acc,
                chttpd:start_chunked_response(Req, Code, Headers);
            _ ->
                {ok, Resp}
        end,
        {ok, Resp3} = chttpd:send_chunked_error(Resp2, Error),
        {stop, Resp3}
    end.

send_non_empty_chunk(Acc, []) ->
    Acc;
send_non_empty_chunk(#lacc{resp=undefined} = Acc, Chunk) ->
    #lacc{req=Req, code=Code, headers=Headers} = Acc,
    {ok, Resp} = chttpd:start_chunked_response(Req, Code, Headers),
    send_non_empty_chunk(Acc#lacc{resp = Resp}, Chunk);
send_non_empty_chunk(#lacc{resp=Resp} = Acc, Chunk) ->
    chttpd:send_chunk(Resp, Chunk),
    Acc.


apply_etag(JsonResp, undefined) ->
    JsonResp;
apply_etag({ExternalResponse}, CurrentEtag) ->
    % Here we embark on the delicate task of replacing or creating the
    % headers on the JsonResponse object. We need to control the Etag and
    % Vary headers. If the external function controls the Etag, we'd have to
    % run it to check for a match, which sort of defeats the purpose.
    apply_headers(ExternalResponse, [
        {<<"ETag">>, CurrentEtag},
        {<<"Vary">>, <<"Accept">>}
    ]).

apply_headers(JsonResp, []) ->
    JsonResp;
apply_headers(JsonResp, NewHeaders) ->
    case couch_util:get_value(<<"headers">>, JsonResp) of
        undefined ->
            {[{<<"headers">>, {NewHeaders}}| JsonResp]};
        JsonHeaders ->
            Headers = apply_headers1(JsonHeaders, NewHeaders),
            NewKV = {<<"headers">>, Headers},
            {lists:keyreplace(<<"headers">>, 1, JsonResp, NewKV)}
    end.
apply_headers1(JsonHeaders, [{Key, Value} | Rest]) ->
    NewJsonHeaders = json_apply_field({Key, Value}, JsonHeaders),
    apply_headers1(NewJsonHeaders, Rest);
apply_headers1(JsonHeaders, []) ->
    JsonHeaders.


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


% This loads the db info if we have a fully loaded db record, but we might not
% have the db locally on this node, so then load the info through fabric.
json_req_obj(Req, Db) ->
    case couch_db:is_clustered(Db) of
        true ->
            % use a separate process because we're already in a receive loop,
            % and json_req_obj calls fabric:get_db_info()
            JRO = fun() -> exit(chttpd_external:json_req_obj(Req, Db)) end,
            spawn_monitor(JRO),
            receive {'DOWN', _, _, _, JsonReq} -> JsonReq end;
        false ->
            chttpd_external:json_req_obj(Req, Db)
    end.

last_chunk(Req, undefined) ->
    chttpd:send_response(Req, 200, [], <<"">>);
last_chunk(_Req, Resp) ->
    chttpd:send_chunk(Resp, []).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

apply_headers_test_() ->
    [
        should_apply_headers(),
        should_apply_headers_with_merge(),
        should_apply_headers_with_merge_overwrite()
    ].

should_apply_headers() ->
    ?_test(begin
        JsonResp = [{<<"code">>, 201}],
        Headers  = [{<<"foo">>, <<"bar">>}],
        {Props} = apply_headers(JsonResp, Headers),
        JsonHeaders = couch_util:get_value(<<"headers">>, Props),
        ?assertEqual({Headers}, JsonHeaders)
    end).

should_apply_headers_with_merge() ->
    ?_test(begin
        BaseHeaders = [{<<"bar">>, <<"baz">>}],
        NewHeaders  = [{<<"foo">>, <<"bar">>}],
        JsonResp = [
            {<<"code">>, 201},
            {<<"headers">>, {BaseHeaders}}
        ],
        {Props} = apply_headers(JsonResp, NewHeaders),
        JsonHeaders = couch_util:get_value(<<"headers">>, Props),
        ExpectedHeaders = {NewHeaders ++ BaseHeaders},
        ?assertEqual(ExpectedHeaders, JsonHeaders)
    end).

should_apply_headers_with_merge_overwrite() ->
    ?_test(begin
        BaseHeaders = [{<<"foo">>, <<"bar">>}],
        NewHeaders  = [{<<"foo">>, <<"baz">>}],
        JsonResp = [
            {<<"code">>, 201},
            {<<"headers">>, {BaseHeaders}}
        ],
        {Props} = apply_headers(JsonResp, NewHeaders),
        JsonHeaders = couch_util:get_value(<<"headers">>, Props),
        ?assertEqual({NewHeaders}, JsonHeaders)
    end).


send_list_row_test_() ->
    Cases = couch_tests_combinatorics:product([
        [
            {"[<<\"end\">>, [], []]", fun(_, _) -> [<<"end">>, [], []] end},
            {"[<<\"end\">>, []]", fun(_, _) -> [<<"end">>, []] end},
            {"throw(timeout)", fun(_, _) -> throw(timeout) end}
        ],
        [
            req,
            undefined
        ]]),
    {"Ensure send_list_row returns a valid response on end or error",
        {setup, fun setup/0, fun(_) -> meck:unload() end, [
            {
                lists:flatten(io_lib:format("~s -- ~p", [N, R])),
                should_return_valid_response(F, R)
            } || [{N, F}, R] <- Cases
        ]}
    }.

setup() ->
    ok = meck:expect(chttpd, send_chunk,
        fun(Resp, _) -> {ok, Resp} end),
    ok = meck:expect(chttpd, send_chunked_error,
        fun(Resp, _) -> {ok, Resp} end),
    ok = meck:expect(chttpd, start_chunked_response,
        fun(_, _, _) -> {ok, resp} end),
    ok = meck:expect(chttpd_external, parse_external_response, 1,
        #extern_resp_args{headers = []}).

should_return_valid_response(Spec, Req) ->
    ?_test(begin
        ok = meck:expect(couch_query_servers, proc_prompt, Spec),
        Acc = #lacc{qserver = {proc, undefined}, req = Req, resp = resp},
        ?assertEqual({stop, resp}, send_list_row([], Acc))
    end).

-endif.
