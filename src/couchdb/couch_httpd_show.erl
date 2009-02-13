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

-module(couch_httpd_show).
    
-export([handle_doc_show_req/2, handle_view_list_req/2]).


-include("couch_db.hrl").

-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,end_json_response/1,
    start_chunked_response/3, send_error/4]).
    
handle_doc_show_req(#httpd{
        method='GET',
        path_parts=[_, _, DesignName, ShowName, Docid]
    }=Req, Db) ->
    DesignId = <<"_design/", DesignName/binary>>,
    #doc{body={Props}} = couch_httpd_db:couch_doc_open(Db, DesignId, [], []),
    Lang = proplists:get_value(<<"language">>, Props, <<"javascript">>),
    ShowSrc = get_nested_json_value({Props}, [<<"shows">>, ShowName]),
    Doc = couch_httpd_db:couch_doc_open(Db, Docid, [], []),
    send_doc_show_response(Lang, ShowSrc, Doc, Req, Db);

handle_doc_show_req(#httpd{
        method='GET',
        path_parts=[_, _, DesignName, ShowName]
    }=Req, Db) ->
    DesignId = <<"_design/", DesignName/binary>>,
    #doc{body={Props}} = couch_httpd_db:couch_doc_open(Db, DesignId, [], []),
    Lang = proplists:get_value(<<"language">>, Props, <<"javascript">>),
    ShowSrc = get_nested_json_value({Props}, [<<"shows">>, ShowName]),
    send_doc_show_response(Lang, ShowSrc, nil, Req, Db);

handle_doc_show_req(#httpd{method='GET'}=Req, _Db) ->
    send_error(Req, 404, <<"show_error">>, <<"Invalid path.">>);

handle_doc_show_req(Req, _Db) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_view_list_req(#httpd{method='GET',path_parts=[_, _, DesignName, ListName, ViewName]}=Req, Db) ->
    DesignId = <<"_design/", DesignName/binary>>,
    #doc{body={Props}} = couch_httpd_db:couch_doc_open(Db, DesignId, [], []),
    Lang = proplists:get_value(<<"language">>, Props, <<"javascript">>),
    ListSrc = get_nested_json_value({Props}, [<<"lists">>, ListName]),
    send_view_list_response(Lang, ListSrc, ViewName, DesignId, Req, Db);

handle_view_list_req(Req, _Db) ->
    send_method_not_allowed(Req, "GET,HEAD").


get_nested_json_value({Props}, [Key|Keys]) ->
    case proplists:get_value(Key, Props, nil) of
    nil -> throw({not_found, <<"missing json key: ", Key/binary>>});
    Value -> get_nested_json_value(Value, Keys)
    end;
get_nested_json_value(Value, []) ->
    Value;
get_nested_json_value(_NotJSONObj, _) ->
    throw({not_found, json_mismatch}).

send_view_list_response(Lang, ListSrc, ViewName, DesignId, Req, Db) ->
    % TODO add etags when we get view etags
    #view_query_args{
        stale = Stale,
        reduce = Reduce
    } = QueryArgs = couch_httpd_view:parse_view_query(Req, nil, nil, true),
    case couch_view:get_map_view(Db, DesignId, ViewName, Stale) of
    {ok, View} ->    
        output_map_list(Req, Lang, ListSrc, View, Db, QueryArgs);
    {not_found, _Reason} ->
        case couch_view:get_reduce_view(Db, DesignId, ViewName, Stale) of
        {ok, ReduceView} ->
            case Reduce of
            false ->
                MapView = couch_view:extract_map_view(ReduceView),
                output_map_list(Req, Lang, ListSrc, MapView, Db, QueryArgs);
            _ ->
                throw({not_implemented, reduce_view_lists})
            end;
        {not_found, Reason} ->
            throw({not_found, Reason})
        end
    end.
    
output_map_list(Req, Lang, ListSrc, View, Db, QueryArgs) ->
    #view_query_args{
        limit = Limit,
        direction = Dir,
        skip = SkipCount,
        start_key = StartKey,
        start_docid = StartDocId
    } = QueryArgs,
    {ok, RowCount} = couch_view:get_row_count(View),
    Start = {StartKey, StartDocId},
    % get the os process here
    % pass it into the view fold with closures
    {ok, QueryServer} = couch_query_servers:start_view_list(Lang, ListSrc),

    StartListRespFun = fun(Req2, Code, TotalViewCount, Offset) ->
        JsonResp = couch_query_servers:render_list_head(QueryServer, 
            Req2, Db, TotalViewCount, Offset),
        #extern_resp_args{
            code = Code,
            data = BeginBody,
            ctype = CType,
            headers = Headers
        } = couch_httpd_external:parse_external_response(JsonResp),
        JsonHeaders = couch_httpd_external:default_or_content_type(CType, Headers),
        {ok, Resp} = start_chunked_response(Req, Code, JsonHeaders),
        {ok, Resp, binary_to_list(BeginBody)}
    end,
    
    SendListRowFun = fun(Resp, Db2, {{Key, DocId}, Value}, 
        RowFront, _IncludeDocs) ->
        JsonResp = couch_query_servers:render_list_row(QueryServer, 
            Req, Db2, {{Key, DocId}, Value}),
        #extern_resp_args{
            stop = StopIter,
            data = RowBody
        } = couch_httpd_external:parse_external_response(JsonResp),
        RowFront2 = case RowFront of
        nil -> [];
        _ -> RowFront
        end,
        case StopIter of
        true -> stop;
        _ -> send_chunk(Resp, RowFront2 ++ binary_to_list(RowBody))
        end
    end,
    
    FoldlFun = couch_httpd_view:make_view_fold_fun(Req, QueryArgs, Db, RowCount,
        #view_fold_helper_funs{
            reduce_count = fun couch_view:reduce_to_count/1,
            start_response = StartListRespFun,
            send_row = SendListRowFun
        }),
    FoldAccInit = {Limit, SkipCount, undefined, []},
    FoldResult = couch_view:fold(View, Start, Dir, FoldlFun, FoldAccInit),
    finish_view_list(Req, Db, QueryServer, RowCount, FoldResult, StartListRespFun).

finish_view_list(Req, Db, QueryServer, TotalRows, 
    FoldResult, StartListRespFun) ->
    case FoldResult of
    {ok, {_, _, undefined, _}} ->
        {ok, Resp, BeginBody} = StartListRespFun(Req, 200, TotalRows, null),
        JsonTail = couch_query_servers:render_list_tail(QueryServer, Req, Db),
        #extern_resp_args{
            data = Tail
        } = couch_httpd_external:parse_external_response(JsonTail),
        send_chunk(Resp, BeginBody ++ Tail),
        send_chunk(Resp, []);
    {ok, {_, _, Resp, _AccRevRows}} ->
        JsonTail = couch_query_servers:render_list_tail(QueryServer, Req, Db),
        #extern_resp_args{
            data = Tail
        } = couch_httpd_external:parse_external_response(JsonTail),
        send_chunk(Resp, Tail),
        send_chunk(Resp, []);
    Error ->
        throw(Error)
    end.

send_doc_show_response(Lang, ShowSrc, nil, #httpd{mochi_req=MReq}=Req, Db) ->
    % compute etag with no doc
    Headers = MReq:get(headers),
    Hlist = mochiweb_headers:to_list(Headers),
    Accept = proplists:get_value('Accept', Hlist),
    CurrentEtag = couch_httpd:make_etag({Lang, ShowSrc, nil, Accept}),
    couch_httpd:etag_respond(Req, CurrentEtag, fun() -> 
        ExternalResp = couch_query_servers:render_doc_show(Lang, ShowSrc, 
            nil, Req, Db),
        JsonResp = apply_etag(ExternalResp, CurrentEtag),
        couch_httpd_external:send_external_response(Req, JsonResp)
    end);

send_doc_show_response(Lang, ShowSrc, #doc{revs=[DocRev|_]}=Doc, #httpd{mochi_req=MReq}=Req, Db) ->
    % calculate the etag
    Headers = MReq:get(headers),
    Hlist = mochiweb_headers:to_list(Headers),
    Accept = proplists:get_value('Accept', Hlist),
    CurrentEtag = couch_httpd:make_etag({Lang, ShowSrc, DocRev, Accept}),
    % We know our etag now    
    couch_httpd:etag_respond(Req, CurrentEtag, fun() -> 
        ExternalResp = couch_query_servers:render_doc_show(Lang, ShowSrc, 
            Doc, Req, Db),
        JsonResp = apply_etag(ExternalResp, CurrentEtag),
        couch_httpd_external:send_external_response(Req, JsonResp)
    end).

set_or_replace_header(H, L) ->
    set_or_replace_header(H, L, []).

set_or_replace_header({Key, NewValue}, [{Key, _OldVal} | Headers], Acc) ->
    % drop matching keys
    set_or_replace_header({Key, NewValue}, Headers, Acc);
set_or_replace_header({Key, NewValue}, [{OtherKey, OtherVal} | Headers], Acc) ->
    % something else is next, leave it alone.
    set_or_replace_header({Key, NewValue}, Headers, [{OtherKey, OtherVal} | Acc]);
set_or_replace_header({Key, NewValue}, [], Acc) ->
    % end of list, add ours
    [{Key, NewValue}|Acc].

apply_etag({ExternalResponse}, CurrentEtag) ->
    % Here we embark on the delicate task of replacing or creating the  
    % headers on the JsonResponse object. We need to control the Etag and 
    % Vary headers. If the external function controls the Etag, we'd have to 
    % run it to check for a match, which sort of defeats the purpose.
    case proplists:get_value(<<"headers">>, ExternalResponse, nil) of
    nil ->
        % no JSON headers
        % add our Etag and Vary headers to the response
        {[{<<"headers">>, {[{<<"Etag">>, CurrentEtag}, {<<"Vary">>, <<"Accept">>}]}} | ExternalResponse]};
    {JsonHeaders} ->
        {[case Field of
        {<<"headers">>, {JsonHeaders}} -> % add our headers
            JsonHeadersEtagged = set_or_replace_header({<<"Etag">>, CurrentEtag}, JsonHeaders),
            JsonHeadersVaried = set_or_replace_header({<<"Vary">>, <<"Accept">>}, JsonHeadersEtagged),
            {<<"headers">>, {JsonHeadersVaried}};
        _ -> % skip non-header fields
            Field
        end || Field <- ExternalResponse]}
    end.
