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

-module(couch_rep_att).

-export([convert_stub/2, cleanup/0]).

-include("couch_db.hrl").

convert_stub(#att{data=stub, name=Name} = Attachment,
	     {#http_db{} = Db, Id, Rev}) ->
    {Pos, [RevId|_]} = Rev,
    Request = Db#http_db{
        resource = lists:flatten([couch_util:url_encode(Id), "/",
            couch_util:url_encode(Name)]),
        qs = [{rev, couch_doc:rev_to_str({Pos,RevId})}]
    },
    Ref = make_ref(),
    RcvFun = fun() -> attachment_receiver(Ref, Request) end,
    Attachment#att{data=RcvFun}.

cleanup() ->
    receive
    {ibrowse_async_response, _, _} ->
        %% TODO maybe log, didn't expect to have data here
        cleanup();
    {ibrowse_async_response_end, _} ->
        cleanup();
    {ibrowse_async_headers, _, _, _} ->
        cleanup()
    after 0 ->
        erase(),
        ok
    end.
        
% internal funs

attachment_receiver(Ref, Request) ->
    try case get(Ref) of
    undefined ->
        {ReqId, ContentEncoding} = start_http_request(Request),
        put(Ref, {ReqId, ContentEncoding}),
        receive_data(Ref, ReqId, ContentEncoding);
    {ReqId, ContentEncoding} ->
        receive_data(Ref, ReqId, ContentEncoding)
    end
    catch
    throw:{attachment_request_failed, _} ->
        case {Request#http_db.retries, Request#http_db.pause} of
        {0, _} ->
             ?LOG_INFO("request for ~p failed", [Request#http_db.resource]),
             throw({attachment_request_failed, max_retries_reached});
        {N, Pause} when N > 0 ->
            ?LOG_INFO("request for ~p timed out, retrying in ~p seconds",
                [Request#http_db.resource, Pause/1000]),
            timer:sleep(Pause),
            cleanup(),
            attachment_receiver(Ref, Request#http_db{retries = N-1})
        end
    end.

receive_data(Ref, ReqId, ContentEncoding) ->
    receive
    {ibrowse_async_response, ReqId, {chunk_start,_}} ->
        receive_data(Ref, ReqId, ContentEncoding);
    {ibrowse_async_response, ReqId, chunk_end} ->
        receive_data(Ref, ReqId, ContentEncoding);
    {ibrowse_async_response, ReqId, {error, Err}} ->
        ?LOG_ERROR("streaming attachment ~p failed with ~p", [ReqId, Err]),
        throw({attachment_request_failed, Err});
    {ibrowse_async_response, ReqId, Data} ->
        % ?LOG_DEBUG("got ~p bytes for ~p", [size(Data), ReqId]),
        Data;
    {ibrowse_async_response_end, ReqId} ->
        ?LOG_ERROR("streaming att. ended but more data requested ~p", [ReqId]),
        throw({attachment_request_failed, premature_end})
    after 31000 ->
        throw({attachment_request_failed, timeout})
    end.

start_http_request(Req) ->
    %% set stream_to here because self() has changed
    Req2 = Req#http_db{options = [{stream_to,self()} | Req#http_db.options]},
    {ibrowse_req_id, ReqId} = couch_rep_httpc:request(Req2),
    receive {ibrowse_async_headers, ReqId, Code, Headers} ->
        case validate_headers(Req2, list_to_integer(Code), Headers) of
        {ok, ContentEncoding} ->
            {ReqId, ContentEncoding};
        {ok, ContentEncoding, NewReqId} ->
            {NewReqId, ContentEncoding}
        end
    after 10000 ->
        throw({attachment_request_failed, timeout})
    end.

validate_headers(_Req, 200, Headers) ->
    MochiHeaders = mochiweb_headers:make(Headers),
    {ok, mochiweb_headers:get_value("Content-Encoding", MochiHeaders)};
validate_headers(Req, Code, Headers) when Code > 299, Code < 400 ->
    Url = couch_rep_httpc:redirect_url(Headers, Req#http_db.url),
    NewReq = couch_rep_httpc:redirected_request(Req, Url),
    {ibrowse_req_id, ReqId} = couch_rep_httpc:request(NewReq),
    receive {ibrowse_async_headers, ReqId, NewCode, NewHeaders} ->
        {ok, Encoding} = validate_headers(NewReq, list_to_integer(NewCode),
            NewHeaders)
    end,
    {ok, Encoding, ReqId};
validate_headers(Req, Code, _Headers) ->
    #http_db{url=Url, resource=Resource} = Req,
    ?LOG_ERROR("got ~p for ~s~s", [Code, Url, Resource]),
    throw({attachment_request_failed, {bad_code, Code}}).
