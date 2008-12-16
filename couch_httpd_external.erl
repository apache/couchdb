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

-module(couch_httpd_external).

-export([handle_external_req/2]).

-import(couch_httpd,[send_error/4]).

-include("couch_db.hrl").

-record(extern_resp_args, {
    code = 200,
    data = <<>>,
    ctype = "application/json",
    headers = []
}).

handle_external_req(#httpd{mochi_req=Req, 
                        method=Verb,
                        path_parts=[_DbName, _External, UrlName | Path]
                    }=HttpReq, Db) ->
    ReqBody = Req:recv_body(),
    ParsedForm = case Req:get_primary_header_value("content-type") of
        "application/x-www-form-urlencoded" ++ _ ->
            mochiweb_util:parse_qs(ReqBody);
        _ ->
            []
    end,
    Response = couch_external_manager:execute(binary_to_list(UrlName), 
        Db, Verb, Path, Req:parse_qs(), ReqBody, ParsedForm,
        Req:parse_cookie()),
        
    case Response of
    {unknown_external_server, Msg} ->
        send_error(HttpReq, 404, <<"external_server_error">>, Msg);
    _ ->
        send_external_response(Req, Response)
    end;
handle_external_req(#httpd{path_parts=[_, _]}=Req, _Db) ->
    send_error(Req, 404, <<"external_server_error">>, <<"No server name specified.">>);
handle_external_req(Req, _) ->
    send_error(Req, 404, <<"external_server_error">>, <<"Broken assumption">>).

send_external_response(Req, Response) ->
    #extern_resp_args{
        code = Code,
        data = Data,
        ctype = CType,
        headers = Headers
    } = parse_external_response(Response),
    ?LOG_DEBUG("External Response ~p",[Response]),
    Resp = Req:respond({Code, 
        default_or_content_type(CType, Headers), chunked}),
    Resp:write_chunk(Data),
    Resp:write_chunk(""),
    {ok, Resp}.

parse_external_response({Response}) ->
    lists:foldl(fun({Key,Value}, Args) ->
        case {Key, Value} of
            {"", _} ->
                Args;
            {<<"code">>, Value} ->
                Args#extern_resp_args{code=Value};
            {<<"json">>, Value} ->
                Args#extern_resp_args{
                    data=?JSON_ENCODE(Value),
                    ctype="application/json"};
            {<<"body">>, Value} ->
                Args#extern_resp_args{data=Value, ctype="text/html"};
            {<<"headers">>, {Headers}} ->
                NewHeaders = lists:map(fun({Header, HVal}) ->
                    {binary_to_list(Header), binary_to_list(HVal)}
                end, Headers),
                Args#extern_resp_args{headers=NewHeaders};
            _ -> % unknown key
                Msg = lists:flatten(io_lib:format("Invalid data from external server: ~s = ~p", [Key, Value])),
                throw({external_response_error, Msg})
            end
        end, #extern_resp_args{}, Response).

default_or_content_type(DefaultContentType, Headers) ->
    {ContentType, OtherHeaders} = lists:partition(
        fun({HeaderName, _}) ->
            HeaderName == "Content-Type"
        end, Headers),

    % XXX: What happens if we were passed multiple content types? We add another?
    case ContentType of
        [{"Content-Type", SetContentType}] ->
            TrueContentType = SetContentType;
        _Else ->
            TrueContentType = DefaultContentType
    end,

    HeadersWithContentType = lists:append(OtherHeaders, [{"Content-Type", TrueContentType}]),
    HeadersWithContentType.
