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

-module(couch_httpd_external).

-compile(tuple_calls).

-export([send_external_response/2, json_req_obj/2, json_req_obj/3]).
-export([default_or_content_type/2, parse_external_response/1]).

-import(couch_httpd,[send_error/4]).

-include_lib("couch/include/couch_db.hrl").

json_req_obj(Req, Db) -> json_req_obj(Req, Db, null).
json_req_obj(#httpd{mochi_req=Req,
               method=Method,
               requested_path_parts=RequestedPath,
               path_parts=Path,
               req_body=ReqBody,
               peer=Peer
            }, Db, DocId) ->
    Body = case ReqBody of
        undefined ->
            MaxSize = config:get_integer("httpd", "max_http_request_size",
                4294967296),
            Req:recv_body(MaxSize);
        Else -> Else
    end,
    ParsedForm = case Req:get_primary_header_value("content-type") of
        "application/x-www-form-urlencoded" ++ _ ->
            case Body of
            undefined -> [];
            _ -> mochiweb_util:parse_qs(Body)
            end;
        _ ->
            []
    end,
    Headers = Req:get(headers),
    Hlist = mochiweb_headers:to_list(Headers),
    {ok, Info} = couch_db:get_db_info(Db),
    
% add headers...
    {[{<<"info">>, {Info}},
        {<<"id">>, DocId},
        {<<"uuid">>, couch_uuids:new()},
        {<<"method">>, Method},
        {<<"requested_path">>, RequestedPath},
        {<<"path">>, Path},
        {<<"raw_path">>, ?l2b(Req:get(raw_path))},
        {<<"query">>, json_query_keys(to_json_terms(Req:parse_qs()))},
        {<<"headers">>, to_json_terms(Hlist)},
        {<<"body">>, Body},
        {<<"peer">>, ?l2b(Peer)},
        {<<"form">>, to_json_terms(ParsedForm)},
        {<<"cookie">>, to_json_terms(Req:parse_cookie())},
        {<<"userCtx">>, couch_util:json_user_ctx(Db)},
        {<<"secObj">>, couch_db:get_security(Db)}]}.

to_json_terms(Data) ->
    to_json_terms(Data, []).

to_json_terms([], Acc) ->
    {lists:reverse(Acc)};
to_json_terms([{Key, Value} | Rest], Acc) when is_atom(Key) ->
    to_json_terms(Rest, [{list_to_binary(atom_to_list(Key)), list_to_binary(Value)} | Acc]);
to_json_terms([{Key, Value} | Rest], Acc) ->
    to_json_terms(Rest, [{list_to_binary(Key), list_to_binary(Value)} | Acc]).

json_query_keys({Json}) ->
    json_query_keys(Json, []).
json_query_keys([], Acc) ->
    {lists:reverse(Acc)};
json_query_keys([{<<"startkey">>, Value} | Rest], Acc) ->
    json_query_keys(Rest, [{<<"startkey">>, ?JSON_DECODE(Value)}|Acc]);
json_query_keys([{<<"endkey">>, Value} | Rest], Acc) ->
    json_query_keys(Rest, [{<<"endkey">>, ?JSON_DECODE(Value)}|Acc]);
json_query_keys([{<<"key">>, Value} | Rest], Acc) ->
    json_query_keys(Rest, [{<<"key">>, ?JSON_DECODE(Value)}|Acc]);
json_query_keys([Term | Rest], Acc) ->
    json_query_keys(Rest, [Term|Acc]).

send_external_response(Req, Response) ->
    #extern_resp_args{
        code = Code,
        data = Data,
        ctype = CType,
        headers = Headers,
        json = Json
    } = parse_external_response(Response),
    Headers1 = default_or_content_type(CType, Headers),
    case Json of
    nil ->
        couch_httpd:send_response(Req, Code, Headers1, Data);
    Json ->
        couch_httpd:send_json(Req, Code, Headers1, Json)
    end.

parse_external_response({Response}) ->
    lists:foldl(fun({Key,Value}, Args) ->
        case {Key, Value} of
            {"", _} ->
                Args;
            {<<"code">>, Value} ->
                Args#extern_resp_args{code=Value};
            {<<"stop">>, true} ->
                Args#extern_resp_args{stop=true};
            {<<"json">>, Value} ->
                Args#extern_resp_args{
                    json=Value,
                    ctype="application/json"};
            {<<"body">>, Value} ->
                Args#extern_resp_args{data=Value, ctype="text/html; charset=utf-8"};
            {<<"base64">>, Value} ->
                Args#extern_resp_args{
                    data=base64:decode(Value),
                    ctype="application/binary"
                };
            {<<"headers">>, {Headers}} ->
                NewHeaders = lists:map(fun({Header, HVal}) ->
                    {binary_to_list(Header), binary_to_list(HVal)}
                end, Headers),
                Args#extern_resp_args{headers=NewHeaders};
            _ -> % unknown key
                Msg = lists:flatten(io_lib:format("Invalid data from external server: ~p", [{Key, Value}])),
                throw({external_response_error, Msg})
            end
        end, #extern_resp_args{}, Response).

default_or_content_type(DefaultContentType, Headers) ->
    IsContentType = fun({X, _}) -> string:to_lower(X) == "content-type" end,
    case lists:any(IsContentType, Headers) of
    false ->
        [{"Content-Type", DefaultContentType} | Headers];
    true ->
        Headers
    end.
