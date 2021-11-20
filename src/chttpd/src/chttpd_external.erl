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

-module(chttpd_external).

-compile(tuple_calls).

-export([send_external_response/2]).
-export([json_req_obj_fields/0, json_req_obj/2, json_req_obj/3, json_req_obj/4]).
-export([default_or_content_type/2, parse_external_response/1]).

-import(chttpd, [send_error/4]).

-include_lib("couch/include/couch_db.hrl").

json_req_obj(Req, Db) ->
    json_req_obj(Req, Db, null).
json_req_obj(Req, Db, DocId) ->
    json_req_obj(Req, Db, DocId, all).
json_req_obj(Req, Db, DocId, all) ->
    Fields = json_req_obj_fields(),
    json_req_obj(Req, Db, DocId, Fields);
json_req_obj(Req, Db, DocId, Fields) when is_list(Fields) ->
    {[{Field, json_req_obj_field(Field, Req, Db, DocId)} || Field <- Fields]}.

json_req_obj_fields() ->
    [
        <<"info">>,
        <<"uuid">>,
        <<"id">>,
        <<"method">>,
        <<"requested_path">>,
        <<"path">>,
        <<"raw_path">>,
        <<"query">>,
        <<"headers">>,
        <<"body">>,
        <<"peer">>,
        <<"form">>,
        <<"cookie">>,
        <<"userCtx">>,
        <<"secObj">>
    ].

json_req_obj_field(<<"info">>, #httpd{}, Db, _DocId) ->
    {ok, Info} = get_db_info(Db),
    {Info};
json_req_obj_field(<<"uuid">>, #httpd{}, _Db, _DocId) ->
    couch_uuids:new();
json_req_obj_field(<<"id">>, #httpd{}, _Db, DocId) ->
    DocId;
json_req_obj_field(<<"method">>, #httpd{method = Method}, _Db, _DocId) ->
    Method;
json_req_obj_field(<<"requested_path">>, #httpd{requested_path_parts = Path}, _Db, _DocId) ->
    Path;
json_req_obj_field(<<"path">>, #httpd{path_parts = Path}, _Db, _DocId) ->
    Path;
json_req_obj_field(<<"raw_path">>, #httpd{mochi_req = Req}, _Db, _DocId) ->
    ?l2b(Req:get(raw_path));
json_req_obj_field(<<"query">>, #httpd{mochi_req = Req}, _Db, _DocId) ->
    json_query_keys(to_json_terms(Req:parse_qs()));
json_req_obj_field(<<"headers">>, #httpd{mochi_req = Req}, _Db, _DocId) ->
    Headers = Req:get(headers),
    Hlist = mochiweb_headers:to_list(Headers),
    to_json_terms(Hlist);
json_req_obj_field(<<"body">>, #httpd{req_body = undefined, mochi_req = Req}, _Db, _DocId) ->
    MaxSize = chttpd_util:get_chttpd_config_integer(
        "max_http_request_size", 4294967296
    ),
    try
        Req:recv_body(MaxSize)
    catch
        exit:normal ->
            exit({bad_request, <<"Invalid request body">>})
    end;
json_req_obj_field(<<"body">>, #httpd{req_body = Body}, _Db, _DocId) ->
    Body;
json_req_obj_field(<<"peer">>, #httpd{peer = undefined, mochi_req = Req}, _, _) ->
    ?l2b(Req:get(peer));
json_req_obj_field(<<"peer">>, #httpd{peer = Peer}, _Db, _DocId) ->
    ?l2b(Peer);
json_req_obj_field(<<"form">>, #httpd{mochi_req = Req, method = Method} = HttpReq, Db, DocId) ->
    Body = json_req_obj_field(<<"body">>, HttpReq, Db, DocId),
    ParsedForm =
        case Req:get_primary_header_value("content-type") of
            "application/x-www-form-urlencoded" ++ _ when
                Method =:= 'POST' orelse Method =:= 'PUT'
            ->
                mochiweb_util:parse_qs(Body);
            _ ->
                []
        end,
    to_json_terms(ParsedForm);
json_req_obj_field(<<"cookie">>, #httpd{mochi_req = Req}, _Db, _DocId) ->
    to_json_terms(Req:parse_cookie());
json_req_obj_field(<<"userCtx">>, #httpd{}, Db, _DocId) ->
    couch_util:json_user_ctx(Db);
json_req_obj_field(<<"secObj">>, #httpd{user_ctx = UserCtx}, Db, _DocId) ->
    get_db_security(Db, UserCtx).

get_db_info(Db) ->
    case couch_db:is_clustered(Db) of
        true ->
            fabric:get_db_info(Db);
        false ->
            couch_db:get_db_info(Db)
    end.

get_db_security(Db, #user_ctx{}) ->
    case couch_db:is_clustered(Db) of
        true ->
            fabric:get_security(Db);
        false ->
            couch_db:get_security(Db)
    end.

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
    json_query_keys(Rest, [{<<"startkey">>, ?JSON_DECODE(Value)} | Acc]);
json_query_keys([{<<"endkey">>, Value} | Rest], Acc) ->
    json_query_keys(Rest, [{<<"endkey">>, ?JSON_DECODE(Value)} | Acc]);
json_query_keys([{<<"key">>, Value} | Rest], Acc) ->
    json_query_keys(Rest, [{<<"key">>, ?JSON_DECODE(Value)} | Acc]);
json_query_keys([{<<"descending">>, Value} | Rest], Acc) ->
    json_query_keys(Rest, [{<<"descending">>, ?JSON_DECODE(Value)} | Acc]);
json_query_keys([Term | Rest], Acc) ->
    json_query_keys(Rest, [Term | Acc]).

send_external_response(Req, Response) ->
    #extern_resp_args{
        code = Code,
        data = Data,
        ctype = CType,
        headers = Headers0,
        json = Json
    } = parse_external_response(Response),
    Headers1 = default_or_content_type(CType, Headers0),
    case Json of
        nil ->
            Headers2 = chttpd_util:maybe_add_csp_header("showlist", Headers1, "sandbox"),
            chttpd:send_response(Req, Code, Headers2, Data);
        Json ->
            chttpd:send_json(Req, Code, Headers1, Json)
    end.

parse_external_response({Response}) ->
    lists:foldl(
        fun({Key, Value}, Args) ->
            case {Key, Value} of
                {"", _} ->
                    Args;
                {<<"code">>, Value} ->
                    Args#extern_resp_args{code = Value};
                {<<"stop">>, true} ->
                    Args#extern_resp_args{stop = true};
                {<<"json">>, Value} ->
                    Args#extern_resp_args{
                        json = Value,
                        ctype = "application/json"
                    };
                {<<"body">>, Value} ->
                    Args#extern_resp_args{data = Value, ctype = "text/html; charset=utf-8"};
                {<<"base64">>, Value} ->
                    Args#extern_resp_args{
                        data = base64:decode(Value),
                        ctype = "application/binary"
                    };
                {<<"headers">>, {Headers}} ->
                    NewHeaders = lists:map(
                        fun({Header, HVal}) ->
                            {couch_util:to_list(Header), couch_util:to_list(HVal)}
                        end,
                        Headers
                    ),
                    Args#extern_resp_args{headers = NewHeaders};
                % unknown key
                _ ->
                    Msg = lists:flatten(
                        io_lib:format("Invalid data from external server: ~p", [{Key, Value}])
                    ),
                    throw({external_response_error, Msg})
            end
        end,
        #extern_resp_args{},
        Response
    ).

default_or_content_type(DefaultContentType, Headers) ->
    IsContentType = fun({X, _}) -> string:to_lower(X) == "content-type" end,
    case lists:any(IsContentType, Headers) of
        false ->
            [{"Content-Type", DefaultContentType} | Headers];
        true ->
            Headers
    end.
