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

-module(couch_httpd).
-include("couch_db.hrl").

-export([start_link/0, stop/0, handle_request/3]).

-export([header_value/2,header_value/3,qs_value/2,qs_value/3,qs/1,path/1]).
-export([check_is_admin/1,unquote/1]).
-export([parse_form/1,json_body/1,body/1,doc_etag/1]).
-export([primary_header_value/2,partition/1,serve_file/3]).
-export([start_chunked_response/3,send_chunk/2]).
-export([start_json_response/2, start_json_response/3, end_json_response/1]).
-export([send_response/4,send_method_not_allowed/2,send_error/4]).
-export([send_json/2,send_json/3,send_json/4]).


% Maximum size of document PUT request body (4GB)
-define(MAX_DOC_SIZE, (4*1024*1024*1024)).

start_link() ->
    % read config and register for configuration changes

    % just stop if one of the config settings change. couch_server_sup
    % will restart us and then we will pick up the new settings.

    BindAddress = couch_config:get("httpd", "bind_address", any),
    Port = couch_config:get("httpd", "port", "5984"),
    
    UrlHandlersList = lists:map(
        fun({UrlKey, SpecStr}) ->
            case couch_util:parse_term(SpecStr) of
            {ok, {M, F, A}} ->
                {list_to_binary(UrlKey), fun(Req) -> apply(M, F, [Req, A]) end};
            {ok, {M, F}} ->
                {list_to_binary(UrlKey), fun(Req) -> apply(M, F, [Req]) end}
            end
        end, couch_config:get("httpd_global_handlers")),
        
    DbUrlHandlersList = lists:map(
        fun({UrlKey, SpecStr}) ->
            case couch_util:parse_term(SpecStr) of
            {ok, {M, F, A}} ->
                {list_to_binary(UrlKey),
                    fun(Req, Db) -> apply(M, F, [Req, Db, A]) end};
            {ok, {M, F}} ->
                {list_to_binary(UrlKey),
                    fun(Req, Db) -> apply(M, F, [Req, Db]) end}
            end
        end, couch_config:get("httpd_db_handlers")),
    UrlHandlers = dict:from_list(UrlHandlersList),
    DbUrlHandlers = dict:from_list(DbUrlHandlersList),
    Loop = fun(Req)->
            apply(?MODULE, handle_request, [Req, UrlHandlers, DbUrlHandlers])
        end,

    % and off we go
    {ok, Pid} = mochiweb_http:start([
        {loop, Loop},
        {name, ?MODULE},
        {ip, BindAddress},
        {port, Port}
    ]),
    ok = couch_config:register(
        fun("httpd", "bind_address") ->
            ?MODULE:stop();
        ("httpd", "port") ->
            ?MODULE:stop();
        ("httpd_global_handlers", _) ->
            ?MODULE:stop();
        ("httpd_db_handlers", _) ->
            ?MODULE:stop()
        end, Pid),

    {ok, Pid}.

stop() ->
    mochiweb_http:stop(?MODULE).
    

handle_request(MochiReq, UrlHandlers, DbUrlHandlers) ->

    % for the path, use the raw path with the query string and fragment
    % removed, but URL quoting left intact
    RawUri = MochiReq:get(raw_path),
    {"/" ++ Path, _, _} = mochiweb_util:urlsplit_path(RawUri),
    
    HandlerKey =
    case mochiweb_util:partition(Path, "/") of
    {"", "", ""} ->
        <<"/">>; % Special case the root url handler
    {FirstPart, _, _} ->
        list_to_binary(FirstPart)
    end,
    ?LOG_DEBUG("~p ~s ~p~nHeaders: ~p", [
        MochiReq:get(method),
        RawUri,
        MochiReq:get(version),
        mochiweb_headers:to_list(MochiReq:get(headers))
    ]),
    
    Method =
    case MochiReq:get(method) of
        % alias HEAD to GET as mochiweb takes care of stripping the body
        'HEAD' -> 'GET';
        
        % already an atom
        Meth when is_atom(Meth) -> Meth;
        
        % Non standard HTTP verbs aren't atoms (COPY, MOVE etc) so convert when
        % possible (if any module references the atom, then it's existing).
        Meth -> try list_to_existing_atom(Meth) catch _ -> Meth end
    end,
    HttpReq = #httpd{
        mochi_req = MochiReq,
        method = Method,
        path_parts = [list_to_binary(couch_httpd:unquote(Part))
                || Part <- string:tokens(Path, "/")],
        db_url_handlers = DbUrlHandlers
        },
    
    DefaultFun = fun couch_httpd_db:handle_request/1,
    HandlerFun = couch_util:dict_find(HandlerKey, UrlHandlers, DefaultFun),
    
    {ok, Resp} =
    try
        HandlerFun(HttpReq)
    catch
        Error ->
            send_error(HttpReq, Error)
    end,

    ?LOG_INFO("~s - - ~p ~s ~B", [
        MochiReq:get(peer),
        MochiReq:get(method),
        RawUri,
        Resp:get(code)
    ]),
    {ok, Resp}.



% Utilities

partition(Path) ->
    mochiweb_util:partition(Path, "/").

header_value(#httpd{mochi_req=MochiReq}, Key) ->
    MochiReq:get_header_value(Key).

header_value(#httpd{mochi_req=MochiReq}, Key, Default) ->
    case MochiReq:get_header_value(Key) of
    undefined -> Default;
    Value -> Value
    end.

primary_header_value(#httpd{mochi_req=MochiReq}, Key) ->
    MochiReq:get_primary_header_value(Key).
    
serve_file(#httpd{mochi_req=MochiReq}, RelativePath, DocumentRoot) ->
    {ok, MochiReq:serve_file(RelativePath, DocumentRoot, server_header())}.

qs_value(Req, Key) ->
    qs_value(Req, Key, undefined).
    
qs_value(Req, Key, Default) ->
    proplists:get_value(Key, qs(Req), Default).

qs(#httpd{mochi_req=MochiReq}) ->
    MochiReq:parse_qs().

path(#httpd{mochi_req=MochiReq}) ->
    MochiReq:get(path).

unquote(UrlEncodedString) ->
    mochiweb_util:unquote(UrlEncodedString).

parse_form(#httpd{mochi_req=MochiReq}) ->
    mochiweb_multipart:parse_form(MochiReq).

body(#httpd{mochi_req=MochiReq}) ->
    MochiReq:recv_body(?MAX_DOC_SIZE).

json_body(#httpd{mochi_req=MochiReq}) ->
    ?JSON_DECODE(MochiReq:recv_body(?MAX_DOC_SIZE)).

doc_etag(#doc{revs=[DiskRev|_]}) ->
     "\"" ++ binary_to_list(DiskRev) ++ "\"".

check_is_admin(Req) ->
    IsNamedAdmin =
    case header_value(Req, "Authorization") of
    "Basic " ++ Base64Value ->
        [User, Pass] =
            string:tokens(?b2l(couch_util:decodeBase64(Base64Value)),":"),
        couch_server:is_admin(User, Pass);
    _ ->
        false
    end,
    
    case IsNamedAdmin of
    true ->
        ok;
    false ->
        case couch_server:has_admins() of
        true ->
            throw(admin_auth_error);
        false ->
            % if no admins, then everyone is admin! Yay, admin party!
            ok
        end
    end.

start_chunked_response(#httpd{mochi_req=MochiReq}, Code, Headers) ->
    {ok, MochiReq:respond({Code, Headers ++ server_header(), chunked})}.

send_chunk(Resp, Data) ->
    Resp:write_chunk(Data),
    {ok, Resp}.

send_response(#httpd{mochi_req=MochiReq}, Code, Headers, Body) ->
    if Code >= 400 ->
        ?LOG_DEBUG("HTTPd ~p error response:~n ~s", [Code, Body]);
    true -> ok
    end,
    {ok, MochiReq:respond({Code, Headers ++ server_header(), Body})}.

send_method_not_allowed(Req, Methods) ->
    send_response(Req, 405, [{"Allow", Methods}], <<>>).

send_json(Req, Value) ->
    send_json(Req, 200, Value).

send_json(Req, Code, Value) ->
    send_json(Req, Code, [], Value).

send_json(Req, Code, Headers, Value) ->
    DefaultHeaders = [
        {"Content-Type", negotiate_content_type(Req)},
        {"Cache-Control", "must-revalidate"}
    ],
    send_response(Req, Code, DefaultHeaders ++ Headers, ?JSON_ENCODE(Value)).

start_json_response(Req, Code) ->
    start_json_response(Req, Code, []).

start_json_response(Req, Code, Headers) ->
    DefaultHeaders = [
        {"Content-Type", negotiate_content_type(Req)},
        {"Cache-Control", "must-revalidate"}
    ],
    start_chunked_response(Req, Code, DefaultHeaders ++ Headers).

end_json_response(Resp) ->
    send_chunk(Resp, []).



send_error(Req, bad_request) ->
    send_error(Req, 400, <<"bad_request">>, <<>>);
send_error(Req, {bad_request, Reason}) ->
    send_error(Req, 400, <<"bad_request">>, Reason);
send_error(Req, not_found) ->
    send_error(Req, 404, <<"not_found">>, <<"Missing">>);
send_error(Req, {not_found, Reason}) ->
    send_error(Req, 404, <<"not_found">>, Reason);
send_error(Req, conflict) ->
    send_error(Req, 412, <<"conflict">>, <<"Document update conflict.">>);
send_error(Req, admin_auth_error) ->
    send_json(Req, 401,
        [{"WWW-Authenticate", "Basic realm=\"admin\""}],
        {[{<<"error">>,  <<"auth_error">>},
         {<<"reason">>, <<"Admin user name and password required">>}]});
send_error(Req, {doc_validation, Msg}) ->
    send_error(Req, 406, <<"doc_validation">>, Msg);
send_error(Req, file_exists) ->
    send_error(Req, 409, <<"file_exists">>, <<"The database could not be "
        "created, the file already exists.">>);
send_error(Req, {Error, Reason}) ->
    send_error(Req, 500, Error, Reason);
send_error(Req, Error) ->
    send_error(Req, 500, <<"error">>, Error).



send_error(Req, Code, Error, Msg) when is_atom(Error) ->
    send_error(Req, Code, list_to_binary(atom_to_list(Error)), Msg);
send_error(Req, Code, Error, Msg) when is_list(Msg) ->
    case (catch list_to_binary(Msg)) of
    Bin when is_binary(Bin) ->
        send_error(Req, Code, Error, Bin);
    _ ->
        send_error(Req, Code, Error, io_lib:format("~p", [Msg]))
    end;
send_error(Req, Code, Error, Msg) when not is_binary(Error) ->
    send_error(Req, Code, list_to_binary(io_lib:format("~p", [Error])), Msg);
send_error(Req, Code, Error, Msg) when not is_binary(Msg) ->
    send_error(Req, Code, Error, list_to_binary(io_lib:format("~p", [Msg])));
send_error(Req, Code, Error, <<>>) ->
    send_json(Req, Code, {[{error, Error}]});
send_error(Req, Code, Error, Msg) ->
    send_json(Req, Code, {[{error, Error}, {reason, Msg}]}).
    


negotiate_content_type(#httpd{mochi_req=MochiReq}) ->
    %% Determine the appropriate Content-Type header for a JSON response
    %% depending on the Accept header in the request. A request that explicitly
    %% lists the correct JSON MIME type will get that type, otherwise the
    %% response will have the generic MIME type "text/plain"
    AcceptedTypes = case MochiReq:get_header_value("Accept") of
        undefined       -> [];
        AcceptHeader    -> string:tokens(AcceptHeader, ", ")
    end,
    case lists:member("application/json", AcceptedTypes) of
        true  -> "application/json";
        false -> "text/plain;charset=utf-8"
    end.

server_header() ->
    OTPVersion = "R" ++ integer_to_list(erlang:system_info(compat_rel)) ++ "B",
    [{"Server", "CouchDB/" ++ couch_server:get_version() ++
                " (Erlang OTP/" ++ OTPVersion ++ ")"}].
