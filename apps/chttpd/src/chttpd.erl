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

-module(chttpd).
-include_lib("couch/include/couch_db.hrl").

-export([start_link/0, stop/0, handle_request/1, config_change/2,
    primary_header_value/2, header_value/2, header_value/3, qs_value/2,
    qs_value/3, qs/1, path/1, absolute_uri/2, body_length/1,
    verify_is_server_admin/1, unquote/1, quote/1, recv/2,recv_chunked/4,
    error_info/1, parse_form/1, json_body/1, json_body_obj/1, body/1,
    doc_etag/1, make_etag/1, etag_respond/3, partition/1, serve_file/3,
    server_header/0, start_chunked_response/3,send_chunk/2,
    start_response_length/4, send/2, start_json_response/2,
    start_json_response/3, end_json_response/1, send_response/4,
    send_method_not_allowed/2, send_error/2, send_error/4, send_redirect/2,
    send_chunked_error/2, send_json/2,send_json/3,send_json/4]).

start_link() ->
    Options = [
        {loop, fun ?MODULE:handle_request/1},
        {name, ?MODULE},
        {ip, couch_config:get("chttpd", "bind_address", any)},
        {port, couch_config:get("chttpd", "port", "5984")},
        {backlog, list_to_integer(couch_config:get("chttpd", "backlog", "128"))}
    ],
    case mochiweb_http:start(Options) of
    {ok, Pid} ->
        ok = couch_config:register(fun ?MODULE:config_change/2, Pid),
        {ok, Pid};
    {error, Reason} ->
        io:format("Failure to start Mochiweb: ~s~n", [Reason]),
        {error, Reason}
    end.

config_change("chttpd", "bind_address") ->
    ?MODULE:stop();
config_change("chttpd", "port") ->
    ?MODULE:stop();
config_change("chttpd", "backlog") ->
    ?MODULE:stop().

stop() ->
    mochiweb_http:stop(?MODULE).

handle_request(MochiReq) ->
    Begin = now(),

    AuthenticationFuns = [
        fun chttpd_auth:cookie_authentication_handler/1,
        fun chttpd_auth:default_authentication_handler/1
    ],

    % for the path, use the raw path with the query string and fragment
    % removed, but URL quoting left intact
    RawUri = MochiReq:get(raw_path),
    {"/" ++ Path, _, _} = mochiweb_util:urlsplit_path(RawUri),
    {HandlerKey, _, _} = mochiweb_util:partition(Path, "/"),

    LogForClosedSocket = io_lib:format("mochiweb_recv_error for ~s - ~p ~s", [
        MochiReq:get(peer),
        MochiReq:get(method),
        RawUri
    ]),

    Method1 =
    case MochiReq:get(method) of
        % already an atom
        Meth when is_atom(Meth) -> Meth;

        % Non standard HTTP verbs aren't atoms (COPY, MOVE etc) so convert when
        % possible (if any module references the atom, then it's existing).
        Meth -> couch_util:to_existing_atom(Meth)
    end,
    increment_method_stats(Method1),
    % alias HEAD to GET as mochiweb takes care of stripping the body
    Method = case Method1 of
        'HEAD' -> 'GET';
        Other -> Other
    end,

    HttpReq = #httpd{
        mochi_req = MochiReq,
        method = Method,
        path_parts = [list_to_binary(chttpd:unquote(Part))
                || Part <- string:tokens(Path, "/")],
        db_url_handlers = db_url_handlers(),
        design_url_handlers = design_url_handlers()
    },

    {ok, Resp} =
    try
        case authenticate_request(HttpReq, AuthenticationFuns) of
        #httpd{} = Req ->
            HandlerFun = url_handler(HandlerKey),
            HandlerFun(Req);
        Response ->
            Response
        end
    catch
        throw:{http_head_abort, Resp0} ->
            {ok, Resp0};
        throw:{invalid_json, S} ->
            ?LOG_ERROR("attempted upload of invalid JSON ~s", [S]),
            send_error(HttpReq, {bad_request, "invalid UTF-8 JSON"});
        exit:{mochiweb_recv_error, E} ->
            ?LOG_INFO(LogForClosedSocket ++ " - ~p", [E]),
            exit(normal);
        throw:Error ->
            send_error(HttpReq, Error);
        error:database_does_not_exist ->
            send_error(HttpReq, database_does_not_exist);
        error:badarg ->
            ?LOG_ERROR("Badarg error in HTTP request",[]),
            ?LOG_INFO("Stacktrace: ~p",[erlang:get_stacktrace()]),
            send_error(HttpReq, badarg);
        error:function_clause ->
            ?LOG_ERROR("function_clause error in HTTP request",[]),
            ?LOG_INFO("Stacktrace: ~p",[erlang:get_stacktrace()]),
            send_error(HttpReq, function_clause);
        Tag:Error ->
            ?LOG_ERROR("Uncaught error in HTTP request: ~p",[{Tag, Error}]),
            ?LOG_INFO("Stacktrace: ~p",[erlang:get_stacktrace()]),
            send_error(HttpReq, Error)
    end,

    RequestTime = timer:now_diff(now(), Begin)/1000,
    Peer = MochiReq:get(peer),
    Code = Resp:get(code),
    Host = MochiReq:get_header_value("Host"),
    ?LOG_INFO("~s ~s ~s ~s ~B ~B", [Peer, Host,
        atom_to_list(Method1), RawUri, Code, round(RequestTime)]),
    couch_stats_collector:record({couchdb, request_time}, RequestTime),
    couch_stats_collector:increment({httpd, requests}),
    {ok, Resp}.

% Try authentication handlers in order until one returns a result
authenticate_request(#httpd{user_ctx=#user_ctx{}} = Req, _AuthFuns) ->
    Req;
authenticate_request(#httpd{} = Req, [AuthFun|Rest]) ->
    authenticate_request(AuthFun(Req), Rest);
authenticate_request(#httpd{} = Req, []) ->
    case couch_config:get("chttpd", "require_valid_user", "false") of
    "true" ->
        throw({unauthorized, <<"Authentication required.">>});
    "false" ->
        case couch_config:get("admins") of
        [] ->
            Ctx = #user_ctx{roles=[<<"_reader">>, <<"_writer">>, <<"_admin">>]},
            Req#httpd{user_ctx = Ctx};
        _ ->
            Req#httpd{user_ctx=#user_ctx{}}
        end
    end;
authenticate_request(Response, _AuthFuns) ->
    Response.

increment_method_stats(Method) ->
    couch_stats_collector:increment({httpd_request_methods, Method}).

url_handler("") ->              fun chttpd_misc:handle_welcome_req/1;
url_handler("favicon.ico") ->   fun chttpd_misc:handle_favicon_req/1;
url_handler("_utils") ->        fun chttpd_misc:handle_utils_dir_req/1;
url_handler("_all_dbs") ->      fun chttpd_misc:handle_all_dbs_req/1;
url_handler("_active_tasks") -> fun chttpd_misc:handle_task_status_req/1;
url_handler("_config") ->       fun chttpd_misc:handle_config_req/1;
url_handler("_replicate") ->    fun chttpd_misc:handle_replicate_req/1;
url_handler("_uuids") ->        fun chttpd_misc:handle_uuids_req/1;
url_handler("_log") ->          fun chttpd_misc:handle_log_req/1;
url_handler("_sleep") ->        fun chttpd_misc:handle_sleep_req/1;
url_handler("_session") ->      fun chttpd_auth:handle_session_req/1;
url_handler("_user") ->         fun chttpd_auth:handle_user_req/1;
url_handler("_oauth") ->        fun chttpd_oauth:handle_oauth_req/1;
url_handler("_restart") ->      fun showroom_http:handle_restart_req/1;
url_handler("_membership") ->   fun mem3_httpd:handle_membership_req/1;
url_handler(_) ->               fun chttpd_db:handle_request/1.

db_url_handlers() ->
    [
        {<<"_view_cleanup">>,   fun chttpd_db:handle_view_cleanup_req/2},
        {<<"_compact">>,        fun chttpd_db:handle_compact_req/2},
        {<<"_design">>,         fun chttpd_db:handle_design_req/2},
        {<<"_temp_view">>,      fun chttpd_db:handle_temp_view_req/2},
        {<<"_changes">>,        fun chttpd_db:handle_changes_req/2},
        {<<"_search">>,         fun chttpd_external:handle_search_req/2}
    ].

design_url_handlers() ->
    [
        {<<"_view">>,           fun chttpd_view:handle_view_req/3},
        {<<"_show">>,           fun chttpd_show:handle_doc_show_req/3},
        {<<"_list">>,           fun chttpd_show:handle_view_list_req/3},
        {<<"_update">>,         fun chttpd_show:handle_doc_update_req/3},
        {<<"_info">>,           fun chttpd_db:handle_design_info_req/3},
        {<<"_rewrite">>,        fun chttpd_rewrite:handle_rewrite_req/3}
    ].

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

serve_file(#httpd{mochi_req=MochiReq}=Req, RelativePath, DocumentRoot) ->
    {ok, MochiReq:serve_file(RelativePath, DocumentRoot,
        server_header() ++ chttpd_auth:cookie_auth_header(Req, []))}.

qs_value(Req, Key) ->
    qs_value(Req, Key, undefined).

qs_value(Req, Key, Default) ->
    couch_util:get_value(Key, qs(Req), Default).

qs(#httpd{mochi_req=MochiReq}) ->
    MochiReq:parse_qs().

path(#httpd{mochi_req=MochiReq}) ->
    MochiReq:get(path).

absolute_uri(#httpd{mochi_req=MochiReq}, Path) ->
    XHost = couch_config:get("httpd", "x_forwarded_host", "X-Forwarded-Host"),
    Host = case MochiReq:get_header_value(XHost) of
        undefined ->
            case MochiReq:get_header_value("Host") of
                undefined ->
                    {ok, {Address, Port}} = inet:sockname(MochiReq:get(socket)),
                    inet_parse:ntoa(Address) ++ ":" ++ integer_to_list(Port);
                Value1 ->
                    Value1
            end;
        Value -> Value
    end,
    XSsl = couch_config:get("httpd", "x_forwarded_ssl", "X-Forwarded-Ssl"),
    Scheme = case MochiReq:get_header_value(XSsl) of
        "on" -> "https";
        _ ->
            XProto = couch_config:get("httpd", "x_forwarded_proto",
                "X-Forwarded-Proto"),
            case MochiReq:get_header_value(XProto) of
                % Restrict to "https" and "http" schemes only
                "https" -> "https";
                _ -> "http"
            end
    end,
    Scheme ++ "://" ++ Host ++ Path.

unquote(UrlEncodedString) ->
    mochiweb_util:unquote(UrlEncodedString).

quote(UrlDecodedString) ->
    mochiweb_util:quote_plus(UrlDecodedString).

parse_form(#httpd{mochi_req=MochiReq}) ->
    mochiweb_multipart:parse_form(MochiReq).

recv(#httpd{mochi_req=MochiReq}, Len) ->
    MochiReq:recv(Len).

recv_chunked(#httpd{mochi_req=MochiReq}, MaxChunkSize, ChunkFun, InitState) ->
    % Fun is called once with each chunk
    % Fun({Length, Binary}, State)
    % called with Length == 0 on the last time.
    MochiReq:stream_body(MaxChunkSize, ChunkFun, InitState).

body_length(Req) ->
    case header_value(Req, "Transfer-Encoding") of
        undefined ->
            case header_value(Req, "Content-Length") of
                undefined -> undefined;
                Length -> list_to_integer(Length)
            end;
        "chunked" -> chunked;
        Unknown -> {unknown_transfer_encoding, Unknown}
    end.

body(#httpd{mochi_req=MochiReq, req_body=ReqBody}) ->
    case ReqBody of
        undefined ->
            % Maximum size of document PUT request body (4GB)
            MaxSize = list_to_integer(
                couch_config:get("couchdb", "max_document_size", "4294967296")),
            MochiReq:recv_body(MaxSize);
        _Else ->
            ReqBody
    end.

json_body(Httpd) ->
    ?JSON_DECODE(body(Httpd)).

json_body_obj(Httpd) ->
    case json_body(Httpd) of
        {Props} -> {Props};
        _Else ->
            throw({bad_request, "Request body must be a JSON object"})
    end.


doc_etag(#doc{revs={Start, [DiskRev|_]}}) ->
    "\"" ++ ?b2l(couch_doc:rev_to_str({Start, DiskRev})) ++ "\"".

make_etag(Term) ->
    <<SigInt:128/integer>> = erlang:md5(term_to_binary(Term)),
    list_to_binary(io_lib:format("\"~.36B\"",[SigInt])).

etag_match(Req, CurrentEtag) when is_binary(CurrentEtag) ->
    etag_match(Req, binary_to_list(CurrentEtag));

etag_match(Req, CurrentEtag) ->
    EtagsToMatch = string:tokens(
        chttpd:header_value(Req, "If-None-Match", ""), ", "),
    lists:member(CurrentEtag, EtagsToMatch).

etag_respond(Req, CurrentEtag, RespFun) ->
    case etag_match(Req, CurrentEtag) of
    true ->
        % the client has this in their cache.
        chttpd:send_response(Req, 304, [{"Etag", CurrentEtag}], <<>>);
    false ->
        % Run the function.
        RespFun()
    end.

verify_is_server_admin(#httpd{user_ctx=#user_ctx{roles=Roles}}) ->
    case lists:member(<<"_admin">>, Roles) of
    true -> ok;
    false -> throw({unauthorized, <<"You are not a server admin.">>})
    end.

start_response_length(#httpd{mochi_req=MochiReq}=Req, Code, Headers, Length) ->
    couch_stats_collector:increment({httpd_status_codes, Code}),
    Resp = MochiReq:start_response_length({Code, Headers ++ server_header() ++
        chttpd_auth:cookie_auth_header(Req, Headers), Length}),
    case MochiReq:get(method) of
    'HEAD' -> throw({http_head_abort, Resp});
    _ -> ok
    end,
    {ok, Resp}.

send(Resp, Data) ->
    Resp:send(Data),
    {ok, Resp}.

start_chunked_response(#httpd{mochi_req=MochiReq}=Req, Code, Headers) ->
    couch_stats_collector:increment({httpd_status_codes, Code}),
    Resp = MochiReq:respond({Code, Headers ++ server_header() ++
        chttpd_auth:cookie_auth_header(Req, Headers), chunked}),
    case MochiReq:get(method) of
    'HEAD' -> throw({http_head_abort, Resp});
    _ -> ok
    end,
    {ok, Resp}.

send_chunk(Resp, Data) ->
    Resp:write_chunk(Data),
    {ok, Resp}.

send_response(#httpd{mochi_req=MochiReq}=Req, Code, Headers, Body) ->
    couch_stats_collector:increment({httpd_status_codes, Code}),
    if Code >= 400 ->
        ?LOG_DEBUG("httpd ~p error response:~n ~s", [Code, Body]);
    true -> ok
    end,
    {ok, MochiReq:respond({Code, Headers ++ server_header() ++
        chttpd_auth:cookie_auth_header(Req, Headers), Body})}.

send_method_not_allowed(Req, Methods) ->
    send_error(Req, 405, [{"Allow", Methods}], <<"method_not_allowed">>,
        ?l2b("Only " ++ Methods ++ " allowed")).

send_json(Req, Value) ->
    send_json(Req, 200, Value).

send_json(Req, Code, Value) ->
    send_json(Req, Code, [], Value).

send_json(Req, Code, Headers, Value) ->
    DefaultHeaders = [
        {"Content-Type", negotiate_content_type(Req)},
        {"Cache-Control", "must-revalidate"}
    ],
    Body = list_to_binary(
        [start_jsonp(Req), ?JSON_ENCODE(Value), end_jsonp(), $\n]
    ),
    send_response(Req, Code, DefaultHeaders ++ Headers, Body).

start_json_response(Req, Code) ->
    start_json_response(Req, Code, []).

start_json_response(Req, Code, Headers) ->
    DefaultHeaders = [
        {"Content-Type", negotiate_content_type(Req)},
        {"Cache-Control", "must-revalidate"}
    ],
    start_jsonp(Req), % Validate before starting chunked.
    %start_chunked_response(Req, Code, DefaultHeaders ++ Headers).
    {ok, Resp} = start_chunked_response(Req, Code, DefaultHeaders ++ Headers),
    case start_jsonp(Req) of
        [] -> ok;
        Start -> send_chunk(Resp, Start)
    end,
    {ok, Resp}.

end_json_response(Resp) ->
    send_chunk(Resp, end_jsonp() ++ [$\r,$\n]),
    %send_chunk(Resp, [$\n]),
    send_chunk(Resp, []).

start_jsonp(Req) ->
    case get(jsonp) of
        undefined -> put(jsonp, qs_value(Req, "callback", no_jsonp));
        _ -> ok
    end,
    case get(jsonp) of
        no_jsonp -> [];
        [] -> [];
        CallBack ->
            try
                validate_callback(CallBack),
                CallBack ++ "("
            catch
                Error ->
                    put(jsonp, no_jsonp),
                    throw(Error)
            end
    end.

end_jsonp() ->
    Resp = case get(jsonp) of
        no_jsonp -> [];
        [] -> [];
        _ -> ");"
    end,
    put(jsonp, undefined),
    Resp.

validate_callback(CallBack) when is_binary(CallBack) ->
    validate_callback(binary_to_list(CallBack));
validate_callback([]) ->
    ok;
validate_callback([Char | Rest]) ->
    case Char of
        _ when Char >= $a andalso Char =< $z -> ok;
        _ when Char >= $A andalso Char =< $Z -> ok;
        _ when Char >= $0 andalso Char =< $9 -> ok;
        _ when Char == $. -> ok;
        _ when Char == $_ -> ok;
        _ when Char == $[ -> ok;
        _ when Char == $] -> ok;
        _ ->
            throw({bad_request, invalid_callback})
    end,
    validate_callback(Rest).


error_info({Error, Reason}) when is_list(Reason) ->
    error_info({Error, couch_util:to_binary(Reason)});
error_info(bad_request) ->
    {400, <<"bad_request">>, <<>>};
error_info({bad_request, Reason}) ->
    {400, <<"bad_request">>, Reason};
error_info({query_parse_error, Reason}) ->
    {400, <<"query_parse_error">>, Reason};
error_info(database_does_not_exist) ->
    {404, <<"not_found">>, <<"Database does not exist.">>};
error_info(not_found) ->
    {404, <<"not_found">>, <<"missing">>};
error_info({not_found, Reason}) ->
    {404, <<"not_found">>, Reason};
error_info({not_acceptable, Reason}) ->
    {406, <<"not_acceptable">>, Reason};
error_info(conflict) ->
    {409, <<"conflict">>, <<"Document update conflict.">>};
error_info({conflict, _}) ->
    {409, <<"conflict">>, <<"Document update conflict.">>};
error_info({forbidden, Msg}) ->
    {403, <<"forbidden">>, Msg};
error_info({forbidden, Error, Msg}) ->
    {403, Error, Msg};
error_info({unauthorized, Msg}) ->
    {401, <<"unauthorized">>, Msg};
error_info(file_exists) ->
    {412, <<"file_exists">>, <<"The database could not be "
        "created, the file already exists.">>};
error_info({r_quorum_not_met, Reason}) ->
    {412, <<"read_quorum_not_met">>, Reason};
error_info({w_quorum_not_met, Reason}) ->
    {500, <<"write_quorum_not_met">>, Reason};
error_info({bad_ctype, Reason}) ->
    {415, <<"bad_content_type">>, Reason};
error_info({error, illegal_database_name}) ->
    {400, <<"illegal_database_name">>, <<"Only lowercase characters (a-z), "
        "digits (0-9), and any of the characters _, $, (, ), +, -, and / "
        "are allowed">>};
error_info({missing_stub, Reason}) ->
    {412, <<"missing_stub">>, Reason};
error_info(not_implemented) ->
    {501, <<"not_implemented">>, <<"this feature is not yet implemented">>};
error_info({Error, Reason}) ->
    {500, couch_util:to_binary(Error), couch_util:to_binary(Reason)};
error_info(Error) ->
    {500, <<"unknown_error">>, couch_util:to_binary(Error)}.

send_error(_Req, {already_sent, Resp, _Error}) ->
    {ok, Resp};

send_error(#httpd{mochi_req=MochiReq}=Req, Error) ->
    {Code, ErrorStr, ReasonStr} = error_info(Error),
    Headers = if Code == 401 ->
        case MochiReq:get_header_value("X-CouchDB-WWW-Authenticate") of
        undefined ->
            case couch_config:get("httpd", "WWW-Authenticate", nil) of
            nil ->
                [];
            Type ->
                [{"WWW-Authenticate", Type}]
            end;
        Type ->
            [{"WWW-Authenticate", Type}]
        end;
    true ->
        []
    end,
    send_error(Req, Code, Headers, ErrorStr, ReasonStr).

send_error(Req, Code, ErrorStr, ReasonStr) ->
    send_error(Req, Code, [], ErrorStr, ReasonStr).

send_error(Req, Code, Headers, ErrorStr, ReasonStr) ->
    send_json(Req, Code, Headers,
        {[{<<"error">>,  ErrorStr},
         {<<"reason">>, ReasonStr}]}).

% give the option for list functions to output html or other raw errors
send_chunked_error(Resp, {_Error, {[{<<"body">>, Reason}]}}) ->
    send_chunk(Resp, Reason),
    send_chunk(Resp, []);

send_chunked_error(Resp, Error) ->
    {Code, ErrorStr, ReasonStr} = error_info(Error),
    JsonError = {[{<<"code">>, Code},
        {<<"error">>,  ErrorStr},
        {<<"reason">>, ReasonStr}]},
    send_chunk(Resp, ?l2b([$\n,?JSON_ENCODE(JsonError),$\n])),
    send_chunk(Resp, []).

send_redirect(Req, Path) ->
     Headers = [{"Location", chttpd:absolute_uri(Req, Path)}],
     send_response(Req, 301, Headers, <<>>).

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
    couch_httpd:server_header().
