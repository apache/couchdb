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

-module(couch_httpd).
-include("couch_db.hrl").

-export([start_link/0, start_link/1, stop/0, config_change/2, 
        handle_request/5]).

-export([header_value/2,header_value/3,qs_value/2,qs_value/3,qs/1,qs_json_value/3]).
-export([path/1,absolute_uri/2,body_length/1]).
-export([verify_is_server_admin/1,unquote/1,quote/1,recv/2,recv_chunked/4,error_info/1]).
-export([make_fun_spec_strs/1]).
-export([make_arity_1_fun/1, make_arity_2_fun/1, make_arity_3_fun/1]).
-export([parse_form/1,json_body/1,json_body_obj/1,body/1,doc_etag/1, make_etag/1, etag_respond/3]).
-export([primary_header_value/2,partition/1,serve_file/3,serve_file/4, server_header/0]).
-export([start_chunked_response/3,send_chunk/2,log_request/2]).
-export([start_response_length/4, start_response/3, send/2]).
-export([start_json_response/2, start_json_response/3, end_json_response/1]).
-export([send_response/4,send_method_not_allowed/2,send_error/4, send_redirect/2,send_chunked_error/2]).
-export([send_json/2,send_json/3,send_json/4,last_chunk/1,parse_multipart_request/3]).
-export([accepted_encodings/1,handle_request_int/5,validate_referer/1,validate_ctype/2]).

start_link() ->
    start_link(http).
start_link(http) ->
    Port = couch_config:get("httpd", "port", "5984"),
    start_link(?MODULE, [{port, Port}]);
start_link(https) ->
    Port = couch_config:get("ssl", "port", "6984"),
    CertFile = couch_config:get("ssl", "cert_file", nil),
    KeyFile = couch_config:get("ssl", "key_file", nil),
    Options = case CertFile /= nil andalso KeyFile /= nil of
        true ->
            SslOpts = [{certfile, CertFile}, {keyfile, KeyFile}],

            %% set password if one is needed for the cert
            SslOpts1 = case couch_config:get("ssl", "password", nil) of
                nil -> SslOpts;
                Password ->
                    SslOpts ++ [{password, Password}]
            end,
            % do we verify certificates ?
            FinalSslOpts = case couch_config:get("ssl",
                    "verify_ssl_certificates", "false") of
                "false" -> SslOpts1;
                "true" ->
                    case couch_config:get("ssl",
                            "cacert_file", nil) of
                        nil ->
                            io:format("Verify SSL certificate "
                                ++"enabled but file containing "
                                ++"PEM encoded CA certificates is "
                                ++"missing", []),
                            throw({error, missing_cacerts});
                        CaCertFile ->
                            Depth = list_to_integer(couch_config:get("ssl",
                                    "ssl_certificate_max_depth",
                                    "1")),
                            FinalOpts = [
                                {cacertfile, CaCertFile},
                                {depth, Depth},
                                {verify, verify_peer}],
                            % allows custom verify fun.
                            case couch_config:get("ssl",
                                    "verify_fun", nil) of
                                nil -> FinalOpts;
                                SpecStr ->
                                    FinalOpts 
                                    ++ [{verify_fun, make_arity_3_fun(SpecStr)}]
                            end
                    end
            end,
            
            [{port, Port},
                {ssl, true},
                {ssl_opts, FinalSslOpts}];
        false ->
            io:format("SSL enabled but PEM certificates are missing.", []),
            throw({error, missing_certs})
    end,
    start_link(https, Options).
start_link(Name, Options) ->
    % read config and register for configuration changes

    % just stop if one of the config settings change. couch_server_sup
    % will restart us and then we will pick up the new settings.

    BindAddress = couch_config:get("httpd", "bind_address", any),
    DefaultSpec = "{couch_httpd_db, handle_request}",
    DefaultFun = make_arity_1_fun(
        couch_config:get("httpd", "default_handler", DefaultSpec)
    ),

    UrlHandlersList = lists:map(
        fun({UrlKey, SpecStr}) ->
            {?l2b(UrlKey), make_arity_1_fun(SpecStr)}
        end, couch_config:get("httpd_global_handlers")),

    DbUrlHandlersList = lists:map(
        fun({UrlKey, SpecStr}) ->
            {?l2b(UrlKey), make_arity_2_fun(SpecStr)}
        end, couch_config:get("httpd_db_handlers")),

    DesignUrlHandlersList = lists:map(
        fun({UrlKey, SpecStr}) ->
            {?l2b(UrlKey), make_arity_3_fun(SpecStr)}
        end, couch_config:get("httpd_design_handlers")),

    UrlHandlers = dict:from_list(UrlHandlersList),
    DbUrlHandlers = dict:from_list(DbUrlHandlersList),
    DesignUrlHandlers = dict:from_list(DesignUrlHandlersList),
    {ok, ServerOptions} = couch_util:parse_term(
        couch_config:get("httpd", "server_options", "[]")),
    {ok, SocketOptions} = couch_util:parse_term(
        couch_config:get("httpd", "socket_options", "[]")),

    set_auth_handlers(),

    Loop = fun(Req)->
        case SocketOptions of
        [] ->
            ok;
        _ ->
            ok = mochiweb_socket:setopts(Req:get(socket), SocketOptions)
        end,
        apply(?MODULE, handle_request, [
            Req, DefaultFun, UrlHandlers, DbUrlHandlers, DesignUrlHandlers
        ])
    end,

    % set mochiweb options
    FinalOptions = lists:append([Options, ServerOptions, [
            {loop, Loop},
            {name, Name},
            {ip, BindAddress}]]),

    % launch mochiweb
    {ok, Pid} = case mochiweb_http:start(FinalOptions) of
        {ok, MochiPid} -> 
            {ok, MochiPid};
        {error, Reason} ->
            io:format("Failure to start Mochiweb: ~s~n",[Reason]),
            throw({error, Reason})
    end,

    ok = couch_config:register(fun ?MODULE:config_change/2, Pid),
    {ok, Pid}.


stop() ->
    mochiweb_http:stop(couch_httpd),
    mochiweb_http:stop(https).

config_change("httpd", "bind_address") ->
    ?MODULE:stop();
config_change("httpd", "port") ->
    ?MODULE:stop();
config_change("httpd", "default_handler") ->
    ?MODULE:stop();
config_change("httpd", "server_options") ->
    ?MODULE:stop();
config_change("httpd", "socket_options") ->
    ?MODULE:stop();
config_change("httpd", "authentication_handlers") ->
    set_auth_handlers();
config_change("httpd_global_handlers", _) ->
    ?MODULE:stop();
config_change("httpd_db_handlers", _) ->
    ?MODULE:stop();
config_change("ssl", _) ->
    ?MODULE:stop().

set_auth_handlers() ->
    AuthenticationSrcs = make_fun_spec_strs(
        couch_config:get("httpd", "authentication_handlers", "")),
    AuthHandlers = lists:map(
        fun(A) -> {make_arity_1_fun(A), ?l2b(A)} end, AuthenticationSrcs),
    ok = application:set_env(couch, auth_handlers, AuthHandlers).

% SpecStr is a string like "{my_module, my_fun}"
%  or "{my_module, my_fun, <<"my_arg">>}"
make_arity_1_fun(SpecStr) ->
    case couch_util:parse_term(SpecStr) of
    {ok, {Mod, Fun, SpecArg}} ->
        fun(Arg) -> Mod:Fun(Arg, SpecArg) end;
    {ok, {Mod, Fun}} ->
        fun(Arg) -> Mod:Fun(Arg) end
    end.

make_arity_2_fun(SpecStr) ->
    case couch_util:parse_term(SpecStr) of
    {ok, {Mod, Fun, SpecArg}} ->
        fun(Arg1, Arg2) -> Mod:Fun(Arg1, Arg2, SpecArg) end;
    {ok, {Mod, Fun}} ->
        fun(Arg1, Arg2) -> Mod:Fun(Arg1, Arg2) end
    end.

make_arity_3_fun(SpecStr) ->
    case couch_util:parse_term(SpecStr) of
    {ok, {Mod, Fun, SpecArg}} ->
        fun(Arg1, Arg2, Arg3) -> Mod:Fun(Arg1, Arg2, Arg3, SpecArg) end;
    {ok, {Mod, Fun}} ->
        fun(Arg1, Arg2, Arg3) -> Mod:Fun(Arg1, Arg2, Arg3) end
    end.

% SpecStr is "{my_module, my_fun}, {my_module2, my_fun2}"
make_fun_spec_strs(SpecStr) ->
    re:split(SpecStr, "(?<=})\\s*,\\s*(?={)", [{return, list}]).

handle_request(MochiReq, DefaultFun, UrlHandlers, DbUrlHandlers, 
    DesignUrlHandlers) ->
    %% reset rewrite count for new request
    erlang:put(?REWRITE_COUNT, 0),

    MochiReq1 = couch_httpd_vhost:dispatch_host(MochiReq),
    
    handle_request_int(MochiReq1, DefaultFun,
                UrlHandlers, DbUrlHandlers, DesignUrlHandlers).

handle_request_int(MochiReq, DefaultFun,
            UrlHandlers, DbUrlHandlers, DesignUrlHandlers) ->
    Begin = now(),
    % for the path, use the raw path with the query string and fragment
    % removed, but URL quoting left intact
    RawUri = MochiReq:get(raw_path),
    {"/" ++ Path, _, _} = mochiweb_util:urlsplit_path(RawUri),

    Headers = MochiReq:get(headers), 

    % get requested path
    RequestedPath = case MochiReq:get_header_value("x-couchdb-vhost-path") of
        undefined ->
            case MochiReq:get_header_value("x-couchdb-requested-path") of
                undefined -> RawUri;
                R -> R
            end;
        P -> P
    end,
    
    HandlerKey =
    case mochiweb_util:partition(Path, "/") of
    {"", "", ""} ->
        <<"/">>; % Special case the root url handler
    {FirstPart, _, _} ->
        list_to_binary(FirstPart)
    end,
    ?LOG_DEBUG("~p ~s ~p from ~p~nHeaders: ~p", [
        MochiReq:get(method),
        RawUri,
        MochiReq:get(version),
        MochiReq:get(peer),
        mochiweb_headers:to_list(MochiReq:get(headers))
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

    % allow broken HTTP clients to fake a full method vocabulary with an X-HTTP-METHOD-OVERRIDE header
    MethodOverride = MochiReq:get_primary_header_value("X-HTTP-Method-Override"),
    Method2 = case lists:member(MethodOverride, ["GET", "HEAD", "POST", "PUT", "DELETE", "TRACE", "CONNECT", "COPY"]) of
    true -> 
        ?LOG_INFO("MethodOverride: ~s (real method was ~s)", [MethodOverride, Method1]),
        case Method1 of
        'POST' -> couch_util:to_existing_atom(MethodOverride);
        _ -> 
            % Ignore X-HTTP-Method-Override when the original verb isn't POST.
            % I'd like to send a 406 error to the client, but that'd require a nasty refactor.
            % throw({not_acceptable, <<"X-HTTP-Method-Override may only be used with POST requests.">>})
            Method1
        end;
    _ -> Method1
    end,

    % alias HEAD to GET as mochiweb takes care of stripping the body
    Method = case Method2 of
        'HEAD' -> 'GET';
        Other -> Other
    end,

    HttpReq = #httpd{
        mochi_req = MochiReq,
        peer = MochiReq:get(peer),
        method = Method,
        requested_path_parts =
            [?l2b(unquote(Part)) || Part <- string:tokens(RequestedPath, "/")],
        path_parts = [?l2b(unquote(Part)) || Part <- string:tokens(Path, "/")],
        db_url_handlers = DbUrlHandlers,
        design_url_handlers = DesignUrlHandlers,
        default_fun = DefaultFun,
        url_handlers = UrlHandlers,
        user_ctx = erlang:erase(pre_rewrite_user_ctx)
    },

    HandlerFun = couch_util:dict_find(HandlerKey, UrlHandlers, DefaultFun),
    {ok, AuthHandlers} = application:get_env(couch, auth_handlers),

    {ok, Resp} =
    try
        case authenticate_request(HttpReq, AuthHandlers) of
        #httpd{} = Req ->
            HandlerFun(Req);
        Response ->
            Response
        end
    catch
        throw:{http_head_abort, Resp0} ->
            {ok, Resp0};
        throw:{invalid_json, S} ->
            ?LOG_ERROR("attempted upload of invalid JSON (set log_level to debug to log it)", []),
            ?LOG_DEBUG("Invalid JSON: ~p",[S]),
            send_error(HttpReq, {bad_request, invalid_json});
        throw:unacceptable_encoding ->
            ?LOG_ERROR("unsupported encoding method for the response", []),
            send_error(HttpReq, {not_acceptable, "unsupported encoding"});
        throw:bad_accept_encoding_value ->
            ?LOG_ERROR("received invalid Accept-Encoding header", []),
            send_error(HttpReq, bad_request);
        exit:normal ->
            exit(normal);
        exit:snappy_nif_not_loaded ->
            ErrorReason = "To access the database or view index, Apache CouchDB"
                " must be built with Erlang OTP R13B04 or higher.",
            ?LOG_ERROR("~s", [ErrorReason]),
            send_error(HttpReq, {bad_otp_release, ErrorReason});
        throw:Error ->
            Stack = erlang:get_stacktrace(),
            ?LOG_DEBUG("Minor error in HTTP request: ~p",[Error]),
            ?LOG_DEBUG("Stacktrace: ~p",[Stack]),
            send_error(HttpReq, Error);
        error:badarg ->
            Stack = erlang:get_stacktrace(),
            ?LOG_ERROR("Badarg error in HTTP request",[]),
            ?LOG_INFO("Stacktrace: ~p",[Stack]),
            send_error(HttpReq, badarg);
        error:function_clause ->
            Stack = erlang:get_stacktrace(),
            ?LOG_ERROR("function_clause error in HTTP request",[]),
            ?LOG_INFO("Stacktrace: ~p",[Stack]),
            send_error(HttpReq, function_clause);
        Tag:Error ->
            Stack = erlang:get_stacktrace(),
            ?LOG_ERROR("Uncaught error in HTTP request: ~p",[{Tag, Error}]),
            ?LOG_INFO("Stacktrace: ~p",[Stack]),
            send_error(HttpReq, Error)
    end,
    RequestTime = round(timer:now_diff(now(), Begin)/1000),
    couch_stats_collector:record({couchdb, request_time}, RequestTime),
    couch_stats_collector:increment({httpd, requests}),
    {ok, Resp}.

% Try authentication handlers in order until one sets a user_ctx
% the auth funs also have the option of returning a response
% move this to couch_httpd_auth?
authenticate_request(#httpd{user_ctx=#user_ctx{}} = Req, _AuthHandlers) ->
    Req;
authenticate_request(#httpd{} = Req, []) ->
    case couch_config:get("couch_httpd_auth", "require_valid_user", "false") of
    "true" ->
        throw({unauthorized, <<"Authentication required.">>});
    "false" ->
        Req#httpd{user_ctx=#user_ctx{}}
    end;
authenticate_request(#httpd{} = Req, [{AuthFun, AuthSrc} | RestAuthHandlers]) ->
    R = case AuthFun(Req) of
        #httpd{user_ctx=#user_ctx{}=UserCtx}=Req2 ->
            Req2#httpd{user_ctx=UserCtx#user_ctx{handler=AuthSrc}};
        Else -> Else
    end,
    authenticate_request(R, RestAuthHandlers);
authenticate_request(Response, _AuthSrcs) ->
    Response.

increment_method_stats(Method) ->
    couch_stats_collector:increment({httpd_request_methods, Method}).

validate_referer(Req) ->
    Host = host_for_request(Req),
    Referer = header_value(Req, "Referer", fail),
    case Referer of
    fail ->
        throw({bad_request, <<"Referer header required.">>});
    Referer ->
        {_,RefererHost,_,_,_} = mochiweb_util:urlsplit(Referer),
        if
            RefererHost =:= Host -> ok;
            true -> throw({bad_request, <<"Referer header must match host.">>})
        end
    end.

validate_ctype(Req, Ctype) ->
    case header_value(Req, "Content-Type") of
    undefined ->
        throw({bad_ctype, "Content-Type must be "++Ctype});
    ReqCtype ->
        case string:tokens(ReqCtype, ";") of
        [Ctype] -> ok;
        [Ctype, _Rest] -> ok;
        _Else ->
            throw({bad_ctype, "Content-Type must be "++Ctype})
        end
    end.

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

accepted_encodings(#httpd{mochi_req=MochiReq}) ->
    case MochiReq:accepted_encodings(["gzip", "identity"]) of
    bad_accept_encoding_value ->
        throw(bad_accept_encoding_value);
    [] ->
        throw(unacceptable_encoding);
    EncList ->
        EncList
    end.

serve_file(Req, RelativePath, DocumentRoot) ->
    serve_file(Req, RelativePath, DocumentRoot, []).

serve_file(#httpd{mochi_req=MochiReq}=Req, RelativePath, DocumentRoot, ExtraHeaders) ->
    log_request(Req, 200),
    {ok, MochiReq:serve_file(RelativePath, DocumentRoot,
        server_header() ++ couch_httpd_auth:cookie_auth_header(Req, []) ++ ExtraHeaders)}.

qs_value(Req, Key) ->
    qs_value(Req, Key, undefined).

qs_value(Req, Key, Default) ->
    couch_util:get_value(Key, qs(Req), Default).

qs_json_value(Req, Key, Default) ->
    case qs_value(Req, Key, Default) of
    Default ->
        Default;
    Result ->
        ?JSON_DECODE(Result)
    end.

qs(#httpd{mochi_req=MochiReq}) ->
    MochiReq:parse_qs().

path(#httpd{mochi_req=MochiReq}) ->
    MochiReq:get(path).

host_for_request(#httpd{mochi_req=MochiReq}) ->
    XHost = couch_config:get("httpd", "x_forwarded_host", "X-Forwarded-Host"),
    case MochiReq:get_header_value(XHost) of
        undefined ->
            case MochiReq:get_header_value("Host") of
                undefined ->
                    {ok, {Address, Port}} = inet:sockname(MochiReq:get(socket)),
                    inet_parse:ntoa(Address) ++ ":" ++ integer_to_list(Port);
                Value1 ->
                    Value1
            end;
        Value -> Value
    end.

absolute_uri(#httpd{mochi_req=MochiReq}=Req, Path) ->
    Host = host_for_request(Req),
    XSsl = couch_config:get("httpd", "x_forwarded_ssl", "X-Forwarded-Ssl"),
    Scheme = case MochiReq:get_header_value(XSsl) of
                 "on" -> "https";
                 _ ->
                     XProto = couch_config:get("httpd", "x_forwarded_proto", "X-Forwarded-Proto"),
                     case MochiReq:get_header_value(XProto) of
                         %% Restrict to "https" and "http" schemes only
                         "https" -> "https";
                         _ -> case MochiReq:get(scheme) of
                                  https -> "https";
                                  http -> "http"
                              end
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

body(#httpd{mochi_req=MochiReq, req_body=undefined} = Req) ->
    case body_length(Req) of
        undefined ->
            MaxSize = list_to_integer(
                couch_config:get("couchdb", "max_document_size", "4294967296")),
            MochiReq:recv_body(MaxSize);
        chunked ->
            ChunkFun = fun({0, _Footers}, Acc) ->
                lists:reverse(Acc);
            ({_Len, Chunk}, Acc) ->
                [Chunk | Acc]
            end,
            recv_chunked(Req, 8192, ChunkFun, []);
        Len ->
            MochiReq:recv_body(Len)
    end;
body(#httpd{req_body=ReqBody}) ->
    ReqBody.

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
    <<SigInt:128/integer>> = couch_util:md5(term_to_binary(Term)),
    iolist_to_binary([$", io_lib:format("~.36B", [SigInt]), $"]).

etag_match(Req, CurrentEtag) when is_binary(CurrentEtag) ->
    etag_match(Req, binary_to_list(CurrentEtag));

etag_match(Req, CurrentEtag) ->
    EtagsToMatch = string:tokens(
        header_value(Req, "If-None-Match", ""), ", "),
    lists:member(CurrentEtag, EtagsToMatch).

etag_respond(Req, CurrentEtag, RespFun) ->
    case etag_match(Req, CurrentEtag) of
    true ->
        % the client has this in their cache.
        send_response(Req, 304, [{"ETag", CurrentEtag}], <<>>);
    false ->
        % Run the function.
        RespFun()
    end.

verify_is_server_admin(#httpd{user_ctx=UserCtx}) ->
    verify_is_server_admin(UserCtx);
verify_is_server_admin(#user_ctx{roles=Roles}) ->
    case lists:member(<<"_admin">>, Roles) of
    true -> ok;
    false -> throw({unauthorized, <<"You are not a server admin.">>})
    end.

log_request(#httpd{mochi_req=MochiReq,peer=Peer}, Code) ->
    ?LOG_INFO("~s - - ~s ~s ~B", [
        Peer,
        MochiReq:get(method),
        MochiReq:get(raw_path),
        Code
    ]).


start_response_length(#httpd{mochi_req=MochiReq}=Req, Code, Headers, Length) ->
    log_request(Req, Code),
    couch_stats_collector:increment({httpd_status_codes, Code}),
    Resp = MochiReq:start_response_length({Code, Headers ++ server_header() ++ couch_httpd_auth:cookie_auth_header(Req, Headers), Length}),
    case MochiReq:get(method) of
    'HEAD' -> throw({http_head_abort, Resp});
    _ -> ok
    end,
    {ok, Resp}.

start_response(#httpd{mochi_req=MochiReq}=Req, Code, Headers) ->
    log_request(Req, Code),
    couch_stats_collector:increment({httpd_status_cdes, Code}),
    CookieHeader = couch_httpd_auth:cookie_auth_header(Req, Headers),
    Headers2 = Headers ++ server_header() ++ CookieHeader,
    Resp = MochiReq:start_response({Code, Headers2}),
    case MochiReq:get(method) of
        'HEAD' -> throw({http_head_abort, Resp});
        _ -> ok
    end,
    {ok, Resp}.

send(Resp, Data) ->
    Resp:send(Data),
    {ok, Resp}.

no_resp_conn_header([]) ->
    true;
no_resp_conn_header([{Hdr, _}|Rest]) ->
    case string:to_lower(Hdr) of
        "connection" -> false;
        _ -> no_resp_conn_header(Rest)
    end.

http_1_0_keep_alive(Req, Headers) ->
    KeepOpen = Req:should_close() == false,
    IsHttp10 = Req:get(version) == {1, 0},
    NoRespHeader = no_resp_conn_header(Headers),
    case KeepOpen andalso IsHttp10 andalso NoRespHeader of
        true -> [{"Connection", "Keep-Alive"} | Headers];
        false -> Headers
    end.

start_chunked_response(#httpd{mochi_req=MochiReq}=Req, Code, Headers) ->
    log_request(Req, Code),
    couch_stats_collector:increment({httpd_status_codes, Code}),
    Headers2 = http_1_0_keep_alive(MochiReq, Headers),
    Resp = MochiReq:respond({Code, Headers2 ++ server_header() ++ couch_httpd_auth:cookie_auth_header(Req, Headers2), chunked}),
    case MochiReq:get(method) of
    'HEAD' -> throw({http_head_abort, Resp});
    _ -> ok
    end,
    {ok, Resp}.

send_chunk(Resp, Data) ->
    case iolist_size(Data) of
    0 -> ok; % do nothing
    _ -> Resp:write_chunk(Data)
    end,
    {ok, Resp}.

last_chunk(Resp) ->
    Resp:write_chunk([]),
    {ok, Resp}.

send_response(#httpd{mochi_req=MochiReq}=Req, Code, Headers, Body) ->
    log_request(Req, Code),
    couch_stats_collector:increment({httpd_status_codes, Code}),
    Headers2 = http_1_0_keep_alive(MochiReq, Headers),
    if Code >= 400 ->
        ?LOG_DEBUG("httpd ~p error response:~n ~s", [Code, Body]);
    true -> ok
    end,
    {ok, MochiReq:respond({Code, Headers2 ++ server_header() ++ couch_httpd_auth:cookie_auth_header(Req, Headers2), Body})}.

send_method_not_allowed(Req, Methods) ->
    send_error(Req, 405, [{"Allow", Methods}], <<"method_not_allowed">>, ?l2b("Only " ++ Methods ++ " allowed")).

send_json(Req, Value) ->
    send_json(Req, 200, Value).

send_json(Req, Code, Value) ->
    send_json(Req, Code, [], Value).

send_json(Req, Code, Headers, Value) ->
    initialize_jsonp(Req),
    DefaultHeaders = [
        {"Content-Type", negotiate_content_type(Req)},
        {"Cache-Control", "must-revalidate"}
    ],
    Body = [start_jsonp(), ?JSON_ENCODE(Value), end_jsonp(), $\n],
    send_response(Req, Code, DefaultHeaders ++ Headers, Body).

start_json_response(Req, Code) ->
    start_json_response(Req, Code, []).

start_json_response(Req, Code, Headers) ->
    initialize_jsonp(Req),
    DefaultHeaders = [
        {"Content-Type", negotiate_content_type(Req)},
        {"Cache-Control", "must-revalidate"}
    ],
    {ok, Resp} = start_chunked_response(Req, Code, DefaultHeaders ++ Headers),
    case start_jsonp() of
        [] -> ok;
        Start -> send_chunk(Resp, Start)
    end,
    {ok, Resp}.

end_json_response(Resp) ->
    send_chunk(Resp, end_jsonp() ++ [$\n]),
    last_chunk(Resp).

initialize_jsonp(Req) ->
    case get(jsonp) of
        undefined -> put(jsonp, qs_value(Req, "callback", no_jsonp));
        _ -> ok
    end,
    case get(jsonp) of
        no_jsonp -> [];
        [] -> [];
        CallBack ->
            try
                % make sure jsonp is configured on (default off)
                case couch_config:get("httpd", "allow_jsonp", "false") of
                "true" ->
                    validate_callback(CallBack);
                _Else ->
                    put(jsonp, no_jsonp)
                end
            catch
                Error ->
                    put(jsonp, no_jsonp),
                    throw(Error)
            end
    end.

start_jsonp() ->
    case get(jsonp) of
        no_jsonp -> [];
        [] -> [];
        CallBack -> ["/* CouchDB */", CallBack, "("]
    end.

end_jsonp() ->
    case erlang:erase(jsonp) of
        no_jsonp -> [];
        [] -> [];
        _ -> ");"
    end.

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
    error_info({Error, ?l2b(Reason)});
error_info(bad_request) ->
    {400, <<"bad_request">>, <<>>};
error_info({bad_request, Reason}) ->
    {400, <<"bad_request">>, Reason};
error_info({query_parse_error, Reason}) ->
    {400, <<"query_parse_error">>, Reason};
% Prior art for md5 mismatch resulting in a 400 is from AWS S3
error_info(md5_mismatch) ->
    {400, <<"content_md5_mismatch">>, <<"Possible message corruption.">>};
error_info(not_found) ->
    {404, <<"not_found">>, <<"missing">>};
error_info({not_found, Reason}) ->
    {404, <<"not_found">>, Reason};
error_info({not_acceptable, Reason}) ->
    {406, <<"not_acceptable">>, Reason};
error_info(conflict) ->
    {409, <<"conflict">>, <<"Document update conflict.">>};
error_info({forbidden, Msg}) ->
    {403, <<"forbidden">>, Msg};
error_info({unauthorized, Msg}) ->
    {401, <<"unauthorized">>, Msg};
error_info(file_exists) ->
    {412, <<"file_exists">>, <<"The database could not be "
        "created, the file already exists.">>};
error_info({bad_ctype, Reason}) ->
    {415, <<"bad_content_type">>, Reason};
error_info(requested_range_not_satisfiable) ->
    {416, <<"requested_range_not_satisfiable">>, <<"Requested range not satisfiable">>};
error_info({error, illegal_database_name}) ->
    {400, <<"illegal_database_name">>, <<"Only lowercase characters (a-z), "
        "digits (0-9), and any of the characters _, $, (, ), +, -, and / "
        "are allowed. Must begin with a letter.">>};
error_info({missing_stub, Reason}) ->
    {412, <<"missing_stub">>, Reason};
error_info({Error, Reason}) ->
    ?LOG_ERROR("Uncaught server error: ~p", [{Error, Reason}]),
    {500, couch_util:to_binary(Error), couch_util:to_binary(Reason)};
error_info(Error) ->
    ?LOG_ERROR("Uncaught server error: ~p", [Error]),
    {500, <<"unknown_error">>, couch_util:to_binary(Error)}.

error_headers(#httpd{mochi_req=MochiReq}=Req, Code, ErrorStr, ReasonStr) ->
    if Code == 401 ->
        % this is where the basic auth popup is triggered
        case MochiReq:get_header_value("X-CouchDB-WWW-Authenticate") of
        undefined ->
            case couch_config:get("httpd", "WWW-Authenticate", nil) of
            nil ->
                % If the client is a browser and the basic auth popup isn't turned on
                % redirect to the session page.
                case ErrorStr of
                <<"unauthorized">> ->
                    case couch_config:get("couch_httpd_auth", "authentication_redirect", nil) of
                    nil -> {Code, []};
                    AuthRedirect ->
                        case couch_config:get("couch_httpd_auth", "require_valid_user", "false") of
                        "true" ->
                            % send the browser popup header no matter what if we are require_valid_user
                            {Code, [{"WWW-Authenticate", "Basic realm=\"server\""}]};
                        _False ->
                            case MochiReq:accepts_content_type("application/json") of
                            true ->
                                {Code, []};
                            false ->
                                case MochiReq:accepts_content_type("text/html") of
                                true ->
                                    % Redirect to the path the user requested, not
                                    % the one that is used internally.
                                    UrlReturnRaw = case MochiReq:get_header_value("x-couchdb-vhost-path") of
                                    undefined ->
                                        MochiReq:get(path);
                                    VHostPath ->
                                        VHostPath
                                    end,
                                    RedirectLocation = lists:flatten([
                                        AuthRedirect,
                                        "?return=", couch_util:url_encode(UrlReturnRaw),
                                        "&reason=", couch_util:url_encode(ReasonStr)
                                    ]),
                                    {302, [{"Location", absolute_uri(Req, RedirectLocation)}]};
                                false ->
                                    {Code, []}
                                end
                            end
                        end
                    end;
                _Else ->
                    {Code, []}
                end;
            Type ->
                {Code, [{"WWW-Authenticate", Type}]}
            end;
        Type ->
           {Code, [{"WWW-Authenticate", Type}]}
        end;
    true ->
        {Code, []}
    end.

send_error(_Req, {already_sent, Resp, _Error}) ->
    {ok, Resp};

send_error(Req, Error) ->
    {Code, ErrorStr, ReasonStr} = error_info(Error),
    {Code1, Headers} = error_headers(Req, Code, ErrorStr, ReasonStr),
    send_error(Req, Code1, Headers, ErrorStr, ReasonStr).

send_error(Req, Code, ErrorStr, ReasonStr) ->
    send_error(Req, Code, [], ErrorStr, ReasonStr).

send_error(Req, Code, Headers, ErrorStr, ReasonStr) ->
    send_json(Req, Code, Headers,
        {[{<<"error">>,  ErrorStr},
         {<<"reason">>, ReasonStr}]}).

% give the option for list functions to output html or other raw errors
send_chunked_error(Resp, {_Error, {[{<<"body">>, Reason}]}}) ->
    send_chunk(Resp, Reason),
    last_chunk(Resp);

send_chunked_error(Resp, Error) ->
    {Code, ErrorStr, ReasonStr} = error_info(Error),
    JsonError = {[{<<"code">>, Code},
        {<<"error">>,  ErrorStr},
        {<<"reason">>, ReasonStr}]},
    send_chunk(Resp, ?l2b([$\n,?JSON_ENCODE(JsonError),$\n])),
    last_chunk(Resp).

send_redirect(Req, Path) ->
     send_response(Req, 301, [{"Location", absolute_uri(Req, Path)}], <<>>).

negotiate_content_type(Req) ->
    case get(jsonp) of
        no_jsonp -> negotiate_content_type1(Req);
        [] -> negotiate_content_type1(Req);
        _Callback -> "text/javascript"
    end.

negotiate_content_type1(#httpd{mochi_req=MochiReq}) ->
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
        false -> "text/plain; charset=utf-8"
    end.

server_header() ->
    [{"Server", "CouchDB/" ++ couch_server:get_version() ++
                " (Erlang OTP/" ++ erlang:system_info(otp_release) ++ ")"}].


-record(mp, {boundary, buffer, data_fun, callback}).


parse_multipart_request(ContentType, DataFun, Callback) ->
    Boundary0 = iolist_to_binary(get_boundary(ContentType)),
    Boundary = <<"\r\n--", Boundary0/binary>>,
    Mp = #mp{boundary= Boundary,
            buffer= <<>>,
            data_fun=DataFun,
            callback=Callback},
    {Mp2, _NilCallback} = read_until(Mp, <<"--", Boundary0/binary>>,
        fun nil_callback/1),
    #mp{buffer=Buffer, data_fun=DataFun2, callback=Callback2} =
            parse_part_header(Mp2),
    {Buffer, DataFun2, Callback2}.

nil_callback(_Data)->
    fun nil_callback/1.

get_boundary({"multipart/" ++ _, Opts}) ->
    case couch_util:get_value("boundary", Opts) of
        S when is_list(S) ->
            S
    end;
get_boundary(ContentType) ->
    {"multipart/" ++ _ , Opts} = mochiweb_util:parse_header(ContentType),
    get_boundary({"multipart/", Opts}).
    


split_header(<<>>) ->
    [];
split_header(Line) ->
    {Name, [$: | Value]} = lists:splitwith(fun (C) -> C =/= $: end,
                                           binary_to_list(Line)),
    [{string:to_lower(string:strip(Name)),
     mochiweb_util:parse_header(Value)}].

read_until(#mp{data_fun=DataFun, buffer=Buffer}=Mp, Pattern, Callback) ->
    case find_in_binary(Pattern, Buffer) of
    not_found ->
        Callback2 = Callback(Buffer),
        {Buffer2, DataFun2} = DataFun(),
        Buffer3 = iolist_to_binary(Buffer2),
        read_until(Mp#mp{data_fun=DataFun2,buffer=Buffer3}, Pattern, Callback2);
    {partial, 0} ->
        {NewData, DataFun2} = DataFun(),
        read_until(Mp#mp{data_fun=DataFun2,
                buffer= iolist_to_binary([Buffer,NewData])},
                Pattern, Callback);
    {partial, Skip} ->
        <<DataChunk:Skip/binary, Rest/binary>> = Buffer,
        Callback2 = Callback(DataChunk),
        {NewData, DataFun2} = DataFun(),
        read_until(Mp#mp{data_fun=DataFun2,
                buffer= iolist_to_binary([Rest | NewData])},
                Pattern, Callback2);
    {exact, 0} ->
        PatternLen = size(Pattern),
        <<_:PatternLen/binary, Rest/binary>> = Buffer,
        {Mp#mp{buffer= Rest}, Callback};
    {exact, Skip} ->
        PatternLen = size(Pattern),
        <<DataChunk:Skip/binary, _:PatternLen/binary, Rest/binary>> = Buffer,
        Callback2 = Callback(DataChunk),
        {Mp#mp{buffer= Rest}, Callback2}
    end.


parse_part_header(#mp{callback=UserCallBack}=Mp) ->
    {Mp2, AccCallback} = read_until(Mp, <<"\r\n\r\n">>,
            fun(Next) -> acc_callback(Next, []) end),
    HeaderData = AccCallback(get_data),
    
    Headers =
    lists:foldl(fun(Line, Acc) ->
            split_header(Line) ++ Acc
        end, [], re:split(HeaderData,<<"\r\n">>, [])),
    NextCallback = UserCallBack({headers, Headers}),
    parse_part_body(Mp2#mp{callback=NextCallback}).

parse_part_body(#mp{boundary=Prefix, callback=Callback}=Mp) ->
    {Mp2, WrappedCallback} = read_until(Mp, Prefix,
            fun(Data) -> body_callback_wrapper(Data, Callback) end),
    Callback2 = WrappedCallback(get_callback),
    Callback3 = Callback2(body_end),
    case check_for_last(Mp2#mp{callback=Callback3}) of
    {last, #mp{callback=Callback3}=Mp3} ->
        Mp3#mp{callback=Callback3(eof)};
    {more, Mp3} ->
        parse_part_header(Mp3)
    end.

acc_callback(get_data, Acc)->
    iolist_to_binary(lists:reverse(Acc));
acc_callback(Data, Acc)->
    fun(Next) -> acc_callback(Next, [Data | Acc]) end.

body_callback_wrapper(get_callback, Callback) ->
    Callback;
body_callback_wrapper(Data, Callback) ->
    Callback2 = Callback({body, Data}),
    fun(Next) -> body_callback_wrapper(Next, Callback2) end.


check_for_last(#mp{buffer=Buffer, data_fun=DataFun}=Mp) ->
    case Buffer of
    <<"--",_/binary>> -> {last, Mp};
    <<_, _, _/binary>> -> {more, Mp};
    _ -> % not long enough
        {Data, DataFun2} = DataFun(),
        check_for_last(Mp#mp{buffer= <<Buffer/binary, Data/binary>>,
                data_fun = DataFun2})
    end.

find_in_binary(B, Data) when size(B) > 0 ->
    case size(Data) - size(B) of
        Last when Last < 0 ->
            partial_find(B, Data, 0, size(Data));
        Last ->
            find_in_binary(B, size(B), Data, 0, Last)
    end.

find_in_binary(B, BS, D, N, Last) when N =< Last->
    case D of
        <<_:N/binary, B:BS/binary, _/binary>> ->
            {exact, N};
        _ ->
            find_in_binary(B, BS, D, 1 + N, Last)
    end;
find_in_binary(B, BS, D, N, Last) when N =:= 1 + Last ->
    partial_find(B, D, N, BS - 1).

partial_find(_B, _D, _N, 0) ->
    not_found;
partial_find(B, D, N, K) ->
    <<B1:K/binary, _/binary>> = B,
    case D of
        <<_Skip:N/binary, B1/binary>> ->
            {partial, N};
        _ ->
            partial_find(B, D, 1 + N, K - 1)
    end.


