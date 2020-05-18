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

-compile(tuple_calls).

-include_lib("couch/include/couch_db.hrl").

-export([start_link/0, start_link/1, stop/0, handle_request/5]).

-export([header_value/2,header_value/3,qs_value/2,qs_value/3,qs/1,qs_json_value/3]).
-export([path/1,absolute_uri/2,body_length/1]).
-export([verify_is_server_admin/1,unquote/1,quote/1,recv/2,recv_chunked/4,error_info/1]).
-export([make_fun_spec_strs/1]).
-export([make_arity_1_fun/1, make_arity_2_fun/1, make_arity_3_fun/1]).
-export([parse_form/1,json_body/1,json_body_obj/1,body/1]).
-export([doc_etag/1, doc_etag/3, make_etag/1, etag_match/2, etag_respond/3, etag_maybe/2]).
-export([primary_header_value/2,partition/1,serve_file/3,serve_file/4, server_header/0]).
-export([start_chunked_response/3,send_chunk/2,log_request/2]).
-export([start_response_length/4, start_response/3, send/2]).
-export([start_json_response/2, start_json_response/3, end_json_response/1]).
-export([send_response/4,send_response_no_cors/4,send_method_not_allowed/2,
    send_error/2,send_error/4, send_redirect/2,send_chunked_error/2]).
-export([send_json/2,send_json/3,send_json/4,last_chunk/1,parse_multipart_request/3]).
-export([accepted_encodings/1,handle_request_int/5,validate_referer/1,validate_ctype/2]).
-export([http_1_0_keep_alive/2]).
-export([validate_host/1]).
-export([validate_bind_address/1]).
-export([check_max_request_length/1]).
-export([handle_request/1]).
-export([set_auth_handlers/0]).

-define(HANDLER_NAME_IN_MODULE_POS, 6).
-define(MAX_DRAIN_BYTES, 1048576).
-define(MAX_DRAIN_TIME_MSEC, 1000).

start_link() ->
    start_link(http).
start_link(http) ->
    Port = config:get("httpd", "port", "5984"),
    start_link(?MODULE, [{port, Port}]);
start_link(https) ->
    Port = config:get("ssl", "port", "6984"),
    {ok, Ciphers} = couch_util:parse_term(config:get("ssl", "ciphers", undefined)),
    {ok, Versions} = couch_util:parse_term(config:get("ssl", "tls_versions", undefined)),
    {ok, SecureRenegotiate} = couch_util:parse_term(config:get("ssl", "secure_renegotiate", undefined)),
    ServerOpts0 =
        [{cacertfile, config:get("ssl", "cacert_file", undefined)},
         {keyfile, config:get("ssl", "key_file", undefined)},
         {certfile, config:get("ssl", "cert_file", undefined)},
         {password, config:get("ssl", "password", undefined)},
         {secure_renegotiate, SecureRenegotiate},
         {versions, Versions},
         {ciphers, Ciphers}],

    case (couch_util:get_value(keyfile, ServerOpts0) == undefined orelse
        couch_util:get_value(certfile, ServerOpts0) == undefined) of
        true ->
            couch_log:error("SSL enabled but PEM certificates are missing", []),
            throw({error, missing_certs});
        false ->
            ok
    end,

    ServerOpts = [Opt || {_, V}=Opt <- ServerOpts0, V /= undefined],

    ClientOpts = case config:get("ssl", "verify_ssl_certificates", "false") of
        "false" ->
            [];
        "true" ->
            FailIfNoPeerCert = case config:get("ssl", "fail_if_no_peer_cert", "false") of
            "false" -> false;
            "true" -> true
            end,
            [{depth, list_to_integer(config:get("ssl",
                "ssl_certificate_max_depth", "1"))},
             {fail_if_no_peer_cert, FailIfNoPeerCert},
             {verify, verify_peer}] ++
            case config:get("ssl", "verify_fun", undefined) of
                undefined -> [];
                SpecStr ->
                    [{verify_fun, make_arity_3_fun(SpecStr)}]
            end
    end,
    SslOpts = ServerOpts ++ ClientOpts,

    Options =
        [{port, Port},
         {ssl, true},
         {ssl_opts, SslOpts}],
    start_link(https, Options).
start_link(Name, Options) ->
    BindAddress = case config:get("httpd", "bind_address", "any") of
                      "any" -> any;
                      Else -> Else
                  end,
    ok = validate_bind_address(BindAddress),

    {ok, ServerOptions} = couch_util:parse_term(
        config:get("httpd", "server_options", "[]")),
    {ok, SocketOptions} = couch_util:parse_term(
        config:get("httpd", "socket_options", "[]")),

    set_auth_handlers(),
    Handlers = get_httpd_handlers(),

    % ensure uuid is set so that concurrent replications
    % get the same value.
    couch_server:get_uuid(),

    Loop = fun(Req)->
        case SocketOptions of
        [] ->
            ok;
        _ ->
            ok = mochiweb_socket:setopts(Req:get(socket), SocketOptions)
        end,
        apply(?MODULE, handle_request, [Req | Handlers])
    end,

    % set mochiweb options
    FinalOptions = lists:append([Options, ServerOptions, [
            {loop, Loop},
            {name, Name},
            {ip, BindAddress}]]),

    % launch mochiweb
    case mochiweb_http:start(FinalOptions) of
        {ok, MochiPid} ->
            {ok, MochiPid};
        {error, Reason} ->
            couch_log:error("Failure to start Mochiweb: ~s~n", [Reason]),
            throw({error, Reason})
    end.


stop() ->
    mochiweb_http:stop(couch_httpd),
    catch mochiweb_http:stop(https).


set_auth_handlers() ->
    AuthenticationSrcs = make_fun_spec_strs(
        config:get("httpd", "authentication_handlers", "")),
    AuthHandlers = lists:map(
        fun(A) -> {auth_handler_name(A), make_arity_1_fun(A)} end, AuthenticationSrcs),
    AuthenticationFuns = AuthHandlers ++ [
        fun couch_httpd_auth:party_mode_handler/1 %% must be last
    ],
    ok = application:set_env(couch, auth_handlers, AuthenticationFuns).

auth_handler_name(SpecStr) ->
    lists:nth(?HANDLER_NAME_IN_MODULE_POS, re:split(SpecStr, "[\\W_]", [])).

get_httpd_handlers() ->
    {ok, HttpdGlobalHandlers} = application:get_env(couch, httpd_global_handlers),

    UrlHandlersList = lists:map(
        fun({UrlKey, SpecStr}) ->
            {?l2b(UrlKey), make_arity_1_fun(SpecStr)}
        end, HttpdGlobalHandlers),

    {ok, HttpdDbHandlers} = application:get_env(couch, httpd_db_handlers),

    DbUrlHandlersList = lists:map(
        fun({UrlKey, SpecStr}) ->
            {?l2b(UrlKey), make_arity_2_fun(SpecStr)}
        end, HttpdDbHandlers),

    {ok, HttpdDesignHandlers} = application:get_env(couch, httpd_design_handlers),

    DesignUrlHandlersList = lists:map(
        fun({UrlKey, SpecStr}) ->
            {?l2b(UrlKey), make_arity_3_fun(SpecStr)}
        end, HttpdDesignHandlers),

    UrlHandlers = dict:from_list(UrlHandlersList),
    DbUrlHandlers = dict:from_list(DbUrlHandlersList),
    DesignUrlHandlers = dict:from_list(DesignUrlHandlersList),
    DefaultFun = make_arity_1_fun("{couch_httpd_db, handle_request}"),
    [DefaultFun, UrlHandlers, DbUrlHandlers, DesignUrlHandlers].

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

handle_request(MochiReq) ->
    Body = proplists:get_value(body, MochiReq:get(opts)),
    erlang:put(mochiweb_request_body, Body),
    apply(?MODULE, handle_request, [MochiReq | get_httpd_handlers()]).

handle_request(MochiReq, DefaultFun, UrlHandlers, DbUrlHandlers,
    DesignUrlHandlers) ->
    %% reset rewrite count for new request
    erlang:put(?REWRITE_COUNT, 0),

    MochiReq1 = couch_httpd_vhost:dispatch_host(MochiReq),

    handle_request_int(MochiReq1, DefaultFun,
                UrlHandlers, DbUrlHandlers, DesignUrlHandlers).

handle_request_int(MochiReq, DefaultFun,
            UrlHandlers, DbUrlHandlers, DesignUrlHandlers) ->
    Begin = os:timestamp(),
    % for the path, use the raw path with the query string and fragment
    % removed, but URL quoting left intact
    RawUri = MochiReq:get(raw_path),
    {"/" ++ Path, _, _} = mochiweb_util:urlsplit_path(RawUri),

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
    couch_log:debug("~p ~s ~p from ~p~nHeaders: ~p", [
        MochiReq:get(method),
        RawUri,
        MochiReq:get(version),
        peer(MochiReq),
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
    Method2 = case lists:member(MethodOverride, ["GET", "HEAD", "POST",
                                                 "PUT", "DELETE",
                                                 "TRACE", "CONNECT",
                                                 "COPY"]) of
    true ->
        couch_log:info("MethodOverride: ~s (real method was ~s)",
                       [MethodOverride, Method1]),
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
        peer = peer(MochiReq),
        method = Method,
        requested_path_parts =
            [?l2b(unquote(Part)) || Part <- string:tokens(RequestedPath, "/")],
        path_parts = [?l2b(unquote(Part)) || Part <- string:tokens(Path, "/")],
        db_url_handlers = DbUrlHandlers,
        design_url_handlers = DesignUrlHandlers,
        default_fun = DefaultFun,
        url_handlers = UrlHandlers,
        user_ctx = erlang:erase(pre_rewrite_user_ctx),
        auth = erlang:erase(pre_rewrite_auth)
    },

    HandlerFun = couch_util:dict_find(HandlerKey, UrlHandlers, DefaultFun),

    {ok, Resp} =
    try
        validate_host(HttpReq),
        check_request_uri_length(RawUri),
        case chttpd_cors:maybe_handle_preflight_request(HttpReq) of
        not_preflight ->
            case authenticate_request(HttpReq) of
            #httpd{} = Req ->
                HandlerFun(Req);
            Response ->
                Response
            end;
        Response ->
            Response
        end
    catch
        throw:{http_head_abort, Resp0} ->
            {ok, Resp0};
        throw:{invalid_json, S} ->
            couch_log:error("attempted upload of invalid JSON"
                            " (set log_level to debug to log it)", []),
            couch_log:debug("Invalid JSON: ~p",[S]),
            send_error(HttpReq, {bad_request, invalid_json});
        throw:unacceptable_encoding ->
            couch_log:error("unsupported encoding method for the response", []),
            send_error(HttpReq, {not_acceptable, "unsupported encoding"});
        throw:bad_accept_encoding_value ->
            couch_log:error("received invalid Accept-Encoding header", []),
            send_error(HttpReq, bad_request);
        exit:normal ->
            exit(normal);
        exit:snappy_nif_not_loaded ->
            ErrorReason = "To access the database or view index, Apache CouchDB"
                          " must be built with Erlang OTP R13B04 or higher.",
            couch_log:error("~s", [ErrorReason]),
            send_error(HttpReq, {bad_otp_release, ErrorReason});
        exit:{body_too_large, _} ->
            send_error(HttpReq, request_entity_too_large);
        exit:{uri_too_long, _} ->
            send_error(HttpReq, request_uri_too_long);
        throw:Error ->
            Stack = erlang:get_stacktrace(),
            couch_log:debug("Minor error in HTTP request: ~p",[Error]),
            couch_log:debug("Stacktrace: ~p",[Stack]),
            send_error(HttpReq, Error);
        error:badarg ->
            Stack = erlang:get_stacktrace(),
            couch_log:error("Badarg error in HTTP request",[]),
            couch_log:info("Stacktrace: ~p",[Stack]),
            send_error(HttpReq, badarg);
        error:function_clause ->
            Stack = erlang:get_stacktrace(),
            couch_log:error("function_clause error in HTTP request",[]),
            couch_log:info("Stacktrace: ~p",[Stack]),
            send_error(HttpReq, function_clause);
        Tag:Error ->
            Stack = erlang:get_stacktrace(),
            couch_log:error("Uncaught error in HTTP request: ~p",
                            [{Tag, Error}]),
            couch_log:info("Stacktrace: ~p",[Stack]),
            send_error(HttpReq, Error)
    end,
    RequestTime = round(timer:now_diff(os:timestamp(), Begin)/1000),
    couch_stats:update_histogram([couchdb, request_time], RequestTime),
    couch_stats:increment_counter([couchdb, httpd, requests]),
    {ok, Resp}.

validate_host(#httpd{} = Req) ->
    case config:get_boolean("httpd", "validate_host", false) of
        true ->
            Host = hostname(Req),
            ValidHosts = valid_hosts(),
            case lists:member(Host, ValidHosts) of
                true ->
                    ok;
                false ->
                    throw({bad_request, <<"Invalid host header">>})
            end;
        false ->
            ok
    end.

hostname(#httpd{} = Req) ->
    case header_value(Req, "Host") of
        undefined ->
            undefined;
        Host ->
            [Name | _] = re:split(Host, ":[0-9]+$", [{parts, 2}, {return, list}]),
            Name
    end.

valid_hosts() ->
    List = config:get("httpd", "valid_hosts", ""),
    re:split(List, ",", [{return, list}]).

check_request_uri_length(Uri) ->
    check_request_uri_length(Uri, config:get("httpd", "max_uri_length")).

check_request_uri_length(_Uri, undefined) ->
    ok;
check_request_uri_length(Uri, MaxUriLen) when is_list(MaxUriLen) ->
    case length(Uri) > list_to_integer(MaxUriLen) of
        true ->
            throw(request_uri_too_long);
        false ->
            ok
    end.

authenticate_request(Req) ->
    {ok, AuthenticationFuns} = application:get_env(couch, auth_handlers),
    chttpd:authenticate_request(Req, couch_auth_cache, AuthenticationFuns).

increment_method_stats(Method) ->
    couch_stats:increment_counter([couchdb, httpd_request_methods, Method]).

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
        [Ctype | _Rest] -> ok;
        _Else ->
            throw({bad_ctype, "Content-Type must be "++Ctype})
        end
    end.


check_max_request_length(Req) ->
    Len = list_to_integer(header_value(Req, "Content-Length", "0")),
    MaxLen = config:get_integer("httpd", "max_http_request_size", 4294967296),
    case Len > MaxLen of
        true ->
            exit({body_too_large, Len});
        false ->
            ok
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

serve_file(Req0, RelativePath0, DocumentRoot0, ExtraHeaders) ->
    Headers0 = basic_headers(Req0, ExtraHeaders),
    {ok, {Req1, Code1, Headers1, RelativePath1, DocumentRoot1}} =
        chttpd_plugin:before_serve_file(
            Req0, 200, Headers0, RelativePath0, DocumentRoot0),
    log_request(Req1, Code1),
    #httpd{mochi_req = MochiReq} = Req1,
    {ok, MochiReq:serve_file(RelativePath1, DocumentRoot1, Headers1)}.

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
    XHost = config:get("httpd", "x_forwarded_host", "X-Forwarded-Host"),
    case MochiReq:get_header_value(XHost) of
        undefined ->
            case MochiReq:get_header_value("Host") of
                undefined ->
                    {ok, {Address, Port}} = case MochiReq:get(socket) of
                        {ssl, SslSocket} -> ssl:sockname(SslSocket);
                        Socket -> inet:sockname(Socket)
                    end,
                    inet_parse:ntoa(Address) ++ ":" ++ integer_to_list(Port);
                Value1 ->
                    Value1
            end;
        Value -> Value
    end.

absolute_uri(#httpd{mochi_req=MochiReq}=Req, [$/ | _] = Path) ->
    Host = host_for_request(Req),
    XSsl = config:get("httpd", "x_forwarded_ssl", "X-Forwarded-Ssl"),
    Scheme = case MochiReq:get_header_value(XSsl) of
                 "on" -> "https";
                 _ ->
                     XProto = config:get("httpd", "x_forwarded_proto", "X-Forwarded-Proto"),
                     case MochiReq:get_header_value(XProto) of
                         %% Restrict to "https" and "http" schemes only
                         "https" -> "https";
                         _ -> case MochiReq:get(scheme) of
                                  https -> "https";
                                  http -> "http"
                              end
                     end
             end,
    Scheme ++ "://" ++ Host ++ Path;
absolute_uri(_Req, _Path) ->
    throw({bad_request, "path must begin with a /."}).

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

body_length(#httpd{mochi_req=MochiReq}) ->
    MochiReq:get(body_length).

body(#httpd{mochi_req=MochiReq, req_body=undefined}) ->
    MaxSize = config:get_integer("httpd", "max_http_request_size", 4294967296),
    MochiReq:recv_body(MaxSize);
body(#httpd{req_body=ReqBody}) ->
    ReqBody.

json_body(Httpd) ->
    case body(Httpd) of
        undefined ->
            throw({bad_request, "Missing request body"});
        Body ->
            ?JSON_DECODE(maybe_decompress(Httpd, Body))
    end.

json_body_obj(Httpd) ->
    case json_body(Httpd) of
        {Props} -> {Props};
        _Else ->
            throw({bad_request, "Request body must be a JSON object"})
    end.


maybe_decompress(Httpd, Body) ->
    case header_value(Httpd, "Content-Encoding", "identity") of
    "gzip" ->
        zlib:gunzip(Body);
    "identity" ->
        Body;
    Else ->
        throw({bad_ctype, [Else, " is not a supported content encoding."]})
    end.

doc_etag(#doc{id=Id, body=Body, revs={Start, [DiskRev|_]}}) ->
    doc_etag(Id, Body, {Start, DiskRev}).

doc_etag(<<"_local/", _/binary>>, Body, {Start, DiskRev}) ->
    make_etag({Start, DiskRev, Body});
doc_etag(_Id, _Body, {Start, DiskRev}) ->
    rev_etag({Start, DiskRev}).

rev_etag({Start, DiskRev}) ->
    Rev = couch_doc:rev_to_str({Start, DiskRev}),
     <<$", Rev/binary, $">>.

make_etag(Term) ->
    <<SigInt:128/integer>> = couch_hash:md5_hash(term_to_binary(Term)),
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

etag_maybe(Req, RespFun) ->
    try
        RespFun()
    catch
        throw:{etag_match, ETag} ->
            send_response(Req, 304, [{"ETag", ETag}], <<>>)
    end.

verify_is_server_admin(#httpd{user_ctx=UserCtx}) ->
    verify_is_server_admin(UserCtx);
verify_is_server_admin(#user_ctx{roles=Roles}) ->
    case lists:member(<<"_admin">>, Roles) of
    true -> ok;
    false -> throw({unauthorized, <<"You are not a server admin.">>})
    end.

log_request(#httpd{mochi_req=MochiReq,peer=Peer}=Req, Code) ->
    case erlang:get(dont_log_request) of
        true ->
            ok;
        _ ->
            couch_log:notice("~s - - ~s ~s ~B", [
                Peer,
                MochiReq:get(method),
                MochiReq:get(raw_path),
                Code
            ]),
            gen_event:notify(couch_plugin, {log_request, Req, Code})
    end.

log_response(Code, _) when Code < 400 ->
    ok;
log_response(Code, Body) ->
    case {erlang:get(dont_log_response), Body} of
        {true, _} ->
            ok;
        {_, {json, JsonObj}} ->
            ErrorMsg = couch_util:json_encode(JsonObj),
            couch_log:error("httpd ~p error response:~n ~s", [Code, ErrorMsg]);
        _ ->
            couch_log:error("httpd ~p error response:~n ~s", [Code, Body])
    end.

start_response_length(#httpd{mochi_req=MochiReq}=Req, Code, Headers0, Length) ->
    Headers1 = basic_headers(Req, Headers0),
    Resp = handle_response(Req, Code, Headers1, Length, start_response_length),
    case MochiReq:get(method) of
    'HEAD' -> throw({http_head_abort, Resp});
    _ -> ok
    end,
    {ok, Resp}.

start_response(#httpd{mochi_req=MochiReq}=Req, Code, Headers0) ->
    Headers1 = basic_headers(Req, Headers0),
    Resp = handle_response(Req, Code, Headers1, undefined, start_response),
    case MochiReq:get(method) of
        'HEAD' -> throw({http_head_abort, Resp});
        _ -> ok
    end,
    {ok, Resp}.

send({remote, Pid, Ref} = Resp, Data) ->
    Pid ! {Ref, send, Data},
    {ok, Resp};
send(Resp, Data) ->
    Resp:send(Data),
    {ok, Resp}.

no_resp_conn_header([]) ->
    true;
no_resp_conn_header([{Hdr, V}|Rest]) when is_binary(Hdr)->
    no_resp_conn_header([{?b2l(Hdr), V}|Rest]);
no_resp_conn_header([{Hdr, _}|Rest]) when is_list(Hdr)->
    case string:to_lower(Hdr) of
        "connection" -> false;
        _ -> no_resp_conn_header(Rest)
    end.

http_1_0_keep_alive(#httpd{mochi_req = MochiReq}, Headers) ->
    http_1_0_keep_alive(MochiReq, Headers);
http_1_0_keep_alive(Req, Headers) ->
    KeepOpen = Req:should_close() == false,
    IsHttp10 = Req:get(version) == {1, 0},
    NoRespHeader = no_resp_conn_header(Headers),
    case KeepOpen andalso IsHttp10 andalso NoRespHeader of
        true -> [{"Connection", "Keep-Alive"} | Headers];
        false -> Headers
    end.

start_chunked_response(#httpd{mochi_req=MochiReq}=Req, Code, Headers0) ->
    Headers1 = add_headers(Req, Headers0),
    Resp = handle_response(Req, Code, Headers1, chunked, respond),
    case MochiReq:get(method) of
    'HEAD' -> throw({http_head_abort, Resp});
    _ -> ok
    end,
    {ok, Resp}.

send_chunk({remote, Pid, Ref} = Resp, Data) ->
    Pid ! {Ref, chunk, Data},
    {ok, Resp};
send_chunk(Resp, Data) ->
    case iolist_size(Data) of
    0 -> ok; % do nothing
    _ -> Resp:write_chunk(Data)
    end,
    {ok, Resp}.

last_chunk({remote, Pid, Ref} = Resp) ->
    Pid ! {Ref, chunk, <<>>},
    {ok, Resp};
last_chunk(Resp) ->
    Resp:write_chunk([]),
    {ok, Resp}.

send_response(Req, Code, Headers0, Body) ->
    Headers1 = chttpd_cors:headers(Req, Headers0),
    send_response_no_cors(Req, Code, Headers1, Body).

send_response_no_cors(#httpd{mochi_req=MochiReq}=Req, Code, Headers, Body) ->
    Headers1 = http_1_0_keep_alive(MochiReq, Headers),
    Headers2 = basic_headers_no_cors(Req, Headers1),
    Headers3 = chttpd_xframe_options:header(Req, Headers2),
	Headers4 = chttpd_prefer_header:maybe_return_minimal(Req, Headers3),
    Resp = handle_response(Req, Code, Headers4, Body, respond),
    log_response(Code, Body),
    {ok, Resp}.

send_method_not_allowed(Req, Methods) ->
    send_error(Req, 405, [{"Allow", Methods}], <<"method_not_allowed">>, ?l2b("Only " ++ Methods ++ " allowed")).

send_json(Req, Value) ->
    send_json(Req, 200, Value).

send_json(Req, Code, Value) ->
    send_json(Req, Code, [], Value).

send_json(Req, Code, Headers, Value) ->
    initialize_jsonp(Req),
    AllHeaders = maybe_add_default_headers(Req, Headers),
    send_response(Req, Code, AllHeaders, {json, Value}).

start_json_response(Req, Code) ->
    start_json_response(Req, Code, []).

start_json_response(Req, Code, Headers) ->
    initialize_jsonp(Req),
    AllHeaders = maybe_add_default_headers(Req, Headers),
    {ok, Resp} = start_chunked_response(Req, Code, AllHeaders),
    case start_jsonp() of
        [] -> ok;
        Start -> send_chunk(Resp, Start)
    end,
    {ok, Resp}.

end_json_response(Resp) ->
    send_chunk(Resp, end_jsonp() ++ [$\n]),
    last_chunk(Resp).

maybe_add_default_headers(ForRequest, ToHeaders) ->
    DefaultHeaders = [
        {"Cache-Control", "must-revalidate"},
        {"Content-Type", negotiate_content_type(ForRequest)}
    ],
    lists:ukeymerge(1, lists:keysort(1, ToHeaders), DefaultHeaders).

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
                case config:get("httpd", "allow_jsonp", "false") of
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
error_info({illegal_docid, Reason}) ->
    {400, <<"illegal_docid">>, Reason};
error_info({illegal_partition, Reason}) ->
    {400, <<"illegal_partition">>, Reason};
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
error_info(request_entity_too_large) ->
    {413, <<"too_large">>, <<"the request entity is too large">>};
error_info({request_entity_too_large, {attachment, AttName}}) ->
    {413, <<"attachment_too_large">>, AttName};
error_info({request_entity_too_large, DocID}) ->
    {413, <<"document_too_large">>, DocID};
error_info(request_uri_too_long) ->
    {414, <<"too_long">>, <<"the request uri is too long">>};
error_info({bad_ctype, Reason}) ->
    {415, <<"bad_content_type">>, Reason};
error_info(requested_range_not_satisfiable) ->
    {416, <<"requested_range_not_satisfiable">>, <<"Requested range not satisfiable">>};
error_info({error, {illegal_database_name, Name}}) ->
    Message = <<"Name: '", Name/binary, "'. Only lowercase characters (a-z), ",
        "digits (0-9), and any of the characters _, $, (, ), +, -, and / ",
        "are allowed. Must begin with a letter.">>,
    {400, <<"illegal_database_name">>, Message};
error_info({missing_stub, Reason}) ->
    {412, <<"missing_stub">>, Reason};
error_info({misconfigured_server, Reason}) ->
    {500, <<"misconfigured_server">>, couch_util:to_binary(Reason)};
error_info({Error, Reason}) ->
    {500, couch_util:to_binary(Error), couch_util:to_binary(Reason)};
error_info(Error) ->
    {500, <<"unknown_error">>, couch_util:to_binary(Error)}.

error_headers(#httpd{mochi_req=MochiReq}=Req, Code, ErrorStr, ReasonStr) ->
    if Code == 401 ->
        % this is where the basic auth popup is triggered
        case MochiReq:get_header_value("X-CouchDB-WWW-Authenticate") of
        undefined ->
            case config:get("httpd", "WWW-Authenticate", undefined) of
            undefined ->
                % If the client is a browser and the basic auth popup isn't turned on
                % redirect to the session page.
                case ErrorStr of
                <<"unauthorized">> ->
                    case config:get("couch_httpd_auth", "authentication_redirect", undefined) of
                    undefined -> {Code, []};
                    AuthRedirect ->
                        case config:get("couch_httpd_auth", "require_valid_user", "false") of
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

negotiate_content_type(_Req) ->
    case get(jsonp) of
        no_jsonp -> "application/json";
        [] -> "application/json";
        _Callback -> "application/javascript"
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
    {Name, Rest} = lists:splitwith(fun (C) -> C =/= $: end,
                                   binary_to_list(Line)),
    [$: | Value] = case Rest of
        [] ->
            throw({bad_request, <<"bad part header">>});
        Res ->
            Res
    end,
    [{string:to_lower(string:strip(Name)),
     mochiweb_util:parse_header(Value)}].

read_until(#mp{data_fun=DataFun, buffer=Buffer}=Mp, Pattern, Callback) ->
    case couch_util:find_in_binary(Pattern, Buffer) of
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

validate_bind_address(any) -> ok;
validate_bind_address(Address) ->
    case inet_parse:address(Address) of
        {ok, _} -> ok;
        _ -> throw({error, invalid_bind_address})
    end.

add_headers(Req, Headers0) ->
    Headers = basic_headers(Req, Headers0),
    Headers1 = http_1_0_keep_alive(Req, Headers),
    chttpd_prefer_header:maybe_return_minimal(Req, Headers1).

basic_headers(Req, Headers0) ->
    Headers1 = basic_headers_no_cors(Req, Headers0),
    Headers2 = chttpd_xframe_options:header(Req, Headers1),
    chttpd_cors:headers(Req, Headers2).

basic_headers_no_cors(Req, Headers) ->
    Headers
        ++ server_header()
        ++ couch_httpd_auth:cookie_auth_header(Req, Headers).

handle_response(Req0, Code0, Headers0, Args0, Type) ->
    {ok, {Req1, Code1, Headers1, Args1}} = before_response(Req0, Code0, Headers0, Args0),
    couch_stats:increment_counter([couchdb, httpd_status_codes, Code1]),
    log_request(Req0, Code1),
    respond_(Req1, Code1, Headers1, Args1, Type).

before_response(Req0, Code0, Headers0, {json, JsonObj}) ->
    {ok, {Req1, Code1, Headers1, Body1}} =
        chttpd_plugin:before_response(Req0, Code0, Headers0, JsonObj),
    Body2 = [start_jsonp(), ?JSON_ENCODE(Body1), end_jsonp(), $\n],
    {ok, {Req1, Code1, Headers1, Body2}};
before_response(Req0, Code0, Headers0, Args0) ->
    chttpd_plugin:before_response(Req0, Code0, Headers0, Args0).

respond_(#httpd{mochi_req = MochiReq} = Req, Code, Headers, Args, Type) ->
    case MochiReq:get(socket) of
        {remote, Pid, Ref} ->
            Pid ! {Ref, Code, Headers, Args, Type},
            {remote, Pid, Ref};
        _Else ->
            http_respond_(Req, Code, Headers, Args, Type)
    end.

http_respond_(#httpd{mochi_req = MochiReq}, Code, Headers, _Args, start_response) ->
    MochiReq:start_response({Code, Headers});
http_respond_(#httpd{mochi_req = MochiReq}, 413, Headers, Args, Type) ->
    % Special handling for the 413 response. Make sure the socket is closed as
    % we don't know how much data was read before the error was thrown. Also
    % drain all the data in the receive buffer to avoid connction being reset
    % before the 413 response is parsed by the client. This is still racy, it
    % just increases the chances of 413 being detected correctly by the client
    % (rather than getting a brutal TCP reset).
    erlang:put(mochiweb_request_force_close, true),
    Result = MochiReq:Type({413, Headers, Args}),
    Socket = MochiReq:get(socket),
    mochiweb_socket:recv(Socket, ?MAX_DRAIN_BYTES, ?MAX_DRAIN_TIME_MSEC),
    Result;
http_respond_(#httpd{mochi_req = MochiReq}, Code, Headers, Args, Type) ->
    MochiReq:Type({Code, Headers, Args}).

peer(MochiReq) ->
    case MochiReq:get(socket) of
        {remote, Pid, _} ->
            node(Pid);
        _ ->
            MochiReq:get(peer)
    end.

%%%%%%%% module tests below %%%%%%%%

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

maybe_add_default_headers_test_() ->
    DummyRequest = [],
    NoCache = {"Cache-Control", "no-cache"},
    ApplicationJson = {"Content-Type", "application/json"},
    % couch_httpd uses process dictionary to check if currently in a
    % json serving method. Defaults to 'application/javascript' otherwise.
    % Therefore must-revalidate and application/javascript should be added
    % by chttpd if such headers are not present
    MustRevalidate = {"Cache-Control", "must-revalidate"},
    ApplicationJavascript = {"Content-Type", "application/javascript"},
    Cases = [
        {[],
         [MustRevalidate, ApplicationJavascript],
          "Should add Content-Type and Cache-Control to empty heaeders"},

        {[NoCache],
         [NoCache, ApplicationJavascript],
          "Should add Content-Type only if Cache-Control is present"},

        {[ApplicationJson],
         [MustRevalidate, ApplicationJson],
          "Should add Cache-Control if Content-Type is present"},

        {[NoCache, ApplicationJson],
         [NoCache, ApplicationJson],
          "Should not add headers if Cache-Control and Content-Type are there"}
    ],
    Tests = lists:map(fun({InitialHeaders, ProperResult, Desc}) ->
        {Desc,
        ?_assertEqual(ProperResult,
         maybe_add_default_headers(DummyRequest, InitialHeaders))}
    end, Cases),
    {"Tests adding default headers", Tests}.

log_request_test_() ->
    {setup,
        fun() ->
            ok = meck:new([couch_log]),
            ok = meck:expect(couch_log, error, fun(Fmt, Args) ->
                case catch io_lib_format:fwrite(Fmt, Args) of
                    {'EXIT', Error} -> Error;
                    _ -> ok
                end
            end)
        end,
        fun(_) ->
            meck:unload()
        end,
        [
            fun() -> should_accept_code_and_message(true) end,
            fun() -> should_accept_code_and_message(false) end
        ]
    }.

should_accept_code_and_message(DontLogFlag) ->
    erlang:put(dont_log_response, DontLogFlag),
    {"with dont_log_response = " ++ atom_to_list(DontLogFlag),
        [
            {"Should accept code 200 and string message",
            ?_assertEqual(ok, log_response(200, "OK"))},
            {"Should accept code 200 and JSON message",
            ?_assertEqual(ok, log_response(200, {json, {[{ok, true}]}}))},
            {"Should accept code >= 400 and string error",
            ?_assertEqual(ok, log_response(405, method_not_allowed))},
            {"Should accept code >= 400 and JSON error",
            ?_assertEqual(ok,
                log_response(405, {json, {[{error, method_not_allowed}]}}))},
            {"Should accept code >= 500 and string error",
            ?_assertEqual(ok, log_response(500, undef))},
            {"Should accept code >= 500 and JSON error",
            ?_assertEqual(ok, log_response(500, {json, {[{error, undef}]}}))}
        ]
    }.

-endif.
