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

-export([start_link/0, start_link/1, start_link/2,
    stop/0, handle_request/1, handle_request_int/1,
    primary_header_value/2, header_value/2, header_value/3, qs_value/2,
    qs_value/3, qs/1, qs_json_value/3, path/1, absolute_uri/2, body_length/1,
    verify_is_server_admin/1, unquote/1, quote/1, recv/2, recv_chunked/4,
    error_info/1, parse_form/1, json_body/1, json_body_obj/1, body/1,
    doc_etag/1, make_etag/1, etag_respond/3, etag_match/2,
    partition/1, serve_file/3, serve_file/4,
    server_header/0, start_chunked_response/3,send_chunk/2,
    start_response_length/4, send/2, start_json_response/2,
    start_json_response/3, end_json_response/1, send_response/4,
    send_method_not_allowed/2, send_error/2, send_error/4, send_redirect/2,
    send_chunked_error/2, send_json/2,send_json/3,send_json/4]).

-export([authenticate_request/3]).

-export([start_delayed_json_response/2, start_delayed_json_response/3,
    start_delayed_json_response/4,
    start_delayed_chunked_response/3, start_delayed_chunked_response/4,
    send_delayed_chunk/2, send_delayed_last_chunk/1,
    send_delayed_error/2, end_delayed_json_response/1,
    get_delayed_req/1]).

-export([
    chunked_response_buffer_size/0,
    close_delayed_json_object/4
]).

-record(delayed_resp, {
    start_fun,
    req,
    code,
    headers,
    first_chunk,
    resp=nil
}).

start_link() ->
    start_link(http).
start_link(http) ->
    Port = config:get("chttpd", "port", "5984"),
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
            io:format("SSL enabled but PEM certificates are missing.", []),
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
                    [{verify_fun, couch_httpd:make_arity_3_fun(SpecStr)}]
            end
    end,
    SslOpts = ServerOpts ++ ClientOpts,

    Options =
        [{port, Port},
         {ssl, true},
         {ssl_opts, SslOpts}],
    start_link(https, Options).

start_link(Name, Options) ->
    IP = with_default(config:get("chttpd", "bind_address"), any),

    Options1 = Options ++ [
        {loop, fun ?MODULE:handle_request/1},
        {name, Name},
        {ip, IP}
    ],
    ServerOptsCfg = config:get("chttpd", "server_options", "[]"),
    {ok, ServerOpts} = couch_util:parse_term(ServerOptsCfg),
    Options2 = lists:keymerge(1, lists:sort(Options1), lists:sort(ServerOpts)),
    case mochiweb_http:start(Options2) of
    {ok, Pid} ->
        {ok, Pid};
    {error, Reason} ->
        io:format("Failure to start Mochiweb: ~s~n", [Reason]),
        {error, Reason}
    end.

stop() ->
    catch mochiweb_http:stop(https),
    mochiweb_http:stop(?MODULE).

handle_request(MochiReq0) ->
    erlang:put(?REWRITE_COUNT, 0),
    MochiReq = couch_httpd_vhost:dispatch_host(MochiReq0),
    handle_request_int(MochiReq).

handle_request_int(MochiReq) ->
    Begin = os:timestamp(),
    case config:get("chttpd", "socket_options") of
    undefined ->
        ok;
    SocketOptsCfg ->
        {ok, SocketOpts} = couch_util:parse_term(SocketOptsCfg),
        ok = mochiweb_socket:setopts(MochiReq:get(socket), SocketOpts)
    end,

    % for the path, use the raw path with the query string and fragment
    % removed, but URL quoting left intact
    RawUri = MochiReq:get(raw_path),
    {"/" ++ Path, _, _} = mochiweb_util:urlsplit_path(RawUri),

    Peer = MochiReq:get(peer),
    LogForClosedSocket = io_lib:format("mochiweb_recv_error for ~s - ~p ~s", [
        Peer,
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

    % allow broken HTTP clients to fake a full method vocabulary with an X-HTTP-METHOD-OVERRIDE header
    MethodOverride = MochiReq:get_primary_header_value("X-HTTP-Method-Override"),
    Method2 = case lists:member(MethodOverride, ["GET", "HEAD", "POST", "PUT", "DELETE", "TRACE", "CONNECT", "COPY"]) of
    true ->
        couch_log:notice("MethodOverride: ~s (real method was ~s)", [MethodOverride, Method1]),
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
        method = Method,
        path_parts = [list_to_binary(chttpd:unquote(Part))
                || Part <- string:tokens(Path, "/")]
    },

    HandlerKey =
        case HttpReq#httpd.path_parts of
            [] -> <<>>;
            [Key|_] -> ?l2b(quote(Key))
        end,

    % put small token on heap to keep requests synced to backend calls
    erlang:put(nonce, couch_util:to_hex(crypto:rand_bytes(5))),

    % suppress duplicate log
    erlang:put(dont_log_request, true),

    Result =
    try
        couch_httpd:validate_host(HttpReq),
        chttpd_csrf:validate(HttpReq),
        check_request_uri_length(RawUri),
        case chttpd_cors:maybe_handle_preflight_request(HttpReq) of
        not_preflight ->
            case chttpd_auth:authenticate(HttpReq, fun authenticate_request/1) of
            #httpd{} = Req ->
                HandlerFun = chttpd_handlers:url_handler(
                    HandlerKey, fun chttpd_db:handle_request/1),
                AuthorizedReq = chttpd_auth:authorize(possibly_hack(Req),
                    fun chttpd_auth_request:authorize_request/1),
                HandlerFun(AuthorizedReq);
            Response ->
                Response
            end;
        Response ->
            Response
        end
    catch
        throw:{http_head_abort, Resp0} ->
            {ok, Resp0};
        throw:{http_abort, Resp0, Reason0} ->
            {aborted, Resp0, Reason0};
        throw:{invalid_json, _} ->
            send_error(HttpReq, {bad_request, "invalid UTF-8 JSON"});
        exit:{mochiweb_recv_error, E} ->
            couch_log:notice(LogForClosedSocket ++ " - ~p", [E]),
            exit(normal);
        exit:{uri_too_long, _} ->
            send_error(HttpReq, request_uri_too_long);
        exit:{body_too_large, _} ->
            send_error(HttpReq, request_entity_too_large);
        throw:Error ->
            send_error(HttpReq, Error);
        error:database_does_not_exist ->
            send_error(HttpReq, database_does_not_exist);
        Tag:Error ->
            Stack = erlang:get_stacktrace(),
            % TODO improve logging and metrics collection for client disconnects
            case {Tag, Error, Stack} of
                {exit, normal, [{mochiweb_request, send, _, _} | _]} ->
                    exit(normal); % Client disconnect (R15+)
                {exit, normal, [{mochiweb_request, send, _} | _]} ->
                    exit(normal); % Client disconnect (R14)
                _Else ->
                    send_error(HttpReq, {Error, nil, Stack})
            end
    end,

    RequestTime = timer:now_diff(os:timestamp(), Begin)/1000,
    {Status, Code} = case Result of
    {ok, #delayed_resp{resp=Resp}} ->
        {ok, Resp:get(code)};
    {ok, Resp} ->
        {ok, Resp:get(code)};
    {aborted, Resp, _} ->
        {aborted, Resp:get(code)}
    end,
    Host = MochiReq:get_header_value("Host"),
    couch_log:notice("~s ~s ~s ~s ~s ~B ~p ~B", [get(nonce), Peer, Host,
        atom_to_list(Method1), RawUri, Code, Status, round(RequestTime)]),
    couch_stats:update_histogram([couchdb, request_time], RequestTime),
    case Result of
    {ok, _} ->
        couch_stats:increment_counter([couchdb, httpd, requests]),
        {ok, Resp};
    {aborted, _, Reason} ->
        couch_stats:increment_counter([couchdb, httpd, aborted_requests]),
        couch_log:error("Response abnormally terminated: ~p", [Reason]),
        exit(normal)
    end.

%% HACK: replication currently handles two forms of input, #db{} style
%% and #http_db style. We need a third that makes use of fabric. #db{}
%% works fine for replicating the dbs and nodes database because they
%% aren't sharded. So for now when a local db is specified as the source or
%% the target, it's hacked to make it a full url and treated as a remote.
possibly_hack(#httpd{path_parts=[<<"_replicate">>]}=Req) ->
    {Props0} = chttpd:json_body_obj(Req),
    Props1 = fix_uri(Req, Props0, <<"source">>),
    Props2 = fix_uri(Req, Props1, <<"target">>),
    put(post_body, {Props2}),
    Req;
possibly_hack(Req) ->
    Req.

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

fix_uri(Req, Props, Type) ->
    case replication_uri(Type, Props) of
    undefined ->
        Props;
    Uri0 ->
        case is_http(Uri0) of
        true ->
            Props;
        false ->
            Uri = make_uri(Req,replication_uri(Type, Props)),
            [{Type,Uri}|proplists:delete(Type,Props)]
        end
    end.

replication_uri(Type, PostProps) ->
    case couch_util:get_value(Type, PostProps) of
    {Props} ->
        couch_util:get_value(<<"url">>, Props);
    Else ->
        Else
    end.

is_http(<<"http://", _/binary>>) ->
    true;
is_http(<<"https://", _/binary>>) ->
    true;
is_http(_) ->
    false.

make_uri(Req, Raw) ->
    Url = list_to_binary(["http://", config:get("httpd", "bind_address"),
                         ":", config:get("chttpd", "port"), "/", Raw]),
    Headers = [
        {<<"authorization">>, ?l2b(header_value(Req,"authorization",""))},
        {<<"cookie">>, ?l2b(header_value(Req,"cookie",""))}
    ],
    {[{<<"url">>,Url}, {<<"headers">>,{Headers}}]}.
%%% end hack

authenticate_request(Req) ->
    AuthenticationFuns = [
        {<<"cookie">>, fun chttpd_auth:cookie_authentication_handler/1},
        {<<"default">>, fun chttpd_auth:default_authentication_handler/1},
        {<<"local">>, fun chttpd_auth:party_mode_handler/1} %% should be last
    ],
    authenticate_request(Req, chttpd_auth_cache, AuthenticationFuns).

authenticate_request(#httpd{} = Req0, AuthModule, AuthFuns) ->
    Req = Req0#httpd{
        auth_module = AuthModule,
        authentication_handlers = AuthFuns},
    authenticate_request(Req, AuthFuns).

% Try authentication handlers in order until one returns a result
authenticate_request(#httpd{user_ctx=#user_ctx{}} = Req, _AuthFuns) ->
    Req;
authenticate_request(#httpd{} = Req, [{Name, AuthFun}|Rest]) ->
    authenticate_request(maybe_set_handler(AuthFun(Req), Name), Rest);
authenticate_request(Response, _AuthFuns) ->
    Response.

maybe_set_handler(#httpd{user_ctx=#user_ctx{} = UserCtx} = Req, Name) ->
    Req#httpd{user_ctx = UserCtx#user_ctx{handler = Name}};
maybe_set_handler(Else, _) ->
    Else.

increment_method_stats(Method) ->
    couch_stats:increment_counter([couchdb, httpd_request_methods, Method]).

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

serve_file(Req, RelativePath, DocumentRoot) ->
    serve_file(Req, RelativePath, DocumentRoot, []).

serve_file(#httpd{mochi_req=MochiReq}=Req, RelativePath, DocumentRoot,
           ExtraHeaders) ->
    Headers = server_header() ++
	couch_httpd_auth:cookie_auth_header(Req, []) ++
	ExtraHeaders,
    Headers1 = chttpd_cors:headers(Req, Headers),
    Headers2 = chttpd_csrf:headers(Req, Headers1),
    {ok, MochiReq:serve_file(RelativePath, DocumentRoot, Headers2)}.

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

absolute_uri(#httpd{mochi_req=MochiReq}, Path) ->
    XHost = config:get("httpd", "x_forwarded_host", "X-Forwarded-Host"),
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
    XSsl = config:get("httpd", "x_forwarded_ssl", "X-Forwarded-Ssl"),
    Scheme = case MochiReq:get_header_value(XSsl) of
        "on" -> "https";
        _ ->
            XProto = config:get("httpd", "x_forwarded_proto",
                "X-Forwarded-Proto"),
            case MochiReq:get_header_value(XProto) of
                % Restrict to "https" and "http" schemes only
                "https" -> "https";
                _ ->
                    case MochiReq:get(scheme) of
                        https ->
                            "https";
                        http ->
                            "http"
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

body_length(#httpd{mochi_req=MochiReq}) ->
    MochiReq:get(body_length).

body(#httpd{mochi_req=MochiReq, req_body=ReqBody}) ->
    case ReqBody of
        undefined ->
            % Maximum size of document PUT request body (4GB)
            MaxSize = list_to_integer(
                config:get("couchdb", "max_document_size", "4294967296")),
            Begin = os:timestamp(),
            try
                MochiReq:recv_body(MaxSize)
            after
                T = timer:now_diff(os:timestamp(), Begin) div 1000,
                put(body_time, T)
            end;
        _Else ->
            ReqBody
    end.

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
        Headers0 = [{"Etag", CurrentEtag}],
        Headers1 = chttpd_cors:headers(Req, Headers0),
        Headers2 = chttpd_csrf:headers(Req, Headers1),
        chttpd:send_response(Req, 304, Headers2, <<>>);
    false ->
        % Run the function.
        RespFun()
    end.

verify_is_server_admin(#httpd{user_ctx=#user_ctx{roles=Roles}}) ->
    case lists:member(<<"_admin">>, Roles) of
    true -> ok;
    false -> throw({unauthorized, <<"You are not a server admin.">>})
    end.

start_response_length(#httpd{mochi_req=MochiReq}=Req, Code, Headers0, Length) ->
    couch_stats:increment_counter([couchdb, httpd_status_codes, Code]),
    Headers1 = Headers0 ++ server_header() ++
	couch_httpd_auth:cookie_auth_header(Req, Headers0),
    Headers2 = chttpd_cors:headers(Req, Headers1),
    Headers3 = chttpd_csrf:headers(Req, Headers2),
    Resp = MochiReq:start_response_length({Code, Headers3, Length}),
    case MochiReq:get(method) of
    'HEAD' -> throw({http_head_abort, Resp});
    _ -> ok
    end,
    {ok, Resp}.

send(Resp, Data) ->
    Resp:send(Data),
    {ok, Resp}.

start_chunked_response(#httpd{mochi_req=MochiReq}=Req, Code, Headers0) ->
    couch_stats:increment_counter([couchdb, httpd_status_codes, Code]),
    Headers1 = Headers0 ++ server_header() ++
        couch_httpd_auth:cookie_auth_header(Req, Headers0),
    Headers2 = chttpd_cors:headers(Req, Headers1),
    Headers3 = chttpd_csrf:headers(Req, Headers2),
    Resp = MochiReq:respond({Code, Headers3, chunked}),
    case MochiReq:get(method) of
    'HEAD' -> throw({http_head_abort, Resp});
    _ -> ok
    end,
    {ok, Resp}.

send_chunk(Resp, Data) ->
    Resp:write_chunk(Data),
    {ok, Resp}.

send_response(#httpd{mochi_req=MochiReq}=Req, Code, Headers0, Body) ->
    couch_stats:increment_counter([couchdb, httpd_status_codes, Code]),
    Headers = Headers0 ++ server_header() ++
	[timing(), reqid() | couch_httpd_auth:cookie_auth_header(Req, Headers0)],
    {ok, MochiReq:respond({Code, Headers, Body})}.


send_method_not_allowed(Req, Methods) ->
    send_error(Req, 405, [{"Allow", Methods}], <<"method_not_allowed">>,
        ?l2b("Only " ++ Methods ++ " allowed"), []).

send_json(Req, Value) ->
    send_json(Req, 200, Value).

send_json(Req, Code, Value) ->
    send_json(Req, Code, [], Value).

send_json(Req, Code, Headers0, Value) ->
    Headers1 = [timing(), reqid() | Headers0],
    Headers2 = chttpd_cors:headers(Req, Headers1),
    Headers3 = chttpd_csrf:headers(Req, Headers2),
    couch_httpd:send_json(Req, Code, Headers3, Value).

start_json_response(Req, Code) ->
    start_json_response(Req, Code, []).

start_json_response(Req, Code, Headers0) ->
    Headers1 = [timing(), reqid() | Headers0],
    Headers2 = chttpd_cors:headers(Req, Headers1),
    Headers3 = chttpd_csrf:headers(Req, Headers2),
    couch_httpd:start_json_response(Req, Code, Headers3).

end_json_response(Resp) ->
    couch_httpd:end_json_response(Resp).

start_delayed_json_response(Req, Code) ->
    start_delayed_json_response(Req, Code, []).

start_delayed_json_response(Req, Code, Headers) ->
    start_delayed_json_response(Req, Code, Headers, "").

start_delayed_json_response(Req, Code, Headers, FirstChunk) ->
    {ok, #delayed_resp{
        start_fun = fun start_json_response/3,
        req = Req,
        code = Code,
        headers = Headers,
        first_chunk = FirstChunk}}.

start_delayed_chunked_response(Req, Code, Headers) ->
    start_delayed_chunked_response(Req, Code, Headers, "").

start_delayed_chunked_response(Req, Code, Headers, FirstChunk) ->
    {ok, #delayed_resp{
        start_fun = fun start_chunked_response/3,
        req = Req,
        code = Code,
        headers = Headers,
        first_chunk = FirstChunk}}.

send_delayed_chunk(#delayed_resp{}=DelayedResp, Chunk) ->
    {ok, #delayed_resp{resp=Resp}=DelayedResp1} =
        start_delayed_response(DelayedResp),
    {ok, Resp} = send_chunk(Resp, Chunk),
    {ok, DelayedResp1}.

send_delayed_last_chunk(Req) ->
    send_delayed_chunk(Req, []).

send_delayed_error(#delayed_resp{req=Req,resp=nil}=DelayedResp, Reason) ->
    {Code, ErrorStr, ReasonStr} = error_info(Reason),
    {ok, Resp} = send_error(Req, Code, ErrorStr, ReasonStr),
    {ok, DelayedResp#delayed_resp{resp=Resp}};
send_delayed_error(#delayed_resp{resp=Resp}, Reason) ->
    log_error_with_stack_trace(Reason),
    throw({http_abort, Resp, Reason}).

close_delayed_json_object(Resp, Buffer, Terminator, 0) ->
    % Use a separate chunk to close the streamed array to maintain strict
    % compatibility with earlier versions. See COUCHDB-2724
    {ok, R1} = chttpd:send_delayed_chunk(Resp, Buffer),
    send_delayed_chunk(R1, Terminator);
close_delayed_json_object(Resp, Buffer, Terminator, _Threshold) ->
    send_delayed_chunk(Resp, [Buffer | Terminator]).

end_delayed_json_response(#delayed_resp{}=DelayedResp) ->
    {ok, #delayed_resp{resp=Resp}} =
        start_delayed_response(DelayedResp),
    end_json_response(Resp).

get_delayed_req(#delayed_resp{req=#httpd{mochi_req=MochiReq}}) ->
    MochiReq;
get_delayed_req(Resp) ->
    Resp:get(request).

start_delayed_response(#delayed_resp{resp=nil}=DelayedResp) ->
    #delayed_resp{
        start_fun=StartFun,
        req=Req,
        code=Code,
        headers=Headers,
        first_chunk=FirstChunk
    }=DelayedResp,
    {ok, Resp} = StartFun(Req, Code, Headers),
    case FirstChunk of
        "" -> ok;
        _ -> {ok, Resp} = send_chunk(Resp, FirstChunk)
    end,
    {ok, DelayedResp#delayed_resp{resp=Resp}};
start_delayed_response(#delayed_resp{}=DelayedResp) ->
    {ok, DelayedResp}.

error_info({Error, Reason}) when is_list(Reason) ->
    error_info({Error, couch_util:to_binary(Reason)});
error_info(bad_request) ->
    {400, <<"bad_request">>, <<>>};
error_info({bad_request, Reason}) ->
    {400, <<"bad_request">>, Reason};
error_info({bad_request, Error, Reason}) ->
    {400, couch_util:to_binary(Error), couch_util:to_binary(Reason)};
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
error_info({forbidden, Error, Msg}) ->
    {403, Error, Msg};
error_info({forbidden, Msg}) ->
    {403, <<"forbidden">>, Msg};
error_info({unauthorized, Msg}) ->
    {401, <<"unauthorized">>, Msg};
error_info(file_exists) ->
    {412, <<"file_exists">>, <<"The database could not be "
        "created, the file already exists.">>};
error_info({r_quorum_not_met, Reason}) ->
    {412, <<"read_quorum_not_met">>, Reason};
error_info({error, {nodedown, Reason}}) ->
    {412, <<"nodedown">>, Reason};
error_info({maintenance_mode, Node}) ->
    {412, <<"nodedown">>, Node};
error_info({maintenance_mode, nil, Node}) ->
    {412, <<"nodedown">>, Node};
error_info({w_quorum_not_met, Reason}) ->
    {500, <<"write_quorum_not_met">>, Reason};
error_info(request_uri_too_long) ->
    {414, <<"too_long">>, <<"the request uri is too long">>};
error_info({bad_ctype, Reason}) ->
    {415, <<"bad_content_type">>, Reason};
error_info(requested_range_not_satisfiable) ->
    {416, <<"requested_range_not_satisfiable">>, <<"Requested range not satisfiable">>};
error_info({error, illegal_database_name}) ->
    {400, <<"illegal_database_name">>, <<"Only lowercase letters (a-z), "
        "digits (0-9), and any of the characters _, $, (, ), +, -, and / are "
        "allowed. Moreover, the database name must begin with a letter.">>};
error_info({missing_stub, Reason}) ->
    {412, <<"missing_stub">>, Reason};
error_info(request_entity_too_large) ->
    {413, <<"too_large">>, <<"the request entity is too large">>};
error_info({error, security_migration_updates_disabled}) ->
    {503, <<"security_migration">>, <<"Updates to security docs are disabled during "
        "security migration.">>};
error_info(not_implemented) ->
    {501, <<"not_implemented">>, <<"this feature is not yet implemented">>};
error_info(timeout) ->
    {500, <<"timeout">>, <<"The request could not be processed in a reasonable"
        " amount of time.">>};
error_info({timeout, _Reason}) ->
    error_info(timeout);
error_info({Error, null}) ->
    error_info(Error);
error_info({Error, Reason}) ->
    {500, couch_util:to_binary(Error), couch_util:to_binary(Reason)};
error_info({Error, nil, _Stack}) ->
    error_info(Error);
error_info({Error, Reason, _Stack}) ->
    {500, couch_util:to_binary(Error), couch_util:to_binary(Reason)};
error_info(Error) ->
    {500, <<"unknown_error">>, couch_util:to_binary(Error)}.

error_headers(#httpd{mochi_req=MochiReq}=Req, 401=Code, ErrorStr, ReasonStr) ->
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
                        % if the accept header matches html, then do the redirect. else proceed as usual.
                        Accepts = case MochiReq:get_header_value("Accept") of
                        undefined ->
                           % According to the HTTP 1.1 spec, if the Accept
                           % header is missing, it means the client accepts
                           % all media types.
                           "html";
                        Else ->
                            Else
                        end,
                        case re:run(Accepts, "\\bhtml\\b",
                                [{capture, none}, caseless]) of
                        nomatch ->
                            {Code, []};
                        match ->
                            AuthRedirectBin = ?l2b(AuthRedirect),
                            % Redirect to the path the user requested, not
                            % the one that is used internally.
                            UrlReturnRaw = case MochiReq:get_header_value("x-couchdb-vhost-path") of
                                undefined -> MochiReq:get(path);
                                VHostPath -> VHostPath
                            end,
                            UrlReturn = ?l2b(couch_util:url_encode(UrlReturnRaw)),
                            UrlReason = ?l2b(couch_util:url_encode(ReasonStr)),
                            {302, [{"Location", couch_httpd:absolute_uri(Req, <<AuthRedirectBin/binary,"?return=",UrlReturn/binary,"&reason=",UrlReason/binary>>)}]}
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
error_headers(_, Code, _, _) ->
    {Code, []}.

send_error(_Req, {already_sent, Resp, _Error}) ->
    {ok, Resp};

send_error(Req, Error) ->
    {Code, ErrorStr, ReasonStr} = error_info(Error),
    {Code1, Headers} = error_headers(Req, Code, ErrorStr, ReasonStr),
    send_error(Req, Code1, Headers, ErrorStr, ReasonStr, json_stack(Error)).

send_error(Req, Code, ErrorStr, ReasonStr) ->
    send_error(Req, Code, [], ErrorStr, ReasonStr, []).

send_error(Req, Code, Headers, ErrorStr, ReasonStr, []) ->
    send_json(Req, Code, Headers,
        {[{<<"error">>,  ErrorStr},
        {<<"reason">>, ReasonStr}]});
send_error(Req, Code, Headers, ErrorStr, ReasonStr, Stack) ->
    log_error_with_stack_trace({ErrorStr, ReasonStr, Stack}),
    send_json(Req, Code, [stack_trace_id(Stack) | Headers],
        {[{<<"error">>,  ErrorStr},
        {<<"reason">>, ReasonStr} |
        case Stack of [] -> []; _ -> [{<<"ref">>, stack_hash(Stack)}] end
    ]}).

% give the option for list functions to output html or other raw errors
send_chunked_error(Resp, {_Error, {[{<<"body">>, Reason}]}}) ->
    send_chunk(Resp, Reason),
    send_chunk(Resp, []);

send_chunked_error(Resp, Error) ->
    Stack = json_stack(Error),
    log_error_with_stack_trace(Error),
    {Code, ErrorStr, ReasonStr} = error_info(Error),
    JsonError = {[{<<"code">>, Code},
        {<<"error">>,  ErrorStr},
        {<<"reason">>, ReasonStr} |
        case Stack of [] -> []; _ -> [{<<"ref">>, stack_hash(Stack)}] end
    ]},
    send_chunk(Resp, ?l2b([$\n,?JSON_ENCODE(JsonError),$\n])),
    send_chunk(Resp, []).

send_redirect(Req, Path) ->
    Headers0 = [{"Location", chttpd:absolute_uri(Req, Path)}],
    Headers1 = chttpd_cors:headers(Req, Headers0),
    Headers2 = chttpd_csrf:headers(Req, Headers1),
    send_response(Req, 301, Headers2, <<>>).

server_header() ->
    couch_httpd:server_header().

timing() ->
    case get(body_time) of
        undefined ->
            {"X-CouchDB-Body-Time", "0"};
        Time ->
            {"X-CouchDB-Body-Time", integer_to_list(Time)}
    end.

reqid() ->
    {"X-Couch-Request-ID", get(nonce)}.

json_stack({bad_request, _, _}) ->
    [];
json_stack({_Error, _Reason, Stack}) when is_list(Stack) ->
    lists:map(fun json_stack_item/1, Stack);
json_stack(_) ->
    [].

json_stack_item({M,F,A}) ->
    list_to_binary(io_lib:format("~s:~s/~B", [M, F, json_stack_arity(A)]));
json_stack_item({M,F,A,L}) ->
    case proplists:get_value(line, L) of
    undefined -> json_stack_item({M,F,A});
    Line -> list_to_binary(io_lib:format("~s:~s/~B L~B",
        [M, F, json_stack_arity(A), Line]))
    end;
json_stack_item(_) ->
    <<"bad entry in stacktrace">>.

json_stack_arity(A) ->
    if is_integer(A) -> A; is_list(A) -> length(A); true -> 0 end.

maybe_decompress(Httpd, Body) ->
    case header_value(Httpd, "Content-Encoding", "identity") of
    "gzip" ->
        try
            zlib:gunzip(Body)
        catch error:data_error ->
            throw({bad_request, "Request body is not properly gzipped."})
        end;
    "identity" ->
        Body;
    Else ->
        throw({bad_ctype, [Else, " is not a supported content encoding."]})
    end.

log_error_with_stack_trace({bad_request, _, _}) ->
    ok;
log_error_with_stack_trace({Error, Reason, Stack}) ->
    EFmt = if is_binary(Error) -> "~s"; true -> "~w" end,
    RFmt = if is_binary(Reason) -> "~s"; true -> "~w" end,
    Fmt = "req_err(~w) " ++ EFmt ++ " : " ++ RFmt ++ "~n    ~p",
    couch_log:error(Fmt, [stack_hash(Stack), Error, Reason, Stack]);
log_error_with_stack_trace(_) ->
    ok.

stack_trace_id(Stack) ->
    {"X-Couch-Stack-Hash", stack_hash(Stack)}.

stack_hash(Stack) ->
    erlang:crc32(term_to_binary(Stack)).

with_default(undefined, Default) -> Default;
with_default(Value, _) -> Value.

%% @doc CouchDB uses a chunked transfer-encoding to stream responses to
%% _all_docs, _changes, _view and other similar requests. This configuration
%% value sets the maximum size of a chunk; the system will buffer rows in the
%% response until it reaches this threshold and then send all the rows in one
%% chunk to improve network efficiency. The default value is chosen so that
%% the assembled chunk fits into the default Ethernet frame size (some reserved
%% padding is necessary to accommodate the reporting of the chunk length). Set
%% this value to 0 to restore the older behavior of sending each row in a
%% dedicated chunk.
chunked_response_buffer_size() ->
    config:get_integer("httpd", "chunked_response_buffer", 1490).
