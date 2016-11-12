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
-include_lib("chttpd/include/chttpd.hrl").

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
    send_response_no_cors/4,
    send_method_not_allowed/2, send_error/2, send_error/4, send_redirect/2,
    send_chunked_error/2, send_json/2,send_json/3,send_json/4,
    validate_ctype/2]).

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
    {ok, Ciphers} = couch_util:parse_term(config:get("ssl", "ciphers", "undefined")),
    {ok, Versions} = couch_util:parse_term(config:get("ssl", "tls_versions", "undefined")),
    {ok, SecureRenegotiate} = couch_util:parse_term(config:get("ssl", "secure_renegotiate", "undefined")),
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
    IP = case config:get("chttpd", "bind_address", "any") of
             "any" -> any;
             Else -> Else
         end,
    ok = couch_httpd:validate_bind_address(IP),

    set_auth_handlers(),

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

    % get requested path
    RequestedPath = case MochiReq:get_header_value("x-couchdb-vhost-path") of
        undefined ->
            case MochiReq:get_header_value("x-couchdb-requested-path") of
                undefined -> RawUri;
                R -> R
            end;
        P -> P
    end,

    Peer = MochiReq:get(peer),

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

    Nonce = couch_util:to_hex(crypto:rand_bytes(5)),

    HttpReq0 = #httpd{
        mochi_req = MochiReq,
        begin_ts = Begin,
        peer = Peer,
        original_method = Method1,
        nonce = Nonce,
        method = Method,
        path_parts = [list_to_binary(chttpd:unquote(Part))
                || Part <- string:tokens(Path, "/")],
        requested_path_parts = [?l2b(unquote(Part))
                || Part <- string:tokens(RequestedPath, "/")]
    },

    % put small token on heap to keep requests synced to backend calls
    erlang:put(nonce, Nonce),

    % suppress duplicate log
    erlang:put(dont_log_request, true),
    erlang:put(dont_log_response, true),

    {HttpReq2, Response} = case before_request(HttpReq0) of
        {ok, HttpReq1} ->
            process_request(HttpReq1);
        {error, Response0} ->
            {HttpReq0, Response0}
    end,

    {Status, Code, Reason, Resp} = split_response(Response),

    HttpResp = #httpd_resp{
        code = Code,
        status = Status,
        response = Resp,
        nonce = HttpReq2#httpd.nonce,
        reason = Reason
    },

    case after_request(HttpReq2, HttpResp) of
        #httpd_resp{status = ok, response = Resp} ->
            {ok, Resp};
        #httpd_resp{status = aborted, reason = Reason} ->
            couch_log:error("Response abnormally terminated: ~p", [Reason]),
            exit(normal)
    end.

before_request(HttpReq) ->
    try
        chttpd_plugin:before_request(HttpReq)
    catch Tag:Error ->
        {error, catch_error(HttpReq, Tag, Error)}
    end.

after_request(HttpReq, HttpResp0) ->
    {ok, HttpResp1} =
        try
            chttpd_plugin:after_request(HttpReq, HttpResp0)
        catch _Tag:Error ->
            Stack = erlang:get_stacktrace(),
            send_error(HttpReq, {Error, nil, Stack}),
            {ok, HttpResp0#httpd_resp{status = aborted}}
        end,
    HttpResp2 = update_stats(HttpReq, HttpResp1),
    maybe_log(HttpReq, HttpResp2),
    HttpResp2.

process_request(#httpd{mochi_req = MochiReq} = HttpReq) ->
    HandlerKey =
        case HttpReq#httpd.path_parts of
            [] -> <<>>;
            [Key|_] -> ?l2b(quote(Key))
        end,

    RawUri = MochiReq:get(raw_path),

    try
        couch_httpd:validate_host(HttpReq),
        check_request_uri_length(RawUri),
        check_url_encoding(RawUri),
        case chttpd_cors:maybe_handle_preflight_request(HttpReq) of
        not_preflight ->
            case chttpd_auth:authenticate(HttpReq, fun authenticate_request/1) of
            #httpd{} = Req ->
                HandlerFun = chttpd_handlers:url_handler(
                    HandlerKey, fun chttpd_db:handle_request/1),
                AuthorizedReq = chttpd_auth:authorize(possibly_hack(Req),
                    fun chttpd_auth_request:authorize_request/1),
                {AuthorizedReq, HandlerFun(AuthorizedReq)};
            Response ->
                {HttpReq, Response}
            end;
        Response ->
            {HttpReq, Response}
        end
    catch Tag:Error ->
        {HttpReq, catch_error(HttpReq, Tag, Error)}
    end.

catch_error(_HttpReq, throw, {http_head_abort, Resp}) ->
    {ok, Resp};
catch_error(_HttpReq, throw, {http_abort, Resp, Reason}) ->
    {aborted, Resp, Reason};
catch_error(HttpReq, throw, {invalid_json, _}) ->
    send_error(HttpReq, {bad_request, "invalid UTF-8 JSON"});
catch_error(HttpReq, exit, {mochiweb_recv_error, E}) ->
    #httpd{
        mochi_req = MochiReq,
        peer = Peer,
        original_method = Method
    } = HttpReq,
    couch_log:notice("mochiweb_recv_error for ~s - ~p ~s - ~p", [
        Peer,
        Method,
        MochiReq:get(raw_path),
        E]),
    exit(normal);
catch_error(HttpReq, exit, {uri_too_long, _}) ->
    send_error(HttpReq, request_uri_too_long);
catch_error(HttpReq, exit, {body_too_large, _}) ->
    send_error(HttpReq, request_entity_too_large);
catch_error(HttpReq, throw, Error) ->
    send_error(HttpReq, Error);
catch_error(HttpReq, error, database_does_not_exist) ->
    send_error(HttpReq, database_does_not_exist);
catch_error(HttpReq, Tag, Error) ->
    Stack = erlang:get_stacktrace(),
    % TODO improve logging and metrics collection for client disconnects
    case {Tag, Error, Stack} of
        {exit, normal, [{mochiweb_request, send, _, _} | _]} ->
            exit(normal); % Client disconnect (R15+)
        _Else ->
            send_error(HttpReq, {Error, nil, Stack})
    end.

split_response({ok, #delayed_resp{resp=Resp}}) ->
    {ok, Resp:get(code), undefined, Resp};
split_response({ok, Resp}) ->
    {ok, Resp:get(code), undefined, Resp};
split_response({aborted, Resp, AbortReason}) ->
    {aborted, Resp:get(code), AbortReason, Resp}.

update_stats(HttpReq, #httpd_resp{end_ts = undefined} = Res) ->
    update_stats(HttpReq, Res#httpd_resp{end_ts = os:timestamp()});
update_stats(#httpd{begin_ts = BeginTime}, #httpd_resp{} = Res) ->
    #httpd_resp{status = Status, end_ts = EndTime} = Res,
    RequestTime = timer:now_diff(EndTime, BeginTime) / 1000,
    couch_stats:update_histogram([couchdb, request_time], RequestTime),
    case Status of
        ok ->
            couch_stats:increment_counter([couchdb, httpd, requests]);
        aborted ->
            couch_stats:increment_counter([couchdb, httpd, aborted_requests])
    end,
    Res.

maybe_log(#httpd{} = HttpReq, #httpd_resp{should_log = true} = HttpResp) ->
    #httpd{
        mochi_req = MochiReq,
        begin_ts = BeginTime,
        original_method = Method,
        peer = Peer
    } = HttpReq,
    #httpd_resp{
        end_ts = EndTime,
        code = Code,
        status = Status
    } = HttpResp,
    User = get_user(HttpReq),
    Host = MochiReq:get_header_value("Host"),
    RawUri = MochiReq:get(raw_path),
    RequestTime = timer:now_diff(EndTime, BeginTime) / 1000,
    couch_log:notice("~s ~s ~s ~s ~s ~B ~p ~B", [Host, Peer, User,
        Method, RawUri, Code, Status, round(RequestTime)]);
maybe_log(_HttpReq, #httpd_resp{should_log = false}) ->
    ok.


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

check_url_encoding([]) ->
    ok;
check_url_encoding([$%, A, B | Rest]) when ?is_hex(A), ?is_hex(B) ->
    check_url_encoding(Rest);
check_url_encoding([$% | _]) ->
    throw({bad_request, invalid_url_encoding});
check_url_encoding([_ | Rest]) ->
    check_url_encoding(Rest).

fix_uri(Req, Props, Type) ->
    case replication_uri(Type, Props) of
    undefined ->
        Props;
    Uri0 ->
        case is_http(Uri0) of
        true ->
            Props;
        false ->
            Uri = make_uri(Req, quote(Uri0)),
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
    Port = integer_to_list(mochiweb_socket_server:get(chttpd, port)),
    Url = list_to_binary(["http://", config:get("httpd", "bind_address"),
                          ":", Port, "/", Raw]),
    Headers = [
        {<<"authorization">>, ?l2b(header_value(Req,"authorization",""))},
        {<<"cookie">>, ?l2b(extract_cookie(Req))}
    ],
    {[{<<"url">>,Url}, {<<"headers">>,{Headers}}]}.

extract_cookie(#httpd{mochi_req = MochiReq}) ->
    case MochiReq:get_cookie_value("AuthSession") of
        undefined ->
            "";
        AuthSession ->
            "AuthSession=" ++ AuthSession
    end.
%%% end hack

set_auth_handlers() ->
    AuthenticationDefault =  "{chttpd_auth, cookie_authentication_handler},
      {chttpd_auth, default_authentication_handler}",
    AuthenticationSrcs = couch_httpd:make_fun_spec_strs(
        config:get("chttpd", "authentication_handlers", AuthenticationDefault)),
    AuthHandlers = lists:map(
        fun(A) -> {auth_handler_name(A), couch_httpd:make_arity_1_fun(A)} end, AuthenticationSrcs),
    AuthenticationFuns = AuthHandlers ++ [
        fun chttpd_auth:party_mode_handler/1 %% must be last
    ],
    ok = application:set_env(chttpd, auth_handlers, AuthenticationFuns).

% SpecStr is a string like "{my_module, my_fun}"
% Takes the first token of the function name in front '_' as auth handler name
% e.g.
% chttpd_auth:default_authentication_handler: default
% chttpd_auth_cookie_authentication_handler: cookie
% couch_http_auth:proxy_authentication_handler: proxy
%
% couch_http:auth_handler_name can't be used here, since it assumes the name
% of the auth handler to be the 6th token split by [\\W_]
% - this only works for modules with exactly two underscores in their name
% - is not very robust (a space after the ',' is assumed)
auth_handler_name(SpecStr) ->
    {ok, {_, Fun}} = couch_util:parse_term(SpecStr),
    hd(binary:split(atom_to_binary(Fun, latin1), <<"_">>)).

authenticate_request(Req) ->
    {ok, AuthenticationFuns} = application:get_env(chttpd, auth_handlers),
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
authenticate_request(#httpd{} = Req, [AuthFun|Rest]) ->
    authenticate_request(AuthFun(Req), Rest);
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

serve_file(Req0, RelativePath0, DocumentRoot0, ExtraHeaders) ->
    couch_httpd:serve_file(Req0, RelativePath0, DocumentRoot0, ExtraHeaders).

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

qs(#httpd{mochi_req = MochiReq, qs = undefined}) ->
    MochiReq:parse_qs();
qs(#httpd{qs = QS}) ->
    QS.

path(#httpd{mochi_req=MochiReq}) ->
    MochiReq:get(path).

absolute_uri(#httpd{mochi_req=MochiReq, absolute_uri = undefined}, Path) ->
    XHost = config:get("httpd", "x_forwarded_host", "X-Forwarded-Host"),
    Host = case MochiReq:get_header_value(XHost) of
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
    Scheme ++ "://" ++ Host ++ Path;
absolute_uri(#httpd{absolute_uri = URI}, Path) ->
    URI ++ Path.

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

validate_ctype(Req, Ctype) ->
    couch_httpd:validate_ctype(Req, Ctype).

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


doc_etag(#doc{id=Id, body=Body, revs={Start, [DiskRev|_]}}) ->
    couch_httpd:doc_etag(Id, Body, {Start, DiskRev}).

make_etag(Term) ->
    <<SigInt:128/integer>> = couch_crypto:hash(md5, term_to_binary(Term)),
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
        Headers = [{"ETag", CurrentEtag}],
        chttpd:send_response(Req, 304, Headers, <<>>);
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
    Headers1 = basic_headers(Req, Headers0),
    Resp = handle_response(Req, Code, Headers1, Length, start_response_length),
    case MochiReq:get(method) of
    'HEAD' -> throw({http_head_abort, Resp});
    _ -> ok
    end,
    {ok, Resp}.

send(Resp, Data) ->
    Resp:send(Data),
    {ok, Resp}.

start_chunked_response(#httpd{mochi_req=MochiReq}=Req, Code, Headers0) ->
    Headers1 = basic_headers(Req, Headers0),
    Resp = handle_response(Req, Code, Headers1, chunked, respond),
    case MochiReq:get(method) of
    'HEAD' -> throw({http_head_abort, Resp});
    _ -> ok
    end,
    {ok, Resp}.

send_chunk(Resp, Data) ->
    Resp:write_chunk(Data),
    {ok, Resp}.

send_response(Req, Code, Headers0, Body) ->
    Headers1 = [timing(), reqid() | Headers0],
    couch_httpd:send_response(Req, Code, Headers1, Body).

send_response_no_cors(Req, Code, Headers0, Body) ->
    Headers1 = [timing(), reqid() | Headers0],
    couch_httpd:send_response_no_cors(Req, Code, Headers1, Body).

send_method_not_allowed(Req, Methods) ->
    send_error(Req, 405, [{"Allow", Methods}], <<"method_not_allowed">>,
        ?l2b("Only " ++ Methods ++ " allowed"), []).

send_json(Req, Value) ->
    send_json(Req, 200, Value).

send_json(Req, Code, Value) ->
    send_json(Req, Code, [], Value).

send_json(Req, Code, Headers0, Value) ->
    Headers1 = [timing(), reqid() | Headers0],
    couch_httpd:send_json(Req, Code, Headers1, Value).

start_json_response(Req, Code) ->
    start_json_response(Req, Code, []).

start_json_response(Req, Code, Headers0) ->
    Headers1 = [timing(), reqid() | Headers0],
    couch_httpd:start_json_response(Req, Code, Headers1).

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
error_info({error, {illegal_database_name, Name}}) ->
    Message = <<"Name: '", Name/binary, "'. Only lowercase characters (a-z), ",
        "digits (0-9), and any of the characters _, $, (, ), +, -, and / ",
        "are allowed. Must begin with a letter.">>,
    {400, <<"illegal_database_name">>, Message};
error_info({illegal_docid, Reason}) ->
    {400, <<"illegal_docid">>, Reason};
error_info({_DocID,{illegal_docid,DocID}}) ->
    {400, <<"illegal_docid">>,DocID};
error_info({error, {database_name_too_long, DbName}}) ->
    {400, <<"database_name_too_long">>,
        <<"At least one path segment of `", DbName/binary, "` is too long.">>};
error_info({doc_validation, Reason}) ->
    {400, <<"doc_validation">>, Reason};
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
error_info({_Error, _Reason} = Error) ->
    maybe_handle_error(Error);
error_info({Error, nil, _Stack}) ->
    error_info(Error);
error_info({Error, Reason, _Stack}) ->
    error_info({Error, Reason});
error_info(Error) ->
    maybe_handle_error(Error).

maybe_handle_error(Error) ->
    case chttpd_plugin:handle_error(Error) of
        {_Code, _Reason, _Description} = Result ->
            Result;
        {Err, Reason} ->
            {500, couch_util:to_binary(Err), couch_util:to_binary(Reason)};
        Error ->
            {500, <<"unknown_error">>, couch_util:to_binary(Error)}
    end.


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
    Headers = [{"Location", chttpd:absolute_uri(Req, Path)}],
    send_response(Req, 301, Headers, <<>>).

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

basic_headers(Req, Headers0) ->
    Headers = Headers0
        ++ server_header()
        ++ couch_httpd_auth:cookie_auth_header(Req, Headers0),
    chttpd_cors:headers(Req, Headers).

handle_response(Req0, Code0, Headers0, Args0, Type) ->
    {ok, {Req1, Code1, Headers1, Args1}} =
        chttpd_plugin:before_response(Req0, Code0, Headers0, Args0),
    couch_stats:increment_counter([couchdb, httpd_status_codes, Code1]),
    respond_(Req1, Code1, Headers1, Args1, Type).

respond_(#httpd{mochi_req = MochiReq}, Code, Headers, _Args, start_response) ->
    MochiReq:start_response({Code, Headers});
respond_(#httpd{mochi_req = MochiReq}, Code, Headers, Args, Type) ->
    MochiReq:Type({Code, Headers, Args}).

get_user(#httpd{user_ctx = #user_ctx{name = null}}) ->
    % admin party
    "undefined";
get_user(#httpd{user_ctx = #user_ctx{name = User}}) ->
    couch_util:url_encode(User);
get_user(#httpd{user_ctx = undefined}) ->
    "undefined".

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

check_url_encoding_pass_test_() ->
    [
        ?_assertEqual(ok, check_url_encoding("/dbname")),
        ?_assertEqual(ok, check_url_encoding("/dbname/doc_id")),
        ?_assertEqual(ok, check_url_encoding("/dbname/doc_id?rev=1-abcdefgh")),
        ?_assertEqual(ok, check_url_encoding("/dbname%25")),
        ?_assertEqual(ok, check_url_encoding("/dbname/doc_id%25")),
        ?_assertEqual(ok, check_url_encoding("/dbname%25%3a")),
        ?_assertEqual(ok, check_url_encoding("/dbname/doc_id%25%3a")),
        ?_assertEqual(ok, check_url_encoding("/user%2Fdbname")),
        ?_assertEqual(ok, check_url_encoding("/user%2Fdbname/doc_id")),
        ?_assertEqual(ok, check_url_encoding("/dbname/escaped%25doc_id")),
        ?_assertEqual(ok, check_url_encoding("/dbname/doc%2eid")),
        ?_assertEqual(ok, check_url_encoding("/dbname/doc%2Eid")),
        ?_assertEqual(ok, check_url_encoding("/dbname-with-dash")),
        ?_assertEqual(ok, check_url_encoding("/dbname/doc_id-with-dash"))
    ].

check_url_encoding_fail_test_() ->
    [
        ?_assertThrow({bad_request, invalid_url_encoding},
            check_url_encoding("/dbname%")),
        ?_assertThrow({bad_request, invalid_url_encoding},
            check_url_encoding("/dbname/doc_id%")),
        ?_assertThrow({bad_request, invalid_url_encoding},
            check_url_encoding("/dbname/doc_id%?rev=1-abcdefgh")),
        ?_assertThrow({bad_request, invalid_url_encoding},
            check_url_encoding("/dbname%2")),
        ?_assertThrow({bad_request, invalid_url_encoding},
            check_url_encoding("/dbname/doc_id%2")),
        ?_assertThrow({bad_request, invalid_url_encoding},
            check_url_encoding("/user%2Fdbname%")),
        ?_assertThrow({bad_request, invalid_url_encoding},
            check_url_encoding("/user%2Fdbname/doc_id%")),
        ?_assertThrow({bad_request, invalid_url_encoding},
            check_url_encoding("%")),
        ?_assertThrow({bad_request, invalid_url_encoding},
            check_url_encoding("/%")),
        ?_assertThrow({bad_request, invalid_url_encoding},
            check_url_encoding("/%2")),
        ?_assertThrow({bad_request, invalid_url_encoding},
            check_url_encoding("/dbname%2%3A")),
        ?_assertThrow({bad_request, invalid_url_encoding},
            check_url_encoding("/dbname%%3Ae")),
        ?_assertThrow({bad_request, invalid_url_encoding},
            check_url_encoding("/dbname%2g")),
        ?_assertThrow({bad_request, invalid_url_encoding},
            check_url_encoding("/dbname%g2"))
    ].

log_format_test() ->
    ?assertEqual(
        "127.0.0.1:15984 127.0.0.1 undefined "
        "GET /_cluster_setup 201 ok 10000",
        test_log_request("/_cluster_setup", undefined)),
    ?assertEqual(
        "127.0.0.1:15984 127.0.0.1 user_foo "
        "GET /_all_dbs 201 ok 10000",
        test_log_request("/_all_dbs", #user_ctx{name = <<"user_foo">>})),

    %% Utf8Name = unicode:characters_to_binary(Something),
    Utf8User = <<227,130,136,227,129,134,227,129,147,227,129,157>>,
    ?assertEqual(
        "127.0.0.1:15984 127.0.0.1 %E3%82%88%E3%81%86%E3%81%93%E3%81%9D "
        "GET /_all_dbs 201 ok 10000",
        test_log_request("/_all_dbs", #user_ctx{name = Utf8User})),
    ok.

test_log_request(RawPath, UserCtx) ->
    Headers = mochiweb_headers:make([{"HOST", "127.0.0.1:15984"}]),
    MochiReq = mochiweb_request:new(socket, [], 'POST', RawPath, version, Headers),
    Req = #httpd{
        mochi_req = MochiReq,
        begin_ts = {1458,588713,124003},
        original_method = 'GET',
        peer = "127.0.0.1",
        nonce = "nonce",
        user_ctx = UserCtx
    },
    Resp = #httpd_resp{
        end_ts = {1458,588723,124303},
        code = 201,
        status = ok
    },
    ok = meck:new(couch_log, [passthrough]),
    ok = meck:expect(couch_log, notice, fun(Format, Args) ->
        lists:flatten(io_lib:format(Format, Args))
    end),
    Message = maybe_log(Req, Resp),
    ok = meck:unload(couch_log),
    Message.

-endif.
