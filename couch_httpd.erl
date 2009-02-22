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

-export([header_value/2,header_value/3,qs_value/2,qs_value/3,qs/1,path/1,absolute_uri/2]).
-export([verify_is_server_admin/1,unquote/1,quote/1,recv/2,recv_chunked/4]).
-export([parse_form/1,json_body/1,body/1,doc_etag/1, make_etag/1, etag_respond/3]).
-export([primary_header_value/2,partition/1,serve_file/3]).
-export([start_chunked_response/3,send_chunk/2]).
-export([start_json_response/2, start_json_response/3, end_json_response/1]).
-export([send_response/4,send_method_not_allowed/2,send_error/4, send_redirect/2]).
-export([send_json/2,send_json/3,send_json/4]).
-export([default_authentication_handler/1,special_test_authentication_handler/1]).


start_link() ->
    % read config and register for configuration changes

    % just stop if one of the config settings change. couch_server_sup
    % will restart us and then we will pick up the new settings.

    BindAddress = couch_config:get("httpd", "bind_address", any),
    Port = couch_config:get("httpd", "port", "5984"),
    
    UrlHandlersList = lists:map(
        fun({UrlKey, SpecStr}) ->
            {?l2b(UrlKey), make_arity_1_fun(SpecStr)}
        end, couch_config:get("httpd_global_handlers")),
        
    DbUrlHandlersList = lists:map(
        fun({UrlKey, SpecStr}) ->
            {?l2b(UrlKey), make_arity_2_fun(SpecStr)}
        end, couch_config:get("httpd_db_handlers")),
    UrlHandlers = dict:from_list(UrlHandlersList),
    DbUrlHandlers = dict:from_list(DbUrlHandlersList),
    Loop = fun(Req)->
            apply(?MODULE, handle_request,
                    [Req, UrlHandlers, DbUrlHandlers])
        end,

    % and off we go
    
    {ok, Pid} = case mochiweb_http:start([
        {loop, Loop},
        {name, ?MODULE},
        {ip, BindAddress},
        {port, Port}
    ]) of
    {ok, MochiPid} -> {ok, MochiPid};
    {error, Reason} ->
        io:format("Failure to start Mochiweb: ~s~n",[Reason]),
        throw({error, Reason})
    end,

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

% SpecStr is a string like "{my_module, my_fun}" 
%  or "{my_module, my_fun, <<"my_arg">>}"
make_arity_1_fun(SpecStr) ->
    case couch_util:parse_term(SpecStr) of
    {ok, {Mod, Fun, SpecArg}} ->
        fun(Arg) -> apply(Mod, Fun, [Arg, SpecArg]) end;
    {ok, {Mod, Fun}} ->
        fun(Arg) -> apply(Mod, Fun, [Arg]) end
    end.

make_arity_2_fun(SpecStr) ->
    case couch_util:parse_term(SpecStr) of
    {ok, {Mod, Fun, SpecArg}} ->
        fun(Arg1, Arg2) -> apply(Mod, Fun, [Arg1, Arg2, SpecArg]) end;
    {ok, {Mod, Fun}} ->
        fun(Arg1, Arg2) -> apply(Mod, Fun, [Arg1, Arg2]) end
    end.
    

stop() ->
    mochiweb_http:stop(?MODULE).
    

handle_request(MochiReq, UrlHandlers, DbUrlHandlers) ->
    statistics(runtime), % prepare request_time counter, see end of function
    AuthenticationFun = make_arity_1_fun(
            couch_config:get("httpd", "authentication_handler")),
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
        path_parts = [list_to_binary(couch_httpd:unquote(Part))
                || Part <- string:tokens(Path, "/")],
        db_url_handlers = DbUrlHandlers
        },
    DefaultFun = fun couch_httpd_db:handle_request/1,
    HandlerFun = couch_util:dict_find(HandlerKey, UrlHandlers, DefaultFun),
    
    {ok, Resp} =
    try
        HandlerFun(HttpReq#httpd{user_ctx=AuthenticationFun(HttpReq)})
    catch
        throw:Error ->
            send_error(HttpReq, Error);
        Tag:Error ->
            ?LOG_ERROR("Uncaught error in HTTP request: ~p",[{Tag, Error}]),
            ?LOG_DEBUG("Stacktrace: ~p",[erlang:get_stacktrace()]),
            send_error(HttpReq, Error)
    end,

    ?LOG_INFO("~s - - ~p ~s ~B", [
        MochiReq:get(peer),
        MochiReq:get(method),
        RawUri,
        Resp:get(code)
    ]),
    {_TotalRuntime, RequestTime} = statistics(runtime),
    couch_stats_collector:record({couchdb, request_time}, RequestTime),
    couch_stats_collector:increment({httpd, requests}),
    {ok, Resp}.

increment_method_stats(Method) ->
    CounterName = list_to_atom(string:to_lower(atom_to_list(Method)) ++ "_requests"),
    couch_stats_collector:increment({httpd, CounterName}).

special_test_authentication_handler(Req) ->
    case header_value(Req, "WWW-Authenticate") of
    "X-Couch-Test-Auth " ++ NamePass ->
        % NamePass is a colon separated string: "joe schmoe:a password".
        {ok, [Name, Pass]} = regexp:split(NamePass, ":"),
        case {Name, Pass} of
        {"Jan Lehnardt", "apple"} -> ok;
        {"Christopher Lenz", "dog food"} -> ok;
        {"Noah Slater", "biggiesmalls endian"} -> ok;
        {"Chris Anderson", "mp3"} -> ok;
        {"Damien Katz", "pecan pie"} -> ok;
        {_, _} ->
            throw({unauthorized, <<"Name or password is incorrect.">>})
        end,
        #user_ctx{name=?l2b(Name)};
    _ ->
        % No X-Couch-Test-Auth credentials sent, give admin access so the
        % previous authentication can be restored after the test
        #user_ctx{roles=[<<"_admin">>]}
    end.

default_authentication_handler(Req) ->
    case basic_username_pw(Req) of
    {User, Pass} ->
        case couch_server:is_admin(User, Pass) of
        true ->
            #user_ctx{name=?l2b(User), roles=[<<"_admin">>]};
        false ->
            throw({unauthorized, <<"Name or password is incorrect.">>})
        end;
    nil ->
        case couch_server:has_admins() of
        true ->
            #user_ctx{};
        false ->
            % if no admins, then everyone is admin! Yay, admin party!
            #user_ctx{roles=[<<"_admin">>]}
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

absolute_uri(#httpd{mochi_req=MochiReq}, Path) ->
    Host = case MochiReq:get_header_value("Host") of
        undefined ->
            {ok, {Address, Port}} = inet:sockname(MochiReq:get(socket)),
            inet_parse:ntoa(Address) ++ ":" ++ integer_to_list(Port);
        Value -> Value
    end,
    "http://" ++ Host ++ Path.

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

body(#httpd{mochi_req=MochiReq}) ->
    % Maximum size of document PUT request body (4GB)
    MaxSize = list_to_integer(
        couch_config:get("couchdb", "max_document_size", "4294967296")),
    MochiReq:recv_body(MaxSize).

json_body(Httpd) ->
    ?JSON_DECODE(body(Httpd)).

doc_etag(#doc{revs=[DiskRev|_]}) ->
    "\"" ++ binary_to_list(DiskRev) ++ "\"".

make_etag(Term) ->
    <<SigInt:128/integer>> = erlang:md5(term_to_binary(Term)),
    list_to_binary("\"" ++ lists:flatten(io_lib:format("~.36B",[SigInt])) ++ "\"").

etag_match(Req, CurrentEtag) when is_binary(CurrentEtag) ->
    etag_match(Req, binary_to_list(CurrentEtag));

etag_match(Req, CurrentEtag) ->
    EtagsToMatch = string:tokens(
        couch_httpd:header_value(Req, "If-None-Match", ""), ", "),
    lists:member(CurrentEtag, EtagsToMatch).

etag_respond(Req, CurrentEtag, RespFun) ->
    case etag_match(Req, CurrentEtag) of
    true ->
        % the client has this in their cache.
        couch_httpd:send_response(Req, 304, [{"Etag", CurrentEtag}], <<>>);
    false ->
        % Run the function.
        RespFun()
    end.

verify_is_server_admin(#httpd{user_ctx=#user_ctx{roles=Roles}}) ->
    case lists:member(<<"_admin">>, Roles) of
    true -> ok;
    false -> throw({unauthorized, <<"You are not a server admin.">>})
    end.



basic_username_pw(Req) ->
    case header_value(Req, "Authorization") of
    "Basic " ++ Base64Value ->
        case string:tokens(?b2l(couch_util:decodeBase64(Base64Value)),":") of
        [User, Pass] ->
            {User, Pass};
        [User] ->
            {User, ""};
        _ ->
            nil
        end;
    _ ->
        nil
    end.


start_chunked_response(#httpd{mochi_req=MochiReq}, Code, Headers) ->
    couch_stats_collector:increment({http_status_codes, Code}),
    {ok, MochiReq:respond({Code, Headers ++ server_header(), chunked})}.

send_chunk(Resp, Data) ->
    Resp:write_chunk(Data),
    {ok, Resp}.

send_response(#httpd{mochi_req=MochiReq}, Code, Headers, Body) ->
    couch_stats_collector:increment({http_status_codes, Code}),
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
    send_error(Req, 409, <<"conflict">>, <<"Document update conflict.">>);
send_error(Req, {invalid_doc, Reason}) ->
    send_error(Req, 400, <<"invalid_doc">>, Reason);
send_error(Req, {forbidden, Msg}) ->
    send_json(Req, 403,
        {[{<<"error">>,  <<"forbidden">>},
         {<<"reason">>, Msg}]});
send_error(Req, {unauthorized, Msg}) ->
    case couch_config:get("httpd", "WWW-Authenticate", nil) of
    nil ->
        Headers = [];
    Type ->
        Headers = [{"WWW-Authenticate", Type}]
    end,
    send_json(Req, 401, Headers,
        {[{<<"error">>,  <<"unauthorized">>},
         {<<"reason">>, Msg}]});
send_error(Req, {http_error, Code, Headers, Error, Reason}) ->
    send_json(Req, Code, Headers,
        {[{<<"error">>, Error}, {<<"reason">>, Reason}]});
send_error(Req, {user_error, {Props}}) ->
    {Headers} = proplists:get_value(<<"headers">>, Props, {[]}),
    send_json(Req,
        proplists:get_value(<<"http_status">>, Props, 500),
        Headers,
        {[{<<"error">>, proplists:get_value(<<"error">>, Props)},
            {<<"reason">>, proplists:get_value(<<"reason">>, Props)}]});
send_error(Req, file_exists) ->
    send_error(Req, 412, <<"file_exists">>, <<"The database could not be "
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
    send_json(Req, Code, {[{<<"error">>, Error}]});
send_error(Req, Code, Error, Msg) ->
    send_json(Req, Code, {[{<<"error">>, Error}, {<<"reason">>, Msg}]}).
    
send_redirect(Req, Path) ->
    Headers = [{"Location", couch_httpd:absolute_uri(Req, Path)}],
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
    OTPVersion = "R" ++ integer_to_list(erlang:system_info(compat_rel)) ++ "B",
    [{"Server", "CouchDB/" ++ couch_server:get_version() ++
                " (Erlang OTP/" ++ OTPVersion ++ ")"}].
