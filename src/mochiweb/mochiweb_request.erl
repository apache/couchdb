%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc MochiWeb HTTP Request abstraction.

-module(mochiweb_request, [Socket, Method, RawPath, Version, Headers]).
-author('bob@mochimedia.com').

-include_lib("kernel/include/file.hrl").
-include("internal.hrl").

-define(QUIP, "Any of you quaids got a smint?").

-export([get_header_value/1, get_primary_header_value/1, get/1, dump/0]).
-export([send/1, recv/1, recv/2, recv_body/0, recv_body/1, stream_body/3]).
-export([start_response/1, start_response_length/1, start_raw_response/1]).
-export([respond/1, ok/1]).
-export([not_found/0, not_found/1]).
-export([parse_post/0, parse_qs/0]).
-export([should_close/0, cleanup/0]).
-export([parse_cookie/0, get_cookie_value/1]).
-export([serve_file/2, serve_file/3]).
-export([accepted_encodings/1]).
-export([accepts_content_type/1]).

-define(SAVE_QS, mochiweb_request_qs).
-define(SAVE_PATH, mochiweb_request_path).
-define(SAVE_RECV, mochiweb_request_recv).
-define(SAVE_BODY, mochiweb_request_body).
-define(SAVE_BODY_LENGTH, mochiweb_request_body_length).
-define(SAVE_POST, mochiweb_request_post).
-define(SAVE_COOKIE, mochiweb_request_cookie).
-define(SAVE_FORCE_CLOSE, mochiweb_request_force_close).

%% @type iolist() = [iolist() | binary() | char()].
%% @type iodata() = binary() | iolist().
%% @type key() = atom() | string() | binary()
%% @type value() = atom() | string() | binary() | integer()
%% @type headers(). A mochiweb_headers structure.
%% @type response(). A mochiweb_response parameterized module instance.
%% @type ioheaders() = headers() | [{key(), value()}].

% 5 minute default idle timeout
-define(IDLE_TIMEOUT, 300000).

% Maximum recv_body() length of 1MB
-define(MAX_RECV_BODY, (1024*1024)).

%% @spec get_header_value(K) -> undefined | Value
%% @doc Get the value of a given request header.
get_header_value(K) ->
    mochiweb_headers:get_value(K, Headers).

get_primary_header_value(K) ->
    mochiweb_headers:get_primary_value(K, Headers).

%% @type field() = socket | scheme | method | raw_path | version | headers | peer | path | body_length | range

%% @spec get(field()) -> term()
%% @doc Return the internal representation of the given field. If
%%      <code>socket</code> is requested on a HTTPS connection, then
%%      an ssl socket will be returned as <code>{ssl, SslSocket}</code>.
%%      You can use <code>SslSocket</code> with the <code>ssl</code>
%%      application, eg: <code>ssl:peercert(SslSocket)</code>.
get(socket) ->
    Socket;
get(scheme) ->
    case mochiweb_socket:type(Socket) of
        plain ->
            http;
        ssl ->
            https
    end;
get(method) ->
    Method;
get(raw_path) ->
    RawPath;
get(version) ->
    Version;
get(headers) ->
    Headers;
get(peer) ->
    case mochiweb_socket:peername(Socket) of
        {ok, {Addr={10, _, _, _}, _Port}} ->
            case get_header_value("x-forwarded-for") of
                undefined ->
                    inet_parse:ntoa(Addr);
                Hosts ->
                    string:strip(lists:last(string:tokens(Hosts, ",")))
            end;
        {ok, {{127, 0, 0, 1}, _Port}} ->
            case get_header_value("x-forwarded-for") of
                undefined ->
                    "127.0.0.1";
                Hosts ->
                    string:strip(lists:last(string:tokens(Hosts, ",")))
            end;
        {ok, {Addr, _Port}} ->
            inet_parse:ntoa(Addr);
        {error, enotconn} ->
            exit(normal)
    end;
get(path) ->
    case erlang:get(?SAVE_PATH) of
        undefined ->
            {Path0, _, _} = mochiweb_util:urlsplit_path(RawPath),
            Path = mochiweb_util:unquote(Path0),
            put(?SAVE_PATH, Path),
            Path;
        Cached ->
            Cached
    end;
get(body_length) ->
    case erlang:get(?SAVE_BODY_LENGTH) of
        undefined ->
            BodyLength = body_length(),
            put(?SAVE_BODY_LENGTH, {cached, BodyLength}),
            BodyLength;
        {cached, Cached} ->
            Cached
    end;
get(range) ->
    case get_header_value(range) of
        undefined ->
            undefined;
        RawRange ->
            mochiweb_http:parse_range_request(RawRange)
    end.

%% @spec dump() -> {mochiweb_request, [{atom(), term()}]}
%% @doc Dump the internal representation to a "human readable" set of terms
%%      for debugging/inspection purposes.
dump() ->
    {?MODULE, [{method, Method},
               {version, Version},
               {raw_path, RawPath},
               {headers, mochiweb_headers:to_list(Headers)}]}.

%% @spec send(iodata()) -> ok
%% @doc Send data over the socket.
send(Data) ->
    case mochiweb_socket:send(Socket, Data) of
        ok ->
            ok;
        _ ->
            exit(normal)
    end.

%% @spec recv(integer()) -> binary()
%% @doc Receive Length bytes from the client as a binary, with the default
%%      idle timeout.
recv(Length) ->
    recv(Length, ?IDLE_TIMEOUT).

%% @spec recv(integer(), integer()) -> binary()
%% @doc Receive Length bytes from the client as a binary, with the given
%%      Timeout in msec.
recv(Length, Timeout) ->
    case mochiweb_socket:recv(Socket, Length, Timeout) of
        {ok, Data} ->
            put(?SAVE_RECV, true),
            Data;
        _ ->
            exit(normal)
    end.

%% @spec body_length() -> undefined | chunked | unknown_transfer_encoding | integer()
%% @doc  Infer body length from transfer-encoding and content-length headers.
body_length() ->
    case get_header_value("transfer-encoding") of
        undefined ->
            case get_header_value("content-length") of
                undefined ->
                    undefined;
                Length ->
                    list_to_integer(Length)
            end;
        "chunked" ->
            chunked;
        Unknown ->
            {unknown_transfer_encoding, Unknown}
    end.


%% @spec recv_body() -> binary()
%% @doc Receive the body of the HTTP request (defined by Content-Length).
%%      Will only receive up to the default max-body length of 1MB.
recv_body() ->
    recv_body(?MAX_RECV_BODY).

%% @spec recv_body(integer()) -> binary()
%% @doc Receive the body of the HTTP request (defined by Content-Length).
%%      Will receive up to MaxBody bytes.
recv_body(MaxBody) ->
    case erlang:get(?SAVE_BODY) of
        undefined ->
            % we could use a sane constant for max chunk size
            Body = stream_body(?MAX_RECV_BODY, fun
                ({0, _ChunkedFooter}, {_LengthAcc, BinAcc}) ->
                    iolist_to_binary(lists:reverse(BinAcc));
                ({Length, Bin}, {LengthAcc, BinAcc}) ->
                    NewLength = Length + LengthAcc,
                    if NewLength > MaxBody ->
                        exit({body_too_large, chunked});
                    true ->
                        {NewLength, [Bin | BinAcc]}
                    end
                end, {0, []}, MaxBody),
            put(?SAVE_BODY, Body),
            Body;
        Cached -> Cached
    end.

stream_body(MaxChunkSize, ChunkFun, FunState) ->
    stream_body(MaxChunkSize, ChunkFun, FunState, undefined).

stream_body(MaxChunkSize, ChunkFun, FunState, MaxBodyLength) ->
    Expect = case get_header_value("expect") of
                 undefined ->
                     undefined;
                 Value when is_list(Value) ->
                     string:to_lower(Value)
             end,
    case Expect of
        "100-continue" ->
            start_raw_response({100, gb_trees:empty()});
        _Else ->
            ok
    end,
    case body_length() of
        undefined ->
            undefined;
        {unknown_transfer_encoding, Unknown} ->
            exit({unknown_transfer_encoding, Unknown});
        chunked ->
            % In this case the MaxBody is actually used to
            % determine the maximum allowed size of a single
            % chunk.
            stream_chunked_body(MaxChunkSize, ChunkFun, FunState);
        0 ->
            <<>>;
        Length when is_integer(Length) ->
            case MaxBodyLength of
            MaxBodyLength when is_integer(MaxBodyLength), MaxBodyLength < Length ->
                exit({body_too_large, content_length});
            _ ->
                stream_unchunked_body(Length, ChunkFun, FunState)
            end;
        Length ->
            exit({length_not_integer, Length})
    end.


%% @spec start_response({integer(), ioheaders()}) -> response()
%% @doc Start the HTTP response by sending the Code HTTP response and
%%      ResponseHeaders. The server will set header defaults such as Server
%%      and Date if not present in ResponseHeaders.
start_response({Code, ResponseHeaders}) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = mochiweb_headers:default_from_list(server_headers(),
                                                    HResponse),
    start_raw_response({Code, HResponse1}).

%% @spec start_raw_response({integer(), headers()}) -> response()
%% @doc Start the HTTP response by sending the Code HTTP response and
%%      ResponseHeaders.
start_raw_response({Code, ResponseHeaders}) ->
    F = fun ({K, V}, Acc) ->
                [mochiweb_util:make_io(K), <<": ">>, V, <<"\r\n">> | Acc]
        end,
    End = lists:foldl(F, [<<"\r\n">>],
                      mochiweb_headers:to_list(ResponseHeaders)),
    send([make_version(Version), make_code(Code), <<"\r\n">> | End]),
    mochiweb:new_response({THIS, Code, ResponseHeaders}).


%% @spec start_response_length({integer(), ioheaders(), integer()}) -> response()
%% @doc Start the HTTP response by sending the Code HTTP response and
%%      ResponseHeaders including a Content-Length of Length. The server
%%      will set header defaults such as Server
%%      and Date if not present in ResponseHeaders.
start_response_length({Code, ResponseHeaders, Length}) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = mochiweb_headers:enter("Content-Length", Length, HResponse),
    start_response({Code, HResponse1}).

%% @spec respond({integer(), ioheaders(), iodata() | chunked | {file, IoDevice}}) -> response()
%% @doc Start the HTTP response with start_response, and send Body to the
%%      client (if the get(method) /= 'HEAD'). The Content-Length header
%%      will be set by the Body length, and the server will insert header
%%      defaults.
respond({Code, ResponseHeaders, {file, IoDevice}}) ->
    Length = mochiweb_io:iodevice_size(IoDevice),
    Response = start_response_length({Code, ResponseHeaders, Length}),
    case Method of
        'HEAD' ->
            ok;
        _ ->
            mochiweb_io:iodevice_stream(fun send/1, IoDevice)
    end,
    Response;
respond({Code, ResponseHeaders, chunked}) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    HResponse1 = case Method of
                     'HEAD' ->
                         %% This is what Google does, http://www.google.com/
                         %% is chunked but HEAD gets Content-Length: 0.
                         %% The RFC is ambiguous so emulating Google is smart.
                         mochiweb_headers:enter("Content-Length", "0",
                                                HResponse);
                     _ when Version >= {1, 1} ->
                         %% Only use chunked encoding for HTTP/1.1
                         mochiweb_headers:enter("Transfer-Encoding", "chunked",
                                                HResponse);
                     _ ->
                         %% For pre-1.1 clients we send the data as-is
                         %% without a Content-Length header and without
                         %% chunk delimiters. Since the end of the document
                         %% is now ambiguous we must force a close.
                         put(?SAVE_FORCE_CLOSE, true),
                         HResponse
                 end,
    start_response({Code, HResponse1});
respond({Code, ResponseHeaders, Body}) ->
    Response = start_response_length({Code, ResponseHeaders, iolist_size(Body)}),
    case Method of
        'HEAD' ->
            ok;
        _ ->
            send(Body)
    end,
    Response.

%% @spec not_found() -> response()
%% @doc Alias for <code>not_found([])</code>.
not_found() ->
    not_found([]).

%% @spec not_found(ExtraHeaders) -> response()
%% @doc Alias for <code>respond({404, [{"Content-Type", "text/plain"}
%% | ExtraHeaders], &lt;&lt;"Not found."&gt;&gt;})</code>.
not_found(ExtraHeaders) ->
    respond({404, [{"Content-Type", "text/plain"} | ExtraHeaders],
             <<"Not found.">>}).

%% @spec ok({value(), iodata()} | {value(), ioheaders(), iodata() | {file, IoDevice}}) ->
%%           response()
%% @doc respond({200, [{"Content-Type", ContentType} | Headers], Body}).
ok({ContentType, Body}) ->
    ok({ContentType, [], Body});
ok({ContentType, ResponseHeaders, Body}) ->
    HResponse = mochiweb_headers:make(ResponseHeaders),
    case THIS:get(range) of
        X when (X =:= undefined orelse X =:= fail) orelse Body =:= chunked ->
            %% http://code.google.com/p/mochiweb/issues/detail?id=54
            %% Range header not supported when chunked, return 200 and provide
            %% full response.
            HResponse1 = mochiweb_headers:enter("Content-Type", ContentType,
                                                HResponse),
            respond({200, HResponse1, Body});
        Ranges ->
            {PartList, Size} = range_parts(Body, Ranges),
            case PartList of
                [] -> %% no valid ranges
                    HResponse1 = mochiweb_headers:enter("Content-Type",
                                                        ContentType,
                                                        HResponse),
                    %% could be 416, for now we'll just return 200
                    respond({200, HResponse1, Body});
                PartList ->
                    {RangeHeaders, RangeBody} =
                        mochiweb_multipart:parts_to_body(PartList, ContentType, Size),
                    HResponse1 = mochiweb_headers:enter_from_list(
                                   [{"Accept-Ranges", "bytes"} |
                                    RangeHeaders],
                                   HResponse),
                    respond({206, HResponse1, RangeBody})
            end
    end.

%% @spec should_close() -> bool()
%% @doc Return true if the connection must be closed. If false, using
%%      Keep-Alive should be safe.
should_close() ->
    ForceClose = erlang:get(?SAVE_FORCE_CLOSE) =/= undefined,
    DidNotRecv = erlang:get(?SAVE_RECV) =:= undefined,
    ForceClose orelse Version < {1, 0}
        %% Connection: close
        orelse get_header_value("connection") =:= "close"
        %% HTTP 1.0 requires Connection: Keep-Alive
        orelse (Version =:= {1, 0}
                andalso get_header_value("connection") =/= "Keep-Alive")
        %% unread data left on the socket, can't safely continue
        orelse (DidNotRecv
                andalso get_header_value("content-length") =/= undefined
                andalso list_to_integer(get_header_value("content-length")) > 0)
        orelse (DidNotRecv
                andalso get_header_value("transfer-encoding") =:= "chunked").

%% @spec cleanup() -> ok
%% @doc Clean up any junk in the process dictionary, required before continuing
%%      a Keep-Alive request.
cleanup() ->
    [erase(K) || K <- [?SAVE_QS,
                       ?SAVE_PATH,
                       ?SAVE_RECV,
                       ?SAVE_BODY,
                       ?SAVE_BODY_LENGTH,
                       ?SAVE_POST,
                       ?SAVE_COOKIE,
                       ?SAVE_FORCE_CLOSE]],
    ok.

%% @spec parse_qs() -> [{Key::string(), Value::string()}]
%% @doc Parse the query string of the URL.
parse_qs() ->
    case erlang:get(?SAVE_QS) of
        undefined ->
            {_, QueryString, _} = mochiweb_util:urlsplit_path(RawPath),
            Parsed = mochiweb_util:parse_qs(QueryString),
            put(?SAVE_QS, Parsed),
            Parsed;
        Cached ->
            Cached
    end.

%% @spec get_cookie_value(Key::string) -> string() | undefined
%% @doc Get the value of the given cookie.
get_cookie_value(Key) ->
    proplists:get_value(Key, parse_cookie()).

%% @spec parse_cookie() -> [{Key::string(), Value::string()}]
%% @doc Parse the cookie header.
parse_cookie() ->
    case erlang:get(?SAVE_COOKIE) of
        undefined ->
            Cookies = case get_header_value("cookie") of
                          undefined ->
                              [];
                          Value ->
                              mochiweb_cookies:parse_cookie(Value)
                      end,
            put(?SAVE_COOKIE, Cookies),
            Cookies;
        Cached ->
            Cached
    end.

%% @spec parse_post() -> [{Key::string(), Value::string()}]
%% @doc Parse an application/x-www-form-urlencoded form POST. This
%%      has the side-effect of calling recv_body().
parse_post() ->
    case erlang:get(?SAVE_POST) of
        undefined ->
            Parsed = case recv_body() of
                         undefined ->
                             [];
                         Binary ->
                             case get_primary_header_value("content-type") of
                                 "application/x-www-form-urlencoded" ++ _ ->
                                     mochiweb_util:parse_qs(Binary);
                                 _ ->
                                     []
                             end
                     end,
            put(?SAVE_POST, Parsed),
            Parsed;
        Cached ->
            Cached
    end.

%% @spec stream_chunked_body(integer(), fun(), term()) -> term()
%% @doc The function is called for each chunk.
%%      Used internally by read_chunked_body.
stream_chunked_body(MaxChunkSize, Fun, FunState) ->
    case read_chunk_length() of
        0 ->
            Fun({0, read_chunk(0)}, FunState);
        Length when Length > MaxChunkSize ->
            NewState = read_sub_chunks(Length, MaxChunkSize, Fun, FunState),
            stream_chunked_body(MaxChunkSize, Fun, NewState);
        Length ->
            NewState = Fun({Length, read_chunk(Length)}, FunState),
            stream_chunked_body(MaxChunkSize, Fun, NewState)
    end.

stream_unchunked_body(0, Fun, FunState) ->
    Fun({0, <<>>}, FunState);
stream_unchunked_body(Length, Fun, FunState) when Length > 0 ->
    PktSize = case Length > ?RECBUF_SIZE of
        true ->
            ?RECBUF_SIZE;
        false ->
            Length
    end,
    Bin = recv(PktSize),
    NewState = Fun({PktSize, Bin}, FunState),
    stream_unchunked_body(Length - PktSize, Fun, NewState).

%% @spec read_chunk_length() -> integer()
%% @doc Read the length of the next HTTP chunk.
read_chunk_length() ->
    mochiweb_socket:setopts(Socket, [{packet, line}]),
    case mochiweb_socket:recv(Socket, 0, ?IDLE_TIMEOUT) of
        {ok, Header} ->
            mochiweb_socket:setopts(Socket, [{packet, raw}]),
            Splitter = fun (C) ->
                               C =/= $\r andalso C =/= $\n andalso C =/= $
                       end,
            {Hex, _Rest} = lists:splitwith(Splitter, binary_to_list(Header)),
            mochihex:to_int(Hex);
        _ ->
            exit(normal)
    end.

%% @spec read_chunk(integer()) -> Chunk::binary() | [Footer::binary()]
%% @doc Read in a HTTP chunk of the given length. If Length is 0, then read the
%%      HTTP footers (as a list of binaries, since they're nominal).
read_chunk(0) ->
    mochiweb_socket:setopts(Socket, [{packet, line}]),
    F = fun (F1, Acc) ->
                case mochiweb_socket:recv(Socket, 0, ?IDLE_TIMEOUT) of
                    {ok, <<"\r\n">>} ->
                        Acc;
                    {ok, Footer} ->
                        F1(F1, [Footer | Acc]);
                    _ ->
                        exit(normal)
                end
        end,
    Footers = F(F, []),
    mochiweb_socket:setopts(Socket, [{packet, raw}]),
    put(?SAVE_RECV, true),
    Footers;
read_chunk(Length) ->
    case mochiweb_socket:recv(Socket, 2 + Length, ?IDLE_TIMEOUT) of
        {ok, <<Chunk:Length/binary, "\r\n">>} ->
            Chunk;
        _ ->
            exit(normal)
    end.

read_sub_chunks(Length, MaxChunkSize, Fun, FunState) when Length > MaxChunkSize ->
    Bin = recv(MaxChunkSize),
    NewState = Fun({size(Bin), Bin}, FunState),
    read_sub_chunks(Length - MaxChunkSize, MaxChunkSize, Fun, NewState);

read_sub_chunks(Length, _MaxChunkSize, Fun, FunState) ->
    Fun({Length, read_chunk(Length)}, FunState).

%% @spec serve_file(Path, DocRoot) -> Response
%% @doc Serve a file relative to DocRoot.
serve_file(Path, DocRoot) ->
    serve_file(Path, DocRoot, []).

%% @spec serve_file(Path, DocRoot, ExtraHeaders) -> Response
%% @doc Serve a file relative to DocRoot.
serve_file(Path, DocRoot, ExtraHeaders) ->
    case mochiweb_util:safe_relative_path(Path) of
        undefined ->
            not_found(ExtraHeaders);
        RelPath ->
            FullPath = filename:join([DocRoot, RelPath]),
            case filelib:is_dir(FullPath) of
                true ->
                    maybe_redirect(RelPath, FullPath, ExtraHeaders);
                false ->
                    maybe_serve_file(FullPath, ExtraHeaders)
            end
    end.

%% Internal API

%% This has the same effect as the DirectoryIndex directive in httpd
directory_index(FullPath) ->
    filename:join([FullPath, "index.html"]).

maybe_redirect([], FullPath, ExtraHeaders) ->
    maybe_serve_file(directory_index(FullPath), ExtraHeaders);

maybe_redirect(RelPath, FullPath, ExtraHeaders) ->
    case string:right(RelPath, 1) of
        "/" ->
            maybe_serve_file(directory_index(FullPath), ExtraHeaders);
        _   ->
            Host = mochiweb_headers:get_value("host", Headers),
            Location = "http://" ++ Host  ++ "/" ++ RelPath ++ "/",
            LocationBin = list_to_binary(Location),
            MoreHeaders = [{"Location", Location},
                           {"Content-Type", "text/html"} | ExtraHeaders],
            Top = <<"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">"
            "<html><head>"
            "<title>301 Moved Permanently</title>"
            "</head><body>"
            "<h1>Moved Permanently</h1>"
            "<p>The document has moved <a href=\"">>,
            Bottom = <<">here</a>.</p></body></html>\n">>,
            Body = <<Top/binary, LocationBin/binary, Bottom/binary>>,
            respond({301, MoreHeaders, Body})
    end.

maybe_serve_file(File, ExtraHeaders) ->
    case file:read_file_info(File) of
        {ok, FileInfo} ->
            LastModified = httpd_util:rfc1123_date(FileInfo#file_info.mtime),
            case get_header_value("if-modified-since") of
                LastModified ->
                    respond({304, ExtraHeaders, ""});
                _ ->
                    case file:open(File, [raw, binary]) of
                        {ok, IoDevice} ->
                            ContentType = mochiweb_util:guess_mime(File),
                            Res = ok({ContentType,
                                      [{"last-modified", LastModified}
                                       | ExtraHeaders],
                                      {file, IoDevice}}),
                            file:close(IoDevice),
                            Res;
                        _ ->
                            not_found(ExtraHeaders)
                    end
            end;
        {error, _} ->
            not_found(ExtraHeaders)
    end.

server_headers() ->
    [{"Server", "MochiWeb/1.0 (" ++ ?QUIP ++ ")"},
     {"Date", httpd_util:rfc1123_date()}].

make_code(X) when is_integer(X) ->
    [integer_to_list(X), [" " | httpd_util:reason_phrase(X)]];
make_code(Io) when is_list(Io); is_binary(Io) ->
    Io.

make_version({1, 0}) ->
    <<"HTTP/1.0 ">>;
make_version(_) ->
    <<"HTTP/1.1 ">>.

range_parts({file, IoDevice}, Ranges) ->
    Size = mochiweb_io:iodevice_size(IoDevice),
    F = fun (Spec, Acc) ->
                case mochiweb_http:range_skip_length(Spec, Size) of
                    invalid_range ->
                        Acc;
                    V ->
                        [V | Acc]
                end
        end,
    LocNums = lists:foldr(F, [], Ranges),
    {ok, Data} = file:pread(IoDevice, LocNums),
    Bodies = lists:zipwith(fun ({Skip, Length}, PartialBody) ->
                                   {Skip, Skip + Length - 1, PartialBody}
                           end,
                           LocNums, Data),
    {Bodies, Size};
range_parts(Body0, Ranges) ->
    Body = iolist_to_binary(Body0),
    Size = size(Body),
    F = fun(Spec, Acc) ->
                case mochiweb_http:range_skip_length(Spec, Size) of
                    invalid_range ->
                        Acc;
                    {Skip, Length} ->
                        <<_:Skip/binary, PartialBody:Length/binary, _/binary>> = Body,
                        [{Skip, Skip + Length - 1, PartialBody} | Acc]
                end
        end,
    {lists:foldr(F, [], Ranges), Size}.

%% @spec accepted_encodings([encoding()]) -> [encoding()] | bad_accept_encoding_value
%% @type encoding() = string().
%%
%% @doc Returns a list of encodings accepted by a request. Encodings that are
%%      not supported by the server will not be included in the return list.
%%      This list is computed from the "Accept-Encoding" header and
%%      its elements are ordered, descendingly, according to their Q values.
%%
%%      Section 14.3 of the RFC 2616 (HTTP 1.1) describes the "Accept-Encoding"
%%      header and the process of determining which server supported encodings
%%      can be used for encoding the body for the request's response.
%%
%%      Examples
%%
%%      1) For a missing "Accept-Encoding" header:
%%         accepted_encodings(["gzip", "identity"]) -> ["identity"]
%%
%%      2) For an "Accept-Encoding" header with value "gzip, deflate":
%%         accepted_encodings(["gzip", "identity"]) -> ["gzip", "identity"]
%%
%%      3) For an "Accept-Encoding" header with value "gzip;q=0.5, deflate":
%%         accepted_encodings(["gzip", "deflate", "identity"]) ->
%%            ["deflate", "gzip", "identity"]
%%
accepted_encodings(SupportedEncodings) ->
    AcceptEncodingHeader = case get_header_value("Accept-Encoding") of
        undefined ->
            "";
        Value ->
            Value
    end,
    case mochiweb_util:parse_qvalues(AcceptEncodingHeader) of
        invalid_qvalue_string ->
            bad_accept_encoding_value;
        QList ->
            mochiweb_util:pick_accepted_encodings(
                QList, SupportedEncodings, "identity"
            )
    end.

%% @spec accepts_content_type(string() | binary()) -> boolean() | bad_accept_header
%%
%% @doc Determines whether a request accepts a given media type by analyzing its
%%      "Accept" header.
%%
%%      Examples
%%
%%      1) For a missing "Accept" header:
%%         accepts_content_type("application/json") -> true
%%
%%      2) For an "Accept" header with value "text/plain, application/*":
%%         accepts_content_type("application/json") -> true
%%
%%      3) For an "Accept" header with value "text/plain, */*; q=0.0":
%%         accepts_content_type("application/json") -> false
%%
%%      4) For an "Accept" header with value "text/plain; q=0.5, */*; q=0.1":
%%         accepts_content_type("application/json") -> true
%%
%%      5) For an "Accept" header with value "text/*; q=0.0, */*":
%%         accepts_content_type("text/plain") -> false
%%
accepts_content_type(ContentType) when is_binary(ContentType) ->
    accepts_content_type(binary_to_list(ContentType));
accepts_content_type(ContentType1) ->
    ContentType = re:replace(ContentType1, "\\s", "", [global, {return, list}]),
    AcceptHeader = case get_header_value("Accept") of
        undefined ->
            "*/*";
        Value ->
            Value
    end,
    case mochiweb_util:parse_qvalues(AcceptHeader) of
        invalid_qvalue_string ->
            bad_accept_header;
        QList ->
            [MainType, _SubType] = string:tokens(ContentType, "/"),
            SuperType = MainType ++ "/*",
            lists:any(
                fun({"*/*", Q}) when Q > 0.0 ->
                        true;
                    ({Type, Q}) when Q > 0.0 ->
                        Type =:= ContentType orelse Type =:= SuperType;
                    (_) ->
                        false
                end,
                QList
            ) andalso
            (not lists:member({ContentType, 0.0}, QList)) andalso
            (not lists:member({SuperType, 0.0}, QList))
    end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
