%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Start and stop the MochiWeb server.

-module(mochiweb).
-author('bob@mochimedia.com').

-export([start/0, stop/0]).
-export([new_request/1, new_response/1]).
-export([all_loaded/0, all_loaded/1, reload/0]).

%% @spec start() -> ok
%% @doc Start the MochiWeb server.
start() ->
    ensure_started(crypto),
    application:start(mochiweb).

%% @spec stop() -> ok
%% @doc Stop the MochiWeb server.
stop() ->
    Res = application:stop(mochiweb),
    application:stop(crypto),
    Res.

reload() ->
    [c:l(Module) || Module <- all_loaded()].

all_loaded() ->
    all_loaded(filename:dirname(code:which(?MODULE))).

all_loaded(Base) when is_atom(Base) ->
    [];
all_loaded(Base) ->
    FullBase = Base ++ "/",
    F = fun ({_Module, Loaded}, Acc) when is_atom(Loaded) ->
                Acc;
            ({Module, Loaded}, Acc) ->
                case lists:prefix(FullBase, Loaded) of
                    true ->
                        [Module | Acc];
                    false ->
                        Acc
                end
        end,
    lists:foldl(F, [], code:all_loaded()).


%% @spec new_request({Socket, Request, Headers}) -> MochiWebRequest
%% @doc Return a mochiweb_request data structure.
new_request({Socket, {Method, {abs_path, Uri}, Version}, Headers}) ->
    mochiweb_request:new(Socket,
                         Method,
                         Uri,
                         Version,
                         mochiweb_headers:make(Headers));
% this case probably doesn't "exist".
new_request({Socket, {Method, {absoluteURI, _Protocol, _Host, _Port, Uri},
                      Version}, Headers}) ->
    mochiweb_request:new(Socket,
                         Method,
                         Uri,
                         Version,
                         mochiweb_headers:make(Headers));
%% Request-URI is "*"
%% From http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1.2
new_request({Socket, {Method, '*'=Uri, Version}, Headers}) ->
    mochiweb_request:new(Socket,
                         Method,
                         Uri,
                         Version,
                         mochiweb_headers:make(Headers)).

%% @spec new_response({Request, integer(), Headers}) -> MochiWebResponse
%% @doc Return a mochiweb_response data structure.
new_response({Request, Code, Headers}) ->
    mochiweb_response:new(Request,
                          Code,
                          mochiweb_headers:make(Headers)).

%% Internal API

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

-record(treq, {path, body= <<>>, xreply= <<>>}).

ssl_cert_opts() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    CertDir = filename:join([EbinDir, "..", "support", "test-materials"]),
    CertFile = filename:join(CertDir, "test_ssl_cert.pem"),
    KeyFile = filename:join(CertDir, "test_ssl_key.pem"),
    [{certfile, CertFile}, {keyfile, KeyFile}].

with_server(Transport, ServerFun, ClientFun) ->
    ServerOpts0 = [{ip, "127.0.0.1"}, {port, 0}, {loop, ServerFun}],
    ServerOpts = case Transport of
        plain ->
            ServerOpts0;
        ssl ->
            ServerOpts0 ++ [{ssl, true}, {ssl_opts, ssl_cert_opts()}]
    end,
    {ok, Server} = mochiweb_http:start(ServerOpts),
    Port = mochiweb_socket_server:get(Server, port),
    Res = (catch ClientFun(Transport, Port)),
    mochiweb_http:stop(Server),
    Res.

request_test() ->
    R = mochiweb_request:new(z, z, "/foo/bar/baz%20wibble+quux?qs=2", z, []),
    "/foo/bar/baz wibble quux" = R:get(path),
    ok.

single_http_GET_test() ->
    do_GET(plain, 1).

single_https_GET_test() ->
    do_GET(ssl, 1).

multiple_http_GET_test() ->
    do_GET(plain, 3).

multiple_https_GET_test() ->
    do_GET(ssl, 3).

hundred_http_GET_test() ->
    do_GET(plain, 100).

hundred_https_GET_test() ->
    do_GET(ssl, 100).

single_128_http_POST_test() ->
    do_POST(plain, 128, 1).

single_128_https_POST_test() ->
    do_POST(ssl, 128, 1).

single_2k_http_POST_test() ->
    do_POST(plain, 2048, 1).

single_2k_https_POST_test() ->
    do_POST(ssl, 2048, 1).

single_100k_http_POST_test() ->
    do_POST(plain, 102400, 1).

single_100k_https_POST_test() ->
    do_POST(ssl, 102400, 1).

multiple_100k_http_POST_test() ->
    do_POST(plain, 102400, 3).

multiple_100K_https_POST_test() ->
    do_POST(ssl, 102400, 3).

hundred_128_http_POST_test() ->
    do_POST(plain, 128, 100).

hundred_128_https_POST_test() ->
    do_POST(ssl, 128, 100).

do_GET(Transport, Times) ->
    PathPrefix = "/whatever/",
    ReplyPrefix = "You requested: ",
    ServerFun = fun (Req) ->
                        Reply = ReplyPrefix ++ Req:get(path),
                        Req:ok({"text/plain", Reply})
                end,
    TestReqs = [begin
                    Path = PathPrefix ++ integer_to_list(N),
                    ExpectedReply = list_to_binary(ReplyPrefix ++ Path),
                    #treq{path=Path, xreply=ExpectedReply}
                end || N <- lists:seq(1, Times)],
    ClientFun = new_client_fun('GET', TestReqs),
    ok = with_server(Transport, ServerFun, ClientFun),
    ok.

do_POST(Transport, Size, Times) ->
    ServerFun = fun (Req) ->
                        Body = Req:recv_body(),
                        Headers = [{"Content-Type", "application/octet-stream"}],
                        Req:respond({201, Headers, Body})
                end,
    TestReqs = [begin
                    Path = "/stuff/" ++ integer_to_list(N),
                    Body = crypto:rand_bytes(Size),
                    #treq{path=Path, body=Body, xreply=Body}
                end || N <- lists:seq(1, Times)],
    ClientFun = new_client_fun('POST', TestReqs),
    ok = with_server(Transport, ServerFun, ClientFun),
    ok.

new_client_fun(Method, TestReqs) ->
    fun (Transport, Port) ->
            client_request(Transport, Port, Method, TestReqs)
    end.

client_request(Transport, Port, Method, TestReqs) ->
    Opts = [binary, {active, false}, {packet, http}],
    SockFun = case Transport of
        plain ->
            {ok, Socket} = gen_tcp:connect("127.0.0.1", Port, Opts),
            fun (recv) ->
                    gen_tcp:recv(Socket, 0);
                ({recv, Length}) ->
                    gen_tcp:recv(Socket, Length);
                ({send, Data}) ->
                    gen_tcp:send(Socket, Data);
                ({setopts, L}) ->
                    inet:setopts(Socket, L)
            end;
        ssl ->
            {ok, Socket} = ssl:connect("127.0.0.1", Port, [{ssl_imp, new} | Opts]),
            fun (recv) ->
                    ssl:recv(Socket, 0);
                ({recv, Length}) ->
                    ssl:recv(Socket, Length);
                ({send, Data}) ->
                    ssl:send(Socket, Data);
                ({setopts, L}) ->
                    ssl:setopts(Socket, L)
            end
    end,
    client_request(SockFun, Method, TestReqs).

client_request(SockFun, _Method, []) ->
    {the_end, {error, closed}} = {the_end, SockFun(recv)},
    ok;
client_request(SockFun, Method,
               [#treq{path=Path, body=Body, xreply=ExReply} | Rest]) ->
    Request = [atom_to_list(Method), " ", Path, " HTTP/1.1\r\n",
               client_headers(Body, Rest =:= []),
               "\r\n",
               Body],
    ok = SockFun({send, Request}),
    case Method of
        'GET' ->
            {ok, {http_response, {1,1}, 200, "OK"}} = SockFun(recv);
        'POST' ->
            {ok, {http_response, {1,1}, 201, "Created"}} = SockFun(recv)
    end,
    ok = SockFun({setopts, [{packet, httph}]}),
    {ok, {http_header, _, 'Server', _, "MochiWeb" ++ _}} = SockFun(recv),
    {ok, {http_header, _, 'Date', _, _}} = SockFun(recv),
    {ok, {http_header, _, 'Content-Type', _, _}} = SockFun(recv),
    {ok, {http_header, _, 'Content-Length', _, ConLenStr}} = SockFun(recv),
    ContentLength = list_to_integer(ConLenStr),
    {ok, http_eoh} = SockFun(recv),
    ok = SockFun({setopts, [{packet, raw}]}),
    {payload, ExReply} = {payload, drain_reply(SockFun, ContentLength, <<>>)},
    ok = SockFun({setopts, [{packet, http}]}),
    client_request(SockFun, Method, Rest).

client_headers(Body, IsLastRequest) ->
    ["Host: localhost\r\n",
     case Body of
        <<>> ->
            "";
        _ ->
            ["Content-Type: application/octet-stream\r\n",
             "Content-Length: ", integer_to_list(byte_size(Body)), "\r\n"]
     end,
     case IsLastRequest of
         true ->
             "Connection: close\r\n";
         false ->
             ""
     end].

drain_reply(_SockFun, 0, Acc) ->
    Acc;
drain_reply(SockFun, Length, Acc) ->
    Sz = erlang:min(Length, 1024),
    {ok, B} = SockFun({recv, Sz}),
    drain_reply(SockFun, Length - Sz, <<Acc/bytes, B/bytes>>).

-endif.
