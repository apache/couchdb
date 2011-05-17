%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Utilities for parsing multipart/form-data.

-module(mochiweb_multipart).
-author('bob@mochimedia.com').

-export([parse_form/1, parse_form/2]).
-export([parse_multipart_request/2]).
-export([parts_to_body/3, parts_to_multipart_body/4]).
-export([default_file_handler/2]).

-define(CHUNKSIZE, 4096).

-record(mp, {state, boundary, length, buffer, callback, req}).

%% TODO: DOCUMENT THIS MODULE.
%% @type key() = atom() | string() | binary().
%% @type value() = atom() | iolist() | integer().
%% @type header() = {key(), value()}.
%% @type bodypart() = {Start::integer(), End::integer(), Body::iolist()}.
%% @type formfile() = {Name::string(), ContentType::string(), Content::binary()}.
%% @type request().
%% @type file_handler() = (Filename::string(), ContentType::string()) -> file_handler_callback().
%% @type file_handler_callback() = (binary() | eof) -> file_handler_callback() | term().

%% @spec parts_to_body([bodypart()], ContentType::string(),
%%                     Size::integer()) -> {[header()], iolist()}
%% @doc Return {[header()], iolist()} representing the body for the given
%%      parts, may be a single part or multipart.
parts_to_body([{Start, End, Body}], ContentType, Size) ->
    HeaderList = [{"Content-Type", ContentType},
                  {"Content-Range",
                   ["bytes ",
                    mochiweb_util:make_io(Start), "-", mochiweb_util:make_io(End),
                    "/", mochiweb_util:make_io(Size)]}],
    {HeaderList, Body};
parts_to_body(BodyList, ContentType, Size) when is_list(BodyList) ->
    parts_to_multipart_body(BodyList, ContentType, Size,
                            mochihex:to_hex(crypto:rand_bytes(8))).

%% @spec parts_to_multipart_body([bodypart()], ContentType::string(),
%%                               Size::integer(), Boundary::string()) ->
%%           {[header()], iolist()}
%% @doc Return {[header()], iolist()} representing the body for the given
%%      parts, always a multipart response.
parts_to_multipart_body(BodyList, ContentType, Size, Boundary) ->
    HeaderList = [{"Content-Type",
                   ["multipart/byteranges; ",
                    "boundary=", Boundary]}],
    MultiPartBody = multipart_body(BodyList, ContentType, Boundary, Size),

    {HeaderList, MultiPartBody}.

%% @spec multipart_body([bodypart()], ContentType::string(),
%%                      Boundary::string(), Size::integer()) -> iolist()
%% @doc Return the representation of a multipart body for the given [bodypart()].
multipart_body([], _ContentType, Boundary, _Size) ->
    ["--", Boundary, "--\r\n"];
multipart_body([{Start, End, Body} | BodyList], ContentType, Boundary, Size) ->
    ["--", Boundary, "\r\n",
     "Content-Type: ", ContentType, "\r\n",
     "Content-Range: ",
         "bytes ", mochiweb_util:make_io(Start), "-", mochiweb_util:make_io(End),
             "/", mochiweb_util:make_io(Size), "\r\n\r\n",
     Body, "\r\n"
     | multipart_body(BodyList, ContentType, Boundary, Size)].

%% @spec parse_form(request()) -> [{string(), string() | formfile()}]
%% @doc Parse a multipart form from the given request using the in-memory
%%      default_file_handler/2.
parse_form(Req) ->
    parse_form(Req, fun default_file_handler/2).

%% @spec parse_form(request(), F::file_handler()) -> [{string(), string() | term()}]
%% @doc Parse a multipart form from the given request using the given file_handler().
parse_form(Req, FileHandler) ->
    Callback = fun (Next) -> parse_form_outer(Next, FileHandler, []) end,
    {_, _, Res} = parse_multipart_request(Req, Callback),
    Res.

parse_form_outer(eof, _, Acc) ->
    lists:reverse(Acc);
parse_form_outer({headers, H}, FileHandler, State) ->
    {"form-data", H1} = proplists:get_value("content-disposition", H),
    Name = proplists:get_value("name", H1),
    Filename = proplists:get_value("filename", H1),
    case Filename of
        undefined ->
            fun (Next) ->
                    parse_form_value(Next, {Name, []}, FileHandler, State)
            end;
        _ ->
            ContentType = proplists:get_value("content-type", H),
            Handler = FileHandler(Filename, ContentType),
            fun (Next) ->
                    parse_form_file(Next, {Name, Handler}, FileHandler, State)
            end
    end.

parse_form_value(body_end, {Name, Acc}, FileHandler, State) ->
    Value = binary_to_list(iolist_to_binary(lists:reverse(Acc))),
    State1 = [{Name, Value} | State],
    fun (Next) -> parse_form_outer(Next, FileHandler, State1) end;
parse_form_value({body, Data}, {Name, Acc}, FileHandler, State) ->
    Acc1 = [Data | Acc],
    fun (Next) -> parse_form_value(Next, {Name, Acc1}, FileHandler, State) end.

parse_form_file(body_end, {Name, Handler}, FileHandler, State) ->
    Value = Handler(eof),
    State1 = [{Name, Value} | State],
    fun (Next) -> parse_form_outer(Next, FileHandler, State1) end;
parse_form_file({body, Data}, {Name, Handler}, FileHandler, State) ->
    H1 = Handler(Data),
    fun (Next) -> parse_form_file(Next, {Name, H1}, FileHandler, State) end.

default_file_handler(Filename, ContentType) ->
    default_file_handler_1(Filename, ContentType, []).

default_file_handler_1(Filename, ContentType, Acc) ->
    fun(eof) ->
            Value = iolist_to_binary(lists:reverse(Acc)),
            {Filename, ContentType, Value};
       (Next) ->
            default_file_handler_1(Filename, ContentType, [Next | Acc])
    end.

parse_multipart_request(Req, Callback) ->
    %% TODO: Support chunked?
    Length = list_to_integer(Req:get_header_value("content-length")),
    Boundary = iolist_to_binary(
                 get_boundary(Req:get_header_value("content-type"))),
    Prefix = <<"\r\n--", Boundary/binary>>,
    BS = byte_size(Boundary),
    Chunk = read_chunk(Req, Length),
    Length1 = Length - byte_size(Chunk),
    <<"--", Boundary:BS/binary, "\r\n", Rest/binary>> = Chunk,
    feed_mp(headers, flash_multipart_hack(#mp{boundary=Prefix,
                                              length=Length1,
                                              buffer=Rest,
                                              callback=Callback,
                                              req=Req})).

parse_headers(<<>>) ->
    [];
parse_headers(Binary) ->
    parse_headers(Binary, []).

parse_headers(Binary, Acc) ->
    case find_in_binary(<<"\r\n">>, Binary) of
        {exact, N} ->
            <<Line:N/binary, "\r\n", Rest/binary>> = Binary,
            parse_headers(Rest, [split_header(Line) | Acc]);
        not_found ->
            lists:reverse([split_header(Binary) | Acc])
    end.

split_header(Line) ->
    {Name, [$: | Value]} = lists:splitwith(fun (C) -> C =/= $: end,
                                           binary_to_list(Line)),
    {string:to_lower(string:strip(Name)),
     mochiweb_util:parse_header(Value)}.

read_chunk(Req, Length) when Length > 0 ->
    case Length of
        Length when Length < ?CHUNKSIZE ->
            Req:recv(Length);
        _ ->
            Req:recv(?CHUNKSIZE)
    end.

read_more(State=#mp{length=Length, buffer=Buffer, req=Req}) ->
    Data = read_chunk(Req, Length),
    Buffer1 = <<Buffer/binary, Data/binary>>,
    flash_multipart_hack(State#mp{length=Length - byte_size(Data),
                                  buffer=Buffer1}).

flash_multipart_hack(State=#mp{length=0, buffer=Buffer, boundary=Prefix}) ->
    %% http://code.google.com/p/mochiweb/issues/detail?id=22
    %% Flash doesn't terminate multipart with \r\n properly so we fix it up here
    PrefixSize = size(Prefix),
    case size(Buffer) - (2 + PrefixSize) of
        Seek when Seek >= 0 ->
            case Buffer of
                <<_:Seek/binary, Prefix:PrefixSize/binary, "--">> ->
                    Buffer1 = <<Buffer/binary, "\r\n">>,
                    State#mp{buffer=Buffer1};
                _ ->
                    State
            end;
        _ ->
            State
    end;
flash_multipart_hack(State) ->
    State.

feed_mp(headers, State=#mp{buffer=Buffer, callback=Callback}) ->
    {State1, P} = case find_in_binary(<<"\r\n\r\n">>, Buffer) of
                      {exact, N} ->
                          {State, N};
                      _ ->
                          S1 = read_more(State),
                          %% Assume headers must be less than ?CHUNKSIZE
                          {exact, N} = find_in_binary(<<"\r\n\r\n">>,
                                                      S1#mp.buffer),
                          {S1, N}
                  end,
    <<Headers:P/binary, "\r\n\r\n", Rest/binary>> = State1#mp.buffer,
    NextCallback = Callback({headers, parse_headers(Headers)}),
    feed_mp(body, State1#mp{buffer=Rest,
                            callback=NextCallback});
feed_mp(body, State=#mp{boundary=Prefix, buffer=Buffer, callback=Callback}) ->
    Boundary = find_boundary(Prefix, Buffer),
    case Boundary of
        {end_boundary, Start, Skip} ->
            <<Data:Start/binary, _:Skip/binary, Rest/binary>> = Buffer,
            C1 = Callback({body, Data}),
            C2 = C1(body_end),
            {State#mp.length, Rest, C2(eof)};
        {next_boundary, Start, Skip} ->
            <<Data:Start/binary, _:Skip/binary, Rest/binary>> = Buffer,
            C1 = Callback({body, Data}),
            feed_mp(headers, State#mp{callback=C1(body_end),
                                      buffer=Rest});
        {maybe, Start} ->
            <<Data:Start/binary, Rest/binary>> = Buffer,
            feed_mp(body, read_more(State#mp{callback=Callback({body, Data}),
                                             buffer=Rest}));
        not_found ->
            {Data, Rest} = {Buffer, <<>>},
            feed_mp(body, read_more(State#mp{callback=Callback({body, Data}),
                                             buffer=Rest}))
    end.

get_boundary(ContentType) ->
    {"multipart/form-data", Opts} = mochiweb_util:parse_header(ContentType),
    case proplists:get_value("boundary", Opts) of
        S when is_list(S) ->
            S
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
        <<_Skip:N/binary, B1:K/binary>> ->
            {partial, N, K};
        _ ->
            partial_find(B, D, 1 + N, K - 1)
    end.

find_boundary(Prefix, Data) ->
    case find_in_binary(Prefix, Data) of
        {exact, Skip} ->
            PrefixSkip = Skip + size(Prefix),
            case Data of
                <<_:PrefixSkip/binary, "\r\n", _/binary>> ->
                    {next_boundary, Skip, size(Prefix) + 2};
                <<_:PrefixSkip/binary, "--\r\n", _/binary>> ->
                    {end_boundary, Skip, size(Prefix) + 4};
                _ when size(Data) < PrefixSkip + 4 ->
                    %% Underflow
                    {maybe, Skip};
                _ ->
                    %% False positive
                    not_found
            end;
        {partial, Skip, Length} when (Skip + Length) =:= size(Data) ->
            %% Underflow
            {maybe, Skip};
        _ ->
            not_found
    end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

ssl_cert_opts() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    CertDir = filename:join([EbinDir, "..", "support", "test-materials"]),
    CertFile = filename:join(CertDir, "test_ssl_cert.pem"),
    KeyFile = filename:join(CertDir, "test_ssl_key.pem"),
    [{certfile, CertFile}, {keyfile, KeyFile}].

with_socket_server(Transport, ServerFun, ClientFun) ->
    ServerOpts0 = [{ip, "127.0.0.1"}, {port, 0}, {loop, ServerFun}],
    ServerOpts = case Transport of
        plain ->
            ServerOpts0;
        ssl ->
            ServerOpts0 ++ [{ssl, true}, {ssl_opts, ssl_cert_opts()}]
    end,
    {ok, Server} = mochiweb_socket_server:start(ServerOpts),
    Port = mochiweb_socket_server:get(Server, port),
    ClientOpts = [binary, {active, false}],
    {ok, Client} = case Transport of
        plain ->
            gen_tcp:connect("127.0.0.1", Port, ClientOpts);
        ssl ->
            ClientOpts1 = [{ssl_imp, new} | ClientOpts],
            {ok, SslSocket} = ssl:connect("127.0.0.1", Port, ClientOpts1),
            {ok, {ssl, SslSocket}}
    end,
    Res = (catch ClientFun(Client)),
    mochiweb_socket_server:stop(Server),
    Res.

fake_request(Socket, ContentType, Length) ->
    mochiweb_request:new(Socket,
                         'POST',
                         "/multipart",
                         {1,1},
                         mochiweb_headers:make(
                           [{"content-type", ContentType},
                            {"content-length", Length}])).

test_callback({body, <<>>}, Rest=[body_end | _]) ->
    %% When expecting the body_end we might get an empty binary
    fun (Next) -> test_callback(Next, Rest) end;
test_callback({body, Got}, [{body, Expect} | Rest]) when Got =/= Expect ->
    %% Partial response
    GotSize = size(Got),
    <<Got:GotSize/binary, Expect1/binary>> = Expect,
    fun (Next) -> test_callback(Next, [{body, Expect1} | Rest]) end;
test_callback(Got, [Expect | Rest]) ->
    ?assertEqual(Got, Expect),
    case Rest of
        [] ->
            ok;
        _ ->
            fun (Next) -> test_callback(Next, Rest) end
    end.

parse3_http_test() ->
    parse3(plain).

parse3_https_test() ->
    parse3(ssl).

parse3(Transport) ->
    ContentType = "multipart/form-data; boundary=---------------------------7386909285754635891697677882",
    BinContent = <<"-----------------------------7386909285754635891697677882\r\nContent-Disposition: form-data; name=\"hidden\"\r\n\r\nmultipart message\r\n-----------------------------7386909285754635891697677882\r\nContent-Disposition: form-data; name=\"file\"; filename=\"test_file.txt\"\r\nContent-Type: text/plain\r\n\r\nWoo multiline text file\n\nLa la la\r\n-----------------------------7386909285754635891697677882--\r\n">>,
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "hidden"}]}}]},
              {body, <<"multipart message">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "file"}, {"filename", "test_file.txt"}]}},
                {"content-type", {"text/plain", []}}]},
              {body, <<"Woo multiline text file\n\nLa la la">>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
			exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

parse2_http_test() ->
    parse2(plain).

parse2_https_test() ->
    parse2(ssl).

parse2(Transport) ->
    ContentType = "multipart/form-data; boundary=---------------------------6072231407570234361599764024",
    BinContent = <<"-----------------------------6072231407570234361599764024\r\nContent-Disposition: form-data; name=\"hidden\"\r\n\r\nmultipart message\r\n-----------------------------6072231407570234361599764024\r\nContent-Disposition: form-data; name=\"file\"; filename=\"\"\r\nContent-Type: application/octet-stream\r\n\r\n\r\n-----------------------------6072231407570234361599764024--\r\n">>,
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "hidden"}]}}]},
              {body, <<"multipart message">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "file"}, {"filename", ""}]}},
                {"content-type", {"application/octet-stream", []}}]},
              {body, <<>>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
			exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

parse_form_http_test() ->
    do_parse_form(plain).

parse_form_https_test() ->
    do_parse_form(ssl).

do_parse_form(Transport) ->
    ContentType = "multipart/form-data; boundary=AaB03x",
    "AaB03x" = get_boundary(ContentType),
    Content = mochiweb_util:join(
                ["--AaB03x",
                 "Content-Disposition: form-data; name=\"submit-name\"",
                 "",
                 "Larry",
                 "--AaB03x",
                 "Content-Disposition: form-data; name=\"files\";"
                 ++ "filename=\"file1.txt\"",
                 "Content-Type: text/plain",
                 "",
                 "... contents of file1.txt ...",
                 "--AaB03x--",
                 ""], "\r\n"),
    BinContent = iolist_to_binary(Content),
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
			exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_form(Req),
                        [{"submit-name", "Larry"},
                         {"files", {"file1.txt", {"text/plain",[]},
                                    <<"... contents of file1.txt ...">>}
                         }] = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

parse_http_test() ->
    do_parse(plain).

parse_https_test() ->
    do_parse(ssl).

do_parse(Transport) ->
    ContentType = "multipart/form-data; boundary=AaB03x",
    "AaB03x" = get_boundary(ContentType),
    Content = mochiweb_util:join(
                ["--AaB03x",
                 "Content-Disposition: form-data; name=\"submit-name\"",
                 "",
                 "Larry",
                 "--AaB03x",
                 "Content-Disposition: form-data; name=\"files\";"
                 ++ "filename=\"file1.txt\"",
                 "Content-Type: text/plain",
                 "",
                 "... contents of file1.txt ...",
                 "--AaB03x--",
                 ""], "\r\n"),
    BinContent = iolist_to_binary(Content),
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "submit-name"}]}}]},
              {body, <<"Larry">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "files"}, {"filename", "file1.txt"}]}},
                 {"content-type", {"text/plain", []}}]},
              {body, <<"... contents of file1.txt ...">>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
			exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

parse_partial_body_boundary_http_test() ->
   parse_partial_body_boundary(plain).

parse_partial_body_boundary_https_test() ->
   parse_partial_body_boundary(ssl).

parse_partial_body_boundary(Transport) ->
    Boundary = string:copies("$", 2048),
    ContentType = "multipart/form-data; boundary=" ++ Boundary,
    ?assertEqual(Boundary, get_boundary(ContentType)),
    Content = mochiweb_util:join(
                ["--" ++ Boundary,
                 "Content-Disposition: form-data; name=\"submit-name\"",
                 "",
                 "Larry",
                 "--" ++ Boundary,
                 "Content-Disposition: form-data; name=\"files\";"
                 ++ "filename=\"file1.txt\"",
                 "Content-Type: text/plain",
                 "",
                 "... contents of file1.txt ...",
                 "--" ++ Boundary ++ "--",
                 ""], "\r\n"),
    BinContent = iolist_to_binary(Content),
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "submit-name"}]}}]},
              {body, <<"Larry">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "files"}, {"filename", "file1.txt"}]}},
                {"content-type", {"text/plain", []}}
               ]},
              {body, <<"... contents of file1.txt ...">>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
			exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

parse_large_header_http_test() ->
    parse_large_header(plain).

parse_large_header_https_test() ->
    parse_large_header(ssl).

parse_large_header(Transport) ->
    ContentType = "multipart/form-data; boundary=AaB03x",
    "AaB03x" = get_boundary(ContentType),
    Content = mochiweb_util:join(
                ["--AaB03x",
                 "Content-Disposition: form-data; name=\"submit-name\"",
                 "",
                 "Larry",
                 "--AaB03x",
                 "Content-Disposition: form-data; name=\"files\";"
                 ++ "filename=\"file1.txt\"",
                 "Content-Type: text/plain",
                 "x-large-header: " ++ string:copies("%", 4096),
                 "",
                 "... contents of file1.txt ...",
                 "--AaB03x--",
                 ""], "\r\n"),
    BinContent = iolist_to_binary(Content),
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "submit-name"}]}}]},
              {body, <<"Larry">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "files"}, {"filename", "file1.txt"}]}},
                {"content-type", {"text/plain", []}},
                {"x-large-header", {string:copies("%", 4096), []}}
               ]},
              {body, <<"... contents of file1.txt ...">>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
			exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

find_boundary_test() ->
    B = <<"\r\n--X">>,
    {next_boundary, 0, 7} = find_boundary(B, <<"\r\n--X\r\nRest">>),
    {next_boundary, 1, 7} = find_boundary(B, <<"!\r\n--X\r\nRest">>),
    {end_boundary, 0, 9} = find_boundary(B, <<"\r\n--X--\r\nRest">>),
    {end_boundary, 1, 9} = find_boundary(B, <<"!\r\n--X--\r\nRest">>),
    not_found = find_boundary(B, <<"--X\r\nRest">>),
    {maybe, 0} = find_boundary(B, <<"\r\n--X\r">>),
    {maybe, 1} = find_boundary(B, <<"!\r\n--X\r">>),
    P = <<"\r\n-----------------------------16037454351082272548568224146">>,
    B0 = <<55,212,131,77,206,23,216,198,35,87,252,118,252,8,25,211,132,229,
          182,42,29,188,62,175,247,243,4,4,0,59, 13,10,45,45,45,45,45,45,45,
          45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,
          49,54,48,51,55,52,53,52,51,53,49>>,
    {maybe, 30} = find_boundary(P, B0),
    not_found = find_boundary(B, <<"\r\n--XJOPKE">>),
    ok.

find_in_binary_test() ->
    {exact, 0} = find_in_binary(<<"foo">>, <<"foobarbaz">>),
    {exact, 1} = find_in_binary(<<"oo">>, <<"foobarbaz">>),
    {exact, 8} = find_in_binary(<<"z">>, <<"foobarbaz">>),
    not_found = find_in_binary(<<"q">>, <<"foobarbaz">>),
    {partial, 7, 2} = find_in_binary(<<"azul">>, <<"foobarbaz">>),
    {exact, 0} = find_in_binary(<<"foobarbaz">>, <<"foobarbaz">>),
    {partial, 0, 3} = find_in_binary(<<"foobar">>, <<"foo">>),
    {partial, 1, 3} = find_in_binary(<<"foobar">>, <<"afoo">>),
    ok.

flash_parse_http_test() ->
    flash_parse(plain).

flash_parse_https_test() ->
    flash_parse(ssl).

flash_parse(Transport) ->
    ContentType = "multipart/form-data; boundary=----------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5",
    "----------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5" = get_boundary(ContentType),
    BinContent = <<"------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"Filename\"\r\n\r\nhello.txt\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"success_action_status\"\r\n\r\n201\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"file\"; filename=\"hello.txt\"\r\nContent-Type: application/octet-stream\r\n\r\nhello\n\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"Upload\"\r\n\r\nSubmit Query\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5--">>,
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "Filename"}]}}]},
              {body, <<"hello.txt">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "success_action_status"}]}}]},
              {body, <<"201">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "file"}, {"filename", "hello.txt"}]}},
                {"content-type", {"application/octet-stream", []}}]},
              {body, <<"hello\n">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "Upload"}]}}]},
              {body, <<"Submit Query">>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
			exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

flash_parse2_http_test() ->
    flash_parse2(plain).

flash_parse2_https_test() ->
    flash_parse2(ssl).

flash_parse2(Transport) ->
    ContentType = "multipart/form-data; boundary=----------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5",
    "----------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5" = get_boundary(ContentType),
    Chunk = iolist_to_binary(string:copies("%", 4096)),
    BinContent = <<"------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"Filename\"\r\n\r\nhello.txt\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"success_action_status\"\r\n\r\n201\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"file\"; filename=\"hello.txt\"\r\nContent-Type: application/octet-stream\r\n\r\n", Chunk/binary, "\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5\r\nContent-Disposition: form-data; name=\"Upload\"\r\n\r\nSubmit Query\r\n------------ei4GI3GI3Ij5Ef1ae0KM7Ij5ei4Ij5--">>,
    Expect = [{headers,
               [{"content-disposition",
                 {"form-data", [{"name", "Filename"}]}}]},
              {body, <<"hello.txt">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "success_action_status"}]}}]},
              {body, <<"201">>},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "file"}, {"filename", "hello.txt"}]}},
                {"content-type", {"application/octet-stream", []}}]},
              {body, Chunk},
              body_end,
              {headers,
               [{"content-disposition",
                 {"form-data", [{"name", "Upload"}]}}]},
              {body, <<"Submit Query">>},
              body_end,
              eof],
    TestCallback = fun (Next) -> test_callback(Next, Expect) end,
    ServerFun = fun (Socket) ->
                        ok = mochiweb_socket:send(Socket, BinContent),
			exit(normal)
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           byte_size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(Transport, ServerFun, ClientFun),
    ok.

parse_headers_test() ->
    ?assertEqual([], parse_headers(<<>>)).

flash_multipart_hack_test() ->
    Buffer = <<"prefix-">>,
    Prefix = <<"prefix">>,
    State = #mp{length=0, buffer=Buffer, boundary=Prefix},
    ?assertEqual(State,
                 flash_multipart_hack(State)).

parts_to_body_single_test() ->
    {HL, B} = parts_to_body([{0, 5, <<"01234">>}],
                            "text/plain",
                            10),
    [{"Content-Range", Range},
     {"Content-Type", Type}] = lists:sort(HL),
    ?assertEqual(
       <<"bytes 0-5/10">>,
       iolist_to_binary(Range)),
    ?assertEqual(
       <<"text/plain">>,
       iolist_to_binary(Type)),
    ?assertEqual(
       <<"01234">>,
       iolist_to_binary(B)),
    ok.

parts_to_body_multi_test() ->
    {[{"Content-Type", Type}],
     _B} = parts_to_body([{0, 5, <<"01234">>}, {5, 10, <<"56789">>}],
                        "text/plain",
                        10),
    ?assertMatch(
       <<"multipart/byteranges; boundary=", _/binary>>,
       iolist_to_binary(Type)),
    ok.

parts_to_multipart_body_test() ->
    {[{"Content-Type", V}], B} = parts_to_multipart_body(
                                   [{0, 5, <<"01234">>}, {5, 10, <<"56789">>}],
                                   "text/plain",
                                   10,
                                   "BOUNDARY"),
    MB = multipart_body(
           [{0, 5, <<"01234">>}, {5, 10, <<"56789">>}],
           "text/plain",
           "BOUNDARY",
           10),
    ?assertEqual(
       <<"multipart/byteranges; boundary=BOUNDARY">>,
       iolist_to_binary(V)),
    ?assertEqual(
       iolist_to_binary(MB),
       iolist_to_binary(B)),
    ok.

multipart_body_test() ->
    ?assertEqual(
       <<"--BOUNDARY--\r\n">>,
       iolist_to_binary(multipart_body([], "text/plain", "BOUNDARY", 0))),
    ?assertEqual(
       <<"--BOUNDARY\r\n"
         "Content-Type: text/plain\r\n"
         "Content-Range: bytes 0-5/10\r\n\r\n"
         "01234\r\n"
         "--BOUNDARY\r\n"
         "Content-Type: text/plain\r\n"
         "Content-Range: bytes 5-10/10\r\n\r\n"
         "56789\r\n"
         "--BOUNDARY--\r\n">>,
       iolist_to_binary(multipart_body([{0, 5, <<"01234">>}, {5, 10, <<"56789">>}],
                                       "text/plain",
                                       "BOUNDARY",
                                       10))),
    ok.

-endif.
