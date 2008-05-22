%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

%% @doc Utilities for parsing multipart/form-data.

-module(mochiweb_multipart).
-author('bob@mochimedia.com').

-export([parse_form/1, parse_form/2]).
-export([parse_multipart_request/2]).
-export([test/0]).

-define(CHUNKSIZE, 4096).

-record(mp, {state, boundary, length, buffer, callback, req}).

%% TODO: DOCUMENT THIS MODULE.

parse_form(Req) ->
    parse_form(Req, fun default_file_handler/2).

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
    BS = size(Boundary),
    Chunk = read_chunk(Req, Length),
    Length1 = Length - size(Chunk),
    <<"--", Boundary:BS/binary, "\r\n", Rest/binary>> = Chunk,
    feed_mp(headers, #mp{boundary=Prefix,
                         length=Length1,
                         buffer=Rest,
                         callback=Callback,
                         req=Req}).

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
    State#mp{length=Length - size(Data),
             buffer=Buffer1}.

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
    case find_boundary(Prefix, Buffer) of
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

with_socket_server(ServerFun, ClientFun) ->
    {ok, Server} = mochiweb_socket_server:start([{ip, "127.0.0.1"},
                                                 {port, 0},
                                                 {loop, ServerFun}]),
    Port = mochiweb_socket_server:get(Server, port),
    {ok, Client} = gen_tcp:connect("127.0.0.1", Port,
                                   [binary, {active, false}]),
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

test_callback(Expect, [Expect | Rest]) ->
    case Rest of
        [] ->
            ok;
        _ ->
            fun (Next) -> test_callback(Next, Rest) end
    end.

test_parse3() ->
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
                        case gen_tcp:send(Socket, BinContent) of
                            ok ->
                                exit(normal)
                        end
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(ServerFun, ClientFun),
    ok.


test_parse2() ->
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
                        case gen_tcp:send(Socket, BinContent) of
                            ok ->
                                exit(normal)
                        end
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(ServerFun, ClientFun),
    ok.

test_parse_form() ->
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
                        case gen_tcp:send(Socket, BinContent) of
                            ok ->
                                exit(normal)
                        end
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           size(BinContent)),
                        Res = parse_form(Req),
                        [{"submit-name", "Larry"},
                         {"files", {"file1.txt", {"text/plain",[]},
                                    <<"... contents of file1.txt ...">>}
                         }] = Res,
                        ok
                end,
    ok = with_socket_server(ServerFun, ClientFun),
    ok.

test_parse() ->
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
                        case gen_tcp:send(Socket, BinContent) of
                            ok ->
                                exit(normal)
                        end
                end,
    ClientFun = fun (Socket) ->
                        Req = fake_request(Socket, ContentType,
                                           size(BinContent)),
                        Res = parse_multipart_request(Req, TestCallback),
                        {0, <<>>, ok} = Res,
                        ok
                end,
    ok = with_socket_server(ServerFun, ClientFun),
    ok.

test_find_boundary() ->
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
    ok.

test_find_in_binary() ->
    {exact, 0} = find_in_binary(<<"foo">>, <<"foobarbaz">>),
    {exact, 1} = find_in_binary(<<"oo">>, <<"foobarbaz">>),
    {exact, 8} = find_in_binary(<<"z">>, <<"foobarbaz">>),
    not_found = find_in_binary(<<"q">>, <<"foobarbaz">>),
    {partial, 7, 2} = find_in_binary(<<"azul">>, <<"foobarbaz">>),
    {exact, 0} = find_in_binary(<<"foobarbaz">>, <<"foobarbaz">>),
    {partial, 0, 3} = find_in_binary(<<"foobar">>, <<"foo">>),
    {partial, 1, 3} = find_in_binary(<<"foobar">>, <<"afoo">>),
    ok.

test() ->
    test_find_in_binary(),
    test_find_boundary(),
    test_parse(),
    test_parse2(),
    test_parse3(),
    test_parse_form(),
    ok.
