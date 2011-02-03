#!/usr/bin/env escript
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

-record(req, {method=get, path="", headers=[], body="", opts=[]}).

server() ->
    lists:concat([
        "http://127.0.0.1:",
        mochiweb_socket_server:get(couch_httpd, port),
        "/_test/"
    ]).

proxy() ->
    "http://127.0.0.1:" ++ integer_to_list(test_web:get_port()) ++ "/".

external() -> "https://www.google.com/".

main(_) ->
    test_util:init_code_path(),

    etap:plan(61),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag("Test died abnormally: ~p", [Other]),
            etap:bail("Bad return value.")
    end,
    ok.

check_request(Name, Req, Remote, Local) ->
    case Remote of
        no_remote -> ok;
        _ -> test_web:set_assert(Remote)
    end,
    Url = case proplists:lookup(url, Req#req.opts) of
        none -> server() ++ Req#req.path;
        {url, DestUrl} -> DestUrl
    end,
    Opts = [{headers_as_is, true} | Req#req.opts],
    Resp =ibrowse:send_req(
        Url, Req#req.headers, Req#req.method, Req#req.body, Opts
    ),
    %etap:diag("ibrowse response: ~p", [Resp]),
    case Local of
        no_local -> ok;
        _ -> etap:fun_is(Local, Resp, Name)
    end,
    case {Remote, Local} of
        {no_remote, _} ->
            ok;
        {_, no_local} ->
            ok;
        _ ->
            etap:is(test_web:check_last(), was_ok, Name ++ " - request handled")
    end,
    Resp.

test() ->
    ExtraConfig = [test_util:source_file("test/etap/180-http-proxy.ini")],
    couch_server_sup:start_link(test_util:config_files() ++ ExtraConfig),
    ibrowse:start(),
    crypto:start(),

    % start the test_web server on a random port
    test_web:start_link(),
    Url = lists:concat([
        "{couch_httpd_proxy, handle_proxy_req, <<\"http://127.0.0.1:",
        test_web:get_port(),
        "/\">>}"
    ]),
    couch_config:set("httpd_global_handlers", "_test", Url, false),

    % let couch_httpd restart
    timer:sleep(100),

    test_basic(),
    test_alternate_status(),
    test_trailing_slash(),
    test_passes_header(),
    test_passes_host_header(),
    test_passes_header_back(),
    test_rewrites_location_headers(),
    test_doesnt_rewrite_external_locations(),
    test_rewrites_relative_location(),
    test_uses_same_version(),
    test_passes_body(),
    test_passes_eof_body_back(),
    test_passes_chunked_body(),
    test_passes_chunked_body_back(),

    test_connect_error(),
    
    ok.

test_basic() ->
    Remote = fun(Req) ->
        'GET' = Req:get(method),
        "/" = Req:get(path),
        0 = Req:get(body_length),
        <<>> = Req:recv_body(),
        {ok, {200, [{"Content-Type", "text/plain"}], "ok"}}
    end,
    Local = fun({ok, "200", _, "ok"}) -> true; (_) -> false end,
    check_request("Basic proxy test", #req{}, Remote, Local).

test_alternate_status() ->
    Remote = fun(Req) ->
        "/alternate_status" = Req:get(path),
        {ok, {201, [], "ok"}}
    end,
    Local = fun({ok, "201", _, "ok"}) -> true; (_) -> false end,
    Req = #req{path="alternate_status"},
    check_request("Alternate status", Req, Remote, Local).

test_trailing_slash() ->
    Remote = fun(Req) ->
        "/trailing_slash/" = Req:get(path),
        {ok, {200, [], "ok"}}
    end,
    Local = fun({ok, "200", _, "ok"}) -> true; (_) -> false end,
    Req = #req{path="trailing_slash/"},
    check_request("Trailing slash", Req, Remote, Local).

test_passes_header() ->
    Remote = fun(Req) ->
        "/passes_header" = Req:get(path),
        "plankton" = Req:get_header_value("X-CouchDB-Ralph"),
        {ok, {200, [], "ok"}}
    end,
    Local = fun({ok, "200", _, "ok"}) -> true; (_) -> false end,
    Req = #req{
        path="passes_header",
        headers=[{"X-CouchDB-Ralph", "plankton"}]
    },
    check_request("Passes header", Req, Remote, Local).

test_passes_host_header() ->
    Remote = fun(Req) ->
        "/passes_host_header" = Req:get(path),
        "www.google.com" = Req:get_header_value("Host"),
        {ok, {200, [], "ok"}}
    end,
    Local = fun({ok, "200", _, "ok"}) -> true; (_) -> false end,
    Req = #req{
        path="passes_host_header",
        headers=[{"Host", "www.google.com"}]
    },
    check_request("Passes host header", Req, Remote, Local).

test_passes_header_back() ->
    Remote = fun(Req) ->
        "/passes_header_back" = Req:get(path),
        {ok, {200, [{"X-CouchDB-Plankton", "ralph"}], "ok"}}
    end,
    Local = fun
        ({ok, "200", Headers, "ok"}) ->
            lists:member({"X-CouchDB-Plankton", "ralph"}, Headers);
        (_) ->
            false
    end,
    Req = #req{path="passes_header_back"},
    check_request("Passes header back", Req, Remote, Local).

test_rewrites_location_headers() ->
    etap:diag("Testing location header rewrites."),
    do_rewrite_tests([
        {"Location", proxy() ++ "foo/bar", server() ++ "foo/bar"},
        {"Content-Location", proxy() ++ "bing?q=2", server() ++ "bing?q=2"},
        {"Uri", proxy() ++ "zip#frag", server() ++ "zip#frag"},
        {"Destination", proxy(), server()}
    ]).

test_doesnt_rewrite_external_locations() ->
    etap:diag("Testing no rewrite of external locations."),
    do_rewrite_tests([
        {"Location", external() ++ "search", external() ++ "search"},
        {"Content-Location", external() ++ "s?q=2", external() ++ "s?q=2"},
        {"Uri", external() ++ "f#f", external() ++ "f#f"},
        {"Destination", external() ++ "f?q=2#f", external() ++ "f?q=2#f"}
    ]).

test_rewrites_relative_location() ->
    etap:diag("Testing relative rewrites."),
    do_rewrite_tests([
        {"Location", "/foo", server() ++ "foo"},
        {"Content-Location", "bar", server() ++ "bar"},
        {"Uri", "/zing?q=3", server() ++ "zing?q=3"},
        {"Destination", "bing?q=stuff#yay", server() ++ "bing?q=stuff#yay"}
    ]).

do_rewrite_tests(Tests) ->
    lists:foreach(fun({Header, Location, Url}) ->
        do_rewrite_test(Header, Location, Url)
    end, Tests).
    
do_rewrite_test(Header, Location, Url) ->
    Remote = fun(Req) ->
        "/rewrite_test" = Req:get(path),
        {ok, {302, [{Header, Location}], "ok"}}
    end,
    Local = fun
        ({ok, "302", Headers, "ok"}) ->
            etap:is(
                couch_util:get_value(Header, Headers),
                Url,
                "Header rewritten correctly."
            ),
            true;
        (_) ->
            false
    end,
    Req = #req{path="rewrite_test"},
    Label = "Rewrite test for ",
    check_request(Label ++ Header, Req, Remote, Local).

test_uses_same_version() ->
    Remote = fun(Req) ->
        "/uses_same_version" = Req:get(path),
        {1, 0} = Req:get(version),
        {ok, {200, [], "ok"}}
    end,
    Local = fun({ok, "200", _, "ok"}) -> true; (_) -> false end,
    Req = #req{
        path="uses_same_version",
        opts=[{http_vsn, {1, 0}}]
    },
    check_request("Uses same version", Req, Remote, Local).

test_passes_body() ->
    Remote = fun(Req) ->
        'PUT' = Req:get(method),
        "/passes_body" = Req:get(path),
        <<"Hooray!">> = Req:recv_body(),
        {ok, {201, [], "ok"}}
    end,
    Local = fun({ok, "201", _, "ok"}) -> true; (_) -> false end,
    Req = #req{
        method=put,
        path="passes_body",
        body="Hooray!"
    },
    check_request("Passes body", Req, Remote, Local).

test_passes_eof_body_back() ->
    BodyChunks = [<<"foo">>, <<"bar">>, <<"bazinga">>],
    Remote = fun(Req) ->
        'GET' = Req:get(method),
        "/passes_eof_body" = Req:get(path),
        {raw, {200, [{"Connection", "close"}], BodyChunks}}
    end,
    Local = fun({ok, "200", _, "foobarbazinga"}) -> true; (_) -> false end,
    Req = #req{path="passes_eof_body"},
    check_request("Passes eof body", Req, Remote, Local).

test_passes_chunked_body() ->
    BodyChunks = [<<"foo">>, <<"bar">>, <<"bazinga">>],
    Remote = fun(Req) ->
        'POST' = Req:get(method),
        "/passes_chunked_body" = Req:get(path),
        RecvBody = fun
            ({Length, Chunk}, [Chunk | Rest]) ->
                Length = size(Chunk),
                Rest;
            ({0, []}, []) ->
                ok
        end,
        ok = Req:stream_body(1024*1024, RecvBody, BodyChunks),
        {ok, {201, [], "ok"}}
    end,
    Local = fun({ok, "201", _, "ok"}) -> true; (_) -> false end,
    Req = #req{
        method=post,
        path="passes_chunked_body",
        headers=[{"Transfer-Encoding", "chunked"}],
        body=mk_chunked_body(BodyChunks)
    },
    check_request("Passes chunked body", Req, Remote, Local).

test_passes_chunked_body_back() ->
    Name = "Passes chunked body back",
    Remote = fun(Req) ->
        'GET' = Req:get(method),
        "/passes_chunked_body_back" = Req:get(path),
        BodyChunks = [<<"foo">>, <<"bar">>, <<"bazinga">>],
        {chunked, {200, [{"Transfer-Encoding", "chunked"}], BodyChunks}}
    end,
    Req = #req{
        path="passes_chunked_body_back",
        opts=[{stream_to, self()}]
    },

    Resp = check_request(Name, Req, Remote, no_local),

    etap:fun_is(
        fun({ibrowse_req_id, _}) -> true; (_) -> false end,
        Resp,
        "Received an ibrowse request id."
    ),
    {_, ReqId} = Resp,
    
    % Grab headers from response
    receive
        {ibrowse_async_headers, ReqId, "200", Headers} ->
            etap:is(
                proplists:get_value("Transfer-Encoding", Headers),
                "chunked",
                "Response included the Transfer-Encoding: chunked header"
            ),
        ibrowse:stream_next(ReqId)
    after 1000 ->
        throw({error, timeout})
    end,
    
    % Check body received
    % TODO: When we upgrade to ibrowse >= 2.0.0 this check needs to
    %       check that the chunks returned are what we sent from the
    %       Remote test.
    etap:diag("TODO: UPGRADE IBROWSE"),
    etap:is(recv_body(ReqId, []), <<"foobarbazinga">>, "Decoded chunked body."),

    % Check test_web server.
    etap:is(test_web:check_last(), was_ok, Name ++ " - request handled").

test_connect_error() ->
    Local = fun({ok, "500", _Headers, _Body}) -> true; (_) -> false end,
    Url = lists:concat([
        "http://127.0.0.1:",
        mochiweb_socket_server:get(couch_httpd, port),
        "/_error"
    ]),
    Req = #req{opts=[{url, Url}]},
    check_request("Connect error", Req, no_remote, Local).


mk_chunked_body(Chunks) ->
    mk_chunked_body(Chunks, []).

mk_chunked_body([], Acc) ->
    iolist_to_binary(lists:reverse(Acc, "0\r\n\r\n"));
mk_chunked_body([Chunk | Rest], Acc) ->
    Size = to_hex(size(Chunk)),
    mk_chunked_body(Rest, ["\r\n", Chunk, "\r\n", Size | Acc]).

to_hex(Val) ->
    to_hex(Val, []).

to_hex(0, Acc) ->
    Acc;
to_hex(Val, Acc) ->
    to_hex(Val div 16, [hex_char(Val rem 16) | Acc]).

hex_char(V) when V < 10 -> $0 + V;
hex_char(V) -> $A + V - 10.

recv_body(ReqId, Acc) ->
    receive
        {ibrowse_async_response, ReqId, Data} ->
            recv_body(ReqId, [Data | Acc]);
        {ibrowse_async_response_end, ReqId} ->
            iolist_to_binary(lists:reverse(Acc));
        Else ->
            throw({error, unexpected_mesg, Else})
    after 5000 ->
        throw({error, timeout})
    end.
