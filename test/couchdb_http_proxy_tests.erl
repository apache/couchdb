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

-module(couchdb_http_proxy_tests).

-include("couch_eunit.hrl").

-record(req, {method=get, path="", headers=[], body="", opts=[]}).

-define(CONFIG_FIXTURE_TEMP,
    begin
        FileName = filename:join([?TEMPDIR, ?tempfile() ++ ".ini"]),
        {ok, Fd} = file:open(FileName, write),
        ok = file:truncate(Fd),
        ok = file:close(Fd),
        FileName
    end).
-define(TIMEOUT, 5000).


start() ->
    % we have to write any config changes to temp ini file to not loose them
    % when supervisor will kill all children due to reaching restart threshold
    % (each httpd_global_handlers changes causes couch_httpd restart)
    couch_server_sup:start_link(?CONFIG_CHAIN ++ [?CONFIG_FIXTURE_TEMP]),
    % 49151 is IANA Reserved, let's assume no one is listening there
    couch_config:set("httpd_global_handlers", "_error",
        "{couch_httpd_proxy, handle_proxy_req, <<\"http://127.0.0.1:49151/\">>}"
    ),
    ok.

stop(_) ->
    couch_server_sup:stop(),
    ok.

setup() ->
    {ok, Pid} = test_web:start_link(),
    Value = lists:flatten(io_lib:format(
        "{couch_httpd_proxy, handle_proxy_req, ~p}",
        [list_to_binary(proxy_url())])),
    couch_config:set("httpd_global_handlers", "_test", Value),
    % let couch_httpd restart
    timer:sleep(100),
    Pid.

teardown(Pid) ->
    erlang:monitor(process, Pid),
    test_web:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, test_web_stop})
    end.


http_proxy_test_() ->
    {
        "HTTP Proxy handler tests",
        {
            setup,
            fun start/0, fun stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_proxy_basic_request/1,
                    fun should_return_alternative_status/1,
                    fun should_respect_trailing_slash/1,
                    fun should_proxy_headers/1,
                    fun should_proxy_host_header/1,
                    fun should_pass_headers_back/1,
                    fun should_use_same_protocol_version/1,
                    fun should_proxy_body/1,
                    fun should_proxy_body_back/1,
                    fun should_proxy_chunked_body/1,
                    fun should_proxy_chunked_body_back/1,
                    fun should_rewrite_location_header/1,
                    fun should_not_rewrite_external_locations/1,
                    fun should_rewrite_relative_location/1,
                    fun should_refuse_connection_to_backend/1
                ]
            }

        }
    }.


should_proxy_basic_request(_) ->
    Remote = fun(Req) ->
        'GET' = Req:get(method),
        "/" = Req:get(path),
        0 = Req:get(body_length),
        <<>> = Req:recv_body(),
        {ok, {200, [{"Content-Type", "text/plain"}], "ok"}}
    end,
    Local = fun
        ({ok, "200", _, "ok"}) ->
            true;
        (_) ->
            false
    end,
    ?_test(check_request(#req{}, Remote, Local)).

should_return_alternative_status(_) ->
    Remote = fun(Req) ->
        "/alternate_status" = Req:get(path),
        {ok, {201, [], "ok"}}
    end,
    Local = fun
        ({ok, "201", _, "ok"}) ->
            true;
        (_) ->
            false
    end,
    Req = #req{path = "/alternate_status"},
    ?_test(check_request(Req, Remote, Local)).

should_respect_trailing_slash(_) ->
    Remote = fun(Req) ->
        "/trailing_slash/" = Req:get(path),
        {ok, {200, [], "ok"}}
    end,
    Local = fun
        ({ok, "200", _, "ok"}) ->
            true;
        (_) ->
            false
    end,
    Req = #req{path="/trailing_slash/"},
    ?_test(check_request(Req, Remote, Local)).

should_proxy_headers(_) ->
    Remote = fun(Req) ->
        "/passes_header" = Req:get(path),
        "plankton" = Req:get_header_value("X-CouchDB-Ralph"),
        {ok, {200, [], "ok"}}
    end,
    Local = fun
        ({ok, "200", _, "ok"}) ->
            true;
        (_) ->
            false
    end,
    Req = #req{
        path="/passes_header",
        headers=[{"X-CouchDB-Ralph", "plankton"}]
    },
    ?_test(check_request(Req, Remote, Local)).

should_proxy_host_header(_) ->
    Remote = fun(Req) ->
        "/passes_host_header" = Req:get(path),
        "www.google.com" = Req:get_header_value("Host"),
        {ok, {200, [], "ok"}}
    end,
    Local = fun
        ({ok, "200", _, "ok"}) ->
            true;
        (_) ->
            false
    end,
    Req = #req{
        path="/passes_host_header",
        headers=[{"Host", "www.google.com"}]
    },
    ?_test(check_request(Req, Remote, Local)).

should_pass_headers_back(_) ->
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
    Req = #req{path="/passes_header_back"},
    ?_test(check_request(Req, Remote, Local)).

should_use_same_protocol_version(_) ->
    Remote = fun(Req) ->
        "/uses_same_version" = Req:get(path),
        {1, 0} = Req:get(version),
        {ok, {200, [], "ok"}}
    end,
    Local = fun
        ({ok, "200", _, "ok"}) ->
            true;
        (_) ->
            false
    end,
    Req = #req{
        path="/uses_same_version",
        opts=[{http_vsn, {1, 0}}]
    },
    ?_test(check_request(Req, Remote, Local)).

should_proxy_body(_) ->
    Remote = fun(Req) ->
        'PUT' = Req:get(method),
        "/passes_body" = Req:get(path),
        <<"Hooray!">> = Req:recv_body(),
        {ok, {201, [], "ok"}}
    end,
    Local = fun
        ({ok, "201", _, "ok"}) ->
            true;
        (_) ->
            false
    end,
    Req = #req{
        method=put,
        path="/passes_body",
        body="Hooray!"
    },
    ?_test(check_request(Req, Remote, Local)).

should_proxy_body_back(_) ->
    BodyChunks = [<<"foo">>, <<"bar">>, <<"bazinga">>],
    Remote = fun(Req) ->
        'GET' = Req:get(method),
        "/passes_eof_body" = Req:get(path),
        {raw, {200, [{"Connection", "close"}], BodyChunks}}
    end,
    Local = fun
        ({ok, "200", _, "foobarbazinga"}) ->
            true;
        (_) ->
            false
    end,
    Req = #req{path="/passes_eof_body"},
    ?_test(check_request(Req, Remote, Local)).

should_proxy_chunked_body(_) ->
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
        ok = Req:stream_body(1024 * 1024, RecvBody, BodyChunks),
        {ok, {201, [], "ok"}}
    end,
    Local = fun
        ({ok, "201", _, "ok"}) ->
            true;
        (_) ->
            false
    end,
    Req = #req{
        method=post,
        path="/passes_chunked_body",
        headers=[{"Transfer-Encoding", "chunked"}],
        body=chunked_body(BodyChunks)
    },
    ?_test(check_request(Req, Remote, Local)).

should_proxy_chunked_body_back(_) ->
    ?_test(begin
        Remote = fun(Req) ->
            'GET' = Req:get(method),
            "/passes_chunked_body_back" = Req:get(path),
            BodyChunks = [<<"foo">>, <<"bar">>, <<"bazinga">>],
            {chunked, {200, [{"Transfer-Encoding", "chunked"}], BodyChunks}}
        end,
        Req = #req{
            path="/passes_chunked_body_back",
            opts=[{stream_to, self()}]
        },

        Resp = check_request(Req, Remote, no_local),
        ?assertMatch({ibrowse_req_id, _}, Resp),
        {_, ReqId} = Resp,

        % Grab headers from response
        receive
            {ibrowse_async_headers, ReqId, "200", Headers} ->
                ?assertEqual("chunked",
                             proplists:get_value("Transfer-Encoding", Headers)),
            ibrowse:stream_next(ReqId)
        after 1000 ->
            throw({error, timeout})
        end,

        ?assertEqual(<<"foobarbazinga">>, recv_body(ReqId, [])),
        ?assertEqual(was_ok, test_web:check_last())
    end).

should_refuse_connection_to_backend(_) ->
    Local = fun
        ({ok, "500", _, _}) ->
            true;
        (_) ->
            false
    end,
    Req = #req{opts=[{url, server_url("/_error")}]},
    ?_test(check_request(Req, no_remote, Local)).

should_rewrite_location_header(_) ->
    {
        "Testing location header rewrites",
        do_rewrite_tests([
            {"Location", proxy_url() ++ "/foo/bar",
                         server_url() ++ "/foo/bar"},
            {"Content-Location", proxy_url() ++ "/bing?q=2",
                                 server_url() ++ "/bing?q=2"},
            {"Uri", proxy_url() ++ "/zip#frag",
                    server_url() ++ "/zip#frag"},
            {"Destination", proxy_url(),
                            server_url() ++ "/"}
        ])
    }.

should_not_rewrite_external_locations(_) ->
    {
        "Testing no rewrite of external locations",
        do_rewrite_tests([
            {"Location", external_url() ++ "/search",
                         external_url() ++ "/search"},
            {"Content-Location", external_url() ++ "/s?q=2",
                                 external_url() ++ "/s?q=2"},
            {"Uri", external_url() ++ "/f#f",
                    external_url() ++ "/f#f"},
            {"Destination", external_url() ++ "/f?q=2#f",
                            external_url() ++ "/f?q=2#f"}
        ])
    }.

should_rewrite_relative_location(_) ->
    {
        "Testing relative rewrites",
        do_rewrite_tests([
            {"Location", "/foo",
                         server_url() ++ "/foo"},
            {"Content-Location", "bar",
                                 server_url() ++ "/bar"},
            {"Uri", "/zing?q=3",
                    server_url() ++ "/zing?q=3"},
            {"Destination", "bing?q=stuff#yay",
                            server_url() ++ "/bing?q=stuff#yay"}
        ])
    }.


do_rewrite_tests(Tests) ->
    lists:map(fun({Header, Location, Url}) ->
        should_rewrite_header(Header, Location, Url)
    end, Tests).

should_rewrite_header(Header, Location, Url) ->
    Remote = fun(Req) ->
        "/rewrite_test" = Req:get(path),
        {ok, {302, [{Header, Location}], "ok"}}
    end,
    Local = fun
        ({ok, "302", Headers, "ok"}) ->
            ?assertEqual(Url, couch_util:get_value(Header, Headers)),
            true;
        (E) ->
            ?debugFmt("~p", [E]),
            false
    end,
    Req = #req{path="/rewrite_test"},
    {Header, ?_test(check_request(Req, Remote, Local))}.


server_url() ->
    server_url("/_test").

server_url(Resource) ->
    Addr = couch_config:get("httpd", "bind_address"),
    Port = integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
    lists:concat(["http://", Addr, ":", Port, Resource]).

proxy_url() ->
    "http://127.0.0.1:" ++ integer_to_list(test_web:get_port()).

external_url() ->
    "https://google.com".

check_request(Req, Remote, Local) ->
    case Remote of
        no_remote ->
            ok;
        _ ->
            test_web:set_assert(Remote)
    end,
    Url = case proplists:lookup(url, Req#req.opts) of
        none ->
            server_url() ++ Req#req.path;
        {url, DestUrl} ->
            DestUrl
    end,
    Opts = [{headers_as_is, true} | Req#req.opts],
    Resp =ibrowse:send_req(
        Url, Req#req.headers, Req#req.method, Req#req.body, Opts
    ),
    %?debugFmt("ibrowse response: ~p", [Resp]),
    case Local of
        no_local ->
            ok;
        _ ->
            ?assert(Local(Resp))
    end,
    case {Remote, Local} of
        {no_remote, _} ->
            ok;
        {_, no_local} ->
            ok;
        _ ->
            ?assertEqual(was_ok, test_web:check_last())
    end,
    Resp.

chunked_body(Chunks) ->
    chunked_body(Chunks, []).

chunked_body([], Acc) ->
    iolist_to_binary(lists:reverse(Acc, "0\r\n\r\n"));
chunked_body([Chunk | Rest], Acc) ->
    Size = to_hex(size(Chunk)),
    chunked_body(Rest, ["\r\n", Chunk, "\r\n", Size | Acc]).

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
    after ?TIMEOUT ->
        throw({error, timeout})
    end.
