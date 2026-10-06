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

% Helper module to use gun instead of other http clients we had.
%
% Functions:
%
%   * req/3,4,5: For basic synchronous requests
%
%   * parse_url/1: Helper parser to turn urls with possible auth bits
%     embedded into a gun uri map
%
%   * open/3, close/1: Open/close connections
%
%   * send/5,6: Send requests on opened connections. Can take {Fun, State} for
%     a streaming body.
%
%   * await/3: Wait for response a send.
%
%   * headers/1, method/1, basic_auth/2: convert existing calling conventions
%     to gun's format. These are helpers to avoid modifying all the call sites
%     right off the bat. For example, the codebase expects headers to be
%     strings and methods to be atom so we transform them here accordingly.
%
%   * norm_error/1: shorten/normalize gun's error reason

-module(couch_gun).

-export([
    req/3,
    req/4,
    req/5,
    parse_url/1,
    open/3,
    close/1,
    send/5,
    send/6,
    await/3,
    headers/1,
    method/1,
    basic_auth/2,
    norm_error/1
]).

-define(DEFAULT_TIMEOUT, 30000).

req(Method, Url, Headers) ->
    req(Method, Url, Headers, <<>>, #{}).

req(Method, Url, Headers, Body) ->
    req(Method, Url, Headers, Body, #{}).

req(Method, Url, Headers, Body, #{} = Opts) when is_atom(Method), is_list(Url) ->
    case parse_url(Url) of
        {ok, #{transport := Transport, host := Host, port := Port} = Parsed} ->
            #{path := Path, userinfo := UserInfo} = Parsed,
            Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
            Headers1 = auth_headers(headers(Headers), UserInfo, Opts),
            OpenOpts = maps:with([tls_opts, tcp_opts, gun_opts], Opts),
            case open(Host, Port, OpenOpts#{transport => Transport}) of
                {ok, Pid} ->
                    try
                        case gun:await_up(Pid, Timeout) of
                            {ok, _} ->
                                Ref = send(Pid, Method, Path, Headers1, Body),
                                await(Pid, Ref, Timeout);
                            {error, Reason} ->
                                {error, norm_error(Reason)}
                        end
                    after
                        close(Pid)
                    end;
                {error, Reason} ->
                    {error, norm_error(Reason)}
            end;
        {error, _} = Error ->
            Error
    end.

parse_url("http://" ++ Rest) ->
    parse_auth(tcp, 80, Rest);
parse_url("https://" ++ Rest) ->
    parse_auth(tls, 443, Rest);
parse_url(_) ->
    {error, invalid_uri}.

parse_auth(Transport, DefaultPort, Rest) ->
    {Auth, Path} =
        case lists:splitwith(fun(C) -> C /= $/ andalso C /= $? end, Rest) of
            {A, ""} -> {A, "/"};
            {A, "?" ++ _ = Query} -> {A, "/" ++ Query};
            {A, P} -> {A, P}
        end,
    {UserInfo, HostPort} =
        case string:split(Auth, "@", trailing) of
            [Creds, HP] -> {Creds, HP};
            [HP] -> {undefined, HP}
        end,
    case parse_host_port(HostPort, DefaultPort) of
        {error, _} = Error ->
            Error;
        {Host, Port} ->
            {ok, #{
                transport => Transport,
                host => Host,
                port => Port,
                path => Path,
                userinfo => UserInfo
            }}
    end.

parse_host_port("", _DefaultPort) ->
    {error, invalid_uri};
parse_host_port("[" ++ Rest, DefaultPort) ->
    % ipv6 with brackets [...]
    case string:split(Rest, "]") of
        ["", _] ->
            {error, invalid_uri};
        [Host, ""] ->
            {Host, DefaultPort};
        [Host, ":" ++ PortStr] ->
            case string:to_integer(PortStr) of
                {Port, ""} when is_integer(Port) -> {Host, Port};
                _ -> {error, invalid_uri}
            end;
        _ ->
            {error, invalid_uri}
    end;
parse_host_port(HostPort, DefaultPort) ->
    case string:split(HostPort, ":", trailing) of
        ["", _] ->
            {error, invalid_uri};
        [Host, PortStr] ->
            case string:to_integer(PortStr) of
                {Port, ""} when is_integer(Port) -> {Host, Port};
                _ -> {error, invalid_uri}
            end;
        [Host] ->
            {Host, DefaultPort}
    end.

% Spawn an http/1.1 gun connection process. Opts is a map that can have
% these fields:
%
%   transport - tcp | tls (default is tcp)
%   tls_opts  - tls client opts, default is []
%   tcp_opts  - gen_tcp options
%   gun_opts  - Other gun options (see gun:open/3 docs)
%
% Host could be a string or address tuple. We don't wait for the connection to
% be up a caller may do that with gun:wait_up/2 to get connection errors
% earlier than during the first send.
%
open(Host, Port, #{} = Opts) ->
    Transport = maps:get(transport, Opts, tcp),
    OpenOpts0 = #{transport => Transport, protocols => [http], retry => 0},
    OpenOpts1 =
        case Opts of
            #{tcp_opts := TcpOpts} -> OpenOpts0#{tcp_opts => TcpOpts};
            #{} -> OpenOpts0
        end,
    OpenOpts2 =
        case Transport of
            tls -> OpenOpts1#{tls_opts => maps:get(tls_opts, Opts, [])};
            tcp -> OpenOpts1
        end,
    OpenOpts = maps:merge(OpenOpts2, maps:get(gun_opts, Opts, #{})),
    gun:open(host(Host), Port, OpenOpts).

close(Pid) when is_pid(Pid) ->
    try
        gun:close(Pid)
    catch
        _:_ -> ok
    end,
    ok.

% Send a request and get back a stream ref. Body may be a {Fun, State} tuple.
% Then Fun(State) should return {ok, Data, NewState} and then return eof at the
% end. Data will be sent chunked unless a content-length header is set.
send(Pid, Method, Path, Headers, Body) ->
    send(Pid, Method, Path, Headers, Body, #{}).

send(Pid, Method, Path, Headers, {Fun, State}, ReqOpts) when is_function(Fun, 1) ->
    SRef = gun:headers(Pid, method(Method), Path, headers(Headers), ReqOpts),
    ok = send_body(Pid, SRef, Fun, State),
    SRef;
send(Pid, Method, Path, Headers, Body, ReqOpts) when is_pid(Pid) ->
    gun:request(Pid, method(Method), Path, headers(Headers), body(Body), ReqOpts).

body([]) ->
    <<>>;
body(Body) ->
    Body.

send_body(Pid, SRef, Fun, State) ->
    case Fun(State) of
        {ok, Data, State1} ->
            send_body(Pid, SRef, Fun, State1, Data);
        eof ->
            ok = gun:data(Pid, SRef, fin, <<>>)
    end.

send_body(Pid, SRef, Fun, State, Data0) ->
    case Fun(State) of
        {ok, Data, State1} ->
            % Send pending data before sending the next.
            % We're doing one chunk at a time here
            ok = gun:data(Pid, SRef, nofin, Data0),
            send_body(Pid, SRef, Fun, State1, Data);
        eof ->
            ok = gun:data(Pid, SRef, fin, Data0)
    end.

% Wait for a response. First wait for status + headers then body. 1xx info
% responses are skipped and we don't care about trailers either. If we got a
% bad connection and didn't find out until calling send and await we'll get the
% error here.
await(Pid, SRef, Timeout) when is_pid(Pid) ->
    MRef = monitor(process, Pid),
    try await_headers(Pid, SRef, Timeout, MRef) of
        {response, fin, Code, RespHeaders} ->
            {ok, Code, RespHeaders, <<>>};
        {response, nofin, Code, RespHeaders} ->
            case gun:await_body(Pid, SRef, Timeout, MRef) of
                {ok, RespBody} -> {ok, Code, RespHeaders, RespBody};
                {ok, RespBody, _Trailers} -> {ok, Code, RespHeaders, RespBody};
                {error, Reason} -> {error, norm_error(Reason)}
            end;
        {error, Reason} ->
            {error, norm_error(Reason)}
    after
        demonitor(MRef, [flush])
    end.

await_headers(Pid, SRef, Timeout, MRef) ->
    case gun:await(Pid, SRef, Timeout, MRef) of
        {inform, _Status, _Headers} -> await_headers(Pid, SRef, Timeout, MRef);
        Other -> Other
    end.

% Transform our request headers into gun's lowercase binary shape. Previous
% http client accepted special atom headers like {basic_auth, {User, Pass}} and
% {cookie, Value}, {content_type, Type}, {content_length, Len}. We handle those
% here to avoid modifying all the call sites.

headers(Headers) ->
    [header(H) || H <- Headers].

header({basic_auth, {User, Pass}}) ->
    basic_auth(User, Pass);
header({cookie, Cookie}) ->
    {~"cookie", to_bin(Cookie)};
header({content_type, Value}) ->
    {~"content-type", to_bin(Value)};
header({content_length, Value}) ->
    {~"content-length", to_bin(Value)};
header({Name, Value}) ->
    {string:lowercase(to_bin(Name)), to_bin(Value)}.

method(Method) when is_atom(Method) ->
    string:uppercase(atom_to_binary(Method, utf8)).

basic_auth(User, Pass) ->
    UserPass = base64:encode(iolist_to_binary([User, $:, Pass])),
    {~"authorization", <<"Basic ", UserPass/binary>>}.

% If headers already have authorization set use that, otherwise take from the
% userinfo field from the url
auth_headers(Headers, UserInfo, Opts) ->
    case lists:keymember(~"authorization", 1, Headers) of
        true ->
            Headers;
        false ->
            case {Opts, UserInfo} of
                {#{basic_auth := {User, Pass}}, _} ->
                    [basic_auth(User, Pass) | Headers];
                {#{}, undefined} ->
                    Headers;
                {#{}, UserInfo} ->
                    case string:split(UserInfo, ":") of
                        [User, Pass] -> [basic_auth(User, Pass) | Headers];
                        [User] -> [basic_auth(User, "") | Headers]
                    end
            end
    end.

to_bin(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
to_bin(V) ->
    iolist_to_binary(V).

% gun expects IPs as address tuples
host(Host) when is_list(Host) ->
    case inet:parse_strict_address(Host) of
        {ok, Ip} -> Ip;
        {error, _} -> Host
    end;
host(Host) ->
    Host.

% To simplify error handling return {error, Reason} to make it easier for
% callers to handle it instead of the multi-level nested error shapes from gun.
norm_error({stream_error, Reason}) ->
    norm_error(Reason);
norm_error({connection_error, Reason}) ->
    norm_error(Reason);
norm_error({down, {shutdown, Reason}}) ->
    norm_error(Reason);
norm_error({down, Reason}) ->
    norm_error(Reason);
norm_error({shutdown, Reason}) ->
    norm_error(Reason);
norm_error(Reason) ->
    Reason.

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

parse_url_test() ->
    ?assertEqual(
        {ok, #{transport => tcp, host => "h", port => 80, path => "/", userinfo => undefined}},
        parse_url("http://h")
    ),
    ?assertEqual(
        {ok, #{
            transport => tcp, host => "h", port => 5984, path => "/db?a=b", userinfo => undefined
        }},
        parse_url("http://h:5984/db?a=b")
    ),
    ?assertEqual(
        {ok, #{transport => tls, host => "h", port => 443, path => "/", userinfo => undefined}},
        parse_url("https://h")
    ),
    ?assertEqual(
        {ok, #{
            transport => tcp, host => "127.0.0.1", port => 80, path => "/", userinfo => undefined
        }},
        parse_url("http://127.0.0.1")
    ),
    ?assertEqual(
        {ok, #{transport => tcp, host => "::1", port => 5984, path => "/db", userinfo => undefined}},
        parse_url("http://[::1]:5984/db")
    ),
    ?assertEqual(
        {ok, #{transport => tcp, host => "::1", port => 80, path => "/", userinfo => undefined}},
        parse_url("http://[::1]")
    ),
    ?assertEqual(
        {ok, #{transport => tcp, host => "h", port => 80, path => "/", userinfo => "u:p"}},
        parse_url("http://u:p@h")
    ),
    ?assertEqual(
        {ok, #{
            transport => tcp,
            host => "h",
            port => 15984,
            path => "/_dbs_info?startkey=\"db1\"&endkey=\"db2\"",
            userinfo => undefined
        }},
        parse_url("http://h:15984/_dbs_info?startkey=\"db1\"&endkey=\"db2\"")
    ),
    ?assertEqual(
        {ok, #{transport => tcp, host => "h", port => 80, path => "/?q=1", userinfo => undefined}},
        parse_url("http://h?q=1")
    ),
    ?assertEqual({error, invalid_uri}, parse_url("a potato")),
    ?assertEqual({error, invalid_uri}, parse_url("ftp://h/")),
    ?assertEqual({error, invalid_uri}, parse_url("http://")),
    ?assertEqual({error, invalid_uri}, parse_url("http://:80/")),
    ?assertEqual({error, invalid_uri}, parse_url("http://h:x/")),
    ?assertEqual({error, invalid_uri}, parse_url("http://[::1")),
    ?assertEqual({error, invalid_uri}, parse_url("http://[]:80/")).

host_test() ->
    ?assertEqual("cdb.example.com", host("cdb.example.com")),
    ?assertEqual({127, 0, 0, 1}, host("127.0.0.1")),
    ?assertEqual({0, 0, 0, 0, 0, 0, 0, 1}, host("::1")),
    ?assertEqual({1, 2, 3, 4}, host({1, 2, 3, 4})).

headers_test() ->
    ?assertEqual([], headers([])),
    ?assertEqual(
        [{~"content-type", ~"application/json"}],
        headers([{"Content-Type", "application/json"}])
    ),
    ?assertEqual(
        [{~"x-foo", ~"1"}, {~"accept", ~"*/*"}],
        headers([{'X-Foo', "1"}, {~"Accept", ~"*/*"}])
    ),
    ?assertEqual(
        [basic_auth("u", "p"), {~"cookie", ~"k=v"}],
        headers([{basic_auth, {"u", "p"}}, {cookie, "k=v"}])
    ),
    ?assertEqual(
        [{~"content-type", ~"text/plain"}, {~"content-length", ~"3"}],
        headers([{content_type, "text/plain"}, {content_length, "3"}])
    ).

method_test() ->
    ?assertEqual(~"GET", method(get)),
    ?assertEqual(~"COPY", method(copy)),
    ?assertEqual(~"DELETE", method('Delete')).

basic_auth_test() ->
    ?assertEqual(
        {~"authorization", <<"Basic ", (base64:encode(~"u:p"))/binary>>},
        basic_auth("u", "p")
    ),
    ?assertEqual(basic_auth("u", "p"), basic_auth(~"u", ~"p")).

auth_headers_test() ->
    Auth = basic_auth("u", "p"),
    Override = [{~"authorization", ~"Bearer dabears"}],
    ?assertEqual([], auth_headers([], undefined, #{})),
    ?assertEqual([Auth], auth_headers([], undefined, #{basic_auth => {"u", "p"}})),
    ?assertEqual([Auth], auth_headers([], "u:p", #{})),
    ?assertEqual([basic_auth("u", "")], auth_headers([], "u", #{})),
    ?assertEqual([Auth], auth_headers([], "x:y", #{basic_auth => {"u", "p"}})),
    ?assertEqual(Override, auth_headers(Override, "x:y", #{basic_auth => {"u", "p"}})).

norm_error_test() ->
    ?assertEqual(econnrefused, norm_error({down, {shutdown, econnrefused}})),
    ?assertEqual(closed, norm_error({stream_error, closed})),
    ?assertEqual(closed, norm_error({connection_error, closed})),
    ?assertEqual(timeout, norm_error(timeout)),
    ?assertEqual(normal, norm_error({down, normal})).

-endif.
