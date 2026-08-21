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

-module(test_request).

-export([copy/1, copy/2, copy/3]).
-export([get/1, get/2, get/3]).
-export([post/2, post/3, post/4]).
-export([put/2, put/3, put/4]).
-export([delete/1, delete/2, delete/3]).
-export([options/1, options/2, options/3]).
-export([request/3, request/4, request/5]).

-define(TIMEOUT, 30000).

copy(Url) ->
    copy(Url, []).

copy(Url, Headers) ->
    copy(Url, Headers, []).

copy(Url, Headers, Opts) ->
    request(copy, Url, Headers, [], Opts).

get(Url) ->
    get(Url, []).

get(Url, Headers) ->
    get(Url, Headers, []).

get(Url, Headers, Opts) ->
    request(get, Url, Headers, [], Opts).

post(Url, Body) ->
    post(Url, [], Body).

post(Url, Headers, Body) ->
    post(Url, Headers, Body, []).

post(Url, Headers, Body, Opts) ->
    request(post, Url, Headers, Body, Opts).

put(Url, Body) ->
    put(Url, [], Body).

put(Url, Headers, Body) ->
    put(Url, Headers, Body, []).

put(Url, Headers, Body, Opts) ->
    request(put, Url, Headers, Body, Opts).

delete(Url) ->
    delete(Url, []).

delete(Url, Opts) ->
    delete(Url, [], Opts).

delete(Url, Headers, Opts) ->
    request(delete, Url, Headers, [], Opts).

options(Url) ->
    options(Url, []).

options(Url, Headers) ->
    options(Url, Headers, []).

options(Url, Headers, Opts) ->
    request(options, Url, Headers, [], Opts).

request(Method, Url, Headers) ->
    request(Method, Url, Headers, []).

request(Method, Url, Headers, Body) ->
    request(Method, Url, Headers, Body, [], 3).

request(Method, Url, Headers, Body, Opts) ->
    request(Method, Url, Headers, Body, Opts, 3).

request(_Method, _Url, _Headers, _Body, _Opts, 0) ->
    {error, request_failed};
request(Method, Url, Headers, Body, Opts, N) ->
    {ok, _} = application:ensure_all_started(gun),
    Headers1 = headers(Headers, Opts),
    ReqOpts = #{timeout => ?TIMEOUT, tls_opts => [{verify, verify_none}]},
    case couch_gun:req(Method, Url, Headers1, Body, ReqOpts) of
        {ok, _Code, _RespHeaders, _RespBody} = Resp ->
            Resp;
        {error, closed} ->
            % Retry. Possible race with the server starting.
            request(Method, Url, Headers, Body, Opts, N - 1);
        Error ->
            Error
    end.

headers(Headers, Opts) ->
    lists:foldl(fun apply_opt/2, couch_gun:headers(Headers), Opts).

apply_opt({host_header, Value}, Headers) ->
    [Host] = couch_gun:headers([{host, Value}]),
    lists:keystore(~"host", 1, Headers, Host);
apply_opt({basic_auth, {User, Pass}}, Headers) ->
    Auth = couch_gun:basic_auth(User, Pass),
    lists:keystore(~"authorization", 1, Headers, Auth);
apply_opt(_Other, Headers) ->
    Headers.

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

headers_test() ->
    ?assertEqual([], headers([], [])),
    ?assertEqual(
        [{~"content-type", ~"application/json"}],
        headers([{"Content-Type", "application/json"}], [])
    ),
    Auth = couch_gun:basic_auth("u", "p"),
    ?assertEqual(
        [{~"authorization", ~"Basic dTpw"}],
        headers([{basic_auth, {"u", "p"}}], [])
    ),
    ?assertEqual(
        [{~"cookie", ~"k=v"}],
        headers([{cookie, "k=v"}], [])
    ),
    ?assertEqual(
        [{~"accept", ~"*/*"}, {~"host", ~"potato.local"}],
        headers([{"Accept", "*/*"}], [{host_header, "potato.local"}])
    ),
    ?assertEqual(
        [{~"host", ~"b"}],
        headers([{"Host", "a"}], [{host_header, "b"}])
    ),
    ?assertEqual(
        [Auth],
        headers([], [{basic_auth, {"u", "p"}}])
    ),
    ?assertEqual(
        [Auth],
        headers([{basic_auth, {"x", "y"}}], [{basic_auth, {"u", "p"}}])
    ).

-endif.
