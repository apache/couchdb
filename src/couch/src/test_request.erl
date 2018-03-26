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
    case code:is_loaded(ibrowse) of
        false ->
            {ok, _} = ibrowse:start();
        _ ->
            ok
    end,
    case ibrowse:send_req(Url, Headers, Method, Body, Opts) of
        {ok, Code0, RespHeaders, RespBody0} ->
            Code = list_to_integer(Code0),
            RespBody = iolist_to_binary(RespBody0),
            {ok, Code, RespHeaders, RespBody};
        {error, {'EXIT', {normal, _}}} ->
            % Connection closed right after a successful request that
            % used the same connection.
            request(Method, Url, Headers, Body, N - 1);
        Error ->
            Error
    end.
