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

-module(chttpd_prefer_header_test).

-compile(tuple_calls).

-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").

mock_request(ExcludeHeader) ->
    Headers = mochiweb_headers:make(ExcludeHeader),
    MochiReq = mochiweb_request:new(nil, 'GET', "/", {1, 1}, Headers),
    MochiReq:cleanup(),
    #httpd{mochi_req = MochiReq}.

default_headers() ->
    [
        {"Cache-Control", "must-revalidate"},
        {"Content-Type", "application/json"},
        {"Content-Length", "100"},
        {"ETag", "\"12343\""},
        {"X-Couch-Request-ID", "7bd1adab86"},
        {"X-CouchDB-Body-Time", "0"},
        {"Vary", "Accept-Encoding"},
        {"Server", "CouchDB/2.1.0-f1a1d7f1c (Erlang OTP/19)"}
    ].

minimal_options_headers() ->
    [
        {"Cache-Control", "must-revalidate"},
        {"Content-Type", "application/json"},
        {"Content-Length", "100"},
        {"ETag", "\"12343\""},
        {"Vary", "Accept-Encoding"},
        {"Server", "CouchDB/2.1.0-f1a1d7f1c (Erlang OTP/19)"}
    ].

default_no_exclude_header_test() ->
    Headers = chttpd_prefer_header:maybe_return_minimal(
        mock_request([]),
        default_headers()
    ),
    ?assertEqual(default_headers(), Headers).

unsupported_exclude_header_test() ->
    Req = mock_request([{"prefer", "Wrong"}]),
    Headers = chttpd_prefer_header:maybe_return_minimal(Req, default_headers()),
    ?assertEqual(default_headers(), Headers).

empty_header_test() ->
    Req = mock_request([{"prefer", ""}]),
    Headers = chttpd_prefer_header:maybe_return_minimal(Req, default_headers()),
    ?assertEqual(default_headers(), Headers).

setup_all() ->
    ok = meck:new(config),
    ok = meck:expect(config, get, fun("chttpd", "prefer_minimal", _) ->
        "Cache-Control, Content-Length, Content-Type, ETag, Server, Vary"
    end),
    ok.

teardown_all(_) ->
    meck:unload().

setup() ->
    meck:reset([config]).

teardown(_) ->
    ok.

exclude_headers_test_() ->
    {
        "Test Prefer headers",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun minimal_options/1,
                    fun minimal_options_check_header_case/1,
                    fun minimal_options_check_header_value_case/1
                ]
            }
        }
    }.

minimal_options(_) ->
    Req = mock_request([{"Prefer", "return=minimal"}]),
    Headers = chttpd_prefer_header:maybe_return_minimal(Req, default_headers()),
    ?_assertEqual(minimal_options_headers(), Headers).

minimal_options_check_header_case(_) ->
    Req = mock_request([{"prefer", "return=minimal"}]),
    Headers = chttpd_prefer_header:maybe_return_minimal(Req, default_headers()),
    ?_assertEqual(minimal_options_headers(), Headers).

minimal_options_check_header_value_case(_) ->
    Req = mock_request([{"prefer", "RETURN=MINIMAL"}]),
    Headers = chttpd_prefer_header:maybe_return_minimal(Req, default_headers()),
    ?_assertEqual(minimal_options_headers(), Headers).
