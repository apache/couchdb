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

-module(chttpd_exclude_headers_test).
-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").



mock_request(ExcludeHeader) ->
    Headers = mochiweb_headers:make(ExcludeHeader),
    MochiReq = mochiweb_request:new(nil, 'GET', "/", {1, 1}, Headers),
    MochiReq:cleanup(),
    #httpd{mochi_req = MochiReq}.



default_headers() ->
    [
        {"Cache-Control","must-revalidate"},
        {"Content-Type","application/json"},
        {"Content-Length", "100"},
        {"ETag","\"12343\""},
        {"X-Couch-Request-ID","7bd1adab86"},
        {"X-CouchDB-Body-Time","0"},
        {"Vary", "Accept-Encoding"},
        {"Server","CouchDB/2.1.0-f1a1d7f1c (Erlang OTP/19)"}
    ].



only_cache_headers() ->
    [
        {"Cache-Control","must-revalidate"},
        {"Content-Type","application/json"},
        {"Content-Length", "100"},
        {"ETag","\"12343\""},
        {"Vary", "Accept-Encoding"},
        {"Server","CouchDB/2.1.0-f1a1d7f1c (Erlang OTP/19)"}
    ].



default_no_exclude_header_test() ->
    Headers = chttpd_exclude_headers:maybe_exclude_headers(mock_request([]), default_headers()),
    ?assertEqual(default_headers(), Headers).



unsupported_exclude_header_test() ->
    Req = mock_request([{"X-Couch-Exclude-Headers", "Wrong"}]),
    Headers = chttpd_exclude_headers:maybe_exclude_headers(Req, default_headers()),
    ?assertEqual(default_headers(), Headers).



all_none_cache_headers_test() ->
    Req = mock_request([{"X-Couch-Exclude-Headers", "Non-Essential"}]),
    Headers = chttpd_exclude_headers:maybe_exclude_headers(Req, default_headers()),
    ?assertEqual(only_cache_headers(), Headers).
