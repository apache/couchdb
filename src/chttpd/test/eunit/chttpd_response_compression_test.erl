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

-module(chttpd_response_compression_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_compression_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).
-define(ACCEPT_GZIP, {"Accept-Encoding", "gzip"}).

setup() ->
    Hashed = couch_passwords:hash_admin_password(?PASS),
    ok = config:set("admins", ?USER, ?b2l(Hashed), _Persist = false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    lists:concat(["http://", Addr, ":", Port, "/"]).

teardown(_Url) ->
    ok = config:delete("chttpd", "response_compression", _Persist = false),
    ok = config:delete("admins", ?USER, _Persist = false).

compression_test_() ->
    {
        "chttpd response compression tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0,
            fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(t_no_compression_by_default),
                    ?TDEF_FE(t_gzip_when_enabled_and_accepted),
                    ?TDEF_FE(t_no_gzip_when_enabled_but_not_accepted)
                ]
            }
        }
    }.

t_no_compression_by_default(Url) ->
    {ok, Code, Headers, _Body} = req_get(Url, [?CONTENT_JSON, ?AUTH, ?ACCEPT_GZIP]),
    ?assertEqual(200, Code),
    ContentEncoding = proplists:get_value("Content-Encoding", Headers, "identity"),
    ?assertNotEqual("gzip", ContentEncoding).

t_gzip_when_enabled_and_accepted(Url) ->
    ok = config:set("chttpd", "response_compression", "true", false),
    {ok, Code, Headers, Body} = req_get_raw(Url, [?CONTENT_JSON, ?AUTH, ?ACCEPT_GZIP]),
    ?assertEqual(200, Code),
    ContentEncoding = proplists:get_value("Content-Encoding", Headers, "identity"),
    ?assertEqual("gzip", ContentEncoding),
    % Verify the body is valid gzip
    Decompressed = zlib:gunzip(Body),
    ?assert(byte_size(Decompressed) > 0).

t_no_gzip_when_enabled_but_not_accepted(Url) ->
    ok = config:set("chttpd", "response_compression", "true", false),
    {ok, Code, Headers, _Body} = req_get(Url, [?CONTENT_JSON, ?AUTH]),
    ?assertEqual(200, Code),
    ContentEncoding = proplists:get_value("Content-Encoding", Headers, "identity"),
    ?assertNotEqual("gzip", ContentEncoding).

req_get(Url, Headers) ->
    {ok, Code, RespHeaders, Body} = test_request:get(Url, Headers),
    {ok, Code, RespHeaders, jiffy:decode(Body, [return_maps])}.

req_get_raw(Url, Headers) ->
    {ok, Code, RespHeaders, Body} = test_request:get(Url, Headers),
    {ok, Code, RespHeaders, Body}.
