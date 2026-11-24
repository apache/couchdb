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
    ok = config:set("admins", ?USER, ?b2l(Hashed), false),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    lists:concat(["http://", Addr, ":", Port, "/"]).

teardown(_Url) ->
    ok = config:delete("chttpd", "response_compression", false),
    ok = config:delete("chttpd", "compress_min_size", false),
    ok = config:delete("admins", ?USER, false).

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
                    ?TDEF_FE(t_no_gzip_below_min_size),
                    ?TDEF_FE(t_no_gzip_when_enabled_but_not_accepted),
                    ?TDEF_FE(t_gzip_chunked_via_node_local),
                    ?TDEF_FE(t_gzip_eventsource_changes)
                ]
            }
        }
    }.

t_no_compression_by_default(Url) ->
    {ok, Code, Headers, _Body} = req_get(Url, [?CONTENT_JSON, ?AUTH, ?ACCEPT_GZIP]),
    ?assertEqual(200, Code),
    ContentEncoding = proplists:get_value(~"content-encoding", Headers, ~"identity"),
    ?assertNotEqual(~"gzip", ContentEncoding).

t_gzip_when_enabled_and_accepted(Url) ->
    ok = config:set("chttpd", "response_compression", "true", false),
    ok = config:set("chttpd", "compress_min_size", "1", false),
    {ok, Code, Headers, Body} = req_get_raw(Url, [?CONTENT_JSON, ?AUTH, ?ACCEPT_GZIP]),
    ?assertEqual(200, Code),
    ContentEncoding = proplists:get_value(~"content-encoding", Headers, ~"identity"),
    ?assertEqual(~"gzip", ContentEncoding),
    % Verify the body is valid gzip
    Decompressed = zlib:gunzip(Body),
    ?assert(byte_size(Decompressed) > 0).

t_no_gzip_below_min_size(Url) ->
    ok = config:set("chttpd", "response_compression", "true", false),
    ok = config:set("chttpd", "compress_min_size", "1048576", false),
    {ok, Code, Headers, _Body} = req_get(Url, [?CONTENT_JSON, ?AUTH, ?ACCEPT_GZIP]),
    ?assertEqual(200, Code),
    ContentEncoding = proplists:get_value(~"content-encoding", Headers, ~"identity"),
    ?assertNotEqual(~"gzip", ContentEncoding).

t_no_gzip_when_enabled_but_not_accepted(Url) ->
    ok = config:set("chttpd", "response_compression", "true", false),
    {ok, Code, Headers, _Body} = req_get(Url, [?CONTENT_JSON, ?AUTH]),
    ?assertEqual(200, Code),
    ContentEncoding = proplists:get_value(~"content-encoding", Headers, ~"identity"),
    ?assertNotEqual(~"gzip", ContentEncoding).

t_gzip_chunked_via_node_local(Url) ->
    ok = config:set("chttpd", "response_compression", "true", false),
    ShardsDb = ?b2l(mem3_sync:shards_db()),
    NodeUrl = Url ++ "_node/_local/" ++ ShardsDb ++ "/_all_docs",
    {ok, Code, Headers, Body} = req_get_raw(NodeUrl, [?AUTH, ?ACCEPT_GZIP]),
    ?assertEqual(200, Code),
    ?assertEqual(~"gzip", proplists:get_value(~"content-encoding", Headers)),
    ?assertEqual(~"chunked", proplists:get_value(~"transfer-encoding", Headers)),
    #{~"rows" := Rows} = jiffy:decode(zlib:gunzip(Body), [return_maps]),
    ?assert(is_list(Rows)).

t_gzip_eventsource_changes(Url) ->
    ok = config:set("chttpd", "response_compression", "true", false),
    DbUrl = Url ++ ?b2l(?tempdb()),
    {ok, 201, _, _} = test_request:put(DbUrl, [?CONTENT_JSON, ?AUTH], "{}"),
    try
        FeedUrl = DbUrl ++ "/_changes?feed=eventsource&timeout=100",
        {ok, Code, Headers, Body} = req_get_raw(FeedUrl, [?AUTH, ?ACCEPT_GZIP]),
        ?assertEqual(200, Code),
        ?assertEqual(~"gzip", proplists:get_value(~"content-encoding", Headers)),
        ?assertEqual(~"\n\n", zlib:gunzip(Body))
    after
        test_request:delete(DbUrl, [?AUTH])
    end.

req_get(Url, Headers) ->
    {ok, Code, RespHeaders, Body} = test_request:get(Url, Headers),
    {ok, Code, RespHeaders, jiffy:decode(Body, [return_maps])}.

req_get_raw(Url, Headers) ->
    {ok, Code, RespHeaders, Body} = test_request:get(Url, Headers),
    {ok, Code, RespHeaders, Body}.
