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

-module(chttpd_cors_test).


-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("chttpd/include/chttpd_cors.hrl").


-define(DEFAULT_ORIGIN, "http://example.com").
-define(DEFAULT_ORIGIN_HTTPS, "https://example.com").
-define(EXPOSED_HEADERS,
    "content-type, accept-ranges, etag, server, x-couch-request-id, " ++
    "x-couch-update-newrev, x-couchdb-body-time").

-define(CUSTOM_SUPPORTED_METHODS, ?SUPPORTED_METHODS -- ["CONNECT"]).
-define(CUSTOM_SUPPORTED_HEADERS, ["extra" | ?SUPPORTED_HEADERS -- ["pragma"]]).
-define(CUSTOM_EXPOSED_HEADERS, ["expose" | ?COUCH_HEADERS]).


%% Test helpers


empty_cors_config() ->
    [].


minimal_cors_config() ->
    [
        {<<"enable_cors">>, true},
        {<<"origins">>, {[]}}
    ].


simple_cors_config() ->
    [
        {<<"enable_cors">>, true},
        {<<"origins">>, {[
            {list_to_binary(?DEFAULT_ORIGIN), {[]}}
        ]}}
    ].


wildcard_cors_config() ->
    [
        {<<"enable_cors">>, true},
        {<<"origins">>, {[
            {<<"*">>, {[]}}
        ]}}
    ].

custom_cors_config() ->
    [
        {<<"enable_cors">>, true},
        {<<"allow_methods">>, ?CUSTOM_SUPPORTED_METHODS},
        {<<"allow_headers">>, ?CUSTOM_SUPPORTED_HEADERS},
        {<<"exposed_headers">>, ?CUSTOM_EXPOSED_HEADERS},
        {<<"origins">>, {[
            {<<"*">>, {[]}}
        ]}}
    ].

access_control_cors_config(AllowCredentials) ->
    [
        {<<"enable_cors">>, true},
        {<<"allow_credentials">>, AllowCredentials},
        {<<"origins">>, {[
            {list_to_binary(?DEFAULT_ORIGIN), {[]}}
        ]}}].


multiple_cors_config() ->
    [
        {<<"enable_cors">>, true},
        {<<"origins">>, {[
            {list_to_binary(?DEFAULT_ORIGIN), {[]}},
            {<<"https://example.com">>, {[]}},
            {<<"http://example.com:5984">>, {[]}},
            {<<"https://example.com:5984">>, {[]}}
        ]}}
    ].


mock_request(Method, Path, Headers0) ->
    HeaderKey = "Access-Control-Request-Method",
    Headers = case proplists:get_value(HeaderKey, Headers0, undefined) of
        nil ->
            proplists:delete(HeaderKey, Headers0);
        undefined ->
            case Method of
                'OPTIONS' ->
                    [{HeaderKey, atom_to_list(Method)} | Headers0];
                _ ->
                    Headers0
            end;
        _ ->
            Headers0
    end,
    Headers1 = mochiweb_headers:make(Headers),
    MochiReq = mochiweb_request:new(nil, Method, Path, {1, 1}, Headers1),
    PathParts = [list_to_binary(chttpd:unquote(Part))
        || Part <- string:tokens(Path, "/")],
    #httpd{method=Method, mochi_req=MochiReq, path_parts=PathParts}.


header(#httpd{}=Req, Key) ->
    chttpd:header_value(Req, Key);
header({mochiweb_response, [_, _, Headers]}, Key) ->
    %% header(Headers, Key);
    mochiweb_headers:get_value(Key, Headers);
header(Headers, Key) ->
    couch_util:get_value(Key, Headers, undefined).


string_headers(H) ->
    string:join(H, ", ").


assert_not_preflight_(Val) ->
    ?_assertEqual(not_preflight, Val).


%% CORS disabled tests


cors_disabled_test_() ->
    {"CORS disabled tests",
        [
            {"Empty user",
                {foreach,
                    fun empty_cors_config/0,
                    [
                        fun test_no_access_control_method_preflight_request_/1,
                        fun test_no_headers_/1,
                        fun test_no_headers_server_/1,
                        fun test_no_headers_db_/1
                    ]}}]}.


%% CORS enabled tests


cors_enabled_minimal_config_test_() ->
    {"Minimal CORS enabled, no Origins",
        {foreach,
            fun minimal_cors_config/0,
            [
                fun test_no_access_control_method_preflight_request_/1,
                fun test_incorrect_origin_simple_request_/1,
                fun test_incorrect_origin_preflight_request_/1
            ]}}.


cors_enabled_simple_config_test_() ->
    {"Simple CORS config",
        {foreach,
            fun simple_cors_config/0,
            [
                fun test_no_access_control_method_preflight_request_/1,
                fun test_preflight_request_/1,
                fun test_bad_headers_preflight_request_/1,
                fun test_good_headers_preflight_request_/1,
                fun test_db_request_/1,
                fun test_db_preflight_request_/1,
                fun test_db_host_origin_request_/1,
                fun test_preflight_with_port_no_origin_/1,
                fun test_preflight_with_scheme_no_origin_/1,
                fun test_preflight_with_scheme_port_no_origin_/1,
                fun test_case_sensitive_mismatch_of_allowed_origins_/1
            ]}}.

cors_enabled_custom_config_test_() ->
    {"Simple CORS config with custom allow_methods/allow_headers/exposed_headers",
        {foreach,
            fun custom_cors_config/0,
            [
                fun test_good_headers_preflight_request_with_custom_config_/1,
                fun test_db_request_with_custom_config_/1
            ]}}.


cors_enabled_multiple_config_test_() ->
    {"Multiple options CORS config",
        {foreach,
            fun multiple_cors_config/0,
            [
                fun test_no_access_control_method_preflight_request_/1,
                fun test_preflight_request_/1,
                fun test_db_request_/1,
                fun test_db_preflight_request_/1,
                fun test_db_host_origin_request_/1,
                fun test_preflight_with_port_with_origin_/1,
                fun test_preflight_with_scheme_with_origin_/1,
                fun test_preflight_with_scheme_port_with_origin_/1
            ]}}.


%% Access-Control-Allow-Credentials tests


%% http://www.w3.org/TR/cors/#supports-credentials
%% 6.1.3
%% If the resource supports credentials add a single
%% Access-Control-Allow-Origin header, with the value
%% of the Origin header as value, and add a single
%% Access-Control-Allow-Credentials header with the
%% case-sensitive string "true" as value.
%% Otherwise, add a single Access-Control-Allow-Origin
%% header, with either the value of the Origin header
%% or the string "*" as value.
%% Note: The string "*" cannot be used for a resource
%% that supports credentials.

db_request_credentials_header_off_test_() ->
    {"Allow credentials disabled",
        {setup,
            fun() ->
                access_control_cors_config(false)
            end,
            fun test_db_request_credentials_header_off_/1
        }
    }.


db_request_credentials_header_on_test_() ->
    {"Allow credentials enabled",
        {setup,
            fun() ->
                access_control_cors_config(true)
            end,
            fun test_db_request_credentials_header_on_/1
        }
    }.


%% CORS wildcard tests


cors_enabled_wildcard_test_() ->
    {"Wildcard CORS config",
        {foreach,
            fun wildcard_cors_config/0,
            [
                fun test_no_access_control_method_preflight_request_/1,
                fun test_preflight_request_/1,
                fun test_preflight_request_no_allow_credentials_/1,
                fun test_db_request_/1,
                fun test_db_preflight_request_/1,
                fun test_db_host_origin_request_/1,
                fun test_preflight_with_port_with_origin_/1,
                fun test_preflight_with_scheme_with_origin_/1,
                fun test_preflight_with_scheme_port_with_origin_/1,
                fun test_case_sensitive_mismatch_of_allowed_origins_/1
            ]}}.


%% Test generators


test_no_headers_(OwnerConfig) ->
    Req = mock_request('GET', "/", []),
    assert_not_preflight_(chttpd_cors:maybe_handle_preflight_request(Req, OwnerConfig)).


test_no_headers_server_(OwnerConfig) ->
    Req = mock_request('GET', "/", [{"Origin", "http://127.0.0.1"}]),
    assert_not_preflight_(chttpd_cors:maybe_handle_preflight_request(Req, OwnerConfig)).


test_no_headers_db_(OwnerConfig) ->
    Headers = [{"Origin", "http://127.0.0.1"}],
    Req = mock_request('GET', "/my_db", Headers),
    assert_not_preflight_(chttpd_cors:maybe_handle_preflight_request(Req, OwnerConfig)).


test_incorrect_origin_simple_request_(OwnerConfig) ->
    Req = mock_request('GET', "/", [{"Origin", "http://127.0.0.1"}]),
    [
        ?_assert(chttpd_cors:is_cors_enabled(OwnerConfig)),
        assert_not_preflight_(chttpd_cors:maybe_handle_preflight_request(Req, OwnerConfig))
    ].


test_incorrect_origin_preflight_request_(OwnerConfig) ->
    Headers = [
        {"Origin", "http://127.0.0.1"},
        {"Access-Control-Request-Method", "GET"}
    ],
    Req = mock_request('GET', "/", Headers),
    [
        ?_assert(chttpd_cors:is_cors_enabled(OwnerConfig)),
        assert_not_preflight_(chttpd_cors:maybe_handle_preflight_request(Req, OwnerConfig))
    ].


test_bad_headers_preflight_request_(OwnerConfig) ->
    Headers = [
        {"Origin", ?DEFAULT_ORIGIN},
        {"Access-Control-Request-Method", "GET"},
        {"Access-Control-Request-Headers", "X-Not-An-Allowed-Headers"}
    ],
    Req = mock_request('OPTIONS', "/", Headers),
    [
        ?_assert(chttpd_cors:is_cors_enabled(OwnerConfig)),
        assert_not_preflight_(chttpd_cors:maybe_handle_preflight_request(Req, OwnerConfig))
    ].


test_good_headers_preflight_request_(OwnerConfig) ->
    Headers = [
        {"Origin", ?DEFAULT_ORIGIN},
        {"Access-Control-Request-Method", "GET"},
        {"Access-Control-Request-Headers", "accept-language"}
    ],
    Req = mock_request('OPTIONS', "/", Headers),
    ?assert(chttpd_cors:is_cors_enabled(OwnerConfig)),
    {ok, Headers1} = chttpd_cors:maybe_handle_preflight_request(Req, OwnerConfig),
    [
        ?_assertEqual(?DEFAULT_ORIGIN,
            header(Headers1, "Access-Control-Allow-Origin")),
        ?_assertEqual(string_headers(?SUPPORTED_METHODS),
            header(Headers1, "Access-Control-Allow-Methods")),
        ?_assertEqual(string_headers(["accept-language"]),
            header(Headers1, "Access-Control-Allow-Headers"))
    ].

test_good_headers_preflight_request_with_custom_config_(OwnerConfig) ->
    Headers = [
        {"Origin", ?DEFAULT_ORIGIN},
        {"Access-Control-Request-Method", "GET"},
        {"Access-Control-Request-Headers", "accept-language, extra"}
    ],
    Req = mock_request('OPTIONS', "/", Headers),
    ?assert(chttpd_cors:is_cors_enabled(OwnerConfig)),
    AllowMethods = couch_util:get_value(
        <<"allow_methods">>, OwnerConfig, ?SUPPORTED_METHODS),
    AllowHeaders = couch_util:get_value(
        <<"allow_headers">>, OwnerConfig, ?SUPPORTED_HEADERS),
    {ok, Headers1} = chttpd_cors:maybe_handle_preflight_request(Req, OwnerConfig),
    [
        ?_assertEqual(?DEFAULT_ORIGIN,
            header(Headers1, "Access-Control-Allow-Origin")),
        ?_assertEqual(string_headers(AllowMethods),
            header(Headers1, "Access-Control-Allow-Methods")),
        ?_assertEqual(string_headers(["accept-language", "extra"]),
            header(Headers1, "Access-Control-Allow-Headers"))
    ].


test_preflight_request_(OwnerConfig) ->
    Headers = [
        {"Origin", ?DEFAULT_ORIGIN},
        {"Access-Control-Request-Method", "GET"}
    ],
    Req = mock_request('OPTIONS', "/", Headers),
    {ok, Headers1} = chttpd_cors:maybe_handle_preflight_request(Req, OwnerConfig),
    [
        ?_assertEqual(?DEFAULT_ORIGIN,
            header(Headers1, "Access-Control-Allow-Origin")),
        ?_assertEqual(string_headers(?SUPPORTED_METHODS),
            header(Headers1, "Access-Control-Allow-Methods"))
    ].


test_no_access_control_method_preflight_request_(OwnerConfig) ->
    Headers = [
        {"Origin", ?DEFAULT_ORIGIN},
        {"Access-Control-Request-Method", notnil}
    ],
    Req = mock_request('OPTIONS', "/", Headers),
    assert_not_preflight_(chttpd_cors:maybe_handle_preflight_request(Req, OwnerConfig)).


test_preflight_request_no_allow_credentials_(OwnerConfig) ->
    Headers = [
        {"Origin", ?DEFAULT_ORIGIN},
        {"Access-Control-Request-Method", "GET"}
    ],
    Req = mock_request('OPTIONS', "/", Headers),
    {ok, Headers1} = chttpd_cors:maybe_handle_preflight_request(Req, OwnerConfig),
    [
        ?_assertEqual(?DEFAULT_ORIGIN,
            header(Headers1, "Access-Control-Allow-Origin")),
        ?_assertEqual(string_headers(?SUPPORTED_METHODS),
            header(Headers1, "Access-Control-Allow-Methods")),
        ?_assertEqual(undefined,
            header(Headers1, "Access-Control-Allow-Credentials"))
    ].


test_db_request_(OwnerConfig) ->
    Origin = ?DEFAULT_ORIGIN,
    Headers = [{"Origin", Origin}],
    Req = mock_request('GET', "/my_db", Headers),
    Headers1 = chttpd_cors:headers(Req, Headers, Origin, OwnerConfig),
    [
        ?_assertEqual(?DEFAULT_ORIGIN,
            header(Headers1, "Access-Control-Allow-Origin")),
        ?_assertEqual(?EXPOSED_HEADERS,
            header(Headers1, "Access-Control-Expose-Headers"))
    ].

test_db_request_with_custom_config_(OwnerConfig) ->
    Origin = ?DEFAULT_ORIGIN,
    Headers = [{"Origin", Origin}, {"extra", "EXTRA"}],
    Req = mock_request('GET', "/my_db", Headers),
    Headers1 = chttpd_cors:headers(Req, Headers, Origin, OwnerConfig),
    ExposedHeaders = couch_util:get_value(
        <<"exposed_headers">>, OwnerConfig, ?COUCH_HEADERS),
    [
        ?_assertEqual(?DEFAULT_ORIGIN,
            header(Headers1, "Access-Control-Allow-Origin")),
        ?_assertEqual(lists:sort(["content-type" | ExposedHeaders]),
            lists:sort(
                split_list(header(Headers1, "Access-Control-Expose-Headers"))))
    ].


test_db_preflight_request_(OwnerConfig) ->
    Headers = [
        {"Origin", ?DEFAULT_ORIGIN}
    ],
    Req = mock_request('OPTIONS', "/my_db", Headers),
    {ok, Headers1} = chttpd_cors:maybe_handle_preflight_request(Req, OwnerConfig),
    [
        ?_assertEqual(?DEFAULT_ORIGIN,
            header(Headers1, "Access-Control-Allow-Origin")),
        ?_assertEqual(string_headers(?SUPPORTED_METHODS),
            header(Headers1, "Access-Control-Allow-Methods"))
    ].


test_db_host_origin_request_(OwnerConfig) ->
    Origin = ?DEFAULT_ORIGIN,
    Headers = [
        {"Origin", Origin},
        {"Host", "example.com"}
    ],
    Req = mock_request('GET', "/my_db", Headers),
    Headers1 = chttpd_cors:headers(Req, Headers, Origin, OwnerConfig),
    [
        ?_assertEqual(?DEFAULT_ORIGIN,
            header(Headers1, "Access-Control-Allow-Origin")),
        ?_assertEqual(?EXPOSED_HEADERS,
            header(Headers1, "Access-Control-Expose-Headers"))
    ].


test_preflight_origin_helper_(OwnerConfig, Origin, ExpectedOrigin) ->
    Headers = [
        {"Origin", Origin},
        {"Access-Control-Request-Method", "GET"}
    ],
    Req = mock_request('OPTIONS', "/", Headers),
    Headers1 = chttpd_cors:headers(Req, Headers, Origin, OwnerConfig),
    [?_assertEqual(ExpectedOrigin,
        header(Headers1, "Access-Control-Allow-Origin"))
    ].


test_preflight_with_port_no_origin_(OwnerConfig) ->
    Origin = ?DEFAULT_ORIGIN ++ ":5984",
    test_preflight_origin_helper_(OwnerConfig, Origin, undefined).


test_preflight_with_port_with_origin_(OwnerConfig) ->
    Origin = ?DEFAULT_ORIGIN ++ ":5984",
    test_preflight_origin_helper_(OwnerConfig, Origin, Origin).


test_preflight_with_scheme_no_origin_(OwnerConfig) ->
    test_preflight_origin_helper_(OwnerConfig, ?DEFAULT_ORIGIN_HTTPS, undefined).


test_preflight_with_scheme_with_origin_(OwnerConfig) ->
    Origin = ?DEFAULT_ORIGIN_HTTPS,
    test_preflight_origin_helper_(OwnerConfig, Origin, Origin).


test_preflight_with_scheme_port_no_origin_(OwnerConfig) ->
    Origin = ?DEFAULT_ORIGIN_HTTPS ++ ":5984",
    test_preflight_origin_helper_(OwnerConfig, Origin, undefined).


test_preflight_with_scheme_port_with_origin_(OwnerConfig) ->
    Origin = ?DEFAULT_ORIGIN_HTTPS ++ ":5984",
    test_preflight_origin_helper_(OwnerConfig, Origin, Origin).


test_case_sensitive_mismatch_of_allowed_origins_(OwnerConfig) ->
    Origin = "http://EXAMPLE.COM",
    Headers = [{"Origin", Origin}],
    Req = mock_request('GET', "/", Headers),
    Headers1 = chttpd_cors:headers(Req, Headers, Origin, OwnerConfig),
    [
        ?_assertEqual(?DEFAULT_ORIGIN,
            header(Headers1, "Access-Control-Allow-Origin")),
        ?_assertEqual(?EXPOSED_HEADERS,
            header(Headers1, "Access-Control-Expose-Headers"))
    ].


test_db_request_credentials_header_off_(OwnerConfig) ->
    Origin = ?DEFAULT_ORIGIN,
    Headers = [{"Origin", Origin}],
    Req = mock_request('GET', "/", Headers),
    Headers1 = chttpd_cors:headers(Req, Headers, Origin, OwnerConfig),
    [
        ?_assertEqual(?DEFAULT_ORIGIN,
            header(Headers1, "Access-Control-Allow-Origin")),
        ?_assertEqual(undefined,
            header(Headers1, "Access-Control-Allow-Credentials"))
    ].


test_db_request_credentials_header_on_(OwnerConfig) ->
    Origin = ?DEFAULT_ORIGIN,
    Headers = [{"Origin", Origin}],
    Req = mock_request('GET', "/", Headers),
    Headers1 = chttpd_cors:headers(Req, Headers, Origin, OwnerConfig),
    [
        ?_assertEqual(?DEFAULT_ORIGIN,
            header(Headers1, "Access-Control-Allow-Origin")),
        ?_assertEqual("true",
            header(Headers1, "Access-Control-Allow-Credentials"))
    ].

split_list(S) ->
    re:split(S, "\\s*,\\s*", [trim, {return, list}]).
