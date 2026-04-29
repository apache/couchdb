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

-module(couch_replicator_dns_tests).

-include_lib("couch/include/couch_eunit.hrl").

match_pattern_test_() ->
    [
        % wildcard matching
        ?_assert(
            couch_replicator_dns:match_pattern(
                <<"account.example.test">>, <<"*.example.test">>
            )
        ),
        ?_assertNot(
            couch_replicator_dns:match_pattern(
                <<"example.test">>, <<"*.example.test">>
            )
        ),
        % exact matching
        ?_assert(
            couch_replicator_dns:match_pattern(
                <<"exact.example.test">>, <<"exact.example.test">>
            )
        ),
        ?_assertNot(
            couch_replicator_dns:match_pattern(
                <<"other.example.test">>, <<"exact.example.test">>
            )
        ),
        ?_assertNot(
            couch_replicator_dns:match_pattern(
                <<"short">>, <<"*.verylongpattern.example.test">>
            )
        ),
        % case-insensitive matching
        ?_assert(
            couch_replicator_dns:match_pattern(
                <<"Account.Example.Test">>, <<"*.example.test">>
            )
        ),
        ?_assert(
            couch_replicator_dns:match_pattern(
                <<"account.example.test">>, <<"*.Example.Test">>
            )
        ),
        ?_assert(
            couch_replicator_dns:match_pattern(
                <<"ACCOUNT.EXAMPLE.TEST">>, <<"*.example.test">>
            )
        ),
        ?_assert(
            couch_replicator_dns:match_pattern(
                <<"Exact.Example.Test">>, <<"exact.example.test">>
            )
        ),
        ?_assert(
            couch_replicator_dns:match_pattern(
                <<"exact.example.test">>, <<"Exact.Example.Test">>
            )
        )
    ].

parse_config_test_() ->
    [
        ?_assertEqual(
            2,
            length(
                couch_replicator_dns:parse_config(
                    "*.example.test:proxy.internal, exact.example.test:127.0.0.1"
                )
            )
        ),
        ?_assertEqual([], couch_replicator_dns:parse_config("")),
        ?_assertEqual([], couch_replicator_dns:parse_config("*.example.test:,:proxy.internal, ,")),
        % IPv6 targets are allowed (in brackets)
        ?_assertEqual(
            1,
            length(
                couch_replicator_dns:parse_config(
                    "*.example.test:[2001:db8::1]"
                )
            )
        ),
        ?_assertEqual(
            1,
            length(
                couch_replicator_dns:parse_config(
                    "*.example.test:[::1]"
                )
            )
        ),
        % IPv6 patterns are rejected
        ?_assertEqual([], couch_replicator_dns:parse_config("[2001:db8::1]:proxy.internal")),
        ?_assertEqual([], couch_replicator_dns:parse_config("[::1]:127.0.0.1"))
    ].

resolve_host_test_() ->
    {setup,
        fun() ->
            meck:new(config, [passthrough]),
            meck:expect(config, get, fun
                ("replicator", "dns_overrides", _) ->
                    "*.example.test:proxy.internal,foo.bar.com:127.0.0.1";
                (_, _, Default) ->
                    Default
            end)
        end,
        fun(_) ->
            meck:unload(config)
        end,
        [
            % wildcard pattern matches
            ?_assertEqual(
                {"proxy.internal", "account.example.test"},
                couch_replicator_dns:resolve_host("account.example.test")
            ),
            ?_assertEqual(
                {"proxy.internal", "sub.domain.example.test"},
                couch_replicator_dns:resolve_host("sub.domain.example.test")
            ),
            % exact match
            ?_assertEqual(
                {"127.0.0.1", "foo.bar.com"},
                couch_replicator_dns:resolve_host("foo.bar.com")
            ),
            % no match - different domain
            ?_assertEqual(
                {"other.example.org", undefined},
                couch_replicator_dns:resolve_host("other.example.org")
            ),
            % no match - base domain without subdomain
            ?_assertEqual(
                {"example.test", undefined},
                couch_replicator_dns:resolve_host("example.test")
            ),
            % no match - local addresses
            ?_assertEqual(
                {"127.0.0.1", undefined},
                couch_replicator_dns:resolve_host("127.0.0.1")
            ),
            ?_assertEqual(
                {"localhost", undefined},
                couch_replicator_dns:resolve_host("localhost")
            ),
            % no match - proxy target itself
            ?_assertEqual(
                {"proxy.internal", undefined},
                couch_replicator_dns:resolve_host("proxy.internal")
            ),
            % no match - IP addresses
            ?_assertEqual(
                {"192.168.1.1", undefined},
                couch_replicator_dns:resolve_host("192.168.1.1")
            ),
            ?_assertEqual(
                {"::1", undefined},
                couch_replicator_dns:resolve_host("::1")
            )
        ]}.
