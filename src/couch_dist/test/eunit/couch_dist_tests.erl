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

-module(couch_dist_tests).

-include_lib("eunit/include/eunit.hrl").

no_tls_test_() ->
    {
        "test couch_dist no_tls/1",
        {
            setup,
            fun() -> meck:new(couch_dist, [passthrough]) end,
            fun(_) -> meck:unload() end,
            [
                no_tls_test_with_true(),
                no_tls_test_with_false(),
                no_tls_test_with_character(),
                no_tls_test_with_wildcard(),
                no_tls_test_with_question_mark(),
                no_tls_test_with_error()
            ]
        }
    }.

mock_get_init_args(Reply) ->
    meck:expect(couch_dist, get_init_args, fun() -> Reply end).

no_tls_test_with_true() ->
    ?_test(
        begin
            mock_get_init_args({ok, [["no_tls", "true"]]}),
            ?assert(couch_dist:no_tls('abc123')),
            ?assert(couch_dist:no_tls("123abd"))
        end
    ).

no_tls_test_with_false() ->
    ?_test(
        begin
            mock_get_init_args({ok, [["no_tls", "false"]]}),
            ?assertNot(couch_dist:no_tls('abc123')),
            ?assertNot(couch_dist:no_tls("123abc"))
        end
    ).

no_tls_test_with_character() ->
    ?_test(
        begin
            mock_get_init_args({ok, [["no_tls", "node@127.0.0.1"]]}),
            ?assert(couch_dist:no_tls('node@127.0.0.1')),
            ?assert(couch_dist:no_tls("node@127.0.0.1"))
        end
    ).

no_tls_test_with_wildcard() ->
    ?_test(
        begin
            mock_get_init_args({ok, [["no_tls", "\"a*2\""]]}),
            ?assert(couch_dist:no_tls('ab12')),
            ?assert(couch_dist:no_tls("a12")),
            ?assert(couch_dist:no_tls("a2")),
            ?assertNot(couch_dist:no_tls('a')),
            ?assertNot(couch_dist:no_tls("2"))
        end
    ).

no_tls_test_with_question_mark() ->
    ?_test(
        begin
            mock_get_init_args({ok, [["no_tls", "\"a?2\""]]}),
            ?assert(couch_dist:no_tls('a12')),
            ?assert(couch_dist:no_tls("ab2")),
            ?assertNot(couch_dist:no_tls('a2')),
            ?assertNot(couch_dist:no_tls("a"))
        end
    ).

no_tls_test_with_error() ->
    ?_test(
        begin
            mock_get_init_args(error),
            ?assertNot(couch_dist:no_tls('abc123')),
            ?assertNot(couch_dist:no_tls("123abc"))
        end
    ).
