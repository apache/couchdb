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

-module(couch_query_servers_tests).

-include_lib("couch/include/couch_eunit.hrl").


setup() ->
    meck:new([config, couch_log]).


teardown(_) ->
    meck:unload().


sum_overflow_test_() ->
    {
        "Test overflow detection in the _sum reduce function",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            [
                fun should_return_error_on_overflow/0,
                fun should_return_object_on_log/0,
                fun should_return_object_on_false/0
            ]
        }
    }.


should_return_error_on_overflow() ->
    meck:reset([config, couch_log]),
    meck:expect(
            config, get, ["query_server_config", "reduce_limit", "true"],
            "true"
        ),
    meck:expect(couch_log, error, ['_', '_'], ok),
    KVs = gen_sum_kvs(),
    {ok, [Result]} = couch_query_servers:reduce(<<"foo">>, [<<"_sum">>], KVs),
    ?assertMatch({[{<<"error">>, <<"builtin_reduce_error">>} | _]}, Result),
    ?assert(meck:called(config, get, '_')),
    ?assert(meck:called(couch_log, error, '_')).


should_return_object_on_log() ->
    meck:reset([config, couch_log]),
    meck:expect(
            config, get, ["query_server_config", "reduce_limit", "true"],
            "log"
        ),
    meck:expect(couch_log, error, ['_', '_'], ok),
    KVs = gen_sum_kvs(),
    {ok, [Result]} = couch_query_servers:reduce(<<"foo">>, [<<"_sum">>], KVs),
    ?assertMatch({[_ | _]}, Result),
    Keys = [K || {K, _} <- element(1, Result)],
    ?assert(not lists:member(<<"error">>, Keys)),
    ?assert(meck:called(config, get, '_')),
    ?assert(meck:called(couch_log, error, '_')).


should_return_object_on_false() ->
    meck:reset([config, couch_log]),
    meck:expect(
            config, get, ["query_server_config", "reduce_limit", "true"],
            "false"
        ),
    meck:expect(couch_log, error, ['_', '_'], ok),
    KVs = gen_sum_kvs(),
    {ok, [Result]} = couch_query_servers:reduce(<<"foo">>, [<<"_sum">>], KVs),
    ?assertMatch({[_ | _]}, Result),
    Keys = [K || {K, _} <- element(1, Result)],
    ?assert(not lists:member(<<"error">>, Keys)),
    ?assert(meck:called(config, get, '_')),
    ?assertNot(meck:called(couch_log, error, '_')).


gen_sum_kvs() ->
    lists:map(fun(I) ->
        Props = lists:map(fun(_) ->
            K = couch_util:encodeBase64Url(crypto:strong_rand_bytes(16)),
            {K, 1}
        end, lists:seq(1, 20)),
        [I, {Props}]
    end, lists:seq(1, 10)).
