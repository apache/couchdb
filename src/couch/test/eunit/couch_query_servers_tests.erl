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

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").

setup() ->
    meck:new([config, couch_log]).

teardown(_) ->
    meck:unload().

setup_oom() ->
    test_util:start_couch([ioq]).

teardown_oom(Ctx) ->
    meck:unload(),
    test_util:stop_couch(Ctx).

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

filter_oom_test_() ->
    {
        "Test recovery from oom in filters",
        {
            setup,
            fun setup_oom/0,
            fun teardown_oom/1,
            [
                fun should_split_large_batches/0
            ]
        }
    }.

should_return_error_on_overflow() ->
    meck:reset([config, couch_log]),
    meck:expect(
        config,
        get,
        ["query_server_config", "reduce_limit", "true"],
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
        config,
        get,
        ["query_server_config", "reduce_limit", "true"],
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
        config,
        get,
        ["query_server_config", "reduce_limit", "true"],
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

should_split_large_batches() ->
    Req = {json_req, {[]}},
    Db = undefined,
    DDoc = #doc{
        id = <<"_design/foo">>,
        revs = {0, [<<"bork bork bork">>]},
        body =
            {[
                {<<"filters">>,
                    {[
                        {<<"bar">>, <<"function(req, doc) {return true;}">>}
                    ]}}
            ]}
    },
    FName = <<"bar">>,
    Docs = [
        #doc{id = <<"a">>, body = {[]}},
        #doc{id = <<"b">>, body = {[]}}
    ],
    meck:new(couch_os_process, [passthrough]),
    meck:expect(couch_os_process, prompt, fun(Pid, Data) ->
        case Data of
            [<<"ddoc">>, _, [<<"filters">>, <<"bar">>], [[_, _], _]] ->
                throw({os_process_error, {exit_status, 1}});
            [<<"ddoc">>, _, [<<"filters">>, <<"bar">>], [[_], _]] ->
                [true, [split_batch]];
            _ ->
                meck:passthrough([Pid, Data])
        end
    end),
    {ok, Ret} = couch_query_servers:filter_docs(Req, Db, DDoc, FName, Docs),
    ?assertEqual([split_batch, split_batch], Ret).

gen_sum_kvs() ->
    lists:map(
        fun(I) ->
            Props = lists:map(
                fun(_) ->
                    K = couch_util:encodeBase64Url(crypto:strong_rand_bytes(16)),
                    {K, 1}
                end,
                lists:seq(1, 20)
            ),
            [I, {Props}]
        end,
        lists:seq(1, 10)
    ).
