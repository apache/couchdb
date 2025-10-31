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
    meck:new(couch_log, [passthrough]),
    Ctx = test_util:start_couch([ioq]),
    config:set("query_server_config", "reduce_limit", "true", false),
    config:set("log", "level", "info", false),
    Ctx.

teardown(Ctx) ->
    config:delete("query_server_config", "reduce_limit", true),
    config:delete("query_server_config", "reduce_limit_threshold", true),
    config:delete("query_server_config", "reduce_limit_ratio", true),
    config:delete("log", "level", false),
    test_util:stop_couch(Ctx),
    meck:unload().

query_server_limits_test_() ->
    {
        "Test overflow detection and batch splitting in query server",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(builtin_should_return_error_on_overflow),
                ?TDEF_FE(builtin_should_not_return_error_with_generous_overflow_threshold),
                ?TDEF_FE(builtin_should_not_return_error_with_generous_overflow_ratio),
                ?TDEF_FE(builtin_should_return_object_on_log),
                ?TDEF_FE(builtin_should_return_object_on_false),
                ?TDEF_FE(js_reduce_should_return_error_on_overflow),
                ?TDEF_FE(js_reduce_should_return_object_on_log),
                ?TDEF_FE(js_reduce_should_return_object_on_false),
                ?TDEF_FE(should_split_large_batches)
            ]
        }
    }.

builtin_should_return_error_on_overflow(_) ->
    config:set("query_server_config", "reduce_limit", "true", false),
    meck:reset(couch_log),
    KVs = gen_sum_kvs(),
    {ok, [Result]} = couch_query_servers:reduce(<<"foo">>, [<<"_sum">>], KVs),
    ?assertMatch({[{<<"error">>, <<"builtin_reduce_error">>} | _]}, Result),
    ?assert(meck:called(couch_log, error, '_')).

builtin_should_not_return_error_with_generous_overflow_threshold(_) ->
    config:set("query_server_config", "reduce_limit", "true", false),
    config:set_integer("query_server_config", "reduce_limit_threshold", 1000000, false),
    meck:reset(couch_log),
    KVs = gen_sum_kvs(),
    {ok, [Result]} = couch_query_servers:reduce(<<"foo">>, [<<"_sum">>], KVs),
    ?assertNotMatch({[{<<"error">>, <<"builtin_reduce_error">>} | _]}, Result).

builtin_should_not_return_error_with_generous_overflow_ratio(_) ->
    config:set("query_server_config", "reduce_limit", "true", false),
    config:set_float("query_server_config", "reduce_limit_ratio", 0.1, false),
    meck:reset(couch_log),
    KVs = gen_sum_kvs(),
    {ok, [Result]} = couch_query_servers:reduce(<<"foo">>, [<<"_sum">>], KVs),
    ?assertNotMatch({[{<<"error">>, <<"builtin_reduce_error">>} | _]}, Result).

builtin_should_return_object_on_log(_) ->
    config:set("query_server_config", "reduce_limit", "log", false),
    meck:reset(couch_log),
    KVs = gen_sum_kvs(),
    {ok, [Result]} = couch_query_servers:reduce(<<"foo">>, [<<"_sum">>], KVs),
    ?assertMatch({[_ | _]}, Result),
    Keys = [K || {K, _} <- element(1, Result)],
    ?assert(not lists:member(<<"error">>, Keys)),
    ?assert(meck:called(couch_log, error, '_')).

builtin_should_return_object_on_false(_) ->
    config:set("query_server_config", "reduce_limit", "false", false),
    meck:reset(couch_log),
    KVs = gen_sum_kvs(),
    {ok, [Result]} = couch_query_servers:reduce(<<"foo">>, [<<"_sum">>], KVs),
    ?assertMatch({[_ | _]}, Result),
    Keys = [K || {K, _} <- element(1, Result)],
    ?assert(not lists:member(<<"error">>, Keys)),
    ?assertNot(meck:called(couch_log, error, '_')).

js_reduce_should_return_error_on_overflow(_) ->
    config:set("query_server_config", "reduce_limit", "true", false),
    meck:reset(couch_log),
    KVs = gen_sum_kvs(),
    {ok, [Result]} = couch_query_servers:reduce(<<"javascript">>, [sum_js()], KVs),
    ?assertMatch({[{reduce_overflow_error, <<"Reduce output must shrink", _/binary>>}]}, Result),
    ?assert(meck:called(couch_log, error, '_')).

js_reduce_should_return_object_on_log(_) ->
    config:set("query_server_config", "reduce_limit", "log", false),
    meck:reset(couch_log),
    KVs = gen_sum_kvs(),
    {ok, [Result]} = couch_query_servers:reduce(<<"javascript">>, [sum_js()], KVs),
    ?assertMatch([<<"result">>, [_ | _]], Result),
    ?assert(meck:called(couch_log, info, '_')).

js_reduce_should_return_object_on_false(_) ->
    config:set("query_server_config", "reduce_limit", "false", false),
    meck:reset(couch_log),
    KVs = gen_sum_kvs(),
    {ok, [Result]} = couch_query_servers:reduce(<<"javascript">>, [sum_js()], KVs),
    ?assertMatch([<<"result">>, [_ | _]], Result),
    ?assertNot(meck:called(couch_log, info, '_')).

should_split_large_batches(_) ->
    Req = {json_req, {[]}},
    Db = <<"somedb">>,
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

sum_js() ->
    % Don't do this in real views
    <<
        "\n"
        "     function(keys, vals, rereduce) {\n"
        "         return ['result', vals.concat(vals)]\n"
        "     }\n"
        "    "
    >>.
