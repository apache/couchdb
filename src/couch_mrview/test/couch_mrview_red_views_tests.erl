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

-module(couch_mrview_red_views_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(TIMEOUT, 1000).
-define(ADMIN_USER, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).


start() ->
    {ok, Pid} = couch_server_sup:start_link(?CONFIG_CHAIN),
    Pid.

stop(Pid) ->
    erlang:monitor(process, Pid),
    couch_server_sup:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, server_stop})
    end.

setup() ->
    {ok, Db} = couch_mrview_test_util:init_db(?tempdb(), red),
    Db.

teardown(Db) ->
    couch_db:close(Db),
    couch_server:delete(Db#db.name, [?ADMIN_USER]),
    ok.


reduce_views_test_() ->
    {
        "Reduce views",
        {
            setup,
            fun start/0, fun stop/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_reduce_basic/1,
                    fun should_reduce_key_range/1,
                    fun should_reduce_with_group_level/1,
                    fun should_reduce_with_group_exact/1
                ]
            }
        }
    }.


should_reduce_basic(Db) ->
    Result = run_query(Db, []),
    Expect = {ok, [
        {meta, []},
        {row, [{key, null}, {value, 55}]}
    ]},
    ?_assertEqual(Expect, Result).

should_reduce_key_range(Db) ->
    Result = run_query(Db, [{start_key, [0, 2]}, {end_key, [0, 4]}]),
    Expect = {ok, [
        {meta, []},
        {row, [{key, null}, {value, 6}]}
    ]},
    ?_assertEqual(Expect, Result).

should_reduce_with_group_level(Db) ->
    Result = run_query(Db, [{group_level, 1}]),
    Expect = {ok, [
        {meta, []},
        {row, [{key, [0]}, {value, 30}]},
        {row, [{key, [1]}, {value, 25}]}
    ]},
    ?_assertEqual(Expect, Result).

should_reduce_with_group_exact(Db) ->
    Result = run_query(Db, [{group_level, exact}]),
    Expect = {ok, [
        {meta, []},
        {row, [{key, [0, 2]}, {value, 2}]},
        {row, [{key, [0, 4]}, {value, 4}]},
        {row, [{key, [0, 6]}, {value, 6}]},
        {row, [{key, [0, 8]}, {value, 8}]},
        {row, [{key, [0, 10]}, {value, 10}]},
        {row, [{key, [1, 1]}, {value, 1}]},
        {row, [{key, [1, 3]}, {value, 3}]},
        {row, [{key, [1, 5]}, {value, 5}]},
        {row, [{key, [1, 7]}, {value, 7}]},
        {row, [{key, [1, 9]}, {value, 9}]}
    ]},
    ?_assertEqual(Expect, Result).


run_query(Db, Opts) ->
    couch_mrview:query_view(Db, <<"_design/bar">>, <<"baz">>, Opts).
