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

-module(fabric_bench_test).

-include_lib("couch/include/couch_eunit.hrl").

fabric_bench_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        with([
            ?TDEF(t_default_doc_size, 15),
            ?TDEF(t_small_doc_size, 15),
            ?TDEF(t_large_doc_size, 15),
            ?TDEF(t_old_db_deletion_works),
            ?TDEF(t_newer_db_deletion_doesnt_work),
            ?TDEF(t_db_deletion_ignores_other_dbs)
        ])
    }.

setup() ->
    test_util:start_couch([fabric]).

teardown(Ctx) ->
    test_util:stop_couch(Ctx).

t_default_doc_size(_Ctx) ->
    Opts = #{docs => 100, individual_docs => 5},
    % The goal is to just have it not crash
    ?assertEqual(ok, fabric_bench:go(Opts)).

t_small_doc_size(_Ctx) ->
    Opts = #{q => 4, docs => 100, doc_size => small, individual_docs => 5},
    % The goal is to just have it not crash
    ?assertEqual(ok, fabric_bench:go(Opts)).

t_large_doc_size(_Ctx) ->
    Opts = #{q => 1, docs => 5, doc_size => large, individual_docs => 1},
    % The goal is to just have it not crash
    ?assertEqual(ok, fabric_bench:go(Opts)).

t_old_db_deletion_works(_Ctx) ->
    NineHoursAgoUsec = os:system_time(microsecond) - (9 * 60 * 60 * 1000000),
    Suffix = integer_to_binary(NineHoursAgoUsec),
    Db = <<"fabricbenchdb-", Suffix/binary>>,
    ok = fabric:create_db(Db, [{q, 1}, {n, 1}]),
    fabric_bench:delete_old_dbs(),
    % Quick db creation and deletion is racy so
    % we have to wait until the db is gone before proceeding.
    WaitFun = fun() ->
        try mem3_shards:opts_for_db(Db) of
            _ -> wait
        catch
            error:database_does_not_exist ->
                ok
        end
    end,
    test_util:wait(WaitFun, 1000).

t_newer_db_deletion_doesnt_work(_Ctx) ->
    SevenHoursAgoUsec = os:system_time(microsecond) - (7 * 60 * 60 * 1000000),
    Suffix = integer_to_binary(SevenHoursAgoUsec),
    Db = <<"fabricbenchdb-", Suffix/binary>>,
    ok = fabric:create_db(Db, [{q, 1}, {n, 1}]),
    fabric_bench:delete_old_dbs(),
    ?assertEqual({ok, 0}, fabric:get_doc_count(Db)),
    ok = fabric:delete_db(Db).

t_db_deletion_ignores_other_dbs(_Ctx) ->
    ok = delete_prefixed_dbs(<<"fabricbenchdb">>),
    Db1 = <<"fabricbenchdb-">>,
    Db2 = <<"fabricbenchdb">>,
    Db3 = <<"fabricbenchdb-xyz">>,
    ok = fabric:create_db(Db1, [{q, 1}, {n, 1}]),
    ok = fabric:create_db(Db2, [{q, 1}, {n, 1}]),
    ok = fabric:create_db(Db3, [{q, 1}, {n, 1}]),
    fabric_bench:delete_old_dbs(),
    ?assertEqual({ok, 0}, fabric:get_doc_count(Db1)),
    ?assertEqual({ok, 0}, fabric:get_doc_count(Db2)),
    ?assertEqual({ok, 0}, fabric:get_doc_count(Db3)),
    ok = delete_prefixed_dbs(<<"fabricbenchdb">>).

delete_prefixed_dbs(Prefix) ->
    {ok, Prefixed} = fabric:all_dbs(Prefix),
    lists:foreach(
        fun(Name) ->
            ok = fabric:delete_db(Name)
        end,
        Prefixed
    ).
