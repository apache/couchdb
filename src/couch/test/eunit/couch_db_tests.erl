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

-module(couch_db_tests).

-include_lib("couch/include/couch_eunit.hrl").

-define(TIMEOUT, 120).

create_delete_db_test_() ->
    {
        "Database create/delete tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun() -> ?tempdb() end,
                [
                    fun should_create_db/1,
                    fun should_delete_db/1
                ]
            }
        }
    }.

create_delete_multiple_dbs_test_() ->
    {
        "Multiple database create/delete tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun() -> [?tempdb() || _ <- lists:seq(1, 6)] end,
                [
                    fun should_create_multiple_dbs/1,
                    fun should_delete_multiple_dbs/1
                ]
            }
        }
    }.

create_delete_database_continuously_test_() ->
    {
        "Continious database create/delete tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreachx,
                fun(_) -> ?tempdb() end,
                [
                    {10, fun should_create_delete_database_continuously/2},
                    {100, fun should_create_delete_database_continuously/2}
                ]
            }
        }
    }.

open_db_test_() ->
    {
        "Database open tests",
        {
            setup,
            fun test_util:start_couch/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun() -> ?tempdb() end,
                [
                    fun should_create_db_if_missing/1,
                    fun should_open_db_if_exists/1,
                    fun locking_should_work/1
                ]
            }
        }
    }.

should_create_db(DbName) ->
    ?_test(begin
        {ok, Before} = couch_server:all_databases(),
        ?assertNot(lists:member(DbName, Before)),
        ?assert(create_db(DbName)),
        {ok, After} = couch_server:all_databases(),
        ?assert(lists:member(DbName, After))
    end).

should_delete_db(DbName) ->
    ?_test(begin
        ?assert(create_db(DbName)),
        {ok, Before} = couch_server:all_databases(),
        ?assert(lists:member(DbName, Before)),
        couch_server:delete(DbName, []),
        {ok, After} = couch_server:all_databases(),
        ?assertNot(lists:member(DbName, After))
    end).

should_create_multiple_dbs(DbNames) ->
    ?_test(begin
        [
            gen_server:call(couch_server:couch_server(N), {set_max_dbs_open, 3})
         || N <- lists:seq(1, couch_server:num_servers())
        ],
        {ok, Before} = couch_server:all_databases(),
        [?assertNot(lists:member(DbName, Before)) || DbName <- DbNames],
        [?assert(create_db(DbName)) || DbName <- DbNames],
        {ok, After} = couch_server:all_databases(),
        [?assert(lists:member(DbName, After)) || DbName <- DbNames]
    end).

should_delete_multiple_dbs(DbNames) ->
    ?_test(begin
        [?assert(create_db(DbName)) || DbName <- DbNames],
        {ok, Before} = couch_server:all_databases(),
        [?assert(lists:member(DbName, Before)) || DbName <- DbNames],
        [?assert(delete_db(DbName)) || DbName <- DbNames],
        {ok, After} = couch_server:all_databases(),
        [?assertNot(lists:member(DbName, After)) || DbName <- DbNames]
    end).

should_create_delete_database_continuously(Times, DbName) ->
    {
        lists:flatten(io_lib:format("~b times", [Times])),
        {timeout, ?TIMEOUT,
            ?_test(begin
                ?assert(create_db(DbName)),
                lists:foreach(
                    fun(_) ->
                        ?assert(delete_db(DbName)),
                        ?assert(create_db(DbName))
                    end,
                    lists:seq(1, Times)
                )
            end)}
    }.

should_create_db_if_missing(DbName) ->
    ?_test(begin
        {ok, Before} = couch_server:all_databases(),
        ?assertNot(lists:member(DbName, Before)),
        {ok, Db} = couch_db:open(DbName, [{create_if_missing, true}]),
        ok = couch_db:close(Db),
        {ok, After} = couch_server:all_databases(),
        ?assert(lists:member(DbName, After))
    end).

should_open_db_if_exists(DbName) ->
    ?_test(begin
        ?assert(create_db(DbName)),
        {ok, Before} = couch_server:all_databases(),
        ?assert(lists:member(DbName, Before)),
        {ok, Db} = couch_db:open(DbName, [{create_if_missing, true}]),
        ok = couch_db:close(Db),
        {ok, After} = couch_server:all_databases(),
        ?assert(lists:member(DbName, After))
    end).

locking_should_work(DbName) ->
    ?_test(begin
        ?assertEqual(ok, couch_server:lock(DbName, <<"x">>)),
        ?assertEqual({error, {locked, <<"x">>}}, couch_db:create(DbName, [])),
        ?assertEqual(ok, couch_server:unlock(DbName)),
        {ok, Db} = couch_db:create(DbName, []),
        ?assertEqual(
            {error, already_opened},
            couch_server:lock(DbName, <<>>)
        ),

        ok = couch_db:close(Db),
        catch exit(couch_db:get_pid(Db), kill),
        test_util:wait(fun() ->
            case ets:lookup(couch_server:couch_dbs(DbName), DbName) of
                [] -> ok;
                [_ | _] -> wait
            end
        end),

        ?assertEqual(ok, couch_server:lock(DbName, <<"y">>)),
        ?assertEqual(
            {error, {locked, <<"y">>}},
            couch_db:open(DbName, [])
        ),

        couch_server:unlock(DbName),
        {ok, Db1} = couch_db:open(DbName, [{create_if_missing, true}]),
        ok = couch_db:close(Db1)
    end).

create_db(DbName) ->
    create_db(DbName, []).

create_db(DbName, Opts) ->
    {ok, Db} = couch_db:create(DbName, Opts),
    ok = couch_db:close(Db),
    true.

delete_db(DbName) ->
    ok = couch_server:delete(DbName, []),
    true.
