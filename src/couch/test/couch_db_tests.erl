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



create_delete_db_test_()->
    {
        "Database create/delete tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            fun(_) ->
                [should_create_db(),
                 should_delete_db()]
            end
        }
    }.

create_delete_multiple_dbs_test_()->
    {
        "Multiple database create/delete tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            fun(_) ->
                [should_create_multiple_dbs(),
                 should_delete_multiple_dbs()]
            end
        }
    }.

create_delete_database_continuously_test_() ->
    {
        "Continious database create/delete tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            fun(_) ->
                [should_create_delete_database_continuously()]
            end
        }
    }.

open_db_test_()->
    {
        "Database open tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            fun(_) ->
                [should_create_db_if_missing()]
            end
        }
    }.


should_create_db() ->
    ?_test(begin
        DbName = ?tempdb(),
        {ok, Before} = couch_server:all_databases(),
        ?assertNot(lists:member(DbName, Before)),
        {ok, Db} = couch_db:create(DbName, []),
        ok = couch_db:close(Db),
        {ok, After} = couch_server:all_databases(),
        ?assert(lists:member(DbName, After))
    end).

should_delete_db() ->
    ?_test(begin
        DbName = ?tempdb(),
        couch_db:create(DbName, []),
        {ok, Before} = couch_server:all_databases(),
        ?assert(lists:member(DbName, Before)),
        couch_server:delete(DbName, []),
        {ok, After} = couch_server:all_databases(),
        ?assertNot(lists:member(DbName, After))
    end).

should_create_multiple_dbs() ->
    ?_test(begin
        gen_server:call(couch_server, {set_max_dbs_open, 3}),

        DbNames = [?tempdb() || _ <- lists:seq(1, 6)],
        {ok, Before} = couch_server:all_databases(),
        [?assertNot(lists:member(DbName, Before))  || DbName <- DbNames],

        lists:foreach(fun(DbName) ->
            {ok, Db} = couch_db:create(DbName, []),
            ok = couch_db:close(Db)
        end, DbNames),

        {ok, After} = couch_server:all_databases(),
        [?assert(lists:member(DbName, After)) || DbName <- DbNames]
    end).

should_delete_multiple_dbs() ->
    ?_test(begin
        DbNames = [?tempdb() || _ <- lists:seq(1, 6)],
        lists:foreach(fun(DbName) ->
            {ok, Db} = couch_db:create(DbName, []),
            ok = couch_db:close(Db)
        end, DbNames),

        lists:foreach(fun(DbName) ->
            ok = couch_server:delete(DbName, [])
        end, DbNames),

        {ok, AllDbs} = couch_server:all_databases(),
        NumDeleted = lists:foldl(fun(DbName, Acc) ->
            ?assertNot(lists:member(DbName, AllDbs)),
            Acc + 1
        end, 0, DbNames),

        ?assertEqual(NumDeleted, 6)
    end).

should_create_delete_database_continuously() ->
    ?_test(begin
        DbName = ?tempdb(),
        {ok, Db} = couch_db:create(DbName, []),
        couch_db:close(Db),
        [{timeout, ?TIMEOUT, {integer_to_list(N) ++ " times",
                               ?assert(loop(DbName, N))}}
         || N <- [10, 100]]
    end).

should_create_db_if_missing() ->
    ?_test(begin
        DbName = ?tempdb(),
        {ok, Db} = couch_db:open(DbName, [{create_if_missing, true}]),
        ok = couch_db:close(Db),
        {ok, AllDbs} = couch_server:all_databases(),
        ?assert(lists:member(DbName, AllDbs))
    end).

loop(_, 0) ->
    true;
loop(DbName, N) ->
    ok = cycle(DbName),
    loop(DbName, N - 1).

cycle(DbName) ->
    ok = couch_server:delete(DbName, []),
    {ok, Db} = couch_db:create(DbName, []),
    couch_db:close(Db),
    ok.
