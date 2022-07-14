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

-module(couch_bt_engine_compactor_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DELAY, 100).
-define(WAIT_DELAY_COUNT, 50).

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    create_docs(DbName),
    DbName.

teardown(DbName) when is_binary(DbName) ->
    meck:unload(),
    couch_server:delete(DbName, [?ADMIN_CTX]),
    ok.

basic_compaction_test_() ->
    {
        setup,
        fun test_util:start_couch/0,
        fun test_util:stop_couch/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                fun compaction_resume/1,
                fun is_compacting_works/1
            ]
        }
    }.

compaction_resume(DbName) ->
    ?_test(begin
        check_db_validity(DbName),
        compact_db(DbName),
        check_db_validity(DbName),

        % Force an error when copying document ids
        with_mecked_emsort(fun() ->
            compact_db(DbName)
        end),

        check_db_validity(DbName),
        compact_db(DbName),
        check_db_validity(DbName)
    end).

is_compacting_works(DbName) ->
    ?_test(begin
        check_db_validity(DbName),
        meck:new(couch_emsort, [passthrough]),
        ok = wait_in_emsort_bind(self()),
        {_Pid, Ref} = spawn_monitor(fun() -> compact_db(DbName) end),
        receive
            {in_emsort_bind, From} ->
                % When emsort:open(Fd) is called the files should
                % have been created already
                ?assert(couch_db:is_compacting(DbName)),
                From ! {please_continue, self()}
        end,
        receive
            {'DOWN', Ref, _, _, _} -> ok
        end,
        meck:unload(couch_emsort)
    end).

check_db_validity(DbName) ->
    couch_util:with_db(DbName, fun(Db) ->
        ?assertEqual({ok, 3}, couch_db:get_doc_count(Db)),
        ?assertEqual(3, couch_db:count_changes_since(Db, 0))
    end).

with_mecked_emsort(Fun) ->
    meck:new(couch_emsort, [passthrough]),
    meck:expect(couch_emsort, iter, fun(_) -> erlang:error(kaboom) end),
    try
        Fun()
    after
        meck:unload()
    end.

create_docs(DbName) ->
    couch_util:with_db(DbName, fun(Db) ->
        Doc1 = couch_doc:from_json_obj(
            {[
                {<<"_id">>, <<"doc1">>},
                {<<"value">>, 1}
            ]}
        ),
        Doc2 = couch_doc:from_json_obj(
            {[
                {<<"_id">>, <<"doc2">>},
                {<<"value">>, 2}
            ]}
        ),
        Doc3 = couch_doc:from_json_obj(
            {[
                {<<"_id">>, <<"doc3">>},
                {<<"value">>, 3}
            ]}
        ),
        {ok, _} = couch_db:update_docs(Db, [Doc1, Doc2, Doc3])
    end).

compact_db(DbName) ->
    couch_util:with_db(DbName, fun(Db) ->
        {ok, _} = couch_db:start_compact(Db)
    end),
    wait_db_compact_done(DbName, ?WAIT_DELAY_COUNT).

wait_db_compact_done(_DbName, 0) ->
    Failure = [
        {module, ?MODULE},
        {line, ?LINE},
        {reason, "DB compaction failed to finish"}
    ],
    erlang:error({assertion_failed, Failure});
wait_db_compact_done(DbName, N) ->
    IsDone = couch_util:with_db(DbName, fun(Db) ->
        not is_pid(couch_db:get_compactor_pid(Db))
    end),
    if
        IsDone ->
            ok;
        true ->
            timer:sleep(?DELAY),
            wait_db_compact_done(DbName, N - 1)
    end.

wait_in_emsort_bind(WaitingPid) when is_pid(WaitingPid) ->
    meck:expect(couch_emsort, open, fun(Fd) ->
        WaitingPid ! {in_emsort_bind, self()},
        receive
            {please_continue, WaitingPid} ->
                ok
        end,
        meck:passthrough([Fd])
    end).
