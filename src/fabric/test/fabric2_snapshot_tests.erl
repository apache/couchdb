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

-module(fabric2_snapshot_tests).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fabric2.hrl").
-include("fabric2_test.hrl").

fdb_ss_test_() ->
    {
        "Test snapshot usage",
        setup,
        fun setup/0,
        fun cleanup/1,
        with([
            ?TDEF(retry_without_snapshot),
            ?TDEF(no_retry_with_snapshot)
        ])
    }.

setup() ->
    Ctx = test_util:start_couch([fabric]),
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    {Db, Ctx}.

cleanup({Db, Ctx}) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []),
    test_util:stop_couch(Ctx).

retry_without_snapshot({Db, _}) ->
    DbName = fabric2_db:name(Db),
    put(retry_count, 0),
    erase(conflict_pid),
    InitDbSeq = fabric2_db:get_update_seq(Db),
    DbSeq = fabric2_fdb:transactional(Db, fun(TxDb) ->
        put(retry_count, get(retry_count) + 1),

        % Fetch the update_seq
        Seq = fabric2_db:get_update_seq(TxDb),

        % Generate a no-op write so that we don't hit the
        % optimization to skip commits on read-only
        % transactions
        bump_view_size(TxDb),

        % Generate a conflicting transaction while
        % we're not yet committed
        case get(conflict_pid) of
            undefined ->
                {Pid, Ref} = spawn_monitor(fun() -> generate_conflict(DbName) end),
                receive
                    {'DOWN', Ref, _, _, normal} -> ok
                end,
                put(conflict_pid, Pid);
            Pid when is_pid(Pid) ->
                ok
        end,

        Seq
    end),

    ?assertEqual(2, get(retry_count)),
    ?assertNotEqual(InitDbSeq, DbSeq).

no_retry_with_snapshot({Db, _}) ->
    DbName = fabric2_db:name(Db),
    put(retry_count, 0),
    erase(conflict_pid),
    InitDbSeq = fabric2_db:get_update_seq(Db),
    DbSeq = fabric2_fdb:transactional(Db, fun(TxDb) ->
        put(retry_count, get(retry_count) + 1),

        % Fetch the update_seq
        Seq = fabric2_fdb:with_snapshot(TxDb, fun(SSDb) ->
            fabric2_db:get_update_seq(SSDb)
        end),

        % Generate a no-op write so that we don't hit the
        % optimization to skip commits on read-only
        % transactions
        bump_view_size(TxDb),

        % Generate a conflicting transaction while
        % we're not yet committed
        case get(conflict_pid) of
            undefined ->
                {Pid, Ref} = spawn_monitor(fun() -> generate_conflict(DbName) end),
                receive
                    {'DOWN', Ref, _, _, normal} -> ok
                end,
                put(conflict_pid, Pid);
            Pid when is_pid(Pid) ->
                ok
        end,

        Seq
    end),

    ?assertEqual(1, get(retry_count)),
    ?assertEqual(InitDbSeq, DbSeq).

bump_view_size(TxDb) ->
    #{
        tx := Tx,
        db_prefix := DbPrefix
    } = TxDb,

    DbTuple = {?DB_STATS, <<"sizes">>, <<"views">>},
    DbKey = erlfdb_tuple:pack(DbTuple, DbPrefix),
    erlfdb:add(Tx, DbKey, 0).

generate_conflict(DbName) ->
    {ok, Db} = fabric2_db:open(DbName, [{user_ctx, ?ADMIN_USER}]),
    Doc = #doc{
        id = fabric2_util:uuid(),
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc).
