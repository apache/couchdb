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

-module(couch_mrview_cleanup_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TEST_INDEX, test_index).
-define(DDOC_ID, <<"idx_name">>).

start() ->
    fake_index(),
    Ctx = test_util:start_couch([mem3, fabric]),
    config:set("couchdb", "index_cleanup_delay_msec", "60000", false),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [?ADMIN_CTX]),
    {Ctx, DbName}.

stop({Ctx, DbName}) ->
    meck:unload(?TEST_INDEX),
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]),
    DbDir = config:get("couchdb", "database_dir", "."),
    WaitFun = fun() ->
        filelib:fold_files(
            DbDir,
            <<".*", DbName/binary, "\.[0-9]+.*">>,
            true,
            fun(_F, _A) -> wait end,
            ok
        )
    end,
    ok = test_util:wait(WaitFun),
    config:delete("couchdb", "index_cleanup_delay_msec", false),
    test_util:stop_couch(Ctx),
    ok.

cleanup_test_() ->
    {
        "couch_mrview_cleanup",
        {
            foreach,
            fun start/0,
            fun stop/1,
            [
                ?TDEF_FE(t_orphan_sigs_are_reaped),
                ?TDEF_FE(t_valid_sigs_survive),
                ?TDEF_FE(t_shared_sig_drops_stale_ddoc_row),
                ?TDEF_FE(t_schedule_dedupes_within_window)
            ]
        }
    }.

% Sig is now invalid, reap it!
t_orphan_sigs_are_reaped({_Ctx, DbName}) ->
    [DbShard1 | RestDbShards] = open_shards(DbName),
    {DDoc, DbShard} = create_ddoc(DbShard1, ?DDOC_ID),
    DbShards = [DbShard | RestDbShards],
    N = length(DbShards),
    spawn_indexers(DbShards, DDoc),
    IndexesBefore = get_indexes_by_ddoc(?DDOC_ID, N),
    ?assertEqual(N, length(IndexesBefore)),

    ok = meck:reset(?TEST_INDEX),
    lists:foreach(
        fun(DbShardName) ->
            couch_mrview_cleanup:cleanup_processes(DbShardName, #{})
        end,
        [couch_db:name(S) || S <- DbShards]
    ),

    wait_until_dead(IndexesBefore),
    ?assertEqual(0, length(get_indexes_by_ddoc(?DDOC_ID, 0))),
    ?assertEqual(0, length(lists:filter(fun is_process_alive/1, IndexesBefore))).

% Sig is valid, leave it alone
t_valid_sigs_survive({_Ctx, DbName}) ->
    [DbShard1 | RestDbShards] = open_shards(DbName),
    {DDoc, DbShard} = create_ddoc(DbShard1, ?DDOC_ID),
    DbShards = [DbShard | RestDbShards],
    N = length(DbShards),
    spawn_indexers(DbShards, DDoc),
    IndexesBefore = get_indexes_by_ddoc(?DDOC_ID, N),
    ?assertEqual(N, length(IndexesBefore)),

    % All test indexes share the same sig (as mocked)
    [{_, {_, RawSig}} | _] = ets:match_object(
        couch_index_server:by_db(couch_db:name(DbShard)),
        {couch_db:name(DbShard), {?DDOC_ID, '$1'}}
    ),
    ValidSigs = #{couch_util:to_hex_bin(RawSig) => #{?DDOC_ID => true}},

    lists:foreach(
        fun(DbShardName) ->
            couch_mrview_cleanup:cleanup_processes(DbShardName, ValidSigs)
        end,
        [couch_db:name(S) || S <- DbShards]
    ),

    timer:sleep(100),
    ?assertEqual(N, length(get_indexes_by_ddoc(?DDOC_ID, N))),
    ?assertEqual(N, length(lists:filter(fun is_process_alive/1, IndexesBefore))).

% Two ddocs share the same sig. Indexer has to stay alive old ddoc removed
t_shared_sig_drops_stale_ddoc_row({_Ctx, DbName}) ->
    [DbShard1 | RestDbShards] = open_shards(DbName),
    {DDoc, DbShard} = create_ddoc(DbShard1, ?DDOC_ID),
    DbShards = [DbShard | RestDbShards],
    N = length(DbShards),
    spawn_indexers(DbShards, DDoc),
    IndexesBefore = get_indexes_by_ddoc(?DDOC_ID, N),
    ?assertEqual(N, length(IndexesBefore)),

    [{_, {_, RawSig}} | _] = ets:match_object(
        couch_index_server:by_db(couch_db:name(DbShard)),
        {couch_db:name(DbShard), {?DDOC_ID, '$1'}}
    ),
    OtherDDocId = <<"some_other_ddoc">>,
    ValidSigs = #{couch_util:to_hex_bin(RawSig) => #{OtherDDocId => true}},

    lists:foreach(
        fun(DbShardName) ->
            couch_mrview_cleanup:cleanup_processes(DbShardName, ValidSigs)
        end,
        [couch_db:name(S) || S <- DbShards]
    ),

    test_util:wait(fun() ->
        Stale = lists:flatmap(
            fun(I) ->
                ets:match_object(
                    couch_index_server:by_db(I), {'$1', {?DDOC_ID, '$2'}}
                )
            end,
            seq()
        ),
        case Stale of
            [] -> ok;
            _ -> wait
        end
    end),
    ?assertEqual(N, length(lists:filter(fun is_process_alive/1, IndexesBefore))).

% Three schedule calls should dedup
t_schedule_dedupes_within_window({_Ctx, DbName}) ->
    ClusteredDbName = mem3:dbname(DbName),
    ok = couch_index_cleanup:schedule(ClusteredDbName),
    ok = couch_index_cleanup:schedule(ClusteredDbName),
    ok = couch_index_cleanup:schedule(ClusteredDbName),
    ?assertEqual([ClusteredDbName], pending_dbnames()).

% Helpers. Some copied from other tests

open_shards(DbName) ->
    lists:map(
        fun(Sh) ->
            {ok, ShardDb} = couch_db:open(mem3:name(Sh), []),
            ShardDb
        end,
        mem3:local_shards(mem3:dbname(DbName))
    ).

create_ddoc(Db, DDocID) ->
    DDocJson = couch_doc:from_json_obj(
        {[
            {<<"_id">>, DDocID},
            {<<"value">>, 1}
        ]}
    ),
    {ok, _Rev} = couch_db:update_doc(Db, DDocJson, []),
    {ok, Db1} = couch_db:reopen(Db),
    {ok, DDoc} = couch_db:open_doc(Db1, DDocID, [ejson_body, ?ADMIN_CTX]),
    {DDoc, Db1}.

spawn_indexers(DbShards, DDoc) ->
    ok = meck:reset(?TEST_INDEX),
    lists:foreach(
        fun(ShardDb) ->
            couch_index_server:get_index(?TEST_INDEX, ShardDb, DDoc)
        end,
        DbShards
    ).

fake_index() ->
    ok = meck:new([?TEST_INDEX], [non_strict]),
    ok = meck:expect(?TEST_INDEX, init, fun(Db, DDoc) ->
        {ok, {couch_db:name(Db), DDoc}}
    end),
    ok = meck:expect(?TEST_INDEX, open, fun(_Db, State) ->
        {ok, State}
    end),
    ok = meck:expect(?TEST_INDEX, get, fun
        (db_name, {DbName, _DDoc}) ->
            DbName;
        (idx_name, {_DbName, DDoc}) ->
            DDoc#doc.id;
        (signature, {_DbName, DDoc}) ->
            couch_hash:md5_hash(term_to_binary(DDoc));
        (update_seq, Seq) ->
            Seq
    end),
    ok = meck:expect(?TEST_INDEX, shutdown, ['_'], ok).

get_indexes_by_ddoc(DDocID, N) ->
    Indexes = test_util:wait(fun() ->
        Indxs = lists:flatmap(
            fun(I) ->
                ets:match_object(
                    couch_index_server:by_db(I), {'$1', {DDocID, '$2'}}
                )
            end,
            seq()
        ),
        case length(Indxs) == N of
            true -> Indxs;
            false -> wait
        end
    end),
    lists:foldl(
        fun({DbName, {_DDocID, Sig}}, Acc) ->
            case ets:lookup(couch_index_server:by_sig(DbName), {DbName, Sig}) of
                [{_, Pid}] -> [Pid | Acc];
                _ -> Acc
            end
        end,
        [],
        Indexes
    ).

wait_until_dead(Pids) ->
    test_util:wait(fun() ->
        case lists:filter(fun is_process_alive/1, Pids) of
            [] -> ok;
            _ -> wait
        end
    end).

pending_dbnames() ->
    {st, Pending} = sys:get_state(couch_index_cleanup),
    lists:sort(maps:keys(Pending)).

seq() ->
    lists:seq(1, couch_index_server:num_servers()).
