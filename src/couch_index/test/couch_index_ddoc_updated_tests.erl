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

-module(couch_index_ddoc_updated_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


start() ->
    fake_index(),
    Ctx = test_util:start_couch([mem3, fabric]),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [?ADMIN_CTX]),
    {Ctx, DbName}.


stop({Ctx, DbName}) ->
    (catch meck:unload(test_index)),
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]),
    DbDir = config:get("couchdb", "database_dir", "."),
    WaitFun = fun() ->
        filelib:fold_files(DbDir, <<".*", DbName/binary, "\.[0-9]+.*">>,
            true, fun(_F, _A) -> wait end, ok)
    end,
    ok = test_util:wait(WaitFun),
    test_util:stop_couch(Ctx),
    ok.


ddoc_update_test_() ->
    {
        "Check ddoc update actions",
        {
            setup,
            fun start/0, fun stop/1,
            fun check_all_indexers_exit_on_ddoc_change/1
        }
    }.


check_all_indexers_exit_on_ddoc_change({_Ctx, DbName}) ->
    ?_test(begin
        [DbShard1 | RestDbShards] = lists:map(fun(Sh) ->
           {ok, ShardDb} = couch_db:open(mem3:name(Sh), []),
            ShardDb
        end, mem3:local_shards(mem3:dbname(DbName))),

        % create a DDoc on Db1
        DDocID = <<"idx_name">>,
        DDocJson = couch_doc:from_json_obj({[
           {<<"_id">>, DDocID},
           {<<"value">>, 1}
        ]}),
        {ok, _Rev} = couch_db:update_doc(DbShard1, DDocJson, []),
        {ok, DbShard} = couch_db:reopen(DbShard1),
        {ok, DDoc} = couch_db:open_doc(
            DbShard, DDocID, [ejson_body, ?ADMIN_CTX]),
        DbShards = [DbShard | RestDbShards],
        N = length(DbShards),

        % run couch_index process for each shard database
        ok = meck:reset(test_index),
        lists:foreach(fun(ShardDb) ->
            couch_index_server:get_index(test_index, ShardDb, DDoc)
        end, DbShards),

        IndexesBefore = get_indexes_by_ddoc(DDocID, N),
        ?assertEqual(N, length(IndexesBefore)),

        AliveBefore = lists:filter(fun erlang:is_process_alive/1, IndexesBefore),
        ?assertEqual(N, length(AliveBefore)),

        % update ddoc
        DDocJson2 = couch_doc:from_json_obj({[
            {<<"_id">>, DDocID},
            {<<"value">>, 2},
            {<<"_rev">>, couch_doc:rev_to_str(DDoc#doc.revs)}
        ]}),
        {ok, _} = couch_db:update_doc(DbShard, DDocJson2, []),

        % assert that all index processes exit after ddoc updated
        ok = meck:reset(test_index),
        couch_index_server:handle_db_event(
            DbShard#db.name, {ddoc_updated, DDocID}, {st, ""}),

        ok = meck:wait(N, test_index, init, ['_', '_'], 5000),
        IndexesAfter = get_indexes_by_ddoc(DDocID, 0),
        ?assertEqual(0, length(IndexesAfter)),

        %% assert that previously running indexes are gone
        AliveAfter = lists:filter(fun erlang:is_process_alive/1, IndexesBefore),
        ?assertEqual(0, length(AliveAfter)),
        ok
    end).


fake_index() ->
    ok = meck:new([test_index], [non_strict]),
    ok = meck:expect(test_index, init, fun(Db, DDoc) ->
        {ok, {couch_db:name(Db), DDoc}}
    end),
    ok = meck:expect(test_index, open, fun(_Db, State) ->
        {ok, State}
    end),
    ok = meck:expect(test_index, get, fun
        (db_name, {DbName, _DDoc}) ->
            DbName;
        (idx_name, {_DbName, DDoc}) ->
            DDoc#doc.id;
        (signature, {_DbName, DDoc}) ->
            crypto:hash(md5, term_to_binary(DDoc));
        (update_seq, Seq) ->
            Seq
    end).


get_indexes_by_ddoc(DDocID, N) ->
    Indexes = test_util:wait(fun() ->
        Indxs = ets:match_object(
            couchdb_indexes_by_db, {'$1', {DDocID, '$2'}}),
        case length(Indxs) == N of
            true ->
                Indxs;
            false ->
                wait
        end
    end),
    lists:foldl(fun({DbName, {_DDocID, Sig}}, Acc) ->
        case ets:lookup(couchdb_indexes_by_sig, {DbName, Sig}) of
            [{_, Pid}] -> [Pid|Acc];
            _ -> Acc
        end
    end, [], Indexes).

