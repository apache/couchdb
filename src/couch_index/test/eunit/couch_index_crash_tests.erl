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

-module(couch_index_crash_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

start() ->
    failing_index(),
    Ctx = test_util:start_couch([mem3, fabric]),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [?ADMIN_CTX]),
    {Ctx, DbName}.

stop({Ctx, DbName}) ->
    meck:unload(test_index),
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
    test_util:stop_couch(Ctx),
    ok.

db_event_crash_test() ->
    St =
        {st, "", couch_index_server:server_name(1), couch_index_server:by_sig(1),
            couch_index_server:by_pid(1), couch_index_server:by_db(1),
            couch_index_server:openers(1)},
    %% Assert that we get back what we sent in, and implicitly didn't crash instead.
    ?assertEqual(
        {ok, St},
        couch_index_server:handle_db_event(
            <<"shards/fake">>, {ddoc_updated, <<"fakeddoc">>}, St
        )
    ).

index_crashes_while_opening_test_() ->
    {
        "Simulate index crashing during open",
        {
            setup,
            fun start/0,
            fun stop/1,
            fun crash_index_while_opening/1
        }
    }.

crash_index_while_opening({_Ctx, DbName}) ->
    ?_test(begin
        [DbShard1 | RestDbShards] = lists:map(
            fun(Sh) ->
                {ok, ShardDb} = couch_db:open(mem3:name(Sh), []),
                ShardDb
            end,
            mem3:local_shards(mem3:dbname(DbName))
        ),

        % create a DDoc on Db1
        DDocID = <<"idx_name">>,
        DDocJson = couch_doc:from_json_obj(
            {[
                {<<"_id">>, DDocID},
                {<<"value">>, 1}
            ]}
        ),
        {ok, _Rev} = couch_db:update_doc(DbShard1, DDocJson, []),
        {ok, DbShard} = couch_db:reopen(DbShard1),
        {ok, DDoc} = couch_db:open_doc(
            DbShard, DDocID, [ejson_body, ?ADMIN_CTX]
        ),
        DbShards = [DbShard | RestDbShards],

        %% fetch the index and confirm it fails
        lists:foreach(
            fun(ShardDb) ->
                ?assertMatch({ok, _}, couch_index_server:get_index(test_index, ShardDb, DDoc))
            end,
            DbShards
        ),

        %% assert openers ETS table is empty
        lists:foreach(
            fun(I) -> ?assertEqual([], ets:tab2list(couch_index_server:openers(I))) end,
            seq()
        ),
        ok
    end).

failing_index() ->
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
            couch_hash:md5_hash(term_to_binary(DDoc));
        (update_seq, Seq) ->
            Seq
    end),
    ok = meck:expect(test_index, shutdown, ['_'], ok).

seq() ->
    lists:seq(1, couch_index_server:num_servers()).
