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

-define(TEST_INDEX, test_index).

start() ->
    meck:new(couch_index_server, [passthrough]),
    meck:new(couch_index, [passthrough]),
    Ctx = test_util:start_couch([mem3, fabric]),
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [?ADMIN_CTX, {q, 1}, {n, 1}]),
    {Ctx, DbName}.

stop({Ctx, DbName}) ->
    meck:unload(),
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
    % mock st record from couch_index_server
    St = {
        st,
        "",
        couch_index_server:server_name(1),
        couch_index_server:by_sig(1),
        couch_index_server:by_pid(1),
        couch_index_server:by_db(1),
        couch_index_server:openers(1)
    },
    %% Assert that we get back what we sent in, and implicitly didn't crash instead.
    ?assertEqual(
        {ok, St},
        couch_index_server:handle_db_event(
            <<"shards/fake">>, {ddoc_updated, <<"fakeddoc">>}, St
        )
    ).

index_crash_test_() ->
    {
        "Simulate index crashing",
        {
            foreach,
            fun start/0,
            fun stop/1,
            [
                ?TDEF_FE(t_can_open_mock_index),
                ?TDEF_FE(t_index_open_returns_error),
                ?TDEF_FE(t_index_open_raises_error),
                ?TDEF_FE(t_index_open_exits_with_error),
                ?TDEF_FE(t_index_process_dies)
            ]
        }
    }.

index_crash_test_with_client_waiting_test_() ->
    {
        "Simulate index crashing while clients are waiting",
        {
            foreach,
            fun start/0,
            fun stop/1,
            [
                ?TDEF_FE(t_index_open_with_clients_waiting),
                ?TDEF_FE(t_index_open_crashes_with_client_waiting),
                ?TDEF_FE(t_index_open_returns_error_with_client_waiting)
            ]
        }
    }.

t_can_open_mock_index({_Ctx, DbName}) ->
    failing_index(dontfail),

    [DbShard1] = open_shards(DbName),

    % create a DDoc on Db1
    {DDoc, DbShard} = create_ddoc(DbShard1, <<"idx_name">>),

    meck:reset(couch_index_server),
    %% fetch the fake index works
    ?assertMatch({ok, _}, get_index(DbShard, DDoc)),

    %% assert openers ETS table is empty
    lists:foreach(fun(I) -> ?assertEqual([], openers(I)) end, seq()),

    ?assert(meck:called(couch_index_server, handle_call, [{async_open, '_', '_'}, '_', '_'])).

t_index_open_returns_error({_Ctx, DbName}) ->
    failing_index({return, idxerr}),

    [DbShard1] = open_shards(DbName),

    % create a DDoc on Db1
    {DDoc, DbShard} = create_ddoc(DbShard1, <<"idx_name">>),

    meck:reset(couch_index_server),
    %% fetch the index and confirm it fails
    ?assertEqual({error, idxerr}, get_index(DbShard, DDoc)),

    %% assert openers ETS table is empty
    lists:foreach(fun(I) -> ?assertEqual([], openers(I)) end, seq()),

    ?assert(meck:called(couch_index_server, handle_call, [{async_error, '_', '_'}, '_', '_'])).

t_index_open_raises_error({_Ctx, DbName}) ->
    failing_index({raise, idxerr}),

    [DbShard1] = open_shards(DbName),

    % create a DDoc on Db1
    {DDoc, DbShard} = create_ddoc(DbShard1, <<"idx_name">>),

    meck:reset(couch_index_server),
    %% fetch the index and confirm it fails
    ?assertEqual({meck_raise, error, idxerr}, get_index(DbShard, DDoc)),

    %% assert openers ETS table is empty
    lists:foreach(fun(I) -> ?assertEqual([], openers(I)) end, seq()),

    ?assert(meck:called(couch_index_server, handle_call, [{async_error, '_', '_'}, '_', '_'])).

t_index_open_exits_with_error({_Ctx, DbName}) ->
    failing_index({exit, idxerr}),

    [DbShard1] = open_shards(DbName),

    % create a DDoc on Db1
    {DDoc, DbShard} = create_ddoc(DbShard1, <<"idx_name">>),

    meck:reset(couch_index_server),
    %% fetch the index and confirm it fails
    ?assertEqual({meck_raise, exit, idxerr}, get_index(DbShard, DDoc)),

    %% assert openers ETS table is empty
    lists:foreach(fun(I) -> ?assertEqual([], openers(I)) end, seq()),

    ?assert(meck:called(couch_index_server, handle_call, [{async_error, '_', '_'}, '_', '_'])).

t_index_process_dies({_Ctx, DbName}) ->
    failing_index(dontfail),

    [DbShard1] = open_shards(DbName),

    % create a DDoc on Db1
    {DDoc, DbShard} = create_ddoc(DbShard1, <<"idx_name">>),

    meck:reset(couch_index_server),
    {ok, IdxPid} = get_index(DbShard, DDoc),
    ?assert(is_pid(IdxPid)),

    % Save index servers before so we can assert a dying index won't take any
    % of them down.
    ServerPids = lists:sort([whereis(N) || N <- couch_index_server:names()]),

    meck:reset(couch_index_server),
    exit(IdxPid, boom),
    meck:wait(couch_index_server, handle_info, [{'EXIT', IdxPid, boom}, '_'], 1000),

    {ok, IdxPid2} = get_index(DbShard, DDoc),
    ?assert(is_pid(IdxPid2)),

    % Same index servers are still up
    ServerPids2 = lists:sort([whereis(N) || N <- couch_index_server:names()]),
    ?assertEqual(ServerPids, ServerPids2).

t_index_open_with_clients_waiting({_Ctx, DbName}) ->
    failing_index({blockopen, self()}),

    [DbShard1] = open_shards(DbName),

    % create a DDoc on Db1
    {DDoc, DbShard} = create_ddoc(DbShard1, <<"idx_name">>),

    meck:reset(couch_index_server),
    {_, CRef1} = spawn_client(DbShard, DDoc),
    BlockPid =
        receive
            {blockopen, Pid} -> Pid
        end,
    {_, CRef2} = spawn_client(DbShard, DDoc),

    % Clients are waiting for the response
    receive
        {'DOWN', CRef1, _, _, _} ->
            ?assert(false, "should not have received a response yet");
        {'DOWN', CRef2, _, _, _} ->
            ?assert(false, "should not have received a response yet")
    after 500 ->
        ?assert(true)
    end,

    BlockPid ! continue,

    {ok, IdxPid1} = wait_client_or_fail(CRef1),
    {ok, IdxPid2} = wait_client_or_fail(CRef2),
    ?assert(is_pid(IdxPid1)),
    ?assert(is_pid(IdxPid2)),
    ?assertEqual(IdxPid1, IdxPid2),

    ?assertEqual(0, couch_index_server:aggregate_queue_len()),

    %% assert opener ets table is empty
    lists:foreach(fun(I) -> ?assertEqual([], openers(I)) end, seq()),

    ?assert(meck:called(couch_index_server, handle_call, [{async_open, '_', '_'}, '_', '_'])),

    % here we should get the pid from the ets table
    ?assertMatch({ok, _}, get_index(DbShard, DDoc)),
    {ok, IdxPid3} = get_index(DbShard, DDoc),
    ?assertEqual(IdxPid1, IdxPid3).

t_index_open_crashes_with_client_waiting({_Ctx, DbName}) ->
    failing_index({blockopen, self()}),

    [DbShard1] = open_shards(DbName),

    % create a DDoc on Db1
    {DDoc, DbShard} = create_ddoc(DbShard1, <<"idx_name">>),

    meck:reset(couch_index_server),
    {_, CRef1} = spawn_client(DbShard, DDoc),
    BlockPid =
        receive
            {blockopen, Pid} -> Pid
        end,
    {_, CRef2} = spawn_client(DbShard, DDoc),

    % Clients are waiting for the response
    receive
        {'DOWN', CRef1, _, _, _} ->
            ?assert(false, "should not have received a response yet");
        {'DOWN', CRef2, _, _, _} ->
            ?assert(false, "should not have received a response yet")
    after 500 ->
        ?assert(true)
    end,

    exit(BlockPid, kill),

    Res1 = wait_client_or_fail(CRef1),
    Res2 = wait_client_or_fail(CRef2),
    ?assertMatch(killed, Res1),
    ?assertMatch(killed, Res2),

    ?assertEqual(0, couch_index_server:aggregate_queue_len()),

    %% assert ETS tables are empty
    lists:foreach(
        fun(I) ->
            ?assertEqual([], openers(I)),
            ?assertEqual([], by_db(I)),
            ?assertEqual([], by_sig(I))
        end,
        seq()
    ).

t_index_open_returns_error_with_client_waiting({_Ctx, DbName}) ->
    failing_index({blockopen, self()}),

    [DbShard1] = open_shards(DbName),

    % create a DDoc on Db1
    {DDoc, DbShard} = create_ddoc(DbShard1, <<"idx_name">>),

    meck:reset(couch_index_server),
    {_, CRef1} = spawn_client(DbShard, DDoc),
    BlockPid =
        receive
            {blockopen, Pid} -> Pid
        end,
    {_, CRef2} = spawn_client(DbShard, DDoc),

    % Clients are waiting for the response
    receive
        {'DOWN', CRef1, _, _, _} ->
            ?assert(false, "should not have received a response yet");
        {'DOWN', CRef2, _, _, _} ->
            ?assert(false, "should not have received a response yet")
    after 500 ->
        ?assert(true)
    end,

    BlockPid ! {return, retfail},

    Res1 = wait_client_or_fail(CRef1),
    Res2 = wait_client_or_fail(CRef2),
    ?assertMatch(retfail, Res1),
    ?assertMatch(retfail, Res2),

    ?assertEqual(0, couch_index_server:aggregate_queue_len()),

    %% assert openers ETS table is empty
    lists:foreach(
        fun(I) ->
            ?assertEqual([], openers(I)),
            ?assertEqual([], by_db(I)),
            ?assertEqual([], by_sig(I))
        end,
        seq()
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

open_shards(DbName) ->
    lists:map(
        fun(Sh) ->
            {ok, ShardDb} = couch_db:open(mem3:name(Sh), []),
            ShardDb
        end,
        mem3:local_shards(mem3:dbname(DbName))
    ).

get_index(ShardDb, DDoc) ->
    couch_index_server:get_index(?TEST_INDEX, ShardDb, DDoc).

openers(I) ->
    ets:tab2list(couch_index_server:openers(I)).

by_db(I) ->
    ets:tab2list(couch_index_server:by_db(I)).

by_sig(I) ->
    ets:tab2list(couch_index_server:by_sig(I)).

failing_index(Error) ->
    ok = meck:new([?TEST_INDEX], [non_strict]),
    ok = meck:expect(?TEST_INDEX, init, fun(Db, DDoc) ->
        {ok, {couch_db:name(Db), DDoc}}
    end),
    ok = meck:expect(?TEST_INDEX, open, fun(_Db, State) ->
        case Error of
            dontfail ->
                {ok, State};
            {blockopen, ControllerPid} ->
                case block(blockopen, ControllerPid) of
                    {return, Err} ->
                        Err;
                    continue ->
                        {ok, State}
                end;
            {return, Err} ->
                {error, Err};
            {raise, Err} ->
                meck:raise(error, Err);
            {exit, Err} ->
                meck:raise(exit, Err)
        end
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

seq() ->
    lists:seq(1, couch_index_server:num_servers()).

block(BlockMsg, ControllerPid) when is_atom(BlockMsg), is_pid(ControllerPid) ->
    ControllerPid ! {BlockMsg, self()},
    receive
        continue ->
            continue;
        {return, Err} ->
            {return, Err};
        {raise, Err} ->
            meck:raise(error, Err);
        {exit, Err} ->
            meck:raise(exit, Err)
    end.

spawn_client(ShardDb, DDoc) ->
    spawn_monitor(fun() -> exit(get_index(ShardDb, DDoc)) end).

wait_client_or_fail(Ref) ->
    receive
        {'DOWN', Ref, _, _, Res} ->
            Res
    after 500 ->
        ?assert(false, "Failed to get index")
    end.
