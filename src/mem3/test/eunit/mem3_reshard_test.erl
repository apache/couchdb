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

-module(mem3_reshard_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/src/mem3_reshard.hrl").
% for all_docs function
-include_lib("couch_mrview/include/couch_mrview.hrl").

-define(ID, <<"_id">>).
-define(TIMEOUT, 60).

setup() ->
    HaveDreyfus = code:lib_dir(dreyfus) /= {error, bad_name},
    case HaveDreyfus of
        false -> ok;
        true -> mock_dreyfus_indices()
    end,

    HaveHastings = code:lib_dir(hastings) /= {error, bad_name},
    case HaveHastings of
        false -> ok;
        true -> mock_hastings_indices()
    end,
    {Db1, Db2} = {?tempdb(), ?tempdb()},
    create_db(Db1, [{q, 1}, {n, 1}]),
    PartProps = [{partitioned, true}, {hash, [couch_partition, hash, []]}],
    create_db(Db2, [{q, 1}, {n, 1}, {props, PartProps}]),
    config:set("reshard", "retry_interval_sec", "0", _Persist = false),
    #{db1 => Db1, db2 => Db2}.

teardown(#{} = Dbs) ->
    mem3_reshard:reset_state(),
    maps:map(fun(_, Db) -> delete_db(Db) end, Dbs),
    config:delete("reshard", "retry_interval_sec", _Persist = false),
    meck:unload().

start_couch() ->
    test_util:start_couch(?CONFIG_CHAIN, [mem3, fabric]).

stop_couch(Ctx) ->
    test_util:stop_couch(Ctx).

mem3_reshard_db_test_() ->
    {
        "mem3 shard split db tests",
        {
            setup,
            fun start_couch/0,
            fun stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun split_one_shard/1,
                    fun split_shard_with_lots_of_purges/1,
                    fun update_docs_before_topoff1/1,
                    fun indices_are_built/1,
                    fun split_partitioned_db/1,
                    fun split_twice/1,
                    fun couch_events_are_emitted/1,
                    fun retries_work/1,
                    fun target_reset_in_initial_copy/1,
                    fun split_an_incomplete_shard_map/1,
                    fun target_shards_are_locked/1
                ]
            }
        }
    }.

% This is a basic test to check that shard splitting preserves documents, and
% db meta props like revs limits and security.
split_one_shard(#{db1 := Db}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            DocSpec = #{docs => 10, delete => [5, 9], mrview => 1, local => 1},
            add_test_docs(Db, DocSpec),

            % Save documents before the split
            Docs0 = get_all_docs(Db),
            Local0 = get_local_docs(Db),

            % Set some custom metadata properties
            set_revs_limit(Db, 942),
            set_purge_infos_limit(Db, 943),
            SecObj = {[{<<"foo">>, <<"bar">>}]},
            set_security(Db, SecObj),

            % DbInfo is saved after setting metadata bits
            % as those could bump the update sequence
            DbInfo0 = get_db_info(Db),

            % Split the one shard
            [#shard{name = Shard}] = lists:sort(mem3:local_shards(Db)),
            {ok, JobId} = mem3_reshard:start_split_job(Shard),
            wait_state(JobId, completed),

            % Perform some basic checks that the shard was split
            Shards1 = lists:sort(mem3:local_shards(Db)),
            ?assertEqual(2, length(Shards1)),
            [#shard{range = R1}, #shard{range = R2}] = Shards1,
            ?assertEqual([16#00000000, 16#7fffffff], R1),
            ?assertEqual([16#80000000, 16#ffffffff], R2),

            % Check metadata bits after the split
            ?assertEqual(942, get_revs_limit(Db)),
            ?assertEqual(943, get_purge_infos_limit(Db)),
            ?assertEqual(SecObj, get_security(Db)),

            DbInfo1 = get_db_info(Db),
            Docs1 = get_all_docs(Db),
            Local1 = get_local_docs(Db),

            % When comparing db infos, ignore update sequences they won't be the
            % same since they are more shards involved after the split
            ?assertEqual(without_seqs(DbInfo0), without_seqs(DbInfo1)),

            % Update seq prefix number is a sum of all shard update sequences
            #{<<"update_seq">> := UpdateSeq0} = update_seq_to_num(DbInfo0),
            #{<<"update_seq">> := UpdateSeq1} = update_seq_to_num(DbInfo1),
            ?assertEqual(UpdateSeq0 * 2, UpdateSeq1),

            % Finally compare that the documents are still there after the split
            ?assertEqual(Docs0, Docs1),

            % Don't forget about the local but don't include internal checkpoints
            % as some of those are munged and transformed during the split
            ?assertEqual(without_meta_locals(Local0), without_meta_locals(Local1))
        end)}.

% Test to check that shard with high number of purges can be split
split_shard_with_lots_of_purges(#{db1 := Db}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            % Set a low purge infos limit, we are planning to overrun it
            set_purge_infos_limit(Db, 10),

            % Add docs 1..20 and purge them
            add_test_docs(Db, #{docs => [1, 20]}),
            IdRevs = maps:fold(
                fun(Id, #{<<"_rev">> := Rev}, Acc) ->
                    [{Id, [Rev]} | Acc]
                end,
                [],
                get_all_docs(Db)
            ),
            ?assertMatch({ok, _}, purge_docs(Db, IdRevs)),

            % Compact to trim the purge sequence
            ok = compact(Db),

            % Add some extra docs, these won't be purged
            add_test_docs(Db, #{docs => [21, 30]}),
            Docs0 = get_all_docs(Db),

            % Save db info before splitting
            DbInfo0 = get_db_info(Db),

            % Split the one shard
            [#shard{name = Shard}] = lists:sort(mem3:local_shards(Db)),
            {ok, JobId} = mem3_reshard:start_split_job(Shard),
            wait_state(JobId, completed),

            % Perform some basic checks that the shard was split
            Shards1 = lists:sort(mem3:local_shards(Db)),
            ?assertEqual(2, length(Shards1)),
            [#shard{range = R1}, #shard{range = R2}] = Shards1,
            ?assertEqual([16#00000000, 16#7fffffff], R1),
            ?assertEqual([16#80000000, 16#ffffffff], R2),

            % Check metadata bits after the split
            ?assertEqual(10, get_purge_infos_limit(Db)),

            DbInfo1 = get_db_info(Db),
            Docs1 = get_all_docs(Db),

            % When comparing db infos, ignore update sequences they won't be the
            % same since they are more shards involved after the split
            ?assertEqual(without_seqs(DbInfo0), without_seqs(DbInfo1)),

            % Finally compare that the documents are still there after the split
            ?assertEqual(Docs0, Docs1)
        end)}.

% This test checks that document added while the shard is being split are not
% lost. Topoff1 state happens before indices are built
update_docs_before_topoff1(#{db1 := Db}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            add_test_docs(Db, #{docs => 10}),

            intercept_state(topoff1),

            [#shard{name = Shard}] = lists:sort(mem3:local_shards(Db)),
            {ok, JobId} = mem3_reshard:start_split_job(Shard),

            receive
                {JobPid, topoff1} -> ok
            end,
            add_test_docs(Db, #{docs => [10, 19], local => 1}),
            Docs0 = get_all_docs(Db),
            Local0 = get_local_docs(Db),
            DbInfo0 = get_db_info(Db),
            JobPid ! continue,

            wait_state(JobId, completed),

            % Perform some basic checks that the shard was split
            Shards1 = lists:sort(mem3:local_shards(Db)),
            ?assertEqual(2, length(Shards1)),

            DbInfo1 = get_db_info(Db),
            Docs1 = get_all_docs(Db),
            Local1 = get_local_docs(Db),

            ?assertEqual(without_seqs(DbInfo0), without_seqs(DbInfo1)),

            % Update sequence after initial copy with 10 docs would be 10 on each
            % target shard (to match the source) and the total update sequence
            % would have been 20. But then 10 more docs were added (3 might have
            % ended up on one target and 7 on another) so the final update sequence
            % would then be 20 + 10 = 30.
            ?assertMatch(#{<<"update_seq">> := 30}, update_seq_to_num(DbInfo1)),

            ?assertEqual(Docs0, Docs1),
            ?assertEqual(without_meta_locals(Local0), without_meta_locals(Local1))
        end)}.

% This test that indices are built during shard splitting.
indices_are_built(#{db1 := Db}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            HaveDreyfus = code:lib_dir(dreyfus) /= {error, bad_name},
            HaveHastings = code:lib_dir(hastings) /= {error, bad_name},

            add_test_docs(Db, #{docs => 10, mrview => 2, search => 2, geo => 2}),
            [#shard{name = Shard}] = lists:sort(mem3:local_shards(Db)),
            {ok, JobId} = mem3_reshard:start_split_job(Shard),
            wait_state(JobId, completed),
            Shards1 = lists:sort(mem3:local_shards(Db)),
            ?assertEqual(2, length(Shards1)),
            MRViewGroupInfo = get_group_info(Db, <<"_design/mrview00000">>),
            ?assertMatch(#{<<"update_seq">> := 32}, MRViewGroupInfo),

            HaveDreyfus = code:lib_dir(dreyfus) /= {error, bad_name},
            case HaveDreyfus of
                false ->
                    ok;
                true ->
                    % 4 because there are 2 indices and 2 target shards
                    ?assertEqual(4, meck:num_calls(dreyfus_index, await, 2))
            end,

            HaveHastings = code:lib_dir(hastings) /= {error, bad_name},
            case HaveHastings of
                false ->
                    ok;
                true ->
                    % 4 because there are 2 indices and 2 target shards
                    ?assertEqual(4, meck:num_calls(hastings_index, await, 2))
            end
        end)}.

mock_dreyfus_indices() ->
    meck:expect(dreyfus_index, design_doc_to_indexes, fun(Doc) ->
        #doc{body = {BodyProps}} = Doc,
        case couch_util:get_value(<<"indexes">>, BodyProps) of
            undefined ->
                [];
            {[_]} ->
                [{dreyfus, <<"db">>, dreyfus_index1}]
        end
    end),
    meck:expect(dreyfus_index_manager, get_index, fun(_, _) -> {ok, pid} end),
    meck:expect(dreyfus_index, await, fun(_, _) -> ok end).

mock_hastings_indices() ->
    meck:expect(hastings_index, design_doc_to_indexes, fun(Doc) ->
        #doc{body = {BodyProps}} = Doc,
        case couch_util:get_value(<<"st_indexes">>, BodyProps) of
            undefined ->
                [];
            {[_]} ->
                [{hastings, <<"db">>, hastings_index1}]
        end
    end),
    meck:expect(hastings_index_manager, get_index, fun(_, _) -> {ok, pid} end),
    meck:expect(hastings_index, await, fun(_, _) -> ok end).

% Split partitioned database
split_partitioned_db(#{db2 := Db}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            DocSpec = #{
                pdocs => #{
                    <<"PX">> => 5,
                    <<"PY">> => 5
                },
                mrview => 1,
                local => 1
            },
            add_test_docs(Db, DocSpec),

            % Save documents before the split
            Docs0 = get_all_docs(Db),
            Local0 = get_local_docs(Db),

            % Set some custom metadata properties
            set_revs_limit(Db, 942),
            set_purge_infos_limit(Db, 943),
            SecObj = {[{<<"foo">>, <<"bar">>}]},
            set_security(Db, SecObj),

            % DbInfo is saved after setting metadata bits
            % as those could bump the update sequence
            DbInfo0 = get_db_info(Db),
            PX0 = get_partition_info(Db, <<"PX">>),
            PY0 = get_partition_info(Db, <<"PY">>),

            % Split the one shard
            [#shard{name = Shard}] = lists:sort(mem3:local_shards(Db)),
            {ok, JobId} = mem3_reshard:start_split_job(Shard),
            wait_state(JobId, completed),

            % Perform some basic checks that the shard was split
            Shards1 = lists:sort(mem3:local_shards(Db)),
            ?assertEqual(2, length(Shards1)),
            [#shard{range = R1}, #shard{range = R2}] = Shards1,
            ?assertEqual([16#00000000, 16#7fffffff], R1),
            ?assertEqual([16#80000000, 16#ffffffff], R2),

            % Check metadata bits after the split
            ?assertEqual(942, get_revs_limit(Db)),
            ?assertEqual(943, get_purge_infos_limit(Db)),
            ?assertEqual(SecObj, get_security(Db)),

            DbInfo1 = get_db_info(Db),
            Docs1 = get_all_docs(Db),
            Local1 = get_local_docs(Db),

            % When comparing db infos, ignore update sequences they won't be the
            % same since they are more shards involved after the split
            ?assertEqual(without_seqs(DbInfo0), without_seqs(DbInfo1)),

            % Update seq prefix number is a sum of all shard update sequences
            #{<<"update_seq">> := UpdateSeq0} = update_seq_to_num(DbInfo0),
            #{<<"update_seq">> := UpdateSeq1} = update_seq_to_num(DbInfo1),
            ?assertEqual(UpdateSeq0 * 2, UpdateSeq1),

            % Finally compare that documents are still there after the split
            ?assertEqual(Docs0, Docs1),

            ?assertEqual(PX0, get_partition_info(Db, <<"PX">>)),
            ?assertEqual(PY0, get_partition_info(Db, <<"PY">>)),

            % Don't forget about the local but don't include internal checkpoints
            % as some of those are munged and transformed during the split
            ?assertEqual(without_meta_locals(Local0), without_meta_locals(Local1))
        end)}.

% Make sure a shard can be split again after it was split once. This checks that
% too many got added to some range, such that on next split they'd fail to fit
% in to any of the new target ranges.
split_twice(#{db1 := Db}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            DocSpec = #{docs => 100, delete => [80, 99], mrview => 2, local => 100},
            add_test_docs(Db, DocSpec),

            % Save documents before the split
            Docs0 = get_all_docs(Db),
            Local0 = get_local_docs(Db),

            % Set some custom metadata properties
            set_revs_limit(Db, 942),
            set_purge_infos_limit(Db, 943),
            SecObj = {[{<<"foo">>, <<"bar">>}]},
            set_security(Db, SecObj),

            % DbInfo is saved after setting metadata bits
            % as those could bump the update sequence
            DbInfo0 = get_db_info(Db),

            % Split the one shard
            [#shard{name = Shard1}] = lists:sort(mem3:local_shards(Db)),
            {ok, JobId1} = mem3_reshard:start_split_job(Shard1),
            wait_state(JobId1, completed),

            % Perform some basic checks that the shard was split
            Shards1 = lists:sort(mem3:local_shards(Db)),
            ?assertEqual(2, length(Shards1)),
            [#shard{range = R1}, #shard{range = R2}] = Shards1,
            ?assertEqual([16#00000000, 16#7fffffff], R1),
            ?assertEqual([16#80000000, 16#ffffffff], R2),

            % Check metadata bits after the split
            ?assertEqual(942, get_revs_limit(Db)),
            ?assertEqual(943, get_purge_infos_limit(Db)),
            ?assertEqual(SecObj, get_security(Db)),

            DbInfo1 = get_db_info(Db),
            Docs1 = get_all_docs(Db),
            Local1 = get_local_docs(Db),

            % When comparing db infos, ignore update sequences they won't be the
            % same since they are more shards involved after the split
            ?assertEqual(without_seqs(DbInfo0), without_seqs(DbInfo1)),

            % Update seq prefix number is a sum of all shard update sequences
            #{<<"update_seq">> := UpdateSeq0} = update_seq_to_num(DbInfo0),
            #{<<"update_seq">> := UpdateSeq1} = update_seq_to_num(DbInfo1),
            ?assertEqual(UpdateSeq0 * 2, UpdateSeq1),

            ?assertEqual(Docs0, Docs1),
            ?assertEqual(without_meta_locals(Local0), without_meta_locals(Local1)),

            % Split the first range again
            [#shard{name = Shard2}, _] = lists:sort(mem3:local_shards(Db)),
            {ok, JobId2} = mem3_reshard:start_split_job(Shard2),
            wait_state(JobId2, completed),

            Shards2 = lists:sort(mem3:local_shards(Db)),
            ?assertEqual(3, length(Shards2)),
            [R3, R4, R5] = [R || #shard{range = R} <- Shards2],
            ?assertEqual([16#00000000, 16#3fffffff], R3),
            ?assertEqual([16#40000000, 16#7fffffff], R4),
            ?assertEqual([16#80000000, 16#ffffffff], R5),

            % Check metadata bits after the second split
            ?assertEqual(942, get_revs_limit(Db)),
            ?assertEqual(943, get_purge_infos_limit(Db)),
            ?assertEqual(SecObj, get_security(Db)),

            DbInfo2 = get_db_info(Db),
            Docs2 = get_all_docs(Db),
            Local2 = get_local_docs(Db),

            ?assertEqual(without_seqs(DbInfo1), without_seqs(DbInfo2)),
            % Update seq prefix number is a sum of all shard update sequences
            % But only 1 shard out of 2 was split
            #{<<"update_seq">> := UpdateSeq2} = update_seq_to_num(DbInfo2),
            ?assertEqual(trunc(UpdateSeq1 * 1.5), UpdateSeq2),
            ?assertEqual(Docs1, Docs2),
            ?assertEqual(without_meta_locals(Local1), without_meta_locals(Local2))
        end)}.

couch_events_are_emitted(#{db1 := Db}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            couch_event:register_all(self()),

            % Split the one shard
            [#shard{name = Shard}] = lists:sort(mem3:local_shards(Db)),
            {ok, JobId} = mem3_reshard:start_split_job(Shard),
            wait_state(JobId, completed),

            % Perform some basic checks that the shard was split
            Shards1 = lists:sort(mem3:local_shards(Db)),
            ?assertEqual(2, length(Shards1)),
            [#shard{range = R1}, #shard{range = R2}] = Shards1,
            ?assertEqual([16#00000000, 16#7fffffff], R1),
            ?assertEqual([16#80000000, 16#ffffffff], R2),

            Flush = fun F(Events) ->
                receive
                    {'$couch_event', DbName, Event} when
                        Event =:= deleted orelse
                            Event =:= updated
                    ->
                        case binary:match(DbName, Db) of
                            nomatch -> F(Events);
                            {_, _} -> F([Event | Events])
                        end
                after 0 ->
                    lists:reverse(Events)
                end
            end,
            Events = Flush([]),
            StartAtDeleted = lists:dropwhile(fun(E) -> E =/= deleted end, Events),
            ?assertMatch([deleted, deleted, updated, updated | _], StartAtDeleted),
            couch_event:unregister(self())
        end)}.

retries_work(#{db1 := Db}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            meck:expect(couch_db_split, split, fun(_, _, _) ->
                error(kapow)
            end),

            [#shard{name = Shard}] = lists:sort(mem3:local_shards(Db)),
            {ok, JobId} = mem3_reshard:start_split_job(Shard),

            wait_state(JobId, failed),
            ?assertEqual(3, meck:num_calls(couch_db_split, split, 3))
        end)}.

target_reset_in_initial_copy(#{db1 := Db}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            [#shard{} = Src] = lists:sort(mem3:local_shards(Db)),
            Job = #job{
                source = Src,
                target = [#shard{name = <<"t1">>}, #shard{name = <<"t2">>}],
                job_state = running,
                split_state = initial_copy
            },
            meck:expect(couch_db_split, cleanup_target, 2, ok),
            meck:expect(couch_server, exists, fun
                (<<"t1">>) -> true;
                (<<"t2">>) -> true;
                (DbName) -> meck:passthrough([DbName])
            end),
            JobPid = spawn(fun() -> mem3_reshard_job:initial_copy_impl(Job) end),
            meck:wait(2, couch_db_split, cleanup_target, ['_', '_'], 5000),
            exit(JobPid, kill),
            ?assertEqual(2, meck:num_calls(couch_db_split, cleanup_target, 2))
        end)}.

split_an_incomplete_shard_map(#{db1 := Db}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            [#shard{name = Shard}] = lists:sort(mem3:local_shards(Db)),
            meck:expect(mem3_util, calculate_max_n, 1, 0),
            ?assertMatch(
                {error, {not_enough_shard_copies, _}},
                mem3_reshard:start_split_job(Shard)
            )
        end)}.

% Opening a db target db in initial copy phase will throw an error
target_shards_are_locked(#{db1 := Db}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            add_test_docs(Db, #{docs => 10}),

            % Make the job stops right when it was about to copy the docs
            TestPid = self(),
            meck:new(couch_db, [passthrough]),
            meck:expect(couch_db, start_link, fun(Engine, TName, FilePath, Opts) ->
                TestPid ! {start_link, self(), TName},
                receive
                    continue ->
                        meck:passthrough([Engine, TName, FilePath, Opts])
                end
            end),

            [#shard{name = Shard}] = lists:sort(mem3:local_shards(Db)),
            {ok, JobId} = mem3_reshard:start_split_job(Shard),
            {Target0, JobPid} =
                receive
                    {start_link, Pid, TName} -> {TName, Pid}
                end,
            ?assertEqual(
                {error, {locked, <<"shard splitting">>}},
                couch_db:open_int(Target0, [])
            ),

            % Send two continues for two targets
            JobPid ! continue,
            JobPid ! continue,

            wait_state(JobId, completed)
        end)}.

intercept_state(State) ->
    TestPid = self(),
    meck:new(mem3_reshard_job, [passthrough]),
    meck:expect(mem3_reshard_job, checkpoint_done, fun(Job) ->
        case Job#job.split_state of
            State ->
                TestPid ! {self(), State},
                receive
                    continue -> meck:passthrough([Job]);
                    cancel -> ok
                end;
            _ ->
                meck:passthrough([Job])
        end
    end).

wait_state(JobId, State) ->
    test_util:wait(
        fun() ->
            case mem3_reshard:job(JobId) of
                {ok, {Props}} ->
                    case couch_util:get_value(job_state, Props) of
                        State ->
                            ok;
                        _ ->
                            timer:sleep(100),
                            wait
                    end;
                {error, not_found} ->
                    timer:sleep(100),
                    wait
            end
        end,
        30000
    ).

set_revs_limit(DbName, Limit) ->
    with_proc(fun() -> fabric:set_revs_limit(DbName, Limit, [?ADMIN_CTX]) end).

get_revs_limit(DbName) ->
    with_proc(fun() -> fabric:get_revs_limit(DbName) end).

get_purge_infos_limit(DbName) ->
    with_proc(fun() -> fabric:get_purge_infos_limit(DbName) end).

set_purge_infos_limit(DbName, Limit) ->
    with_proc(fun() ->
        fabric:set_purge_infos_limit(DbName, Limit, [?ADMIN_CTX])
    end).

purge_docs(DbName, DocIdRevs) ->
    with_proc(fun() ->
        fabric:purge_docs(DbName, DocIdRevs, [])
    end).

compact(DbName) ->
    InitFileSize = get_db_file_size(DbName),
    ok = with_proc(fun() -> fabric:compact(DbName) end),
    test_util:wait(
        fun() ->
            case {compact_running(DbName), get_db_file_size(DbName)} of
                {true, _} -> wait;
                {false, FileSize} when FileSize == InitFileSize -> wait;
                {false, FileSize} when FileSize < InitFileSize -> ok
            end
        end,
        5000,
        200
    ).

compact_running(DbName) ->
    {ok, DbInfo} = with_proc(fun() -> fabric:get_db_info(DbName) end),
    #{<<"compact_running">> := CompactRunning} = to_map(DbInfo),
    CompactRunning.

get_db_file_size(DbName) ->
    {ok, DbInfo} = with_proc(fun() -> fabric:get_db_info(DbName) end),
    #{<<"sizes">> := #{<<"file">> := FileSize}} = to_map(DbInfo),
    FileSize.

set_security(DbName, SecObj) ->
    with_proc(fun() -> fabric:set_security(DbName, SecObj) end).

get_security(DbName) ->
    with_proc(fun() -> fabric:get_security(DbName, [?ADMIN_CTX]) end).

get_db_info(DbName) ->
    with_proc(fun() ->
        {ok, Info} = fabric:get_db_info(DbName),
        maps:with(
            [
                <<"db_name">>,
                <<"doc_count">>,
                <<"props">>,
                <<"doc_del_count">>,
                <<"update_seq">>,
                <<"purge_seq">>,
                <<"disk_format_version">>
            ],
            to_map(Info)
        )
    end).

get_group_info(DbName, DesignId) ->
    with_proc(fun() ->
        {ok, GInfo} = fabric:get_view_group_info(DbName, DesignId),
        maps:with(
            [
                <<"language">>, <<"purge_seq">>, <<"signature">>, <<"update_seq">>
            ],
            to_map(GInfo)
        )
    end).

get_partition_info(DbName, Partition) ->
    with_proc(fun() ->
        {ok, PInfo} = fabric:get_partition_info(DbName, Partition),
        maps:with(
            [
                <<"db_name">>, <<"doc_count">>, <<"doc_del_count">>, <<"partition">>
            ],
            to_map(PInfo)
        )
    end).

get_all_docs(DbName) ->
    get_all_docs(DbName, #mrargs{}).

get_all_docs(DbName, #mrargs{} = QArgs0) ->
    GL = erlang:group_leader(),
    with_proc(
        fun() ->
            Cb = fun
                ({row, Props}, Acc) ->
                    Doc = to_map(couch_util:get_value(doc, Props)),
                    #{?ID := Id} = Doc,
                    {ok, Acc#{Id => Doc}};
                ({meta, _}, Acc) ->
                    {ok, Acc};
                (complete, Acc) ->
                    {ok, Acc}
            end,
            QArgs = QArgs0#mrargs{include_docs = true},
            {ok, Docs} = fabric:all_docs(DbName, Cb, #{}, QArgs),
            Docs
        end,
        GL
    ).

get_local_docs(DbName) ->
    LocalNS = {namespace, <<"_local">>},
    maps:map(
        fun(_, Doc) ->
            maps:without([<<"_rev">>], Doc)
        end,
        get_all_docs(DbName, #mrargs{extra = [LocalNS]})
    ).

without_seqs(#{} = InfoMap) ->
    maps:without([<<"update_seq">>, <<"purge_seq">>], InfoMap).

without_meta_locals(#{} = Local) ->
    maps:filter(
        fun
            (<<"_local/purge-mrview-", _/binary>>, _) -> false;
            (<<"_local/shard-sync-", _/binary>>, _) -> false;
            (_, _) -> true
        end,
        Local
    ).

update_seq_to_num(#{} = InfoMap) ->
    maps:map(
        fun
            (<<"update_seq">>, Seq) -> seq_to_num(Seq);
            (<<"purge_seq">>, PSeq) -> seq_to_num(PSeq);
            (_, V) -> V
        end,
        InfoMap
    ).

seq_to_num(Seq) ->
    [SeqNum, _] = binary:split(Seq, <<"-">>),
    binary_to_integer(SeqNum).

to_map([_ | _] = Props) ->
    to_map({Props});
to_map({[_ | _]} = EJson) ->
    jiffy:decode(jiffy:encode(EJson), [return_maps]).

create_db(DbName, Opts) ->
    GL = erlang:group_leader(),
    with_proc(fun() -> fabric:create_db(DbName, Opts) end, GL).

delete_db(DbName) ->
    GL = erlang:group_leader(),
    with_proc(fun() -> fabric:delete_db(DbName, [?ADMIN_CTX]) end, GL).

with_proc(Fun) ->
    with_proc(Fun, undefined, 30000).

with_proc(Fun, GroupLeader) ->
    with_proc(Fun, GroupLeader, 30000).

with_proc(Fun, GroupLeader, Timeout) ->
    {Pid, Ref} = spawn_monitor(fun() ->
        case GroupLeader of
            undefined -> ok;
            _ -> erlang:group_leader(GroupLeader, self())
        end,
        exit({with_proc_res, Fun()})
    end),
    receive
        {'DOWN', Ref, process, Pid, {with_proc_res, Res}} ->
            Res;
        {'DOWN', Ref, process, Pid, Error} ->
            error(Error)
    after Timeout ->
        erlang:demonitor(Ref, [flush]),
        exit(Pid, kill),
        error({with_proc_timeout, Fun, Timeout})
    end.

add_test_docs(DbName, #{} = DocSpec) ->
    Docs =
        docs(maps:get(docs, DocSpec, [])) ++
            pdocs(maps:get(pdocs, DocSpec, #{})) ++
            ddocs(mrview, maps:get(mrview, DocSpec, [])) ++
            ddocs(search, maps:get(search, DocSpec, [])) ++
            ddocs(geo, maps:get(geo, DocSpec, [])) ++
            ldocs(maps:get(local, DocSpec, [])),
    Res = update_docs(DbName, Docs),
    Docs1 = lists:map(
        fun({Doc, {ok, {RevPos, Rev}}}) ->
            Doc#doc{revs = {RevPos, [Rev]}}
        end,
        lists:zip(Docs, Res)
    ),
    case delete_docs(maps:get(delete, DocSpec, []), Docs1) of
        [] -> ok;
        [_ | _] = Deleted -> update_docs(DbName, Deleted)
    end,
    ok.

update_docs(DbName, Docs) ->
    with_proc(fun() ->
        case fabric:update_docs(DbName, Docs, [?ADMIN_CTX]) of
            {accepted, Res} -> Res;
            {ok, Res} -> Res
        end
    end).

delete_docs([S, E], Docs) when E >= S ->
    ToDelete = [doc_id(<<"">>, I) || I <- lists:seq(S, E)],
    lists:filtermap(
        fun(#doc{id = Id} = Doc) ->
            case lists:member(Id, ToDelete) of
                true -> {true, Doc#doc{deleted = true}};
                false -> false
            end
        end,
        Docs
    );
delete_docs(_, _) ->
    [].

pdocs(#{} = PMap) ->
    maps:fold(
        fun(Part, DocSpec, DocsAcc) ->
            docs(DocSpec, <<Part/binary, ":">>) ++ DocsAcc
        end,
        [],
        PMap
    ).

docs(DocSpec) ->
    docs(DocSpec, <<"">>).

docs(N, Prefix) when is_integer(N), N > 0 ->
    docs([0, N - 1], Prefix);
docs([S, E], Prefix) when E >= S ->
    [doc(Prefix, I) || I <- lists:seq(S, E)];
docs(_, _) ->
    [].

ddocs(Type, N) when is_integer(N), N > 0 ->
    ddocs(Type, [0, N - 1]);
ddocs(Type, [S, E]) when E >= S ->
    Body = ddprop(Type),
    BType = atom_to_binary(Type, utf8),
    [doc(<<"_design/", BType/binary>>, I, Body, 0) || I <- lists:seq(S, E)];
ddocs(_, _) ->
    [].

ldocs(N) when is_integer(N), N > 0 ->
    ldocs([0, N - 1]);
ldocs([S, E]) when E >= S ->
    [doc(<<"_local/">>, I, bodyprops(), 0) || I <- lists:seq(S, E)];
ldocs(_) ->
    [].

doc(Pref, Id) ->
    Body = bodyprops(),
    doc(Pref, Id, Body, 42).

doc(Pref, Id, BodyProps, AttSize) ->
    #doc{
        id = doc_id(Pref, Id),
        body = {BodyProps},
        atts = atts(AttSize)
    }.

doc_id(Pref, Id) ->
    IdBin = iolist_to_binary(io_lib:format("~5..0B", [Id])),
    <<Pref/binary, IdBin/binary>>.

ddprop(mrview) ->
    [
        {<<"views">>,
            {[
                {<<"v1">>,
                    {[
                        {<<"map">>, <<"function(d){emit(d);}">>}
                    ]}}
            ]}}
    ];
ddprop(geo) ->
    [
        {<<"st_indexes">>,
            {[
                {<<"area">>,
                    {[
                        {<<"analyzer">>, <<"standard">>},
                        {<<"index">>, <<"function(d){if(d.g){st_index(d.g)}}">>}
                    ]}}
            ]}}
    ];
ddprop(search) ->
    [
        {<<"indexes">>,
            {[
                {<<"types">>,
                    {[
                        {<<"index">>, <<"function(d){if(d.g){st_index(d.g.type)}}">>}
                    ]}}
            ]}}
    ].

bodyprops() ->
    [
        {<<"g">>,
            {[
                {<<"type">>, <<"Polygon">>},
                {<<"coordinates">>, [[[-71.0, 48.4], [-70.0, 48.4], [-71.0, 48.4]]]}
            ]}}
    ].

atts(0) ->
    [];
atts(Size) when is_integer(Size), Size >= 1 ->
    Data = <<<<"x">> || _ <- lists:seq(1, Size)>>,
    [
        couch_att:new([
            {name, <<"att">>},
            {type, <<"app/binary">>},
            {att_len, Size},
            {data, Data}
        ])
    ].
