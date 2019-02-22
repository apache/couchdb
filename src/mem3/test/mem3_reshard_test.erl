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
-include_lib("couch_mrview/include/couch_mrview.hrl"). % for all_docs function

-define(ID, <<"_id">>).

setup() ->
    {Db1, Db2} = {?tempdb(), ?tempdb()},
    create_db(Db1, [{q, 1}, {n, 1}]),
    PartProps = [{partitioned, true}, {hash, [couch_partition, hash, []]}],
    create_db(Db2, [{q, 1}, {n, 1}, {props, PartProps}]),
    #{db1 => Db1, db => Db2}.


teardown(#{} = Dbs) ->
    mem3_reshard:reset_state(),
    maps:map(fun(_, Db) -> delete_db(Db) end, Dbs),
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
            fun start_couch/0, fun stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun split_one_shard/1,
                    fun update_docs_before_topoff1/1,
                    fun indices_are_built/1
                ]
            }
        }
    }.


% This is a basic test to check that shard splitting preserves documents, and
% db meta props like revs limits and security.
split_one_shard(#{db1 := Db}) ->
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
        [#shard{name=Shard}] = lists:sort(mem3:local_shards(Db)),
        {ok, JobId} = mem3_reshard:start_split_job(Shard),
        wait_state(JobId, completed),

        % Perform some basic checks that the shard was split
        ResultShards = lists:sort(mem3:local_shards(Db)),
        ?assertEqual(2, length(ResultShards)),
        [#shard{range = R1}, #shard{range = R2}] = ResultShards,
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
    end).


% This test checks that document added while the shard is being split are not
% lost. Topoff1 state happens before indices are built
update_docs_before_topoff1(#{db1 := Db}) ->
    ?_test(begin
        add_test_docs(Db, #{docs => 10}),

        intercept_state(topoff1),

        [#shard{name=Shard}] = lists:sort(mem3:local_shards(Db)),
        {ok, JobId} = mem3_reshard:start_split_job(Shard),

        receive {JobPid, topoff1} -> ok end,
        add_test_docs(Db, #{docs => [10, 19], local => 1}),
        Docs0 = get_all_docs(Db),
        Local0 = get_local_docs(Db),
        DbInfo0 = get_db_info(Db),
        JobPid ! continue,

        wait_state(JobId, completed),

        % Perform some basic checks that the shard was split
        ResultShards = lists:sort(mem3:local_shards(Db)),
        ?assertEqual(2, length(ResultShards)),

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
    end).


% This test that indices are built during shard splitting.
indices_are_built(#{db1 := Db}) ->
    ?_test(begin
        add_test_docs(Db, #{docs => 10, mrview => 2, search => 2, geo => 2}),
        Docs0 = get_all_docs(Db),
        [#shard{name=Shard}] = lists:sort(mem3:local_shards(Db)),
        {ok, JobId} = mem3_reshard:start_split_job(Shard),
        wait_state(JobId, completed),
        ResultShards = lists:sort(mem3:local_shards(Db)),
        ?assertEqual(2, length(ResultShards)),
        MRViewGroupInfo =  get_group_info(Db, <<"_design/mrview00000">>),
        ?assertMatch(#{<<"update_seq">> := 32}, MRViewGroupInfo)
    end).


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


cancel_intercept() ->
     meck:expect(mem3_reshard_job, checkpoint_done, fun(Job) ->
         meck:passthrough([Job])
     end).


wait_state(JobId, State) ->
    test_util:wait(fun() ->
            case mem3_reshard:job(JobId) of
                {ok, {Props}} ->
                    case couch_util:get_value(job_state, Props) of
                        State -> ok;
                        _ -> timer:sleep(100), wait
                    end;
                {error, not_found} -> timer:sleep(100), wait
            end
    end, 30000).


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


set_security(DbName, SecObj) ->
    with_proc(fun() -> fabric:set_security(DbName, SecObj) end).


get_security(DbName) ->
    with_proc(fun() -> fabric:get_security(DbName, [?ADMIN_CTX]) end).


get_db_info(DbName) ->
    with_proc(fun() ->
        {ok, Info} = fabric:get_db_info(DbName),
        maps:with([
            <<"db_name">>, <<"doc_count">>, <<"props">>,  <<"doc_del_count">>,
            <<"update_seq">>, <<"purge_seq">>, <<"disk_format_version">>
        ], to_map(Info))
    end).


get_group_info(DbName, DesignId) ->
    with_proc(fun() ->
        {ok, GInfo} = fabric:get_view_group_info(DbName, DesignId),
        maps:with([
            <<"language">>, <<"purge_seq">>, <<"signature">>, <<"update_seq">>
        ], to_map(GInfo))
    end).


get_partition_info(DbName, Partition) ->
    with_proc(fun() ->
        {ok, PInfo} = fabric:get_partition_info(DbName, Partition),
        maps:with([
            <<"db_name">>, <<"doc_count">>, <<"doc_del_count">>, <<"partition">>
        ], to_map(PInfo))
    end).


get_all_docs(DbName) ->
    get_all_docs(DbName, #mrargs{}).


get_all_docs(DbName, #mrargs{} = QArgs0) ->
    GL = erlang:group_leader(),
    with_proc(fun() ->
        Cb = fun
            ({row, Props}, Acc) ->
                Doc = to_map(couch_util:get_value(doc, Props)),
                #{?ID := Id} = Doc,
                {ok, Acc#{Id => Doc}};
            ({meta, _}, Acc) -> {ok, Acc};
            (complete, Acc) -> {ok, Acc}
        end,
        QArgs = QArgs0#mrargs{include_docs = true},
        {ok, Docs} = fabric:all_docs(DbName, Cb, #{}, QArgs),
        Docs
    end, GL).


get_local_docs(DbName) ->
    LocalNS = {namespace, <<"_local">>},
    maps:map(fun(_, Doc) ->
        maps:without([<<"_rev">>], Doc)
    end, get_all_docs(DbName, #mrargs{extra = [LocalNS]})).


without_seqs(#{} = InfoMap) ->
    maps:without([<<"update_seq">>, <<"purge_seq">>], InfoMap).


without_meta_locals(#{} = Local) ->
    maps:filter(fun
        (<<"_local/purge-mrview-", _/binary>>, _) -> false;
        (<<"_local/shard-sync-", _/binary>>, _) -> false;
        (_, _) -> true
   end, Local).


update_seq_to_num(#{} = InfoMap) ->
    maps:map(fun
        (<<"update_seq">>, Seq) -> seq_to_num(Seq);
        (<<"purge_seq">>, PSeq) -> seq_to_num(PSeq);
        (_, V) -> V
    end, InfoMap).


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
    Docs = docs(maps:get(docs, DocSpec, []))
        ++ ddocs(mrview, maps:get(mrview, DocSpec, []))
        ++ ddocs(search, maps:get(search, DocSpec, []))
        ++ ddocs(geo, maps:get(geo, DocSpec, []))
        ++ ldocs(maps:get(local, DocSpec, [])),
    Res = update_docs(DbName, Docs),
    Docs1 = lists:map(fun({Doc, {ok, {RevPos, Rev}}}) ->
        Doc#doc{revs = {RevPos, [Rev]}}
    end, lists:zip(Docs, Res)),
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
    lists:filtermap(fun(#doc{id = Id} = Doc) ->
        case lists:member(Id, ToDelete) of
            true -> {true, Doc#doc{deleted = true}};
            false -> false
        end
    end, Docs);
delete_docs(_, _) ->
    [].


docs(N) when is_integer(N), N > 0 ->
    docs([0, N - 1]);
docs([S, E]) when E >= S ->
    [doc(<<"">>, I) || I <- lists:seq(S, E)];
docs(_) ->
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
        {<<"views">>, {[
             {<<"v1">>, {[
                 {<<"map">>, <<"function(d){emit(d);}">>}
             ]}}
        ]}}
    ];

ddprop(geo) ->
    [
        {<<"st_indexes">>, {[
            {<<"area">>, {[
                {<<"analyzer">>, <<"standard">>},
                {<<"index">>, <<"function(d){if(d.g){st_index(d.g)}}">> }
            ]}}
        ]}}
    ];

ddprop(search) ->
    [
        {<<"indexes">>, {[
            {<<"types">>, {[
                {<<"index">>, <<"function(d){if(d.g){st_index(d.g.type)}}">>}
            ]}}
        ]}}
   ].


bodyprops() ->
    [
        {<<"g">>, {[
            {<<"type">>, <<"Polygon">>},
            {<<"coordinates">>, [[[-71.0, 48.4], [-70.0, 48.4], [-71.0, 48.4]]]}
        ]}}
   ].


atts(0) ->
    [];

atts(Size) when is_integer(Size), Size >= 1 ->
    Data = << <<"x">> || _ <- lists:seq(1, Size) >>,
    [couch_att:new([
        {name, <<"att">>},
        {type, <<"app/binary">>},
        {att_len, Size},
        {data, Data}
    ])].
