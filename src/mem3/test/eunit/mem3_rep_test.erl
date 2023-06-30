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

-module(mem3_rep_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("mem3/include/mem3.hrl").

-define(ID, <<"_id">>).
% seconds
-define(TIMEOUT, 60).

setup() ->
    {AllSrc, AllTgt} = {?tempdb(), ?tempdb()},
    {PartSrc, PartTgt} = {?tempdb(), ?tempdb()},
    Localdb = ?tempdb(),
    create_db(AllSrc, [{q, 1}, {n, 1}]),
    create_db(AllTgt, [{q, 2}, {n, 1}]),
    PartProps = [{partitioned, true}, {hash, [couch_partition, hash, []]}],
    create_db(PartSrc, [{q, 1}, {n, 1}, {props, PartProps}]),
    create_db(PartTgt, [{q, 2}, {n, 1}, {props, PartProps}]),
    create_local_db(Localdb),
    #{
        allsrc => AllSrc,
        alltgt => AllTgt,
        partsrc => PartSrc,
        parttgt => PartTgt,
        localdb => Localdb
    }.

teardown(#{} = Dbs) ->
    maps:map(
        fun
            (localdb, Db) -> delete_local_db(Db);
            (_, Db) -> delete_db(Db)
        end,
        Dbs
    ).

start_couch() ->
    test_util:start_couch([mem3, fabric]).

stop_couch(Ctx) ->
    test_util:stop_couch(Ctx).

mem3_reshard_db_test_() ->
    {
        "mem3 rep db tests",
        {
            setup,
            fun start_couch/0,
            fun stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(replicate_basics, ?TIMEOUT),
                    ?TDEF_FE(replicate_small_batches, ?TIMEOUT),
                    ?TDEF_FE(replicate_low_batch_count, ?TIMEOUT),
                    ?TDEF_FE(replicate_with_partitions, ?TIMEOUT),
                    ?TDEF_FE(replicate_to_and_from_local, ?TIMEOUT),
                    ?TDEF_FE(replicate_with_purges, ?TIMEOUT)
                ]
            }
        }
    }.

replicate_basics(#{allsrc := AllSrc, alltgt := AllTgt}) ->
    DocSpec = #{docs => 10, delete => [5, 9]},
    add_test_docs(AllSrc, DocSpec),
    SDocs = get_all_docs(AllSrc),

    [Src] = lists:sort(mem3:local_shards(AllSrc)),
    [Tgt1, Tgt2] = lists:sort(mem3:local_shards(AllTgt)),
    #shard{range = R1} = Tgt1,
    #shard{range = R2} = Tgt2,
    TMap = #{R1 => Tgt1, R2 => Tgt2},
    Opts = [{batch_size, 1000}, {batch_count, all}],
    ?assertMatch({ok, 0}, mem3_rep:go(Src, TMap, Opts)),

    ?assertEqual(SDocs, get_all_docs(AllTgt)).

replicate_small_batches(#{allsrc := AllSrc, alltgt := AllTgt}) ->
    DocSpec = #{docs => 10, delete => [5, 9]},
    add_test_docs(AllSrc, DocSpec),
    SDocs = get_all_docs(AllSrc),

    [Src] = lists:sort(mem3:local_shards(AllSrc)),
    [Tgt1, Tgt2] = lists:sort(mem3:local_shards(AllTgt)),
    #shard{range = R1} = Tgt1,
    #shard{range = R2} = Tgt2,
    TMap = #{R1 => Tgt1, R2 => Tgt2},
    Opts = [{batch_size, 2}, {batch_count, all}],
    ?assertMatch({ok, 0}, mem3_rep:go(Src, TMap, Opts)),

    ?assertEqual(SDocs, get_all_docs(AllTgt)).

replicate_low_batch_count(#{allsrc := AllSrc, alltgt := AllTgt}) ->
    DocSpec = #{docs => 10, delete => [5, 9]},
    add_test_docs(AllSrc, DocSpec),
    SDocs = get_all_docs(AllSrc),

    [Src] = lists:sort(mem3:local_shards(AllSrc)),
    [Tgt1, Tgt2] = lists:sort(mem3:local_shards(AllTgt)),
    #shard{range = R1} = Tgt1,
    #shard{range = R2} = Tgt2,
    TMap = #{R1 => Tgt1, R2 => Tgt2},

    Opts1 = [{batch_size, 2}, {batch_count, 1}],
    ?assertMatch({ok, 8}, mem3_rep:go(Src, TMap, Opts1)),

    Opts2 = [{batch_size, 1}, {batch_count, 2}],
    ?assertMatch({ok, 6}, mem3_rep:go(Src, TMap, Opts2)),

    Opts3 = [{batch_size, 1000}, {batch_count, all}],
    ?assertMatch({ok, 0}, mem3_rep:go(Src, TMap, Opts3)),

    ?assertEqual(SDocs, get_all_docs(AllTgt)).

replicate_with_partitions(#{partsrc := PartSrc, parttgt := PartTgt}) ->
    DocSpec = #{
        pdocs => #{
            <<"PX">> => 15,
            <<"PY">> => 19
        }
    },
    add_test_docs(PartSrc, DocSpec),
    SDocs = get_all_docs(PartSrc),
    PXSrc = get_partition_info(PartSrc, <<"PX">>),
    PYSrc = get_partition_info(PartSrc, <<"PY">>),

    [Src] = lists:sort(mem3:local_shards(PartSrc)),
    [Tgt1, Tgt2] = lists:sort(mem3:local_shards(PartTgt)),
    #shard{range = R1} = Tgt1,
    #shard{range = R2} = Tgt2,
    TMap = #{R1 => Tgt1, R2 => Tgt2},
    Opts = [{batch_size, 1000}, {batch_count, all}],
    ?assertMatch({ok, 0}, mem3_rep:go(Src, TMap, Opts)),

    ?assertEqual(PXSrc, get_partition_info(PartTgt, <<"PX">>)),
    ?assertEqual(PYSrc, get_partition_info(PartTgt, <<"PY">>)),
    ?assertEqual(SDocs, get_all_docs(PartTgt)).

replicate_with_purges(#{allsrc := AllSrc, alltgt := AllTgt}) ->
    DocSpec = #{docs => 10, delete => [5, 9], purge => [2, 4]},
    add_test_docs(AllSrc, DocSpec),
    % Add and purge some docs on target to excercise the pull_purges code path
    add_test_docs(AllTgt, #{docs => 3, purge => [0, 2]}),

    [Src] = lists:sort(mem3:local_shards(AllSrc)),
    [Tgt1, Tgt2] = lists:sort(mem3:local_shards(AllTgt)),
    #shard{range = R1} = Tgt1,
    #shard{range = R2} = Tgt2,
    TMap = #{R1 => Tgt1, R2 => Tgt2},
    Opts = [{batch_size, 1000}, {batch_count, all}],
    ?assertMatch({ok, 0}, mem3_rep:go(Src, TMap, Opts)),

    SDocs = get_all_docs(AllSrc),
    % Purges from the target should have been pulled and removed docs 0,1,2.
    % Source should have no live docs.
    ?assertEqual(#{}, SDocs),
    ?assertEqual(#{}, get_all_docs(AllTgt)).

replicate_to_and_from_local(#{localdb := LocalDb, allsrc := ClusteredDb}) ->
    % We'll just tests that we can pull purges from the target
    add_test_docs(ClusteredDb, #{docs => 6, purge => [0, 2]}),

    [#shard{name = TgtDbName}] = mem3:local_shards(ClusteredDb),
    Opts = [{batch_size, 1000}, {batch_count, all}],
    Src1 = #shard{name = LocalDb, node = node()},
    Tgt1 = #shard{name = TgtDbName, node = node()},
    ?assertMatch({ok, 0}, mem3_rep:go(Src1, Tgt1, Opts)),

    % Purge a few more docs in clustered db
    add_test_docs(ClusteredDb, #{purge => [3, 4]}),

    % Replicate the other way: from clustered to source
    Src2 = #shard{name = TgtDbName, node = node()},
    Tgt2 = #shard{name = LocalDb, node = node()},
    ?assertMatch({ok, 0}, mem3_rep:go(Src2, Tgt2, Opts)),
    SDocs = get_all_docs(ClusteredDb),
    ?assertEqual(1, map_size(SDocs)),
    ?assertMatch(#{<<"00005">> := #{}}, SDocs).

get_partition_info(DbName, Partition) ->
    with_proc(fun() ->
        {ok, PInfo} = fabric:get_partition_info(DbName, Partition),
        maps:with(
            [
                <<"doc_count">>, <<"doc_del_count">>, <<"partition">>
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

create_local_db(DbName) ->
    {ok, _} = couch_server:create(DbName, []),
    ok.

delete_local_db(DbName) ->
    couch_server:delete(DbName, []).

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
            pdocs(maps:get(pdocs, DocSpec, #{})),
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
    purge_docs(DbName, maps:get(purge, DocSpec, [])).

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

purge_docs(DbName, [S, E]) when E >= S ->
    Ids = [doc_id(<<"">>, I) || I <- lists:seq(S, E)],
    IdRevs = [{Id, get_revs(DbName, Id)} || Id <- Ids],
    {ok, _} = fabric:purge_docs(DbName, IdRevs, []),
    ok;
purge_docs(_DbName, []) ->
    ok.

get_revs(DbName, DocId) ->
    FDI = fabric:get_full_doc_info(DbName, DocId, []),
    #doc_info{revs = Revs} = couch_doc:to_doc_info(FDI),
    [Rev#rev_info.rev || Rev <- Revs].

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
