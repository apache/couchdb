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
    create_db(AllSrc, [{q, 1}, {n, 1}]),
    create_db(AllTgt, [{q, 2}, {n, 1}]),
    PartProps = [{partitioned, true}, {hash, [couch_partition, hash, []]}],
    create_db(PartSrc, [{q, 1}, {n, 1}, {props, PartProps}]),
    create_db(PartTgt, [{q, 2}, {n, 1}, {props, PartProps}]),
    #{allsrc => AllSrc, alltgt => AllTgt, partsrc => PartSrc, parttgt => PartTgt}.

teardown(#{} = Dbs) ->
    maps:map(fun(_, Db) -> delete_db(Db) end, Dbs).

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
                    fun replicate_basics/1,
                    fun replicate_small_batches/1,
                    fun replicate_low_batch_count/1,
                    fun replicate_with_partitions/1
                ]
            }
        }
    }.

replicate_basics(#{allsrc := AllSrc, alltgt := AllTgt}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
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

            ?assertEqual(SDocs, get_all_docs(AllTgt))
        end)}.

replicate_small_batches(#{allsrc := AllSrc, alltgt := AllTgt}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
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

            ?assertEqual(SDocs, get_all_docs(AllTgt))
        end)}.

replicate_low_batch_count(#{allsrc := AllSrc, alltgt := AllTgt}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
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

            ?assertEqual(SDocs, get_all_docs(AllTgt))
        end)}.

replicate_with_partitions(#{partsrc := PartSrc, parttgt := PartTgt}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
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
            ?assertEqual(SDocs, get_all_docs(PartTgt))
        end)}.

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
