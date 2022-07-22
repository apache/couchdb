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

-module(mem3_reshard_changes_feed_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/src/mem3_reshard.hrl").

% seconds
-define(TIMEOUT, 60).

-define(assertChanges(Expected, Received), begin
    ((fun() ->
        ExpectedIDs = lists:sort([I || #{id := I} <- Expected]),
        ReceivedIDs = lists:sort([I || #{id := I} <- Received]),
        ?assertEqual(ExpectedIDs, ReceivedIDs)
    end)())
end).

setup() ->
    Db1 = ?tempdb(),
    create_db(Db1, [{q, 1}, {n, 1}]),
    #{db1 => Db1}.

teardown(#{} = Dbs) ->
    mem3_reshard:reset_state(),
    maps:map(fun(_, Db) -> delete_db(Db) end, Dbs).

start_couch() ->
    test_util:start_couch(?CONFIG_CHAIN, [mem3, fabric]).

stop_couch(Ctx) ->
    test_util:stop_couch(Ctx).

mem3_reshard_changes_feed_test_() ->
    {
        "mem3 shard split changes feed tests",
        {
            setup,
            fun start_couch/0,
            fun stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun normal_feed_should_work_after_split/1,
                    fun continuous_feed_should_work_during_split/1
                ]
            }
        }
    }.

normal_feed_should_work_after_split(#{db1 := Db}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            DocSpec = #{
                docs => [1, 10],
                delete => [5, 6]
            },
            add_test_docs(Db, DocSpec),

            % gather pre-shard changes
            BaseArgs = #changes_args{feed = "normal", dir = fwd, since = 0},
            {ok, OldChanges, OldEndSeq} = get_changes_feed(Db, BaseArgs),

            % Split the shard
            split_and_wait(Db),

            % verify changes list consistent for all the old seqs
            lists:foldl(
                fun(#{seq := Seq} = C, ExpectedChanges) ->
                    Args = BaseArgs#changes_args{since = Seq},
                    {ok, Changes, _EndSeq} = get_changes_feed(Db, Args),
                    ?assertChanges(ExpectedChanges, Changes),
                    [C | ExpectedChanges]
                end,
                [],
                OldChanges
            ),

            % confirm that old LastSeq respected
            Args1 = BaseArgs#changes_args{since = OldEndSeq},
            {ok, Changes1, EndSeq1} = get_changes_feed(Db, Args1),
            ?assertChanges([], Changes1),

            % confirm that new LastSeq also respected
            Args2 = BaseArgs#changes_args{since = EndSeq1},
            {ok, Changes2, EndSeq2} = get_changes_feed(Db, Args2),
            ?assertChanges([], Changes2),
            ?assertEqual(EndSeq2, EndSeq1),

            % confirm we didn't lost any changes and have consistent last seq
            {ok, Changes3, EndSeq3} = get_changes_feed(Db, BaseArgs),
            ?assertChanges(OldChanges, Changes3),

            % add some docs
            add_test_docs(Db, #{docs => [11, 15]}),
            Args4 = BaseArgs#changes_args{since = EndSeq3},
            {ok, Changes4, EndSeq4} = get_changes_feed(Db, Args4),
            AddedChanges = [#{id => ID} || #doc{id = ID} <- docs([11, 15])],
            ?assertChanges(AddedChanges, Changes4),

            % confirm include_docs and deleted works
            Args5 = BaseArgs#changes_args{include_docs = true},
            {ok, Changes5, EndSeq5} = get_changes_feed(Db, Args5),
            ?assertEqual(EndSeq4, EndSeq5),
            [SampleChange] = [C || #{id := ID} = C <- Changes5, ID == <<"00005">>],
            ?assertMatch(#{deleted := true}, SampleChange),
            ?assertMatch(#{doc := {Body}} when is_list(Body), SampleChange),

            % update and delete some pre and post split docs
            AllDocs = [couch_doc:from_json_obj(Doc) || #{doc := Doc} <- Changes5],
            UpdateDocs = lists:filtermap(
                fun
                    (#doc{id = <<"00002">>}) -> true;
                    (#doc{id = <<"00012">>}) -> true;
                    (#doc{id = <<"00004">>} = Doc) -> {true, Doc#doc{deleted = true}};
                    (#doc{id = <<"00014">>} = Doc) -> {true, Doc#doc{deleted = true}};
                    (_) -> false
                end,
                AllDocs
            ),
            update_docs(Db, UpdateDocs),

            Args6 = BaseArgs#changes_args{since = EndSeq5},
            {ok, Changes6, EndSeq6} = get_changes_feed(Db, Args6),
            UpdatedChanges = [#{id => ID} || #doc{id = ID} <- UpdateDocs],
            ?assertChanges(UpdatedChanges, Changes6),
            [#{seq := Seq6} | _] = Changes6,
            ?assertEqual(EndSeq6, Seq6),

            Args7 = Args6#changes_args{dir = rev, limit = 4},
            {ok, Changes7, EndSeq7} = get_changes_feed(Db, Args7),
            ?assertEqual(4, length(Changes7)),
            [#{seq := Seq7} | _] = Changes7,
            ?assertEqual(EndSeq7, Seq7)
        end)}.

continuous_feed_should_work_during_split(#{db1 := Db}) ->
    {timeout, ?TIMEOUT,
        ?_test(begin
            {UpdaterPid, UpdaterRef} = spawn_monitor(fun() ->
                Updater = fun U({State, I}) ->
                    receive
                        {get_state, {Pid, Ref}} ->
                            Pid ! {state, Ref, {State, I}},
                            U({State, I});
                        add ->
                            DocSpec = #{docs => [I, I]},
                            add_test_docs(Db, DocSpec),
                            U({State, I + 1});
                        split ->
                            spawn_monitor(fun() -> split_and_wait(Db) end),
                            U({"in_process", I});
                        stop ->
                            receive
                                {'DOWN', _, process, _, _} -> ok
                            end,
                            ok
                    end
                end,
                Updater({"before", 1})
            end),

            Callback = fun
                (start, Acc) ->
                    {ok, Acc};
                (waiting_for_updates, Acc) ->
                    Ref = make_ref(),
                    UpdaterPid ! {get_state, {self(), Ref}},
                    receive
                        {state, Ref, {State, _}} -> ok
                    end,
                    case {State, length(Acc)} of
                        {"before", N} when N < 5 ->
                            UpdaterPid ! add,
                            {ok, Acc};
                        {"before", _} ->
                            UpdaterPid ! split,
                            {ok, Acc};
                        {"in_process", N} when N < 10 ->
                            UpdaterPid ! add,
                            {ok, Acc};
                        {"in_process", _} ->
                            {ok, Acc}
                    end;
                (timeout, Acc) ->
                    {ok, Acc};
                ({change, {Change}}, Acc) ->
                    CM = maps:from_list(Change),
                    {ok, [CM | Acc]};
                ({stop, EndSeq, _Pending}, Acc) ->
                    % Notice updater is still running
                    {stop, EndSeq, Acc}
            end,

            BaseArgs = #changes_args{
                feed = "continuous",
                heartbeat = 100,
                timeout = 1000
            },
            StopResult = get_changes_feed(Db, BaseArgs, Callback),

            % Changes feed stopped when source shard was deleted
            ?assertMatch({stop, _, _}, StopResult),
            {stop, StopEndSeq, StopChanges} = StopResult,

            % Add 5 extra docs to the db right after changes feed was stopped
            [UpdaterPid ! add || _ <- lists:seq(1, 5)],

            % The the number of documents that updater had added
            Ref = make_ref(),
            UpdaterPid ! {get_state, {self(), Ref}},
            DocCount =
                receive
                    {state, Ref, {_, I}} -> I - 1
                end,

            UpdaterPid ! stop,
            receive
                {'DOWN', UpdaterRef, process, UpdaterPid, normal} ->
                    ok;
                {'DOWN', UpdaterRef, process, UpdaterPid, Error} ->
                    erlang:error(
                        {test_context_failed, [
                            {module, ?MODULE},
                            {line, ?LINE},
                            {value, Error},
                            {reason, "Updater died"}
                        ]}
                    )
            end,

            AfterArgs = #changes_args{feed = "normal", since = StopEndSeq},
            {ok, AfterChanges, _} = get_changes_feed(Db, AfterArgs),
            DocIDs = [Id || #{id := Id} <- StopChanges ++ AfterChanges],
            ExpectedDocIDs = [doc_id(<<>>, N) || N <- lists:seq(1, DocCount)],
            ?assertEqual(ExpectedDocIDs, lists:usort(DocIDs))
        end)}.

split_and_wait(Db) ->
    [#shard{name = Shard}] = lists:sort(mem3:local_shards(Db)),
    {ok, JobId} = mem3_reshard:start_split_job(Shard),
    wait_state(JobId, completed),
    ResultShards = lists:sort(mem3:local_shards(Db)),
    ?assertEqual(2, length(ResultShards)).

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

get_changes_feed(Db, Args) ->
    get_changes_feed(Db, Args, fun changes_callback/2).

get_changes_feed(Db, Args, Callback) ->
    with_proc(fun() ->
        fabric:changes(Db, Callback, [], Args)
    end).

changes_callback(start, Acc) ->
    {ok, Acc};
changes_callback({change, {Change}}, Acc) ->
    CM = maps:from_list(Change),
    {ok, [CM | Acc]};
changes_callback({stop, EndSeq, _Pending}, Acc) ->
    {ok, Acc, EndSeq}.

%% common helpers from here

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
    Docs = docs(maps:get(docs, DocSpec, [])),
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

docs([S, E]) when E >= S ->
    [doc(<<"">>, I) || I <- lists:seq(S, E)];
docs(_) ->
    [].

doc(Pref, Id) ->
    Body = [{<<"a">>, <<"b">>}],
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
