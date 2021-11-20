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

-module(couch_db_split_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(RINGTOP, 2 bsl 31).
% seconds
-define(TIMEOUT, 60).

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    DbName.

teardown(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    FilePath = couch_db:get_filepath(Db),
    ok = couch_db:close(Db),
    ok = file:delete(FilePath).

split_test_() ->
    Cases = [
        {"Should split an empty shard", 0, 2},
        {"Should split shard in half", 100, 2},
        {"Should split shard in three", 99, 3},
        {"Should split shard in four", 100, 4}
    ],
    {
        setup,
        fun test_util:start_couch/0,
        fun test_util:stop/1,
        [
            {
                foreachx,
                fun(_) -> setup() end,
                fun(_, St) -> teardown(St) end,
                [{Case, fun should_split_shard/2} || Case <- Cases]
            },
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun should_fail_on_missing_source/1,
                    fun should_fail_on_existing_target/1,
                    fun should_fail_on_invalid_target_name/1,
                    fun should_crash_on_invalid_tmap/1,
                    fun should_fail_on_opened_target/1
                ]
            }
        ]
    }.

should_split_shard({Desc, TotalDocs, Q}, DbName) ->
    {ok, ExpectSeq} = create_docs(DbName, TotalDocs),
    Ranges = make_ranges(Q),
    TMap = make_targets(Ranges),
    DocsPerRange = TotalDocs div Q,
    PickFun = make_pickfun(DocsPerRange),
    {Desc, timeout, ?TIMEOUT,
        ?_test(begin
            {ok, UpdateSeq} = couch_db_split:split(DbName, TMap, PickFun),
            ?assertEqual(ExpectSeq, UpdateSeq),
            maps:map(
                fun(Range, Name) ->
                    {ok, Db} = couch_db:open_int(Name, []),
                    FilePath = couch_db:get_filepath(Db),
                    %% target actually exists
                    ?assertMatch({ok, _}, file:read_file_info(FilePath)),
                    %% target's update seq is the same as source's update seq
                    USeq = couch_db:get_update_seq(Db),
                    ?assertEqual(ExpectSeq, USeq),
                    %% target shard has all the expected in its range docs
                    {ok, DocsInShard} = couch_db:fold_docs(
                        Db,
                        fun(FDI, Acc) ->
                            DocId = FDI#full_doc_info.id,
                            ExpectedRange = PickFun(DocId, Ranges, undefined),
                            ?assertEqual(ExpectedRange, Range),
                            {ok, Acc + 1}
                        end,
                        0
                    ),
                    ?assertEqual(DocsPerRange, DocsInShard),
                    ok = couch_db:close(Db),
                    ok = file:delete(FilePath)
                end,
                TMap
            )
        end)}.

should_fail_on_missing_source(_DbName) ->
    DbName = ?tempdb(),
    Ranges = make_ranges(2),
    TMap = make_targets(Ranges),
    Response = couch_db_split:split(DbName, TMap, fun fake_pickfun/3),
    ?_assertEqual({error, missing_source}, Response).

should_fail_on_existing_target(DbName) ->
    Ranges = make_ranges(2),
    TMap = maps:map(
        fun(_, TName) ->
            % We create the target but make sure to remove it from the cache so we
            % hit the eexist error instaed of already_opened
            {ok, Db} = couch_db:create(TName, [?ADMIN_CTX]),
            Pid = couch_db:get_pid(Db),
            ok = couch_db:close(Db),
            exit(Pid, kill),
            test_util:wait(fun() ->
                case ets:lookup(couch_server:couch_dbs(DbName), TName) of
                    [] -> ok;
                    [_ | _] -> wait
                end
            end),
            TName
        end,
        make_targets(Ranges)
    ),
    Response = couch_db_split:split(DbName, TMap, fun fake_pickfun/3),
    ?_assertMatch({error, {target_create_error, _, eexist}}, Response).

should_fail_on_invalid_target_name(DbName) ->
    Ranges = make_ranges(2),
    TMap = maps:map(
        fun([B, _], _) ->
            iolist_to_binary(["_$", couch_util:to_hex(<<B:32/integer>>)])
        end,
        make_targets(Ranges)
    ),
    Expect =
        {error, {target_create_error, <<"_$00000000">>, {illegal_database_name, <<"_$00000000">>}}},
    Response = couch_db_split:split(DbName, TMap, fun fake_pickfun/3),
    ?_assertMatch(Expect, Response).

should_crash_on_invalid_tmap(DbName) ->
    Ranges = make_ranges(1),
    TMap = make_targets(Ranges),
    ?_assertError(
        function_clause,
        couch_db_split:split(DbName, TMap, fun fake_pickfun/3)
    ).

should_fail_on_opened_target(DbName) ->
    Ranges = make_ranges(2),
    TMap = maps:map(
        fun(_, TName) ->
            % We create and keep the target open but delete
            % its file on disk so we don't fail with eexist
            {ok, Db} = couch_db:create(TName, [?ADMIN_CTX]),
            FilePath = couch_db:get_filepath(Db),
            ok = file:delete(FilePath),
            TName
        end,
        make_targets(Ranges)
    ),
    ?_assertMatch(
        {error, {target_create_error, _, already_opened}},
        couch_db_split:split(DbName, TMap, fun fake_pickfun/3)
    ).

copy_local_docs_test_() ->
    Cases = [
        {"Should work with no docs", 0, 2},
        {"Should copy local docs after split in two", 100, 2},
        {"Should copy local docs after split in three", 99, 3},
        {"Should copy local docs after split in four", 100, 4}
    ],
    {
        setup,
        fun test_util:start_couch/0,
        fun test_util:stop/1,
        [
            {
                foreachx,
                fun(_) -> setup() end,
                fun(_, St) -> teardown(St) end,
                [{Case, fun should_copy_local_docs/2} || Case <- Cases]
            },
            {"Should return error on missing source",
                fun should_fail_copy_local_on_missing_source/0}
        ]
    }.

should_copy_local_docs({Desc, TotalDocs, Q}, DbName) ->
    {ok, ExpectSeq} = create_docs(DbName, TotalDocs),
    Ranges = make_ranges(Q),
    TMap = make_targets(Ranges),
    DocsPerRange = TotalDocs div Q,
    PickFun = make_pickfun(DocsPerRange),
    {Desc, timeout, ?TIMEOUT,
        ?_test(begin
            {ok, UpdateSeq} = couch_db_split:split(DbName, TMap, PickFun),
            ?assertEqual(ExpectSeq, UpdateSeq),
            Response = couch_db_split:copy_local_docs(DbName, TMap, PickFun),
            ?assertEqual(ok, Response),
            maps:map(
                fun(Range, Name) ->
                    {ok, Db} = couch_db:open_int(Name, []),
                    FilePath = couch_db:get_filepath(Db),
                    %% target shard has all the expected in its range docs
                    {ok, DocsInShard} = couch_db:fold_local_docs(
                        Db,
                        fun(Doc, Acc) ->
                            DocId = Doc#doc.id,
                            ExpectedRange = PickFun(DocId, Ranges, undefined),
                            ?assertEqual(ExpectedRange, Range),
                            {ok, Acc + 1}
                        end,
                        0,
                        []
                    ),
                    ?assertEqual(DocsPerRange, DocsInShard),
                    ok = couch_db:close(Db),
                    ok = file:delete(FilePath)
                end,
                TMap
            )
        end)}.

should_fail_copy_local_on_missing_source() ->
    DbName = ?tempdb(),
    Ranges = make_ranges(2),
    TMap = make_targets(Ranges),
    PickFun = fun fake_pickfun/3,
    Response = couch_db_split:copy_local_docs(DbName, TMap, PickFun),
    ?assertEqual({error, missing_source}, Response).

cleanup_target_test_() ->
    {
        setup,
        fun test_util:start_couch/0,
        fun test_util:stop/1,
        [
            {
                setup,
                fun setup/0,
                fun teardown/1,
                fun should_delete_existing_targets/1
            },
            {"Should return error on missing source",
                fun should_fail_cleanup_target_on_missing_source/0}
        ]
    }.

should_delete_existing_targets(SourceName) ->
    {ok, ExpectSeq} = create_docs(SourceName, 100),
    Ranges = make_ranges(2),
    TMap = make_targets(Ranges),
    PickFun = make_pickfun(50),
    ?_test(begin
        {ok, UpdateSeq} = couch_db_split:split(SourceName, TMap, PickFun),
        ?assertEqual(ExpectSeq, UpdateSeq),
        maps:map(
            fun(_Range, TargetName) ->
                FilePath = couch_util:with_db(TargetName, fun(Db) ->
                    couch_db:get_filepath(Db)
                end),
                ?assertMatch({ok, _}, file:read_file_info(FilePath)),
                Response = couch_db_split:cleanup_target(SourceName, TargetName),
                ?assertEqual(ok, Response),
                ?assertEqual({error, enoent}, file:read_file_info(FilePath))
            end,
            TMap
        )
    end).

should_fail_cleanup_target_on_missing_source() ->
    SourceName = ?tempdb(),
    TargetName = ?tempdb(),
    Response = couch_db_split:cleanup_target(SourceName, TargetName),
    ?assertEqual({error, missing_source}, Response).

make_pickfun(DocsPerRange) ->
    fun(DocId, Ranges, _HashFun) ->
        Id = docid_to_integer(DocId),
        case {Id div DocsPerRange, Id rem DocsPerRange} of
            {N, 0} ->
                lists:nth(N, Ranges);
            {N, _} ->
                lists:nth(N + 1, Ranges)
        end
    end.

fake_pickfun(_, Ranges, _) ->
    hd(Ranges).

make_targets([]) ->
    maps:new();
make_targets(Ranges) ->
    Targets = lists:map(
        fun(Range) ->
            {Range, ?tempdb()}
        end,
        Ranges
    ),
    maps:from_list(Targets).

make_ranges(Q) when Q > 0 ->
    Incr = (2 bsl 31) div Q,
    lists:map(
        fun
            (End) when End >= ?RINGTOP - 1 ->
                [End - Incr, ?RINGTOP - 1];
            (End) ->
                [End - Incr, End - 1]
        end,
        lists:seq(Incr, ?RINGTOP, Incr)
    );
make_ranges(_) ->
    [].

create_docs(DbName, 0) ->
    couch_util:with_db(DbName, fun(Db) ->
        UpdateSeq = couch_db:get_update_seq(Db),
        {ok, UpdateSeq}
    end);
create_docs(DbName, DocNum) ->
    Docs = lists:foldl(
        fun(I, Acc) ->
            [create_doc(I), create_local_doc(I) | Acc]
        end,
        [],
        lists:seq(DocNum, 1, -1)
    ),
    couch_util:with_db(DbName, fun(Db) ->
        {ok, _Result} = couch_db:update_docs(Db, Docs),
        {ok, Db1} = couch_db:reopen(Db),
        UpdateSeq = couch_db:get_update_seq(Db1),
        {ok, UpdateSeq}
    end).

create_doc(I) ->
    create_prefix_id_doc(I, "").

create_local_doc(I) ->
    create_prefix_id_doc(I, "_local/").

create_prefix_id_doc(I, Prefix) ->
    Id = iolist_to_binary(io_lib:format(Prefix ++ "~3..0B", [I])),
    couch_doc:from_json_obj({[{<<"_id">>, Id}, {<<"value">>, I}]}).

docid_to_integer(<<"_local/", DocId/binary>>) ->
    docid_to_integer(DocId);
docid_to_integer(DocId) ->
    list_to_integer(binary_to_list(DocId)).
