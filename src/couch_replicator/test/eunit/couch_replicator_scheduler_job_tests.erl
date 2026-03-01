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

-module(couch_replicator_scheduler_job_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(CHANGES_READER, couch_replicator_changes_reader).
-define(DOC(Id), #{<<"_id">> => integer_to_binary(Id)}).
-define(DOCS(StartId, StopId), #{
    <<"docs">> =>
        [
            #{<<"_id">> => integer_to_binary(Id)}
         || Id <- lists:seq(StartId, StopId)
        ]
}).
-define(JSON, {"Content-Type", "application/json"}).

setup_replicator_db(Prefix) ->
    RepDb =
        case Prefix of
            <<>> -> <<"_replicator">>;
            <<_/binary>> -> <<Prefix/binary, "/_replicator">>
        end,
    Opts = [{q, 1}, {n, 1}, ?ADMIN_CTX],
    case fabric:create_db(RepDb, Opts) of
        ok -> ok;
        {error, file_exists} -> ok
    end,
    RepDb.

setup_main_replicator_db() ->
    {Ctx, {Source, Target}} = couch_replicator_test_helper:test_setup(),
    RepDb = setup_replicator_db(<<>>),
    meck:new(?CHANGES_READER, [passthrough]),
    {Ctx, {RepDb, Source, Target}}.

setup_prefixed_replicator_db() ->
    {Ctx, {Source, Target}} = couch_replicator_test_helper:test_setup(),
    RepDb = setup_replicator_db(?tempdb()),
    meck:new(?CHANGES_READER, [passthrough]),
    {Ctx, {RepDb, Source, Target}}.

teardown({Ctx, {RepDb, Source, Target}}) ->
    ok = fabric:delete_db(RepDb, [?ADMIN_CTX]),
    config:delete("replicator", "update_docs", _Persist = false),
    couch_replicator_test_helper:test_teardown({Ctx, {Source, Target}}).

scheduler_job_replicate_test_() ->
    {
        foreach,
        fun setup_main_replicator_db/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_replicate_without_since_seq),
            ?TDEF_FE(t_replicate_with_since_seq_only),
            ?TDEF_FE(t_replicate_with_checkpoint_and_since_seq)
        ]
    }.

scheduler_job_main_db_test_() ->
    {
        foreach,
        fun setup_main_replicator_db/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_replicator_without_since_seq, 15),
            ?TDEF_FE(t_replicator_with_since_seq_only, 15),
            ?TDEF_FE(t_replicator_with_checkpoint_and_since_seq, 25)
        ]
    }.

scheduler_job_prefixed_db_test_() ->
    {
        foreach,
        fun setup_prefixed_replicator_db/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_replicator_without_since_seq, 15),
            ?TDEF_FE(t_replicator_with_since_seq_only, 15),
            ?TDEF_FE(t_replicator_with_checkpoint_and_since_seq, 25)
        ]
    }.

t_replicate_without_since_seq({_Ctx, {_RepDb, Source, Target}}) ->
    ok = create_docs(Source, ?DOCS(1, 3)),
    {ok, RepId1} = replicate(Source, Target),
    ?assertEqual(1, num_calls(read_changes, ['_', 0, '_', '_', '_'])),
    ?assertMatch(#{<<"total_rows">> := 3}, all_docs(Target)),

    meck:reset(?CHANGES_READER),
    ok = create_doc(Source, ?DOC(4)),
    {ok, RepId2} = replicate(Source, Target),
    Changes = changes(Source),
    Seq = sequence(?DOC(3), Changes),
    ?assertEqual(RepId1, RepId2),
    ?assertEqual(1, num_calls(read_changes, ['_', Seq, '_', '_', '_'])),
    ?assertMatch(#{<<"total_rows">> := 4}, all_docs(Target)).

t_replicate_with_since_seq_only({_Ctx, {_RepDb, Source, Target}}) ->
    ok = create_docs(Source, ?DOCS(1, 3)),
    Changes = changes(Source),
    SinceSeq = sequence(?DOC(2), Changes),
    replicate(Source, Target, SinceSeq),
    ?assertEqual(1, num_calls(read_changes, ['_', SinceSeq, '_', '_', '_'])),
    ?assertMatch(#{<<"total_rows">> := 1}, all_docs(Target)).

t_replicate_with_checkpoint_and_since_seq({_Ctx, {_RepDb, Source, Target}}) ->
    ok = create_docs(Source, ?DOCS(1, 3)),
    Changes = changes(Source),
    SinceSeq = sequence(?DOC(2), Changes),
    {ok, RepId1} = replicate(Source, Target, SinceSeq),
    ?assertEqual(1, num_calls(read_changes, ['_', SinceSeq, '_', '_', '_'])),
    ?assertMatch(#{<<"total_rows">> := 1}, all_docs(Target)),

    % Replicate with the checkpoint and ignore `since_seq`.
    meck:reset(?CHANGES_READER),
    ok = create_doc(Source, ?DOC(4)),
    {ok, RepId2} = replicate(Source, Target, SinceSeq),
    Seq = sequence(?DOC(3), Changes),
    ?assertEqual(RepId1, RepId2),
    ?assertEqual(1, num_calls(read_changes, ['_', Seq, '_', '_', '_'])),
    ?assertMatch(#{<<"total_rows">> := 2}, all_docs(Target)),

    % No checkpoint exist, so replicate with the `since_seq`.
    meck:reset(?CHANGES_READER),
    ok = create_docs(Source, ?DOCS(5, 7)),
    Changes1 = changes(Source),
    SinceSeq1 = sequence(?DOC(6), Changes1),
    {ok, RepId3} = replicate(Source, Target, SinceSeq1),
    ?assertNotEqual(RepId2, RepId3),
    ?assertEqual(1, num_calls(read_changes, ['_', SinceSeq1, '_', '_', '_'])),
    ?assertMatch(#{<<"total_rows">> := 3}, all_docs(Target)).

t_replicator_without_since_seq({_Ctx, {RepDb, Source, Target}}) ->
    ok = create_docs(Source, ?DOCS(1, 3)),
    SourceUrl = couch_replicator_test_helper:cluster_db_url(Source),
    TargetUrl = couch_replicator_test_helper:cluster_db_url(Target),
    RepDoc = #{<<"source">> => SourceUrl, <<"target">> => TargetUrl},
    {RepDocId, RepId1} = persistent_replicate(RepDb, RepDoc),
    ?assertEqual(1, num_calls(read_changes, ['_', 0, '_', '_', '_'])),
    ?assertMatch(#{<<"total_rows">> := 3}, all_docs(Target)),
    ?assertEqual(null, scheduler_docs_id(RepDb, RepDocId)),

    meck:reset(?CHANGES_READER),
    ok = create_doc(Source, ?DOC(4)),
    {RepDocId2, RepId2} = persistent_replicate(RepDb, RepDoc),
    Changes = changes(Source),
    Seq = sequence(?DOC(3), Changes),
    ?assertEqual(RepId1, RepId2),
    ?assertEqual(1, num_calls(read_changes, ['_', Seq, '_', '_', '_'])),
    ?assertMatch(#{<<"total_rows">> := 4}, all_docs(Target)),
    ?assertEqual(null, scheduler_docs_id(RepDb, RepDocId2)).

t_replicator_with_since_seq_only({_Ctx, {RepDb, Source, Target}}) ->
    ok = create_docs(Source, ?DOCS(1, 3)),
    Changes = changes(Source),
    SinceSeq = sequence(?DOC(2), Changes),
    SourceUrl = couch_replicator_test_helper:cluster_db_url(Source),
    TargetUrl = couch_replicator_test_helper:cluster_db_url(Target),
    RepDoc = #{<<"source">> => SourceUrl, <<"target">> => TargetUrl, <<"since_seq">> => SinceSeq},
    {RepDocId, _} = persistent_replicate(RepDb, RepDoc),
    ?assertEqual(1, num_calls(read_changes, ['_', SinceSeq, '_', '_', '_'])),
    ?assertMatch(#{<<"total_rows">> := 1}, all_docs(Target)),
    ?assertEqual(null, scheduler_docs_id(RepDb, RepDocId)).

t_replicator_with_checkpoint_and_since_seq({_Ctx, {RepDb, Source, Target}}) ->
    ok = create_docs(Source, ?DOCS(1, 3)),
    Changes = changes(Source),
    SinceSeq = sequence(?DOC(2), Changes),
    SourceUrl = couch_replicator_test_helper:cluster_db_url(Source),
    TargetUrl = couch_replicator_test_helper:cluster_db_url(Target),
    RepDoc = #{<<"source">> => SourceUrl, <<"target">> => TargetUrl, <<"since_seq">> => SinceSeq},
    {RepDocId, RepId1} = persistent_replicate(RepDb, RepDoc),
    ?assertEqual(1, num_calls(read_changes, ['_', SinceSeq, '_', '_', '_'])),
    ?assertMatch(#{<<"total_rows">> := 1}, all_docs(Target)),
    ?assertEqual(null, scheduler_docs_id(RepDb, RepDocId)),

    % Old replication: checkpoint exist, so replicate with the checkpoint.
    meck:reset(?CHANGES_READER),
    ok = create_doc(Source, ?DOC(4)),
    {RepDocId2, RepId2} = persistent_replicate(RepDb, RepDoc),
    Seq = sequence(?DOC(3), Changes),
    ?assertEqual(RepId1, RepId2),
    ?assertEqual(1, num_calls(read_changes, ['_', Seq, '_', '_', '_'])),
    ?assertMatch(#{<<"total_rows">> := 2}, all_docs(Target)),
    ?assertEqual(null, scheduler_docs_id(RepDb, RepDocId2)),

    % New replication: no checkpoint exist, so replicate with the `since_seq`.
    meck:reset(?CHANGES_READER),
    ok = create_docs(Source, ?DOCS(5, 7)),
    Changes1 = changes(Source),
    SinceSeq1 = sequence(?DOC(6), Changes1),
    RepDoc1 = #{<<"source">> => SourceUrl, <<"target">> => TargetUrl, <<"since_seq">> => SinceSeq1},
    {RepDocId3, RepId3} = persistent_replicate(RepDb, RepDoc1),
    ?assertNotEqual(RepId2, RepId3),
    ?assertEqual(1, num_calls(read_changes, ['_', SinceSeq1, '_', '_', '_'])),
    ?assertMatch(#{<<"total_rows">> := 3}, all_docs(Target)),
    ?assertEqual(null, scheduler_docs_id(RepDb, RepDocId3)).

%%%%%%%%%%%%%%%%%%%% Utility Functions %%%%%%%%%%%%%%%%%%%%
url(UrlPath) ->
    binary_to_list(couch_replicator_test_helper:cluster_db_url(UrlPath)).

create_docs(DbName, Docs) ->
    case req(post, url(DbName) ++ "/_bulk_docs", Docs) of
        {201, _} -> ok;
        Error -> error({failed_to_create_docs, DbName, Error})
    end.

create_doc(DbName, Doc) ->
    case req(post, url(DbName), Doc) of
        {201, _} -> ok;
        Error -> error({failed_to_create_doc, DbName, Error})
    end.

all_docs(DbName) ->
    {200, Res} = req(get, url(DbName) ++ "/_all_docs"),
    ?assert(maps:is_key(<<"offset">>, Res)),
    ?assert(maps:is_key(<<"rows">>, Res)),
    ?assert(maps:is_key(<<"total_rows">>, Res)),
    Res.

changes(DbName) ->
    {200, Res} = req(get, url(DbName) ++ "/_changes"),
    ?assert(maps:is_key(<<"last_seq">>, Res)),
    ?assert(maps:is_key(<<"pending">>, Res)),
    ?assert(maps:is_key(<<"results">>, Res)),
    Res.

sequence(Doc, Changes) ->
    #{<<"_id">> := DocId} = Doc,
    #{<<"results">> := Results} = Changes,
    case lists:search(fun(M) -> maps:get(<<"id">>, M) == DocId end, Results) of
        {value, #{<<"seq">> := Seq}} -> Seq;
        false -> not_found
    end.

replicate(RepObject) ->
    couch_replicator_test_helper:replicate(RepObject).

replicate(Source, Target) ->
    replicate(#{
        <<"source">> => ?l2b(url(Source)),
        <<"target">> => ?l2b(url(Target))
    }).

replicate(Source, Target, SinceSeq) ->
    replicate(#{
        <<"source">> => ?l2b(url(Source)),
        <<"target">> => ?l2b(url(Target)),
        <<"since_seq">> => SinceSeq
    }).

persistent_replicate(RepDb, RepDoc) ->
    RepDocId = ?docid(),
    rep_toggle(stop),
    RepDocUrl = rep_doc_url(RepDb, RepDocId),
    {201, _} = req(put, RepDocUrl, RepDoc),
    RepId = scheduler_docs_id(RepDb, RepDocId),
    rep_toggle(start),
    ok = test_util:wait(
        fun() ->
            case req(get, RepDocUrl) of
                {200, #{<<"_replication_state">> := <<"completed">>}} -> ok;
                {_, #{}} -> wait
            end
        end,
        7000,
        1000
    ),
    {RepDocId, RepId}.

rep_toggle(start) ->
    config:set("replicator", "max_jobs", "500", false);
rep_toggle(stop) ->
    config:set("replicator", "max_jobs", "0", false).

rep_doc_url(RepDb, DocId) when is_binary(RepDb) ->
    rep_doc_url(binary_to_list(RepDb), DocId);
rep_doc_url(RepDb, DocId) when is_binary(DocId) ->
    rep_doc_url(RepDb, binary_to_list(DocId));
rep_doc_url(RepDb, DocId) when is_list(RepDb), is_list(DocId) ->
    UrlQuotedRepDb = mochiweb_util:quote_plus(RepDb),
    url(UrlQuotedRepDb ++ "/" ++ DocId).

scheduler_docs_id(RepDb, RepDocId) ->
    RepDocIdBin = ?l2b(RepDocId),
    SchedulerDocsUrl =
        case RepDb of
            <<"_replicator">> -> url(<<"/_scheduler/docs">>);
            <<_/binary>> -> url(<<"/_scheduler/docs/", RepDb/binary>>)
        end,
    Docs = test_util:wait(
        fun() ->
            case req(get, SchedulerDocsUrl) of
                {200, #{<<"docs">> := [_ | _] = Docs}} -> Docs;
                {200, #{<<"docs">> := []}} -> wait
            end
        end,
        7000,
        1000
    ),
    [RepId] = [Id || #{<<"doc_id">> := DocId, <<"id">> := Id} <- Docs, DocId =:= RepDocIdBin],
    RepId.

req(Method, Url) ->
    Headers = [?JSON],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers),
    {Code, jiffy:decode(Res, [return_maps])}.

req(Method, Url, #{} = Body) ->
    req(Method, Url, jiffy:encode(Body));
req(Method, Url, Body) ->
    Headers = [?JSON],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, jiffy:decode(Res, [return_maps])}.

num_calls(Fun, Args) ->
    meck:num_calls(?CHANGES_READER, Fun, Args).
