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

-module(couch_replicator_many_leaves_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DOCS_CONFLICTS, [
    {<<"doc1">>, 10},
    % use some _design docs as well to test the special handling for them
    {<<"_design/doc2">>, 100},
    % a number > MaxURLlength (7000) / length(DocRevisionString)
    {<<"doc3">>, 210}
]).
-define(NUM_ATTS, 2).
-define(TIMEOUT_EUNIT, 60).
-define(i2l(I), integer_to_list(I)).
-define(io2b(Io), iolist_to_binary(Io)).

docs_with_many_leaves_test_() ->
    {
        "Replicate documents with many leaves",
        {
            foreach,
            fun couch_replicator_test_helper:test_setup/0,
            fun couch_replicator_test_helper:test_teardown/1,
            [
                ?TDEF_FE(should_populate_replicate_compact, ?TIMEOUT_EUNIT)
            ]
        }
    }.

docs_with_many_leaves_test_winning_revs_only_test_() ->
    {
        "Replicate winning revs only for documents with many leaves",
        {
            foreach,
            fun couch_replicator_test_helper:test_setup/0,
            fun couch_replicator_test_helper:test_teardown/1,
            [
                ?TDEF_FE(should_replicate_winning_revs_only, ?TIMEOUT_EUNIT)
            ]
        }
    }.

should_populate_replicate_compact({_Ctx, {Source, Target}}) ->
    populate_db(Source),
    replicate(Source, Target, []),
    verify_target(Source, Target, ?DOCS_CONFLICTS, all_revs),
    add_attachments(Source, ?NUM_ATTS, ?DOCS_CONFLICTS),
    replicate(Source, Target, []),
    verify_target(Source, Target, ?DOCS_CONFLICTS, all_revs).

should_replicate_winning_revs_only({_Ctx, {Source, Target}}) ->
    populate_db(Source),
    replicate(Source, Target, [{<<"winning_revs_only">>, true}]),
    verify_target(Source, Target, ?DOCS_CONFLICTS, winning_revs),
    add_attachments(Source, ?NUM_ATTS, ?DOCS_CONFLICTS),
    replicate(Source, Target, [{<<"winning_revs_only">>, true}]),
    verify_target(Source, Target, ?DOCS_CONFLICTS, winning_revs).

replicate(Source, Target, Options) ->
    RepObj = {
        [
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)}
        ] ++ Options
    },
    couch_replicator_test_helper:replicate(RepObj).

populate_db(DbName) ->
    lists:foreach(
        fun({DocId, NumConflicts}) ->
            Value = <<"0">>,
            Doc = #doc{
                id = DocId,
                body = {[{<<"value">>, Value}]}
            },
            {ok, {Pos, Rev}} = fabric:update_doc(DbName, Doc, [?ADMIN_CTX]),
            % Update first initial doc rev twice to ensure it's always a winner
            Doc1 = Doc#doc{revs = {Pos, [Rev]}},
            {ok, _} = fabric:update_doc(DbName, Doc1, [?ADMIN_CTX]),
            {ok, _} = add_doc_siblings(DbName, DocId, NumConflicts)
        end,
        ?DOCS_CONFLICTS
    ).

add_doc_siblings(Db, DocId, NumLeaves) when NumLeaves > 0 ->
    add_doc_siblings(Db, DocId, NumLeaves, [], []).

add_doc_siblings(Db, _DocId, 0, AccDocs, AccRevs) ->
    {ok, []} = fabric:update_docs(Db, AccDocs, [?REPLICATED_CHANGES, ?ADMIN_CTX]),
    {ok, AccRevs};
add_doc_siblings(Db, DocId, NumLeaves, AccDocs, AccRevs) ->
    Value = ?l2b(?i2l(NumLeaves)),
    Rev = couch_hash:md5_hash(Value),
    Doc = #doc{
        id = DocId,
        revs = {1, [Rev]},
        body = {[{<<"value">>, Value}]}
    },
    add_doc_siblings(
        Db,
        DocId,
        NumLeaves - 1,
        [Doc | AccDocs],
        [{1, Rev} | AccRevs]
    ).

verify_target(_SourceDb, _TargetDb, [], _Mode) ->
    ok;
verify_target(SourceDb, TargetDb, [{DocId, NumConflicts} | Rest], all_revs) ->
    SourceLookups = open_revs_conflicts(SourceDb, DocId),
    TargetLookups = open_revs_conflicts(TargetDb, DocId),
    SourceDocs = [Doc || {ok, Doc} <- SourceLookups],
    TargetDocs = [Doc || {ok, Doc} <- TargetLookups],
    Total = NumConflicts + 1,
    ?assertEqual(Total, length(TargetDocs)),
    lists:foreach(
        fun({SourceDoc, TargetDoc}) ->
            SourceJson = couch_doc:to_json_obj(SourceDoc, [attachments]),
            TargetJson = couch_doc:to_json_obj(TargetDoc, [attachments]),
            ?assertEqual(SourceJson, TargetJson)
        end,
        lists:zip(SourceDocs, TargetDocs)
    ),
    verify_target(SourceDb, TargetDb, Rest, all_revs);
verify_target(SourceDb, TargetDb, [{DocId, _NumConflicts} | Rest], winning_revs) ->
    SourceWinner = open_doc(SourceDb, DocId),
    TargetWinner = open_doc(TargetDb, DocId),
    SourceWinnerJson = couch_doc:to_json_obj(SourceWinner, [attachments]),
    TargetWinnerJson = couch_doc:to_json_obj(TargetWinner, [attachments]),
    % Source winner is the same as the target winner
    ?assertEqual(SourceWinnerJson, TargetWinnerJson),
    TargetAll = open_revs_conflicts(TargetDb, DocId),
    % There is only one version on the target
    ?assert(length(TargetAll) == 1),
    verify_target(SourceDb, TargetDb, Rest, winning_revs).

add_attachments(_SourceDb, _NumAtts, []) ->
    ok;
add_attachments(SourceDb, NumAtts, [{DocId, NumConflicts} | Rest]) ->
    SourceLookups = open_revs(SourceDb, DocId, []),
    SourceDocs = [Doc || {ok, Doc} <- SourceLookups],
    Total = NumConflicts + 1,
    ?assertEqual(Total, length(SourceDocs)),
    NewDocs = lists:foldl(
        fun(#doc{atts = Atts, revs = {Pos, [Rev | _]}} = Doc, Acc) ->
            NewAtts = lists:foldl(
                fun(I, AttAcc) ->
                    [make_att(I, Pos, Rev, 100) | AttAcc]
                end,
                [],
                lists:seq(1, NumAtts)
            ),
            [Doc#doc{atts = Atts ++ NewAtts} | Acc]
        end,
        [],
        SourceDocs
    ),
    {ok, UpdateResults} = fabric:update_docs(SourceDb, NewDocs, [?ADMIN_CTX]),
    NewRevs = [R || {ok, R} <- UpdateResults],
    ?assertEqual(length(NewDocs), length(NewRevs)),
    add_attachments(SourceDb, NumAtts, Rest).

make_att(Id, Pos, Rev, Size) ->
    AttData = crypto:strong_rand_bytes(Size),
    RevStr = couch_doc:rev_to_str({Pos, Rev}),
    couch_att:new([
        {name, ?io2b(["att_", ?i2l(Id), "_", RevStr])},
        {type, <<"application/foobar">>},
        {att_len, byte_size(AttData)},
        {data, AttData}
    ]).

db_url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).

open_revs_conflicts(DbName, Id) ->
    Opts = [conflicts, deleted_conflicts],
    {ok, Lookups} = fabric:open_revs(DbName, Id, all, Opts),
    Lookups.

open_revs(DbName, Id, Opts) ->
    {ok, Lookups} = fabric:open_revs(DbName, Id, all, Opts),
    Lookups.

open_doc(DbName, Id) ->
    {ok, Doc} = fabric:open_doc(DbName, Id, [?ADMIN_CTX]),
    Doc.
