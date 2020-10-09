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

-module(fabric2_doc_crud_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fabric2.hrl").
-include("fabric2_test.hrl").


doc_crud_test_() ->
    {
        "Test document CRUD operations",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            with([
                ?TDEF(open_missing_doc),
                ?TDEF(create_new_doc),
                ?TDEF(create_ddoc_basic),
                ?TDEF(create_ddoc_requires_admin),
                ?TDEF(create_ddoc_requires_validation),
                ?TDEF(create_ddoc_requires_compilation),
                ?TDEF(can_create_a_partitioned_ddoc),
                ?TDEF(update_doc_basic),
                ?TDEF(update_ddoc_basic),
                ?TDEF(update_doc_replicated),
                ?TDEF(update_doc_replicated_add_conflict),
                ?TDEF(update_doc_replicated_changes_winner),
                ?TDEF(update_doc_replicated_extension),
                ?TDEF(update_doc_replicate_existing_rev),
                ?TDEF(update_winning_conflict_branch),
                ?TDEF(update_non_winning_conflict_branch),
                ?TDEF(delete_doc_basic),
                ?TDEF(delete_changes_winner),
                ?TDEF(recreate_doc_basic),
                ?TDEF(conflict_on_create_new_with_rev),
                ?TDEF(conflict_on_update_with_no_rev),
                ?TDEF(allow_create_new_as_deleted),
                ?TDEF(conflict_on_recreate_as_deleted),
                ?TDEF(conflict_on_extend_deleted),
                ?TDEF(open_doc_revs_basic),
                ?TDEF(open_doc_revs_all),
                ?TDEF(open_doc_revs_latest),
                ?TDEF(get_missing_revs_basic),
                ?TDEF(get_missing_revs_on_missing_doc),
                ?TDEF(open_missing_local_doc),
                ?TDEF(create_local_doc_basic),
                ?TDEF(update_local_doc_basic),
                ?TDEF(delete_local_doc_basic),
                ?TDEF(recreate_local_doc),
                ?TDEF(create_local_doc_bad_rev),
                ?TDEF(create_local_doc_random_rev),
                ?TDEF(create_a_large_local_doc),
                ?TDEF(create_2_large_local_docs),
                ?TDEF(local_doc_with_previous_encoding),
                ?TDEF(before_doc_update_skips_local_docs),
                ?TDEF(open_doc_opts)
            ])
        }
    }.


setup() ->
    Ctx = test_util:start_couch([fabric]),
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    {Db, Ctx}.


cleanup({Db, Ctx}) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []),
    test_util:stop_couch(Ctx).


open_missing_doc({Db, _}) ->
    ?assertEqual({not_found, missing}, fabric2_db:open_doc(Db, <<"foo">>)).


create_new_doc({Db, _}) ->
    Doc = #doc{
        id = fabric2_util:uuid(),
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {RevPos, Rev}} = fabric2_db:update_doc(Db, Doc),
    NewDoc = Doc#doc{revs = {RevPos, [Rev]}},
    ?assertEqual({ok, NewDoc}, fabric2_db:open_doc(Db, Doc#doc.id)).


create_ddoc_basic({Db, _}) ->
    UUID = fabric2_util:uuid(),
    DDocId = <<"_design/", UUID/binary>>,
    Doc = #doc{
        id = DDocId,
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {RevPos, Rev}} = fabric2_db:update_doc(Db, Doc),
    NewDoc = Doc#doc{revs = {RevPos, [Rev]}},
    ?assertEqual({ok, NewDoc}, fabric2_db:open_doc(Db, Doc#doc.id)).


can_create_a_partitioned_ddoc({Db, _}) ->
    UUID = fabric2_util:uuid(),
    DDocId = <<"_design/", UUID/binary>>,
    Doc = #doc{
        id = DDocId,
        body = {[
            {<<"options">>, {[{<<"partitioned">>, true}]}},
            {<<"views">>, {[
                {<<"foo">>, {[
                    {<<"map">>, <<"function(doc) {}">>}
                ]}}
            ]}}
        ]}
    },
    ?assertMatch({ok, {_, _}}, fabric2_db:update_doc(Db, Doc)).


create_ddoc_requires_admin({Db, _}) ->
    Db2 = fabric2_db:set_user_ctx(Db, #user_ctx{}),
    UUID = fabric2_util:uuid(),
    DDocId = <<"_design/", UUID/binary>>,
    Doc = #doc{
        id = DDocId,
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    ?assertThrow({unauthorized, _}, fabric2_db:update_doc(Db2, Doc)).


create_ddoc_requires_validation({Db, _}) ->
    UUID = fabric2_util:uuid(),
    DDocId = <<"_design/", UUID/binary>>,
    Doc = #doc{
        id = DDocId,
        body = {[
            {<<"views">>, {[
                {<<"foo">>, {[
                    {<<"map">>, <<"function(doc) {}">>},
                    {<<"reduce">>, <<"_not_a_builtin_reduce">>}
                ]}}
            ]}}
        ]}
    },
    ?assertThrow(
            {bad_request, invalid_design_doc, _},
            fabric2_db:update_doc(Db, Doc)
        ).


create_ddoc_requires_compilation({Db, _}) ->
    UUID = fabric2_util:uuid(),
    DDocId = <<"_design/", UUID/binary>>,
    Doc = #doc{
        id = DDocId,
        body = {[
            {<<"language">>, <<"javascript">>},
            {<<"views">>, {[
                {<<"foo">>, {[
                    {<<"map">>, <<"Hopefully this is invalid JavaScript">>}
                ]}}
            ]}}
        ]}
    },
    ?assertThrow(
            {bad_request, compilation_error, _},
            fabric2_db:update_doc(Db, Doc)
        ).


update_doc_basic({Db, _}) ->
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        body = {[{<<"state">>, 1}]}
    },
    {ok, {Pos1, Rev1}} = fabric2_db:update_doc(Db, Doc1),
    Doc2 = Doc1#doc{
        revs = {Pos1, [Rev1]},
        body = {[{<<"state">>, 2}]}
    },
    {ok, {Pos2, Rev2}} = fabric2_db:update_doc(Db, Doc2),
    Doc3 = Doc2#doc{
        revs = {Pos2, [Rev2, Rev1]}
    },
    ?assertEqual({ok, Doc3}, fabric2_db:open_doc(Db, Doc2#doc.id)).


update_ddoc_basic({Db, _}) ->
    UUID = fabric2_util:uuid(),
    DDocId = <<"_design/", UUID/binary>>,
    Doc1 = #doc{
        id = DDocId,
        body = {[{<<"state">>, 1}]}
    },
    {ok, {Pos1, Rev1}} = fabric2_db:update_doc(Db, Doc1),
    Doc2 = Doc1#doc{
        revs = {Pos1, [Rev1]},
        body = {[{<<"state">>, 2}]}
    },
    {ok, {Pos2, Rev2}} = fabric2_db:update_doc(Db, Doc2),
    Doc3 = Doc2#doc{
        revs = {Pos2, [Rev2, Rev1]}
    },
    ?assertEqual({ok, Doc3}, fabric2_db:open_doc(Db, Doc2#doc.id)).


update_doc_replicated({Db, _}) ->
    Doc = #doc{
        id = fabric2_util:uuid(),
        revs = {2, [fabric2_util:uuid(), fabric2_util:uuid()]},
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc, [replicated_changes]),
    ?assertEqual({ok, Doc}, fabric2_db:open_doc(Db, Doc#doc.id)).


update_doc_replicated_add_conflict({Db, _}) ->
    [Rev1, Rev2, Rev3] = lists:sort([
            fabric2_util:uuid(),
            fabric2_util:uuid(),
            fabric2_util:uuid()
        ]),
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        revs = {2, [Rev3, Rev1]},
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc1, [replicated_changes]),
    ?assertEqual({ok, Doc1}, fabric2_db:open_doc(Db, Doc1#doc.id)),
    Doc2 = Doc1#doc{
        revs = {2, [Rev2, Rev1]},
        body = {[{<<"bar">>, <<"foo">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc2, [replicated_changes]),
    ?assertEqual({ok, Doc1}, fabric2_db:open_doc(Db, Doc2#doc.id)).


update_doc_replicated_changes_winner({Db, _}) ->
    [Rev1, Rev2, Rev3] = lists:sort([
            fabric2_util:uuid(),
            fabric2_util:uuid(),
            fabric2_util:uuid()
        ]),
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        revs = {2, [Rev2, Rev1]},
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc1, [replicated_changes]),
    ?assertEqual({ok, Doc1}, fabric2_db:open_doc(Db, Doc1#doc.id)),
    Doc2 = Doc1#doc{
        revs = {2, [Rev3, Rev1]},
        body = {[{<<"bar">>, <<"foo">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc2, [replicated_changes]),
    ?assertEqual({ok, Doc2}, fabric2_db:open_doc(Db, Doc2#doc.id)).


update_doc_replicated_extension({Db, _}) ->
    % No sort necessary and avoided on purpose to
    % demonstrate that this is not sort dependent
    Rev1 = fabric2_util:uuid(),
    Rev2 = fabric2_util:uuid(),
    Rev3 = fabric2_util:uuid(),
    Rev4 = fabric2_util:uuid(),
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        revs = {2, [Rev2, Rev1]},
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc1, [replicated_changes]),
    Doc2 = Doc1#doc{
        revs = {4, [Rev4, Rev3, Rev2]},
        body = {[{<<"bar">>, <<"foo">>}]}
    },
    {ok, {4, _}} = fabric2_db:update_doc(Db, Doc2, [replicated_changes]),
    {ok, Doc3} = fabric2_db:open_doc(Db, Doc2#doc.id),
    ?assertEqual({4, [Rev4, Rev3, Rev2, Rev1]}, Doc3#doc.revs),
    ?assertEqual(Doc2#doc{revs = undefined}, Doc3#doc{revs = undefined}).


update_doc_replicate_existing_rev({Db, _}) ->
    Rev1 = fabric2_util:uuid(),
    Rev2 = fabric2_util:uuid(),
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        revs = {2, [Rev2, Rev1]},
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc1, [replicated_changes]),
    {ok, []} = fabric2_db:update_docs(Db, [Doc1], [replicated_changes]),
    ?assertEqual({ok, Doc1}, fabric2_db:open_doc(Db, Doc1#doc.id)).


update_winning_conflict_branch({Db, _}) ->
    [Rev1, Rev2, Rev3] = lists:sort([
            fabric2_util:uuid(),
            fabric2_util:uuid(),
            fabric2_util:uuid()
        ]),
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        revs = {2, [Rev3, Rev1]},
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc1, [replicated_changes]),
    Doc2 = Doc1#doc{
        revs = {2, [Rev2, Rev1]},
        body = {[{<<"bar">>, <<"foo">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc2, [replicated_changes]),
    % Update the winning branch
    Doc3 = Doc1#doc{
        revs = {2, [Rev3, Rev1]},
        body = {[{<<"baz">>, 2}]}
    },
    {ok, {3, Rev4}} = fabric2_db:update_doc(Db, Doc3),
    {ok, Doc4} = fabric2_db:open_doc(Db, Doc3#doc.id),
    % Assert we've got the correct winner
    ?assertEqual({3, [Rev4, Rev3, Rev1]}, Doc4#doc.revs),
    ?assertEqual(Doc3#doc{revs = undefined}, Doc4#doc{revs = undefined}).


update_non_winning_conflict_branch({Db, _}) ->
    [Rev1, Rev2, Rev3] = lists:sort([
            fabric2_util:uuid(),
            fabric2_util:uuid(),
            fabric2_util:uuid()
        ]),
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        revs = {2, [Rev3, Rev1]},
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc1, [replicated_changes]),
    Doc2 = Doc1#doc{
        revs = {2, [Rev2, Rev1]},
        body = {[{<<"bar">>, <<"foo">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc2, [replicated_changes]),
    % Update the non winning branch
    Doc3 = Doc1#doc{
        revs = {2, [Rev2, Rev1]},
        body = {[{<<"baz">>, 2}]}
    },
    {ok, {3, Rev4}} = fabric2_db:update_doc(Db, Doc3),
    {ok, Doc4} = fabric2_db:open_doc(Db, Doc3#doc.id),
    % Assert we've got the correct winner
    ?assertEqual({3, [Rev4, Rev2, Rev1]}, Doc4#doc.revs),
    ?assertEqual(Doc3#doc{revs = undefined}, Doc4#doc{revs = undefined}).


delete_doc_basic({Db, _}) ->
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        body = {[{<<"state">>, 1}]}
    },
    {ok, {Pos1, Rev1}} = fabric2_db:update_doc(Db, Doc1),
    Doc2 = Doc1#doc{
        revs = {Pos1, [Rev1]},
        deleted = true,
        body = {[{<<"state">>, 2}]}
    },
    {ok, {Pos2, Rev2}} = fabric2_db:update_doc(Db, Doc2),
    Doc3 = Doc2#doc{revs = {Pos2, [Rev2, Rev1]}},
    ?assertEqual({ok, Doc3}, fabric2_db:open_doc(Db, Doc2#doc.id, [deleted])).


delete_changes_winner({Db, _}) ->
    [Rev1, Rev2, Rev3] = lists:sort([
            fabric2_util:uuid(),
            fabric2_util:uuid(),
            fabric2_util:uuid()
        ]),
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        revs = {2, [Rev3, Rev1]},
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc1, [replicated_changes]),
    Doc2 = Doc1#doc{
        revs = {2, [Rev2, Rev1]},
        body = {[{<<"bar">>, <<"foo">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc2, [replicated_changes]),
    % Delete the winning branch
    Doc3 = Doc1#doc{
        revs = {2, [Rev3, Rev1]},
        deleted = true,
        body = {[]}
    },
    {ok, {3, _}} = fabric2_db:update_doc(Db, Doc3),
    ?assertEqual({ok, Doc2}, fabric2_db:open_doc(Db, Doc3#doc.id)).


recreate_doc_basic({Db, _}) ->
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        body = {[{<<"state">>, 1}]}
    },
    {ok, {1, Rev1}} = fabric2_db:update_doc(Db, Doc1),
    Doc2 = Doc1#doc{
        revs = {1, [Rev1]},
        deleted = true,
        body = {[{<<"state">>, 2}]}
    },
    {ok, {2, Rev2}} = fabric2_db:update_doc(Db, Doc2),
    Doc3 = Doc1#doc{
        revs = {0, []},
        deleted = false,
        body = {[{<<"state">>, 3}]}
    },
    {ok, {3, Rev3}} = fabric2_db:update_doc(Db, Doc3),
    {ok, Doc4} = fabric2_db:open_doc(Db, Doc3#doc.id),
    ?assertEqual({3, [Rev3, Rev2, Rev1]}, Doc4#doc.revs),
    ?assertEqual(Doc3#doc{revs = undefined}, Doc4#doc{revs = undefined}).


conflict_on_create_new_with_rev({Db, _}) ->
    Doc = #doc{
        id = fabric2_util:uuid(),
        revs = {1, [fabric2_util:uuid()]},
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    ?assertThrow(conflict, fabric2_db:update_doc(Db, Doc)).


conflict_on_update_with_no_rev({Db, _}) ->
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        body = {[{<<"state">>, 1}]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc1),
    Doc2 = Doc1#doc{
        revs = {0, []},
        body = {[{<<"state">>, 2}]}
    },
    ?assertThrow(conflict, fabric2_db:update_doc(Db, Doc2)).


allow_create_new_as_deleted({Db, _}) ->
    Doc = #doc{
        id = fabric2_util:uuid(),
        deleted = true,
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {1, Rev}} = fabric2_db:update_doc(Db, Doc),
    ?assertEqual({not_found, deleted}, fabric2_db:open_doc(Db, Doc#doc.id)),
    Doc1 = Doc#doc{
        revs = {1, [Rev]}
    },
    ?assertEqual({ok, Doc1}, fabric2_db:open_doc(Db, Doc#doc.id, [deleted])),
    % Only works when the document has never existed to match CouchDB 3.x
    % behavior
    ?assertThrow(conflict, fabric2_db:update_doc(Db, Doc)).


conflict_on_recreate_as_deleted({Db, _}) ->
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        body = {[{<<"state">>, 1}]}
    },
    {ok, {Pos1, Rev1}} = fabric2_db:update_doc(Db, Doc1),
    Doc2 = Doc1#doc{
        revs = {Pos1, [Rev1]},
        deleted = true,
        body = {[{<<"state">>, 2}]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc2),
    Doc3 = Doc1#doc{
        revs = {0, []},
        deleted = true,
        body = {[{<<"state">>, 3}]}
    },
    ?assertThrow(conflict, fabric2_db:update_doc(Db, Doc3)).


conflict_on_extend_deleted({Db, _}) ->
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        body = {[{<<"state">>, 1}]}
    },
    {ok, {Pos1, Rev1}} = fabric2_db:update_doc(Db, Doc1),
    Doc2 = Doc1#doc{
        revs = {Pos1, [Rev1]},
        deleted = true,
        body = {[{<<"state">>, 2}]}
    },
    {ok, {Pos2, Rev2}} = fabric2_db:update_doc(Db, Doc2),
    Doc3 = Doc1#doc{
        revs = {Pos2, [Rev2]},
        deleted = false,
        body = {[{<<"state">>, 3}]}
    },
    ?assertThrow(conflict, fabric2_db:update_doc(Db, Doc3)).


open_doc_revs_basic({Db, _}) ->
    [Rev1, Rev2, Rev3] = lists:sort([
            fabric2_util:uuid(),
            fabric2_util:uuid(),
            fabric2_util:uuid()
        ]),
    DocId = fabric2_util:uuid(),
    Doc1 = #doc{
        id = DocId,
        revs = {2, [Rev3, Rev1]},
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc1, [replicated_changes]),
    Doc2 = Doc1#doc{
        revs = {2, [Rev2, Rev1]},
        body = {[{<<"bar">>, <<"foo">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc2, [replicated_changes]),

    {ok, [{ok, Doc3}]} = fabric2_db:open_doc_revs(Db, DocId, [{2, Rev3}], []),
    ?assertEqual(Doc1, Doc3),

    {ok, [{ok, Doc4}]} = fabric2_db:open_doc_revs(Db, DocId, [{2, Rev2}], []),
    ?assertEqual(Doc2, Doc4),

    Revs = [{2, Rev3}, {2, Rev2}, {1, Rev1}],
    {ok, Docs} = fabric2_db:open_doc_revs(Db, DocId, Revs, []),
    ?assert(length(Docs) == 3),
    ?assert(lists:member({ok, Doc1}, Docs)),
    ?assert(lists:member({ok, Doc2}, Docs)),
    ?assert(lists:member({{not_found, missing}, {1, Rev1}}, Docs)),

    % Make sure crazy madeup revisions are accepted
    MissingRevs = [{5, fabric2_util:uuid()}, {1, fabric2_util:uuid()}],
    {ok, NFMissing} = fabric2_db:open_doc_revs(Db, DocId, MissingRevs, []),
    ?assertEqual(2, length(NFMissing)),
    lists:foreach(fun(MR) ->
        ?assert(lists:member({{not_found, missing}, MR}, NFMissing))
    end, MissingRevs).


open_doc_revs_all({Db, _}) ->
    [Rev1, Rev2, Rev3] = lists:sort([
            fabric2_util:uuid(),
            fabric2_util:uuid(),
            fabric2_util:uuid()
        ]),
    DocId = fabric2_util:uuid(),
    Doc1 = #doc{
        id = DocId,
        revs = {2, [Rev3, Rev1]},
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc1, [replicated_changes]),
    Doc2 = Doc1#doc{
        revs = {2, [Rev2, Rev1]},
        body = {[{<<"bar">>, <<"foo">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc2, [replicated_changes]),

    {ok, Docs} = fabric2_db:open_doc_revs(Db, DocId, all, []),
    ?assert(length(Docs) == 2),
    ?assert(lists:member({ok, Doc1}, Docs)),
    ?assert(lists:member({ok, Doc2}, Docs)).


open_doc_revs_latest({Db, _}) ->
    [Rev1, Rev2, Rev3] = lists:sort([
            fabric2_util:uuid(),
            fabric2_util:uuid(),
            fabric2_util:uuid()
        ]),
    DocId = fabric2_util:uuid(),
    Doc1 = #doc{
        id = DocId,
        revs = {2, [Rev3, Rev1]},
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc1, [replicated_changes]),
    Doc2 = Doc1#doc{
        revs = {2, [Rev2, Rev1]},
        body = {[{<<"bar">>, <<"foo">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc2, [replicated_changes]),

    Opts = [latest],
    {ok, [{ok, Doc3}]} = fabric2_db:open_doc_revs(Db, DocId, [{2, Rev3}], Opts),
    ?assertEqual(Doc1, Doc3),

    {ok, Docs} = fabric2_db:open_doc_revs(Db, DocId, [{1, Rev1}], Opts),
    ?assert(length(Docs) == 2),
    ?assert(lists:member({ok, Doc1}, Docs)),
    ?assert(lists:member({ok, Doc2}, Docs)).


get_missing_revs_basic({Db, _}) ->
    [Rev1, Rev2, Rev3] = lists:sort([
            fabric2_util:uuid(),
            fabric2_util:uuid(),
            fabric2_util:uuid()
        ]),
    DocId = fabric2_util:uuid(),
    Doc1 = #doc{
        id = DocId,
        revs = {2, [Rev3, Rev1]},
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc1, [replicated_changes]),
    Doc2 = Doc1#doc{
        revs = {2, [Rev2, Rev1]},
        body = {[{<<"bar">>, <<"foo">>}]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc2, [replicated_changes]),

    % Check that we can find all revisions
    AllRevs = [{1, Rev1}, {2, Rev2}, {2, Rev3}],
    ?assertEqual(
            {ok, []},
            fabric2_db:get_missing_revs(Db, [{DocId, AllRevs}])
        ),

    % Check that a missing revision is found with no possible ancestors
    MissingRev = {2, fabric2_util:uuid()},
    ?assertEqual(
            {ok, [{DocId, [MissingRev], []}]},
            fabric2_db:get_missing_revs(Db, [{DocId, [MissingRev]}])
        ),

    % Check that only a missing rev is returned
    ?assertEqual(
            {ok, [{DocId, [MissingRev], []}]},
            fabric2_db:get_missing_revs(Db, [{DocId, [MissingRev | AllRevs]}])
        ),

    % Check that we can find possible ancestors
    MissingWithAncestors = {4, fabric2_util:uuid()},
    PossibleAncestors = [{2, Rev2}, {2, Rev3}],
    ?assertEqual(
            {ok, [{DocId, [MissingWithAncestors], PossibleAncestors}]},
            fabric2_db:get_missing_revs(Db, [{DocId, [MissingWithAncestors]}])
        ).


get_missing_revs_on_missing_doc({Db, _}) ->
    Revs = lists:sort([
            couch_doc:rev_to_str({1, fabric2_util:uuid()}),
            couch_doc:rev_to_str({2, fabric2_util:uuid()}),
            couch_doc:rev_to_str({800, fabric2_util:uuid()})
        ]),
    DocId = fabric2_util:uuid(),
    {ok, Resp} = fabric2_db:get_missing_revs(Db, [{DocId, Revs}]),
    ?assertMatch([{DocId, [_ | _], []}], Resp),
    [{DocId, Missing, _}] = Resp,
    MissingStrs = [couch_doc:rev_to_str(Rev) || Rev <- Missing],
    ?assertEqual(Revs, lists:sort(MissingStrs)).


open_missing_local_doc({Db, _}) ->
    ?assertEqual(
            {not_found, missing},
            fabric2_db:open_doc(Db, <<"_local/foo">>, [])
        ).


create_local_doc_basic({Db, _}) ->
    UUID = fabric2_util:uuid(),
    LDocId = <<?LOCAL_DOC_PREFIX, UUID/binary>>,
    Doc1 = #doc{
        id = LDocId,
        revs = {0, []},
        deleted = false,
        body = {[{<<"ohai">>, <<"there">>}]}
    },
    ?assertEqual({ok, {0, <<"1">>}}, fabric2_db:update_doc(Db, Doc1)),
    {ok, Doc2} = fabric2_db:open_doc(Db, Doc1#doc.id, []),
    ?assertEqual(Doc1#doc{revs = {0, [<<"1">>]}}, Doc2).


update_local_doc_basic({Db, _}) ->
    UUID = fabric2_util:uuid(),
    LDocId = <<?LOCAL_DOC_PREFIX, UUID/binary>>,
    Doc1 = #doc{
        id = LDocId,
        revs = {0, []},
        deleted = false,
        body = {[{<<"ohai">>, <<"there">>}]}
    },
    ?assertEqual({ok, {0, <<"1">>}}, fabric2_db:update_doc(Db, Doc1)),
    Doc2 = Doc1#doc{
        revs = {0, [<<"1">>]},
        body = {[{<<"whiz">>, <<"bang">>}]}
    },
    ?assertEqual({ok, {0, <<"2">>}}, fabric2_db:update_doc(Db, Doc2)),
    {ok, Doc3} = fabric2_db:open_doc(Db, Doc1#doc.id, []),
    ?assertEqual(Doc2#doc{revs = {0, [<<"2">>]}}, Doc3).


delete_local_doc_basic({Db, _}) ->
    UUID = fabric2_util:uuid(),
    LDocId = <<?LOCAL_DOC_PREFIX, UUID/binary>>,
    Doc1 = #doc{
        id = LDocId,
        revs = {0, []},
        deleted = false,
        body = {[{<<"ohai">>, <<"there">>}]}
    },
    ?assertEqual({ok, {0, <<"1">>}}, fabric2_db:update_doc(Db, Doc1)),
    Doc2 = Doc1#doc{
        revs = {0, [<<"1">>]},
        deleted = true,
        body = {[]}
    },
    ?assertEqual({ok, {0, <<"0">>}}, fabric2_db:update_doc(Db, Doc2)),
    ?assertEqual(
            {not_found, missing},
            fabric2_db:open_doc(Db, LDocId)
        ).


recreate_local_doc({Db, _}) ->
    UUID = fabric2_util:uuid(),
    LDocId = <<?LOCAL_DOC_PREFIX, UUID/binary>>,
    Doc1 = #doc{
        id = LDocId,
        revs = {0, []},
        deleted = false,
        body = {[{<<"ohai">>, <<"there">>}]}
    },
    ?assertEqual({ok, {0, <<"1">>}}, fabric2_db:update_doc(Db, Doc1)),
    Doc2 = Doc1#doc{
        revs = {0, [<<"1">>]},
        deleted = true,
        body = {[]}
    },
    ?assertEqual({ok, {0, <<"0">>}}, fabric2_db:update_doc(Db, Doc2)),
    ?assertEqual(
            {not_found, missing},
            fabric2_db:open_doc(Db, LDocId)
        ),

    ?assertEqual({ok, {0, <<"1">>}}, fabric2_db:update_doc(Db, Doc1)),
    {ok, Doc3} = fabric2_db:open_doc(Db, LDocId),
    ?assertEqual(Doc1#doc{revs = {0, [<<"1">>]}}, Doc3).


create_local_doc_bad_rev({Db, _}) ->
    UUID = fabric2_util:uuid(),
    LDocId = <<?LOCAL_DOC_PREFIX, UUID/binary>>,
    Doc1 = #doc{
        id = LDocId,
        revs = {0, [<<"not a number">>]}
    },
    ?assertThrow(<<"Invalid rev format">>, fabric2_db:update_doc(Db, Doc1)),

    Doc2 = Doc1#doc{
        revs = bad_bad_rev_roy_brown
    },
    ?assertThrow(<<"Invalid rev format">>, fabric2_db:update_doc(Db, Doc2)).


create_local_doc_random_rev({Db, _}) ->
    % Local docs don't care what rev is passed as long
    % as long as its a number.
    UUID = fabric2_util:uuid(),
    LDocId = <<?LOCAL_DOC_PREFIX, UUID/binary>>,
    Doc1 = #doc{
        id = LDocId,
        revs = {0, [<<"42">>]},
        body = {[{<<"state">>, 1}]}
    },
    ?assertEqual({ok, {0, <<"43">>}}, fabric2_db:update_doc(Db, Doc1)),
    {ok, Doc2} = fabric2_db:open_doc(Db, LDocId, []),
    ?assertEqual(Doc1#doc{revs = {0, [<<"43">>]}}, Doc2),

    Doc3 = Doc1#doc{
        revs = {0, [<<"1234567890">>]},
        body = {[{<<"state">>, 2}]}
    },
    ?assertEqual({ok, {0, <<"1234567891">>}}, fabric2_db:update_doc(Db, Doc3)),
    {ok, Doc4} = fabric2_db:open_doc(Db, LDocId, []),
    ?assertEqual(Doc3#doc{revs = {0, [<<"1234567891">>]}}, Doc4),

    Doc5 = Doc1#doc{
        revs = {0, [<<"1">>]},
        body = {[{<<"state">>, 3}]}
    },
    ?assertEqual({ok, {0, <<"2">>}}, fabric2_db:update_doc(Db, Doc5)),
    {ok, Doc6} = fabric2_db:open_doc(Db, LDocId, []),
    ?assertEqual(Doc5#doc{revs = {0, [<<"2">>]}}, Doc6).


create_a_large_local_doc({Db, _}) ->
    UUID = fabric2_util:uuid(),
    LDocId = <<?LOCAL_DOC_PREFIX, UUID/binary>>,
    Body = << <<"x">> || _ <- lists:seq(1, 300000) >>,
    Doc1 = #doc{
        id = LDocId,
        revs = {0, []},
        body = Body
    },
    ?assertEqual({ok, {0, <<"1">>}}, fabric2_db:update_doc(Db, Doc1)),
    {ok, Doc2} = fabric2_db:open_doc(Db, Doc1#doc.id, []),
    ?assertEqual(Doc1#doc{revs = {0, [<<"1">>]}}, Doc2),

    % Read via fold_local_docs
    {ok, Result} = fabric2_db:fold_local_docs(Db, fun(Data, Acc) ->
        case Data of
            {row, [{id, DocId} | _]} when LDocId =:= DocId ->
                {ok, [Data | Acc]};
            _ ->
                {ok, Acc}
        end
    end, [], []),
    ?assertEqual([{row, [
         {id, LDocId},
         {key, LDocId},
         {value, {[{rev, <<"0-1">>}]}}
    ]}], Result).


create_2_large_local_docs({Db, _}) ->
    % Create a large doc then overwrite with a smaller one. The reason is to
    % ensure the previous one correctly clears its range before writting the
    % new smaller one it its place.
    UUID = fabric2_util:uuid(),
    LDocId = <<?LOCAL_DOC_PREFIX, UUID/binary>>,
    Body1 = << <<"x">> || _ <- lists:seq(1, 400000) >>,
    Body2 = << <<"y">> || _ <- lists:seq(1, 150000) >>,

    Doc1 = #doc{
        id = LDocId,
        revs = {0, []},
        body = Body1
    },

    ?assertEqual({ok, {0, <<"1">>}}, fabric2_db:update_doc(Db, Doc1)),

    Doc2 = Doc1#doc{body = Body2},
    ?assertEqual({ok, {0, <<"1">>}}, fabric2_db:update_doc(Db, Doc2)),

    {ok, Doc3} = fabric2_db:open_doc(Db, LDocId, []),
    ?assertEqual(Doc2#doc{revs = {0, [<<"1">>]}}, Doc3).


local_doc_with_previous_encoding({Db, _}) ->
    #{db_prefix := DbPrefix} = Db,

    Id = <<"_local/old_doc">>,
    Body = {[{<<"x">>, 5}]},
    Rev = <<"1">>,
    Key = erlfdb_tuple:pack({?DB_LOCAL_DOCS, Id}, DbPrefix),

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        #{tx := Tx} = TxDb,
        Term = term_to_binary({Rev, Body}, [{minor_version, 1}]),
        ok = erlfdb:set(Tx, Key, Term)
    end),

    % Read old doc
    {ok, Doc1} = fabric2_db:open_doc(Db, Id, []),
    ?assertEqual({0, [<<"1">>]}, Doc1#doc.revs),
    ?assertEqual({[{<<"x">>, 5}]}, Doc1#doc.body),

    % Read via fold_local_docs.
    {ok, Result} = fabric2_db:fold_local_docs(Db, fun(Data, Acc) ->
        case Data of
            {row, [{id, DocId} | _]} when Id =:= DocId ->
                {ok, [Data | Acc]};
            _ ->
                {ok, Acc}
        end
    end, [], []),
    ?assertEqual([{row, [
         {id, Id},
         {key, Id},
         {value, {[{rev, <<"0-1">>}]}}
    ]}], Result),

    % Update doc
    NewBody = {[{<<"y">>, 6}]},
    Doc2 = Doc1#doc{body = NewBody},
    ?assertEqual({ok, {0, <<"2">>}}, fabric2_db:update_doc(Db, Doc2)),
    {ok, Doc3} = fabric2_db:open_doc(Db, Doc2#doc.id, []),
    ?assertEqual({0, [<<"2">>]}, Doc3#doc.revs),
    ?assertEqual(NewBody, Doc3#doc.body),

    % Old doc now has only the rev number in it
    <<255, OldDocBin/binary>> = fabric2_fdb:transactional(Db, fun(TxDb) ->
        #{tx := Tx} = TxDb,
        erlfdb:wait(erlfdb:get(Tx, Key))
    end),
    Unpacked = erlfdb_tuple:unpack(OldDocBin),
    ?assertMatch({?CURR_LDOC_FORMAT, <<"2">>, _}, Unpacked).


before_doc_update_skips_local_docs({Db0, _}) ->

    BduFun = fun(Doc, _, _) ->
        Doc#doc{body = {[<<"bdu_was_here">>, true]}}
    end,

    Db = Db0#{before_doc_update := BduFun},

    LDoc1 = #doc{id = <<"_local/ldoc1">>},
    Doc1 = #doc{id = <<"doc1">>},

    ?assertMatch({ok, {_, _}}, fabric2_db:update_doc(Db, LDoc1)),
    ?assertMatch({ok, {_, _}}, fabric2_db:update_doc(Db, Doc1)),

    {ok, LDoc2} = fabric2_db:open_doc(Db, LDoc1#doc.id),
    {ok, Doc2} = fabric2_db:open_doc(Db, Doc1#doc.id),

    ?assertEqual({[]}, LDoc2#doc.body),
    ?assertEqual({[<<"bdu_was_here">>, true]}, Doc2#doc.body).


open_doc_opts({Db, _}) ->
    % Build out state so that we can exercise each doc
    % open option. This requires a live revision with
    % an attachment, a conflict, and a deleted conflict.
    DocId = couch_uuids:random(),
    Att1 = couch_att:new([
        {name, <<"foo.txt">>},
        {type, <<"application/octet-stream">>},
        {att_len, 6},
        {data, <<"foobar">>},
        {encoding, identity},
        {md5, <<>>}
    ]),
    Doc1A = #doc{
        id = DocId,
        atts = [Att1]
    },
    {ok, {Pos1, Rev1A}} = fabric2_db:update_doc(Db, Doc1A),
    Att2 = couch_att:store([
            {data, stub},
            {revpos, 1}
        ], Att1),
    Doc1B = Doc1A#doc{
        revs = {Pos1, [Rev1A]},
        atts = [Att2]
    },
    {ok, {Pos2, Rev1B}} = fabric2_db:update_doc(Db, Doc1B),

    Rev2 = crypto:strong_rand_bytes(16),
    Rev3 = crypto:strong_rand_bytes(16),
    Rev4 = crypto:strong_rand_bytes(16),

    % Create a live conflict
    Doc2 = #doc{
        id = DocId,
        revs = {1, [Rev2]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc2, [replicated_changes]),

    % Create a deleted conflict
    Doc3 = #doc{
        id = DocId,
        revs = {1, [Rev3]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc3, [replicated_changes]),
    Doc4 = #doc{
        id = DocId,
        revs = {2, [Rev4, Rev3]},
        deleted = true
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc4, [replicated_changes]),

    OpenOpts1 = [
        revs_info,
        conflicts,
        deleted_conflicts,
        local_seq,
        {atts_since, [{Pos1, Rev1A}]}
    ],
    {ok, OpenedDoc1} = fabric2_db:open_doc(Db, DocId, OpenOpts1),

    #doc{
        id = DocId,
        revs = {2, [Rev1B, Rev1A]},
        atts = [Att3],
        meta = Meta
    } = OpenedDoc1,
    ?assertEqual(stub, couch_att:fetch(data, Att3)),
    ?assertEqual(
            {revs_info, Pos2, [{Rev1B, available}, {Rev1A, missing}]},
            lists:keyfind(revs_info, 1, Meta)
        ),
    ?assertEqual(
            {conflicts, [{1, Rev2}]},
            lists:keyfind(conflicts, 1, Meta)
        ),
    ?assertEqual(
            {deleted_conflicts, [{2, Rev4}]},
            lists:keyfind(deleted_conflicts, 1, Meta)
        ),
    ?assertMatch({_, <<_/binary>>}, lists:keyfind(local_seq, 1, Meta)),

    % Empty atts_since list
    {ok, OpenedDoc2} = fabric2_db:open_doc(Db, DocId, [{atts_since, []}]),
    #doc{atts = [Att4]} = OpenedDoc2,
    ?assertNotEqual(stub, couch_att:fetch(data, Att4)),

    % Missing ancestor
    Rev5 = crypto:strong_rand_bytes(16),
    OpenOpts2 = [{atts_since, [{5, Rev5}]}],
    {ok, OpenedDoc3} = fabric2_db:open_doc(Db, DocId, OpenOpts2),
    #doc{atts = [Att5]} = OpenedDoc3,
    ?assertNotEqual(stub, couch_att:fetch(data, Att5)).

