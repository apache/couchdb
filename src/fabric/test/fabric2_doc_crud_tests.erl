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


doc_crud_test_() ->
    {
        "Test document CRUD operations",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {with, [
                fun open_missing_doc/1,
                fun create_new_doc/1,
                fun create_ddoc_basic/1,
                fun create_ddoc_requires_admin/1,
                fun create_ddoc_requires_validation/1,
                fun create_ddoc_requires_compilation/1,
                fun update_doc_basic/1,
                fun update_ddoc_basic/1,
                fun update_doc_replicated/1,
                fun update_doc_replicated_add_conflict/1,
                fun update_doc_replicated_changes_winner/1,
                fun update_doc_replicated_extension/1,
                fun update_doc_replicate_existing_rev/1,
                fun update_winning_conflict_branch/1,
                fun update_non_winning_conflict_branch/1,
                fun delete_doc_basic/1,
                fun delete_changes_winner/1,
                fun recreate_doc_basic/1,
                fun conflict_on_create_new_with_rev/1,
                fun conflict_on_update_with_no_rev/1,
                fun conflict_on_create_as_deleted/1,
                fun conflict_on_recreate_as_deleted/1,
                fun conflict_on_extend_deleted/1,
                fun open_doc_revs_basic/1,
                fun open_doc_revs_all/1,
                fun open_doc_revs_latest/1,
                fun get_missing_revs_basic/1,
                fun get_missing_revs_on_missing_doc/1,
                fun open_missing_local_doc/1,
                fun create_local_doc_basic/1,
                fun update_local_doc_basic/1,
                fun delete_local_doc_basic/1,
                fun recreate_local_doc/1,
                fun create_local_doc_bad_rev/1,
                fun create_local_doc_random_rev/1
            ]}
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
    ?assertThrow({error, conflict}, fabric2_db:update_doc(Db, Doc)).


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
    ?assertThrow({error, conflict}, fabric2_db:update_doc(Db, Doc2)).


conflict_on_create_as_deleted({Db, _}) ->
    Doc = #doc{
        id = fabric2_util:uuid(),
        deleted = true,
        body = {[{<<"foo">>, <<"bar">>}]}
    },
    ?assertThrow({error, conflict}, fabric2_db:update_doc(Db, Doc)).


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
    ?assertThrow({error, conflict}, fabric2_db:update_doc(Db, Doc3)).


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
    ?assertThrow({error, conflict}, fabric2_db:update_doc(Db, Doc3)).


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
    ?assertThrow(
            {error, <<"Invalid rev format">>},
            fabric2_db:update_doc(Db, Doc1)
        ),

    Doc2 = Doc1#doc{
        revs = bad_bad_rev_roy_brown
    },
    ?assertThrow(
            {error, <<"Invalid rev format">>},
            fabric2_db:update_doc(Db, Doc2)
        ).


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
