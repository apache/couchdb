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
                fun update_doc_basic/1,
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
                fun conflict_on_extend_deleted/1
            ]}
        }
    }.


setup() ->
    Ctx = test_util:start_couch([fabric]),
    {ok, Db} = fabric2_db:create(?tempdb(), []),
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
    ?assertEqual({ok, Doc3}, fabric2_db:open_doc(Db, Doc2#doc.id)).


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