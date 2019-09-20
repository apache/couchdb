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

-module(fabric2_rev_stemming).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").


doc_crud_test_() ->
    {
        "Test document CRUD operations with stemming",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {with, [
                fun update_doc/1,
                fun update_doc_replicated_no_stemming/1,
                fun update_doc_replicated_with_stemming/1,
                fun update_doc_replicate_existing_rev/1,
                fun update_winning_conflict_branch/1,
                fun update_non_winning_conflict_branch/1,
                fun delete_doc_basic/1,
                fun recreate_doc_basic/1
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


update_doc({Db, _}) ->
    ok = fabric2_db:set_revs_limit(Db, 2),
    Doc1 = #doc{id = fabric2_util:uuid()},
    {ok, {Pos1, Rev1}} = fabric2_db:update_doc(Db, Doc1),
    Doc2 = Doc1#doc{revs = {Pos1, [Rev1]}},
    {ok, {Pos2, Rev2}} = fabric2_db:update_doc(Db, Doc2),
    Doc3 = Doc2#doc{revs = {Pos2, [Rev2, Rev1]}},
    ?assertEqual({ok, Doc3}, fabric2_db:open_doc(Db, Doc2#doc.id)),

    {ok, {_, Rev3}} = fabric2_db:update_doc(Db, Doc3),
    {ok, Doc4} = fabric2_db:open_doc(Db, Doc3#doc.id),
    ?assertEqual({3, [Rev3, Rev2]}, Doc4#doc.revs).


update_doc_replicated_no_stemming({Db, _}) ->
    ok = fabric2_db:set_revs_limit(Db, 2),
    Rev1 = fabric2_util:uuid(),
    Rev2 = fabric2_util:uuid(),
    Doc = #doc{
        id = fabric2_util:uuid(),
        revs = {2, [Rev2, Rev1]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc, [replicated_changes]),
    {ok, #doc{revs = Revs}} = fabric2_db:open_doc(Db, Doc#doc.id),
    ?assertEqual({2, [Rev2, Rev1]}, Revs).


update_doc_replicated_with_stemming({Db, _}) ->
    ok = fabric2_db:set_revs_limit(Db, 1),
    Rev1 = fabric2_util:uuid(),
    Rev2 = fabric2_util:uuid(),
    Doc = #doc{
        id = fabric2_util:uuid(),
        revs = {2, [Rev2, Rev1]}
    },
    {ok, _} = fabric2_db:update_doc(Db, Doc, [replicated_changes]),
    {ok, #doc{revs = Revs}} = fabric2_db:open_doc(Db, Doc#doc.id),
    ?assertEqual({2, [Rev2]}, Revs).


update_doc_replicate_existing_rev({Db, _}) ->
    ok = fabric2_db:set_revs_limit(Db, 1),
    Rev1 = fabric2_util:uuid(),
    Rev2 = fabric2_util:uuid(),
    Doc1 = #doc{
        id = fabric2_util:uuid(),
        revs = {2, [Rev2, Rev1]}
    },
    {ok, {2, _}} = fabric2_db:update_doc(Db, Doc1, [replicated_changes]),
    {ok, []} = fabric2_db:update_docs(Db, [Doc1], [replicated_changes]),
    {ok, Doc} =  fabric2_db:open_doc(Db, Doc1#doc.id),
    ?assertEqual({2, [Rev2]}, Doc#doc.revs).


update_winning_conflict_branch({Db, _}) ->
    ok = fabric2_db:set_revs_limit(Db, 2),
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
    ?assertEqual({3, [Rev4, Rev3]}, Doc4#doc.revs),
    ?assertEqual(Doc3#doc{revs = undefined}, Doc4#doc{revs = undefined}).


update_non_winning_conflict_branch({Db, _}) ->
    ok = fabric2_db:set_revs_limit(Db, 2),
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
    ?assertEqual({3, [Rev4, Rev2]}, Doc4#doc.revs).


delete_doc_basic({Db, _}) ->
    ok = fabric2_db:set_revs_limit(Db, 1),
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
    Doc3 = Doc2#doc{revs = {Pos2, [Rev2]}},
    ?assertEqual({ok, Doc3}, fabric2_db:open_doc(Db, Doc2#doc.id, [deleted])).


recreate_doc_basic({Db, _}) ->
    ok = fabric2_db:set_revs_limit(Db, 1),
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
    ?assertEqual({3, [Rev3]}, Doc4#doc.revs),
    ?assertEqual(Doc3#doc{revs = undefined}, Doc4#doc{revs = undefined}).
