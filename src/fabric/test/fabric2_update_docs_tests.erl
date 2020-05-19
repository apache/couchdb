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

-module(fabric2_update_docs_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fabric2_test.hrl").


update_docs_test_() ->
    {
        "Test update_docs",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            {
                foreach,
                fun setup/0,
                fun cleanup/1,
                [
                    ?TDEF_FE(update_docs),
                    ?TDEF_FE(update_docs_replicated),
                    ?TDEF_FE(update_docs_batches),
                    ?TDEF_FE(update_docs_replicated_batches),
                    ?TDEF_FE(update_docs_duplicate_ids_conflict),
                    ?TDEF_FE(update_docs_duplicate_ids_with_batches),
                    ?TDEF_FE(update_docs_replicate_batches_duplicate_id)
                ]
            }
        }
    }.


setup_all() ->
    test_util:start_couch([fabric]).


teardown_all(Ctx) ->
    test_util:stop_couch(Ctx).


setup() ->
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    meck:new(erlfdb, [passthrough]),
    Db.


cleanup(#{} = Db) ->
    meck:unload(),
    ok = fabric2_db:delete(fabric2_db:name(Db), []).


update_docs(Db) ->
    ?assertEqual({ok, []}, fabric2_db:update_docs(Db, [])),

    Doc1 = doc(),
    Res1 = fabric2_db:update_docs(Db, [Doc1]),
    ?assertMatch({ok, [_]}, Res1),
    {ok, [Doc1Res]} = Res1,
    ?assertMatch({ok, {1, <<_/binary>>}}, Doc1Res),
    {ok, {1, Rev1}} = Doc1Res,
    {ok, Doc1Open} = fabric2_db:open_doc(Db, Doc1#doc.id),
    ?assertEqual(Doc1#doc{revs = {1, [Rev1]}}, Doc1Open),

    Doc2 = doc(),
    Doc3 = doc(),
    Res2 = fabric2_db:update_docs(Db, [Doc2, Doc3]),
    ?assertMatch({ok, [_, _]}, Res2),
    {ok, [Doc2Res, Doc3Res]} = Res2,
    ?assertMatch({ok, {1, <<_/binary>>}}, Doc2Res),
    ?assertMatch({ok, {1, <<_/binary>>}}, Doc3Res).


update_docs_replicated(Db) ->
    Opts = [replicated_changes],

    ?assertEqual({ok, []}, fabric2_db:update_docs(Db, [], Opts)),

    Doc1 = doc(10, {1, [rev()]}),
    ?assertMatch({ok, []}, fabric2_db:update_docs(Db, [Doc1], Opts)),
    {ok, Doc1Open} = fabric2_db:open_doc(Db, Doc1#doc.id),
    ?assertEqual(Doc1, Doc1Open),

    Doc2 = doc(10, {1, [rev()]}),
    Doc3 = doc(10, {1, [rev()]}),
    ?assertMatch({ok, []}, fabric2_db:update_docs(Db, [Doc2, Doc3], Opts)),
    {ok, Doc2Open} = fabric2_db:open_doc(Db, Doc2#doc.id),
    ?assertEqual(Doc2, Doc2Open),
    {ok, Doc3Open} = fabric2_db:open_doc(Db, Doc3#doc.id),
    ?assertEqual(Doc3, Doc3Open).


update_docs_batches(Db) ->
    Opts = [{batch_size, 5000}],

    Docs1 = [doc(9000), doc(9000)],

    meck:reset(erlfdb),
    ?assertMatch({ok, [_ | _]}, fabric2_db:update_docs(Db, Docs1, Opts)),
    ?assertEqual(2, meck:num_calls(erlfdb, transactional, 2)),

    lists:foreach(fun(#doc{} = Doc) ->
        ?assertMatch({ok, #doc{}}, fabric2_db:open_doc(Db, Doc#doc.id))
    end, Docs1),

    Docs2 = [doc(10), doc(10), doc(9000), doc(10)],

    meck:reset(erlfdb),
    ?assertMatch({ok, [_ | _]}, fabric2_db:update_docs(Db, Docs2, Opts)),
    ?assertEqual(2, meck:num_calls(erlfdb, transactional, 2)),

    lists:foreach(fun(#doc{} = Doc) ->
        ?assertMatch({ok, #doc{}}, fabric2_db:open_doc(Db, Doc#doc.id))
    end, Docs2).


update_docs_replicated_batches(Db) ->
    Opts = [{batch_size, 5000}, replicated_changes],

    Docs1 = [doc(Size, {1, [rev()]}) || Size <- [9000, 9000]],

    meck:reset(erlfdb),
    ?assertMatch({ok, []}, fabric2_db:update_docs(Db, Docs1, Opts)),
    ?assertEqual(2, meck:num_calls(erlfdb, transactional, 2)),

    lists:foreach(fun(#doc{} = Doc) ->
        ?assertEqual({ok, Doc}, fabric2_db:open_doc(Db, Doc#doc.id))
    end, Docs1),

    Docs2 = [doc(Size, {1, [rev()]}) || Size <- [10, 10, 9000, 10]],

    meck:reset(erlfdb),
    ?assertMatch({ok, []}, fabric2_db:update_docs(Db, Docs2, Opts)),
    ?assertEqual(2, meck:num_calls(erlfdb, transactional, 2)),

    lists:foreach(fun(#doc{} = Doc) ->
        ?assertEqual({ok, Doc}, fabric2_db:open_doc(Db, Doc#doc.id))
    end, Docs2).


update_docs_duplicate_ids_conflict(Db) ->
    Doc = doc(),

    Res = fabric2_db:update_docs(Db, [Doc, doc(), Doc]),
    ?assertMatch({ok, [_, _, _]}, Res),

    {ok, [Doc1Res, Doc2Res, Doc3Res]} = Res,
    ?assertMatch({ok, {1, <<_/binary>>}}, Doc1Res),
    ?assertMatch({ok, {1, <<_/binary>>}}, Doc2Res),
    ?assertMatch(conflict, Doc3Res).


update_docs_duplicate_ids_with_batches(Db) ->
    Opts = [{batch_size, 5000}],

    Doc = doc(9000),

    meck:reset(erlfdb),
    Res = fabric2_db:update_docs(Db, [Doc, doc(9000), Doc], Opts),
    ?assertMatch({ok, [_, _, _]}, Res),
    ?assertEqual(3, meck:num_calls(erlfdb, transactional, 2)),

    {ok, [Doc1Res, Doc2Res, Doc3Res]} = Res,
    ?assertMatch({ok, {1, <<_/binary>>}}, Doc1Res),
    ?assertMatch({ok, {1, <<_/binary>>}}, Doc2Res),
    ?assertMatch(conflict, Doc3Res).


update_docs_replicate_batches_duplicate_id(Db) ->
    Opts = [replicated_changes],

    Doc = doc(10, {1, [rev()]}),
    Docs = [Doc, Doc],

    meck:reset(erlfdb),
    ?assertMatch({ok, []}, fabric2_db:update_docs(Db, Docs, Opts)),
    ?assertEqual(2, meck:num_calls(erlfdb, transactional, 2)),

    ?assertEqual({ok, Doc}, fabric2_db:open_doc(Db, Doc#doc.id)).


% Utility functions

doc() ->
    doc(2).


doc(Size) ->
    doc(Size, undefined).


doc(Size, Revs) ->
    Doc = #doc{
        id = fabric2_util:uuid(),
        body = doc_body(Size)
    },
    case Revs of
        undefined -> Doc;
        _ -> Doc#doc{revs = Revs}
    end.


rev() ->
    fabric2_util:to_hex(crypto:strong_rand_bytes(16)).


doc_body(Size) when is_integer(Size), Size >= 2 ->
    Val = fabric2_util:to_hex(crypto:strong_rand_bytes(Size div 2)),
    {[{<<"x">>, Val}]}.
