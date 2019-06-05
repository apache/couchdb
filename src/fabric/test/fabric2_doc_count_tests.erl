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

-module(fabric2_doc_count_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(DOC_COUNT, 10).


doc_count_test_() ->
    {
        "Test document counting operations",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {with, [
                fun normal_docs/1,
                fun design_docs/1,
                fun local_docs/1
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


normal_docs({Db, _}) ->
    {DocCount, DelDocCount, DDocCount, LDocCount} = get_doc_counts(Db),

    Docs1 = lists:map(fun(Id) ->
        Doc = #doc{
            id = integer_to_binary(Id),
            body = {[{<<"value">>, Id}]}
        },
        {ok, {RevPos, Rev}} = fabric2_db:update_doc(Db, Doc, []),
        Doc#doc{revs = {RevPos, [Rev]}}
    end, lists:seq(1, ?DOC_COUNT)),

    check_doc_counts(
            Db,
            DocCount + ?DOC_COUNT,
            DelDocCount,
            DDocCount,
            LDocCount
        ),

    Docs2 = lists:map(fun(Doc) ->
        {[{<<"value">>, V}]} = Doc#doc.body,
        NewDoc = case V rem 2 of
            0 -> Doc#doc{deleted = true};
            1 -> Doc
        end,
        {ok, {RevPos, Rev}} = fabric2_db:update_doc(Db, NewDoc, []),
        NewDoc#doc{revs = {RevPos, [Rev]}}
    end, Docs1),

    check_doc_counts(
            Db,
            DocCount + ?DOC_COUNT div 2,
            DelDocCount + ?DOC_COUNT div 2,
            DDocCount,
            LDocCount
        ),

    lists:map(fun(Doc) ->
        case Doc#doc.deleted of
            true ->
                Undeleted = Doc#doc{
                    revs = {0, []},
                    deleted = false
                },
                {ok, {RevPos, Rev}} = fabric2_db:update_doc(Db, Undeleted, []),
                Undeleted#doc{revs = {RevPos, [Rev]}};
            false ->
                Doc
        end
    end, Docs2),

    check_doc_counts(
            Db,
            DocCount + ?DOC_COUNT,
            DelDocCount,
            DDocCount,
            LDocCount
        ).


design_docs({Db, _}) ->
    {DocCount, DelDocCount, DDocCount, LDocCount} = get_doc_counts(Db),

    Docs1 = lists:map(fun(Id) ->
        BinId = integer_to_binary(Id),
        DDocId = <<?DESIGN_DOC_PREFIX, BinId/binary>>,
        Doc = #doc{
            id = DDocId,
            body = {[{<<"value">>, Id}]}
        },
        {ok, {RevPos, Rev}} = fabric2_db:update_doc(Db, Doc, []),
        Doc#doc{revs = {RevPos, [Rev]}}
    end, lists:seq(1, ?DOC_COUNT)),

    check_doc_counts(
            Db,
            DocCount + ?DOC_COUNT,
            DelDocCount,
            DDocCount + ?DOC_COUNT,
            LDocCount
        ),

    Docs2 = lists:map(fun(Doc) ->
        {[{<<"value">>, V}]} = Doc#doc.body,
        NewDoc = case V rem 2 of
            0 -> Doc#doc{deleted = true};
            1 -> Doc
        end,
        {ok, {RevPos, Rev}} = fabric2_db:update_doc(Db, NewDoc, []),
        NewDoc#doc{revs = {RevPos, [Rev]}}
    end, Docs1),

    check_doc_counts(
            Db,
            DocCount + ?DOC_COUNT div 2,
            DelDocCount + ?DOC_COUNT div 2,
            DDocCount + ?DOC_COUNT div 2,
            LDocCount
        ),

    lists:map(fun(Doc) ->
        case Doc#doc.deleted of
            true ->
                Undeleted = Doc#doc{
                    revs = {0, []},
                    deleted = false
                },
                {ok, {RevPos, Rev}} = fabric2_db:update_doc(Db, Undeleted, []),
                Undeleted#doc{revs = {RevPos, [Rev]}};
            false ->
                Doc
        end
    end, Docs2),

    check_doc_counts(
            Db,
            DocCount + ?DOC_COUNT,
            DelDocCount,
            DDocCount + ?DOC_COUNT,
            LDocCount
        ).


local_docs({Db, _}) ->
    {DocCount, DelDocCount, DDocCount, LDocCount} = get_doc_counts(Db),

    Docs1 = lists:map(fun(Id) ->
        BinId = integer_to_binary(Id),
        LDocId = <<?LOCAL_DOC_PREFIX, BinId/binary>>,
        Doc = #doc{
            id = LDocId,
            body = {[{<<"value">>, Id}]}
        },
        {ok, {RevPos, Rev}} = fabric2_db:update_doc(Db, Doc, []),
        Doc#doc{revs = {RevPos, [Rev]}}
    end, lists:seq(1, ?DOC_COUNT)),

    check_doc_counts(
            Db,
            DocCount,
            DelDocCount,
            DDocCount,
            LDocCount + ?DOC_COUNT
        ),

    Docs2 = lists:map(fun(Doc) ->
        {[{<<"value">>, V}]} = Doc#doc.body,
        NewDoc = case V rem 2 of
            0 -> Doc#doc{deleted = true};
            1 -> Doc
        end,
        {ok, {RevPos, Rev}} = fabric2_db:update_doc(Db, NewDoc, []),
        NewDoc#doc{revs = {RevPos, [Rev]}}
    end, Docs1),

    check_doc_counts(
            Db,
            DocCount,
            DelDocCount,
            DDocCount,
            LDocCount + ?DOC_COUNT div 2
        ),

    lists:map(fun(Doc) ->
        case Doc#doc.deleted of
            true ->
                Undeleted = Doc#doc{
                    revs = {0, []},
                    deleted = false
                },
                {ok, {RevPos, Rev}} = fabric2_db:update_doc(Db, Undeleted, []),
                Undeleted#doc{revs = {RevPos, [Rev]}};
            false ->
                Doc
        end
    end, Docs2),

    check_doc_counts(
            Db,
            DocCount,
            DelDocCount,
            DDocCount,
            LDocCount + ?DOC_COUNT
        ).


get_doc_counts(Db) ->
    DocCount = fabric2_db:get_doc_count(Db),
    DelDocCount = fabric2_db:get_del_doc_count(Db),
    DDocCount = fabric2_db:get_doc_count(Db, <<"_design">>),
    LDocCount = fabric2_db:get_doc_count(Db, <<"_local">>),
    {DocCount, DelDocCount, DDocCount, LDocCount}.


check_doc_counts(Db, DocCount, DelDocCount, DDocCount, LDocCount) ->
    ?assertEqual(DocCount, fabric2_db:get_doc_count(Db)),
    ?assertEqual(DelDocCount, fabric2_db:get_del_doc_count(Db)),
    ?assertEqual(DocCount, fabric2_db:get_doc_count(Db, <<"_all_docs">>)),
    ?assertEqual(DDocCount, fabric2_db:get_doc_count(Db, <<"_design">>)),
    ?assertEqual(LDocCount, fabric2_db:get_doc_count(Db, <<"_local">>)).
