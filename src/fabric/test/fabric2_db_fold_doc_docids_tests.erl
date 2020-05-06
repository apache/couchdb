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

-module(fabric2_db_fold_doc_docids_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fabric2_test.hrl").

doc_fold_test_() ->
    {
        "Test document fold operations",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            {
                foreach,
                fun setup/0,
                fun cleanup/1,
                [
                    ?TDEF_FE(fold_docs_simple),
                    ?TDEF_FE(fold_docs_lots),
                    ?TDEF_FE(fold_docs_local),
                    ?TDEF_FE(fold_docs_mixed)
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
    Db.


cleanup(Db) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []).


fold_docs_simple(Db) ->
    Docs = create_docs(Db, 10),
    run_fold(Db, Docs).


fold_docs_lots(Db) ->
    Docs = create_docs(Db, 110),
    run_fold(Db, Docs).


fold_docs_local(Db) ->
    Docs = create_local_docs(Db, 10),
    run_fold(Db, Docs).


fold_docs_mixed(Db) ->
    Docs = create_mixed_docs(Db, 200),
    run_fold(Db, Docs).


run_fold(Db, Docs) ->
    SortedIds = get_ids(Docs),
    Ids = shuffle(SortedIds),
    Returned = fabric2_fdb:transactional(Db, fun (TxDb) ->
        fold_docs_return_ids(TxDb, Ids)
    end),
    ?assertEqual(Returned, Ids).


fold_docs_return_ids(TxDb, Ids) ->
    CB = fun(DocId, _Doc, Acc) ->
        {ok, Acc ++ [DocId]}
    end,
    {ok, Acc} = fabric2_db:fold_docs(TxDb, Ids, CB, [], []),
    Acc.

get_ids(Docs) ->
    lists:map(fun (#doc{id = Id}) -> Id end, Docs).


create_mixed_docs(Db, Size) ->
    fabric2_fdb:transactional(Db, fun (TxDb) ->
        Docs = lists:map(fun (Id) ->
            case Id rem 3 == 0 of
                true -> create_local_doc(Id);
                false -> create_doc(Id)
            end
        end, lists:seq(0, Size)),
        {ok, _} = fabric2_db:update_docs(TxDb, Docs),
        Docs
    end).


create_local_docs(Db, Size) ->
    fabric2_fdb:transactional(Db, fun (TxDb) ->
        Docs = lists:map(fun (Id) ->
            create_local_doc(Id)
        end, lists:seq(0, Size)),
        {ok, _} = fabric2_db:update_docs(TxDb, Docs),
        Docs
    end).


create_docs(Db, Size) ->
    fabric2_fdb:transactional(Db, fun (TxDb) ->
        Docs = lists:map(fun (Id) ->
            create_doc(Id)
        end, lists:seq(0, Size)),
        {ok, _} = fabric2_db:update_docs(TxDb, Docs),
        Docs
    end).


create_doc(Id) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, list_to_binary([<<"doc-">>, integer_to_binary(Id)])},
        {<<"value">>, 1}
    ]}).


create_local_doc(Id) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, list_to_binary([<<"_local/doc-">>, integer_to_binary(Id)])},
        {<<"value">>, 1}
    ]}).


shuffle(List) when is_list(List) ->
    Tagged = [{rand:uniform(), Item} || Item <- List],
    {_, Randomized} = lists:unzip(lists:sort(Tagged)),
    Randomized.
