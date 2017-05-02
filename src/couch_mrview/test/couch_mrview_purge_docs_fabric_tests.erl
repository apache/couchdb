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

-module(couch_mrview_purge_docs_fabric_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-define(TIMEOUT, 1000).


setup() ->
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [?ADMIN_CTX]),
    DbName.


teardown(DbName) ->
    ok = fabric:delete_db(DbName, [?ADMIN_CTX]).


view_purge_fabric_test_() ->
    {
        "Map views",
        {
            setup,
            fun() -> test_util:start_couch([fabric, mem3]) end,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun test_purge_verify_index/1
                ]
            }
        }
    }.


test_purge_verify_index(DbName) ->
    ?_test(begin
        Docs1 = couch_mrview_test_util:make_docs(normal, 5),
        {ok, _} = fabric:update_docs(DbName, Docs1, [?ADMIN_CTX]),
        {ok, _} = fabric:update_doc(
            DbName,
            couch_mrview_test_util:ddoc(map),
            [?ADMIN_CTX]
        ),

        purge_docs(DbName, [<<"1">>]),

        Result2 = fabric:query_view(DbName, <<"bar">>, <<"baz">>, #mrargs{}),
        Expect2 = {ok, [
            {meta, [{total, 4}, {offset, 0}]},
            {row, [{id, <<"2">>}, {key, 2}, {value, 2}]},
            {row, [{id, <<"3">>}, {key, 3}, {value, 3}]},
            {row, [{id, <<"4">>}, {key, 4}, {value, 4}]},
            {row, [{id, <<"5">>}, {key, 5}, {value, 5}]}
        ]},
        ?assertEqual(Expect2, Result2),

        {ok, DDoc} = fabric:open_doc(DbName, <<"_design/bar">>, []),
        {ok, IdxState} = couch_mrview_util:ddoc_to_mrst(DbName, DDoc),
        Sig = IdxState#mrst.sig,
        HexSig = list_to_binary(couch_index_util:hexsig(Sig)),
        DocId = couch_mrview_util:get_local_purge_doc_id(HexSig),
        {ok, LocPurgeDoc} = fabric:open_doc(DbName, DocId, []),
        {Props} = couch_doc:to_json_obj(LocPurgeDoc,[]),
        {Options} = couch_util:get_value(<<"verify_options">>, Props),
        ?assertEqual(true, couch_mrview_index:verify_index_exists(Options)),

        ok
    end).

get_rev(#full_doc_info{} = FDI) ->
    #doc_info{
        revs = [#rev_info{} = PrevRev | _]
    } = couch_doc:to_doc_info(FDI),
    PrevRev#rev_info.rev.


purge_docs(DbName, DocIds) ->
    lists:foreach(fun(DocId) ->
        FDI = fabric:get_full_doc_info(DbName, DocId, []),
        Rev = get_rev(FDI),
        {ok, [{ok, _}]} = fabric:purge_docs(DbName, [{DocId, [Rev]}], [])
    end, DocIds).
