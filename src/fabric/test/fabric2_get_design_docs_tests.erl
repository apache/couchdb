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

-module(fabric2_get_design_docs_tests).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("fabric2_test.hrl").


get_design_docs_test_() ->
    {
        "Test get_design_docs",
        {
            setup,
            fun setup_all/0,
            fun cleanup_all/1,
            {
                foreach,
                fun setup/0,
                fun cleanup/1,
                [
                    ?TDEF_FE(empty_db),
                    ?TDEF_FE(get_one),
                    ?TDEF_FE(get_two),
                    ?TDEF_FE(get_many),
                    ?TDEF_FE(get_many_with_regular_docs),
                    ?TDEF_FE(dont_return_deleted_ddocs)
                ]
            }
        }
    }.


setup_all() ->
    test_util:start_couch([fabric]).


cleanup_all(Ctx) ->
    test_util:stop_couch(Ctx).


setup() ->
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),
    Db.


cleanup(Db) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []).


empty_db(Db) ->
    DDocs = fabric2_db:get_design_docs(Db),
    ?assertEqual([], DDocs).


get_one(Db) ->
    DDoc = create_ddoc(Db, <<"foo">>),
    DDocs = fabric2_db:get_design_docs(Db),
    ?assertEqual([DDoc], DDocs).


get_two(Db) ->
    DDoc1 = create_ddoc(Db, <<"foo">>),
    DDoc2 = create_ddoc(Db, <<"bar">>),
    DDocs = fabric2_db:get_design_docs(Db),
    % DDocs come back sorted
    ?assertEqual([DDoc2, DDoc1], DDocs).


get_many(Db) ->
    DDocsIn = lists:map(fun(Seq) ->
        Id = io_lib:format("~2..0b", [Seq]),
        create_ddoc(Db, iolist_to_binary(Id))
    end, lists:seq(1, 10)),
    DDocsOut = fabric2_db:get_design_docs(Db),
    ?assertEqual(DDocsIn, DDocsOut).


get_many_with_regular_docs(Db) ->
    RegularIds = [
        <<"0">>,
        <<"012aCb">>,
        <<"Another_doc">>,
        <<"Znother_doc">>,
        <<"a_doc_as_well">>,
        <<"zebra_doc">>
    ],
    lists:foreach(fun(DocId) ->
        create_doc(Db, DocId)
    end, RegularIds),
    DDocsIn = lists:map(fun(Seq) ->
        Id = io_lib:format("~2..0b", [Seq]),
        create_ddoc(Db, iolist_to_binary(Id))
    end, lists:seq(1, 10)),
    DDocsOut = fabric2_db:get_design_docs(Db),
    ?assertEqual(DDocsIn, DDocsOut).


dont_return_deleted_ddocs(Db) ->
    DDocsIn = lists:flatmap(fun(Seq) ->
        Id = io_lib:format("~2..0b", [Seq]),
        DDoc = create_ddoc(Db, iolist_to_binary(Id)),
        case Seq rem 2 == 0 of
            true ->
                delete_ddoc(Db, DDoc),
                [];
            false ->
                [DDoc]
        end
    end, lists:seq(1, 10)),
    DDocsOut = fabric2_db:get_design_docs(Db),
    ?assertEqual(DDocsIn, DDocsOut).


create_ddoc(Db, Id) ->
    create_doc(Db, <<"_design/", Id/binary>>).


delete_ddoc(Db, DDoc) ->
    {ok, _} = fabric2_db:update_doc(Db, DDoc#doc{deleted = true}).


create_doc(Db, Id) ->
    Doc = #doc{id = Id},
    {ok, {Pos, Rev}} = fabric2_db:update_doc(Db, Doc),
    Doc#doc{revs = {Pos, [Rev]}}.
