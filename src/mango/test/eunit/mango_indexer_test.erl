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

-module(mango_indexer_test).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_eunit.hrl").
-include_lib("mango/src/mango_cursor.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


indexer_test_() ->
    {
        "Test indexing",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            {
                foreach,
                fun foreach_setup/0,
                fun foreach_teardown/1,
                [with([
                    ?TDEF(index_docs),
                    ?TDEF(update_doc),
                    ?TDEF(delete_doc)
                ])]
            }
        }
    }.


setup() ->
    Ctx = test_util:start_couch([
        fabric
    ]),
    Ctx.


cleanup(Ctx) ->
    test_util:stop_couch(Ctx).


foreach_setup() ->
    {ok, Db} = fabric2_db:create(?tempdb(), [{user_ctx, ?ADMIN_USER}]),

    DDoc = create_idx_ddoc(Db),
    fabric2_db:update_docs(Db, [DDoc]),

    Docs = make_docs(3),
    fabric2_db:update_docs(Db, Docs),
    {Db, couch_doc:to_json_obj(DDoc, [])}.


foreach_teardown({Db, _}) ->
    ok = fabric2_db:delete(fabric2_db:name(Db), []).


index_docs({Db, DDoc}) ->
    Docs = run_query(Db, DDoc),
    ?assertEqual([
        [{id, <<"1">>}, {value, 1}],
        [{id, <<"2">>}, {value, 2}],
        [{id, <<"3">>}, {value, 3}]
    ], Docs).

update_doc({Db, DDoc}) ->
    {ok, Doc} = fabric2_db:open_doc(Db, <<"2">>),
    JsonDoc = couch_doc:to_json_obj(Doc, []),
    JsonDoc2 = couch_util:json_apply_field({<<"value">>, 4}, JsonDoc),
    Doc2 = couch_doc:from_json_obj(JsonDoc2),
    fabric2_db:update_doc(Db, Doc2),

    Docs = run_query(Db, DDoc),
    ?assertEqual([
        [{id, <<"1">>}, {value, 1}],
        [{id, <<"3">>}, {value, 3}],
        [{id, <<"2">>}, {value, 4}]
    ], Docs).


delete_doc({Db, DDoc}) ->
    {ok, Doc} = fabric2_db:open_doc(Db, <<"2">>),
    JsonDoc = couch_doc:to_json_obj(Doc, []),
    JsonDoc2 = couch_util:json_apply_field({<<"_deleted">>, true}, JsonDoc),
    Doc2 = couch_doc:from_json_obj(JsonDoc2),
    fabric2_db:update_doc(Db, Doc2),

    Docs = run_query(Db, DDoc),
    ?assertEqual([
        [{id, <<"1">>}, {value, 1}],
        [{id, <<"3">>}, {value, 3}]
    ], Docs).


run_query(Db, DDoc) ->
    Args = #{
        start_key => [],
        start_key_docid => <<>>,
        end_key => [],
        end_key_docid => <<255>>,
        dir => fwd,
        skip => 0
    },
    [Idx] = mango_idx:from_ddoc(Db, DDoc),
    Cursor = #cursor{
        db = Db,
        index = Idx,
        user_acc = []
    },
    {ok, Cursor1} = mango_fdb:query(Db, fun query_cb/2, Cursor, Args),
    Acc = Cursor1#cursor.user_acc,
    lists:map(fun ({Props}) ->
        [
            {id, couch_util:get_value(<<"_id">>, Props)},
            {value, couch_util:get_value(<<"value">>, Props)}
        ]

    end, Acc).


create_idx_ddoc(Db) ->
    Opts = [
        {def, {[{<<"fields">>,{[{<<"value">>,<<"asc">>}]}}]}},
        {type, <<"json">>},
        {name, <<"idx_01">>},
        {ddoc, auto_name},
        {w, 3},
        {partitioned, db_default}
    ],

    {ok, Idx} = mango_idx:new(Db, Opts),
    {ok, DDoc} = mango_util:load_ddoc(Db, mango_idx:ddoc(Idx), []),
    {ok, NewDDoc} = mango_idx:add(DDoc, Idx),
    NewDDoc.


make_docs(Count) ->
    [doc(I) || I <- lists:seq(1, Count)].


doc(Id) ->
    couch_doc:from_json_obj({[
        {<<"_id">>, list_to_binary(integer_to_list(Id))},
        {<<"value">>, Id}
    ]}).


query_cb({doc, _, Doc}, #cursor{user_acc = Acc} = Cursor) ->
    {ok, Cursor#cursor{
        user_acc =  Acc ++ [Doc]
    }}.

