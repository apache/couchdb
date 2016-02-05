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

-module(test_engine_read_write_docs).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


cet_read_empty_docs() ->
    {ok, Engine, St} = test_engine_util:init_engine(),

    ?assertEqual([not_found], Engine:open_docs(St, [<<"foo">>])),
    ?assertEqual(
        [not_found, not_found],
        Engine:open_docs(St, [<<"a">>, <<"b">>])
    ).


cet_read_empty_local_docs() ->
    {ok, Engine, St} = test_engine_util:init_engine(),

    ?assertEqual([not_found], Engine:open_local_docs(St, [<<"_local/foo">>])),
    ?assertEqual(
        [not_found, not_found],
        Engine:open_local_docs(St, [<<"_local/a">>, <<"_local/b">>])
    ).


cet_write_one_doc() ->
    {ok, Engine, DbPath, St1} = test_engine_util:init_engine(dbpath),

    ?assertEqual(0, Engine:get_doc_count(St1)),
    ?assertEqual(0, Engine:get_del_doc_count(St1)),
    ?assertEqual(0, Engine:get_update_seq(St1)),

    Actions = [
        {create, {<<"foo">>, [{<<"vsn">>, 1}]}}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),
    {ok, St3} = Engine:commit_data(St2),
    Engine:terminate(normal, St3),
    {ok, St4} = Engine:init(DbPath, []),

    ?assertEqual(1, Engine:get_doc_count(St4)),
    ?assertEqual(0, Engine:get_del_doc_count(St4)),
    ?assertEqual(1, Engine:get_update_seq(St4)),

    [FDI] = Engine:open_docs(St4, [<<"foo">>]),
    #rev_info{
        rev = {RevPos, PrevRevId},
        deleted = Deleted,
        body_sp = DocPtr
    } = test_engine_util:prev_rev(FDI),

    Doc0 = #doc{
        id = <<"foo">>,
        revs = {RevPos, [PrevRevId]},
        deleted = Deleted,
        body = DocPtr
    },

    Doc1 = Engine:read_doc_body(St4, Doc0),
    Body1 = if not is_binary(Doc1#doc.body) -> Doc1#doc.body; true ->
        couch_compress:decompress(Doc1#doc.body)
    end,
    ?assertEqual([{<<"vsn">>, 1}], Body1).


cet_write_two_docs() ->
    {ok, Engine, DbPath, St1} = test_engine_util:init_engine(dbpath),

    ?assertEqual(0, Engine:get_doc_count(St1)),
    ?assertEqual(0, Engine:get_del_doc_count(St1)),
    ?assertEqual(0, Engine:get_update_seq(St1)),

    Actions = [
        {create, {<<"foo">>, [{<<"vsn">>, 1}]}},
        {create, {<<"bar">>, [{<<"stuff">>, true}]}}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),
    {ok, St3} = Engine:commit_data(St2),
    Engine:terminate(normal, St3),
    {ok, St4} = Engine:init(DbPath, []),

    ?assertEqual(2, Engine:get_doc_count(St4)),
    ?assertEqual(0, Engine:get_del_doc_count(St4)),
    ?assertEqual(2, Engine:get_update_seq(St4)),

    Resps = Engine:open_docs(St4, [<<"foo">>, <<"bar">>]),
    ?assertEqual(false, lists:member(not_found, Resps)).


cet_write_three_doc_batch() ->
    {ok, Engine, DbPath, St1} = test_engine_util:init_engine(dbpath),

    ?assertEqual(0, Engine:get_doc_count(St1)),
    ?assertEqual(0, Engine:get_del_doc_count(St1)),
    ?assertEqual(0, Engine:get_update_seq(St1)),

    Actions = [
        {batch, [
            {create, {<<"foo">>, [{<<"vsn">>, 1}]}},
            {create, {<<"bar">>, [{<<"stuff">>, true}]}},
            {create, {<<"baz">>, []}}
        ]}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),
    {ok, St3} = Engine:commit_data(St2),
    Engine:terminate(normal, St3),
    {ok, St4} = Engine:init(DbPath, []),

    ?assertEqual(3, Engine:get_doc_count(St4)),
    ?assertEqual(0, Engine:get_del_doc_count(St4)),
    ?assertEqual(3, Engine:get_update_seq(St4)),

    Resps = Engine:open_docs(St4, [<<"foo">>, <<"bar">>, <<"baz">>]),
    ?assertEqual(false, lists:member(not_found, Resps)).


cet_update_doc() ->
    {ok, Engine, DbPath, St1} = test_engine_util:init_engine(dbpath),

    ?assertEqual(0, Engine:get_doc_count(St1)),
    ?assertEqual(0, Engine:get_del_doc_count(St1)),
    ?assertEqual(0, Engine:get_update_seq(St1)),

    Actions = [
        {create, {<<"foo">>, [{<<"vsn">>, 1}]}},
        {update, {<<"foo">>, [{<<"vsn">>, 2}]}}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),
    {ok, St3} = Engine:commit_data(St2),
    Engine:terminate(normal, St3),
    {ok, St4} = Engine:init(DbPath, []),

    ?assertEqual(1, Engine:get_doc_count(St4)),
    ?assertEqual(0, Engine:get_del_doc_count(St4)),
    ?assertEqual(2, Engine:get_update_seq(St4)),

    [FDI] = Engine:open_docs(St4, [<<"foo">>]),

    #rev_info{
        rev = {RevPos, PrevRevId},
        deleted = Deleted,
        body_sp = DocPtr
    } = test_engine_util:prev_rev(FDI),

    Doc0 = #doc{
        id = <<"foo">>,
        revs = {RevPos, [PrevRevId]},
        deleted = Deleted,
        body = DocPtr
    },

    Doc1 = Engine:read_doc_body(St4, Doc0),
    Body1 = if not is_binary(Doc1#doc.body) -> Doc1#doc.body; true ->
        couch_compress:decompress(Doc1#doc.body)
    end,

    ?assertEqual([{<<"vsn">>, 2}], Body1).


cet_delete_doc() ->
    {ok, Engine, DbPath, St1} = test_engine_util:init_engine(dbpath),

    ?assertEqual(0, Engine:get_doc_count(St1)),
    ?assertEqual(0, Engine:get_del_doc_count(St1)),
    ?assertEqual(0, Engine:get_update_seq(St1)),

    Actions = [
        {create, {<<"foo">>, [{<<"vsn">>, 1}]}},
        {delete, {<<"foo">>, []}}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),
    {ok, St3} = Engine:commit_data(St2),
    Engine:terminate(normal, St3),
    {ok, St4} = Engine:init(DbPath, []),

    ?assertEqual(0, Engine:get_doc_count(St4)),
    ?assertEqual(1, Engine:get_del_doc_count(St4)),
    ?assertEqual(2, Engine:get_update_seq(St4)),

    [FDI] = Engine:open_docs(St4, [<<"foo">>]),

    #rev_info{
        rev = {RevPos, PrevRevId},
        deleted = Deleted,
        body_sp = DocPtr
    } = test_engine_util:prev_rev(FDI),

    Doc0 = #doc{
        id = <<"foo">>,
        revs = {RevPos, [PrevRevId]},
        deleted = Deleted,
        body = DocPtr
    },

    Doc1 = Engine:read_doc_body(St4, Doc0),
    Body1 = if not is_binary(Doc1#doc.body) -> Doc1#doc.body; true ->
        couch_compress:decompress(Doc1#doc.body)
    end,

    ?assertEqual([], Body1).


cet_write_local_doc() ->
    {ok, Engine, DbPath, St1} = test_engine_util:init_engine(dbpath),

    ?assertEqual(0, Engine:get_doc_count(St1)),
    ?assertEqual(0, Engine:get_del_doc_count(St1)),
    ?assertEqual(0, Engine:get_update_seq(St1)),

    Actions = [
        {create, {<<"_local/foo">>, [{<<"yay">>, false}]}}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),
    {ok, St3} = Engine:commit_data(St2),
    Engine:terminate(normal, St3),
    {ok, St4} = Engine:init(DbPath, []),

    ?assertEqual(0, Engine:get_doc_count(St4)),
    ?assertEqual(0, Engine:get_del_doc_count(St4)),
    ?assertEqual(0, Engine:get_update_seq(St4)),

    [not_found] = Engine:open_docs(St4, [<<"_local/foo">>]),
    [#doc{} = Doc] = Engine:open_local_docs(St4, [<<"_local/foo">>]),
    ?assertEqual([{<<"yay">>, false}], Doc#doc.body).


cet_write_mixed_batch() ->
    {ok, Engine, DbPath, St1} = test_engine_util:init_engine(dbpath),

    ?assertEqual(0, Engine:get_doc_count(St1)),
    ?assertEqual(0, Engine:get_del_doc_count(St1)),
    ?assertEqual(0, Engine:get_update_seq(St1)),

    Actions = [
        {batch, [
            {create, {<<"bar">>, []}},
            {create, {<<"_local/foo">>, [{<<"yay">>, false}]}}
        ]}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),
    {ok, St3} = Engine:commit_data(St2),
    Engine:terminate(normal, St3),
    {ok, St4} = Engine:init(DbPath, []),

    ?assertEqual(1, Engine:get_doc_count(St4)),
    ?assertEqual(0, Engine:get_del_doc_count(St4)),
    ?assertEqual(1, Engine:get_update_seq(St4)),

    [#full_doc_info{}] = Engine:open_docs(St4, [<<"bar">>]),
    [not_found] = Engine:open_docs(St4, [<<"_local/foo">>]),

    [not_found] = Engine:open_local_docs(St4, [<<"bar">>]),
    [#doc{}] = Engine:open_local_docs(St4, [<<"_local/foo">>]).


cet_update_local_doc() ->
    {ok, Engine, DbPath, St1} = test_engine_util:init_engine(dbpath),

    ?assertEqual(0, Engine:get_doc_count(St1)),
    ?assertEqual(0, Engine:get_del_doc_count(St1)),
    ?assertEqual(0, Engine:get_update_seq(St1)),

    Actions = [
        {create, {<<"_local/foo">>, []}},
        {update, {<<"_local/foo">>, [{<<"stuff">>, null}]}}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),
    {ok, St3} = Engine:commit_data(St2),
    Engine:terminate(normal, St3),
    {ok, St4} = Engine:init(DbPath, []),

    ?assertEqual(0, Engine:get_doc_count(St4)),
    ?assertEqual(0, Engine:get_del_doc_count(St4)),
    ?assertEqual(0, Engine:get_update_seq(St4)),

    [not_found] = Engine:open_docs(St4, [<<"_local/foo">>]),
    [#doc{} = Doc] = Engine:open_local_docs(St4, [<<"_local/foo">>]),
    ?assertEqual([{<<"stuff">>, null}], Doc#doc.body).


cet_delete_local_doc() ->
    {ok, Engine, DbPath, St1} = test_engine_util:init_engine(dbpath),

    ?assertEqual(0, Engine:get_doc_count(St1)),
    ?assertEqual(0, Engine:get_del_doc_count(St1)),
    ?assertEqual(0, Engine:get_update_seq(St1)),

    Actions = [
        {create, {<<"_local/foo">>, []}},
        {delete, {<<"_local/foo">>, []}}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),
    {ok, St3} = Engine:commit_data(St2),
    Engine:terminate(normal, St3),
    {ok, St4} = Engine:init(DbPath, []),

    ?assertEqual(0, Engine:get_doc_count(St4)),
    ?assertEqual(0, Engine:get_del_doc_count(St4)),
    ?assertEqual(0, Engine:get_update_seq(St4)),

    [not_found] = Engine:open_docs(St4, [<<"_local/foo">>]),
    ?assertEqual([not_found], Engine:open_local_docs(St4, [<<"_local/foo">>])).
