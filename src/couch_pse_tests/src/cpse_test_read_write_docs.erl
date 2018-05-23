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

-module(cpse_test_read_write_docs).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


cet_read_empty_docs() ->
    {ok, Db} = cpse_util:create_db(),

    ?assertEqual([not_found], couch_db_engine:open_docs(Db, [<<"foo">>])),
    ?assertEqual(
        [not_found, not_found],
        couch_db_engine:open_docs(Db, [<<"a">>, <<"b">>])
    ).


cet_read_empty_local_docs() ->
    {ok, Db} = cpse_util:create_db(),

    {LocalA, LocalB} = {<<"_local/a">>, <<"_local/b">>},
    ?assertEqual([not_found], couch_db_engine:open_local_docs(Db, [LocalA])),
    ?assertEqual(
        [not_found, not_found],
        couch_db_engine:open_local_docs(Db, [LocalA, LocalB])
    ).


cet_write_one_doc() ->
    {ok, Db1} = cpse_util:create_db(),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_update_seq(Db1)),

    Actions = [
        {create, {<<"foo">>, {[{<<"vsn">>, 1}]}}}
    ],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions),
    ?assertEqual(1, couch_db_engine:get_doc_count(Db2)),

    {ok, _} = couch_db:ensure_full_commit(Db2),
    cpse_util:shutdown_db(Db2),

    {ok, Db3} = couch_db:reopen(Db2),

    ?assertEqual(1, couch_db_engine:get_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
    ?assertEqual(1, couch_db_engine:get_update_seq(Db3)),

    [FDI] = couch_db_engine:open_docs(Db3, [<<"foo">>]),
    #rev_info{
        rev = {RevPos, PrevRevId},
        deleted = Deleted,
        body_sp = DocPtr
    } = cpse_util:prev_rev(FDI),

    Doc0 = #doc{
        id = <<"foo">>,
        revs = {RevPos, [PrevRevId]},
        deleted = Deleted,
        body = DocPtr
    },

    Doc1 = couch_db_engine:read_doc_body(Db3, Doc0),
    Body1 = if not is_binary(Doc1#doc.body) -> Doc1#doc.body; true ->
        couch_compress:decompress(Doc1#doc.body)
    end,
    ?assertEqual({[{<<"vsn">>, 1}]}, Body1).


cet_write_two_docs() ->
    {ok, Db1} = cpse_util:create_db(),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_update_seq(Db1)),

    Actions = [
        {create, {<<"foo">>, {[{<<"vsn">>, 1}]}}},
        {create, {<<"bar">>, {[{<<"stuff">>, true}]}}}
    ],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions),
    {ok, _} = couch_db:ensure_full_commit(Db2),
    cpse_util:shutdown_db(Db2),

    {ok, Db3} = couch_db:reopen(Db2),

    ?assertEqual(2, couch_db_engine:get_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
    ?assertEqual(2, couch_db_engine:get_update_seq(Db3)),

    Resps = couch_db_engine:open_docs(Db3, [<<"foo">>, <<"bar">>]),
    ?assertEqual(false, lists:member(not_found, Resps)).


cet_write_three_doc_batch() ->
    {ok, Db1} = cpse_util:create_db(),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_update_seq(Db1)),

    Actions = [
        {batch, [
            {create, {<<"foo">>, {[{<<"vsn">>, 1}]}}},
            {create, {<<"bar">>, {[{<<"stuff">>, true}]}}},
            {create, {<<"baz">>, {[]}}}
        ]}
    ],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions),
    {ok, _} = couch_db:ensure_full_commit(Db2),
    cpse_util:shutdown_db(Db2),

    {ok, Db3} = couch_db:reopen(Db2),

    ?assertEqual(3, couch_db_engine:get_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
    ?assertEqual(3, couch_db_engine:get_update_seq(Db3)),

    Resps = couch_db_engine:open_docs(Db3, [<<"foo">>, <<"bar">>, <<"baz">>]),
    ?assertEqual(false, lists:member(not_found, Resps)).


cet_update_doc() ->
    {ok, Db1} = cpse_util:create_db(),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_update_seq(Db1)),

    Actions = [
        {create, {<<"foo">>, {[{<<"vsn">>, 1}]}}},
        {update, {<<"foo">>, {[{<<"vsn">>, 2}]}}}
    ],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions),
    {ok, _} = couch_db:ensure_full_commit(Db2),
    cpse_util:shutdown_db(Db2),

    {ok, Db3} = couch_db:reopen(Db2),

    ?assertEqual(1, couch_db_engine:get_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
    ?assertEqual(2, couch_db_engine:get_update_seq(Db3)),

    [FDI] = couch_db_engine:open_docs(Db3, [<<"foo">>]),

    #rev_info{
        rev = {RevPos, PrevRevId},
        deleted = Deleted,
        body_sp = DocPtr
    } = cpse_util:prev_rev(FDI),

    Doc0 = #doc{
        id = <<"foo">>,
        revs = {RevPos, [PrevRevId]},
        deleted = Deleted,
        body = DocPtr
    },

    Doc1 = couch_db_engine:read_doc_body(Db3, Doc0),
    Body1 = if not is_binary(Doc1#doc.body) -> Doc1#doc.body; true ->
        couch_compress:decompress(Doc1#doc.body)
    end,

    ?assertEqual({[{<<"vsn">>, 2}]}, Body1).


cet_delete_doc() ->
    {ok, Db1} = cpse_util:create_db(),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_update_seq(Db1)),

    Actions = [
        {create, {<<"foo">>, {[{<<"vsn">>, 1}]}}},
        {delete, {<<"foo">>, {[]}}}
    ],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions),
    {ok, _} = couch_db:ensure_full_commit(Db2),
    cpse_util:shutdown_db(Db2),

    {ok, Db3} = couch_db:reopen(Db2),
    ?assertEqual(0, couch_db_engine:get_doc_count(Db3)),
    ?assertEqual(1, couch_db_engine:get_del_doc_count(Db3)),
    ?assertEqual(2, couch_db_engine:get_update_seq(Db3)),

    [FDI] = couch_db_engine:open_docs(Db3, [<<"foo">>]),

    #rev_info{
        rev = {RevPos, PrevRevId},
        deleted = Deleted,
        body_sp = DocPtr
    } = cpse_util:prev_rev(FDI),

    Doc0 = #doc{
        id = <<"foo">>,
        revs = {RevPos, [PrevRevId]},
        deleted = Deleted,
        body = DocPtr
    },

    Doc1 = couch_db_engine:read_doc_body(Db3, Doc0),
    Body1 = if not is_binary(Doc1#doc.body) -> Doc1#doc.body; true ->
        couch_compress:decompress(Doc1#doc.body)
    end,

    ?assertEqual({[]}, Body1).


cet_write_local_doc() ->
    {ok, Db1} = cpse_util:create_db(),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_update_seq(Db1)),

    Actions = [
        {create, {<<"_local/foo">>, {[{<<"yay">>, false}]}}}
    ],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions),
    {ok, _} = couch_db:ensure_full_commit(Db2),
    cpse_util:shutdown_db(Db2),

    {ok, Db3} = couch_db:reopen(Db2),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_update_seq(Db3)),

    [not_found] = couch_db_engine:open_docs(Db3, [<<"_local/foo">>]),
    [#doc{} = Doc] = couch_db_engine:open_local_docs(Db3, [<<"_local/foo">>]),
    ?assertEqual({[{<<"yay">>, false}]}, Doc#doc.body).


cet_write_mixed_batch() ->
    {ok, Db1} = cpse_util:create_db(),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_update_seq(Db1)),

    Actions = [
        {batch, [
            {create, {<<"bar">>, {[]}}},
            {create, {<<"_local/foo">>, {[{<<"yay">>, false}]}}}
        ]}
    ],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions),
    {ok, _} = couch_db:ensure_full_commit(Db2),
    cpse_util:shutdown_db(Db2),

    {ok, Db3} = couch_db:reopen(Db2),

    ?assertEqual(1, couch_db_engine:get_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
    ?assertEqual(1, couch_db_engine:get_update_seq(Db3)),

    [#full_doc_info{}] = couch_db_engine:open_docs(Db3, [<<"bar">>]),
    [not_found] = couch_db_engine:open_docs(Db3, [<<"_local/foo">>]),

    [not_found] = couch_db_engine:open_local_docs(Db3, [<<"bar">>]),
    [#doc{}] = couch_db_engine:open_local_docs(Db3, [<<"_local/foo">>]).


cet_update_local_doc() ->
    {ok, Db1} = cpse_util:create_db(),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_update_seq(Db1)),

    Actions = [
        {create, {<<"_local/foo">>, {[]}}},
        {update, {<<"_local/foo">>, {[{<<"stuff">>, null}]}}}
    ],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions),
    {ok, _} = couch_db:ensure_full_commit(Db1),
    cpse_util:shutdown_db(Db2),

    {ok, Db3} = couch_db:reopen(Db2),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_update_seq(Db3)),

    [not_found] = couch_db_engine:open_docs(Db3, [<<"_local/foo">>]),
    [#doc{} = Doc] = couch_db_engine:open_local_docs(Db3, [<<"_local/foo">>]),
    ?assertEqual({[{<<"stuff">>, null}]}, Doc#doc.body).


cet_delete_local_doc() ->
    {ok, Db1} = cpse_util:create_db(),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db1)),
    ?assertEqual(0, couch_db_engine:get_update_seq(Db1)),

    Actions = [
        {create, {<<"_local/foo">>, []}},
        {delete, {<<"_local/foo">>, []}}
    ],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions),
    {ok, _} = couch_db:ensure_full_commit(Db2),
    cpse_util:shutdown_db(Db2),

    {ok, Db3} = couch_db:reopen(Db2),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_update_seq(Db3)),

    [not_found] = couch_db_engine:open_docs(Db3, [<<"_local/foo">>]),
    ?assertEqual(
            [not_found],
            couch_db_engine:open_local_docs(Db3, [<<"_local/foo">>])
        ).
