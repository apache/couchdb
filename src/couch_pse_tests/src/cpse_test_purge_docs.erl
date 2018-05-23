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

-module(cpse_test_purge_docs).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


cet_purge_simple() ->
    {ok, Db1} = cpse_util:create_db(),

    Actions1 = [
        {create, {<<"foo">>, {[{<<"vsn">>, 1}]}}}
    ],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions1),

    ?assertEqual(1, couch_db_engine:get_doc_count(Db2)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db2)),
    ?assertEqual(1, couch_db_engine:get_update_seq(Db2)),
    ?assertEqual(0, couch_db_engine:get_purge_seq(Db2)),
    ?assertEqual([], couch_db_engine:get_last_purged(Db2)),

    [FDI] = couch_db_engine:open_docs(Db2, [<<"foo">>]),
    PrevRev = cpse_util:prev_rev(FDI),
    Rev = PrevRev#rev_info.rev,

    Actions2 = [
        {purge, {<<"foo">>, Rev}}
    ],
    {ok, Db3} = cpse_util:apply_actions(Db2, Actions2),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
    ?assertEqual(2, couch_db_engine:get_update_seq(Db3)),
    ?assertEqual(1, couch_db_engine:get_purge_seq(Db3)),
    ?assertEqual([{<<"foo">>, [Rev]}], couch_db_engine:get_last_purged(Db3)).


cet_purge_conflicts() ->
    {ok, Db1} = cpse_util:create_db(),

    Actions1 = [
        {create, {<<"foo">>, {[{<<"vsn">>, 1}]}}},
        {conflict, {<<"foo">>, {[{<<"vsn">>, 2}]}}}
    ],
    {ok, Db2} = cpse_util:apply_actions(Db1, Actions1),

    ?assertEqual(1, couch_db_engine:get_doc_count(Db2)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db2)),
    ?assertEqual(2, couch_db_engine:get_update_seq(Db2)),
    ?assertEqual(0, couch_db_engine:get_purge_seq(Db2)),
    ?assertEqual([], couch_db_engine:get_last_purged(Db2)),

    [FDI1] = couch_db_engine:open_docs(Db2, [<<"foo">>]),
    PrevRev1 = cpse_util:prev_rev(FDI1),
    Rev1 = PrevRev1#rev_info.rev,

    Actions2 = [
        {purge, {<<"foo">>, Rev1}}
    ],
    {ok, Db3} = cpse_util:apply_actions(Db2, Actions2),

    ?assertEqual(1, couch_db_engine:get_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
    ?assertEqual(4, couch_db_engine:get_update_seq(Db3)),
    ?assertEqual(1, couch_db_engine:get_purge_seq(Db3)),
    ?assertEqual([{<<"foo">>, [Rev1]}], couch_db_engine:get_last_purged(Db3)),

    [FDI2] = couch_db_engine:open_docs(Db3, [<<"foo">>]),
    PrevRev2 = cpse_util:prev_rev(FDI2),
    Rev2 = PrevRev2#rev_info.rev,

    Actions3 = [
        {purge, {<<"foo">>, Rev2}}
    ],
    {ok, Db4} = cpse_util:apply_actions(Db3, Actions3),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db4)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db4)),
    ?assertEqual(5, couch_db_engine:get_update_seq(Db4)),
    ?assertEqual(2, couch_db_engine:get_purge_seq(Db4)),
    ?assertEqual([{<<"foo">>, [Rev2]}], couch_db_engine:get_last_purged(Db4)).


cet_add_delete_purge() ->
    {ok, Db1} = cpse_util:create_db(),

    Actions1 = [
        {create, {<<"foo">>, {[{<<"vsn">>, 1}]}}},
        {delete, {<<"foo">>, {[{<<"vsn">>, 2}]}}}
    ],

    {ok, Db2} = cpse_util:apply_actions(Db1, Actions1),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db2)),
    ?assertEqual(1, couch_db_engine:get_del_doc_count(Db2)),
    ?assertEqual(2, couch_db_engine:get_update_seq(Db2)),
    ?assertEqual(0, couch_db_engine:get_purge_seq(Db2)),
    ?assertEqual([], couch_db_engine:get_last_purged(Db2)),

    [FDI] = couch_db_engine:open_docs(Db2, [<<"foo">>]),
    PrevRev = cpse_util:prev_rev(FDI),
    Rev = PrevRev#rev_info.rev,

    Actions2 = [
        {purge, {<<"foo">>, Rev}}
    ],
    {ok, Db3} = cpse_util:apply_actions(Db2, Actions2),

    ?assertEqual(0, couch_db_engine:get_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
    ?assertEqual(3, couch_db_engine:get_update_seq(Db3)),
    ?assertEqual(1, couch_db_engine:get_purge_seq(Db3)),
    ?assertEqual([{<<"foo">>, [Rev]}], couch_db_engine:get_last_purged(Db3)).


cet_add_two_purge_one() ->
    {ok, Db1} = cpse_util:create_db(),

    Actions1 = [
        {create, {<<"foo">>, {[{<<"vsn">>, 1}]}}},
        {create, {<<"bar">>, {[]}}}
    ],

    {ok, Db2} = cpse_util:apply_actions(Db1, Actions1),

    ?assertEqual(2, couch_db_engine:get_doc_count(Db2)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db2)),
    ?assertEqual(2, couch_db_engine:get_update_seq(Db2)),
    ?assertEqual(0, couch_db_engine:get_purge_seq(Db2)),
    ?assertEqual([], couch_db_engine:get_last_purged(Db2)),

    [FDI] = couch_db_engine:open_docs(Db2, [<<"foo">>]),
    PrevRev = cpse_util:prev_rev(FDI),
    Rev = PrevRev#rev_info.rev,

    Actions2 = [
        {purge, {<<"foo">>, Rev}}
    ],
    {ok, Db3} = cpse_util:apply_actions(Db2, Actions2),

    ?assertEqual(1, couch_db_engine:get_doc_count(Db3)),
    ?assertEqual(0, couch_db_engine:get_del_doc_count(Db3)),
    ?assertEqual(3, couch_db_engine:get_update_seq(Db3)),
    ?assertEqual(1, couch_db_engine:get_purge_seq(Db3)),
    ?assertEqual([{<<"foo">>, [Rev]}], couch_db_engine:get_last_purged(Db3)).
