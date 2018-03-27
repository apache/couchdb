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

-module(test_engine_purge_docs).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


cet_purge_simple() ->
    {ok, Engine, St1} = test_engine_util:init_engine(),

    Actions1 = [
        {create, {<<"foo">>, [{<<"vsn">>, 1}]}}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions1),

    ?assertEqual(1, Engine:get_doc_count(St2)),
    ?assertEqual(0, Engine:get_del_doc_count(St2)),
    ?assertEqual(1, Engine:get_update_seq(St2)),
    ?assertEqual(0, Engine:get_purge_seq(St2)),
    ?assertEqual([], Engine:get_last_purged(St2)),

    [FDI] = Engine:open_docs(St2, [<<"foo">>]),
    PrevRev = test_engine_util:prev_rev(FDI),
    Rev = PrevRev#rev_info.rev,

    Actions2 = [
        {purge, {<<"foo">>, Rev}}
    ],
    {ok, St3} = test_engine_util:apply_actions(Engine, St2, Actions2),

    ?assertEqual(0, Engine:get_doc_count(St3)),
    ?assertEqual(0, Engine:get_del_doc_count(St3)),
    ?assertEqual(2, Engine:get_update_seq(St3)),
    ?assertEqual(1, Engine:get_purge_seq(St3)),
    ?assertEqual([{<<"foo">>, [Rev]}], Engine:get_last_purged(St3)).


cet_purge_conflicts() ->
    {ok, Engine, St1} = test_engine_util:init_engine(),

    Actions1 = [
        {create, {<<"foo">>, [{<<"vsn">>, 1}]}},
        {conflict, {<<"foo">>, [{<<"vsn">>, 2}]}}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions1),

    ?assertEqual(1, Engine:get_doc_count(St2)),
    ?assertEqual(0, Engine:get_del_doc_count(St2)),
    ?assertEqual(2, Engine:get_update_seq(St2)),
    ?assertEqual(0, Engine:get_purge_seq(St2)),
    ?assertEqual([], Engine:get_last_purged(St2)),

    [FDI1] = Engine:open_docs(St2, [<<"foo">>]),
    PrevRev1 = test_engine_util:prev_rev(FDI1),
    Rev1 = PrevRev1#rev_info.rev,

    Actions2 = [
        {purge, {<<"foo">>, Rev1}}
    ],
    {ok, St3} = test_engine_util:apply_actions(Engine, St2, Actions2),

    ?assertEqual(1, Engine:get_doc_count(St3)),
    ?assertEqual(0, Engine:get_del_doc_count(St3)),
    ?assertEqual(4, Engine:get_update_seq(St3)),
    ?assertEqual(1, Engine:get_purge_seq(St3)),
    ?assertEqual([{<<"foo">>, [Rev1]}], Engine:get_last_purged(St3)),

    [FDI2] = Engine:open_docs(St3, [<<"foo">>]),
    PrevRev2 = test_engine_util:prev_rev(FDI2),
    Rev2 = PrevRev2#rev_info.rev,

    Actions3 = [
        {purge, {<<"foo">>, Rev2}}
    ],
    {ok, St4} = test_engine_util:apply_actions(Engine, St3, Actions3),

    ?assertEqual(0, Engine:get_doc_count(St4)),
    ?assertEqual(0, Engine:get_del_doc_count(St4)),
    ?assertEqual(5, Engine:get_update_seq(St4)),
    ?assertEqual(2, Engine:get_purge_seq(St4)),
    ?assertEqual([{<<"foo">>, [Rev2]}], Engine:get_last_purged(St4)).


cet_add_delete_purge() ->
    {ok, Engine, St1} = test_engine_util:init_engine(),

    Actions1 = [
        {create, {<<"foo">>, [{<<"vsn">>, 1}]}},
        {delete, {<<"foo">>, [{<<"vsn">>, 2}]}}
    ],

    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions1),

    ?assertEqual(0, Engine:get_doc_count(St2)),
    ?assertEqual(1, Engine:get_del_doc_count(St2)),
    ?assertEqual(2, Engine:get_update_seq(St2)),
    ?assertEqual(0, Engine:get_purge_seq(St2)),
    ?assertEqual([], Engine:get_last_purged(St2)),

    [FDI] = Engine:open_docs(St2, [<<"foo">>]),
    PrevRev = test_engine_util:prev_rev(FDI),
    Rev = PrevRev#rev_info.rev,

    Actions2 = [
        {purge, {<<"foo">>, Rev}}
    ],
    {ok, St3} = test_engine_util:apply_actions(Engine, St2, Actions2),

    ?assertEqual(0, Engine:get_doc_count(St3)),
    ?assertEqual(0, Engine:get_del_doc_count(St3)),
    ?assertEqual(3, Engine:get_update_seq(St3)),
    ?assertEqual(1, Engine:get_purge_seq(St3)),
    ?assertEqual([{<<"foo">>, [Rev]}], Engine:get_last_purged(St3)).


cet_add_two_purge_one() ->
    {ok, Engine, St1} = test_engine_util:init_engine(),

    Actions1 = [
        {create, {<<"foo">>, [{<<"vsn">>, 1}]}},
        {create, {<<"bar">>, []}}
    ],

    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions1),

    ?assertEqual(2, Engine:get_doc_count(St2)),
    ?assertEqual(0, Engine:get_del_doc_count(St2)),
    ?assertEqual(2, Engine:get_update_seq(St2)),
    ?assertEqual(0, Engine:get_purge_seq(St2)),
    ?assertEqual([], Engine:get_last_purged(St2)),

    [FDI] = Engine:open_docs(St2, [<<"foo">>]),
    PrevRev = test_engine_util:prev_rev(FDI),
    Rev = PrevRev#rev_info.rev,

    Actions2 = [
        {purge, {<<"foo">>, Rev}}
    ],
    {ok, St3} = test_engine_util:apply_actions(Engine, St2, Actions2),

    ?assertEqual(1, Engine:get_doc_count(St3)),
    ?assertEqual(0, Engine:get_del_doc_count(St3)),
    ?assertEqual(3, Engine:get_update_seq(St3)),
    ?assertEqual(1, Engine:get_purge_seq(St3)),
    ?assertEqual([{<<"foo">>, [Rev]}], Engine:get_last_purged(St3)).
