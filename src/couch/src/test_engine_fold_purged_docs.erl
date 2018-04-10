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

-module(test_engine_fold_purged_docs).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(NUM_DOCS, 100).


cet_empty_purged_docs() ->
    {ok, Engine, St} = test_engine_util:init_engine(),
    ?assertEqual({ok, []}, Engine:fold_purged_docs(St, 0, fun fold_fun/2, [], [])).


cet_all_purged_docs() ->
    {ok, Engine, St1} = test_engine_util:init_engine(),

    {RActions, RIds} = lists:foldl(fun(Id, {CActions, CIds}) ->
        Id1 = docid(Id),
        Action = {create, {Id1, [{<<"int">>, Id}]}},
        {[Action| CActions], [Id1| CIds]}
     end, {[], []}, lists:seq(1, ?NUM_DOCS)),
    Actions = lists:reverse(RActions),
    Ids = lists:reverse(RIds),
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions),

    FDIs = Engine:open_docs(St2, Ids),
    {RevActions2, RevIdRevs} = lists:foldl(fun(FDI, {CActions, CIdRevs}) ->
        Id = FDI#full_doc_info.id,
        PrevRev = test_engine_util:prev_rev(FDI),
        Rev = PrevRev#rev_info.rev,
        Action = {purge, {Id, Rev}},
        {[Action| CActions], [{Id, [Rev]}| CIdRevs]}
     end, {[], []}, FDIs),
    {Actions2, IdsRevs} = {lists:reverse(RevActions2), lists:reverse(RevIdRevs)},

    {ok, St3} = test_engine_util:apply_actions(Engine, St2, Actions2),
    {ok, PurgedIdRevs} = Engine:fold_purged_docs(St3, 0, fun fold_fun/2, [], []),
    ?assertEqual(IdsRevs, lists:reverse(PurgedIdRevs)).


cet_start_seq() ->
    {ok, Engine, St1} = test_engine_util:init_engine(),
    Actions1 = [
        {create, {docid(1), [{<<"int">>, 1}]}},
        {create, {docid(2), [{<<"int">>, 2}]}},
        {create, {docid(3), [{<<"int">>, 3}]}},
        {create, {docid(4), [{<<"int">>, 4}]}},
        {create, {docid(5), [{<<"int">>, 5}]}}
    ],
    Ids = [docid(1), docid(2), docid(3), docid(4), docid(5)],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions1),

    FDIs = Engine:open_docs(St2, Ids),
    {RActions2, RIdRevs} = lists:foldl(fun(FDI, {CActions, CIdRevs}) ->
        Id = FDI#full_doc_info.id,
        PrevRev = test_engine_util:prev_rev(FDI),
        Rev = PrevRev#rev_info.rev,
        Action = {purge, {Id, Rev}},
        {[Action| CActions], [{Id, [Rev]}| CIdRevs]}
    end, {[], []}, FDIs),
    {ok, St3} = test_engine_util:apply_actions(Engine, St2, lists:reverse(RActions2)),

    StartSeq = 3,
    StartSeqIdRevs = lists:nthtail(StartSeq, lists:reverse(RIdRevs)),
    {ok, PurgedIdRevs} = Engine:fold_purged_docs(St3, StartSeq, fun fold_fun/2, [], []),
    ?assertEqual(StartSeqIdRevs, lists:reverse(PurgedIdRevs)).


cet_id_rev_repeated() ->
    {ok, Engine, St1} = test_engine_util:init_engine(),

    Actions1 = [
        {create, {<<"foo">>, [{<<"vsn">>, 1}]}},
        {conflict, {<<"foo">>, [{<<"vsn">>, 2}]}}
    ],
    {ok, St2} = test_engine_util:apply_actions(Engine, St1, Actions1),

    [FDI1] = Engine:open_docs(St2, [<<"foo">>]),
    PrevRev1 = test_engine_util:prev_rev(FDI1),
    Rev1 = PrevRev1#rev_info.rev,
    Actions2 = [
        {purge, {<<"foo">>, Rev1}}
    ],
    {ok, St3} = test_engine_util:apply_actions(Engine, St2, Actions2),
    PurgedIdRevs0 = [{<<"foo">>, [Rev1]}],
    {ok, PurgedIdRevs1} = Engine:fold_purged_docs(St3, 0, fun fold_fun/2, [], []),
    ?assertEqual(PurgedIdRevs0, PurgedIdRevs1),
    ?assertEqual(1, Engine:get_purge_seq(St3)),

    % purge the same Id,Rev when the doc still exists
    {ok, St4} = test_engine_util:apply_actions(Engine, St3, Actions2),
    {ok, PurgedIdRevs2} = Engine:fold_purged_docs(St4, 0, fun fold_fun/2, [], []),
    ?assertEqual(PurgedIdRevs0, PurgedIdRevs2),
    ?assertEqual(1, Engine:get_purge_seq(St4)),

    [FDI2] = Engine:open_docs(St4, [<<"foo">>]),
    PrevRev2 = test_engine_util:prev_rev(FDI2),
    Rev2 = PrevRev2#rev_info.rev,
    Actions3 = [
        {purge, {<<"foo">>, Rev2}}
    ],
    {ok, St5} = test_engine_util:apply_actions(Engine, St4, Actions3),
    PurgedIdRevs00 = [{<<"foo">>, [Rev1]}, {<<"foo">>, [Rev2]}],

    % purge the same Id,Rev when the doc was completely purged
    {ok, St6} = test_engine_util:apply_actions(Engine, St5, Actions3),
    {ok, PurgedIdRevs3} = Engine:fold_purged_docs(St6, 0, fun fold_fun/2, [], []),
    ?assertEqual(PurgedIdRevs00, lists:reverse(PurgedIdRevs3)),
    ?assertEqual(2, Engine:get_purge_seq(St6)).


fold_fun({_PSeq, _UUID, Id, Revs}, Acc) ->
    [{Id, Revs} | Acc].


docid(I) ->
    Str = io_lib:format("~4..0b", [I]),
    iolist_to_binary(Str).
