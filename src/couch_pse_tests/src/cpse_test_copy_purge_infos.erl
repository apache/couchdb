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

-module(cpse_test_copy_purge_infos).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(NUM_DOCS, 100).

setup_each() ->
    {ok, SrcDb} = cpse_util:create_db(),
    {ok, SrcDb2} = create_and_purge(SrcDb),
    {ok, TrgDb} = cpse_util:create_db(),
    {SrcDb2, TrgDb}.

teardown_each({SrcDb, TrgDb}) ->
    ok = couch_server:delete(couch_db:name(SrcDb), []),
    ok = couch_server:delete(couch_db:name(TrgDb), []).

cpse_copy_empty_purged_info({_, Db}) ->
    {ok, Db1} = couch_db_engine:copy_purge_infos(Db, []),
    ?assertEqual(ok, cpse_util:assert_each_prop(Db1, [{purge_infos, []}])).

cpse_copy_purged_info({SrcDb, TrgDb}) ->
    {ok, RPIs} = couch_db_engine:fold_purge_infos(
        SrcDb,
        0,
        fun(PI, Acc) ->
            {ok, [PI | Acc]}
        end,
        [],
        []
    ),
    PIs = lists:reverse(RPIs),
    AEPFold = fun({PSeq, UUID, Id, Revs}, {CPSeq, CPurges}) ->
        {max(PSeq, CPSeq), [{UUID, Id, Revs} | CPurges]}
    end,
    {PurgeSeq, RPurges} = lists:foldl(AEPFold, {0, []}, PIs),
    Purges = lists:reverse(RPurges),
    {ok, TrgDb2} = couch_db_engine:copy_purge_infos(TrgDb, PIs),
    AssertProps = [{purge_seq, PurgeSeq}, {purge_infos, Purges}],
    ?assertEqual(ok, cpse_util:assert_each_prop(TrgDb2, AssertProps)).

create_and_purge(Db) ->
    {RActions, RIds} = lists:foldl(
        fun(Id, {CActions, CIds}) ->
            Id1 = docid(Id),
            Action = {create, {Id1, {[{<<"int">>, Id}]}}},
            {[Action | CActions], [Id1 | CIds]}
        end,
        {[], []},
        lists:seq(1, ?NUM_DOCS)
    ),
    Actions = lists:reverse(RActions),
    Ids = lists:reverse(RIds),
    {ok, Db1} = cpse_util:apply_batch(Db, Actions),

    FDIs = couch_db_engine:open_docs(Db1, Ids),
    RActions2 = lists:foldl(
        fun(FDI, CActions) ->
            Id = FDI#full_doc_info.id,
            PrevRev = cpse_util:prev_rev(FDI),
            Rev = PrevRev#rev_info.rev,
            Action = {purge, {Id, Rev}},
            [Action | CActions]
        end,
        [],
        FDIs
    ),
    Actions2 = lists:reverse(RActions2),
    {ok, Db2} = cpse_util:apply_batch(Db1, Actions2),
    {ok, Db2}.

docid(I) ->
    Str = io_lib:format("~4..0b", [I]),
    iolist_to_binary(Str).
