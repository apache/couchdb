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

-module(couch_replicator_use_checkpoints_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DOCS_COUNT, 100).
-define(TIMEOUT_EUNIT, 30).
-define(i2l(I), integer_to_list(I)).
-define(io2b(Io), iolist_to_binary(Io)).

setup_checkpoints() ->
    {Ctx, {Source, Target}} = couch_replicator_test_helper:test_setup(),
    Fun = fun notifier_checkpoint_fun/1,
    {ok, Listener} = couch_replicator_notifier:start_link(Fun),
    {Ctx, {Source, Target, Listener}}.

setup_no_checkpoints() ->
    {Ctx, {Source, Target}} = couch_replicator_test_helper:test_setup(),
    Fun = fun notifier_no_checkpoint_fun/1,
    {ok, Listener} = couch_replicator_notifier:start_link(Fun),
    {Ctx, {Source, Target, Listener}}.

teardown({Ctx, {Source, Target, Listener}}) ->
    couch_replicator_notifier:stop(Listener),
    couch_replicator_test_helper:test_teardown({Ctx, {Source, Target}}).

notifier_checkpoint_fun({finished, _, {CheckpointHistory}}) ->
    SId = lists:keyfind(<<"session_id">>, 1, CheckpointHistory),
    SId =/= false orelse ?debugFmt("~nsession_id not found when using checkpoints", []),
    ?assertNotEqual(false, SId);
notifier_checkpoint_fun(_) ->
    ok.

notifier_no_checkpoint_fun({finished, _, {CheckpointHistory}}) ->
    ?assertEqual([{<<"use_checkpoints">>, false}], CheckpointHistory);
notifier_no_checkpoint_fun(_) ->
    ok.

use_checkpoints_test_() ->
    {
        "Replication test using checkpoints",
        {
            foreach,
            fun setup_checkpoints/0,
            fun teardown/1,
            [
                ?TDEF_FE(use_checkpoints, ?TIMEOUT_EUNIT)
            ]
        }
    }.

dont_use_checkpoints_test_() ->
    {
        "Replication test without using checkpoints",
        {
            foreach,
            fun setup_no_checkpoints/0,
            fun teardown/1,
            [
                ?TDEF_FE(dont_use_checkpoints, ?TIMEOUT_EUNIT)
            ]
        }
    }.

use_checkpoints({_Ctx, {Source, Target, _}}) ->
    populate_db(Source, ?DOCS_COUNT),
    replicate(Source, Target, true),
    compare_dbs(Source, Target).

dont_use_checkpoints({_Ctx, {Source, Target, _}}) ->
    populate_db(Source, ?DOCS_COUNT),
    replicate(Source, Target, false),
    compare_dbs(Source, Target).

populate_db(DbName, DocCount) ->
    Docs = lists:foldl(
        fun(DocIdCounter, Acc) ->
            Id = ?io2b(["doc", ?i2l(DocIdCounter)]),
            Value = ?io2b(["val", ?i2l(DocIdCounter)]),
            Doc = #doc{
                id = Id,
                body = {[{<<"value">>, Value}]}
            },
            [Doc | Acc]
        end,
        [],
        lists:seq(1, DocCount)
    ),
    {ok, _} = fabric:update_docs(DbName, Docs, [?ADMIN_CTX]).

compare_dbs(Source, Target) ->
    couch_replicator_test_helper:cluster_compare_dbs(Source, Target).

db_url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).

replicate(Source, Target, UseCheckpoints) ->
    couch_replicator_test_helper:replicate(
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"use_checkpoints">>, UseCheckpoints}
        ]}
    ).
