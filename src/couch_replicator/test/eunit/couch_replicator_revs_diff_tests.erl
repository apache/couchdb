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

-module(couch_replicator_revs_diff_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DOC_COUNT, 100).
-define(BATCH_SIZE, 5).

revs_diff_test_() ->
    {
        "Use _revs_diff when replicating",
        {
            foreach,
            fun couch_replicator_test_helper:test_setup/0,
            fun couch_replicator_test_helper:test_teardown/1,
            [
                ?TDEF_FE(use_revs_diff_when_most_docs_are_present, 15),
                ?TDEF_FE(skip_revs_diff_when_most_docs_are_missing, 15)
            ]
        }
    }.

use_revs_diff_when_most_docs_are_present({_Ctx, {Source, Target}}) ->
    populate_db(Source, ?DOC_COUNT),
    populate_db(Target, ?DOC_COUNT),
    meck:new(couch_replicator_api_wrap, [passthrough]),
    replicate(Source, Target),
    Calls = meck:num_calls(couch_replicator_api_wrap, get_missing_revs, 2),
    ExpectAtLeast = ?DOC_COUNT / ?BATCH_SIZE,
    ?assert(Calls >= ExpectAtLeast).

skip_revs_diff_when_most_docs_are_missing({_Ctx, {Source, Target}}) ->
    populate_db(Source, ?DOC_COUNT),
    meck:new(couch_replicator_api_wrap, [passthrough]),
    replicate(Source, Target),
    Calls = meck:num_calls(couch_replicator_api_wrap, get_missing_revs, 2),
    % This is not exact. But expect to skip at least half the revs_diffs calls.
    ExpectAtMost = (?DOC_COUNT / ?BATCH_SIZE) / 2,
    ?assert(Calls =< ExpectAtMost).

populate_db(DbName, DocCount) ->
    Fun = fun(Id, Acc) -> [#doc{id = integer_to_binary(Id)} | Acc] end,
    Docs = lists:foldl(Fun, [], lists:seq(1, DocCount)),
    {ok, _} = fabric:update_docs(DbName, Docs, [?ADMIN_CTX]).

db_url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).

replicate(Source, Target) ->
    couch_replicator_test_helper:replicate(
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"worker_processes">>, <<"1">>},
            {<<"worker_batch_size">>, integer_to_binary(?BATCH_SIZE)}
        ]}
    ).
