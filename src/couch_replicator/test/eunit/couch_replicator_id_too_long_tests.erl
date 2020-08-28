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

-module(couch_replicator_id_too_long_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


id_too_long_replication_test_() ->
    {
        "Doc id too long tests",
        {
            setup,
            fun couch_replicator_test_helper:start_couch/0,
            fun couch_replicator_test_helper:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(should_succeed),
                    ?TDEF_FE(should_fail)

                ]
            }
        }
    }.


setup() ->
    Source = couch_replicator_test_helper:create_db(),
    create_doc(Source),
    Target = couch_replicator_test_helper:create_db(),
    {Source, Target}.


teardown({Source, Target}) ->
    config:delete("replicator", "max_document_id_length", false),
    couch_replicator_test_helper:delete_db(Source),
    couch_replicator_test_helper:delete_db(Target).


should_succeed({Source, Target}) ->
    config:set("replicator", "max_document_id_length", "5", false),
    {ok, _} = couch_replicator_test_helper:replicate(Source, Target),
    ?assertEqual(ok, couch_replicator_test_helper:compare_dbs(Source, Target)).


should_fail({Source, Target}) ->
    config:set("replicator", "max_document_id_length", "4", false),
    {ok, _} = couch_replicator_test_helper:replicate(Source, Target),
    ExceptIds = [<<"12345">>],
    ?assertEqual(ok, couch_replicator_test_helper:compare_dbs(Source, Target,
        ExceptIds)).


create_doc(DbName) ->
    Docs = [#{<<"_id">> => <<"12345">>}],
    couch_replicator_test_helper:create_docs(DbName, Docs).
