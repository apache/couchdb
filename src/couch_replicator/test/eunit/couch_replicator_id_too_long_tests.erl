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

id_too_long_replication_test_() ->
    {
        "Doc id too long tests",
        {
            foreach,
            fun couch_replicator_test_helper:test_setup/0,
            fun couch_replicator_test_helper:test_teardown/1,
            [
                ?TDEF_FE(should_succeed),
                ?TDEF_FE(should_fail)
            ]
        }
    }.

should_succeed({_Ctx, {Source, Target}}) ->
    create_doc(Source),
    config:set("replicator", "max_document_id_length", "5", _Persist = false),
    replicate(Source, Target),
    ?assertEqual(ok, compare(Source, Target)).

should_fail({_Ctx, {Source, Target}}) ->
    create_doc(Source),
    config:set("replicator", "max_document_id_length", "4", _Persist = false),
    replicate(Source, Target),
    ?assertError({not_found, <<"12345">>}, compare(Source, Target)).

create_doc(DbName) ->
    Doc = couch_doc:from_json_obj({[{<<"_id">>, <<"12345">>}]}),
    {ok, _} = fabric:update_doc(DbName, Doc, [?ADMIN_CTX]).

db_url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).

compare(Source, Target) ->
    couch_replicator_test_helper:cluster_compare_dbs(Source, Target).

replicate(Source, Target) ->
    couch_replicator_test_helper:replicate(db_url(Source), db_url(Target)).
