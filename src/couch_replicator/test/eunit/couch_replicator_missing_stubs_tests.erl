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

-module(couch_replicator_missing_stubs_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(REVS_LIMIT, 3).
-define(TIMEOUT_EUNIT, 30).

missing_stubs_test_() ->
    {
        "Replicate docs with missing stubs (COUCHDB-1365)",
        {
            foreach,
            fun couch_replicator_test_helper:test_setup/0,
            fun couch_replicator_test_helper:test_teardown/1,
            [
                ?TDEF_FE(replicate_docs_with_missing_att_stubs, ?TIMEOUT_EUNIT)
            ]
        }
    }.

replicate_docs_with_missing_att_stubs({_Ctx, {Source, Target}}) ->
    populate_db(Source),
    fabric:set_revs_limit(Target, ?REVS_LIMIT, [?ADMIN_CTX]),
    replicate(Source, Target),
    compare(Source, Target),
    update_docs(Source, ?REVS_LIMIT * 2),
    replicate(Source, Target),
    compare(Source, Target).

populate_db(DbName) ->
    AttData = crypto:strong_rand_bytes(6000),
    Doc = #doc{
        id = <<"doc1">>,
        atts = [
            couch_att:new([
                {name, <<"doc1_att1">>},
                {type, <<"application/foobar">>},
                {att_len, byte_size(AttData)},
                {data, AttData}
            ])
        ]
    },
    {ok, _} = fabric:update_doc(DbName, Doc, [?ADMIN_CTX]).

update_docs(DbName, Times) ->
    lists:foreach(
        fun({Id, _Rev}) ->
            {ok, Doc} = fabric:open_doc(DbName, Id, [?ADMIN_CTX]),
            update_doc(DbName, Doc, Times)
        end,
        couch_replicator_test_helper:cluster_doc_revs(DbName)
    ).

update_doc(DbName, Doc, Times) ->
    {Pos0, [Rev0 | _]} = Doc#doc.revs,
    lists:foldl(
        fun(_, {Pos, RevId}) ->
            Val = base64:encode(crypto:strong_rand_bytes(100)),
            NewDoc = Doc#doc{
                revs = {Pos, [RevId]},
                body = {[{<<"value">>, Val}]}
            },
            {ok, NewRev} = fabric:update_doc(DbName, NewDoc, [?ADMIN_CTX]),
            NewRev
        end,
        {Pos0, Rev0},
        lists:seq(1, Times)
    ).

db_url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).

replicate(Source, Target) ->
    couch_replicator_test_helper:replicate(db_url(Source), db_url(Target)).

compare(Source, Target) ->
    couch_replicator_test_helper:cluster_compare_dbs(Source, Target).
