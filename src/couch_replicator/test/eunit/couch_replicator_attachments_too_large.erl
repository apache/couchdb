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

-module(couch_replicator_attachments_too_large).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

attachment_too_large_replication_test_() ->
    {
        "Attachment size too large replication tests",
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
    create_doc_with_attachment(Source, <<"doc">>, 1000),
    config:set("couchdb", "max_attachment_size", "1000", _Persist = false),
    ok = replicate(Source, Target),
    ?assertEqual(ok, compare(Source, Target)).

should_fail({_Ctx, {Source, Target}}) ->
    create_doc_with_attachment(Source, <<"doc">>, 1000),
    config:set("couchdb", "max_attachment_size", "999", _Persist = false),
    ok = replicate(Source, Target),
    ?assertError({not_found, <<"doc">>}, compare(Source, Target)).

create_doc_with_attachment(DbName, DocId, AttSize) ->
    Doc = #doc{id = DocId, atts = att(AttSize)},
    {ok, {1, _Rev}} = fabric:update_doc(DbName, Doc, [?ADMIN_CTX]).

att(Size) when is_integer(Size), Size >= 1 ->
    [
        couch_att:new([
            {name, <<"att">>},
            {type, <<"app/binary">>},
            {att_len, Size},
            {data, fun(_Bytes) ->
                <<<<"x">> || _ <- lists:seq(1, Size)>>
            end}
        ])
    ].

db_url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).

replicate(Source, Target) ->
    couch_replicator_test_helper:replicate(db_url(Source), db_url(Target)).

compare(Source, Target) ->
    couch_replicator_test_helper:cluster_compare_dbs(Source, Target).
