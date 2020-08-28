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
-include_lib("couch_replicator/src/couch_replicator.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


attachment_too_large_replication_test_() ->
    {
        setup,
        fun couch_replicator_test_helper:start_couch/0,
        fun couch_replicator_test_helper:stop_couch/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_should_succeed),
                ?TDEF_FE(t_should_fail)
            ]
        }
    }.


setup() ->
    Source = couch_replicator_test_helper:create_db(),
    create_doc_with_attachment(Source, <<"doc">>, 1000),
    Target = couch_replicator_test_helper:create_db(),
    {Source, Target}.


teardown({Source, Target}) ->
    config:delete("couchdb", "max_attachment_size", false),
    couch_replicator_test_helper:delete_db(Source),
    couch_replicator_test_helper:delete_db(Target).


t_should_succeed({Source, Target}) ->
    config:set("couchdb", "max_attachment_size", "1000", false),
    {ok, _} = couch_replicator_test_helper:replicate(Source, Target),
    ?assertEqual(ok, couch_replicator_test_helper:compare_dbs(Source, Target)).


t_should_fail({Source, Target}) ->
    config:set("couchdb", "max_attachment_size", "999", false),
    {ok, _} = couch_replicator_test_helper:replicate(Source, Target),
    ExceptIds = [<<"doc">>],
    ?assertEqual(ok, couch_replicator_test_helper:compare_dbs(Source,
        Target, ExceptIds)).


create_doc_with_attachment(DbName, DocId, AttSize) ->
    Doc = #doc{id = DocId, atts = att(AttSize)},
    couch_replicator_test_helper:create_docs(DbName, [Doc]),
    ok.


att(Size) when is_integer(Size), Size >= 1 ->
    [couch_att:new([
        {name, <<"att">>},
        {type, <<"app/binary">>},
        {att_len, Size},
        {data, fun(_Bytes) ->
            << <<"x">> || _ <- lists:seq(1, Size) >>
        end}
    ])].
