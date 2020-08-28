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

-module(couch_replicator_large_atts_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


-define(ATT_SIZE_1, 2 * 1024 * 1024).
-define(ATT_SIZE_2, round(6.6 * 1024 * 1024)).
-define(DOCS_COUNT, 11).
-define(TIMEOUT_EUNIT, 120).


large_atts_test_() ->
    {
        "Large attachment replication test",
        {
            setup,
            fun couch_replicator_test_helper:start_couch/0,
            fun couch_replicator_test_helper:stop_couch/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    ?TDEF_FE(should_replicate_attachments, 120)
                ]
            }
        }
    }.


setup() ->
    AttCfg = config:get("attachments", "compressible_types"),
    config:set("attachments", "compressible_types", "text/*", false),
    Source = couch_replicator_test_helper:create_db(),
    ok = populate_db(Source, ?DOCS_COUNT),
    Target = couch_replicator_test_helper:create_db(),
    {AttCfg, Source, Target}.


teardown({AttCfg, Source, Target}) ->
    couch_replicator_test_helper:delete_db(Source),
    couch_replicator_test_helper:delete_db(Target),
    case AttCfg of
        undefined ->
            config:delete("attachments", "compressible_types", false);
        _ ->
            config:set("attachments", "compressible_types", AttCfg)
    end.


should_replicate_attachments({_AttCfg, Source, Target}) ->
    ?assertMatch({ok, _},
        couch_replicator_test_helper:replicate(Source, Target)),
    ?assertEqual(ok, couch_replicator_test_helper:compare_dbs(Source, Target)).


populate_db(DbName, DocCount) ->
    Docs = lists:foldl(fun(DocIdCounter, Acc) ->
        Doc = #doc{
            id = iolist_to_binary(["doc", integer_to_list(DocIdCounter)]),
            body = {[]},
            atts = [
                att(<<"att1">>, ?ATT_SIZE_1, <<"text/plain">>),
                att(<<"att2">>, ?ATT_SIZE_2, <<"app/binary">>)
            ]
        },
        [Doc | Acc]
    end, [], lists:seq(1, DocCount)),
    couch_replicator_test_helper:create_docs(DbName, Docs).


att(Name, Size, Type) ->
    couch_att:new([
        {name, Name},
        {type, Type},
        {att_len, Size},
        {data, fun(Count) -> crypto:strong_rand_bytes(Count) end}
    ]).
