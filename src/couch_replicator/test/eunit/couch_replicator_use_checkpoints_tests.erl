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
-include_lib("fabric/test/fabric2_test.hrl").


-define(DOCS_COUNT, 100).
-define(i2l(I), integer_to_list(I)).
-define(io2b(Io), iolist_to_binary(Io)).


use_checkpoints_test_() ->
    {
        setup,
        fun couch_replicator_test_helper:start_couch/0,
        fun couch_replicator_test_helper:stop_couch/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_replicate_with_checkpoints, 15),
                ?TDEF_FE(t_replicate_without_checkpoints, 15)
            ]
        }
    }.


setup() ->
    Source = couch_replicator_test_helper:create_db(),
    Target = couch_replicator_test_helper:create_db(),
    {Source, Target}.


teardown({Source, Target}) ->
    couch_replicator_test_helper:delete_db(Source),
    couch_replicator_test_helper:delete_db(Target).


t_replicate_with_checkpoints({Source, Target}) ->
    populate_db(Source, ?DOCS_COUNT),
    Res = couch_replicator_test_helper:replicate(#{
        <<"source">> => Source,
        <<"target">> => Target,
        <<"use_checkpoints">> => true
    }),
    ?assertMatch({ok, _}, Res),

    {ok, History} = Res,
    ?assertMatch(#{<<"history">> := _, <<"session_id">> := _}, History),

    Checkpoints = maps:get(<<"history">>, History),
    SessionId = maps:get(<<"session_id">>, History),
    ?assert(is_binary(SessionId)),
    ?assert(is_list(Checkpoints)),
    ?assert(length(Checkpoints) >= 1),

    couch_replicator_test_helper:compare_dbs(Source, Target).


t_replicate_without_checkpoints({Source, Target}) ->
    populate_db(Source, ?DOCS_COUNT),
    Res = couch_replicator_test_helper:replicate(#{
        <<"source">> => Source,
        <<"target">> => Target,
        <<"use_checkpoints">> => false
    }),
    ?assertEqual({ok, #{<<"use_checkpoints">> => false}}, Res),
    couch_replicator_test_helper:compare_dbs(Source, Target).


populate_db(DbName, DocCount) ->
    Docs = lists:foldl(fun(DocIdCounter, Acc) ->
        Id = ?io2b(["doc", ?i2l(DocIdCounter)]),
        Value = ?io2b(["val", ?i2l(DocIdCounter)]),
        Doc = #doc{
            id = Id,
            body = {[{<<"value">>, Value}]}
        },
        [Doc | Acc]
    end, [], lists:seq(1, DocCount)),
    couch_replicator_test_helper:create_docs(DbName, Docs).
