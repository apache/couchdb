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

-module(couch_replicator_selector_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

selector_replication_test_() ->
    {
        "Selector filtered replication tests",
        {
            foreach,
            fun couch_replicator_test_helper:test_setup/0,
            fun couch_replicator_test_helper:test_teardown/1,
            [
                ?TDEF_FE(should_succeed)
            ]
        }
    }.

should_succeed({_Ctx, {Source, Target}}) ->
    create_docs(Source),
    replicate(
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"selector">>, {[{<<"_id">>, <<"doc2">>}]}}
        ]}
    ),
    %% FilteredFun is an Erlang version of following mango selector
    FilterFun = fun(DocId, #doc{}) ->
        DocId == <<"doc2">>
    end,
    {TargetDocCount, AllReplies} = compare_dbs(Source, Target, FilterFun),
    % Target DB has proper number of docs
    ?assertEqual(1, TargetDocCount),
    % All the docs selected as expected
    ?assert(lists:all(fun(Valid) -> Valid end, AllReplies)).

compare_dbs(Source, Target, FilterFun) ->
    {ok, TargetDocCount} = fabric:get_doc_count(Target),
    Replies = lists:foldl(
        fun({Id, Rev}, Acc) ->
            SrcDoc = read_doc(Source, Id, Rev),
            TgtDoc = read_doc(Target, Id, Rev),
            case FilterFun(Id, SrcDoc) of
                true ->
                    [is_record(TgtDoc, doc) | Acc];
                false ->
                    [TgtDoc =:= not_found | Acc]
            end
        end,
        [],
        couch_replicator_test_helper:cluster_doc_revs(Source)
    ),
    {TargetDocCount, Replies}.

read_doc(Db, DocId, Rev) ->
    couch_replicator_test_helper:cluster_open_rev(Db, DocId, Rev).

create_docs(DbName) ->
    Doc1 = couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"doc1">>}
        ]}
    ),
    Doc2 = couch_doc:from_json_obj(
        {[
            {<<"_id">>, <<"doc2">>}
        ]}
    ),
    {ok, _} = fabric:update_docs(DbName, [Doc1, Doc2], [?ADMIN_CTX]).

db_url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).

replicate(RepObject) ->
    couch_replicator_test_helper:replicate(RepObject).
