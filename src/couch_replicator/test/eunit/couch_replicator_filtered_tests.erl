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

-module(couch_replicator_filtered_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DDOC,
    {[
        {<<"_id">>, <<"_design/filter_ddoc">>},
        {<<"filters">>,
            {[
                {<<"testfilter">>, <<
                    "\n"
                    "            function(doc, req){if (doc.class == 'mammal') return true;}\n"
                    "        "
                >>},
                {<<"queryfilter">>, <<
                    "\n"
                    "            function(doc, req) {\n"
                    "                if (doc.class && req.query.starts) {\n"
                    "                    return doc.class.indexOf(req.query.starts) === 0;\n"
                    "                }\n"
                    "                else {\n"
                    "                    return false;\n"
                    "                }\n"
                    "            }\n"
                    "        "
                >>}
            ]}},
        {<<"views">>,
            {[
                {<<"mammals">>,
                    {[
                        {<<"map">>, <<
                            "\n"
                            "                function(doc) {\n"
                            "                    if (doc.class == 'mammal') {\n"
                            "                        emit(doc._id, null);\n"
                            "                    }\n"
                            "                }\n"
                            "            "
                        >>}
                    ]}}
            ]}}
    ]}
).

filtered_replication_test_() ->
    {
        "Filtered replication tests",
        {
            foreach,
            fun couch_replicator_test_helper:test_setup/0,
            fun couch_replicator_test_helper:test_teardown/1,
            [
                ?TDEF_FE(should_succeed),
                ?TDEF_FE(should_succeed_with_query),
                ?TDEF_FE(should_succeed_with_view)
            ]
        }
    }.

should_succeed({_Ctx, {Source, Target}}) ->
    create_docs(Source),
    RepObject =
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"filter">>, <<"filter_ddoc/testfilter">>}
        ]},
    replicate(RepObject),
    %% FilteredFun is an Erlang version of following JS function
    %% function(doc, req){if (doc.class == 'mammal') return true;}
    FilterFun = fun(_DocId, #doc{body = {Props}}) ->
        couch_util:get_value(<<"class">>, Props) == <<"mammal">>
    end,
    {TargetDocCount, AllReplies} = compare_dbs(Source, Target, FilterFun),
    % Target DB has proper number of docs,
    ?assertEqual(1, TargetDocCount),
    % All the docs filtered as expected
    ?assert(lists:all(fun(Valid) -> Valid end, AllReplies)).

should_succeed_with_query({_Ctx, {Source, Target}}) ->
    create_docs(Source),
    RepObject =
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"filter">>, <<"filter_ddoc/queryfilter">>},
            {<<"query_params">>,
                {[
                    {<<"starts">>, <<"a">>}
                ]}}
        ]},
    replicate(RepObject),
    FilterFun = fun(_DocId, #doc{body = {Props}}) ->
        case couch_util:get_value(<<"class">>, Props) of
            <<"a", _/binary>> -> true;
            _ -> false
        end
    end,
    {TargetDocCount, AllReplies} = compare_dbs(Source, Target, FilterFun),
    % Target DB has proper number of docs
    ?assertEqual(2, TargetDocCount),
    % All the docs filtered as expected,
    ?assert(lists:all(fun(Valid) -> Valid end, AllReplies)).

should_succeed_with_view({_Ctx, {Source, Target}}) ->
    create_docs(Source),
    RepObject =
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"filter">>, <<"_view">>},
            {<<"query_params">>,
                {[
                    {<<"view">>, <<"filter_ddoc/mammals">>}
                ]}}
        ]},
    replicate(RepObject),
    FilterFun = fun(_DocId, #doc{body = {Props}}) ->
        couch_util:get_value(<<"class">>, Props) == <<"mammal">>
    end,
    {TargetDocCount, AllReplies} = compare_dbs(Source, Target, FilterFun),
    % Target DB has proper number of docs
    ?assertEqual(1, TargetDocCount),
    % All the docs filtered as expected
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
    Docs = [
        couch_doc:from_json_obj(?DDOC),
        #doc{
            id = <<"doc1">>,
            body =
                {[
                    {<<"class">>, <<"mammal">>},
                    {<<"value">>, 1}
                ]}
        },
        #doc{
            id = <<"doc2">>,
            body =
                {[
                    {<<"class">>, <<"amphibians">>},
                    {<<"value">>, 2}
                ]}
        },
        #doc{
            id = <<"doc3">>,
            body =
                {[
                    {<<"class">>, <<"reptiles">>},
                    {<<"value">>, 3}
                ]}
        },
        #doc{
            id = <<"doc4">>,
            body =
                {[
                    {<<"class">>, <<"arthropods">>},
                    {<<"value">>, 2}
                ]}
        }
    ],
    {ok, [_ | _]} = fabric:update_docs(DbName, Docs, [?ADMIN_CTX]).

db_url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).

replicate(RepObject) ->
    couch_replicator_test_helper:replicate(RepObject).
