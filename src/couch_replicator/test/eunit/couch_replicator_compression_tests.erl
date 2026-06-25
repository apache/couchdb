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

-module(couch_replicator_compression_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DOCS_COUNT, 10).
-define(TIMEOUT_EUNIT, 30).

compression_test_() ->
    {
        "Replication compression tests",
        {
            foreach,
            fun couch_replicator_test_helper:test_setup/0,
            fun couch_replicator_test_helper:test_teardown/1,
            [
                ?TDEF_FE(should_not_compress_by_default, ?TIMEOUT_EUNIT),
                ?TDEF_FE(should_compress_when_enabled, ?TIMEOUT_EUNIT)
            ]
        }
    }.

should_not_compress_by_default({_Ctx, {Source, Target}}) ->
    Before = couch_stats:sample([couch_replicator, requests_compressed, gzip]),
    populate_db(Source, ?DOCS_COUNT),
    replicate(Source, Target),
    compare_dbs(Source, Target),
    After = couch_stats:sample([couch_replicator, requests_compressed, gzip]),
    ?assertEqual(Before, After).

should_compress_when_enabled({_Ctx, {Source, Target}}) ->
    config:set("replicator", "compress_requests", "true", false),
    config:set("replicator", "compress_min_size", "10", false),
    config:set("replicator", "compression_algorithm", "gzip", false),
    Before = couch_stats:sample([couch_replicator, requests_compressed, gzip]),
    populate_db(Source, ?DOCS_COUNT),
    replicate(Source, Target),
    compare_dbs(Source, Target),
    After = couch_stats:sample([couch_replicator, requests_compressed, gzip]),
    ?assert(After > Before),
    config:delete("replicator", "compress_requests", false),
    config:delete("replicator", "compress_min_size", false),
    config:delete("replicator", "compression_algorithm", false).

populate_db(DbName, Count) ->
    Docs = lists:map(
        fun(I) ->
            Id = iolist_to_binary(io_lib:format("doc~p", [I])),
            Data = list_to_binary(lists:duplicate(100, $x)),
            {[
                {<<"_id">>, Id},
                {<<"value">>, I},
                {<<"data">>, Data}
            ]}
        end,
        lists:seq(1, Count)
    ),
    {ok, _} = fabric:update_docs(DbName, Docs, [?ADMIN_CTX]),
    ok.

replicate(Source, Target) ->
    SourceUrl = couch_replicator_test_helper:cluster_db_url(Source),
    TargetUrl = couch_replicator_test_helper:cluster_db_url(Target),
    RepObject = {[
        {<<"source">>, SourceUrl},
        {<<"target">>, TargetUrl},
        {<<"continuous">>, false}
    ]},
    {ok, _} = couch_replicator_test_helper:replicate(RepObject).

compare_dbs(Source, Target) ->
    {ok, SourceInfo} = fabric:get_db_info(Source),
    {ok, TargetInfo} = fabric:get_db_info(Target),
    SourceDocCount = couch_util:get_value(doc_count, SourceInfo),
    TargetDocCount = couch_util:get_value(doc_count, TargetInfo),
    ?assertEqual(SourceDocCount, TargetDocCount),
    ?assertEqual(?DOCS_COUNT, TargetDocCount).
