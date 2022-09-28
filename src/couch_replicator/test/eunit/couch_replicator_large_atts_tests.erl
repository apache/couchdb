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

-define(ATT_SIZE_1, 2 * 1024 * 1024).
-define(ATT_SIZE_2, round(6.6 * 1024 * 1024)).
-define(DOCS_COUNT, 11).
-define(TIMEOUT_EUNIT, 120).

setup() ->
    Ctx = couch_replicator_test_helper:test_setup(),
    config:set("attachments", "compressible_types", "text/*", _Persist = false),
    Ctx.

teardown(Ctx) ->
    config:delete("attachments", "compressible_types", _Persist = false),
    couch_replicator_test_helper:test_teardown(Ctx).

large_atts_test_() ->
    {
        "Replicate docs with large attachments",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(should_replicate_atts, ?TIMEOUT_EUNIT)
            ]
        }
    }.

should_replicate_atts({_Ctx, {Source, Target}}) ->
    populate_db(Source, ?DOCS_COUNT),
    ?assertEqual(ok, replicate(Source, Target)),
    couch_replicator_test_helper:cluster_compare_dbs(Source, Target).

populate_db(DbName, DocCount) ->
    Docs = lists:foldl(
        fun(DocIdCounter, Acc) ->
            Doc = #doc{
                id = integer_to_binary(DocIdCounter),
                body = {[]},
                atts = [
                    att(<<"att1">>, ?ATT_SIZE_1, <<"text/plain">>),
                    att(<<"att2">>, ?ATT_SIZE_2, <<"app/binary">>)
                ]
            },
            [Doc | Acc]
        end,
        [],
        lists:seq(1, DocCount)
    ),
    {ok, _} = fabric:update_docs(DbName, Docs, [?ADMIN_CTX]).

att(Name, Size, Type) ->
    couch_att:new([
        {name, Name},
        {type, Type},
        {att_len, Size},
        {data, fun(Count) -> crypto:strong_rand_bytes(Count) end}
    ]).

db_url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).

replicate(Source, Target) ->
    couch_replicator_test_helper:replicate(db_url(Source), db_url(Target)).
