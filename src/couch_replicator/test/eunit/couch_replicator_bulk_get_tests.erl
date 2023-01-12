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

-module(couch_replicator_bulk_get_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DOC_COUNT, 10).

bulk_get_test_() ->
    {
        "Use _bulk_get when replicating",
        {
            foreach,
            fun couch_replicator_test_helper:test_setup/0,
            fun couch_replicator_test_helper:test_teardown/1,
            [
                ?TDEF_FE(use_bulk_get),
                ?TDEF_FE(use_bulk_get_with_ddocs),
                ?TDEF_FE(use_bulk_get_with_attachments),
                ?TDEF_FE(dont_use_bulk_get),
                ?TDEF_FE(dont_use_bulk_get_ddocs),
                ?TDEF_FE(dont_use_bulk_get_attachments),
                ?TDEF_FE(job_enable_overrides_global_disable),
                ?TDEF_FE(global_disable_works)
            ]
        }
    }.

use_bulk_get({_Ctx, {Source, Target}}) ->
    populate_db(Source, ?DOC_COUNT),
    meck:new(couch_replicator_api_wrap, [passthrough]),
    replicate(Source, Target, true),
    BulkGets = meck:num_calls(couch_replicator_api_wrap, bulk_get, 3),
    JustGets = meck:num_calls(couch_replicator_api_wrap, open_doc_revs, 6),
    DocUpdates = meck:num_calls(couch_replicator_api_wrap, update_doc, 4),
    ?assertEqual(0, JustGets),
    ?assertEqual(0, DocUpdates),
    ?assert(BulkGets >= 1),
    compare_dbs(Source, Target).

use_bulk_get_with_ddocs({_Ctx, {Source, Target}}) ->
    populate_db_ddocs(Source, ?DOC_COUNT),
    meck:new(couch_replicator_api_wrap, [passthrough]),
    replicate(Source, Target, true),
    BulkGets = meck:num_calls(couch_replicator_api_wrap, bulk_get, 3),
    JustGets = meck:num_calls(couch_replicator_api_wrap, open_doc_revs, 6),
    DocUpdates = meck:num_calls(couch_replicator_api_wrap, update_doc, 4),
    ?assertEqual(?DOC_COUNT, JustGets),
    ?assertEqual(?DOC_COUNT, DocUpdates),
    ?assert(BulkGets >= 1),
    compare_dbs(Source, Target).

use_bulk_get_with_attachments({_Ctx, {Source, Target}}) ->
    populate_db_atts(Source, ?DOC_COUNT),
    meck:new(couch_replicator_api_wrap, [passthrough]),
    replicate(Source, Target, true),
    BulkGets = meck:num_calls(couch_replicator_api_wrap, bulk_get, 3),
    JustGets = meck:num_calls(couch_replicator_api_wrap, open_doc_revs, 6),
    DocUpdates = meck:num_calls(couch_replicator_api_wrap, update_doc, 4),
    ?assertEqual(?DOC_COUNT, JustGets),
    ?assertEqual(?DOC_COUNT, DocUpdates),
    ?assert(BulkGets >= 1),
    compare_dbs(Source, Target).

dont_use_bulk_get({_Ctx, {Source, Target}}) ->
    populate_db(Source, ?DOC_COUNT),
    meck:new(couch_replicator_api_wrap, [passthrough]),
    replicate(Source, Target, false),
    BulkGets = meck:num_calls(couch_replicator_api_wrap, bulk_get, 3),
    JustGets = meck:num_calls(couch_replicator_api_wrap, open_doc_revs, 6),
    DocUpdates = meck:num_calls(couch_replicator_api_wrap, update_doc, 4),
    ?assertEqual(0, BulkGets),
    ?assertEqual(0, DocUpdates),
    ?assertEqual(?DOC_COUNT, JustGets),
    compare_dbs(Source, Target).

dont_use_bulk_get_ddocs({_Ctx, {Source, Target}}) ->
    populate_db_ddocs(Source, ?DOC_COUNT),
    meck:new(couch_replicator_api_wrap, [passthrough]),
    replicate(Source, Target, false),
    BulkGets = meck:num_calls(couch_replicator_api_wrap, bulk_get, 3),
    JustGets = meck:num_calls(couch_replicator_api_wrap, open_doc_revs, 6),
    DocUpdates = meck:num_calls(couch_replicator_api_wrap, update_doc, 4),
    ?assertEqual(0, BulkGets),
    ?assertEqual(?DOC_COUNT, JustGets),
    ?assertEqual(?DOC_COUNT, DocUpdates),
    compare_dbs(Source, Target).

dont_use_bulk_get_attachments({_Ctx, {Source, Target}}) ->
    populate_db_atts(Source, ?DOC_COUNT),
    meck:new(couch_replicator_api_wrap, [passthrough]),
    replicate(Source, Target, false),
    BulkGets = meck:num_calls(couch_replicator_api_wrap, bulk_get, 3),
    JustGets = meck:num_calls(couch_replicator_api_wrap, open_doc_revs, 6),
    DocUpdates = meck:num_calls(couch_replicator_api_wrap, update_doc, 4),
    ?assertEqual(0, BulkGets),
    ?assertEqual(?DOC_COUNT, JustGets),
    ?assertEqual(?DOC_COUNT, DocUpdates),
    compare_dbs(Source, Target).

job_enable_overrides_global_disable({_Ctx, {Source, Target}}) ->
    populate_db(Source, ?DOC_COUNT),
    Persist = false,
    config:set("replicator", "use_bulk_get", "false", Persist),
    meck:new(couch_replicator_api_wrap, [passthrough]),
    replicate(Source, Target, true),
    BulkGets = meck:num_calls(couch_replicator_api_wrap, bulk_get, 3),
    JustGets = meck:num_calls(couch_replicator_api_wrap, open_doc_revs, 6),
    ?assertEqual(0, JustGets),
    ?assert(BulkGets >= 1),
    compare_dbs(Source, Target).

global_disable_works({_Ctx, {Source, Target}}) ->
    populate_db(Source, ?DOC_COUNT),
    Persist = false,
    config:set("replicator", "use_bulk_get", "false", Persist),
    meck:new(couch_replicator_api_wrap, [passthrough]),
    replicate(Source, Target),
    BulkGets = meck:num_calls(couch_replicator_api_wrap, bulk_get, 3),
    JustGets = meck:num_calls(couch_replicator_api_wrap, open_doc_revs, 6),
    ?assertEqual(0, BulkGets),
    ?assertEqual(?DOC_COUNT, JustGets),
    compare_dbs(Source, Target).

populate_db(DbName, DocCount) ->
    IdFun = fun(Id) -> integer_to_binary(Id) end,
    Fun = fun(Id, Acc) -> [#doc{id = IdFun(Id)} | Acc] end,
    Docs = lists:foldl(Fun, [], lists:seq(1, DocCount)),
    {ok, _} = fabric:update_docs(DbName, Docs, [?ADMIN_CTX]).

populate_db_ddocs(DbName, DocCount) ->
    IdFun = fun(Id) -> <<"_design/", (integer_to_binary(Id))/binary>> end,
    Fun = fun(Id, Acc) -> [#doc{id = IdFun(Id)} | Acc] end,
    Docs = lists:foldl(Fun, [], lists:seq(1, DocCount)),
    {ok, _} = fabric:update_docs(DbName, Docs, [?ADMIN_CTX]).

populate_db_atts(DbName, DocCount) ->
    IdFun = fun(Id) -> integer_to_binary(Id) end,
    Fun = fun(Id, Acc) -> [#doc{id = IdFun(Id), atts = [att(<<"a">>)]} | Acc] end,
    Docs = lists:foldl(Fun, [], lists:seq(1, DocCount)),
    {ok, _} = fabric:update_docs(DbName, Docs, [?ADMIN_CTX]).

att(Name) when is_binary(Name) ->
    couch_att:new([
        {name, Name},
        {att_len, 1},
        {type, <<"app/binary">>},
        {data, <<"x">>}
    ]).

compare_dbs(Source, Target) ->
    couch_replicator_test_helper:cluster_compare_dbs(Source, Target).

db_url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).

replicate(Source, Target) ->
    couch_replicator_test_helper:replicate(
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"worker_processes">>, <<"1">>}
        ]}
    ).

replicate(Source, Target, UseBulkGet) ->
    couch_replicator_test_helper:replicate(
        {[
            {<<"source">>, db_url(Source)},
            {<<"target">>, db_url(Target)},
            {<<"worker_processes">>, <<"1">>},
            {<<"use_bulk_get">>, UseBulkGet}
        ]}
    ).
