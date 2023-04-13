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

-module(couch_replicator_scheduler_docs_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(JSON, {"Content-Type", "application/json"}).

setup_replicator_db(Prefix) ->
    RepDb =
        case Prefix of
            <<>> -> <<"_replicator">>;
            <<_/binary>> -> <<Prefix/binary, "/_replicator">>
        end,
    Opts = [{q, 1}, {n, 1}, ?ADMIN_CTX],
    case fabric:create_db(RepDb, Opts) of
        ok -> ok;
        {error, file_exists} -> ok
    end,
    RepDb.

setup_main_replicator_db() ->
    {Ctx, {Source, Target}} = couch_replicator_test_helper:test_setup(),
    RepDb = setup_replicator_db(<<>>),
    {Ctx, {RepDb, Source, Target}}.

setup_prefixed_replicator_db() ->
    {Ctx, {Source, Target}} = couch_replicator_test_helper:test_setup(),
    RepDb = setup_replicator_db(?tempdb()),
    {Ctx, {RepDb, Source, Target}}.

teardown({Ctx, {RepDb, Source, Target}}) ->
    ok = fabric:delete_db(RepDb, [?ADMIN_CTX]),
    couch_replicator_test_helper:test_teardown({Ctx, {Source, Target}}).

scheduler_docs_test_main_db_test_() ->
    {
        foreach,
        fun setup_main_replicator_db/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_scheduler_docs_total_rows, 10)
        ]
    }.

scheduler_docs_test_prefixed_db_test_() ->
    {
        foreach,
        fun setup_prefixed_replicator_db/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_scheduler_docs_total_rows, 10)
        ]
    }.

replicator_bdu_test_main_db_test_() ->
    {
        setup,
        fun setup_prefixed_replicator_db/0,
        fun teardown/1,
        with([
            ?TDEF(t_local_docs_can_be_written),
            ?TDEF(t_design_docs_can_be_written),
            ?TDEF(t_malformed_docs_are_rejected)
        ])
    }.

replicator_bdu_test_prefixed_db_test_() ->
    {
        setup,
        fun setup_prefixed_replicator_db/0,
        fun teardown/1,
        with([
            ?TDEF(t_local_docs_can_be_written),
            ?TDEF(t_design_docs_can_be_written),
            ?TDEF(t_malformed_docs_are_rejected)
        ])
    }.

t_scheduler_docs_total_rows({_Ctx, {RepDb, Source, Target}}) ->
    SourceUrl = couch_replicator_test_helper:cluster_db_url(Source),
    TargetUrl = couch_replicator_test_helper:cluster_db_url(Target),
    RepDoc = #{<<"source">> => SourceUrl, <<"target">> => TargetUrl},
    RepDocUrl = rep_doc_url(RepDb, ?docid()),
    {201, _} = req(put, RepDocUrl, RepDoc),
    SchedulerDocsUrl =
        case RepDb of
            <<"_replicator">> -> url(<<"/_scheduler/docs">>);
            <<_/binary>> -> url(<<"/_scheduler/docs/", RepDb/binary>>)
        end,
    Body = test_util:wait(
        fun() ->
            case req(get, SchedulerDocsUrl) of
                {200, #{<<"docs">> := [_ | _]} = Decoded} -> Decoded;
                {_, #{}} -> wait
            end
        end,
        10000,
        1000
    ),
    Docs = maps:get(<<"docs">>, Body),
    TotalRows = maps:get(<<"total_rows">>, Body),
    ?assertEqual(TotalRows, length(Docs)),
    ok.

t_local_docs_can_be_written({_Ctx, {RepDb, _, _}}) ->
    DocUrl1 = rep_doc_url(RepDb, <<"_local/doc1">>),
    ?assertMatch({201, _}, req(put, DocUrl1, #{})),
    DocUrl2 = rep_doc_url(RepDb, <<"_local/doc2">>),
    ?assertMatch({201, _}, req(put, DocUrl2, #{<<"foo">> => <<"bar">>})).

t_design_docs_can_be_written({_Ctx, {RepDb, _, _}}) ->
    DocUrl1 = rep_doc_url(RepDb, <<"_design/ddoc1">>),
    ?assertMatch({201, _}, req(put, DocUrl1, #{})),
    DocUrl2 = rep_doc_url(RepDb, <<"_design/ddoc2">>),
    ?assertMatch({201, _}, req(put, DocUrl2, #{<<"foo">> => <<"bar">>})).

t_malformed_docs_are_rejected({_Ctx, {RepDb, _, _}}) ->
    % couch_replicator_parse holds most of the BDU validation logic
    % Here we just test that the BDU works with a few basic cases
    DocUrl1 = rep_doc_url(RepDb, <<"rep1">>),
    ?assertMatch({403, _}, req(put, DocUrl1, #{})),
    DocUrl2 = rep_doc_url(RepDb, <<"rep2">>),
    ?assertMatch({403, _}, req(put, DocUrl2, #{<<"foo">> => <<"bar">>})).

rep_doc_url(RepDb, DocId) when is_binary(RepDb) ->
    rep_doc_url(binary_to_list(RepDb), DocId);
rep_doc_url(RepDb, DocId) when is_binary(DocId) ->
    rep_doc_url(RepDb, binary_to_list(DocId));
rep_doc_url(RepDb, DocId) when is_list(RepDb), is_list(DocId) ->
    UrlQuotedRepDb = mochiweb_util:quote_plus(RepDb),
    url(UrlQuotedRepDb ++ "/" ++ DocId).

url(UrlPath) ->
    binary_to_list(couch_replicator_test_helper:cluster_db_url(UrlPath)).

req(Method, Url) ->
    Headers = [?JSON],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers),
    {Code, jiffy:decode(Res, [return_maps])}.

req(Method, Url, #{} = Body) ->
    req(Method, Url, jiffy:encode(Body));
req(Method, Url, Body) ->
    Headers = [?JSON],
    {ok, Code, _, Res} = test_request:request(Method, Url, Headers, Body),
    {Code, jiffy:decode(Res, [return_maps])}.
