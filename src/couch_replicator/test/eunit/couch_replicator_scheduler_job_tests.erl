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

-module(couch_replicator_scheduler_job_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(CHANGES_READER, couch_replicator_changes_reader).
-define(DOC1, #{<<"_id">> => <<"doc1">>}).
-define(DOC2, #{<<"_id">> => <<"doc2">>}).
-define(DOC3, #{<<"_id">> => <<"doc3">>}).
-define(DOC4, #{<<"_id">> => <<"doc4">>}).
-define(DOCS, #{<<"docs">> => [?DOC1, ?DOC2, ?DOC3]}).
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
    meck:new(?CHANGES_READER, [passthrough]),
    {Ctx, {RepDb, Source, Target}}.

setup_prefixed_replicator_db() ->
    {Ctx, {Source, Target}} = couch_replicator_test_helper:test_setup(),
    RepDb = setup_replicator_db(?tempdb()),
    meck:new(?CHANGES_READER, [passthrough]),
    {Ctx, {RepDb, Source, Target}}.

teardown({Ctx, {RepDb, Source, Target}}) ->
    ok = fabric:delete_db(RepDb, [?ADMIN_CTX]),
    config:delete("replicator", "update_docs", _Persist = false),
    couch_replicator_test_helper:test_teardown({Ctx, {Source, Target}}).

scheduler_job_replicate_test() ->
    {
        foreach,
        fun setup_main_replicator_db/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_replicate_without_since_seq),
            ?TDEF_FE(t_replicate_with_since_seq)
        ]
    }.

t_replicate_without_since_seq({_Ctx, {_RepDb, Source, Target}}) ->
    ok = create_docs(Source),
    replicate(Source, Target),
    ?assertEqual(1, num_calls(read_changes, ['_', 0, '_', '_', '_'])),

    meck:reset(?CHANGES_READER),
    ok = create_doc(Source),
    replicate(Source, Target),
    Changes = changes(Source),
    Seq = sequence(?DOC3, Changes),
    ?assertEqual(1, num_calls(read_changes, ['_', Seq, '_', '_', '_'])).

t_replicate_with_since_seq({_Ctx, {_RepDb, Source, Target}}) ->
    ok = create_docs(Source),
    Changes = changes(Source),
    SinceSeq = sequence(?DOC2, Changes),
    replicate(Source, Target, SinceSeq),
    ?assertEqual(1, num_calls(read_changes, ['_', SinceSeq, '_', '_', '_'])),

    meck:reset(?CHANGES_READER),
    ok = create_doc(Source),
    replicate(Source, Target),
    Seq = sequence(?DOC3, Changes),
    ?assertEqual(1, num_calls(read_changes, ['_', Seq, '_', '_', '_'])).

%%%%%%%%%%%%%%%%%%%% Utility Functions %%%%%%%%%%%%%%%%%%%%
url(UrlPath) ->
    binary_to_list(couch_replicator_test_helper:cluster_db_url(UrlPath)).

create_docs(DbName) ->
    case req(post, url(DbName) ++ "/_bulk_docs", ?DOCS) of
        {201, _} -> ok;
        Error -> error({failed_to_create_docs, DbName, Error})
    end.

create_doc(DbName) ->
    case req(post, url(DbName), ?DOC4) of
        {201, _} -> ok;
        Error -> error({failed_to_create_doc, DbName, Error})
    end.

changes(DbName) ->
    {200, Res} = req(get, url(DbName) ++ "/_changes"),
    ?assert(maps:is_key(<<"last_seq">>, Res)),
    ?assert(maps:is_key(<<"pending">>, Res)),
    ?assert(maps:is_key(<<"results">>, Res)),
    Res.

sequence(Doc, Changes) ->
    #{<<"_id">> := DocId} = Doc,
    #{<<"results">> := Results} = Changes,
    case lists:search(fun(M) -> maps:get(<<"id">>, M) == DocId end, Results) of
        {value, #{<<"seq">> := Seq}} -> Seq;
        false -> not_found
    end.

replicate(RepObject) ->
    couch_replicator_test_helper:replicate(RepObject).

replicate(Source, Target) ->
    replicate(#{
        <<"source">> => ?l2b(url(Source)),
        <<"target">> => ?l2b(url(Target))
    }).

replicate(Source, Target, SinceSeq) ->
    ?debugVal(SinceSeq),
    replicate(#{
        <<"source">> => ?l2b(url(Source)),
        <<"target">> => ?l2b(url(Target)),
        <<"since_seq">> => SinceSeq
    }).

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

num_calls(Fun, Args) ->
    meck:num_calls(?CHANGES_READER, Fun, Args).
