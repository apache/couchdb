#!/usr/bin/env escript
%% -*- erlang -*-
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

% Test replication of documents with many leaf revisions.
% Motivated by COUCHDB-1340 and other similar issues where a document
% GET with a too long ?open_revs revision list doesn't work due to
% maximum web server limits for the HTTP request path.

-record(user_ctx, {
    name = null,
    roles = [],
    handler
}).

-record(doc, {
    id = <<"">>,
    revs = {0, []},
    body = {[]},
    atts = [],
    deleted = false,
    meta = []
}).

-define(b2l(B), binary_to_list(B)).
-define(l2b(L), list_to_binary(L)).
-define(i2l(I), integer_to_list(I)).


source_db_name() -> <<"couch_test_rep_db_a">>.
target_db_name() -> <<"couch_test_rep_db_b">>.

doc_ids() ->
    [<<"doc1">>, <<"doc2">>, <<"doc3">>].

doc_num_conflicts(<<"doc1">>) -> 100;
doc_num_conflicts(<<"doc2">>) -> 200;
doc_num_conflicts(<<"doc3">>) -> 550.


main(_) ->
    test_util:init_code_path(),

    etap:plan(16),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.


test() ->
    couch_server_sup:start_link(test_util:config_files()),
    ibrowse:start(),
    crypto:start(),

    Pairs = [
        {source_db_name(), target_db_name()},
        {{remote, source_db_name()}, target_db_name()},
        {source_db_name(), {remote, target_db_name()}},
        {{remote, source_db_name()}, {remote, (target_db_name())}}
    ],

    {ok, SourceDb} = create_db(source_db_name()),
    etap:diag("Populating source database"),
    {ok, DocRevs} = populate_db(SourceDb),
    ok = couch_db:close(SourceDb),

    lists:foreach(
        fun({Source, Target}) ->
            etap:diag("Creating target database"),
            {ok, TargetDb} = create_db(target_db_name()),

            ok = couch_db:close(TargetDb),
            etap:diag("Triggering replication"),
            replicate(Source, Target),
            etap:diag("Replication finished, comparing source and target databases"),
            {ok, TargetDb2} = couch_db:open_int(target_db_name(), []),
            verify_target(TargetDb2, DocRevs),
            ok = couch_db:close(TargetDb2),

            etap:diag("Deleting target database"),
            delete_db(TargetDb),
            ok = timer:sleep(1000)
        end,
        Pairs),

    delete_db(SourceDb),
    couch_server_sup:stop(),
    ok.


populate_db(Db) ->
    DocRevsDict = lists:foldl(
        fun(DocId, Acc) ->
            Value = <<"0">>,
            Doc = #doc{
                id = DocId,
                body = {[ {<<"value">>, Value} ]}
            },
            {ok, Rev} = couch_db:update_doc(Db, Doc, []),
            {ok, RevsDict} = add_doc_siblings(Db, DocId, doc_num_conflicts(DocId)),
            RevsDict2 = dict:store(Rev, Value, RevsDict),
            dict:store(DocId, RevsDict2, Acc)
        end,
        dict:new(), doc_ids()),
    {ok, dict:to_list(DocRevsDict)}.


add_doc_siblings(Db, DocId, NumLeaves) when NumLeaves > 0 ->
    add_doc_siblings(Db, DocId, NumLeaves, [], dict:new()).


add_doc_siblings(Db, _DocId, 0, AccDocs, RevsDict) ->
    {ok, []} = couch_db:update_docs(Db, AccDocs, [], replicated_changes),
    {ok, RevsDict};

add_doc_siblings(Db, DocId, NumLeaves, AccDocs, RevsDict) ->
    Value = list_to_binary(integer_to_list(NumLeaves)),
    Rev = couch_util:md5(Value),
    RevsDict2 = dict:store({1, Rev}, Value, RevsDict),
    Doc = #doc{
        id = DocId,
        revs = {1, [Rev]},
        body = {[ {<<"value">>, Value} ]}
    },
    add_doc_siblings(Db, DocId, NumLeaves - 1, [Doc | AccDocs], RevsDict2).


verify_target(_TargetDb, []) ->
    ok;

verify_target(TargetDb, [{DocId, RevsDict} | Rest]) ->
    {ok, Lookups} = couch_db:open_doc_revs(
        TargetDb,
        DocId,
        [R || {R, _} <- dict:to_list(RevsDict)],
        [ejson_body]),
    Docs = [Doc || {ok, Doc} <- Lookups],
    Total = doc_num_conflicts(DocId) + 1,
    etap:is(
        length(Docs),
        Total,
        "Target has " ++ ?i2l(Total) ++ " leaf revisions of document " ++ ?b2l(DocId)),
    etap:diag("Verifying all revisions of document " ++ ?b2l(DocId)),
    lists:foreach(
        fun(#doc{revs = {Pos, [RevId]}, body = {Body}}) ->
            Rev = {Pos, RevId},
            {ok, Value} = dict:find(Rev, RevsDict),
            case couch_util:get_value(<<"value">>, Body) of
            Value ->
                ok;
            Other ->
                etap:bail("Wrong value for revision " ++
                    ?b2l(couch_doc:rev_to_str(Rev)) ++ " of document " ++
                    ?b2l(DocId) ++ ". Expected `" ++ couch_util:to_list(Value) ++
                    "`, got `" ++ couch_util:to_list(Other) ++ "`")
            end
        end,
        Docs),
    verify_target(TargetDb, Rest).


db_url(DbName) ->
    iolist_to_binary([
        "http://", couch_config:get("httpd", "bind_address", "127.0.0.1"),
        ":", integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
        "/", DbName
    ]).


create_db(DbName) ->
    couch_db:create(
        DbName,
        [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}, overwrite]).


delete_db(Db) ->
    ok = couch_server:delete(
        couch_db:name(Db), [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}]).


replicate({remote, Db}, Target) ->
    replicate(db_url(Db), Target);

replicate(Source, {remote, Db}) ->
    replicate(Source, db_url(Db));

replicate(Source, Target) ->
    RepObject = {[
        {<<"source">>, Source},
        {<<"target">>, Target}
    ]},
    {ok, Rep} = couch_replicator_utils:parse_rep_doc(
        RepObject, #user_ctx{roles = [<<"_admin">>]}),
    {ok, Pid} = couch_replicator:async_replicate(Rep),
    MonRef = erlang:monitor(process, Pid),
    receive
    {'DOWN', MonRef, process, Pid, Reason} ->
        etap:is(Reason, normal, "Replication finished successfully")
    after 300000 ->
        etap:bail("Timeout waiting for replication to finish")
    end.
