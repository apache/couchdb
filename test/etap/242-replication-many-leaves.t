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

-record(att, {
    name,
    type,
    att_len,
    disk_len,
    md5= <<>>,
    revpos=0,
    data,
    encoding=identity
}).

-define(b2l(B), binary_to_list(B)).
-define(l2b(L), list_to_binary(L)).
-define(i2l(I), integer_to_list(I)).


source_db_name() -> <<"couch_test_rep_db_a">>.
target_db_name() -> <<"couch_test_rep_db_b">>.

doc_ids() ->
    [<<"doc1">>, <<"doc2">>, <<"doc3">>].

doc_num_conflicts(<<"doc1">>) -> 10;
doc_num_conflicts(<<"doc2">>) -> 100;
% a number > MaxURLlength (7000) / length(DocRevisionString)
doc_num_conflicts(<<"doc3">>) -> 210.


main(_) ->
    test_util:init_code_path(),

    etap:plan(56),
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
    couch_config:set("replicator", "connection_timeout", "90000", false),

    Pairs = [
        {source_db_name(), target_db_name()},
        {{remote, source_db_name()}, target_db_name()},
        {source_db_name(), {remote, target_db_name()}},
        {{remote, source_db_name()}, {remote, (target_db_name())}}
    ],

    lists:foreach(
        fun({Source, Target}) ->
            {ok, SourceDb} = create_db(source_db_name()),
            etap:diag("Populating source database"),
            {ok, DocRevs} = populate_db(SourceDb),
            ok = couch_db:close(SourceDb),
            etap:diag("Creating target database"),
            {ok, TargetDb} = create_db(target_db_name()),

            ok = couch_db:close(TargetDb),
            etap:diag("Triggering replication"),
            replicate(Source, Target),
            etap:diag("Replication finished, comparing source and target databases"),
            {ok, SourceDb2} = couch_db:open_int(source_db_name(), []),
            {ok, TargetDb2} = couch_db:open_int(target_db_name(), []),
            verify_target(SourceDb2, TargetDb2, DocRevs),
            ok = couch_db:close(SourceDb2),
            ok = couch_db:close(TargetDb2),

            {ok, SourceDb3} = couch_db:open_int(source_db_name(), []),
            {ok, DocRevs2} = add_attachments(SourceDb3, DocRevs, 2),
            ok = couch_db:close(SourceDb3),
            etap:diag("Triggering replication again"),
            replicate(Source, Target),
            etap:diag("Replication finished, comparing source and target databases"),
            {ok, SourceDb4} = couch_db:open_int(source_db_name(), []),
            {ok, TargetDb4} = couch_db:open_int(target_db_name(), []),
            verify_target(SourceDb4, TargetDb4, DocRevs2),
            ok = couch_db:close(SourceDb4),
            ok = couch_db:close(TargetDb4),

            etap:diag("Deleting source and target databases"),
            delete_db(TargetDb),
            delete_db(SourceDb),
            ok = timer:sleep(1000)
        end,
        Pairs),

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
            {ok, DocRevs} = add_doc_siblings(Db, DocId, doc_num_conflicts(DocId)),
            dict:store(DocId, [Rev | DocRevs], Acc)
        end,
        dict:new(), doc_ids()),
    {ok, dict:to_list(DocRevsDict)}.


add_doc_siblings(Db, DocId, NumLeaves) when NumLeaves > 0 ->
    add_doc_siblings(Db, DocId, NumLeaves, [], []).


add_doc_siblings(Db, _DocId, 0, AccDocs, AccRevs) ->
    {ok, []} = couch_db:update_docs(Db, AccDocs, [], replicated_changes),
    {ok, AccRevs};

add_doc_siblings(Db, DocId, NumLeaves, AccDocs, AccRevs) ->
    Value = list_to_binary(integer_to_list(NumLeaves)),
    Rev = couch_util:md5(Value),
    Doc = #doc{
        id = DocId,
        revs = {1, [Rev]},
        body = {[ {<<"value">>, Value} ]}
    },
    add_doc_siblings(Db, DocId, NumLeaves - 1, [Doc | AccDocs], [{1, Rev} | AccRevs]).


verify_target(_SourceDb, _TargetDb, []) ->
    ok;

verify_target(SourceDb, TargetDb, [{DocId, RevList} | Rest]) ->
    {ok, Lookups} = couch_db:open_doc_revs(
        TargetDb,
        DocId,
        RevList,
        [conflicts, deleted_conflicts]),
    Docs = [Doc || {ok, Doc} <- Lookups],
    {ok, SourceLookups} = couch_db:open_doc_revs(
        SourceDb,
        DocId,
        RevList,
        [conflicts, deleted_conflicts]),
    SourceDocs = [Doc || {ok, Doc} <- SourceLookups],
    Total = doc_num_conflicts(DocId) + 1,
    etap:is(
        length(Docs),
        Total,
        "Target has " ++ ?i2l(Total) ++ " leaf revisions of document " ++ ?b2l(DocId)),
    etap:diag("Verifying all revisions of document " ++ ?b2l(DocId)),
    lists:foreach(
        fun({#doc{id = Id, revs = Revs} = TgtDoc, #doc{id = Id, revs = Revs} = SrcDoc}) ->
            SourceJson = couch_doc:to_json_obj(SrcDoc, [attachments]),
            TargetJson = couch_doc:to_json_obj(TgtDoc, [attachments]),
            case TargetJson of
            SourceJson ->
                ok;
            _ ->
                {Pos, [Rev | _]} = Revs,
                etap:bail("Wrong value for revision " ++
                    ?b2l(couch_doc:rev_to_str({Pos, Rev})) ++
                    " of document " ++ ?b2l(DocId))
            end
        end,
        lists:zip(Docs, SourceDocs)),
    verify_target(SourceDb, TargetDb, Rest).


add_attachments(Source, DocIdRevs, NumAtts) ->
    add_attachments(Source, DocIdRevs, NumAtts, []).

add_attachments(_SourceDb, [], _NumAtts, Acc) ->
    {ok, Acc};

add_attachments(SourceDb, [{DocId, RevList} | Rest], NumAtts, IdRevsAcc) ->
    {ok, SourceLookups} = couch_db:open_doc_revs(
        SourceDb,
        DocId,
        RevList,
        []),
    SourceDocs = [Doc || {ok, Doc} <- SourceLookups],
    Total = doc_num_conflicts(DocId) + 1,
    etap:is(
        length(SourceDocs),
        Total,
        "Source still has " ++ ?i2l(Total) ++
            " leaf revisions of document " ++ ?b2l(DocId)),
    etap:diag("Adding " ++ ?i2l(NumAtts) ++
        " attachments to each revision of the document " ++ ?b2l(DocId)),
    NewDocs = lists:foldl(
        fun(#doc{atts = Atts, revs = {Pos, [Rev | _]}} = Doc, Acc) ->
            NewAtts = lists:foldl(
                fun(I, AttAcc) ->
                    AttData = crypto:rand_bytes(100),
                    NewAtt = #att{
                        name = iolist_to_binary(
                            ["att_", ?i2l(I), "_", couch_doc:rev_to_str({Pos, Rev})]),
                        type = <<"application/foobar">>,
                        att_len = byte_size(AttData),
                        data = AttData
                    },
                    [NewAtt | AttAcc]
                end,
                [], lists:seq(1, NumAtts)),
            [Doc#doc{atts = Atts ++ NewAtts} | Acc]
        end,
        [], SourceDocs),
    {ok, UpdateResults} = couch_db:update_docs(SourceDb, NewDocs, []),
    NewRevs = [R || {ok, R} <- UpdateResults],
    etap:is(
        length(NewRevs),
        length(NewDocs),
        "Document revisions updated with " ++ ?i2l(NumAtts) ++ " attachments"),
    add_attachments(SourceDb, Rest, NumAtts, [{DocId, NewRevs} | IdRevsAcc]).


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
    after 900000 ->
        etap:bail("Timeout waiting for replication to finish")
    end.
