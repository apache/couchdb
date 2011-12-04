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

% Test replication of large attachments. Verify that both source and
% target have the same attachment data and metadata.

-define(b2l(Bin), binary_to_list(Bin)).

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


source_db_name() -> <<"couch_test_rep_db_a">>.
target_db_name() -> <<"couch_test_rep_db_b">>.


main(_) ->
    test_util:init_code_path(),

    etap:plan(1192),
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
    couch_config:set("attachments", "compressible_types", "text/*", false),

    Pairs = [
        {source_db_name(), target_db_name()},
        {{remote, source_db_name()}, target_db_name()},
        {source_db_name(), {remote, target_db_name()}},
        {{remote, source_db_name()}, {remote, (target_db_name())}}
    ],

    {ok, SourceDb} = create_db(source_db_name()),
    etap:diag("Populating source database"),
    populate_db(SourceDb, 11),
    ok = couch_db:close(SourceDb),

    lists:foreach(
        fun({Source, Target}) ->
            etap:diag("Creating target database"),
            {ok, TargetDb} = create_db(target_db_name()),

            ok = couch_db:close(TargetDb),
            etap:diag("Triggering replication"),
            replicate(Source, Target),
            etap:diag("Replication finished, comparing source and target databases"),
            compare_dbs(SourceDb, TargetDb),

            etap:diag("Deleting target database"),
            delete_db(TargetDb),
            ok = timer:sleep(1000)
        end,
        Pairs),

    delete_db(SourceDb),
    couch_server_sup:stop(),
    ok.


populate_db(Db, DocCount) ->
    Docs = lists:foldl(
        fun(DocIdCounter, Acc) ->
            Doc = #doc{
                id = iolist_to_binary(["doc", integer_to_list(DocIdCounter)]),
                body = {[]},
                atts = [
                    att(<<"att1">>, 2 * 1024 * 1024, <<"text/plain">>),
                    att(<<"att2">>, round(6.6 * 1024 * 1024), <<"app/binary">>)
                ]
            },
            [Doc | Acc]
        end,
        [], lists:seq(1, DocCount)),
    {ok, _} = couch_db:update_docs(Db, Docs, []).


att(Name, Size, Type) ->
    #att{
        name = Name,
        type = Type,
        att_len = Size,
        data = fun(Count) -> crypto:rand_bytes(Count) end
    }.


compare_dbs(Source, Target) ->
    {ok, SourceDb} = couch_db:open_int(couch_db:name(Source), []),
    {ok, TargetDb} = couch_db:open_int(couch_db:name(Target), []),

    Fun = fun(FullDocInfo, _, Acc) ->
        {ok, DocSource} = couch_db:open_doc(SourceDb, FullDocInfo),
        Id = DocSource#doc.id,

        etap:diag("Verifying document " ++ ?b2l(Id)),

        {ok, DocTarget} = couch_db:open_doc(TargetDb, Id),
        etap:is(DocTarget#doc.body, DocSource#doc.body,
            "Same body in source and target databases"),

        #doc{atts = SourceAtts} = DocSource,
        #doc{atts = TargetAtts} = DocTarget,
        etap:is(
            lists:sort([N || #att{name = N} <- SourceAtts]),
            lists:sort([N || #att{name = N} <- TargetAtts]),
            "Document has same number (and names) of attachments in "
            "source and target databases"),

        lists:foreach(
            fun(#att{name = AttName} = Att) ->
                etap:diag("Verifying attachment " ++ ?b2l(AttName)),

                {ok, AttTarget} = find_att(TargetAtts, AttName),
                SourceMd5 = att_md5(Att),
                TargetMd5 = att_md5(AttTarget),
                case AttName of
                <<"att1">> ->
                    etap:is(Att#att.encoding, gzip,
                        "Attachment is gzip encoded in source database"),
                    etap:is(AttTarget#att.encoding, gzip,
                        "Attachment is gzip encoded in target database"),
                    DecSourceMd5 = att_decoded_md5(Att),
                    DecTargetMd5 = att_decoded_md5(AttTarget),
                    etap:is(DecTargetMd5, DecSourceMd5,
                        "Same identity content in source and target databases");
                _ ->
                    etap:is(Att#att.encoding, identity,
                        "Attachment is not encoded in source database"),
                    etap:is(AttTarget#att.encoding, identity,
                        "Attachment is not encoded in target database")
                end,
                etap:is(TargetMd5, SourceMd5,
                    "Same content in source and target databases"),
                etap:is(is_integer(Att#att.disk_len), true,
                    "#att.disk_len is an integer in source database"),
                etap:is(is_integer(Att#att.att_len), true,
                    "#att.att_len is an integer in source database"),
                etap:is(is_integer(AttTarget#att.disk_len), true,
                    "#att.disk_len is an integer in target database"),
                etap:is(is_integer(AttTarget#att.att_len), true,
                    "#att.att_len is an integer in target database"),
                etap:is(Att#att.disk_len, AttTarget#att.disk_len,
                    "Same identity length in source and target databases"),
                etap:is(Att#att.att_len, AttTarget#att.att_len,
                    "Same encoded length in source and target databases"),
                etap:is(Att#att.type, AttTarget#att.type,
                    "Same type in source and target databases"),
                etap:is(Att#att.md5, SourceMd5, "Correct MD5 in source database"),
                etap:is(AttTarget#att.md5, SourceMd5, "Correct MD5 in target database")
            end,
            SourceAtts),

        {ok, Acc}
    end,

    {ok, _, _} = couch_db:enum_docs(SourceDb, Fun, [], []),
    ok = couch_db:close(SourceDb),
    ok = couch_db:close(TargetDb).


find_att([], _Name) ->
    nil;
find_att([#att{name = Name} = Att | _], Name) ->
    {ok, Att};
find_att([_ | Rest], Name) ->
    find_att(Rest, Name).


att_md5(Att) ->
    Md50 = couch_doc:att_foldl(
        Att,
        fun(Chunk, Acc) -> couch_util:md5_update(Acc, Chunk) end,
        couch_util:md5_init()),
    couch_util:md5_final(Md50).

att_decoded_md5(Att) ->
    Md50 = couch_doc:att_foldl_decode(
        Att,
        fun(Chunk, Acc) -> couch_util:md5_update(Acc, Chunk) end,
        couch_util:md5_init()),
    couch_util:md5_final(Md50).


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
