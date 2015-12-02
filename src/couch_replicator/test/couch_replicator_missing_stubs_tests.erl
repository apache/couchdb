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

-module(couch_replicator_missing_stubs_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(ADMIN_ROLE, #user_ctx{roles=[<<"_admin">>]}).
-define(ADMIN_USER, {user_ctx, ?ADMIN_ROLE}).
-define(REVS_LIMIT, 3).
-define(TIMEOUT_STOP, 1000).
-define(TIMEOUT_EUNIT, 30).


setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_USER]),
    ok = couch_db:close(Db),
    DbName.

setup(local) ->
    setup();
setup(remote) ->
    {remote, setup()};
setup({A, B}) ->
    {ok, _} = couch_server_sup:start_link(?CONFIG_CHAIN),
    Source = setup(A),
    Target = setup(B),
    {Source, Target}.

teardown({remote, DbName}) ->
    teardown(DbName);
teardown(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_USER]),
    ok.

teardown(_, {Source, Target}) ->
    teardown(Source),
    teardown(Target),

    Pid = whereis(couch_server_sup),
    erlang:monitor(process, Pid),
    couch_server_sup:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT_STOP ->
        throw({timeout, server_stop})
    end.


missing_stubs_test_() ->
    Pairs = [{local, local}, {local, remote},
             {remote, local}, {remote, remote}],
    {
        "Replicate docs with missing stubs (COUCHDB-1365)",
        {
            foreachx,
            fun setup/1, fun teardown/2,
            [{Pair, fun should_replicate_docs_with_missed_att_stubs/2}
             || Pair <- Pairs]
        }
    }.


should_replicate_docs_with_missed_att_stubs({From, To}, {Source, Target}) ->
    {lists:flatten(io_lib:format("~p -> ~p", [From, To])),
     {inorder, [
        should_populate_source(Source),
        should_set_target_revs_limit(Target, ?REVS_LIMIT),
        should_replicate(Source, Target),
        should_compare_databases(Source, Target),
        should_update_source_docs(Source, ?REVS_LIMIT * 2),
        should_replicate(Source, Target),
        should_compare_databases(Source, Target)
     ]}}.

should_populate_source({remote, Source}) ->
    should_populate_source(Source);
should_populate_source(Source) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(populate_db(Source))}.

should_replicate({remote, Source}, Target) ->
    should_replicate(db_url(Source), Target);
should_replicate(Source, {remote, Target}) ->
    should_replicate(Source, db_url(Target));
should_replicate(Source, Target) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(replicate(Source, Target))}.

should_set_target_revs_limit({remote, Target}, RevsLimit) ->
    should_set_target_revs_limit(Target, RevsLimit);
should_set_target_revs_limit(Target, RevsLimit) ->
    ?_test(begin
        {ok, Db} = couch_db:open_int(Target, [?ADMIN_USER]),
        ?assertEqual(ok, couch_db:set_revs_limit(Db, RevsLimit)),
        ok = couch_db:close(Db)
    end).

should_compare_databases({remote, Source}, Target) ->
    should_compare_databases(Source, Target);
should_compare_databases(Source, {remote, Target}) ->
    should_compare_databases(Source, Target);
should_compare_databases(Source, Target) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(compare_dbs(Source, Target))}.

should_update_source_docs({remote, Source}, Times) ->
    should_update_source_docs(Source, Times);
should_update_source_docs(Source, Times) ->
    {timeout, ?TIMEOUT_EUNIT, ?_test(update_db_docs(Source, Times))}.


populate_db(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    AttData = crypto:rand_bytes(6000),
    Doc = #doc{
        id = <<"doc1">>,
        atts = [
            #att{
                name = <<"doc1_att1">>,
                type = <<"application/foobar">>,
                att_len = byte_size(AttData),
                data = AttData
            }
        ]
    },
    {ok, _} = couch_db:update_doc(Db, Doc, []),
    couch_db:close(Db).

update_db_docs(DbName, Times) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, _, _} = couch_db:enum_docs(
        Db,
        fun(FDI, _, Acc) -> db_fold_fun(FDI, Acc) end,
        {DbName, Times},
        []),
    ok = couch_db:close(Db).

db_fold_fun(FullDocInfo, {DbName, Times}) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, Doc} = couch_db:open_doc(Db, FullDocInfo),
    lists:foldl(
        fun(_, {Pos, RevId}) ->
            {ok, Db2} = couch_db:reopen(Db),
            NewDocVersion = Doc#doc{
                revs = {Pos, [RevId]},
                body = {[{<<"value">>, base64:encode(crypto:rand_bytes(100))}]}
            },
            {ok, NewRev} = couch_db:update_doc(Db2, NewDocVersion, []),
            NewRev
        end,
        {element(1, Doc#doc.revs), hd(element(2, Doc#doc.revs))},
        lists:seq(1, Times)),
    ok = couch_db:close(Db),
    {ok, {DbName, Times}}.

compare_dbs(Source, Target) ->
    {ok, SourceDb} = couch_db:open_int(Source, []),
    {ok, TargetDb} = couch_db:open_int(Target, []),

    Fun = fun(FullDocInfo, _, Acc) ->
        {ok, DocSource} = couch_db:open_doc(SourceDb, FullDocInfo,
                                            [conflicts, deleted_conflicts]),
        Id = DocSource#doc.id,

        {ok, DocTarget} = couch_db:open_doc(TargetDb, Id,
                                            [conflicts, deleted_conflicts]),
        ?assertEqual(DocSource#doc.body, DocTarget#doc.body),

        ?assertEqual(couch_doc:to_json_obj(DocSource, []),
                     couch_doc:to_json_obj(DocTarget, [])),

        #doc{atts = SourceAtts} = DocSource,
        #doc{atts = TargetAtts} = DocTarget,
        ?assertEqual(lists:sort([N || #att{name = N} <- SourceAtts]),
                     lists:sort([N || #att{name = N} <- TargetAtts])),

        lists:foreach(
            fun(#att{name = AttName} = Att) ->
                {ok, AttTarget} = find_att(TargetAtts, AttName),
                SourceMd5 = att_md5(Att),
                TargetMd5 = att_md5(AttTarget),
                case AttName of
                    <<"att1">> ->
                        ?assertEqual(gzip, Att#att.encoding),
                        ?assertEqual(gzip, AttTarget#att.encoding),
                        DecSourceMd5 = att_decoded_md5(Att),
                        DecTargetMd5 = att_decoded_md5(AttTarget),
                        ?assertEqual(DecSourceMd5, DecTargetMd5);
                    _ ->
                        ?assertEqual(identity, Att#att.encoding),
                        ?assertEqual(identity, AttTarget#att.encoding)
                end,
                ?assertEqual(SourceMd5, TargetMd5),
                ?assert(is_integer(Att#att.disk_len)),
                ?assert(is_integer(Att#att.att_len)),
                ?assert(is_integer(AttTarget#att.disk_len)),
                ?assert(is_integer(AttTarget#att.att_len)),
                ?assertEqual(Att#att.disk_len, AttTarget#att.disk_len),
                ?assertEqual(Att#att.att_len, AttTarget#att.att_len),
                ?assertEqual(Att#att.type, AttTarget#att.type),
                ?assertEqual(Att#att.md5, AttTarget#att.md5)
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

replicate(Source, Target) ->
    RepObject = {[
        {<<"source">>, Source},
        {<<"target">>, Target}
    ]},
    {ok, Rep} = couch_replicator_utils:parse_rep_doc(RepObject, ?ADMIN_ROLE),
    {ok, Pid} = couch_replicator:async_replicate(Rep),
    MonRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MonRef, process, Pid, _} ->
            ok
    end.
