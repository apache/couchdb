-module(couch_replicator_test_helper).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([compare_dbs/2, db_url/1, replicate/2]).

compare_dbs(Source, Target) ->
    {ok, SourceDb} = couch_db:open_int(Source, []),
    {ok, TargetDb} = couch_db:open_int(Target, []),

    Fun = fun(FullDocInfo, _, Acc) ->
        {ok, DocSource} = couch_db:open_doc(SourceDb, FullDocInfo),
        Id = DocSource#doc.id,

        {ok, DocTarget} = couch_db:open_doc(TargetDb, Id),
        ?assertEqual(DocSource#doc.body, DocTarget#doc.body),

        #doc{atts = SourceAtts} = DocSource,
        #doc{atts = TargetAtts} = DocTarget,
        ?assertEqual(lists:sort([couch_att:fetch(name, Att) || Att <- SourceAtts]),
                     lists:sort([couch_att:fetch(name, Att) || Att <- TargetAtts])),

        FunCompareAtts = fun(Att) ->
            AttName = couch_att:fetch(name, Att),
            {ok, AttTarget} = find_att(TargetAtts, AttName),
            SourceMd5 = att_md5(Att),
            TargetMd5 = att_md5(AttTarget),
            case AttName of
                <<"att1">> ->
                    ?assertEqual(gzip, couch_att:fetch(encoding, Att)),
                    ?assertEqual(gzip, couch_att:fetch(encoding, AttTarget)),
                    DecSourceMd5 = att_decoded_md5(Att),
                    DecTargetMd5 = att_decoded_md5(AttTarget),
                    ?assertEqual(DecSourceMd5, DecTargetMd5);
                _ ->
                    ?assertEqual(identity, couch_att:fetch(encoding, AttTarget)),
                    ?assertEqual(identity, couch_att:fetch(encoding, AttTarget))
            end,
            ?assertEqual(SourceMd5, TargetMd5),
            ?assert(is_integer(couch_att:fetch(disk_len, Att))),
            ?assert(is_integer(couch_att:fetch(att_len, Att))),
            ?assert(is_integer(couch_att:fetch(disk_len, AttTarget))),
            ?assert(is_integer(couch_att:fetch(att_len, AttTarget))),
            ?assertEqual(couch_att:fetch(disk_len, Att),
                         couch_att:fetch(disk_len, AttTarget)),
            ?assertEqual(couch_att:fetch(att_len, Att),
                         couch_att:fetch(att_len, AttTarget)),
            ?assertEqual(couch_att:fetch(type, Att),
                         couch_att:fetch(type, AttTarget)),
            ?assertEqual(couch_att:fetch(md5, Att),
                         couch_att:fetch(md5, AttTarget))
        end,

        lists:foreach(FunCompareAtts, SourceAtts),

        {ok, Acc}
    end,

    {ok, _, _} = couch_db:enum_docs(SourceDb, Fun, [], []),
    ok = couch_db:close(SourceDb),
    ok = couch_db:close(TargetDb).

find_att([], _Name) ->
    nil;
find_att([Att | Rest], Name) ->
    case couch_att:fetch(name, Att) of
        Name ->
            {ok, Att};
        _ ->
            find_att(Rest, Name)
    end.


att_md5(Att) ->
    Md50 = couch_att:foldl(
        Att,
        fun(Chunk, Acc) -> couch_util:md5_update(Acc, Chunk) end,
        couch_util:md5_init()),
    couch_util:md5_final(Md50).

att_decoded_md5(Att) ->
    Md50 = couch_att:foldl_decode(
        Att,
        fun(Chunk, Acc) -> couch_util:md5_update(Acc, Chunk) end,
        couch_util:md5_init()),
    couch_util:md5_final(Md50).

db_url(DbName) ->
    iolist_to_binary([
        "http://", config:get("httpd", "bind_address", "127.0.0.1"),
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
