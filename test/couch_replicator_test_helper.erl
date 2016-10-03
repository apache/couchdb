-module(couch_replicator_test_helper).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-export([compare_dbs/2, compare_dbs/3, db_url/1, replicate/1, replicate/2]).


compare_dbs(Source, Target) ->
    compare_dbs(Source, Target, []).


compare_dbs(Source, Target, ExceptIds) ->
    {ok, SourceDb} = couch_db:open_int(Source, []),
    {ok, TargetDb} = couch_db:open_int(Target, []),

    Fun = fun(FullDocInfo, _, Acc) ->
        {ok, DocSource} = couch_db:open_doc(SourceDb, FullDocInfo),
        Id = DocSource#doc.id,
        case lists:member(Id, ExceptIds) of
            true ->
                ?assertEqual(not_found, couch_db:get_doc_info(TargetDb, Id));
            false ->
                {ok, TDoc} = couch_db:open_doc(TargetDb, Id),
                compare_docs(DocSource, TDoc)
        end,
        {ok, Acc}
    end,

    {ok, _, _} = couch_db:enum_docs(SourceDb, Fun, [], []),
    ok = couch_db:close(SourceDb),
    ok = couch_db:close(TargetDb).


compare_docs(Doc1, Doc2) ->
    ?assertEqual(Doc1#doc.body, Doc2#doc.body),
    #doc{atts = Atts1} = Doc1,
    #doc{atts = Atts2} = Doc2,
    ?assertEqual(lists:sort([couch_att:fetch(name, Att) || Att <- Atts1]),
                 lists:sort([couch_att:fetch(name, Att) || Att <- Atts2])),
    FunCompareAtts = fun(Att) ->
        AttName = couch_att:fetch(name, Att),
        {ok, AttTarget} = find_att(Atts2, AttName),
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
    lists:foreach(FunCompareAtts, Atts1).


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
        fun(Chunk, Acc) -> couch_crypto:hash_update(md5, Acc, Chunk) end,
        couch_crypto:hash_init(md5)),
    couch_crypto:hash_final(md5, Md50).

att_decoded_md5(Att) ->
    Md50 = couch_att:foldl_decode(
        Att,
        fun(Chunk, Acc) -> couch_crypto:hash_update(md5, Acc, Chunk) end,
        couch_crypto:hash_init(md5)),
    couch_crypto:hash_final(md5, Md50).

db_url(DbName) ->
    iolist_to_binary([
        "http://", config:get("httpd", "bind_address", "127.0.0.1"),
        ":", integer_to_list(mochiweb_socket_server:get(couch_httpd, port)),
        "/", DbName
    ]).

replicate(Source, Target) ->
    replicate({[
        {<<"source">>, Source},
        {<<"target">>, Target}
    ]}).

replicate({[_ | _]} = RepObject) ->
    {ok, Rep} = couch_replicator_utils:parse_rep_doc(RepObject, ?ADMIN_USER),
    {ok, Pid} = couch_replicator:async_replicate(Rep),
    MonRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MonRef, process, Pid, _} ->
            ok
    end.
