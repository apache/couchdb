-module(couch_replicator_test_helper).

-export([
    cluster_compare_dbs/2,
    cluster_compare_dbs/3,
    cluster_doc_revs/1,
    cluster_open_rev/3,
    cluster_url/0,
    cluster_db_url/1,
    replicate/1,
    get_pid/1,
    replicate/2,
    test_setup/0,
    test_teardown/1,
    setup_db/0,
    teardown_db/1
]).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").

-define(USERNAME, "rep_test_user").
-define(PASSWORD, "rep_test_pass").

cluster_compare_dbs(Source, Target) ->
    cluster_compare_dbs(Source, Target, []).

cluster_compare_dbs(Source, Target, ExceptIds) ->
    ?assertMatch({ok, [_ | _]}, fabric:get_db_info(Source)),
    ?assertMatch({ok, [_ | _]}, fabric:get_db_info(Target)),
    lists:foreach(
        fun({Id, Rev}) ->
            SrcDoc = cluster_open_rev(Source, Id, Rev),
            TgtDoc = cluster_open_rev(Target, Id, Rev),
            case lists:member(Id, ExceptIds) of
                true ->
                    ?assertEqual(not_found, TgtDoc);
                false ->
                    compare_docs(SrcDoc, TgtDoc)
            end
        end,
        cluster_doc_revs(Source)
    ).

cluster_open_rev(DbName, Id, Rev) ->
    {ok, [Result]} = fabric:open_revs(DbName, Id, [Rev], []),
    case Result of
        {ok, #doc{} = Doc} ->
            Doc;
        {{not_found, missing}, _} ->
            not_found
    end.

cluster_doc_revs(DbName) ->
    Opts = [{style, all_docs}],
    {ok, Acc} = fabric_util:isolate(fun() ->
        fabric:changes(DbName, fun changes_callback/2, [], Opts)
    end),
    Acc.

changes_callback(start, Acc) ->
    {ok, Acc};
changes_callback({change, {Change}}, Acc) ->
    Id = proplists:get_value(id, Change),
    Revs = proplists:get_value(changes, Change),
    IdRevs = [{Id, couch_doc:parse_rev(R)} || {[{<<"rev">>, R}]} <- Revs],
    {ok, IdRevs ++ Acc};
changes_callback(timeout, Acc) ->
    {ok, Acc};
changes_callback({stop, _EndSeq, _Pending}, Acc) ->
    {ok, Acc}.

compare_docs(#doc{} = Doc1, not_found) ->
    error({not_found, Doc1#doc.id});
compare_docs(Doc1, Doc2) ->
    ?assertEqual(Doc1#doc.body, Doc2#doc.body),
    #doc{atts = Atts1} = Doc1,
    #doc{atts = Atts2} = Doc2,
    ?assertEqual(
        lists:sort([couch_att:fetch(name, Att) || Att <- Atts1]),
        lists:sort([couch_att:fetch(name, Att) || Att <- Atts2])
    ),
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
        ?assertEqual(
            couch_att:fetch(disk_len, Att),
            couch_att:fetch(disk_len, AttTarget)
        ),
        ?assertEqual(
            couch_att:fetch(att_len, Att),
            couch_att:fetch(att_len, AttTarget)
        ),
        ?assertEqual(
            couch_att:fetch(type, Att),
            couch_att:fetch(type, AttTarget)
        ),
        ?assertEqual(
            couch_att:fetch(md5, Att),
            couch_att:fetch(md5, AttTarget)
        )
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
        fun(Chunk, Acc) -> couch_hash:md5_hash_update(Acc, Chunk) end,
        couch_hash:md5_hash_init()
    ),
    couch_hash:md5_hash_final(Md50).

att_decoded_md5(Att) ->
    Md50 = couch_att:foldl_decode(
        Att,
        fun(Chunk, Acc) -> couch_hash:md5_hash_update(Acc, Chunk) end,
        couch_hash:md5_hash_init()
    ),
    couch_hash:md5_hash_final(Md50).

cluster_url() ->
    Fmt = "http://~s:~s@~s:~b",
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Args = [?USERNAME, ?PASSWORD, Addr, Port],
    ?l2b(io_lib:format(Fmt, Args)).

cluster_db_url(Path) when is_list(Path) ->
    cluster_db_url(list_to_binary(Path));
cluster_db_url(<<"/", _/binary>> = Path) ->
    <<(cluster_url())/binary, Path/binary>>;
cluster_db_url(Path) ->
    <<(cluster_url())/binary, "/", Path/binary>>.

get_pid(RepId) ->
    [Pid] = couch_replicator_pg:pids(RepId),
    ?assert(is_pid(Pid)),
    Pid.

replicate(Source, Target) ->
    replicate(
        {[
            {<<"source">>, Source},
            {<<"target">>, Target}
        ]}
    ).

replicate(#{} = RepObject) ->
    replicate(jiffy:decode(jiffy:encode(RepObject)));
replicate({[_ | _]} = RepObject) ->
    {ok, Rep} = couch_replicator_parse:parse_rep_doc(RepObject, ?ADMIN_USER),
    ok = couch_replicator_scheduler:add_job(Rep),
    couch_replicator_scheduler:reschedule(),
    Pid = get_pid(Rep#rep.id),
    MonRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MonRef, process, Pid, _} ->
            ok
    end,
    ok = couch_replicator_scheduler:remove_job(Rep#rep.id).

setup_db() ->
    DbName = ?tempdb(),
    ok = fabric:create_db(DbName, [{q, 1}, {n, 1}, ?ADMIN_CTX]),
    DbName.

teardown_db(DbName) ->
    try
        ok = fabric:delete_db(DbName, [?ADMIN_CTX])
    catch
        error:database_does_not_exist ->
            ok
    end.

test_setup() ->
    Ctx = test_util:start_couch([fabric, mem3, chttpd, couch_replicator]),
    Hashed = couch_passwords:hash_admin_password(?PASSWORD),
    Persist = false,
    ok = config:set("admins", ?USERNAME, ?b2l(Hashed), Persist),
    ok = config:set("replicator", "cluster_start_period", "0", Persist),
    Source = setup_db(),
    Target = setup_db(),
    {Ctx, {Source, Target}}.

test_teardown({Ctx, {Source, Target}}) ->
    meck:unload(),
    teardown_db(Source),
    teardown_db(Target),
    Persist = false,
    config:delete("admins", ?USERNAME, Persist),
    config:delete("replicator", "cluster_start_period", Persist),
    ok = test_util:stop_couch(Ctx).
