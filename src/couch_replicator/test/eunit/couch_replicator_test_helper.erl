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

-module(couch_replicator_test_helper).


-export([
    start_couch/0,
    stop_couch/1,

    create_db/0,
    create_db/1,
    delete_db/1,

    server_url/0,
    db_url/1,

    create_docs/2,

    compare_dbs/2,
    compare_dbs/3,
    compare_fold/4,

    compare_docs/2,

    get_pid/1,

    replicate/1,
    replicate/2,
    replicate_continuous/1,
    replicate_continuous/2,

    cancel/1,
    cancel/2,

    scheduler_jobs/0
]).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").


-define(USERNAME, "rep_eunit_admin").
-define(PASSWORD, "rep_eunit_password").


start_couch() ->
    Ctx = test_util:start_couch([fabric, chttpd, couch_replicator]),
    Hashed = couch_passwords:hash_admin_password(?PASSWORD),
    ok = config:set("admins", ?USERNAME, ?b2l(Hashed), _Persist = false),
    Ctx.


stop_couch(Ctx) ->
    config:delete("admins", ?USERNAME, _Persist = false),
    test_util:stop_couch(Ctx).


create_db() ->
    {ok, Db} = fabric2_db:create(?tempdb(), [?ADMIN_CTX]),
    fabric2_db:name(Db).


create_db(DbName) when is_binary(DbName) ->
    {ok, Db} = fabric2_db:create(DbName, [?ADMIN_CTX]),
    fabric2_db:name(Db).


delete_db(DbName) ->
    try
        ok = fabric2_db:delete(DbName, [?ADMIN_CTX])
    catch
        error:database_does_not_exist ->
            ok
    end.


server_url() ->
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Fmt = "http://~s:~s@~s:~b",
    ?l2b(io_lib:format(Fmt, [?USERNAME, ?PASSWORD, Addr, Port])).


db_url(DbName) ->
    ?l2b(io_lib:format("~s/~s", [server_url(), DbName])).


create_docs(DbName, Docs) when is_binary(DbName), is_list(Docs) ->
    {ok, Db} = fabric2_db:open(DbName, [?ADMIN_CTX]),
    Docs1 = lists:map(fun(Doc) ->
        case Doc of
            #{} ->
                Doc1 = couch_util:json_decode(couch_util:json_encode(Doc)),
                couch_doc:from_json_obj(Doc1);
            #doc{} ->
                Doc
        end
    end, Docs),
    {ok, ResList} = fabric2_db:update_docs(Db, Docs1),
    lists:foreach(fun(Res) ->
        ?assertMatch({ok, {_, Rev}} when is_binary(Rev), Res)
    end, ResList).


compare_dbs(Source, Target) ->
    Fun = fun(SrcDoc, TgtDoc, ok) -> compare_docs(SrcDoc, TgtDoc) end,
    compare_fold(Source, Target, Fun, ok).


compare_dbs(Source, Target, ExceptIds) when is_binary(Source),
        is_binary(Target), is_list(ExceptIds) ->
    Fun = fun(SrcDoc, TgtDoc, ok) ->
        case lists:member(SrcDoc#doc.id, ExceptIds) of
            true -> ?assertEqual(not_found, TgtDoc);
            false -> compare_docs(SrcDoc, TgtDoc)
        end,
        ok
    end,
    compare_fold(Source, Target, Fun, ok).


compare_fold(Source, Target, Fun, Acc0) when
        is_binary(Source), is_binary(Target), is_function(Fun, 3) ->
    {ok, SourceDb} = fabric2_db:open(Source, [?ADMIN_CTX]),
    {ok, TargetDb} = fabric2_db:open(Target, [?ADMIN_CTX]),
    fabric2_fdb:transactional(SourceDb, fun(TxSourceDb) ->
        FoldFun = fun
            ({meta, _Meta}, Acc) ->
                {ok, Acc};
            (complete, Acc) ->
                {ok, Acc};
            ({row, Row}, Acc) ->
                {_, Id} = lists:keyfind(id, 1, Row),
                SrcDoc = open_doc(TxSourceDb, Id),
                TgtDoc = open_doc(TargetDb, Id),
                {ok, Fun(SrcDoc, TgtDoc, Acc)}
        end,
        Opts = [{restart_tx, true}],
        {ok, AccF} = fabric2_db:fold_docs(TxSourceDb, FoldFun, Acc0, Opts),
        AccF
    end).


compare_docs(#doc{} = Doc1, Doc2) when
        is_record(Doc2, doc) orelse Doc2 =:= not_found ->
    ?assert(Doc2 =/= not_found),
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


get_pid(RepId) ->
    JobId = case couch_replicator_jobs:get_job_id(undefined, RepId) of
        {ok, JobId0} -> JobId0;
        {error, not_found} -> RepId
    end,
    {ok, #{<<"state">> := <<"running">>, <<"pid">> := Pid0}} =
            couch_replicator_jobs:get_job_data(undefined, JobId),
    Pid = list_to_pid(binary_to_list(Pid0)),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    Pid.


replicate({[_ | _]} = EJson) ->
    Str = couch_util:json_encode(EJson),
    replicate(couch_util:json_decode(Str, [return_maps]));

replicate(#{} = Rep0) ->
    Rep = maybe_db_urls(Rep0),
    {ok, Id, _} = couch_replicator_parse:parse_transient_rep(Rep, null),
    ok = cancel(Id),
    try
        couch_replicator:replicate(Rep, ?ADMIN_USER)
    after
        ok = cancel(Id)
    end.


replicate(Source, Target) ->
    replicate(#{
        <<"source">> => Source,
        <<"target">> => Target
    }).


replicate_continuous({[_ | _]} = EJson) ->
    Str = couch_util:json_encode(EJson),
    replicate_continuous(couch_util:json_decode(Str, [return_maps]));

replicate_continuous(#{<<"continuous">> := true} = Rep0) ->
    Rep = maybe_db_urls(Rep0),
    {ok, {continuous, RepId}} = couch_replicator:replicate(Rep, ?ADMIN_USER),
    {ok, get_pid(RepId), RepId}.


replicate_continuous(Source, Target) ->
    replicate_continuous(#{
        <<"source">> => Source,
        <<"target">> => Target,
        <<"continuous">> => true
    }).


cancel(Id) when is_binary(Id) ->
    CancelRep = #{<<"cancel">> => true, <<"id">> => Id},
    case couch_replicator:replicate(CancelRep, ?ADMIN_USER) of
        {ok, {cancelled, <<_/binary>>}} -> ok;
        {error, not_found} -> ok
    end.


cancel(Id, Pid) when is_pid(Pid), is_binary(Id) ->
    Ref = monitor(process, Pid),
    try
        cancel(Id)
    after
        receive
            {'DOWN', Ref, _, _, _} -> ok
        after 60000 ->
            error(replicator_pid_death_timeout)
        end
    end.


scheduler_jobs() ->
    ServerUrl = couch_replicator_test_helper:server_url(),
    Url = lists:flatten(io_lib:format("~s/_scheduler/jobs", [ServerUrl])),
    {ok, 200, _, Body} = test_request:get(Url, []),
    Json = jiffy:decode(Body, [return_maps]),
    maps:get(<<"jobs">>, Json).


open_doc(Db, DocId) ->
    case fabric2_db:open_doc(Db, DocId, []) of
        {ok, #doc{deleted = false} = Doc} -> Doc;
        {not_found, missing} -> not_found
    end.


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
    Md50 = couch_att:foldl(Att, fun(Chunk, Acc) ->
        couch_hash:md5_hash_update(Acc, Chunk)
    end, couch_hash:md5_hash_init()),
    couch_hash:md5_hash_final(Md50).


att_decoded_md5(Att) ->
    Md50 = couch_att:foldl_decode(Att, fun(Chunk, Acc) ->
        couch_hash:md5_hash_update(Acc, Chunk)
    end, couch_hash:md5_hash_init()),
    couch_hash:md5_hash_final(Md50).


maybe_db_urls(#{} = Rep) ->
    #{<<"source">> := Src, <<"target">> := Tgt} = Rep,
    Src1 = case Src of
        <<"http://", _/binary>> -> Src;
        <<"https://", _/binary>> -> Src;
        <<_/binary>> -> db_url(Src)
    end,
    Tgt1 = case Tgt of
        <<"http://", _/binary>> -> Tgt;
        <<"https://", _/binary>> -> Tgt;
        <<_/binary>> -> db_url(Tgt)
    end,
    Rep#{<<"source">> := Src1, <<"target">> := Tgt1}.
