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

-module(cpse_util).
-compile(export_all).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(TEST_MODULES, [
    cpse_test_open_close_delete,
    cpse_test_get_set_props,
    cpse_test_read_write_docs,
    cpse_test_attachments,
    cpse_test_fold_docs,
    cpse_test_fold_changes,
    cpse_test_purge_docs,
    cpse_test_compaction,
    cpse_test_ref_counting
]).


-define(SHUTDOWN_TIMEOUT, 5000).
-define(COMPACTOR_TIMEOUT, 50000).
-define(ATTACHMENT_WRITE_TIMEOUT, 10000).
-define(MAKE_DOC_SUMMARY_TIMEOUT, 5000).


create_tests(EngineApp, Extension) ->
    create_tests(EngineApp, EngineApp, Extension).


create_tests(EngineApp, EngineModule, Extension) ->
    TestEngine = {EngineApp, EngineModule, Extension},
    application:set_env(couch, test_engine, TestEngine),
    lists:map(fun(TestMod) ->
        {atom_to_list(TestMod), cpse_gather:module(TestMod)}
    end, ?TEST_MODULES).


setup_all() ->
    setup_all([]).


setup_all(ExtraApps) ->
    Ctx = test_util:start_couch(ExtraApps),
    {ok, {_, EngineMod, Extension}} = application:get_env(couch, test_engine),
    EngineModStr = atom_to_list(EngineMod),
    config:set("couchdb_engines", Extension, EngineModStr, false),
    config:set("log", "include_sasl", "false", false),
    Ctx.


teardown_all(Ctx) ->
    test_util:stop_couch(Ctx).


rootdir() ->
    config:get("couchdb", "database_dir", ".").


dbname() ->
    UUID = couch_uuids:random(),
    <<"db-", UUID/binary>>.


get_engine() ->
    case application:get_env(couch, test_engine) of
        {ok, {_App, _Mod, Extension}} ->
            list_to_binary(Extension);
        _ ->
            <<"couch">>
    end.


create_db() ->
    create_db(dbname()).


create_db(DbName) ->
    Engine = get_engine(),
    couch_db:create(DbName, [{engine, Engine}, ?ADMIN_CTX]).


open_db(DbName) ->
    Engine = get_engine(),
    couch_db:open_int(DbName, [{engine, Engine}, ?ADMIN_CTX]).


shutdown_db(Db) ->
    Pid = couch_db:get_pid(Db),
    Ref = erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Ref, _, _, _} ->
            ok
    after ?SHUTDOWN_TIMEOUT ->
        erlang:error(database_shutdown_timeout)
    end,
    test_util:wait(fun() ->
        case ets:member(couch_dbs, couch_db:name(Db)) of
            true -> wait;
            false -> ok
        end
    end).


apply_actions(Db, []) ->
    {ok, Db};

apply_actions(Db, [Action | Rest]) ->
    {ok, NewDb} = apply_action(Db, Action),
    apply_actions(NewDb, Rest).


apply_action(Db, {batch, BatchActions}) ->
    apply_batch(Db, BatchActions);

apply_action(Db, Action) ->
    apply_batch(Db, [Action]).


apply_batch(Db, Actions) ->
    AccIn = {[], [], [], []},
    AccOut = lists:foldl(fun(Action, Acc) ->
        {DocAcc, ConfAcc, LDocAcc, PurgeAcc} = Acc,
        case gen_write(Db, Action) of
            {update, Doc} ->
                {[Doc | DocAcc], ConfAcc, LDocAcc, PurgeAcc};
            {conflict, Doc} ->
                {DocAcc, [Doc | ConfAcc], LDocAcc, PurgeAcc};
            {local, Doc} ->
                {DocAcc, ConfAcc, [Doc | LDocAcc], PurgeAcc};
            {purge, PurgeInfo} ->
                {DocAcc, ConfAcc, LDocAcc, [PurgeInfo | PurgeAcc]}
        end
    end, AccIn, Actions),

    {Docs0, Conflicts0, LDocs0, PurgeInfos0} = AccOut,
    Docs = lists:reverse(Docs0),
    Conflicts = lists:reverse(Conflicts0),
    LDocs = lists:reverse(LDocs0),
    PurgeInfos = lists:reverse(PurgeInfos0),

    {ok, Resp} = couch_db:update_docs(Db, Docs ++ LDocs),
    false = lists:member(conflict, Resp),
    {ok, Db1} = couch_db:reopen(Db),

    {ok, []} = couch_db:update_docs(Db, Conflicts, [], replicated_changes),
    {ok, Db2} = couch_db:reopen(Db1),

    if PurgeInfos == [] -> ok; true ->
        {ok, _, _} = couch_db:purge_docs(Db2, PurgeInfos)
    end,
    couch_db:reopen(Db2).


gen_write(Db, {Action, {<<"_local/", _/binary>> = DocId, Body}}) ->
    PrevRev = case couch_db:open_doc(Db, DocId) of
        {not_found, _} ->
            0;
        {ok, #doc{revs = {0, []}}} ->
            0;
        {ok, #doc{revs = {0, [RevStr | _]}}} ->
            binary_to_integer(RevStr)
    end,
    {RevId, Deleted} = case Action of
        Action when Action == create; Action == update ->
            {PrevRev + 1, false};
        delete ->
            {0, true}
    end,
    {local, #doc{
        id = DocId,
        revs = {0, [list_to_binary(integer_to_list(RevId))]},
        body = Body,
        deleted = Deleted
    }};

gen_write(Db, {Action, {DocId, Body}}) ->
    gen_write(Db, {Action, {DocId, Body, []}});

gen_write(Db, {create, {DocId, Body, Atts}}) ->
    {not_found, _} = couch_db:open_doc(Db, DocId),
    {update, #doc{
        id = DocId,
        revs = {0, []},
        deleted = false,
        body = Body,
        atts = Atts
    }};

gen_write(_Db, {purge, {DocId, PrevRevs0, _}}) ->
    PrevRevs = if is_list(PrevRevs0) -> PrevRevs0; true -> [PrevRevs0] end,
    {purge, {DocId, PrevRevs}};

gen_write(Db, {Action, {DocId, Body, Atts}}) ->
    #full_doc_info{} = PrevFDI = couch_db:get_full_doc_info(Db, DocId),

    #full_doc_info{
        id = DocId
    } = PrevFDI,

    #rev_info{
        rev = PrevRev
    } = prev_rev(PrevFDI),

    NewRev = gen_rev(Action, DocId, PrevRev, Body, Atts),

    Deleted = case Action of
        update -> false;
        conflict -> false;
        delete -> true
    end,

    Type = case Action of
        conflict -> conflict;
        _ -> update
    end,

    {Type, #doc{
        id = DocId,
        revs = NewRev,
        deleted = Deleted,
        body = Body,
        atts = Atts
    }}.


gen_rev(A, DocId, {Pos, Rev}, Body, Atts) when A == update; A == delete ->
    NewRev = crypto:hash(md5, term_to_binary({DocId, Rev, Body, Atts})),
    {Pos + 1, [NewRev, Rev]};
gen_rev(conflict, DocId, _, Body, Atts) ->
    UUID = couch_uuids:random(),
    NewRev = crypto:hash(md5, term_to_binary({DocId, UUID, Body, Atts})),
    {1, [NewRev]}.


prep_atts(_Db, []) ->
    [];

prep_atts(Db, [{FileName, Data} | Rest]) ->
    {_, Ref} = spawn_monitor(fun() ->
        {ok, Stream} = couch_db:open_write_stream(Db, []),
        exit(write_att(Stream, FileName, Data, Data))
    end),
    Att = receive
        {'DOWN', Ref, _, _, {{no_catch, not_supported}, _}} ->
            throw(not_supported);
        {'DOWN', Ref, _, _, Resp} ->
            Resp
        after ?ATTACHMENT_WRITE_TIMEOUT ->
            erlang:error(attachment_write_timeout)
    end,
    [Att | prep_atts(Db, Rest)].


write_att(Stream, FileName, OrigData, <<>>) ->
    {StreamEngine, Len, Len, Md5, Md5} = couch_stream:close(Stream),
    couch_util:check_md5(Md5, couch_hash:md5_hash(OrigData)),
    Len = size(OrigData),
    couch_att:new([
        {name, FileName},
        {type, <<"application/octet-stream">>},
        {data, {stream, StreamEngine}},
        {att_len, Len},
        {disk_len, Len},
        {md5, Md5},
        {encoding, identity}
    ]);

write_att(Stream, FileName, OrigData, Data) ->
    {Chunk, Rest} = case size(Data) > 4096 of
        true ->
            <<Head:4096/binary, Tail/binary>> = Data,
            {Head, Tail};
        false ->
            {Data, <<>>}
    end,
    ok = couch_stream:write(Stream, Chunk),
    write_att(Stream, FileName, OrigData, Rest).


prev_rev(#full_doc_info{} = FDI) ->
    #doc_info{
        revs = [#rev_info{} = PrevRev | _]
    } = couch_doc:to_doc_info(FDI),
    PrevRev.


db_as_term(Db) ->
    [
        {props, db_props_as_term(Db)},
        {docs, db_docs_as_term(Db)},
        {local_docs, db_local_docs_as_term(Db)},
        {changes, db_changes_as_term(Db)}
    ].


db_props_as_term(Db) ->
    Props = [
        get_doc_count,
        get_del_doc_count,
        get_disk_version,
        get_update_seq,
        get_purge_seq,
        get_last_purged,
        get_security,
        get_revs_limit,
        get_uuid,
        get_epochs
    ],
    lists:map(fun(Fun) ->
        {Fun, couch_db_engine:Fun(Db)}
    end, Props).


db_docs_as_term(Db) ->
    FoldFun = fun(FDI, Acc) -> {ok, [FDI | Acc]} end,
    {ok, FDIs} = couch_db:fold_docs(Db, FoldFun, [], []),
    lists:reverse(lists:map(fun(FDI) ->
        fdi_to_term(Db, FDI)
    end, FDIs)).


db_local_docs_as_term(Db) ->
    FoldFun = fun(Doc, Acc) -> {ok, [Doc | Acc]} end,
    {ok, LDocs} = couch_db:fold_local_docs(Db, FoldFun, [], []),
    lists:reverse(LDocs).


db_changes_as_term(Db) ->
    FoldFun = fun(FDI, Acc) -> {ok, [FDI | Acc]} end,
    {ok, Changes} = couch_db:fold_changes(Db, 0, FoldFun, [], []),
    lists:reverse(lists:map(fun(FDI) ->
        fdi_to_term(Db, FDI)
    end, Changes)).


fdi_to_term(Db, FDI) ->
    #full_doc_info{
        id = DocId,
        rev_tree = OldTree
    } = FDI,
    {NewRevTree, _} = couch_key_tree:mapfold(fun(Rev, Node, Type, Acc) ->
        tree_to_term(Rev, Node, Type, Acc, DocId)
    end, Db, OldTree),
    FDI#full_doc_info{
        rev_tree = NewRevTree,
        % Blank out sizes because we allow storage
        % engines to handle this with their own
        % definition until further notice.
        sizes = #size_info{
            active = -1,
            external = -1
        }
    }.


tree_to_term(_Rev, _Leaf, branch, Acc, _DocId) ->
    {?REV_MISSING, Acc};

tree_to_term({Pos, RevId}, #leaf{} = Leaf, leaf, Db, DocId) ->
    #leaf{
        deleted = Deleted,
        ptr = Ptr
    } = Leaf,

    Doc0 = #doc{
        id = DocId,
        revs = {Pos, [RevId]},
        deleted = Deleted,
        body = Ptr
    },

    Doc1 = couch_db_engine:read_doc_body(Db, Doc0),

    Body = if not is_binary(Doc1#doc.body) -> Doc1#doc.body; true ->
        couch_compress:decompress(Doc1#doc.body)
    end,

    Atts1 = if not is_binary(Doc1#doc.atts) -> Doc1#doc.atts; true ->
        couch_compress:decompress(Doc1#doc.atts)
    end,

    StreamSrc = fun(Sp) -> couch_db:open_read_stream(Db, Sp) end,
    Atts2 = [couch_att:from_disk_term(StreamSrc, Att) || Att <- Atts1],
    Atts = [att_to_term(Att) || Att <- Atts2],

    NewLeaf = Leaf#leaf{
        ptr = Body,
        sizes = #size_info{active = -1, external = -1},
        atts = Atts
    },
    {NewLeaf, Db}.


att_to_term(Att) ->
    Bin = couch_att:to_binary(Att),
    couch_att:store(data, Bin, Att).


term_diff(T1, T2) when is_tuple(T1), is_tuple(T2) ->
    tuple_diff(tuple_to_list(T1), tuple_to_list(T2));

term_diff(L1, L2) when is_list(L1), is_list(L2) ->
    list_diff(L1, L2);

term_diff(V1, V2) when V1 == V2 ->
    nodiff;

term_diff(V1, V2) ->
    {V1, V2}.


tuple_diff([], []) ->
    nodiff;

tuple_diff([T1 | _], []) ->
    {longer, T1};

tuple_diff([], [T2 | _]) ->
    {shorter, T2};

tuple_diff([T1 | R1], [T2 | R2]) ->
    case term_diff(T1, T2) of
        nodiff ->
            tuple_diff(R1, R2);
        Else ->
            {T1, Else}
    end.


list_diff([], []) ->
    nodiff;

list_diff([T1 | _], []) ->
    {longer, T1};

list_diff([], [T2 | _]) ->
    {shorter, T2};

list_diff([T1 | R1], [T2 | R2]) ->
    case term_diff(T1, T2) of
        nodiff ->
            list_diff(R1, R2);
        Else ->
            {T1, Else}
    end.


compact(Db) ->
    {ok, Pid} = couch_db:start_compact(Db),
    Ref = erlang:monitor(process, Pid),

    % Ideally I'd assert that Pid is linked to us
    % at this point but its technically possible
    % that it could have finished compacting by
    % the time we check... Quite the quandry.

    receive
        {'DOWN', Ref, _, _, normal} ->
            ok;
        {'DOWN', Ref, _, _, noproc} ->
            ok;
        {'DOWN', Ref, _, _, Reason} ->
            erlang:error({compactor_died, Reason})
        after ?COMPACTOR_TIMEOUT ->
            erlang:error(compactor_timed_out)
    end,

    test_util:wait(fun() ->
        {ok, Db2} = couch_db:open_int(couch_db:name(Db), []),
        try
            CPid = couch_db:get_compactor_pid(Db2),
            case is_pid(CPid) of
                true -> wait;
                false -> ok
            end
        after
            couch_db:close(Db2)
        end
    end).


with_config(Config, Fun) ->
    OldConfig = apply_config(Config),
    try
        Fun()
    after
        apply_config(OldConfig)
    end.


apply_config([]) ->
    [];

apply_config([{Section, Key, Value} | Rest]) ->
    Orig = config:get(Section, Key),
    case Value of
        undefined -> config:delete(Section, Key, false);
        _ -> config:set(Section, Key, Value, false)
    end,
    [{Section, Key, Orig} | apply_config(Rest)].
