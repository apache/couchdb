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

-module(test_engine_util).
-compile(export_all).


-include_lib("couch/include/couch_db.hrl").


-define(TEST_MODULES, [
    test_engine_open_close_delete,
    test_engine_get_set_props,
    test_engine_read_write_docs,
    test_engine_attachments,
    test_engine_fold_docs,
    test_engine_fold_changes,
    test_engine_purge_docs,
    test_engine_compaction,
    test_engine_ref_counting
]).


create_tests(EngineApp) ->
    create_tests(EngineApp, EngineApp).


create_tests(EngineApp, EngineModule) ->
    application:set_env(couch, test_engine, {EngineApp, EngineModule}),
    Tests = lists:map(fun(TestMod) ->
        {atom_to_list(TestMod), gather(TestMod)}
    end, ?TEST_MODULES),
    Setup = fun() ->
        Ctx = test_util:start_couch(),
        config:set("log", "include_sasl", "false", false),
        Ctx
    end,
    {
        setup,
        Setup,
        fun test_util:stop_couch/1,
        fun(_) -> Tests end
    }.


gather(Module) ->
    Exports = Module:module_info(exports),
    Tests = lists:foldl(fun({Fun, Arity}, Acc) ->
        case {atom_to_list(Fun), Arity} of
            {[$c, $e, $t, $_ | _], 0} ->
                TestFun = make_test_fun(Module, Fun),
                [{spawn, TestFun} | Acc];
            _ ->
                Acc
        end
    end, [], Exports),
    lists:reverse(Tests).


make_test_fun(Module, Fun) ->
    Name = lists:flatten(io_lib:format("~s:~s", [Module, Fun])),
    Wrapper = fun() ->
        process_flag(trap_exit, true),
        Module:Fun()
    end,
    {Name, Wrapper}.

rootdir() ->
    config:get("couchdb", "database_dir", ".").


dbpath() ->
    binary_to_list(filename:join(rootdir(), couch_uuids:random())).


get_engine() ->
    case application:get_env(couch, test_engine) of
        {ok, {_, Engine}} ->
            Engine;
        _ ->
            couch_bt_engine
    end.


init_engine() ->
    init_engine(default).


init_engine(default) ->
    Engine = get_engine(),
    DbPath = dbpath(),
    {ok, St} = Engine:init(DbPath, [
            create,
            {default_security_object, []}
        ]),
    {ok, Engine, St};

init_engine(dbpath) ->
    Engine = get_engine(),
    DbPath = dbpath(),
    {ok, St} = Engine:init(DbPath, [
            create,
            {default_security_object, []}
        ]),
    {ok, Engine, DbPath, St}.


apply_actions(_Engine, St, []) ->
    {ok, St};

apply_actions(Engine, St, [Action | Rest]) ->
    NewSt = apply_action(Engine, St, Action),
    apply_actions(Engine, NewSt, Rest).


apply_action(Engine, St, {batch, BatchActions}) ->
    apply_batch(Engine, St, BatchActions);

apply_action(Engine, St, Action) ->
    apply_batch(Engine, St, [Action]).


apply_batch(Engine, St, Actions) ->
    UpdateSeq = Engine:get_update_seq(St) + 1,
    AccIn = {UpdateSeq, [], [], []},
    AccOut = lists:foldl(fun(Action, Acc) ->
        {SeqAcc, DocAcc, LDocAcc, PurgeAcc} = Acc,
        case Action of
            {_, {<<"_local/", _/binary>>, _}} ->
                LDoc = gen_local_write(Engine, St, Action),
                {SeqAcc, DocAcc, [LDoc | LDocAcc], PurgeAcc};
            _ ->
                case gen_write(Engine, St, Action, SeqAcc) of
                    {_OldFDI, _NewFDI} = Pair ->
                        {SeqAcc + 1, [Pair | DocAcc], LDocAcc, PurgeAcc};
                    {Pair, NewSeqAcc, NewPurgeInfo} ->
                        NewPurgeAcc = [NewPurgeInfo | PurgeAcc],
                        {NewSeqAcc, [Pair | DocAcc], LDocAcc, NewPurgeAcc}
                end
        end
    end, AccIn, Actions),
    {_, Docs0, LDocs, PurgeIdRevs} = AccOut,
    Docs = lists:reverse(Docs0),
    {ok, NewSt} = Engine:write_doc_infos(St, Docs, LDocs, PurgeIdRevs),
    NewSt.


gen_local_write(Engine, St, {Action, {DocId, Body}}) ->
    PrevRev = case Engine:open_local_docs(St, [DocId]) of
        [not_found] ->
            0;
        [#doc{revs = {0, []}}] ->
            0;
        [#doc{revs = {0, [RevStr | _]}}] ->
            list_to_integer(binary_to_list(RevStr))
    end,
    {RevId, Deleted} = case Action of
        Action when Action == create; Action == update ->
            {list_to_binary(integer_to_list(PrevRev + 1)), false};
        delete ->
            {<<"0">>, true}
    end,
    #doc{
        id = DocId,
        revs = {0, [RevId]},
        body = Body,
        deleted = Deleted
    }.

gen_write(Engine, St, {Action, {DocId, Body}}, UpdateSeq) ->
    gen_write(Engine, St, {Action, {DocId, Body, []}}, UpdateSeq);

gen_write(Engine, St, {create, {DocId, Body, Atts0}}, UpdateSeq) ->
    [not_found] = Engine:open_docs(St, [DocId]),
    Atts = [couch_att:to_disk_term(Att) || Att <- Atts0],

    Rev = crypto:hash(md5, term_to_binary({DocId, Body, Atts})),

    Doc0 = #doc{
        id = DocId,
        revs = {0, [Rev]},
        deleted = false,
        body = Body,
        atts = Atts
    },

    Doc1 = make_doc_summary(Engine, St, Doc0),
    {ok, Doc2, Len} = Engine:write_doc_body(St, Doc1),

    Sizes = #size_info{
        active = Len,
        external = erlang:external_size(Doc1#doc.body)
    },

    Leaf = #leaf{
        deleted = false,
        ptr = Doc2#doc.body,
        seq = UpdateSeq,
        sizes = Sizes,
        atts = Atts
    },

    {not_found, #full_doc_info{
        id = DocId,
        deleted = false,
        update_seq = UpdateSeq,
        rev_tree = [{0, {Rev, Leaf, []}}],
        sizes = Sizes
    }};

gen_write(Engine, St, {purge, {DocId, PrevRevs0, _}}, UpdateSeq) ->
    [#full_doc_info{} = PrevFDI] = Engine:open_docs(St, [DocId]),
    PrevRevs = if is_list(PrevRevs0) -> PrevRevs0; true -> [PrevRevs0] end,

    #full_doc_info{
        rev_tree = PrevTree
    } = PrevFDI,

    {NewTree, RemRevs} = couch_key_tree:remove_leafs(PrevTree, PrevRevs),
    RemovedAll = lists:sort(RemRevs) == lists:sort(PrevRevs),
    if RemovedAll -> ok; true ->
        % If we didn't purge all the requested revisions
        % then its a bug in the test.
        erlang:error({invalid_purge_test_revs, PrevRevs})
    end,

    case NewTree of
        [] ->
            % We've completely purged the document
            {{PrevFDI, not_found}, UpdateSeq, {DocId, RemRevs}};
        _ ->
            % We have to relabel the update_seq of all
            % leaves. See couch_db_updater for details.
            {NewNewTree, NewUpdateSeq} = couch_key_tree:mapfold(fun
                (_RevId, Leaf, leaf, InnerSeqAcc) ->
                    {Leaf#leaf{seq = InnerSeqAcc}, InnerSeqAcc + 1};
                (_RevId, Value, _Type, InnerSeqAcc) ->
                    {Value, InnerSeqAcc}
            end, UpdateSeq, NewTree),
            NewFDI = PrevFDI#full_doc_info{
                update_seq = NewUpdateSeq - 1,
                rev_tree = NewNewTree
            },
            {{PrevFDI, NewFDI}, NewUpdateSeq, {DocId, RemRevs}}
    end;

gen_write(Engine, St, {Action, {DocId, Body, Atts0}}, UpdateSeq) ->
    [#full_doc_info{} = PrevFDI] = Engine:open_docs(St, [DocId]),
    Atts = [couch_att:to_disk_term(Att) || Att <- Atts0],

    #full_doc_info{
        id = DocId,
        rev_tree = PrevRevTree
    } = PrevFDI,

    #rev_info{
        rev = PrevRev
    } = prev_rev(PrevFDI),

    {RevPos, PrevRevId} = PrevRev,

    Rev = gen_revision(Action, DocId, PrevRev, Body, Atts),

    Doc0 = #doc{
        id = DocId,
        revs = {RevPos + 1, [Rev, PrevRevId]},
        deleted = false,
        body = Body,
        atts = Atts
    },

    Doc1 = make_doc_summary(Engine, St, Doc0),
    {ok, Doc2, Len} = Engine:write_doc_body(St, Doc1),

    Deleted = case Action of
        update -> false;
        conflict -> false;
        delete -> true
    end,

    Sizes = #size_info{
        active = Len,
        external = erlang:external_size(Doc1#doc.body)
    },

    Leaf = #leaf{
        deleted = Deleted,
        ptr = Doc2#doc.body,
        seq = UpdateSeq,
        sizes = Sizes,
        atts = Atts
    },

    Path = gen_path(Action, RevPos, PrevRevId, Rev, Leaf),
    RevsLimit = Engine:get_revs_limit(St),
    NodeType = case Action of
        conflict -> new_branch;
        _ -> new_leaf
    end,
    {NewTree, NodeType} = couch_key_tree:merge(PrevRevTree, Path, RevsLimit),

    NewFDI = PrevFDI#full_doc_info{
        deleted = couch_doc:is_deleted(NewTree),
        update_seq = UpdateSeq,
        rev_tree = NewTree,
        sizes = Sizes
    },

    {PrevFDI, NewFDI}.


gen_revision(conflict, DocId, _PrevRev, Body, Atts) ->
    crypto:hash(md5, term_to_binary({DocId, Body, Atts}));
gen_revision(delete, DocId, PrevRev, Body, Atts) ->
    gen_revision(update, DocId, PrevRev, Body, Atts);
gen_revision(update, DocId, PrevRev, Body, Atts) ->
    crypto:hash(md5, term_to_binary({DocId, PrevRev, Body, Atts})).


gen_path(conflict, _RevPos, _PrevRevId, Rev, Leaf) ->
    {0, {Rev, Leaf, []}};
gen_path(delete, RevPos, PrevRevId, Rev, Leaf) ->
    gen_path(update, RevPos, PrevRevId, Rev, Leaf);
gen_path(update, RevPos, PrevRevId, Rev, Leaf) ->
    {RevPos, {PrevRevId, ?REV_MISSING, [{Rev, Leaf, []}]}}.


make_doc_summary(Engine, St, DocData) ->
    {_, Ref} = spawn_monitor(fun() ->
        exit({result, Engine:serialize_doc(St, DocData)})
    end),
    receive
        {'DOWN', Ref, _, _, {result, Summary}} ->
            Summary;
        {'DOWN', Ref, _, _, Error} ->
            erlang:error({make_doc_summary_error, Error})
    after 1000 ->
        erlang:error(make_doc_summary_timeout)
    end.


prep_atts(_Engine, _St, []) ->
    [];

prep_atts(Engine, St, [{FileName, Data} | Rest]) ->
    {_, Ref} = spawn_monitor(fun() ->
        {ok, Stream} = Engine:open_write_stream(St, []),
        exit(write_att(Stream, FileName, Data, Data))
    end),
    Att = receive
        {'DOWN', Ref, _, _, {{no_catch, not_supported}, _}} ->
            throw(not_supported);
        {'DOWN', Ref, _, _, Resp} ->
            Resp
        after 5000 ->
            erlang:error(attachment_write_timeout)
    end,
    [Att | prep_atts(Engine, St, Rest)].


write_att(Stream, FileName, OrigData, <<>>) ->
    {StreamEngine, Len, Len, Md5, Md5} = couch_stream:close(Stream),
    couch_util:check_md5(Md5, crypto:hash(md5, OrigData)),
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


db_as_term(Engine, St) ->
    [
        {props, db_props_as_term(Engine, St)},
        {docs, db_docs_as_term(Engine, St)},
        {local_docs, db_local_docs_as_term(Engine, St)},
        {changes, db_changes_as_term(Engine, St)}
    ].


db_props_as_term(Engine, St) ->
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
        {Fun, Engine:Fun(St)}
    end, Props).


db_docs_as_term(Engine, St) ->
    FoldFun = fun(FDI, Acc) -> {ok, [FDI | Acc]} end,
    {ok, FDIs} = Engine:fold_docs(St, FoldFun, [], []),
    lists:reverse(lists:map(fun(FDI) ->
        fdi_to_term(Engine, St, FDI)
    end, FDIs)).


db_local_docs_as_term(Engine, St) ->
    FoldFun = fun(Doc, Acc) -> {ok, [Doc | Acc]} end,
    {ok, LDocs} = Engine:fold_local_docs(St, FoldFun, [], []),
    lists:reverse(LDocs).


db_changes_as_term(Engine, St) ->
    FoldFun = fun(FDI, Acc) -> {ok, [FDI | Acc]} end,
    {ok, Changes} = Engine:fold_changes(St, 0, FoldFun, [], []),
    lists:reverse(lists:map(fun(FDI) ->
        fdi_to_term(Engine, St, FDI)
    end, Changes)).


fdi_to_term(Engine, St, FDI) ->
    #full_doc_info{
        id = DocId,
        rev_tree = OldTree
    } = FDI,
    {NewRevTree, _} = couch_key_tree:mapfold(fun(Rev, Node, Type, Acc) ->
        tree_to_term(Rev, Node, Type, Acc, DocId)
    end, {Engine, St}, OldTree),
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

tree_to_term({Pos, RevId}, #leaf{} = Leaf, leaf, {Engine, St}, DocId) ->
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

    Doc1 = Engine:read_doc_body(St, Doc0),

    Body = if not is_binary(Doc1#doc.body) -> Doc1#doc.body; true ->
        couch_compress:decompress(Doc1#doc.body)
    end,

    Atts1 = if not is_binary(Doc1#doc.atts) -> Doc1#doc.atts; true ->
        couch_compress:decompress(Doc1#doc.atts)
    end,

    StreamSrc = fun(Sp) -> Engine:open_read_stream(St, Sp) end,
    Atts2 = [couch_att:from_disk_term(StreamSrc, Att) || Att <- Atts1],
    Atts = [att_to_term(Att) || Att <- Atts2],

    NewLeaf = Leaf#leaf{
        ptr = Body,
        sizes = #size_info{active = -1, external = -1},
        atts = Atts
    },
    {NewLeaf, {Engine, St}}.


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


compact(Engine, St1, DbPath) ->
    DbName = filename:basename(DbPath),
    {ok, St2, Pid} = Engine:start_compaction(St1, DbName, [], self()),
    Ref = erlang:monitor(process, Pid),

    % Ideally I'd assert that Pid is linked to us
    % at this point but its technically possible
    % that it could have finished compacting by
    % the time we check... Quite the quandry.

    Term = receive
        {'$gen_cast', {compact_done, Engine, Term0}} ->
            Term0;
        {'DOWN', Ref, _, _, Reason} ->
            erlang:error({compactor_died, Reason})
        after 10000 ->
            erlang:error(compactor_timed_out)
    end,

    {ok, St2, DbName, Pid, Term}.


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
        undefined -> config:delete(Section, Key);
        _ -> config:set(Section, Key, Value)
    end,
    [{Section, Key, Orig} | apply_config(Rest)].
