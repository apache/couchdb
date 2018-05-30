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

-module(fabric_rpc_purge_tests).


-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(TDEF(A), {A, fun A/1}).

% TODO: Add tests:
%         - filter some updates
%         - allow for an update that was filtered by a node
%         - ignore lagging nodes

main_test_() ->
    {
        setup,
        spawn,
        fun setup_all/0,
        fun teardown_all/1,
        [
            {
                foreach,
                fun setup_no_purge/0,
                fun teardown_no_purge/1,
                lists:map(fun wrap/1, [
                    ?TDEF(t_no_purge_no_filter)
                ])
            },
            {
                foreach,
                fun setup_single_purge/0,
                fun teardown_single_purge/1,
                lists:map(fun wrap/1, [
                    ?TDEF(t_filter),
                    ?TDEF(t_filter_unknown_node),
                    ?TDEF(t_no_filter_old_node),
                    ?TDEF(t_no_filter_different_node),
                    ?TDEF(t_no_filter_after_repl)
                ])
            },
            {
                foreach,
                fun setup_multi_purge/0,
                fun teardown_multi_purge/1,
                lists:map(fun wrap/1, [
                    ?TDEF(t_filter),
                    ?TDEF(t_filter_unknown_node),
                    ?TDEF(t_no_filter_old_node),
                    ?TDEF(t_no_filter_different_node),
                    ?TDEF(t_no_filter_after_repl)
                ])
            }
        ]
    }.


setup_all() ->
    test_util:start_couch().


teardown_all(Ctx) ->
    test_util:stop_couch(Ctx).


setup_no_purge() ->
    {ok, Db} = create_db(),
    populate_db(Db),
    couch_db:name(Db).


teardown_no_purge(DbName) ->
    ok = couch_server:delete(DbName, []).


setup_single_purge() ->
    DbName = setup_no_purge(),
    DocId = <<"0003">>,
    {ok, OldDoc} = open_doc(DbName, DocId),
    purge_doc(DbName, DocId),
    {DbName, DocId, OldDoc, 1}.


teardown_single_purge({DbName, _, _, _}) ->
    teardown_no_purge(DbName).


setup_multi_purge() ->
    DbName = setup_no_purge(),
    DocId = <<"0003">>,
    {ok, OldDoc} = open_doc(DbName, DocId),
    lists:foreach(fun(I) ->
        PDocId = iolist_to_binary(io_lib:format("~4..0b", [I])),
        purge_doc(DbName, PDocId)
    end, lists:seq(1, 5)),
    {DbName, DocId, OldDoc, 3}.


teardown_multi_purge(Ctx) ->
    teardown_single_purge(Ctx).


t_no_purge_no_filter(DbName) ->
    DocId = <<"0003">>,

    {ok, OldDoc} = open_doc(DbName, DocId),
    NewDoc = create_update(OldDoc, 2),

    rpc_update_doc(DbName, NewDoc),

    {ok, CurrDoc} = open_doc(DbName, DocId),
    ?assert(CurrDoc /= OldDoc),
    ?assert(CurrDoc == NewDoc).


t_filter({DbName, DocId, OldDoc, _PSeq}) ->
    ?assertEqual({not_found, missing}, open_doc(DbName, DocId)),
    create_purge_checkpoint(DbName, 0),

    rpc_update_doc(DbName, OldDoc),

    ?assertEqual({not_found, missing}, open_doc(DbName, DocId)).


t_filter_unknown_node({DbName, DocId, OldDoc, _PSeq}) ->
    % Unknown nodes are assumed to start at PurgeSeq = 0
    ?assertEqual({not_found, missing}, open_doc(DbName, DocId)),
    create_purge_checkpoint(DbName, 0),

    {Pos, [Rev | _]} = OldDoc#doc.revs,
    RROpt = {read_repair, [{'blargh@127.0.0.1', [{Pos, Rev}]}]},
    rpc_update_doc(DbName, OldDoc, [RROpt]),

    ?assertEqual({not_found, missing}, open_doc(DbName, DocId)).


t_no_filter_old_node({DbName, DocId, OldDoc, PSeq}) ->
    ?assertEqual({not_found, missing}, open_doc(DbName, DocId)),
    create_purge_checkpoint(DbName, PSeq),

    % The random UUID is to generate a badarg exception when
    % we try and convert it to an existing atom.
    create_purge_checkpoint(DbName, 0, couch_uuids:random()),

    rpc_update_doc(DbName, OldDoc),

    ?assertEqual({ok, OldDoc}, open_doc(DbName, DocId)).


t_no_filter_different_node({DbName, DocId, OldDoc, PSeq}) ->
    ?assertEqual({not_found, missing}, open_doc(DbName, DocId)),
    create_purge_checkpoint(DbName, PSeq),

    % Create a valid purge for a different node
    TgtNode = list_to_binary(atom_to_list('notfoo@127.0.0.1')),
    create_purge_checkpoint(DbName, 0, TgtNode),

    rpc_update_doc(DbName, OldDoc),

    ?assertEqual({ok, OldDoc}, open_doc(DbName, DocId)).


t_no_filter_after_repl({DbName, DocId, OldDoc, PSeq}) ->
    ?assertEqual({not_found, missing}, open_doc(DbName, DocId)),
    create_purge_checkpoint(DbName, PSeq),

    rpc_update_doc(DbName, OldDoc),

    ?assertEqual({ok, OldDoc}, open_doc(DbName, DocId)).


wrap({Name, Fun}) ->
    fun(Arg) ->
        {timeout, 60, {atom_to_list(Name), fun() ->
            process_flag(trap_exit, true),
            Fun(Arg)
        end}}
    end.


create_db() ->
    DbName = ?tempdb(),
    couch_db:create(DbName, [?ADMIN_CTX]).


populate_db(Db) ->
    Docs = lists:map(fun(Idx) ->
        DocId = lists:flatten(io_lib:format("~4..0b", [Idx])),
        #doc{
            id = list_to_binary(DocId),
            body = {[{<<"int">>, Idx}, {<<"vsn">>, 2}]}
        }
    end, lists:seq(1, 100)),
    {ok, _} = couch_db:update_docs(Db, Docs).


open_doc(DbName, DocId) ->
    couch_util:with_db(DbName, fun(Db) ->
        couch_db:open_doc(Db, DocId, [])
    end).


create_update(Doc, NewVsn) ->
    #doc{
        id = DocId,
        revs = {Pos, [Rev | _] = Revs},
        body = {Props}
    } = Doc,
    NewProps = lists:keyreplace(<<"vsn">>, 1, Props, {<<"vsn">>, NewVsn}),
    NewRev = crypto:hash(md5, term_to_binary({DocId, Rev, {NewProps}})),
    Doc#doc{
        revs = {Pos + 1, [NewRev | Revs]},
        body = {NewProps}
    }.


purge_doc(DbName, DocId) ->
    {ok, Doc} = open_doc(DbName, DocId),
    {Pos, [Rev | _]} = Doc#doc.revs,
    PInfo = {couch_uuids:random(), DocId, [{Pos, Rev}]},
    Resp = couch_util:with_db(DbName, fun(Db) ->
        couch_db:purge_docs(Db, [PInfo], [])
    end),
    ?assertEqual({ok, [{ok, [{Pos, Rev}]}]}, Resp).


create_purge_checkpoint(DbName, PurgeSeq) ->
    create_purge_checkpoint(DbName, PurgeSeq, tgt_node_bin()).


create_purge_checkpoint(DbName, PurgeSeq, TgtNode) when is_binary(TgtNode) ->
    Resp = couch_util:with_db(DbName, fun(Db) ->
        SrcUUID = couch_db:get_uuid(Db),
        TgtUUID = couch_uuids:random(),
        CPDoc = #doc{
            id = mem3_rep:make_purge_id(SrcUUID, TgtUUID),
            body = {[
                {<<"target_node">>, TgtNode},
                {<<"purge_seq">>, PurgeSeq}
            ]}
        },
        couch_db:update_docs(Db, [CPDoc], [])
    end),
    ?assertMatch({ok, [_]}, Resp).


rpc_update_doc(DbName, Doc) ->
    {Pos, [Rev | _]} = Doc#doc.revs,
    RROpt = {read_repair, [{tgt_node(), [{Pos, Rev}]}]},
    rpc_update_doc(DbName, Doc, [RROpt]).


rpc_update_doc(DbName, Doc, Opts) ->
    Ref = erlang:make_ref(),
    put(rexi_from, {self(), Ref}),
    fabric_rpc:update_docs(DbName, [Doc], Opts),
    Reply = test_util:wait(fun() ->
        receive
            {Ref, Reply} ->
                Reply
        after 0 ->
            wait
        end
    end),
    ?assertEqual({ok, []}, Reply).


tgt_node() ->
    'foo@127.0.0.1'.


tgt_node_bin() ->
    iolist_to_binary(atom_to_list(tgt_node())).