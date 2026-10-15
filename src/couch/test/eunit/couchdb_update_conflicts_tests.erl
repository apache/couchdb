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

-module(couchdb_update_conflicts_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(i2l(I), integer_to_list(I)).
-define(DOC_ID, <<"foobar">>).
-define(LOCAL_DOC_ID, <<"_local/foobar">>).
-define(NUM_CLIENTS, [100, 500, 1000, 2000, 5000, 10000]).
-define(TIMEOUT, 20000).
-define(VDU, ~"function(doc) {if (doc.balrog) {throw({forbidden:'shallnotpass'});}}").
-define(ATT_NAME, ~"att.txt").
-define(ATT_DATA, ~"original").

start() ->
    test_util:start_couch().

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX, overwrite]),
    Doc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, ?DOC_ID},
            {<<"value">>, 0}
        ]}
    ),
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    ok = couch_db:close(Db),
    RevStr = couch_doc:rev_to_str(Rev),
    {DbName, RevStr}.
setup(_) ->
    setup().

teardown({DbName, _}) ->
    ok = couch_server:delete(DbName, []),
    ok.
teardown(_, {DbName, _RevStr}) ->
    teardown({DbName, _RevStr}).

view_indexes_cleanup_test_() ->
    {
        "Update conflicts",
        {
            setup,
            fun start/0,
            fun test_util:stop_couch/1,
            [
                concurrent_updates(),
                bulk_docs_updates()
            ]
        }
    }.

concurrent_updates() ->
    {
        "Concurrent updates",
        {
            foreachx,
            fun setup/1,
            fun teardown/2,
            [
                {NumClients, fun should_concurrently_update_doc/2}
             || NumClients <- ?NUM_CLIENTS
            ]
        }
    }.

bulk_docs_updates() ->
    {
        "Bulk docs updates",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                fun should_bulk_create_delete_doc/1,
                fun should_bulk_create_local_doc/1,
                fun should_ignore_invalid_local_doc/1
            ]
        }
    }.

should_concurrently_update_doc(NumClients, {DbName, InitRev}) ->
    {
        ?i2l(NumClients) ++ " clients",
        {inorder, [
            {"update doc",
                {timeout, ?TIMEOUT div 1000,
                    ?_test(concurrent_doc_update(NumClients, DbName, InitRev))}},
            {"ensure in single leaf", ?_test(ensure_in_single_revision_leaf(DbName))}
        ]}
    }.

should_bulk_create_delete_doc({DbName, InitRev}) ->
    ?_test(bulk_delete_create(DbName, InitRev)).

should_bulk_create_local_doc({DbName, _}) ->
    ?_test(bulk_create_local_doc(DbName)).

should_ignore_invalid_local_doc({DbName, _}) ->
    ?_test(ignore_invalid_local_doc(DbName)).

concurrent_doc_update(NumClients, DbName, InitRev) ->
    Clients = lists:map(
        fun(Value) ->
            ClientDoc = couch_doc:from_json_obj(
                {[
                    {<<"_id">>, ?DOC_ID},
                    {<<"_rev">>, InitRev},
                    {<<"value">>, Value}
                ]}
            ),
            Pid = spawn_client(DbName, ClientDoc),
            {Value, Pid, monitor(process, Pid)}
        end,
        lists:seq(1, NumClients)
    ),

    lists:foreach(fun({_, Pid, _}) -> Pid ! go end, Clients),

    {NumConflicts, SavedValue} = lists:foldl(
        fun({Value, Pid, MonRef}, {AccConflicts, AccValue}) ->
            receive
                {'DOWN', MonRef, process, Pid, {ok, _NewRev}} ->
                    {AccConflicts, Value};
                {'DOWN', MonRef, process, Pid, conflict} ->
                    {AccConflicts + 1, AccValue};
                {'DOWN', MonRef, process, Pid, Error} ->
                    error(
                        {assertion_failed, [
                            {module, ?MODULE},
                            {line, ?LINE},
                            {reason,
                                "Client " ++ ?i2l(Value) ++
                                    " got update error: " ++
                                    couch_util:to_list(Error)}
                        ]}
                    )
            after ?TIMEOUT div 2 ->
                error(
                    {assertion_failed, [
                        {module, ?MODULE},
                        {line, ?LINE},
                        {reason,
                            "Timeout waiting for client " ++
                                ?i2l(Value) ++ " to die"}
                    ]}
                )
            end
        end,
        {0, nil},
        Clients
    ),
    ?assertEqual(NumClients - 1, NumConflicts),

    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, Leaves} = couch_db:open_doc_revs(Db, ?DOC_ID, all, []),
    ok = couch_db:close(Db),
    ?assertEqual(1, length(Leaves)),

    [{ok, Doc2}] = Leaves,
    {JsonDoc} = couch_doc:to_json_obj(Doc2, []),
    ?assertEqual(SavedValue, couch_util:get_value(<<"value">>, JsonDoc)).

ensure_in_single_revision_leaf(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    {ok, Leaves} = couch_db:open_doc_revs(Db, ?DOC_ID, all, []),
    ok = couch_db:close(Db),
    [{ok, Doc}] = Leaves,

    %% FIXME: server restart won't work from test side
    %% stop(ok),
    %% start(),

    {ok, Db2} = couch_db:open_int(DbName, []),
    {ok, Leaves2} = couch_db:open_doc_revs(Db2, ?DOC_ID, all, []),
    ok = couch_db:close(Db2),
    ?assertEqual(1, length(Leaves2)),

    [{ok, Doc2}] = Leaves,
    ?assertEqual(Doc, Doc2).

bulk_delete_create(DbName, InitRev) ->
    {ok, Db} = couch_db:open_int(DbName, []),

    DeletedDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, ?DOC_ID},
            {<<"_rev">>, InitRev},
            {<<"_deleted">>, true}
        ]}
    ),
    NewDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, ?DOC_ID},
            {<<"value">>, 666}
        ]}
    ),

    {ok, Results} = couch_db:update_docs(Db, [DeletedDoc, NewDoc], []),
    ok = couch_db:close(Db),

    ?assertEqual(2, length([ok || {ok, _} <- Results])),
    [{ok, Rev1}, {ok, Rev2}] = Results,

    {ok, Db2} = couch_db:open_int(DbName, []),
    {ok, [{ok, Doc1}]} = couch_db:open_doc_revs(
        Db2, ?DOC_ID, [Rev1], [conflicts, deleted_conflicts]
    ),
    {ok, [{ok, Doc2}]} = couch_db:open_doc_revs(
        Db2, ?DOC_ID, [Rev2], [conflicts, deleted_conflicts]
    ),
    ok = couch_db:close(Db2),

    {Doc1Props} = couch_doc:to_json_obj(Doc1, []),
    {Doc2Props} = couch_doc:to_json_obj(Doc2, []),

    %% Document was deleted
    ?assert(couch_util:get_value(<<"_deleted">>, Doc1Props)),
    %% New document not flagged as deleted
    ?assertEqual(
        undefined,
        couch_util:get_value(
            <<"_deleted">>,
            Doc2Props
        )
    ),
    %% New leaf revision has the right value
    ?assertEqual(
        666,
        couch_util:get_value(
            <<"value">>,
            Doc2Props
        )
    ),
    %% Deleted document has no conflicts
    ?assertEqual(
        undefined,
        couch_util:get_value(
            <<"_conflicts">>,
            Doc1Props
        )
    ),
    %% Deleted document has no deleted conflicts
    ?assertEqual(
        undefined,
        couch_util:get_value(
            <<"_deleted_conflicts">>,
            Doc1Props
        )
    ),
    %% New leaf revision doesn't have conflicts
    ?assertEqual(
        undefined,
        couch_util:get_value(
            <<"_conflicts">>,
            Doc1Props
        )
    ),
    %% New leaf revision doesn't have deleted conflicts
    ?assertEqual(
        undefined,
        couch_util:get_value(
            <<"_deleted_conflicts">>,
            Doc1Props
        )
    ),

    %% Deleted revision has position 2
    ?assertEqual(2, element(1, Rev1)),
    %% New leaf revision has position 3
    ?assertEqual(3, element(1, Rev2)).

bulk_create_local_doc(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),

    LocalDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, ?LOCAL_DOC_ID},
            {<<"_rev">>, <<"0-1">>}
        ]}
    ),

    {ok, Results} = couch_db:update_docs(
        Db,
        [LocalDoc],
        [],
        ?REPLICATED_CHANGES
    ),
    ok = couch_db:close(Db),
    ?assertEqual([], Results),

    {ok, Db2} = couch_db:open_int(DbName, []),
    {ok, LocalDoc1} = couch_db:open_doc_int(Db2, ?LOCAL_DOC_ID, []),
    ok = couch_db:close(Db2),
    ?assertEqual(?LOCAL_DOC_ID, LocalDoc1#doc.id),
    ?assertEqual({0, [<<"2">>]}, LocalDoc1#doc.revs).

ignore_invalid_local_doc(DbName) ->
    {ok, Db} = couch_db:open_int(DbName, []),

    LocalDoc = couch_doc:from_json_obj(
        {[
            {<<"_id">>, ?LOCAL_DOC_ID},
            {<<"_rev">>, <<"0-abcdef">>}
        ]}
    ),

    {ok, Results} = couch_db:update_docs(
        Db,
        [LocalDoc],
        [],
        ?REPLICATED_CHANGES
    ),
    ok = couch_db:close(Db),
    ?assertEqual([], Results),

    {ok, Db2} = couch_db:open_int(DbName, []),
    Result2 = couch_db:open_doc_int(Db2, ?LOCAL_DOC_ID, []),
    ok = couch_db:close(Db2),
    ?assertEqual({not_found, missing}, Result2).

spawn_client(DbName, Doc) ->
    spawn(fun() ->
        {ok, Db} = couch_db:open_int(DbName, []),
        receive
            go -> ok
        end,
        erlang:yield(),
        Result =
            try
                couch_db:update_doc(Db, Doc, [])
            catch
                _:Error ->
                    Error
            end,
        ok = couch_db:close(Db),
        exit(Result)
    end).

% Test replicated changes coming in from sources with odd (non-default) revs
% limits. With a VDU we used to shortcut the prep-and-validate and not properly
% merge updated rev paths.
%
% Tests start with a two root tree that looks like:
%
%   1-a                :  1-a came in from a revs limit = 1 source
%   2-b -> 3-c -> 4-d  :  4-d came from a revs revs limit = 3 source
%
% Then we get 4-d again with a full revpath history of 1-a -> ... 4->d and then
% we have to check that we re-parent teh revision back to 1-a as a single path.

replicated_mismatched_revslimit_test_() ->
    {
        "Replicated revs from mismatched revslimit sources",
        {
            setup,
            fun start/0,
            fun test_util:stop_couch/1,
            {
                foreach,
                fun setup_db/0,
                fun teardown_db/1,
                [
                    ?TDEF_FE(t_no_vdu),
                    ?TDEF_FE(t_with_vdu),
                    ?TDEF_FE(t_no_vdu_keeps_body),
                    ?TDEF_FE(t_no_vdu_internal_node_stays_bodyless),
                    ?TDEF_FE(t_att_stub),
                    ?TDEF_FE(t_att_data),
                    ?TDEF_FE(t_internal_node_stemmed),
                    ?TDEF_FE(t_internal_node_full_path_patches_rev_tree),
                    ?TDEF_FE(t_multiple_revs_path_in_one_request)
                ]
            }
        }
    }.

setup_db() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    DbName.

teardown_db(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]).

t_no_vdu(DbName) ->
    two_roots(DbName),
    update(DbName, [d_4_full()]),
    % leafs returns [{Pos, RevId, Deleted, RevPathLength}]
    ?assertEqual([{4, ~"d", false, 4}], leafs(DbName)),
    update(DbName, [e_5_deleted()]),
    ?assertEqual([{5, ~"e", true, 5}], leafs(DbName)).

t_no_vdu_keeps_body(DbName) ->
    two_roots(DbName),
    % 4-d again with a full path but a different body. The path is patched,
    % the stored body stays.
    update(DbName, [forbid(d_4_full())]),
    ?assertEqual([{4, ~"d", false, 4}], leafs(DbName)),
    ?assertEqual({[{~"val", ~"d"}]}, body(DbName, {4, ~"d"})).

t_no_vdu_internal_node_stays_bodyless(DbName) ->
    update(DbName, [d_4_stemmed()]),
    % 3-c is an internal rev. Its full path patches the rev tree, but it
    % doesn't get a body stored for it.
    update(DbName, [doc(3, [~"c", ~"b", ~"a"], [{~"val", ~"c"}])]),
    ?assertEqual([{4, ~"d", false, 4}], leafs(DbName)),
    ?assertMatch([{{not_found, missing}, {3, ~"c"}}], revs(DbName, [{3, ~"c"}])),
    % 1-a arriving later is an ancestor now, not a second root
    update(DbName, [doc(1, [~"a"], [{~"val", ~"a"}])]),
    ?assertEqual([{4, ~"d", false, 4}], leafs(DbName)).

t_with_vdu(DbName) ->
    vdu(DbName),
    two_roots(DbName),
    % Patch the rev path but keep the old body on disk
    update(DbName, [forbid(d_4_full())]),
    % One rev path
    ?assertEqual([{4, ~"d", false, 4}], leafs(DbName)),
    % Old body
    ?assertEqual({[{~"val", ~"d"}]}, body(DbName, {4, ~"d"})),
    update(DbName, [e_5_deleted()]),
    ?assertEqual([{5, ~"e", true, 5}], leafs(DbName)),
    % New revs are validated, though
    ?assertMatch({ok, [{_, {forbidden, _}}]}, update(DbName, [forbid(f_5_full())])).

t_att_stub(DbName) ->
    two_roots_with_att(DbName),
    Stub = couch_att:new([
        {name, ?ATT_NAME},
        {data, stub},
        {type, ~"application/octet-stream"},
        {revpos, 4}
    ]),
    Doc = d_4_full(),
    ?assertEqual({ok, []}, update(DbName, [Doc#doc{atts = [Stub]}])),
    ?assertEqual([{4, ~"d", false, 4}], leafs(DbName)),
    % Rev path patched but we kept the stored attachment
    ?assertEqual(?ATT_DATA, attbin(DbName, {4, ~"d"})).

t_att_data(DbName) ->
    two_roots_with_att(DbName),
    Att = att(~"replaced"),
    Doc = d_4_full(),
    update(DbName, [Doc#doc{atts = [Att]}]),
    ?assertEqual([{4, ~"d", false, 4}], leafs(DbName)),
    % Rev path patched but we kept the stored attachment
    ?assertEqual(?ATT_DATA, attbin(DbName, {4, ~"d"})).

t_internal_node_stemmed(DbName) ->
    vdu(DbName),
    two_roots(DbName),
    % 3-c is an internal rev. Sending its short path doesn't change anything
    Doc = forbid(doc(3, [~"c", ~"b"], [{~"val", ~"c"}])),
    update(DbName, [Doc]),
    % Sanity check
    ?assertMatch([{{not_found, missing}, {3, ~"c"}}], revs(DbName, [{3, ~"c"}])),
    % Rev tree doesn't change
    ?assertEqual([{1, ~"a", false, 1}, {4, ~"d", false, 3}], leafs(DbName)).

t_internal_node_full_path_patches_rev_tree(DbName) ->
    vdu(DbName),
    update(DbName, [d_4_stemmed()]),
    % 3-c is an internal rev. We send it with a full path it should patch up
    % the revs tree. Doesn't touch any leafs though.
    Doc = forbid(doc(3, [~"c", ~"b", ~"a"], [{~"val", ~"c"}])),
    update(DbName, [Doc]),
    ?assertEqual([{4, ~"d", false, 4}], leafs(DbName)),
    % Sanity check
    ?assertMatch([{{not_found, missing}, {3, ~"c"}}], revs(DbName, [{3, ~"c"}])).

t_multiple_revs_path_in_one_request(DbName) ->
    vdu(DbName),
    two_roots(DbName),
    % 5-e is internal so it doesn't get validated. 6-f body gets stored. Both
    % are not long enought to re-parent with 1-a
    Doc1 = forbid(doc(5, [~"e", ~"d", ~"c"], [{~"val", ~"e"}])),
    Doc2 = doc(6, [~"f", ~"e", ~"d", ~"c"], [{~"val", ~"f"}]),
    ?assertEqual({ok, []}, update(DbName, [Doc1, Doc2])),
    ?assertEqual([{1, ~"a", false, 1}, {6, ~"f", false, 5}], leafs(DbName)),
    ?assertEqual({[{~"val", ~"f"}]}, body(DbName, {6, ~"f"})),
    % 5-e internal never go through
    ?assertMatch([{{not_found, missing}, {5, ~"e"}}], revs(DbName, [{5, ~"e"}])).

two_roots(DbName) ->
    update(DbName, [doc(1, [~"a"], [{~"val", ~"a"}])]),
    update(DbName, [d_4_stemmed()]),
    ?assertEqual([{1, ~"a", false, 1}, {4, ~"d", false, 3}], leafs(DbName)).

two_roots_with_att(DbName) ->
    update(DbName, [doc(1, [~"a"], [{~"val", ~"a"}])]),
    Doc = d_4_stemmed(),
    update(DbName, [Doc#doc{atts = [att(?ATT_DATA)]}]),
    ?assertEqual([{1, ~"a", false, 1}, {4, ~"d", false, 3}], leafs(DbName)).

d_4_stemmed() ->
    doc(4, [~"d", ~"c", ~"b"], [{~"val", ~"d"}]).

d_4_full() ->
    doc(4, [~"d", ~"c", ~"b", ~"a"], [{~"val", ~"d"}]).

e_5_deleted() ->
    Doc = doc(5, [~"e", ~"d", ~"c"], []),
    Doc#doc{deleted = true}.

f_5_full() ->
    doc(5, [~"f", ~"d", ~"c", ~"b", ~"a"], [{~"val", ~"f"}]).

% This one will be rejected by the VDU
forbid(#doc{body = {Props}} = Doc) ->
    Doc#doc{body = {Props ++ [{~"balrog", true}]}}.

doc(Pos, Revs, Props) when is_list(Props) ->
    #doc{id = ?DOC_ID, revs = {Pos, Revs}, body = {Props}}.

att(Data) ->
    couch_att:new([
        {name, ?ATT_NAME},
        {data, Data},
        {type, ~"application/octet-stream"},
        {att_len, byte_size(Data)},
        {disk_len, byte_size(Data)},
        {revpos, 4}
    ]).

body(DbName, Rev) ->
    [{ok, #doc{body = Body}}] = revs(DbName, [Rev]),
    Body.

attbin(DbName, Rev) ->
    [{ok, #doc{atts = [Att]}}] = revs(DbName, [Rev]),
    couch_att:to_binary(Att).

revs(DbName, Revs) ->
    couch_util:with_db(DbName, fun(Db) ->
        {ok, Res} = couch_db:open_doc_revs(Db, ?DOC_ID, Revs, []),
        Res
    end).

vdu(DbName) ->
    DDoc = #doc{id = ~"_design/nobalrog", body = {[{~"validate_doc_update", ?VDU}]}},
    couch_util:with_db(DbName, fun(Db) ->
        {ok, _} = couch_db:update_doc(Db, DDoc, [])
    end).

update(DbName, Docs) ->
    couch_util:with_db(DbName, fun(Db) ->
        couch_db:update_docs(Db, Docs, [], ?REPLICATED_CHANGES)
    end).

% Get a list of {Pos, RevId, Deleted, RevPathLength} for all leafs
leafs(DbName) ->
    couch_util:with_db(DbName, fun(Db) ->
        {ok, Res} = couch_db:open_doc_revs(Db, ?DOC_ID, all, []),
        RList = [{P, R, D, length(Rs)} || {ok, #doc{revs = {P, [R | _] = Rs}, deleted = D}} <- Res],
        lists:sort(RList)
    end).
