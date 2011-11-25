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

% Verify that compacting databases that are being used as the source or
% target of a replication doesn't affect the replication and that the
% replication doesn't hold their reference counters forever.

-record(user_ctx, {
    name = null,
    roles = [],
    handler
}).

-record(changes_args, {
    feed = "normal",
    dir = fwd,
    since = 0,
    limit = 1000000000000000,
    style = main_only,
    heartbeat,
    timeout,
    filter = "",
    filter_fun,
    filter_args = [],
    include_docs = false,
    conflicts = false,
    db_open_options = []
}).

-record(row, {
    id,
    seq,
    deleted = false
}).


test_db_name() -> <<"couch_test_changes">>.


main(_) ->
    test_util:init_code_path(),

    etap:plan(43),
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

    test_by_doc_ids(),
    test_by_doc_ids_with_since(),
    test_by_doc_ids_continuous(),
    test_design_docs_only(),
    test_heartbeat(),

    couch_server_sup:stop(),
    ok.


test_by_doc_ids() ->
    {ok, Db} = create_db(test_db_name()),

    {ok, _Rev1} = save_doc(Db, {[{<<"_id">>, <<"doc1">>}]}),
    {ok, _Rev2} = save_doc(Db, {[{<<"_id">>, <<"doc2">>}]}),
    {ok, Rev3} = save_doc(Db, {[{<<"_id">>, <<"doc3">>}]}),
    {ok, _Rev4} = save_doc(Db, {[{<<"_id">>, <<"doc4">>}]}),
    {ok, _Rev5} = save_doc(Db, {[{<<"_id">>, <<"doc5">>}]}),
    {ok, _Rev3_2} = save_doc(Db, {[{<<"_id">>, <<"doc3">>}, {<<"_rev">>, Rev3}]}),
    {ok, _Rev6} = save_doc(Db, {[{<<"_id">>, <<"doc6">>}]}),
    {ok, _Rev7} = save_doc(Db, {[{<<"_id">>, <<"doc7">>}]}),
    {ok, _Rev8} = save_doc(Db, {[{<<"_id">>, <<"doc8">>}]}),

    etap:diag("Folding changes in ascending order with _doc_ids filter"),
    ChangesArgs = #changes_args{
        filter = "_doc_ids"
    },
    DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
    Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
    Consumer = spawn_consumer(test_db_name(), ChangesArgs, Req),

    {Rows, LastSeq} = wait_finished(Consumer),
    {ok, Db2} = couch_db:open_int(test_db_name(), []),
    UpSeq = couch_db:get_update_seq(Db2),
    couch_db:close(Db2),
    etap:is(length(Rows), 2, "Received 2 changes rows"),
    etap:is(LastSeq, UpSeq, "LastSeq is same as database update seq number"),
    [#row{seq = Seq1, id = Id1}, #row{seq = Seq2, id = Id2}] = Rows,
    etap:is(Id1, <<"doc4">>, "First row is for doc doc4"),
    etap:is(Seq1, 4, "First row has seq 4"),
    etap:is(Id2, <<"doc3">>, "Second row is for doc doc3"),
    etap:is(Seq2, 6, "Second row has seq 6"),

    stop(Consumer),
    etap:diag("Folding changes in descending order with _doc_ids filter"),
    ChangesArgs2 = #changes_args{
        filter = "_doc_ids",
        dir = rev
    },
    Consumer2 = spawn_consumer(test_db_name(), ChangesArgs2, Req),

    {Rows2, LastSeq2} = wait_finished(Consumer2),
    etap:is(length(Rows2), 2, "Received 2 changes rows"),
    etap:is(LastSeq2, 4, "LastSeq is 4"),
    [#row{seq = Seq1_2, id = Id1_2}, #row{seq = Seq2_2, id = Id2_2}] = Rows2,
    etap:is(Id1_2, <<"doc3">>, "First row is for doc doc3"),
    etap:is(Seq1_2, 6, "First row has seq 4"),
    etap:is(Id2_2, <<"doc4">>, "Second row is for doc doc4"),
    etap:is(Seq2_2, 4, "Second row has seq 6"),

    stop(Consumer2),
    delete_db(Db).


test_by_doc_ids_with_since() ->
    {ok, Db} = create_db(test_db_name()),

    {ok, _Rev1} = save_doc(Db, {[{<<"_id">>, <<"doc1">>}]}),
    {ok, _Rev2} = save_doc(Db, {[{<<"_id">>, <<"doc2">>}]}),
    {ok, Rev3} = save_doc(Db, {[{<<"_id">>, <<"doc3">>}]}),
    {ok, _Rev4} = save_doc(Db, {[{<<"_id">>, <<"doc4">>}]}),
    {ok, _Rev5} = save_doc(Db, {[{<<"_id">>, <<"doc5">>}]}),
    {ok, Rev3_2} = save_doc(Db, {[{<<"_id">>, <<"doc3">>}, {<<"_rev">>, Rev3}]}),
    {ok, _Rev6} = save_doc(Db, {[{<<"_id">>, <<"doc6">>}]}),
    {ok, _Rev7} = save_doc(Db, {[{<<"_id">>, <<"doc7">>}]}),
    {ok, _Rev8} = save_doc(Db, {[{<<"_id">>, <<"doc8">>}]}),

    ChangesArgs = #changes_args{
        filter = "_doc_ids",
        since = 5
    },
    DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
    Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
    Consumer = spawn_consumer(test_db_name(), ChangesArgs, Req),

    {Rows, LastSeq} = wait_finished(Consumer),
    {ok, Db2} = couch_db:open_int(test_db_name(), []),
    UpSeq = couch_db:get_update_seq(Db2),
    couch_db:close(Db2),
    etap:is(LastSeq, UpSeq, "LastSeq is same as database update seq number"),
    etap:is(length(Rows), 1, "Received 1 changes rows"),
    [#row{seq = Seq1, id = Id1}] = Rows,
    etap:is(Id1, <<"doc3">>, "First row is for doc doc3"),
    etap:is(Seq1, 6, "First row has seq 6"),

    stop(Consumer),

    ChangesArgs2 = #changes_args{
        filter = "_doc_ids",
        since = 6
    },
    Consumer2 = spawn_consumer(test_db_name(), ChangesArgs2, Req),

    {Rows2, LastSeq2} = wait_finished(Consumer2),
    {ok, Db3} = couch_db:open_int(test_db_name(), []),
    UpSeq2 = couch_db:get_update_seq(Db3),
    couch_db:close(Db3),
    etap:is(LastSeq2, UpSeq2, "LastSeq is same as database update seq number"),
    etap:is(length(Rows2), 0, "Received 0 change rows"),

    stop(Consumer2),

    {ok, _Rev3_3} = save_doc(
        Db,
        {[{<<"_id">>, <<"doc3">>}, {<<"_deleted">>, true}, {<<"_rev">>, Rev3_2}]}),

    ChangesArgs3 = #changes_args{
        filter = "_doc_ids",
        since = 9
    },
    Consumer3 = spawn_consumer(test_db_name(), ChangesArgs3, Req),

    {Rows3, LastSeq3} = wait_finished(Consumer3),
    {ok, Db4} = couch_db:open_int(test_db_name(), []),
    UpSeq3 = couch_db:get_update_seq(Db4),
    couch_db:close(Db4),
    etap:is(LastSeq3, UpSeq3, "LastSeq is same as database update seq number"),
    etap:is(length(Rows3), 1, "Received 1 changes rows"),
    etap:is(
        [#row{seq = LastSeq3, id = <<"doc3">>, deleted = true}],
        Rows3,
        "Received row with doc3 deleted"),

    stop(Consumer3),

    delete_db(Db).


test_by_doc_ids_continuous() ->
    {ok, Db} = create_db(test_db_name()),

    {ok, _Rev1} = save_doc(Db, {[{<<"_id">>, <<"doc1">>}]}),
    {ok, _Rev2} = save_doc(Db, {[{<<"_id">>, <<"doc2">>}]}),
    {ok, Rev3} = save_doc(Db, {[{<<"_id">>, <<"doc3">>}]}),
    {ok, Rev4} = save_doc(Db, {[{<<"_id">>, <<"doc4">>}]}),
    {ok, _Rev5} = save_doc(Db, {[{<<"_id">>, <<"doc5">>}]}),
    {ok, Rev3_2} = save_doc(Db, {[{<<"_id">>, <<"doc3">>}, {<<"_rev">>, Rev3}]}),
    {ok, _Rev6} = save_doc(Db, {[{<<"_id">>, <<"doc6">>}]}),
    {ok, _Rev7} = save_doc(Db, {[{<<"_id">>, <<"doc7">>}]}),
    {ok, _Rev8} = save_doc(Db, {[{<<"_id">>, <<"doc8">>}]}),

    ChangesArgs = #changes_args{
        filter = "_doc_ids",
        feed = "continuous"
    },
    DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
    Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
    Consumer = spawn_consumer(test_db_name(), ChangesArgs, Req),

    pause(Consumer),
    Rows = get_rows(Consumer),

    etap:is(length(Rows), 2, "Received 2 changes rows"),
    [#row{seq = Seq1, id = Id1}, #row{seq = Seq2, id = Id2}] = Rows,
    etap:is(Id1, <<"doc4">>, "First row is for doc doc4"),
    etap:is(Seq1, 4, "First row has seq 4"),
    etap:is(Id2, <<"doc3">>, "Second row is for doc doc3"),
    etap:is(Seq2, 6, "Second row has seq 6"),

    clear_rows(Consumer),
    {ok, _Rev9} = save_doc(Db, {[{<<"_id">>, <<"doc9">>}]}),
    {ok, _Rev10} = save_doc(Db, {[{<<"_id">>, <<"doc10">>}]}),
    unpause(Consumer),
    pause(Consumer),
    etap:is(get_rows(Consumer), [], "No new rows"),

    {ok, Rev4_2} = save_doc(Db, {[{<<"_id">>, <<"doc4">>}, {<<"_rev">>, Rev4}]}),
    {ok, _Rev11} = save_doc(Db, {[{<<"_id">>, <<"doc11">>}]}),
    {ok, _Rev4_3} = save_doc(Db, {[{<<"_id">>, <<"doc4">>}, {<<"_rev">>, Rev4_2}]}),
    {ok, _Rev12} = save_doc(Db, {[{<<"_id">>, <<"doc12">>}]}),
    {ok, Rev3_3} = save_doc(Db, {[{<<"_id">>, <<"doc3">>}, {<<"_rev">>, Rev3_2}]}),
    unpause(Consumer),
    pause(Consumer),

    NewRows = get_rows(Consumer),
    etap:is(length(NewRows), 2, "Received 2 new rows"),
    [Row14, Row16] = NewRows,
    etap:is(Row14#row.seq, 14, "First row has seq 14"),
    etap:is(Row14#row.id, <<"doc4">>, "First row is for doc doc4"),
    etap:is(Row16#row.seq, 16, "Second row has seq 16"),
    etap:is(Row16#row.id, <<"doc3">>, "Second row is for doc doc3"),

    clear_rows(Consumer),
    {ok, _Rev3_4} = save_doc(Db, {[{<<"_id">>, <<"doc3">>}, {<<"_rev">>, Rev3_3}]}),
    unpause(Consumer),
    pause(Consumer),
    etap:is(get_rows(Consumer), [#row{seq = 17, id = <<"doc3">>}],
        "Got row for seq 17, doc doc3"),

    unpause(Consumer),
    stop(Consumer),
    delete_db(Db).


test_design_docs_only() ->
    {ok, Db} = create_db(test_db_name()),

    {ok, _Rev1} = save_doc(Db, {[{<<"_id">>, <<"doc1">>}]}),
    {ok, _Rev2} = save_doc(Db, {[{<<"_id">>, <<"doc2">>}]}),
    {ok, Rev3} = save_doc(Db, {[{<<"_id">>, <<"_design/foo">>}]}),

    ChangesArgs = #changes_args{
        filter = "_design"
    },
    Consumer = spawn_consumer(test_db_name(), ChangesArgs, {json_req, null}),

    {Rows, LastSeq} = wait_finished(Consumer),
    {ok, Db2} = couch_db:open_int(test_db_name(), []),
    UpSeq = couch_db:get_update_seq(Db2),
    couch_db:close(Db2),

    etap:is(LastSeq, UpSeq, "LastSeq is same as database update seq number"),
    etap:is(length(Rows), 1, "Received 1 changes rows"),
    etap:is(Rows, [#row{seq = 3, id = <<"_design/foo">>}], "Received row with ddoc"),

    stop(Consumer),

    {ok, Db3} = couch_db:open_int(
        test_db_name(), [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}]),
    {ok, _Rev3_2} = save_doc(
        Db3,
        {[{<<"_id">>, <<"_design/foo">>}, {<<"_rev">>, Rev3},
            {<<"_deleted">>, true}]}),

    Consumer2 = spawn_consumer(test_db_name(), ChangesArgs, {json_req, null}),

    {Rows2, LastSeq2} = wait_finished(Consumer2),
    UpSeq2 = UpSeq + 1,
    couch_db:close(Db3),

    etap:is(LastSeq2, UpSeq2, "LastSeq is same as database update seq number"),
    etap:is(length(Rows2), 1, "Received 1 changes rows"),
    etap:is(
        Rows2,
        [#row{seq = 4, id = <<"_design/foo">>, deleted = true}],
        "Received row with deleted ddoc"),

    stop(Consumer2),
    delete_db(Db).

test_heartbeat() ->
    {ok, Db} = create_db(test_db_name()),

    {ok, Rev3} = save_doc(Db, {[
        {<<"_id">>, <<"_design/foo">>},
        {<<"language">>, <<"javascript">>},
            {<<"filters">>, {[
                {<<"foo">>, <<"function(doc) { if ((doc._id == 'doc10') ||
                                                  (doc._id == 'doc11') ||
                                                  (doc._id == 'doc12')) {
                                                return true;
                                               } else {
                                                  return false;
                                               }}">>
            }]}}
    ]}),

    ChangesArgs = #changes_args{
        filter = "foo/foo",
        feed = "continuous",
        timeout = 10000,
        heartbeat = 1000
    },
    Consumer = spawn_consumer(test_db_name(), ChangesArgs, {json_req, null}),

    {ok, _Rev1} = save_doc(Db, {[{<<"_id">>, <<"doc1">>}]}),
    timer:sleep(200),
    {ok, _Rev2} = save_doc(Db, {[{<<"_id">>, <<"doc2">>}]}),
    timer:sleep(200),
    {ok, _Rev3} = save_doc(Db, {[{<<"_id">>, <<"doc3">>}]}),
    timer:sleep(200),
    {ok, _Rev4} = save_doc(Db, {[{<<"_id">>, <<"doc4">>}]}),
    timer:sleep(200),
    {ok, _Rev5} = save_doc(Db, {[{<<"_id">>, <<"doc5">>}]}),
    timer:sleep(200),
    {ok, _Rev6} = save_doc(Db, {[{<<"_id">>, <<"doc6">>}]}),
    timer:sleep(200),
    {ok, _Rev7} = save_doc(Db, {[{<<"_id">>, <<"doc7">>}]}),
    timer:sleep(200),
    {ok, _Rev8} = save_doc(Db, {[{<<"_id">>, <<"doc8">>}]}),
    timer:sleep(200),
    {ok, _Rev9} = save_doc(Db, {[{<<"_id">>, <<"doc9">>}]}),
    Heartbeats = get_heartbeats(Consumer),
    etap:is(Heartbeats, 2, "Received 2 heartbeats now"),
    {ok, _Rev10} = save_doc(Db, {[{<<"_id">>, <<"doc10">>}]}),
    timer:sleep(200),
    {ok, _Rev11} = save_doc(Db, {[{<<"_id">>, <<"doc11">>}]}),
    timer:sleep(200),
    {ok, _Rev12} = save_doc(Db, {[{<<"_id">>, <<"doc12">>}]}),
    Heartbeats2 = get_heartbeats(Consumer),
    etap:is(Heartbeats2, 3, "Received 3 heartbeats now"),
    Rows = get_rows(Consumer),
    etap:is(length(Rows), 3, "Received 3 changes rows"),

    {ok, _Rev13} = save_doc(Db, {[{<<"_id">>, <<"doc13">>}]}),
    timer:sleep(200),
    {ok, _Rev14} = save_doc(Db, {[{<<"_id">>, <<"doc14">>}]}),
    timer:sleep(200),
    Heartbeats3 = get_heartbeats(Consumer),
    etap:is(Heartbeats3, 6, "Received 6 heartbeats now"),
    stop(Consumer),
    couch_db:close(Db),
    delete_db(Db).


save_doc(Db, Json) ->
    Doc = couch_doc:from_json_obj(Json),
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    {ok, couch_doc:rev_to_str(Rev)}.


get_rows(Consumer) ->
    Ref = make_ref(),
    Consumer ! {get_rows, Ref},
    receive
    {rows, Ref, Rows} ->
        Rows
    after 3000 ->
        etap:bail("Timeout getting rows from consumer")
    end.

get_heartbeats(Consumer) ->
    Ref = make_ref(),
    Consumer ! {get_heartbeats, Ref},
    receive
    {hearthbeats, Ref, HeartBeats} ->
        HeartBeats
    after 3000 ->
        etap:bail("Timeout getting heartbeats from consumer")
    end.


clear_rows(Consumer) ->
    Ref = make_ref(),
    Consumer ! {reset, Ref},
    receive
    {ok, Ref} ->
        ok
    after 3000 ->
        etap:bail("Timeout clearing consumer rows")
    end.


stop(Consumer) ->
    Ref = make_ref(),
    Consumer ! {stop, Ref},
    receive
    {ok, Ref} ->
        ok
    after 3000 ->
        etap:bail("Timeout stopping consumer")
    end.


pause(Consumer) ->
    Ref = make_ref(),
    Consumer ! {pause, Ref},
    receive
    {paused, Ref} ->
        ok
    after 3000 ->
        etap:bail("Timeout pausing consumer")
    end.


unpause(Consumer) ->
    Ref = make_ref(),
    Consumer ! {continue, Ref},
    receive
    {ok, Ref} ->
        ok
    after 3000 ->
        etap:bail("Timeout unpausing consumer")
    end.


wait_finished(_Consumer) ->
    receive
    {consumer_finished, Rows, LastSeq} ->
        {Rows, LastSeq}
    after 30000 ->
        etap:bail("Timeout waiting for consumer to finish")
    end.


spawn_consumer(DbName, ChangesArgs0, Req) ->
    Parent = self(),
    spawn(fun() ->
        put(heartbeat_count, 0),
        Callback = fun({change, {Change}, _}, _, Acc) ->
            Id = couch_util:get_value(<<"id">>, Change),
            Seq = couch_util:get_value(<<"seq">>, Change),
            Del = couch_util:get_value(<<"deleted">>, Change, false),
            [#row{id = Id, seq = Seq, deleted = Del} | Acc];
        ({stop, LastSeq}, _, Acc) ->
            Parent ! {consumer_finished, lists:reverse(Acc), LastSeq},
            stop_loop(Parent, Acc);
        (timeout, _, Acc) ->
            put(heartbeat_count, get(heartbeat_count) + 1),
            maybe_pause(Parent, Acc);
        (_, _, Acc) ->
            maybe_pause(Parent, Acc)
        end,
        {ok, Db} = couch_db:open_int(DbName, []),
        ChangesArgs = case (ChangesArgs0#changes_args.timeout =:= undefined)
            andalso (ChangesArgs0#changes_args.heartbeat =:= undefined) of
        true ->
            ChangesArgs0#changes_args{timeout = 10, heartbeat = 10};
        false ->
            ChangesArgs0
        end,
        FeedFun = couch_changes:handle_changes(ChangesArgs, Req, Db),
        try
            FeedFun({Callback, []})
        catch throw:{stop, _} ->
            ok
        end,
        catch couch_db:close(Db)
    end).


maybe_pause(Parent, Acc) ->
    receive
    {get_rows, Ref} ->
        Parent ! {rows, Ref, lists:reverse(Acc)},
        maybe_pause(Parent, Acc);
    {get_heartbeats, Ref} ->
        Parent ! {hearthbeats, Ref, get(heartbeat_count)},
        maybe_pause(Parent, Acc);
    {reset, Ref} ->
        Parent ! {ok, Ref},
        maybe_pause(Parent, []);
    {pause, Ref} ->
        Parent ! {paused, Ref},
        pause_loop(Parent, Acc);
    {stop, Ref} ->
        Parent ! {ok, Ref},
        throw({stop, Acc})
    after 0 ->
        Acc
    end.


pause_loop(Parent, Acc) ->
    receive
    {stop, Ref} ->
        Parent ! {ok, Ref},
        throw({stop, Acc});
    {reset, Ref} ->
        Parent ! {ok, Ref},
        pause_loop(Parent, []);
    {continue, Ref} ->
        Parent ! {ok, Ref},
        Acc;
    {get_rows, Ref} ->
        Parent ! {rows, Ref, lists:reverse(Acc)},
        pause_loop(Parent, Acc)
    end.


stop_loop(Parent, Acc) ->
    receive
    {get_rows, Ref} ->
        Parent ! {rows, Ref, lists:reverse(Acc)},
        stop_loop(Parent, Acc);
    {stop, Ref} ->
        Parent ! {ok, Ref},
        Acc
    end.


create_db(DbName) ->
    couch_db:create(
        DbName,
        [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}, overwrite]).


delete_db(Db) ->
    ok = couch_server:delete(
        couch_db:name(Db), [{user_ctx, #user_ctx{roles = [<<"_admin">>]}}]).
