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

-module(couch_changes_tests).

-include("couch_eunit.hrl").
-include_lib("couchdb/couch_db.hrl").

-define(ADMIN_USER, {user_ctx, #user_ctx{roles = [<<"_admin">>]}}).
-define(TIMEOUT, 3000).
-define(TEST_TIMEOUT, 10000).

-record(row, {
    id,
    seq,
    deleted = false
}).


start() ->
    {ok, Pid} = couch_server_sup:start_link(?CONFIG_CHAIN),
    Pid.

stop(Pid) ->
    erlang:monitor(process, Pid),
    couch_server_sup:stop(),
    receive
        {'DOWN', _, _, Pid, _} ->
            ok
    after ?TIMEOUT ->
        throw({timeout, server_stop})
    end.

setup() ->
    DbName = ?tempdb(),
    {ok, Db} = create_db(DbName),
    Revs = [R || {ok, R} <- [
        save_doc(Db, {[{<<"_id">>, <<"doc1">>}]}),
        save_doc(Db, {[{<<"_id">>, <<"doc2">>}]}),
        save_doc(Db, {[{<<"_id">>, <<"doc3">>}]}),
        save_doc(Db, {[{<<"_id">>, <<"doc4">>}]}),
        save_doc(Db, {[{<<"_id">>, <<"doc5">>}]})
    ]],
    Rev = lists:nth(3, Revs),
    {ok, Rev1} = save_doc(Db, {[{<<"_id">>, <<"doc3">>}, {<<"_rev">>, Rev}]}),
    Revs1 = Revs ++ [Rev1],
    Revs2 = Revs1 ++ [R || {ok, R} <- [
        save_doc(Db, {[{<<"_id">>, <<"doc6">>}]}),
        save_doc(Db, {[{<<"_id">>, <<"_design/foo">>}]}),
        save_doc(Db, {[{<<"_id">>, <<"doc7">>}]}),
        save_doc(Db, {[{<<"_id">>, <<"doc8">>}]})
    ]],
    {DbName, list_to_tuple(Revs2)}.

teardown({DbName, _}) ->
    delete_db(DbName),
    ok.


changes_test_() ->
    {
        "Changes feeed",
        {
            setup,
            fun start/0, fun stop/1,
            [
                filter_by_doc_id(),
                filter_by_design(),
                continuous_feed(),
                filter_by_custom_function()
            ]
        }
    }.

filter_by_doc_id() ->
    {
        "Filter _doc_id",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                fun should_filter_by_specific_doc_ids/1,
                fun should_filter_by_specific_doc_ids_descending/1,
                fun should_filter_by_specific_doc_ids_with_since/1,
                fun should_filter_by_specific_doc_ids_no_result/1,
                fun should_handle_deleted_docs/1
            ]
        }
    }.

filter_by_design() ->
    {
        "Filter _design",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                fun should_emit_only_design_documents/1
            ]
        }
    }.

filter_by_custom_function() ->
    {
        "Filter function",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                fun should_receive_heartbeats/1
            ]
        }
    }.

continuous_feed() ->
    {
        "Continuous Feed",
        {
            foreach,
            fun setup/0, fun teardown/1,
            [
                fun should_filter_continuous_feed_by_specific_doc_ids/1
            ]
        }
    }.


should_filter_by_specific_doc_ids({DbName, _}) ->
    ?_test(
        begin
            ChangesArgs = #changes_args{
                filter = "_doc_ids"
            },
            DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
            Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
            Consumer = spawn_consumer(DbName, ChangesArgs, Req),

            {Rows, LastSeq} = wait_finished(Consumer),
            {ok, Db} = couch_db:open_int(DbName, []),
            UpSeq = couch_db:get_update_seq(Db),
            couch_db:close(Db),
            stop_consumer(Consumer),

            ?assertEqual(2, length(Rows)),
            [#row{seq = Seq1, id = Id1}, #row{seq = Seq2, id = Id2}] = Rows,
            ?assertEqual(<<"doc4">>, Id1),
            ?assertEqual(4, Seq1),
            ?assertEqual(<<"doc3">>, Id2),
            ?assertEqual(6, Seq2),
            ?assertEqual(UpSeq, LastSeq)
        end).

should_filter_by_specific_doc_ids_descending({DbName, _}) ->
    ?_test(
        begin
            ChangesArgs = #changes_args{
                filter = "_doc_ids",
                dir = rev
            },
            DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
            Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
            Consumer = spawn_consumer(DbName, ChangesArgs, Req),

            {Rows, LastSeq} = wait_finished(Consumer),
            {ok, Db} = couch_db:open_int(DbName, []),
            couch_db:close(Db),
            stop_consumer(Consumer),

            ?assertEqual(2, length(Rows)),
            [#row{seq = Seq1, id = Id1}, #row{seq = Seq2, id = Id2}] = Rows,
            ?assertEqual(<<"doc3">>, Id1),
            ?assertEqual(6, Seq1),
            ?assertEqual(<<"doc4">>, Id2),
            ?assertEqual(4, Seq2),
            ?assertEqual(4, LastSeq)
        end).

should_filter_by_specific_doc_ids_with_since({DbName, _}) ->
    ?_test(
        begin
            ChangesArgs = #changes_args{
                filter = "_doc_ids",
                since = 5
            },
            DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
            Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
            Consumer = spawn_consumer(DbName, ChangesArgs, Req),

            {Rows, LastSeq} = wait_finished(Consumer),
            {ok, Db} = couch_db:open_int(DbName, []),
            UpSeq = couch_db:get_update_seq(Db),
            couch_db:close(Db),
            stop_consumer(Consumer),

            ?assertEqual(1, length(Rows)),
            [#row{seq = Seq1, id = Id1}] = Rows,
            ?assertEqual(<<"doc3">>, Id1),
            ?assertEqual(6, Seq1),
            ?assertEqual(UpSeq, LastSeq)
        end).

should_filter_by_specific_doc_ids_no_result({DbName, _}) ->
    ?_test(
        begin
            ChangesArgs = #changes_args{
                filter = "_doc_ids",
                since = 6
            },
            DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
            Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
            Consumer = spawn_consumer(DbName, ChangesArgs, Req),

            {Rows, LastSeq} = wait_finished(Consumer),
            {ok, Db} = couch_db:open_int(DbName, []),
            UpSeq = couch_db:get_update_seq(Db),
            couch_db:close(Db),
            stop_consumer(Consumer),

            ?assertEqual(0, length(Rows)),
            ?assertEqual(UpSeq, LastSeq)
        end).

should_handle_deleted_docs({DbName, Revs}) ->
    ?_test(
        begin
            Rev3_2 = element(6, Revs),
            {ok, Db} = couch_db:open_int(DbName, []),
            {ok, _} = save_doc(
                Db,
                {[{<<"_id">>, <<"doc3">>},
                  {<<"_deleted">>, true},
                  {<<"_rev">>, Rev3_2}]}),

            ChangesArgs = #changes_args{
                filter = "_doc_ids",
                since = 9
            },
            DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
            Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
            Consumer = spawn_consumer(DbName, ChangesArgs, Req),

            {Rows, LastSeq} = wait_finished(Consumer),
            couch_db:close(Db),
            stop_consumer(Consumer),

            ?assertEqual(1, length(Rows)),
            ?assertMatch(
                [#row{seq = LastSeq, id = <<"doc3">>, deleted = true}],
                Rows
            ),
            ?assertEqual(11, LastSeq)
        end).

should_filter_continuous_feed_by_specific_doc_ids({DbName, Revs}) ->
    ?_test(
        begin
            {ok, Db} = couch_db:open_int(DbName, []),
            ChangesArgs = #changes_args{
                filter = "_doc_ids",
                feed = "continuous"
            },
            DocIds = [<<"doc3">>, <<"doc4">>, <<"doc9999">>],
            Req = {json_req, {[{<<"doc_ids">>, DocIds}]}},
            Consumer = spawn_consumer(DbName, ChangesArgs, Req),
            pause(Consumer),

            Rows = get_rows(Consumer),
            ?assertEqual(2, length(Rows)),
            [#row{seq = Seq1, id = Id1}, #row{seq = Seq2, id = Id2}] = Rows,
            ?assertEqual(<<"doc4">>, Id1),
            ?assertEqual(4, Seq1),
            ?assertEqual(<<"doc3">>, Id2),
            ?assertEqual(6, Seq2),

            clear_rows(Consumer),
            {ok, _Rev9} = save_doc(Db, {[{<<"_id">>, <<"doc9">>}]}),
            {ok, _Rev10} = save_doc(Db, {[{<<"_id">>, <<"doc10">>}]}),
            unpause(Consumer),
            pause(Consumer),
            ?assertEqual([], get_rows(Consumer)),

            Rev4 = element(4, Revs),
            Rev3_2 = element(6, Revs),
            {ok, Rev4_2} = save_doc(Db, {[{<<"_id">>, <<"doc4">>},
                                          {<<"_rev">>, Rev4}]}),
            {ok, _} = save_doc(Db, {[{<<"_id">>, <<"doc11">>}]}),
            {ok, _} = save_doc(Db, {[{<<"_id">>, <<"doc4">>},
                                     {<<"_rev">>, Rev4_2}]}),
            {ok, _} = save_doc(Db, {[{<<"_id">>, <<"doc12">>}]}),
            {ok, Rev3_3} = save_doc(Db, {[{<<"_id">>, <<"doc3">>},
                                          {<<"_rev">>, Rev3_2}]}),
            unpause(Consumer),
            pause(Consumer),

            NewRows = get_rows(Consumer),
            ?assertEqual(2, length(NewRows)),
            [Row14, Row16] = NewRows,
            ?assertEqual(<<"doc4">>, Row14#row.id),
            ?assertEqual(15, Row14#row.seq),
            ?assertEqual(<<"doc3">>, Row16#row.id),
            ?assertEqual(17, Row16#row.seq),

            clear_rows(Consumer),
            {ok, _Rev3_4} = save_doc(Db, {[{<<"_id">>, <<"doc3">>},
                                           {<<"_rev">>, Rev3_3}]}),
            unpause(Consumer),
            pause(Consumer),

            FinalRows = get_rows(Consumer),

            unpause(Consumer),
            stop_consumer(Consumer),

            ?assertMatch([#row{seq = 18, id = <<"doc3">>}], FinalRows)
        end).

should_emit_only_design_documents({DbName, Revs}) ->
    ?_test(
        begin
            ChangesArgs = #changes_args{
                filter = "_design"
            },
            Consumer = spawn_consumer(DbName, ChangesArgs, {json_req, null}),

            {Rows, LastSeq} = wait_finished(Consumer),
            {ok, Db} = couch_db:open_int(DbName, []),
            UpSeq = couch_db:get_update_seq(Db),
            couch_db:close(Db),

            ?assertEqual(1, length(Rows)),
            ?assertEqual(UpSeq, LastSeq),
            ?assertEqual([#row{seq = 8, id = <<"_design/foo">>}], Rows),

            stop_consumer(Consumer),

            {ok, Db2} = couch_db:open_int(DbName, [?ADMIN_USER]),
            {ok, _} = save_doc(Db2, {[{<<"_id">>, <<"_design/foo">>},
                                      {<<"_rev">>, element(8, Revs)},
                                      {<<"_deleted">>, true}]}),

            Consumer2 = spawn_consumer(DbName, ChangesArgs, {json_req, null}),

            {Rows2, LastSeq2} = wait_finished(Consumer2),
            UpSeq2 = UpSeq + 1,
            couch_db:close(Db2),

            ?assertEqual(1, length(Rows2)),
            ?assertEqual(UpSeq2, LastSeq2),
            ?assertEqual([#row{seq = 11,
                               id = <<"_design/foo">>,
                               deleted = true}],
                          Rows2)
        end).

should_receive_heartbeats(_) ->
    {timeout, ?TEST_TIMEOUT div 1000,
     ?_test(
         begin
             DbName = ?tempdb(),
             Timeout = 100,
             {ok, Db} = create_db(DbName),

             {ok, _} = save_doc(Db, {[
                 {<<"_id">>, <<"_design/filtered">>},
                 {<<"language">>, <<"javascript">>},
                     {<<"filters">>, {[
                         {<<"foo">>, <<"function(doc) {
                             return ['doc10', 'doc11', 'doc12'].indexOf(doc._id) != -1;}">>
                     }]}}
             ]}),

             ChangesArgs = #changes_args{
                 filter = "filtered/foo",
                 feed = "continuous",
                 timeout = 10000,
                 heartbeat = 1000
             },
             Consumer = spawn_consumer(DbName, ChangesArgs, {json_req, null}),

             {ok, _Rev1} = save_doc(Db, {[{<<"_id">>, <<"doc1">>}]}),
             timer:sleep(Timeout),
             {ok, _Rev2} = save_doc(Db, {[{<<"_id">>, <<"doc2">>}]}),
             timer:sleep(Timeout),
             {ok, _Rev3} = save_doc(Db, {[{<<"_id">>, <<"doc3">>}]}),
             timer:sleep(Timeout),
             {ok, _Rev4} = save_doc(Db, {[{<<"_id">>, <<"doc4">>}]}),
             timer:sleep(Timeout),
             {ok, _Rev5} = save_doc(Db, {[{<<"_id">>, <<"doc5">>}]}),
             timer:sleep(Timeout),
             {ok, _Rev6} = save_doc(Db, {[{<<"_id">>, <<"doc6">>}]}),
             timer:sleep(Timeout),
             {ok, _Rev7} = save_doc(Db, {[{<<"_id">>, <<"doc7">>}]}),
             timer:sleep(Timeout),
             {ok, _Rev8} = save_doc(Db, {[{<<"_id">>, <<"doc8">>}]}),
             timer:sleep(Timeout),
             {ok, _Rev9} = save_doc(Db, {[{<<"_id">>, <<"doc9">>}]}),

             Heartbeats = get_heartbeats(Consumer),
             ?assert(Heartbeats > 0),

             {ok, _Rev10} = save_doc(Db, {[{<<"_id">>, <<"doc10">>}]}),
             timer:sleep(Timeout),
             {ok, _Rev11} = save_doc(Db, {[{<<"_id">>, <<"doc11">>}]}),
             timer:sleep(Timeout),
             {ok, _Rev12} = save_doc(Db, {[{<<"_id">>, <<"doc12">>}]}),

             Heartbeats2 = get_heartbeats(Consumer),
             ?assert(Heartbeats2 > Heartbeats),

             Rows = get_rows(Consumer),
             ?assertEqual(3, length(Rows)),

             {ok, _Rev13} = save_doc(Db, {[{<<"_id">>, <<"doc13">>}]}),
             timer:sleep(Timeout),
             {ok, _Rev14} = save_doc(Db, {[{<<"_id">>, <<"doc14">>}]}),
             timer:sleep(Timeout),

             Heartbeats3 = get_heartbeats(Consumer),
             ?assert(Heartbeats3 > Heartbeats2)
        end)}.


save_doc(Db, Json) ->
    Doc = couch_doc:from_json_obj(Json),
    {ok, Rev} = couch_db:update_doc(Db, Doc, []),
    {ok, couch_doc:rev_to_str(Rev)}.

get_rows(Consumer) ->
    Ref = make_ref(),
    Consumer ! {get_rows, Ref},
    Resp = receive
        {rows, Ref, Rows} ->
            Rows
    after ?TIMEOUT ->
        timeout
    end,
    ?assertNotEqual(timeout, Resp),
    Resp.

get_heartbeats(Consumer) ->
    Ref = make_ref(),
    Consumer ! {get_heartbeats, Ref},
    Resp = receive
        {hearthbeats, Ref, HeartBeats} ->
            HeartBeats
    after ?TIMEOUT ->
        timeout
    end,
    ?assertNotEqual(timeout, Resp),
    Resp.

clear_rows(Consumer) ->
    Ref = make_ref(),
    Consumer ! {reset, Ref},
    Resp = receive
        {ok, Ref} ->
            ok
    after ?TIMEOUT ->
        timeout
    end,
    ?assertNotEqual(timeout, Resp),
    Resp.

stop_consumer(Consumer) ->
    Ref = make_ref(),
    Consumer ! {stop, Ref},
    Resp = receive
        {ok, Ref} ->
            ok
    after ?TIMEOUT ->
        timeout
    end,
    ?assertNotEqual(timeout, Resp),
    Resp.

pause(Consumer) ->
    Ref = make_ref(),
    Consumer ! {pause, Ref},
    Resp = receive
        {paused, Ref} ->
            ok
    after ?TIMEOUT ->
        timeout
    end,
    ?assertNotEqual(timeout, Resp),
    Resp.

unpause(Consumer) ->
    Ref = make_ref(),
    Consumer ! {continue, Ref},
    Resp = receive
        {ok, Ref} ->
            ok
    after ?TIMEOUT ->
       timeout
    end,
    ?assertNotEqual(timeout, Resp),
    Resp.

wait_finished(_Consumer) ->
    Resp = receive
        {consumer_finished, Rows, LastSeq} ->
            {Rows, LastSeq}
    after ?TIMEOUT ->
        timeout
    end,
    ?assertNotEqual(timeout, Resp),
    Resp.

spawn_consumer(DbName, ChangesArgs0, Req) ->
    Parent = self(),
    spawn(fun() ->
        put(heartbeat_count, 0),
        Callback = fun
            ({change, {Change}, _}, _, Acc) ->
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
            throw({stop, Acc});
        V ->
            erlang:error({assertion_failed,
                      [{module, ?MODULE},
                       {line, ?LINE},
                       {value, V},
                       {reason, "Received unexpected message"}]})
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
    couch_db:create(DbName, [?ADMIN_USER, overwrite]).

delete_db(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_USER]).
