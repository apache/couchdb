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

-module(couch_replicator_error_reporting_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").

error_reporting_test_() ->
    {
        foreach,
        fun couch_replicator_test_helper:test_setup/0,
        fun couch_replicator_test_helper:test_teardown/1,
        [
            ?TDEF_FE(t_fail_bulk_docs),
            ?TDEF_FE(t_fail_changes_reader),
            ?TDEF_FE(t_fail_revs_diff),
            ?TDEF_FE(t_fail_bulk_get, 15),
            ?TDEF_FE(t_fail_changes_queue),
            ?TDEF_FE(t_fail_changes_manager),
            ?TDEF_FE(t_fail_changes_reader_proc),
            ?TDEF_FE(t_dont_start_duplicate_job),
            ?TDEF_FE(t_can_start_multiple_jobs),
            ?TDEF_FE(t_stop_duplicate_job)
        ]
    }.

t_fail_bulk_docs({_Ctx, {Source, Target}}) ->
    populate_db(Source, 1, 5),
    {ok, RepId} = replicate(Source, Target),
    wait_target_in_sync(Source, Target),

    {ok, Listener} = rep_result_listener(RepId),
    mock_fail_req("/_bulk_docs", {ok, "403", [], [<<"{\"x\":\"y\"}">>]}),
    populate_db(Source, 6, 6),

    {error, Result} = wait_rep_result(RepId),
    ?assertEqual({bulk_docs_failed, 403, {[{<<"x">>, <<"y">>}]}}, Result),

    couch_replicator_notifier:stop(Listener).

t_fail_changes_reader({_Ctx, {Source, Target}}) ->
    populate_db(Source, 1, 5),
    {ok, RepId} = replicate(Source, Target),
    wait_target_in_sync(Source, Target),

    {ok, Listener} = rep_result_listener(RepId),
    mock_fail_req("/_changes", {ok, "418", [], [<<"{\"x\":\"y\"}">>]}),
    populate_db(Source, 6, 6),

    {error, Result} = wait_rep_result(RepId),
    ?assertEqual({changes_req_failed, 418, {[{<<"x">>, <<"y">>}]}}, Result),

    couch_replicator_notifier:stop(Listener).

t_fail_revs_diff({_Ctx, {Source, Target}}) ->
    populate_db(Source, 1, 5),
    {ok, RepId} = replicate(Source, Target),
    wait_target_in_sync(Source, Target),

    {ok, Listener} = rep_result_listener(RepId),
    mock_fail_req("/_revs_diff", {ok, "407", [], [<<"{\"x\":\"y\"}">>]}),
    populate_db(Source, 6, 6),

    {error, Result} = wait_rep_result(RepId),
    ?assertEqual({revs_diff_failed, 407, {[{<<"x">>, <<"y">>}]}}, Result),

    couch_replicator_notifier:stop(Listener).

t_fail_bulk_get({_Ctx, {Source, Target}}) ->
    % For _bulk_get the expectation is that the replication job will fallback
    % to a plain GET so the shape of the test is a bit different than the other
    % tests here.
    meck:new(couch_replicator_api_wrap, [passthrough]),
    populate_db(Source, 1, 5),
    {ok, _} = replicate(Source, Target),
    wait_target_in_sync(Source, Target),

    % Tolerate a 500 error
    mock_fail_req("/_bulk_get", {ok, "501", [], [<<"not_implemented">>]}),
    meck:reset(couch_replicator_api_wrap),
    populate_db(Source, 6, 6),
    wait_target_in_sync(Source, Target),
    % Check that there was a fallback to a plain GET
    ?assertEqual(1, meck:num_calls(couch_replicator_api_wrap, open_doc_revs, 6)),

    % Tolerate a 400 error
    mock_fail_req("/_bulk_get", {ok, "418", [], [<<"{\"x\":\"y\"}">>]}),
    meck:reset(couch_replicator_api_wrap),
    populate_db(Source, 7, 7),
    wait_target_in_sync(Source, Target),
    % Check that there was a falback to a plain GET
    ?assertEqual(1, meck:num_calls(couch_replicator_api_wrap, open_doc_revs, 6)).

t_fail_changes_queue({_Ctx, {Source, Target}}) ->
    populate_db(Source, 1, 5),
    {ok, RepId} = replicate(Source, Target),
    wait_target_in_sync(Source, Target),

    RepPid = couch_replicator_test_helper:get_pid(RepId),
    State = sys:get_state(RepPid),
    ChangesQueue = element(20, State),
    ?assert(is_process_alive(ChangesQueue)),

    {ok, Listener} = rep_result_listener(RepId),
    exit(ChangesQueue, boom),

    {error, Result} = wait_rep_result(RepId),
    ?assertEqual({changes_queue_died, boom}, Result),
    couch_replicator_notifier:stop(Listener).

t_fail_changes_manager({_Ctx, {Source, Target}}) ->
    populate_db(Source, 1, 5),
    {ok, RepId} = replicate(Source, Target),
    wait_target_in_sync(Source, Target),

    RepPid = couch_replicator_test_helper:get_pid(RepId),
    State = sys:get_state(RepPid),
    ChangesManager = element(21, State),
    ?assert(is_process_alive(ChangesManager)),

    {ok, Listener} = rep_result_listener(RepId),
    exit(ChangesManager, bam),

    {error, Result} = wait_rep_result(RepId),
    ?assertEqual({changes_manager_died, bam}, Result),
    couch_replicator_notifier:stop(Listener).

t_fail_changes_reader_proc({_Ctx, {Source, Target}}) ->
    populate_db(Source, 1, 5),
    {ok, RepId} = replicate(Source, Target),
    wait_target_in_sync(Source, Target),

    RepPid = couch_replicator_test_helper:get_pid(RepId),
    State = sys:get_state(RepPid),
    ChangesReader = element(22, State),
    ?assert(is_process_alive(ChangesReader)),

    {ok, Listener} = rep_result_listener(RepId),
    exit(ChangesReader, kapow),

    {error, Result} = wait_rep_result(RepId),
    ?assertEqual({changes_reader_died, kapow}, Result),
    couch_replicator_notifier:stop(Listener).

t_dont_start_duplicate_job({_Ctx, {Source, Target}}) ->
    meck:new(couch_replicator_pg, [passthrough]),
    Pid = pid_from_another_node(),
    meck:expect(couch_replicator_pg, should_start, fun(_, _) -> {no, Pid} end),
    Rep = make_rep(Source, Target),
    ExpectErr = {error, {already_started, Pid}},
    ?assertEqual(ExpectErr, couch_replicator_scheduler_job:start_link(Rep)).

t_can_start_multiple_jobs({_Ctx, {Source, Target1}}) ->
    Target2 = couch_replicator_test_helper:setup_db(),
    populate_db(Source, 1, 5),

    {ok, RepId1} = replicate(Source, Target1),
    {ok, RepId2} = replicate(Source, Target2),
    RepPid1 = couch_replicator_test_helper:get_pid(RepId1),
    RepPid2 = couch_replicator_test_helper:get_pid(RepId2),
    ?assert(is_pid(RepPid1)),
    ?assert(is_pid(RepPid2)),

    ?assert(is_process_alive(RepPid1)),
    ?assert(is_process_alive(RepPid2)),

    wait_target_in_sync(Source, Target1),
    wait_target_in_sync(Source, Target2),

    ?assert(is_process_alive(RepPid1)),
    ?assert(is_process_alive(RepPid2)),

    exit(RepPid1, kill),
    exit(RepPid2, kill),
    couch_replicator_test_helper:teardown_db(Target2).

t_stop_duplicate_job({_Ctx, {Source, Target}}) ->
    {ok, RepId} = replicate(Source, Target),
    wait_target_in_sync(Source, Target),
    RepPid = couch_replicator_test_helper:get_pid(RepId),
    {ok, Listener} = rep_result_listener(RepId),
    Pid = pid_from_another_node(),
    meck:expect(couch_replicator_pg, should_run, fun(_, _) -> {no, Pid} end),
    RepPid ! {'$gen_cast', checkpoint},
    {error, Result} = wait_rep_result(RepId),
    ?assertEqual(duplicate_job, Result),
    couch_replicator_notifier:stop(Listener).

pid_from_another_node() ->
    % Use a Pid serialized from a node named A@1
    % (A@1)1> term_to_binary(self()).
    Bin = <<131, 88, 100, 0, 3, 65, 64, 49, 0, 0, 0, 89, 0, 0, 0, 0, 99, 137, 147, 218>>,
    Pid = binary_to_term(Bin),
    ?assertEqual('A@1', node(Pid)),
    Pid.

mock_fail_req(Path, Return) ->
    meck:expect(
        ibrowse,
        send_req_direct,
        fun(W, Url, Headers, Meth, Body, Opts, TOut) ->
            Args = [W, Url, Headers, Meth, Body, Opts, TOut],
            #{path := UPath} = uri_string:parse(Url),
            case lists:suffix(Path, UPath) of
                true -> Return;
                false -> meck:passthrough(Args)
            end
        end
    ).

rep_result_listener(RepId) ->
    ReplyTo = self(),
    {ok, _Listener} = couch_replicator_notifier:start_link(
        fun
            ({_, RepId2, _} = Ev) when RepId2 =:= RepId ->
                ReplyTo ! Ev;
            (_) ->
                ok
        end
    ).

wait_rep_result(RepId) ->
    receive
        {finished, RepId, RepResult} -> {ok, RepResult};
        {error, RepId, Reason} -> {error, Reason}
    end.

populate_db(DbName, Start, End) ->
    Docs = lists:foldl(
        fun(DocIdCounter, Acc) ->
            Id = integer_to_binary(DocIdCounter),
            Doc = #doc{id = Id, body = {[]}},
            [Doc | Acc]
        end,
        [],
        lists:seq(Start, End)
    ),
    {ok, [_ | _]} = fabric:update_docs(DbName, Docs, [?ADMIN_CTX]).

wait_target_in_sync(Source, Target) ->
    {ok, SourceDocCount} = fabric:get_doc_count(Source),
    wait_target_in_sync_loop(SourceDocCount, Target, 300).

wait_target_in_sync_loop(_DocCount, _TargetName, 0) ->
    erlang:error(
        {assertion_failed, [
            {module, ?MODULE},
            {line, ?LINE},
            {reason, "Could not get source and target databases in sync"}
        ]}
    );
wait_target_in_sync_loop(DocCount, TargetName, RetriesLeft) ->
    {ok, TargetDocCount} = fabric:get_doc_count(TargetName),
    case TargetDocCount == DocCount of
        true ->
            true;
        false ->
            ok = timer:sleep(500),
            wait_target_in_sync_loop(DocCount, TargetName, RetriesLeft - 1)
    end.

replicate(Source, Target) ->
    Rep = make_rep(Source, Target),
    ok = couch_replicator_scheduler:add_job(Rep),
    couch_replicator_scheduler:reschedule(),
    {ok, Rep#rep.id}.

make_rep(Source, Target) ->
    RepObject =
        {[
            {<<"source">>, url(Source)},
            {<<"target">>, url(Target)},
            {<<"continuous">>, true},
            {<<"worker_processes">>, 1},
            {<<"retries_per_request">>, 1},
            % Low connection timeout so _changes feed gets restarted quicker
            {<<"connection_timeout">>, 3000}
        ]},
    {ok, Rep} = couch_replicator_parse:parse_rep_doc(RepObject, ?ADMIN_USER),
    Rep.

url(DbName) ->
    couch_replicator_test_helper:cluster_db_url(DbName).
