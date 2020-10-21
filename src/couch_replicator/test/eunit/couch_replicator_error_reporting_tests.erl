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


setup_all() ->
    test_util:start_couch([couch_replicator, chttpd, mem3, fabric]).


teardown_all(Ctx) ->
    ok = test_util:stop_couch(Ctx).


setup() ->
    meck:unload(),
    Source = setup_db(),
    Target = setup_db(),
    {Source, Target}.


teardown({Source, Target}) ->
    meck:unload(),
    teardown_db(Source),
    teardown_db(Target),
    ok.


error_reporting_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                fun t_fail_bulk_docs/1,
                fun t_fail_changes_reader/1,
                fun t_fail_revs_diff/1,
                fun t_fail_changes_queue/1,
                fun t_fail_changes_manager/1,
                fun t_fail_changes_reader_proc/1
            ]
        }
    }.


t_fail_bulk_docs({Source, Target}) ->
    ?_test(begin
        populate_db(Source, 1, 5),
        {ok, RepId} = replicate(Source, Target),
        wait_target_in_sync(Source, Target),

        {ok, Listener} = rep_result_listener(RepId),
        mock_fail_req("/_bulk_docs", {ok, "403", [], [<<"{\"x\":\"y\"}">>]}),
        populate_db(Source, 6, 6),

        {error, Result} = wait_rep_result(RepId),
        ?assertEqual({bulk_docs_failed, 403, {[{<<"x">>, <<"y">>}]}}, Result),

        couch_replicator_notifier:stop(Listener)
    end).


t_fail_changes_reader({Source, Target}) ->
    ?_test(begin
        populate_db(Source, 1, 5),
        {ok, RepId} = replicate(Source, Target),
        wait_target_in_sync(Source, Target),

        {ok, Listener} = rep_result_listener(RepId),
        mock_fail_req("/_changes", {ok, "418", [], [<<"{\"x\":\"y\"}">>]}),
        populate_db(Source, 6, 6),

        {error, Result} = wait_rep_result(RepId),
        ?assertEqual({changes_req_failed, 418, {[{<<"x">>, <<"y">>}]}}, Result),

        couch_replicator_notifier:stop(Listener)
    end).


t_fail_revs_diff({Source, Target}) ->
    ?_test(begin
        populate_db(Source, 1, 5),
        {ok, RepId} = replicate(Source, Target),
        wait_target_in_sync(Source, Target),

        {ok, Listener} = rep_result_listener(RepId),
        mock_fail_req("/_revs_diff", {ok, "407", [], [<<"{\"x\":\"y\"}">>]}),
        populate_db(Source, 6, 6),

        {error, Result} = wait_rep_result(RepId),
        ?assertEqual({revs_diff_failed, 407, {[{<<"x">>, <<"y">>}]}}, Result),

        couch_replicator_notifier:stop(Listener)
    end).


t_fail_changes_queue({Source, Target}) ->
    ?_test(begin
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
        couch_replicator_notifier:stop(Listener)
    end).


t_fail_changes_manager({Source, Target}) ->
    ?_test(begin
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
        couch_replicator_notifier:stop(Listener)
    end).


t_fail_changes_reader_proc({Source, Target}) ->
    ?_test(begin
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
        couch_replicator_notifier:stop(Listener)
    end).


mock_fail_req(Path, Return) ->
    meck:expect(ibrowse, send_req_direct,
        fun(W, Url, Headers, Meth, Body, Opts, TOut) ->
            Args = [W, Url, Headers, Meth, Body, Opts, TOut],
            {ok, {_, _, _, _, UPath, _}} = http_uri:parse(Url),
            case lists:suffix(Path, UPath) of
                true -> Return;
                false -> meck:passthrough(Args)
            end
        end).


rep_result_listener(RepId) ->
    ReplyTo = self(),
    {ok, _Listener} = couch_replicator_notifier:start_link(
        fun({_, RepId2, _} = Ev) when RepId2 =:= RepId ->
                ReplyTo ! Ev;
            (_) ->
                ok
        end).


wait_rep_result(RepId) ->
    receive
        {finished, RepId, RepResult} -> {ok, RepResult};
        {error, RepId, Reason} -> {error, Reason}
    end.



setup_db() ->
    DbName = ?tempdb(),
    {ok, Db} = couch_db:create(DbName, [?ADMIN_CTX]),
    ok = couch_db:close(Db),
    DbName.


teardown_db(DbName) ->
    ok = couch_server:delete(DbName, [?ADMIN_CTX]).


populate_db(DbName, Start, End) ->
    {ok, Db} = couch_db:open_int(DbName, []),
    Docs = lists:foldl(
        fun(DocIdCounter, Acc) ->
            Id = integer_to_binary(DocIdCounter),
            Doc = #doc{id = Id, body = {[]}},
            [Doc | Acc]
        end,
        [], lists:seq(Start, End)),
    {ok, _} = couch_db:update_docs(Db, Docs, []),
    ok = couch_db:close(Db).


wait_target_in_sync(Source, Target) ->
    {ok, SourceDb} = couch_db:open_int(Source, []),
    {ok, SourceInfo} = couch_db:get_db_info(SourceDb),
    ok = couch_db:close(SourceDb),
    SourceDocCount = couch_util:get_value(doc_count, SourceInfo),
    wait_target_in_sync_loop(SourceDocCount, Target, 300).


wait_target_in_sync_loop(_DocCount, _TargetName, 0) ->
    erlang:error({assertion_failed, [
          {module, ?MODULE}, {line, ?LINE},
          {reason, "Could not get source and target databases in sync"}
    ]});

wait_target_in_sync_loop(DocCount, TargetName, RetriesLeft) ->
    {ok, Target} = couch_db:open_int(TargetName, []),
    {ok, TargetInfo} = couch_db:get_db_info(Target),
    ok = couch_db:close(Target),
    TargetDocCount = couch_util:get_value(doc_count, TargetInfo),
    case TargetDocCount == DocCount of
        true ->
            true;
        false ->
            ok = timer:sleep(500),
            wait_target_in_sync_loop(DocCount, TargetName, RetriesLeft - 1)
    end.


replicate(Source, Target) ->
    SrcUrl = couch_replicator_test_helper:db_url(Source),
    TgtUrl = couch_replicator_test_helper:db_url(Target),
    RepObject = {[
        {<<"source">>, SrcUrl},
        {<<"target">>, TgtUrl},
        {<<"continuous">>, true},
        {<<"worker_processes">>, 1},
        {<<"retries_per_request">>, 1},
        % Low connection timeout so _changes feed gets restarted quicker
        {<<"connection_timeout">>, 3000}
    ]},
    {ok, Rep} = couch_replicator_utils:parse_rep_doc(RepObject, ?ADMIN_USER),
    ok = couch_replicator_scheduler:add_job(Rep),
    couch_replicator_scheduler:reschedule(),
    {ok, Rep#rep.id}.
