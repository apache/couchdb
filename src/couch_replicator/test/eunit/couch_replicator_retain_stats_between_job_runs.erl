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

-module(couch_replicator_retain_stats_between_job_runs).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").
-include_lib("fabric/test/fabric2_test.hrl").


-define(DELAY, 500).


stats_retained_test_() ->
    {
        setup,
        fun couch_replicator_test_helper:start_couch/0,
        fun couch_replicator_test_helper:stop_couch/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                ?TDEF_FE(t_stats_retained_on_job_removal, 60)
            ]
        }
    }.


setup() ->
    Source = couch_replicator_test_helper:create_db(),
    Target = couch_replicator_test_helper:create_db(),
    config:set("replicator", "stats_update_interval_sec", "0", false),
    config:set("replicator", "checkpoint_interval", "1000", false),
    {Source, Target}.


teardown({Source, Target}) ->
    config:delete("replicator", "stats_update_interval_sec", false),
    config:delete("replicator", "checkpoint_interval", false),
    couch_replicator_test_helper:delete_db(Source),
    couch_replicator_test_helper:delete_db(Target).


t_stats_retained_on_job_removal({Source, Target}) ->
    {ok, _} = add_vdu(Target),
    populate_db_reject_even_docs(Source, 1, 10),
    {ok, Pid1, RepId} = replicate(Source, Target),
    wait_target_in_sync(6, Target), % 5 + 1 vdu

    check_scheduler_jobs(10, 5, 5),

    cancel(RepId, Pid1),

    populate_db_reject_even_docs(Source, 11, 20),
    {ok, Pid2, RepId} = replicate(Source, Target),
    wait_target_in_sync(11, Target), % 6 + 5

    check_scheduler_jobs(20, 10, 10),

    cancel(RepId, Pid2),

    populate_db_reject_even_docs(Source, 21, 30),
    {ok, Pid3, RepId} = replicate(Source, Target),
    wait_target_in_sync(16, Target), % 11 + 5

    check_scheduler_jobs(30, 15, 15),

    cancel(RepId, Pid3).


check_scheduler_jobs(DocsRead, DocsWritten, DocFailed) ->
    Info = wait_scheduler_info(DocsRead),
    ?assert(maps:is_key(<<"changes_pending">>, Info)),
    ?assert(maps:is_key(<<"doc_write_failures">>, Info)),
    ?assert(maps:is_key(<<"docs_read">>, Info)),
    ?assert(maps:is_key(<<"docs_written">>, Info)),
    ?assert(maps:is_key(<<"missing_revisions_found">>, Info)),
    ?assert(maps:is_key(<<"checkpointed_source_seq">>, Info)),
    ?assert(maps:is_key(<<"source_seq">>, Info)),
    ?assert(maps:is_key(<<"revisions_checked">>, Info)),
    ?assertMatch(#{<<"docs_read">> := DocsRead}, Info),
    ?assertMatch(#{<<"docs_written">> := DocsWritten}, Info),
    ?assertMatch(#{<<"doc_write_failures">> := DocFailed}, Info).


wait_scheduler_info(DocsRead) ->
    test_util:wait(fun() ->
        case couch_replicator_test_helper:scheduler_jobs() of
            [] ->
                wait;
            [#{<<"info">> := null}] ->
                wait;
            [#{<<"info">> := Info}] ->
                case Info of
                    #{<<"docs_read">> := DocsRead} -> Info;
                    #{} -> wait
                end
        end
    end).


populate_db_reject_even_docs(DbName, Start, End) ->
    BodyFun = fun(Id) ->
        case Id rem 2 == 0 of
            true -> {[{<<"nope">>, true}]};
            false -> {[]}
        end
    end,
    populate_db(DbName, Start, End, BodyFun).


populate_db(DbName, Start, End, BodyFun) when is_function(BodyFun, 1) ->
    Docs = lists:foldl(fun(DocIdCounter, Acc) ->
        Id = integer_to_binary(DocIdCounter),
        Doc = #doc{id = Id, body = BodyFun(DocIdCounter)},
        [Doc | Acc]
    end, [], lists:seq(Start, End)),
    couch_replicator_test_helper:create_docs(DbName, Docs).


wait_target_in_sync(DocCount, Target) when is_integer(DocCount) ->
    wait_target_in_sync_loop(DocCount, Target, 300).


wait_target_in_sync_loop(_DocCount, _TargetName, 0) ->
    erlang:error({assertion_failed, [
        {module, ?MODULE}, {line, ?LINE},
        {reason, "Could not get source and target databases in sync"}
    ]});

wait_target_in_sync_loop(DocCount, TargetName, RetriesLeft) ->
    {ok, Db} = fabric2_db:open(TargetName, [?ADMIN_CTX]),
    {ok, TargetInfo} = fabric2_db:get_db_info(Db),
    TargetDocCount = couch_util:get_value(doc_count, TargetInfo),
    case TargetDocCount  == DocCount of
        true ->
            true;
        false ->
            ok = timer:sleep(?DELAY),
            wait_target_in_sync_loop(DocCount, TargetName, RetriesLeft - 1)
    end.


replicate(Source, Target) ->
    couch_replicator_test_helper:replicate_continuous(Source, Target).


cancel(RepId, Pid) ->
    couch_replicator_test_helper:cancel(RepId, Pid).


vdu() ->
    <<"function(newDoc, oldDoc, userCtx) {
        if(newDoc.nope === true) {
            throw({forbidden: 'nope'});
        } else {
            return;
        }
    }">>.


add_vdu(DbName) ->
    DocProps = [
        {<<"_id">>, <<"_design/vdu">>},
        {<<"language">>, <<"javascript">>},
        {<<"validate_doc_update">>, vdu()}
    ],
    Doc = couch_doc:from_json_obj({DocProps}, []),
    {ok, Db} = fabric2_db:open(DbName, [?ADMIN_CTX]),
    {ok, _} = fabric2_db:update_doc(Db, Doc, []).
