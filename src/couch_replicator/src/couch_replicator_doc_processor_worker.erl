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

-module(couch_replicator_doc_processor_worker).

-export([
    spawn_worker/4
]).

-include("couch_replicator.hrl").

-import(couch_replicator_utils, [
    pp_rep_id/1
]).

% 61 seconds here because request usually have 10, 15, 30 second
% timeouts set.  We'd want the worker to get a chance to make a few
% requests (maybe one failing one and a retry) and then fail with its
% own error (timeout, network error), which would be more specific and
% informative, before it simply gets killed because of the timeout
% here. That is, if all fails and the worker is actually blocked then
% 61 sec is a safety net to brutally kill the worker so doesn't end up
% hung forever.
-define(WORKER_TIMEOUT_MSEC, 61000).


% Spawn a worker which attempts to calculate replication id then add a
% replication job to scheduler. This function create a monitor to the worker
% a worker will then exit with the #doc_worker_result{} record within
% ?WORKER_TIMEOUT_MSEC timeout period.A timeout is considered a
%`temporary_error`. Result will be sent as the `Reason` in the {'DOWN',...}
% message.
-spec spawn_worker(db_doc_id(), #rep{}, seconds(), reference()) -> pid().
spawn_worker(Id, Rep, WaitSec, WRef) ->
    {Pid, _Ref} = spawn_monitor(fun() ->
        worker_fun(Id, Rep, WaitSec, WRef)
    end),
    Pid.


% Private functions

-spec worker_fun(db_doc_id(), #rep{}, seconds(), reference()) -> no_return().
worker_fun(Id, Rep, WaitSec, WRef) ->
    timer:sleep(WaitSec * 1000),
    Fun = fun() ->
        try maybe_start_replication(Id, Rep, WRef) of
            Res ->
                exit(Res)
        catch
            throw:{filter_fetch_error, Reason} ->
                exit({temporary_error, Reason});
            _Tag:Reason ->
                exit({temporary_error, Reason})
        end
    end,
    {Pid, Ref} = spawn_monitor(Fun),
    receive
        {'DOWN', Ref, _, Pid, Result} ->
            exit(#doc_worker_result{id = Id, wref = WRef, result = Result})
    after ?WORKER_TIMEOUT_MSEC ->
        erlang:demonitor(Ref, [flush]),
        exit(Pid, kill),
        {DbName, DocId} = Id,
        TimeoutSec = round(?WORKER_TIMEOUT_MSEC / 1000),
        Msg = io_lib:format("Replication for db ~p doc ~p failed to start due "
            "to timeout after ~B seconds", [DbName, DocId, TimeoutSec]),
        Result = {temporary_error, couch_util:to_binary(Msg)},
        exit(#doc_worker_result{id = Id, wref = WRef, result = Result})
    end.


% Try to start a replication. Used by a worker. This function should return
% rep_start_result(), also throws {filter_fetch_error, Reason} if cannot fetch
% filter.It can also block for an indeterminate amount of time while fetching
% filter.
maybe_start_replication(Id, RepWithoutId, WRef) ->
    Rep = couch_replicator_docs:update_rep_id(RepWithoutId),
    case maybe_add_job_to_scheduler(Id, Rep, WRef) of
    ignore ->
        ignore;
    {ok, RepId} ->
        {ok, RepId};
    {temporary_error, Reason} ->
        {temporary_error, Reason};
    {permanent_failure, Reason} ->
        {DbName, DocId} = Id,
        couch_replicator_docs:update_failed(DbName, DocId, Reason),
        {permanent_failure, Reason}
    end.


-spec maybe_add_job_to_scheduler(db_doc_id(), #rep{}, reference()) ->
   rep_start_result().
maybe_add_job_to_scheduler({DbName, DocId}, Rep, WRef) ->
    RepId = Rep#rep.id,
    case couch_replicator_scheduler:rep_state(RepId) of
    nil ->
        % Before adding a job check that this worker is still the current
        % worker. This is to handle a race condition where a worker which was
        % sleeping and then checking a replication filter may inadvertently
        % re-add a replication which was already deleted.
        case couch_replicator_doc_processor:get_worker_ref({DbName, DocId}) of
        WRef ->
            ok = couch_replicator_scheduler:add_job(Rep),
            {ok, RepId};
        _NilOrOtherWRef ->
            ignore
        end;
    #rep{doc_id = DocId} ->
        {ok, RepId};
    #rep{doc_id = null} ->
        Msg = io_lib:format("Replication `~s` specified by document `~s`"
            " already running as a transient replication, started via"
            " `_replicate` API endpoint", [pp_rep_id(RepId), DocId]),
        {temporary_error, couch_util:to_binary(Msg)};
    #rep{db_name = OtherDb, doc_id = OtherDocId} ->
        Msg = io_lib:format("Replication `~s` specified by document `~s`"
            " already started, triggered by document `~s` from db `~s`",
            [pp_rep_id(RepId), DocId, OtherDocId, mem3:dbname(OtherDb)]),
        {permanent_failure, couch_util:to_binary(Msg)}
    end.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(DB, <<"db">>).
-define(DOC1, <<"doc1">>).
-define(R1, {"ad08e05057046eabe898a2572bbfb573", ""}).


doc_processor_worker_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            t_should_add_job(),
            t_already_running_same_docid(),
            t_already_running_transient(),
            t_already_running_other_db_other_doc(),
            t_spawn_worker(),
            t_ignore_if_doc_deleted(),
            t_ignore_if_worker_ref_does_not_match()
        ]
    }.


% Replication is already running, with same doc id. Ignore change.
t_should_add_job() ->
   ?_test(begin
       Id = {?DB, ?DOC1},
       Rep = couch_replicator_docs:parse_rep_doc_without_id(change()),
       ?assertEqual({ok, ?R1}, maybe_start_replication(Id, Rep, nil)),
       ?assert(added_job())
   end).


% Replication is already running, with same doc id. Ignore change.
t_already_running_same_docid() ->
   ?_test(begin
       Id = {?DB, ?DOC1},
       mock_already_running(?DB, ?DOC1),
       Rep = couch_replicator_docs:parse_rep_doc_without_id(change()),
       ?assertEqual({ok, ?R1}, maybe_start_replication(Id, Rep, nil)),
       ?assert(did_not_add_job())
   end).


% There is a transient replication with same replication id running. Ignore.
t_already_running_transient() ->
   ?_test(begin
       Id = {?DB, ?DOC1},
       mock_already_running(null, null),
       Rep = couch_replicator_docs:parse_rep_doc_without_id(change()),
       ?assertMatch({temporary_error, _}, maybe_start_replication(Id, Rep,
           nil)),
       ?assert(did_not_add_job())
   end).


% There is a duplicate replication potentially from a different db and doc.
% Write permanent failure to doc.
t_already_running_other_db_other_doc() ->
   ?_test(begin
       Id = {?DB, ?DOC1},
       mock_already_running(<<"otherdb">>, <<"otherdoc">>),
       Rep = couch_replicator_docs:parse_rep_doc_without_id(change()),
       ?assertMatch({permanent_failure, _}, maybe_start_replication(Id, Rep,
           nil)),
       ?assert(did_not_add_job()),
       1 == meck:num_calls(couch_replicator_docs, update_failed, '_')
   end).


% Should spawn worker
t_spawn_worker() ->
   ?_test(begin
       Id = {?DB, ?DOC1},
       Rep = couch_replicator_docs:parse_rep_doc_without_id(change()),
       WRef = make_ref(),
       meck:expect(couch_replicator_doc_processor, get_worker_ref, 1, WRef),
       Pid = spawn_worker(Id, Rep, 0, WRef),
       Res = receive  {'DOWN', _Ref, process, Pid, Reason} -> Reason
           after 1000 -> timeout end,
       Expect = #doc_worker_result{id = Id, wref = WRef, result = {ok, ?R1}},
       ?assertEqual(Expect, Res),
       ?assert(added_job())
   end).


% Should not add job if by the time worker got to fetching the filter
% and getting a replication id, replication doc was deleted
t_ignore_if_doc_deleted() ->
   ?_test(begin
       Id = {?DB, ?DOC1},
       Rep = couch_replicator_docs:parse_rep_doc_without_id(change()),
       meck:expect(couch_replicator_doc_processor, get_worker_ref, 1, nil),
       ?assertEqual(ignore, maybe_start_replication(Id, Rep, make_ref())),
       ?assertNot(added_job())
   end).


% Should not add job if by the time worker got to fetchign the filter
% and building a replication id, another worker was spawned.
t_ignore_if_worker_ref_does_not_match() ->
    ?_test(begin
       Id = {?DB, ?DOC1},
       Rep = couch_replicator_docs:parse_rep_doc_without_id(change()),
       meck:expect(couch_replicator_doc_processor, get_worker_ref, 1,
           make_ref()),
       ?assertEqual(ignore, maybe_start_replication(Id, Rep, make_ref())),
       ?assertNot(added_job())
   end).


% Test helper functions

setup() ->
    meck:expect(couch_replicator_scheduler, add_job, 1, ok),
    meck:expect(config, get, fun(_, _, Default) -> Default end),
    meck:expect(couch_server, get_uuid, 0, this_is_snek),
    meck:expect(couch_replicator_docs, update_failed, 3, ok),
    meck:expect(couch_replicator_scheduler, rep_state, 1, nil),
    meck:expect(couch_replicator_doc_processor, get_worker_ref, 1, nil),
    ok.


teardown(_) ->
    meck:unload().


mock_already_running(DbName, DocId) ->
    meck:expect(couch_replicator_scheduler, rep_state,
         fun(RepId) -> #rep{id = RepId, doc_id = DocId, db_name = DbName} end).


added_job() ->
    1 == meck:num_calls(couch_replicator_scheduler, add_job, '_').


did_not_add_job() ->
    0 == meck:num_calls(couch_replicator_scheduler, add_job, '_').


change() ->
    {[
         {<<"_id">>, ?DOC1},
         {<<"source">>, <<"http://srchost.local/src">>},
         {<<"target">>, <<"http://tgthost.local/tgt">>}
     ]}.

-endif.
