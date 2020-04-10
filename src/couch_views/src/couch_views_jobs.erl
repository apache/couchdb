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

-module(couch_views_jobs).

-export([
    set_timeout/0,
    build_view/3,
    build_view_async/2,
    remove/2
]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.


-include_lib("couch_mrview/include/couch_mrview.hrl").
-include("couch_views.hrl").


set_timeout() ->
    couch_jobs:set_type_timeout(?INDEX_JOB_TYPE, 6).


build_view(TxDb, Mrst, UpdateSeq) ->
    {ok, JobId} = build_view_async(TxDb, Mrst),
    case wait_for_job(JobId, Mrst#mrst.idx_name, UpdateSeq) of
        ok -> ok;
        retry -> build_view(TxDb, Mrst, UpdateSeq)
    end.


build_view_async(TxDb0, Mrst) ->
    JobId = job_id(TxDb0, Mrst),
    JobData = job_data(TxDb0, Mrst),
    DbUUID = fabric2_db:get_uuid(TxDb0),
    TxDb1 = ensure_correct_tx(TxDb0),
    couch_jobs_fdb:tx(couch_jobs_fdb:get_jtx(TxDb1), fun(JTx) ->
        case couch_jobs:get_job_data(JTx, ?INDEX_JOB_TYPE, JobId) of
            {error, not_found} ->
                ok;
            {ok, #{} = OldJobData} ->
                case maps:get(<<"db_uuid">>, OldJobData, undefined) of
                    DbUUID -> ok;
                    _ -> couch_jobs:remove(JTx, ?INDEX_JOB_TYPE, JobId)
                end
        end,
        ok = couch_jobs:add(JTx, ?INDEX_JOB_TYPE, JobId, JobData)
    end),
    {ok, JobId}.


remove(TxDb, Sig) ->
    DbName = fabric2_db:name(TxDb),
    JobId = job_id(DbName, Sig),
    couch_jobs:remove(TxDb, ?INDEX_JOB_TYPE, JobId).


ensure_correct_tx(#{tx := undefined} = TxDb) ->
    TxDb;

ensure_correct_tx(#{tx := Tx} = TxDb) ->
    case erlfdb:is_read_only(Tx) of
        true -> TxDb#{tx := undefined};
        false -> TxDb
    end.


wait_for_job(JobId, DDocId, UpdateSeq) ->
    case couch_jobs:subscribe(?INDEX_JOB_TYPE, JobId) of
        {ok, Subscription, _State, _Data} ->
            wait_for_job(JobId, Subscription, DDocId, UpdateSeq);
        {ok, finished, Data} ->
            case Data of
                #{<<"view_seq">> := ViewSeq} when ViewSeq >= UpdateSeq ->
                    ok;
                _ ->
                    retry
            end
    end.


wait_for_job(JobId, Subscription, DDocId, UpdateSeq) ->
    case wait(Subscription) of
        {not_found, not_found} ->
            erlang:error(index_not_found);
        {error, Error} ->
            erlang:error(Error);
        {finished, #{<<"error">> := <<"ddoc_deleted">>} = Data} ->
            case maps:get(<<"ddoc_id">>, Data) of
                DDocId ->
                    couch_jobs:remove(undefined, ?INDEX_JOB_TYPE, JobId),
                    erlang:error({ddoc_deleted, maps:get(<<"reason">>, Data)});
                _OtherDocId ->
                    % A different design doc wiht the same signature
                    % was deleted. Resubmit this job which will overwrite
                    % the ddoc_id in the job.
                    retry
            end;
        {finished, #{<<"error">> := Error, <<"reason">> := Reason}} ->
            couch_jobs:remove(undefined, ?INDEX_JOB_TYPE, JobId),
            erlang:error({binary_to_existing_atom(Error, latin1), Reason});
        {finished, #{<<"view_seq">> := ViewSeq}} when ViewSeq >= UpdateSeq ->
            ok;
        {finished, _} ->
            wait_for_job(JobId, DDocId, UpdateSeq);
        {_State, #{<<"view_seq">> := ViewSeq}} when ViewSeq >= UpdateSeq ->
            couch_jobs:unsubscribe(Subscription),
            ok;
        {_, _} ->
            wait_for_job(JobId, Subscription, DDocId, UpdateSeq)
    end.


job_id(#{name := DbName}, #mrst{sig = Sig}) ->
    job_id(DbName, Sig);

job_id(DbName, Sig) ->
    HexSig = fabric2_util:to_hex(Sig),
    <<DbName/binary, "-", HexSig/binary>>.


job_data(Db, Mrst) ->
    #mrst{
        idx_name = DDocId,
        sig = Sig
    } = Mrst,

    #{
        db_name => fabric2_db:name(Db),
        db_uuid => fabric2_db:get_uuid(Db),
        ddoc_id => DDocId,
        sig => fabric2_util:to_hex(Sig),
        retries => 0
    }.


wait(Subscription) ->
    case couch_jobs:wait(Subscription, infinity) of
        {?INDEX_JOB_TYPE, _JobId, JobState, JobData} ->
            {JobState, JobData};
        timeout ->
            {error, timeout}
    end.
