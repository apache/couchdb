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
    build_view_async/2
]).

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.


-include_lib("couch_mrview/include/couch_mrview.hrl").
-include("couch_views.hrl").


set_timeout() ->
    couch_jobs:set_type_timeout(?INDEX_JOB_TYPE, 6 * 1000).


build_view(TxDb, Mrst, UpdateSeq) ->
    {ok, JobId} = build_view_async(TxDb, Mrst),
    case wait_for_job(JobId, UpdateSeq) of
        ok -> ok;
        retry -> build_view(TxDb, Mrst, UpdateSeq)
    end.


build_view_async(TxDb, Mrst) ->
    JobId = job_id(TxDb, Mrst),
    JobData = job_data(TxDb, Mrst),
    ok = couch_jobs:add(undefined, ?INDEX_JOB_TYPE, JobId, JobData),
    {ok, JobId}.


wait_for_job(JobId, UpdateSeq) ->
    case couch_jobs:subscribe(?INDEX_JOB_TYPE, JobId) of
        {ok, Subscription, _State, _Data} ->
            wait_for_job(JobId, Subscription, UpdateSeq);
        {ok, finished, Data} ->
            case Data of
                #{<<"view_sig">> := ViewSeq} when ViewSeq >= UpdateSeq ->
                    ok;
                _ ->
                    retry
            end
    end.


wait_for_job(JobId, Subscription, UpdateSeq) ->
    case wait(Subscription) of
        {error, Error} ->
            erlang:error(Error);
        {finished, #{<<"error">> := Error, <<"reason">> := Reason}} ->
            erlang:error({binary_to_existing_atom(Error, latin1), Reason});
        {finished, #{<<"view_seq">> := ViewSeq}} when ViewSeq >= UpdateSeq ->
            ok;
        {finished, _} ->
            wait_for_job(JobId, UpdateSeq);
        {_State, #{<<"view_seq">> := ViewSeq}} when ViewSeq >= UpdateSeq ->
            couch_jobs:unsubscribe(Subscription),
            ok;
        {_, _} ->
            wait_for_job(JobId, Subscription, UpdateSeq)
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
        ddoc_id => DDocId,
        sig => fabric2_util:to_hex(Sig)
    }.


wait(Subscription) ->
    case couch_jobs:wait(Subscription, infinity) of
        {?INDEX_JOB_TYPE, _JobId, JobState, JobData} ->
            {JobState, JobData};
        timeout ->
            {error, timeout}
    end.
