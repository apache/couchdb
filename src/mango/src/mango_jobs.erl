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


-module(mango_jobs).

-include("mango_idx.hrl").
-include("mango.hrl").


-export([
    set_timeout/0,
    build_index/2
]).


set_timeout() ->
    couch_jobs:set_type_timeout(?MANGO_INDEX_JOB_TYPE, 6).


build_index(TxDb, #idx{} = Idx) ->
    #{
        tx := Tx
    } = TxDb,

    mango_fdb:create_build_vs(TxDb, Idx),

    JobId = job_id(TxDb, Idx),
    JobData = job_data(TxDb, Idx),
    ok = couch_jobs:add(undefined, ?MANGO_INDEX_JOB_TYPE, JobId, JobData),
    {ok, JobId}.


job_id(#{name := DbName}, #idx{ddoc = DDoc} = Idx) ->
    Cols = iolist_to_binary(mango_idx:columns(Idx)),
    <<DbName/binary, "_",DDoc/binary, Cols/binary>>.


job_data(Db, Idx) ->
    #{
        db_name => fabric2_db:name(Db),
        ddoc_id => mango_idx:ddoc(Idx),
        columns => mango_idx:columns(Idx),
        retries => 0
    }.

